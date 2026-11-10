package xyz.kd5ujc.jlvm

import cats.effect.Sync
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, ValueKind}
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, Sigma, SigmaError, SigmaProp}
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.zk.Bn254Engine
import xyz.kd5ujc.jlvm.JlvmError._

import scodec.bits.ByteVector

/**
 * The cryptographic operations, over the dilf4s backends. Two backends are native libraries, loaded as an effect at the
 * edge ([[CryptoOps.load]]): every node evaluates with the same complete set, so no operation's availability depends on
 * the classpath.
 *
 * Malformed input (a wrong length, a key that is not a curve point) is a [[JlvmError.Crypto]] error; a well-formed
 * signature that does not verify is `false`.
 */
final class CryptoOps private (secp: Secp256k1, bls: Bls12381, zk: ZkOps) {

  type Result = Either[JlvmError, CanonicalValue]

  /** `None` for operations not implemented here. */
  def apply(op: Op, args: Vector[CanonicalValue]): Option[Result] =
    op match {
      case Op.Ed25519Verify      => Option(signature(op, args)(Ed25519.codec, Ed25519.verifier))
      case Op.Secp256k1Verify    => Option(signature(op, args)(secp.ecdsaCodec, secp.ecdsaVerifier))
      case Op.BlsVerify          => Option(signature(op, args)(bls.codec, bls.verifier))
      case Op.Bip340Verify       => Option(bip340(op, args))
      case Op.Ecrecover          => Option(ecrecover(op, args))
      case Op.BlsAggregateVerify => Option(blsAggregate(op, args))
      case Op.SigmaVerify        => Option(sigma(op, args))
      case Op.SchnorrVerify      => Option(schnorr(op, args))
      case _                     => ProofOps(op, args).orElse(zk(op, args))
    }

  private def crypto[A](op: Op)(e: CryptoError): JlvmError = JlvmError.Crypto(op, CryptoReason.Refused(CryptoCause.Signing(e)))

  private def raw(op: Op, v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case other     => JlvmError.Type(op, TypeReason.Expected(Vector(ValueKind.Bytes), ValueKind.of(other))).asLeft
    }

  /** Bytes charged by weight, flattened ([[FlatBytes]]): the charge pays for the copy when they are not already flat. */
  private def bytes(op: Op, v: CanonicalValue): Either[JlvmError, FlatBytes] = raw(op, v).map(FlatBytes(_))

  /**
   * Bytes charged per argument, never by size (`ecrecover`'s, and `bls_aggregate_verify`'s keys and signature), passed
   * as they are: flattening them would copy bytes nobody paid for. Their backends check the fixed width first, from the
   * size alone, and only then read, and bytes of a fixed width have fewer segments than bytes: each read is bounded.
   */
  private def fixed(op: Op, v: CanonicalValue): Either[JlvmError, ByteVector] = raw(op, v)

  private def triple(op: Op, args: Vector[CanonicalValue]): Either[JlvmError, (FlatBytes, FlatBytes, FlatBytes)] =
    args match {
      case Vector(k, m, s) => (bytes(op, k), bytes(op, m), bytes(op, s)).tupled
      case _               => JlvmError.Arity(op, ArgumentCount.Exactly(3), args.size).asLeft
    }

  /** `[key, message, signature]`, all bytes. */
  private def signature[S](op: Op, args: Vector[CanonicalValue])(codec: SchemeCodec[S], verifier: Verifier[S]): Result =
    triple(op, args).flatMap {
      case (k, m, s) =>
        (codec.verifyingKey(k), codec.signature(s)).tupled.leftMap(crypto(op)).map {
          case (key, sig) =>
            Values.bool(verifier.verify(key, m, sig))
        }
    }

  /** Standard BIP-340: the message is exactly 32 bytes. */
  private def bip340(op: Op, args: Vector[CanonicalValue]): Result =
    triple(op, args).flatMap {
      case (k, m, s) =>
        (secp.bip340Codec.verifyingKey(k), Message32.from(m), secp.bip340Codec.signature(s)).tupled
          .leftMap(crypto(op))
          .map { case (key, msg, sig) => Values.bool(secp.bip340Verify(key, msg, sig)) }
    }

  /**
   * `[digest32, signature65]`: the signer's 20-byte Ethereum address. A signature that recovers no key is an error, not a
   * value: Ethereum's zero address, or a null, would equal an unset owner, and a program comparing against one would
   * accept any signature.
   */
  private def ecrecover(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(d, s) =>
        (fixed(op, d), fixed(op, s)).tupled.flatMap {
          case (digest, sig) =>
            Ecrecover
              .recover(digest, sig)
              .leftMap(e => crypto(op)(e.toCryptoError))
              .flatMap(
                _.toRight(
                  JlvmError.Crypto(op, CryptoReason.NoRecoveredKey)
                )
              )
              .map(address => Values.bytes(FlatBytes(address)))
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Exactly(2), args.size).asLeft
    }

  /** `[proposition, proof, message]` (docs/specs/sigma.md): the proposition is the tagged-map form. */
  private def sigma(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(p, proof, message) =>
        (SigmaProp.decode(p).leftMap(sigmaRefusal(op)), bytes(op, proof), bytes(op, message)).tupled.flatMap {
          case (prop, pf, m) => Sigma.check(prop, pf, m).bimap(sigmaRefusal(op), Values.bool)
        }
      case _ => JlvmError.Arity(op, ArgumentCount.Exactly(3), args.size).asLeft
    }

  /** A sigma refusal retains its typed cause inside the JLVM's `Crypto` (jlvm.md section 3). */
  private def sigmaRefusal(op: Site)(e: SigmaError): JlvmError = JlvmError.Crypto(op, CryptoReason.Refused(CryptoCause.Sigma(e)))

  /** `[key33, message, proof56]`: a single-key sigma proof, Ergo's P2PK signature. */
  private def schnorr(op: Op, args: Vector[CanonicalValue]): Result =
    triple(op, args).flatMap {
      case (k, m, s) =>
        Secp256k1Group
          .decode(k)
          .leftMap(reason => sigmaRefusal(op)(SigmaError.InvalidPoint(SigmaError.PointRole.PublicKey, reason)))
          .flatMap(SigmaProp.dlog(_).flatMap(Sigma.check(_, s, m)).bimap(sigmaRefusal(op), Values.bool))
    }

  /**
   * `[[key, ...], message, signature]`: one message signed by every key (fast aggregate verification). The keys must
   * carry proofs of possession, checked by whoever admitted them: without that, a rogue key can forge an aggregate.
   */
  private def blsAggregate(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(CArray(ks), m, s) =>
        (ks.traverse(fixed(op, _)), bytes(op, m), fixed(op, s)).tupled.flatMap {
          case (keys, msg, sig) =>
            (keys.traverse(bls.codec.verifyingKey), bls.codec.signature(sig)).tupled
              .leftMap(crypto(op))
              .map { case (vks, signature) => Values.bool(bls.fastAggregateVerify(vks.toList, msg, signature)) }
        }
      case Vector(other, _, _) => JlvmError.Type(op, TypeReason.Expected(Vector(ValueKind.Array), ValueKind.of(other), Role.Keys)).asLeft
      case _                   => JlvmError.Arity(op, ArgumentCount.Exactly(3), args.size).asLeft
    }
}

object CryptoOps {

  /** Loads the native backends (secp256k1, BLS12-381, gnark for BN254); fails on a platform without them. */
  def load[F[_]: Sync]: F[CryptoOps] =
    (Secp256k1.load[F], Bls12381.load[F], Bn254Engine.gnark[F]).mapN((s, b, e) => new CryptoOps(s, b, new ZkOps(e)))
}
