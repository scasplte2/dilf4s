package xyz.kd5ujc.jlvm

import cats.effect.Sync
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector

/**
 * The cryptographic operations, over the dilf4s backends. Two backends are native libraries, loaded as an effect at the
 * edge ([[CryptoOps.load]]): every node evaluates with the same complete set, so no operation's availability depends on
 * the classpath.
 *
 * Malformed input (a wrong length, a key that is not a curve point) is a [[JlvmError.Crypto]] error; a well-formed
 * signature that does not verify is `false`.
 */
final class CryptoOps private (secp: Secp256k1, bls: Bls12381) {

  type Result = Either[JlvmError, CanonicalValue]

  /** `None` for operations not implemented here. */
  def apply(op: Op, args: Vector[CanonicalValue]): Option[Result] =
    op match {
      case Op.Ed25519Verify      => Option(signature(op, args)(Ed25519.codec, Ed25519.verifier))
      case Op.Secp256k1Verify    => Option(signature(op, args)(secp.ecdsaCodec, secp.ecdsaVerifier))
      case Op.BlsVerify          => Option(signature(op, args)(bls.codec, bls.verifier))
      case Op.Bip340Verify       => Option(bip340(op, args))
      case Op.BlsAggregateVerify => Option(blsAggregate(op, args))
      case _                     => Option.empty
    }

  private def crypto[A](op: Op)(e: CryptoError): JlvmError = JlvmError.Crypto(op.wire, e.toString)

  private def bytes(op: Op, v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case other     => JlvmError.Type(op.wire, s"expected bytes, got ${Values.kind(other)}").asLeft
    }

  private def triple(op: Op, args: Vector[CanonicalValue]): Either[JlvmError, (ByteVector, ByteVector, ByteVector)] =
    args match {
      case Vector(k, m, s) => (bytes(op, k), bytes(op, m), bytes(op, s)).tupled
      case _               => JlvmError.Arity(op.wire, s"takes [key, message, signature], got ${args.size} arguments").asLeft
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
   * `[[key, ...], message, signature]`: one message signed by every key (fast aggregate verification). The keys must
   * carry proofs of possession, checked by whoever admitted them: without that, a rogue key can forge an aggregate.
   */
  private def blsAggregate(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(CArray(ks), m, s) =>
        (ks.traverse(bytes(op, _)), bytes(op, m), bytes(op, s)).tupled.flatMap {
          case (keys, msg, sig) =>
            (keys.traverse(bls.codec.verifyingKey), bls.codec.signature(sig)).tupled
              .leftMap(crypto(op))
              .map { case (vks, signature) => Values.bool(bls.fastAggregateVerify(vks.toList, msg, signature)) }
        }
      case Vector(other, _, _) => JlvmError.Type(op.wire, s"expected an array of keys, got ${Values.kind(other)}").asLeft
      case _                   => JlvmError.Arity(op.wire, s"takes [[key, ...], message, signature], got ${args.size} arguments").asLeft
    }
}

object CryptoOps {

  /** Loads the native backends (secp256k1, BLS12-381); fails on a platform without them. */
  def load[F[_]: Sync]: F[CryptoOps] = (Secp256k1.load[F], Bls12381.load[F]).mapN(new CryptoOps(_, _))
}
