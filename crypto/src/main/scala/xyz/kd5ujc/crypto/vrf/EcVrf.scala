package xyz.kd5ujc.crypto.vrf

import java.nio.charset.StandardCharsets.US_ASCII

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.hash.{Hasher, Sha512}
import xyz.kd5ujc.crypto.curve25519.Point
import xyz.kd5ujc.crypto.signing.{KeyPair, PublicKeyOf, SigningKey, VerifyingKey}

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, EdwardsPoint, Scalar}
import scodec.bits.ByteVector

/** ECVRF-EDWARDS25519-SHA512-TAI (RFC 9381, suite 0x03). */
sealed trait EcVrfTai

/** ECVRF-EDWARDS25519-SHA512-ELL2 (RFC 9381, suite 0x04). */
sealed trait EcVrfEll2

/**
 * ECVRF-ED25519-SHA512-Elligator2 of draft-irtf-cfrg-vrf-03, as implemented by the Cardano fork of libsodium
 * (`crypto_vrf_ietfdraft03`): Cardano Praos VRF. It shares RFC 9381 ELL2's suite byte and sizes but not its hashing,
 * so the types keep the two apart.
 */
sealed trait EcVrfDraft03

sealed trait VrfError extends Product with Serializable

object VrfError {

  /** Wrong lengths: the input is not a key or proof of this suite at all. */
  final case class Malformed(what: String, reason: String) extends VrfError
  case object InvalidPublicKey extends VrfError
  case object SmallOrderPublicKey extends VrfError
  case object InvalidGamma extends VrfError
  case object NonCanonicalScalar extends VrfError
  case object ChallengeMismatch extends VrfError
  case object KeyMismatch extends VrfError

  /** Hash-to-curve found no point (TAI: all 256 counters failed, probability ~2^-256). */
  final case class HashToCurveFailed(reason: String) extends VrfError
}

/** An 80-byte proof `Gamma || c (16) || s (32)`; its fields are checked only by verification. */
final case class VrfProof[S] private[crypto] (bytes: ByteVector)

/** The 64-byte VRF output `beta`. It is only ever produced by proving or by a successful verification. */
final case class VrfOutput[S] private[crypto] (bytes: ByteVector)

/** Proving and verification for one suite. Proof-to-hash is never exposed for an unverified proof. */
trait Vrf[S] {
  def name: String
  def signingKey(bytes:   ByteVector): Either[VrfError, SigningKey[S]]
  def verifyingKey(bytes: ByteVector): Either[VrfError, VerifyingKey[S]]
  def proof(bytes:        ByteVector): Either[VrfError, VrfProof[S]]
  def publicKey(key:      SigningKey[S]): VerifyingKey[S]
  def prove(key:          SigningKey[S], alpha:   ByteVector): Either[VrfError, (VrfProof[S], VrfOutput[S])]
  def verify(key:         VerifyingKey[S], alpha: ByteVector, proof: VrfProof[S]): Either[VrfError, VrfOutput[S]]

  /** Verification over raw encodings: length errors are [[VrfError.Malformed]], everything else a typed rejection. */
  def verifyBytes(key: ByteVector, alpha: ByteVector, proof: ByteVector): Either[VrfError, VrfOutput[S]]
}

object Vrf {

  def apply[S](implicit V: Vrf[S]): Vrf[S] = V

  val ProofLength: Long = 80
  val OutputLength: Long = 64

  private val sha512 = Hasher[Sha512]
  private def hash(parts: ByteVector*): ByteVector = sha512.digest(parts.foldLeft(ByteVector.empty)(_ ++ _)).bytes

  /** The per-suite differences; everything else is shared. */
  private[crypto] trait Profile {
    def name: String
    def suite: Int
    def decode(b:       ByteVector): Option[Point]
    def hashToCurve(pk: ByteVector, alpha: ByteVector): Either[VrfError, Point]
    def challenge(pk:   ByteVector, h:     ByteVector, gamma: ByteVector, u: ByteVector, v: ByteVector): ByteVector

    /**
     * `U = s B - c Y` and `V = s H - c Gamma`, as the suite's reference evaluates them, with `c` and `s` the exact
     * integers (variable time: verification sees public data only).
     */
    def equation(p: Point, q: Point, c: Scalar, s: Scalar): Point

    /** [[equation]] with `q = B`, on the fixed basepoint table. */
    def equationBase(p: Point, c: Scalar, s: Scalar): Point
    def outputSuffix: ByteVector
  }

  /** RFC 9381 §5: strict RFC 8032 decoding, `Y` in the challenge, trailing zero octets, literal `s B - c Y`. */
  private abstract class Rfc9381Profile extends Profile {
    def decode(b: ByteVector): Option[Point] = Curve25519Maps.decodeStrict(b)
    def challenge(pk: ByteVector, h: ByteVector, gamma: ByteVector, u: ByteVector, v: ByteVector): ByteVector =
      hash(ByteVector(suite, 0x02), pk, h, gamma, u, v, ByteVector(0x00)).take(16)
    // c (below 2^128) times -p, exactly: not (L - c) p, which differs on mixed-order points
    def equation(p:     Point, q: Point, c:  Scalar, s: Scalar): Point = Point.doubleScalarMultiply(s, q, c, p.negate)
    def equationBase(p: Point, c: Scalar, s: Scalar): Point = Point.doubleScalarMultiplyBasepoint(c, p.negate, s)
    val outputSuffix: ByteVector = ByteVector(0x00)
  }

  private object TaiProfile extends Rfc9381Profile {
    val name = "ECVRF-EDWARDS25519-SHA512-TAI"
    val suite: Int = 0x03

    /** §5.4.1.1: the first counter whose hash decodes to a point; the point times the cofactor. */
    def hashToCurve(pk: ByteVector, alpha: ByteVector): Either[VrfError, Point] = {
      @tailrec def loop(ctr: Int): Either[VrfError, Point] =
        if (ctr > 255) VrfError.HashToCurveFailed("no counter in 0..255 gave a curve point").asLeft
        else
          Curve25519Maps.decodeStrict(hash(ByteVector(suite, 0x01), pk, alpha, ByteVector(ctr, 0x00)).take(32)) match {
            case Some(pt) => pt.multiplyByCofactor().asRight
            case None     => loop(ctr + 1)
          }
      loop(0)
    }
  }

  private object Ell2Profile extends Rfc9381Profile {
    val name = "ECVRF-EDWARDS25519-SHA512-ELL2"
    val suite: Int = 0x04
    private val dst = ByteVector("ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_".getBytes(US_ASCII)) ++ ByteVector(suite)

    /** §5.4.1.2: `encode_to_curve(PK || alpha)` with DST `"ECVRF_" || h2c_suite_ID_string || suite_string`. */
    def hashToCurve(pk: ByteVector, alpha: ByteVector): Either[VrfError, Point] =
      Curve25519Maps.encodeToCurveEll2(pk ++ alpha, dst).toRight(VrfError.HashToCurveFailed("Elligator 2 produced no point"))
  }

  /**
   * draft-03 as libsodium: canonical-`y` decoding that admits negative zero, no `Y` in the challenge, no trailing
   * zero octets, and `U = (-c mod L) Y + s B` (which differs from `s B - c Y` on mixed-order points).
   */
  private[crypto] object Draft03Profile extends Profile {
    val name = "ECVRF-ED25519-SHA512-Elligator2 (draft-03)"
    val suite: Int = 0x04
    def decode(b: ByteVector): Option[Point] = Curve25519Maps.decodeDraft03(b)
    def hashToCurve(pk: ByteVector, alpha: ByteVector): Either[VrfError, Point] =
      Curve25519Maps.hashToCurveDraft03(pk, alpha).toRight(VrfError.HashToCurveFailed("Elligator 2 produced no point"))
    def challenge(pk: ByteVector, h: ByteVector, gamma: ByteVector, u: ByteVector, v: ByteVector): ByteVector =
      hash(ByteVector(suite, 0x02), h, gamma, u, v).take(16)
    def equation(p: Point, q: Point, c: Scalar, s: Scalar): Point =
      Point.doubleScalarMultiply(Scalar.ZERO.subtract(c), p, s, q)
    def equationBase(p: Point, c: Scalar, s: Scalar): Point =
      Point.doubleScalarMultiplyBasepoint(Scalar.ZERO.subtract(c), p, s)
    val outputSuffix: ByteVector = ByteVector.empty
  }

  private[crypto] def scalar16(c: ByteVector): Scalar = Scalar.fromBytesModOrder(c.padRight(32).toArray)

  /** RFC 8032 key expansion: the clamped secret scalar and the nonce prefix. */
  private def expand(seed: ByteVector): (Scalar, ByteVector) = {
    val h = hash(seed)
    val a = h.take(32)
    (Scalar.fromBytesModOrder(a.update(0, (a(0) & 0xf8).toByte).update(31, ((a(31) & 0x7f) | 0x40).toByte).toArray), h.drop(32))
  }

  private def encoded(pt: EdwardsPoint): ByteVector = ByteVector.view(pt.compress().toByteArray)

  private def publicFromSeed(seed: ByteVector): ByteVector = encoded(Constants.ED25519_BASEPOINT_TABLE.multiply(expand(seed)._1))

  private class Impl[S](profile: Profile, seedOf: SigningKey[S] => ByteVector, val signingKeyLength: Long) extends Vrf[S] {
    val name: String = profile.name

    def signingKey(bytes: ByteVector): Either[VrfError, SigningKey[S]] =
      Either
        .cond(
          bytes.size == signingKeyLength,
          bytes,
          VrfError.Malformed(s"$name signing key", s"expected $signingKeyLength bytes, got ${bytes.size}")
        )
        .flatMap { b =>
          // draft-03 keys are libsodium's `seed || pk`: the stored public key must be the seed's
          Either.cond(b.size == 32L || publicFromSeed(b.take(32)) == b.drop(32), new SigningKey[S](b), VrfError.KeyMismatch)
        }

    private def decodeKey(bytes: ByteVector): Either[VrfError, Point] =
      for {
        _  <- Either.cond(bytes.size == 32L, (), VrfError.Malformed(s"$name public key", s"expected 32 bytes, got ${bytes.size}"))
        pt <- profile.decode(bytes).toRight(VrfError.InvalidPublicKey)
        _  <- Either.cond(!Curve25519Maps.isSmallOrder(pt), (), VrfError.SmallOrderPublicKey)
      } yield pt

    def verifyingKey(bytes: ByteVector): Either[VrfError, VerifyingKey[S]] = decodeKey(bytes).as(VerifyingKey[S](bytes))

    def proof(bytes: ByteVector): Either[VrfError, VrfProof[S]] =
      Either.cond(
        bytes.size == ProofLength,
        VrfProof[S](bytes),
        VrfError.Malformed(s"$name proof", s"expected $ProofLength bytes, got ${bytes.size}")
      )

    def publicKey(key: SigningKey[S]): VerifyingKey[S] = VerifyingKey[S](publicFromSeed(seedOf(key)))

    /** `beta` from the encoding of `[8]Gamma`. */
    private def output(gamma8: ByteVector): VrfOutput[S] = VrfOutput[S](hash(ByteVector(profile.suite, 0x03), gamma8, profile.outputSuffix))

    /**
     * Secret scalars meet only elisabeth's constant-time multiplications; hash-to-curve sees public inputs only, and its
     * point is carried to elisabeth through its encoding.
     */
    def prove(key: SigningKey[S], alpha: ByteVector): Either[VrfError, (VrfProof[S], VrfOutput[S])] = {
      val (x, prefix) = expand(seedOf(key))
      val pk = publicFromSeed(seedOf(key))
      for {
        point <- profile.hashToCurve(pk, alpha)
        hb = Curve25519Maps.bytes(point)
        h <- Either
          .catchNonFatal(new CompressedEdwardsY(hb.toArray).decompress())
          .leftMap(_ => VrfError.HashToCurveFailed("the point's encoding did not decode"))
      } yield {
        val gamma = h.multiply(x)
        val gb = encoded(gamma)
        val k = Scalar.fromBytesModOrderWide(hash(prefix, hb).toArray)
        val c = profile.challenge(pk, hb, gb, encoded(Constants.ED25519_BASEPOINT_TABLE.multiply(k)), encoded(h.multiply(k)))
        val s = scalar16(c).multiplyAndAdd(x, k)
        (VrfProof[S](gb ++ c ++ ByteVector.view(s.toByteArray)), output(encoded(gamma.multiplyByCofactor())))
      }
    }

    def verify(key: VerifyingKey[S], alpha: ByteVector, proof: VrfProof[S]): Either[VrfError, VrfOutput[S]] =
      verifyBytes(key.bytes, alpha, proof.bytes)

    def verifyBytes(key: ByteVector, alpha: ByteVector, proof: ByteVector): Either[VrfError, VrfOutput[S]] =
      for {
        _ <- this.proof(proof)
        _ <- Either.cond(key.size == 32L, (), VrfError.Malformed(s"$name public key", s"expected 32 bytes, got ${key.size}"))
        y <- decodeKey(key)
        gb = proof.take(32)
        gamma <- profile.decode(gb).toRight(VrfError.InvalidGamma)
        c = proof.slice(32, 48)
        sb = proof.drop(48)
        _ <- Either.cond(Curve25519Maps.le(sb) < Curve25519Maps.L, (), VrfError.NonCanonicalScalar)
        s = Scalar.fromBytesModOrder(sb.toArray)
        // a decoded key's canonical `y` is its own: the only `x = 0` points (negative zero) are small-order, refused
        h <- profile.hashToCurve(key, alpha)
        u = profile.equationBase(y, scalar16(c), s)
        v = profile.equation(gamma, h, scalar16(c), s)
        (hb, ub, vb, gamma8) = Point.compressBatch(h, u, v, gamma.multiplyByCofactor())
        _ <- Either.cond(profile.challenge(key, hb, gb, ub, vb) == c, (), VrfError.ChallengeMismatch)
      } yield output(gamma8)
  }

  implicit val tai: Vrf[EcVrfTai] = new Impl[EcVrfTai](TaiProfile, _.bytes, 32)
  implicit val ell2: Vrf[EcVrfEll2] = new Impl[EcVrfEll2](Ell2Profile, _.bytes, 32)

  /** Cardano keys: 64-byte `seed || pk` (the `vrf.skey` payload). */
  implicit val draft03: Vrf[EcVrfDraft03] = new Impl[EcVrfDraft03](Draft03Profile, _.bytes.take(32), 64)

  implicit def publicKeyOf[S](implicit V: Vrf[S]): PublicKeyOf[S] = (key: SigningKey[S]) => V.publicKey(key)

  /** Key pair from a 32-byte seed (draft-03 keys become `seed || pk`). */
  def fromSeed[S](seed: ByteVector)(implicit V: Vrf[S]): Either[VrfError, KeyPair[S]] =
    Either
      .cond(seed.size == 32L, seed, VrfError.Malformed("VRF seed", s"expected 32 bytes, got ${seed.size}"))
      .flatMap(s => V.signingKey(s).orElse(V.signingKey(s ++ publicFromSeed(s))))
      .map(sk => KeyPair(sk, V.publicKey(sk)))
}
