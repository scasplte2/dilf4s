package xyz.kd5ujc.crypto.signing

import cats.syntax.all._

import cafe.cryptography.curve25519.{CompressedEdwardsY, EdwardsPoint, Scalar}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.math.ec.rfc8032.{Ed25519 => Rfc8032}
import scodec.bits.ByteVector

/**
 * Ed25519 (pure, deterministic). Signing and key derivation use BouncyCastle's constant-time RFC 8032 implementation;
 * each call allocates its own buffers, which never escape.
 *
 * **Verification follows ZIP-215** (the default [[Ed25519.verifier]]): `A` and `R` may be non-canonical encodings but
 * must decode to curve points, `S` must be canonical, and the check is cofactored, `[8]([S]B - R - [k]A) = 0`. ZIP-215
 * fully specifies validity, so every implementation agrees on every signature (consensus-safe, batch-compatible).
 * BouncyCastle's RFC 8032 verifier, which additionally requires canonical encodings of `A` and `R`, is available as
 * [[Ed25519.strictVerifier]].
 */
sealed trait Ed25519

object Ed25519 {

  val SeedLength: Long = 32
  val KeyLength: Long = 32
  val SignatureLength: Long = 64

  /** Verifying keys must decode to a curve point under ZIP-215 rules (non-canonical encodings allowed). */
  implicit val codec: SchemeCodec[Ed25519] =
    SchemeCodec.fixed[Ed25519]("Ed25519", KeyLength, KeyLength, SignatureLength, vk => Zip215.decode(vk).isRight)

  implicit val publicKeyOf: PublicKeyOf[Ed25519] = (key: SigningKey[Ed25519]) => {
    val pk = new Array[Byte](Rfc8032.PUBLIC_KEY_SIZE)
    Rfc8032.generatePublicKey(key.bytes.toArray, 0, pk, 0)
    VerifyingKey[Ed25519](ByteVector.view(pk))
  }

  implicit val signer: Signer[Ed25519] = (key: SigningKey[Ed25519], message: ByteVector) => {
    val sig = new Array[Byte](Rfc8032.SIGNATURE_SIZE)
    val m = message.toArray
    Rfc8032.sign(key.bytes.toArray, 0, m, 0, m.length, sig, 0)
    Signature[Ed25519](ByteVector.view(sig))
  }

  /** ZIP-215 verification (the consensus rule). */
  implicit val verifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) =>
    Zip215.verify(key.bytes, message, signature.bytes)

  /** BouncyCastle's RFC 8032 verification: rejects non-canonical encodings of `A` and `R` that ZIP-215 accepts. */
  val strictVerifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) => {
    val m = message.toArray
    Rfc8032.verify(signature.bytes.toArray, 0, key.bytes.toArray, 0, m, 0, m.length)
  }

  /**
   * libsodium's `crypto_sign_ed25519_verify_detached` (as used by Cardano, e.g. for KES leaves): canonical `S`;
   * a canonical-`y`, non-small-order `A`; and the **cofactorless** check that `[S]B - [k]A` encodes exactly to `R` and
   * is not of small order. Stricter than ZIP-215; use it only where libsodium compatibility is the rule.
   */
  val libsodiumVerifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) =>
    Zip215.verifyLibsodium(key.bytes, message, signature.bytes)

  /** The RFC 8032 secret key is the 32-byte seed itself. */
  implicit val seedDerivation: SeedDerivation[Ed25519] = new SeedDerivation[Ed25519] {
    val seedLength: Long = SeedLength
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Ed25519]] =
      codec.signingKey(seed).map(sk => KeyPair(sk, publicKeyOf.verifyingKey(sk)))
  }

  /** ZIP-215 decoding and the cofactored verification equation; touches only public data. */
  private[signing] object Zip215 {

    /** Decompression accepting non-canonical `y` (and `-0`), rejecting non-curve encodings. */
    def decode(bytes: ByteVector): Either[Throwable, EdwardsPoint] =
      Either.catchNonFatal(new CompressedEdwardsY(bytes.toArray).decompress())

    private def canonicalScalar(bytes: ByteVector): Either[Throwable, Scalar] =
      Either.catchNonFatal(Scalar.fromCanonicalBytes(bytes.toArray))

    private def sha512(bytes: ByteVector): Array[Byte] = {
      val d = new SHA512Digest()
      val in = bytes.toArray
      val out = new Array[Byte](64)
      d.update(in, 0, in.length)
      d.doFinal(out, 0)
      out
    }

    private val p: BigInt = BigInt(2).pow(255) - 19
    private def canonicalY(b: ByteVector): Boolean = BigInt(1, b.update(31, (b(31) & 0x7f).toByte).reverse.toArray) < p

    def verifyLibsodium(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
      signature.size == SignatureLength && key.size == KeyLength && canonicalY(key) && (for {
        a <- decode(key).filterOrElse(!_.multiplyByCofactor().isIdentity, new IllegalArgumentException("small-order key"))
        s <- canonicalScalar(signature.drop(32))
        k = Scalar.fromBytesModOrderWide(sha512(signature.take(32) ++ key ++ message))
        r = EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(k, a.negate(), s)
      } yield ByteVector.view(r.compress().toByteArray) == signature.take(32) && !r.multiplyByCofactor().isIdentity).getOrElse(false)

    def verify(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
      signature.size == SignatureLength && key.size == KeyLength && (for {
        a <- decode(key)
        r <- decode(signature.take(32))
        s <- canonicalScalar(signature.drop(32))
        k = Scalar.fromBytesModOrderWide(sha512(signature.take(32) ++ key ++ message))
      } yield EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(Scalar.ZERO.subtract(k), a, s).subtract(r).multiplyByCofactor().isIdentity)
        .getOrElse(false)
  }
}
