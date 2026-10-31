package xyz.kd5ujc.crypto.signing

import java.math.BigInteger

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Hasher, Sha256}

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECPoint, FixedPointCombMultiplier}
import scodec.bits.ByteVector

/**
 * NIST P-256 (secp256r1) ECDSA over SHA-256 with RFC 6979 deterministic nonces, compact 64-byte `r || s`, and low-S
 * only (the high-S twin is rejected, as for secp256k1). Verifying keys are 33-byte compressed SEC1 points. Signer
 * objects are built per call and never escape.
 */
sealed trait P256Ecdsa

object P256Ecdsa {

  private val curve = CustomNamedCurves.getByName("secp256r1")
  private val domain = new ECDomainParameters(curve.getCurve, curve.getG, curve.getN, curve.getH)
  private val n: BigInt = BigInt(curve.getN)
  private val sha256 = Hasher[Sha256]

  private def scalar(b:  ByteVector): BigInt = BigInt(1, b.toArray)
  private def fixed32(i: BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32)
  private def point(b: ByteVector): Either[Throwable, ECPoint] =
    Either.catchNonFatal(curve.getCurve.decodePoint(b.toArray)).filterOrElse(_.isValid, new IllegalArgumentException("not a valid point"))

  implicit val codec: SchemeCodec[P256Ecdsa] = new SchemeCodec[P256Ecdsa] {
    val name: String = "P-256-ECDSA"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[P256Ecdsa]] =
      SchemeCodec
        .sized("P-256 signing key", 32, bytes)
        .flatMap(b =>
          Either.cond(
            scalar(b) >= 1 && scalar(b) < n,
            new SigningKey[P256Ecdsa](b),
            CryptoError.InvalidKey("P-256 secret key must be a scalar in [1, n)")
          )
        )

    /** 33-byte compressed SEC1 (tag 02 or 03), on the curve: the tag is checked here, not left to the backend. */
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[P256Ecdsa]] =
      SchemeCodec
        .sized("P-256 verifying key", 33, bytes)
        .flatMap(b =>
          Option
            .when(SchemeCodec.compressedTag(b))(b)
            .flatMap(point(_).toOption)
            .toRight(CryptoError.InvalidKey("P-256 verifying key is not a valid point"))
            .as(VerifyingKey[P256Ecdsa](b))
        )
    def signature(bytes: ByteVector): Either[CryptoError, Signature[P256Ecdsa]] =
      if (bytes.size != 64L) CryptoError.InvalidEncoding("P-256 signature", s"expected 64 bytes, got ${bytes.size}").asLeft
      else {
        val (r, s) = (scalar(bytes.take(32)), scalar(bytes.drop(32)))
        Either.cond(
          r >= 1 && r < n && s >= 1 && s <= n / 2,
          Signature[P256Ecdsa](bytes),
          CryptoError.InvalidEncoding("P-256 signature", "r and s must be in range and s must be low (canonical)")
        )
      }
  }

  implicit val publicKeyOf: PublicKeyOf[P256Ecdsa] = (key: SigningKey[P256Ecdsa]) =>
    VerifyingKey[P256Ecdsa](
      ByteVector.view(new FixedPointCombMultiplier().multiply(curve.getG, scalar(key.bytes).bigInteger).normalize().getEncoded(true))
    )

  implicit val signer: Signer[P256Ecdsa] = (key: SigningKey[P256Ecdsa], message: ByteVector) => {
    val s = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    s.init(true, new ECPrivateKeyParameters(scalar(key.bytes).bigInteger, domain))
    val rs = s.generateSignature(sha256.digest(message).bytes.toArray)
    val (r, sv) = (BigInt(rs(0)), BigInt(rs(1)))
    Signature[P256Ecdsa](fixed32(r) ++ fixed32(if (sv > n / 2) n - sv else sv))
  }

  implicit val verifier: Verifier[P256Ecdsa] = (key: VerifyingKey[P256Ecdsa], message: ByteVector, signature: Signature[P256Ecdsa]) =>
    codec.signature(signature.bytes).isRight && point(key.bytes).exists { q =>
      val v = new ECDSASigner()
      v.init(false, new ECPublicKeyParameters(q, domain))
      v.verifySignature(
        sha256.digest(message).bytes.toArray,
        new BigInteger(1, signature.bytes.take(32).toArray),
        new BigInteger(1, signature.bytes.drop(32).toArray)
      )
    }

  implicit val seedDerivation: SeedDerivation[P256Ecdsa] = new SeedDerivation[P256Ecdsa] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[P256Ecdsa]] =
      codec.signingKey(seed).map(sk => KeyPair(sk, publicKeyOf.verifyingKey(sk)))
  }
}
