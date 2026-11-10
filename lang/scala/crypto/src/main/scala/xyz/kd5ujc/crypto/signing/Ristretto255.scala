package xyz.kd5ujc.crypto.signing

import java.nio.charset.StandardCharsets.US_ASCII

import cats.Functor
import cats.effect.std.SecureRandom
import cats.syntax.all._

import xyz.kd5ujc.crypto.signing.CryptoError.{DecodeFailure, EncodingReason, KeyReason, Scheme, Subject}

import cafe.cryptography.curve25519.{CompressedRistretto, Constants, RistrettoElement, Scalar}
import org.bouncycastle.crypto.digests.SHA512Digest
import scodec.bits.ByteVector

/**
 * Schnorr signatures over ristretto255 with the FROST(ristretto255, SHA-512) ciphersuite (RFC 9591): the single-signer
 * form of FROST, so a threshold group signature verifies under the same rule.
 *
 *   - secret key: a canonical, non-zero scalar; verifying key `PK = [sk]G` (32-byte compressed ristretto255)
 *   - nonce: `k = H3(random(32) || sk)` (RFC 9591 `nonce_generate`); signing is randomised
 *   - challenge: `c = H2(R || PK || msg)`; signature `R || z` with `z = k + c sk`
 *   - verification: `[z]G == R + [c]PK`, rejecting identity elements and non-canonical encodings
 *
 * Secret-scalar work runs on elisabeth's constant-time generator table and scalar arithmetic.
 */
sealed trait Ristretto255Schnorr

object Ristretto255Schnorr {

  val RandomnessLength: Int = 32
  private val contextString = "FROST-RISTRETTO255-SHA512-v1"

  private def hashToScalar(label: String, parts: ByteVector*): Scalar = {
    val d = new SHA512Digest()
    val out = new Array[Byte](64)
    (ByteVector(s"$contextString$label".getBytes(US_ASCII)) +: parts).foreach(p => d.update(p.toArray, 0, p.size.toInt))
    d.doFinal(out, 0)
    Scalar.fromBytesModOrderWide(out)
  }

  private def canonicalScalar(b: ByteVector): Either[DecodeFailure, Scalar] =
    Either.catchNonFatal(Scalar.fromCanonicalBytes(b.toArray)).leftMap(DecodeFailure.Foreign(_))

  /** RFC 9591 `DeserializeElement`: canonical encoding of a non-identity element. */
  private def element(b: ByteVector): Either[DecodeFailure, RistrettoElement] =
    Either
      .catchNonFatal(new CompressedRistretto(b.toArray).decompress())
      .leftMap(DecodeFailure.Foreign(_))
      .filterOrElse(e => !e.ctEquals(RistrettoElement.IDENTITY).equals(1), DecodeFailure.Identity)

  implicit val codec: SchemeCodec[Ristretto255Schnorr] = new SchemeCodec[Ristretto255Schnorr] {
    val name: String = "ristretto255-Schnorr"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Ristretto255Schnorr]] =
      SchemeCodec
        .sized(Subject.SigningKey(Scheme.Ristretto255), 32, bytes)
        .flatMap(b =>
          canonicalScalar(b)
            .filterOrElse(s => !s.equals(Scalar.ZERO), DecodeFailure.ZeroScalar)
            .leftMap(e => CryptoError.InvalidKey(KeyReason.ScalarDecode(Scheme.Ristretto255, e)))
            .as(new SigningKey[Ristretto255Schnorr](b))
        )
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Ristretto255Schnorr]] =
      if (bytes.size != 32L)
        CryptoError.InvalidEncoding(Subject.VerifyingKey(Scheme.Ristretto255), EncodingReason.Length(32, bytes.size)).asLeft
      else
        element(bytes).bimap(
          e => CryptoError.InvalidKey(KeyReason.Decode(Scheme.Ristretto255, e)),
          _ => VerifyingKey(bytes)
        )
    def signature(bytes: ByteVector): Either[CryptoError, Signature[Ristretto255Schnorr]] =
      for {
        _ <- Either.cond(
          bytes.size == 64L,
          (),
          CryptoError.InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoLength(bytes.size))
        )
        _ <- element(bytes.take(32)).leftMap(e =>
          CryptoError.InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoElement(e))
        )
        _ <- canonicalScalar(bytes.drop(32)).leftMap(e =>
          CryptoError.InvalidEncoding(Subject.Signature(Scheme.Ristretto255), EncodingReason.RistrettoScalar(e))
        )
      } yield Signature[Ristretto255Schnorr](bytes)
  }

  private def secret(key: SigningKey[Ristretto255Schnorr]): Scalar = Scalar.fromBytesModOrder(key.bytes.toArray)

  implicit val publicKeyOf: PublicKeyOf[Ristretto255Schnorr] = (key: SigningKey[Ristretto255Schnorr]) =>
    VerifyingKey(ByteVector.view(Constants.RISTRETTO_GENERATOR_TABLE.multiply(secret(key)).compress().toByteArray))

  private def signWith(
    key:        SigningKey[Ristretto255Schnorr],
    message:    ByteVector,
    randomness: ByteVector
  ): Signature[Ristretto255Schnorr] = {
    val k = hashToScalar("nonce", randomness, key.bytes)
    val r = ByteVector.view(Constants.RISTRETTO_GENERATOR_TABLE.multiply(k).compress().toByteArray)
    val c = hashToScalar("chal", r, publicKeyOf.verifyingKey(key).bytes, message)
    Signature(r ++ ByteVector.view(c.multiplyAndAdd(secret(key), k).toByteArray))
  }

  /** Signing with caller-supplied nonce randomness (32 bytes): pure, for known-answer tests and external RNGs. */
  def signWithRandomness(
    key:        SigningKey[Ristretto255Schnorr],
    message:    ByteVector,
    randomness: ByteVector
  ): Either[CryptoError, Signature[Ristretto255Schnorr]] =
    Either.cond(
      randomness.size == RandomnessLength.toLong,
      signWith(key, message, randomness),
      CryptoError.InvalidEncoding(Subject.NonceRandomness, EncodingReason.Length(RandomnessLength.toLong, randomness.size))
    )

  def signer[F[_]: Functor](implicit R: SecureRandom[F]): RandomizedSigner[F, Ristretto255Schnorr] =
    (key: SigningKey[Ristretto255Schnorr], message: ByteVector) =>
      R.nextBytes(RandomnessLength).map(rand => signWith(key, message, ByteVector.view(rand)))

  implicit val verifier: Verifier[Ristretto255Schnorr] =
    (key: VerifyingKey[Ristretto255Schnorr], message: ByteVector, signature: Signature[Ristretto255Schnorr]) =>
      signature.bytes.size == 64L && (for {
        pk <- element(key.bytes)
        r  <- element(signature.bytes.take(32))
        z  <- canonicalScalar(signature.bytes.drop(32))
        c = hashToScalar("chal", signature.bytes.take(32), key.bytes, message)
      } yield Constants.RISTRETTO_GENERATOR_TABLE.multiply(z).ctEquals(r.add(pk.multiply(c))).equals(1)).getOrElse(false)

  /** Seed (32 bytes) -> secret scalar: `SHA-512(seed) mod L`, rejecting the (negligible) zero scalar. */
  implicit val seedDerivation: SeedDerivation[Ristretto255Schnorr] = new SeedDerivation[Ristretto255Schnorr] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Ristretto255Schnorr]] = {
      val d = new SHA512Digest()
      val out = new Array[Byte](64)
      d.update(seed.toArray, 0, seed.size.toInt)
      d.doFinal(out, 0)
      Either
        .cond(
          seed.size == seedLength,
          seed,
          CryptoError.InvalidEncoding(Subject.RistrettoSeed, EncodingReason.Length(seedLength, seed.size))
        )
        .flatMap(_ => codec.signingKey(ByteVector.view(Scalar.fromBytesModOrderWide(out).toByteArray)))
        .map(sk => KeyPair(sk, publicKeyOf.verifyingKey(sk)))
    }
  }
}
