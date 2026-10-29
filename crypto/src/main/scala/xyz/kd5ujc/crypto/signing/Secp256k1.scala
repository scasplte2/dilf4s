package xyz.kd5ujc.crypto.signing

import cats.effect.Sync
import cats.effect.std.SecureRandom
import cats.syntax.all._
import cats.{Functor, Monad}

import xyz.kd5ujc.core.hash.{DomainTag, Hasher, Sha256, TaggedHash}

import fr.acinq.secp256k1.{Secp256k1 => Native}
import scodec.bits.ByteVector

/** secp256k1 ECDSA over SHA-256(message): RFC 6979 deterministic nonces, compact 64-byte `r || s`, low-S only. */
sealed trait Secp256k1Ecdsa

/**
 * BIP-340 Schnorr signatures over secp256k1 with 32-byte x-only verifying keys. The signed 32-byte message is the
 * tagged hash `dilf4s/sig/bip340/v1` of the caller's message (BIP-340's own recommendation for arbitrary messages), so
 * signing is total for every message length. Raw 32-byte BIP-340 is available as `schnorrSignRaw` / `schnorrVerifyRaw`.
 */
sealed trait Secp256k1Schnorr

/**
 * bitcoin-core's constant-time libsecp256k1, through ACINQ's JNI bindings. Loading the native library is an effect, so
 * the backend is acquired in `F` with [[Secp256k1.load]]; its instances are then pure. Every native call that can
 * reject input is captured as a typed [[CryptoError]], never thrown.
 */
final class Secp256k1 private (native: Native) {

  private def call[A](operation: String)(thunk: => A): Either[CryptoError, A] =
    Either.catchNonFatal(thunk).leftMap(e => CryptoError.BackendFailure(s"secp256k1 $operation", Option(e.getMessage).getOrElse(e.getClass.getName)))

  private val sha256 = Hasher[Sha256]
  private val schnorrTag = TaggedHash[Sha256](DomainTag.literal(Secp256k1.SchnorrTag))
  private val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  private def validScalar(b: ByteVector): Boolean = b.size == 32L && call("secKeyVerify")(native.secKeyVerify(b.toArray)).getOrElse(false)

  // ------------------------------------------------------------------ ECDSA

  val ecdsaCodec: SchemeCodec[Secp256k1Ecdsa] = new SchemeCodec[Secp256k1Ecdsa] {
    val name: String = "secp256k1-ECDSA"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Secp256k1Ecdsa]] =
      Either.cond(validScalar(bytes), new SigningKey[Secp256k1Ecdsa](bytes), CryptoError.InvalidKey("secp256k1 secret key must be a scalar in [1, n)"))
    /** 33-byte compressed SEC1 point, checked on the curve. */
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Secp256k1Ecdsa]] =
      if (bytes.size != 33L) CryptoError.InvalidEncoding("secp256k1 verifying key", s"expected 33 compressed bytes, got ${bytes.size}").asLeft
      else call("pubkeyParse")(native.pubkeyParse(bytes.toArray)).as(VerifyingKey[Secp256k1Ecdsa](bytes))
    /** 64-byte compact `r || s` with `1 <= r < n` and `1 <= s <= n/2`: the high-S twin is rejected (malleability). */
    def signature(bytes: ByteVector): Either[CryptoError, Signature[Secp256k1Ecdsa]] =
      if (bytes.size != 64L) CryptoError.InvalidEncoding("secp256k1 signature", s"expected 64 bytes, got ${bytes.size}").asLeft
      else {
        val (r, s) = (BigInt(1, bytes.take(32).toArray), BigInt(1, bytes.drop(32).toArray))
        Either.cond(
          r >= 1 && r < n && s >= 1 && s <= n / 2,
          Signature[Secp256k1Ecdsa](bytes),
          CryptoError.InvalidEncoding("secp256k1 signature", "r and s must be in range and s must be low (canonical)")
        )
      }
  }

  val ecdsaPublicKeyOf: PublicKeyOf[Secp256k1Ecdsa] = (key: SigningKey[Secp256k1Ecdsa]) =>
    VerifyingKey[Secp256k1Ecdsa](ByteVector.view(native.pubKeyCompress(native.pubkeyCreate(key.bytes.toArray))))

  val ecdsaSigner: Signer[Secp256k1Ecdsa] = (key: SigningKey[Secp256k1Ecdsa], message: ByteVector) =>
    Signature[Secp256k1Ecdsa](ByteVector.view(Secp256k1.Ffi.signRfc6979(native, sha256.digest(message).bytes.toArray, key.bytes.toArray)))

  val ecdsaVerifier: Verifier[Secp256k1Ecdsa] = (key: VerifyingKey[Secp256k1Ecdsa], message: ByteVector, signature: Signature[Secp256k1Ecdsa]) =>
    call("verify")(native.verify(signature.bytes.toArray, sha256.digest(message).bytes.toArray, key.bytes.toArray)).getOrElse(false)

  val ecdsaSeedDerivation: SeedDerivation[Secp256k1Ecdsa] = new SeedDerivation[Secp256k1Ecdsa] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Secp256k1Ecdsa]] =
      ecdsaCodec.signingKey(seed).map(sk => KeyPair(sk, ecdsaPublicKeyOf.verifyingKey(sk)))
  }

  // ------------------------------------------------------------------ BIP-340 Schnorr

  val schnorrCodec: SchemeCodec[Secp256k1Schnorr] = new SchemeCodec[Secp256k1Schnorr] {
    val name: String = "secp256k1-BIP340"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Secp256k1Schnorr]] =
      Either.cond(validScalar(bytes), new SigningKey[Secp256k1Schnorr](bytes), CryptoError.InvalidKey("secp256k1 secret key must be a scalar in [1, n)"))
    /** 32-byte x-only key; it must lift to a curve point (even y). */
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Secp256k1Schnorr]] =
      if (bytes.size != 32L) CryptoError.InvalidEncoding("BIP-340 verifying key", s"expected 32 bytes, got ${bytes.size}").asLeft
      else call("pubkeyParse")(native.pubkeyParse((ByteVector(0x02) ++ bytes).toArray)).as(VerifyingKey[Secp256k1Schnorr](bytes))
    def signature(bytes: ByteVector): Either[CryptoError, Signature[Secp256k1Schnorr]] =
      Either.cond(bytes.size == 64L, Signature[Secp256k1Schnorr](bytes), CryptoError.InvalidEncoding("BIP-340 signature", s"expected 64 bytes, got ${bytes.size}"))
  }

  val schnorrPublicKeyOf: PublicKeyOf[Secp256k1Schnorr] = (key: SigningKey[Secp256k1Schnorr]) =>
    VerifyingKey[Secp256k1Schnorr](ByteVector.view(native.pubKeyCompress(native.pubkeyCreate(key.bytes.toArray))).drop(1))

  /** Fresh auxiliary randomness per signature, as BIP-340 recommends. Total: the signed digest is always 32 bytes. */
  def schnorrSigner[F[_]: Functor](implicit R: SecureRandom[F]): RandomizedSigner[F, Secp256k1Schnorr] =
    (key: SigningKey[Secp256k1Schnorr], message: ByteVector) =>
      R.nextBytes(32).map(aux => Signature[Secp256k1Schnorr](ByteVector.view(native.signSchnorr(schnorrTag(message).bytes.toArray, key.bytes.toArray, aux))))

  val schnorrVerifier: Verifier[Secp256k1Schnorr] = (key: VerifyingKey[Secp256k1Schnorr], message: ByteVector, signature: Signature[Secp256k1Schnorr]) =>
    schnorrVerifyRaw(key, schnorrTag(message).bytes, signature)

  /** Raw BIP-340 over an exact 32-byte message (interoperability, test vectors); `aux` is the 32-byte auxiliary randomness. */
  def schnorrSignRaw(key: SigningKey[Secp256k1Schnorr], message32: ByteVector, aux: ByteVector): Either[CryptoError, Signature[Secp256k1Schnorr]] =
    if (message32.size != 32L || aux.size != 32L) CryptoError.InvalidEncoding("BIP-340 raw input", "message and auxiliary randomness must be 32 bytes").asLeft
    else call("signSchnorr")(native.signSchnorr(message32.toArray, key.bytes.toArray, aux.toArray)).map(b => Signature[Secp256k1Schnorr](ByteVector.view(b)))

  def schnorrVerifyRaw(key: VerifyingKey[Secp256k1Schnorr], message32: ByteVector, signature: Signature[Secp256k1Schnorr]): Boolean =
    message32.size == 32L && call("verifySchnorr")(native.verifySchnorr(signature.bytes.toArray, message32.toArray, key.bytes.toArray)).getOrElse(false)

  val schnorrSeedDerivation: SeedDerivation[Secp256k1Schnorr] = new SeedDerivation[Secp256k1Schnorr] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Secp256k1Schnorr]] =
      schnorrCodec.signingKey(seed).map(sk => KeyPair(sk, schnorrPublicKeyOf.verifyingKey(sk)))
  }
}

object Secp256k1 {

  private[kd5ujc] val SchnorrTag = "dilf4s/sig/bip340/v1"
  private[kd5ujc] val Tags: List[String] = List(SchnorrTag)

  /** Loads the native library (an effect, and it can fail on an unsupported platform). */
  def load[F[_]: Sync]: F[Secp256k1] = Sync[F].delay(new Secp256k1(Native.get()))

  /** Exposes the backend's instances implicitly for code that summons capabilities by type. */
  final class Instances(val backend: Secp256k1) {
    implicit val ecdsaCodec: SchemeCodec[Secp256k1Ecdsa] = backend.ecdsaCodec
    implicit val ecdsaSigner: Signer[Secp256k1Ecdsa] = backend.ecdsaSigner
    implicit val ecdsaVerifier: Verifier[Secp256k1Ecdsa] = backend.ecdsaVerifier
    implicit val ecdsaPublicKeyOf: PublicKeyOf[Secp256k1Ecdsa] = backend.ecdsaPublicKeyOf
    implicit val ecdsaSeedDerivation: SeedDerivation[Secp256k1Ecdsa] = backend.ecdsaSeedDerivation
    implicit val schnorrCodec: SchemeCodec[Secp256k1Schnorr] = backend.schnorrCodec
    implicit val schnorrVerifier: Verifier[Secp256k1Schnorr] = backend.schnorrVerifier
    implicit val schnorrPublicKeyOf: PublicKeyOf[Secp256k1Schnorr] = backend.schnorrPublicKeyOf
    implicit val schnorrSeedDerivation: SeedDerivation[Secp256k1Schnorr] = backend.schnorrSeedDerivation
    implicit def schnorrSigner[F[_]: Monad: SecureRandom]: RandomizedSigner[F, Secp256k1Schnorr] = backend.schnorrSigner[F]
  }

  /**
   * Sanctioned FFI boundary (`.workspace/crypto-architecture.md` §1): libsecp256k1 uses standard RFC 6979 nonces only
   * when `ndata` is absent, which the Kotlin binding expresses as `null`. This is the one `null` in dilf4s.
   */
  private[signing] object Ffi {
    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    def signRfc6979(native: Native, digest32: Array[Byte], key: Array[Byte]): Array[Byte] =
      native.sign(digest32, key, null) // scalafix:ok DisableSyntax.null
  }
}
