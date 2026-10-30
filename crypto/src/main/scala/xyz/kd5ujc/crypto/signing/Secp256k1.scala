package xyz.kd5ujc.crypto.signing

import cats.Functor
import cats.effect.Sync
import cats.effect.std.SecureRandom
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{DomainTag, Hasher, Sha256, TaggedHash}

import fr.acinq.secp256k1.{Secp256k1 => Native}
import scodec.bits.ByteVector

/** secp256k1 ECDSA over SHA-256(message): RFC 6979 deterministic nonces, compact 64-byte `r || s`, low-S only. */
sealed trait Secp256k1Ecdsa

/**
 * Standard BIP-340 Schnorr over secp256k1, byte-compatible with Bitcoin (Taproot), Nostr and wallets: the signed
 * message is exactly the caller's 32 bytes ([[Message32]]). Use it to interoperate with external signers.
 */
sealed trait Bip340

/**
 * dilf4s Schnorr over secp256k1: BIP-340 applied to the tagged hash `dilf4s/sig/bip340/v1` of a message of any length
 * (BIP-340's recommendation for domain separation). Keys share the [[Bip340]] encodings, but a signature of one scheme
 * never verifies under the other, and the types keep them apart.
 */
sealed trait Secp256k1TaggedSchnorr

/** Exactly 32 bytes: the message type of standard [[Bip340]]. */
final case class Message32 private[signing] (bytes: ByteVector)

object Message32 {
  def from(bytes: ByteVector): Either[CryptoError, Message32] =
    Either.cond(
      bytes.size == 32L,
      Message32(bytes),
      CryptoError.InvalidEncoding("BIP-340 message", s"expected 32 bytes, got ${bytes.size}")
    )
}

/**
 * bitcoin-core's constant-time libsecp256k1, through ACINQ's JNI bindings. Loading the native library is an effect, so
 * the backend is acquired in `F` with [[Secp256k1.load]]; its instances are then pure. Every native call that can
 * reject input is captured as a typed [[CryptoError]], never thrown.
 */
final class Secp256k1 private (native: Native) {

  private def call[A](operation: String)(thunk: => A): Either[CryptoError, A] =
    Either
      .catchNonFatal(thunk)
      .leftMap(e => CryptoError.BackendFailure(s"secp256k1 $operation", Option(e.getMessage).getOrElse(e.getClass.getName)))

  private val sha256 = Hasher[Sha256]
  private val schnorrTag = TaggedHash[Sha256](DomainTag.literal(Secp256k1.SchnorrTag))
  private val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  private def validScalar(b: ByteVector): Boolean = b.size == 32L && call("secKeyVerify")(native.secKeyVerify(b.toArray)).getOrElse(false)

  // ------------------------------------------------------------------ ECDSA

  val ecdsaCodec: SchemeCodec[Secp256k1Ecdsa] = new SchemeCodec[Secp256k1Ecdsa] {
    val name: String = "secp256k1-ECDSA"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Secp256k1Ecdsa]] =
      Either.cond(
        validScalar(bytes),
        new SigningKey[Secp256k1Ecdsa](bytes),
        CryptoError.InvalidKey("secp256k1 secret key must be a scalar in [1, n)")
      )

    /** 33-byte compressed SEC1 point, checked on the curve. */
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Secp256k1Ecdsa]] =
      if (bytes.size != 33L)
        CryptoError.InvalidEncoding("secp256k1 verifying key", s"expected 33 compressed bytes, got ${bytes.size}").asLeft
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

  val ecdsaVerifier: Verifier[Secp256k1Ecdsa] =
    (key: VerifyingKey[Secp256k1Ecdsa], message: ByteVector, signature: Signature[Secp256k1Ecdsa]) =>
      call("verify")(native.verify(signature.bytes.toArray, sha256.digest(message).bytes.toArray, key.bytes.toArray)).getOrElse(false)

  val ecdsaSeedDerivation: SeedDerivation[Secp256k1Ecdsa] = new SeedDerivation[Secp256k1Ecdsa] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Secp256k1Ecdsa]] =
      ecdsaCodec.signingKey(seed).map(sk => KeyPair(sk, ecdsaPublicKeyOf.verifyingKey(sk)))
  }

  /** `(a + b) mod n` on secret scalars, in libsecp256k1 (constant time); fails if either is invalid or the sum is 0. */
  private[crypto] def tweakAdd(a: ByteVector, b: ByteVector): Either[CryptoError, ByteVector] =
    call("privKeyTweakAdd")(native.privKeyTweakAdd(a.toArray, b.toArray)).map(ByteVector.view)

  // ------------------------------------------------------------------ BIP-340 Schnorr (standard and tagged)

  private def schnorrCodecFor[S](scheme: String): SchemeCodec[S] = new SchemeCodec[S] {
    val name: String = scheme
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[S]] =
      Either.cond(validScalar(bytes), new SigningKey[S](bytes), CryptoError.InvalidKey("secp256k1 secret key must be a scalar in [1, n)"))

    /** 32-byte x-only key; it must lift to a curve point (even y). */
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[S]] =
      if (bytes.size != 32L) CryptoError.InvalidEncoding(s"$scheme verifying key", s"expected 32 bytes, got ${bytes.size}").asLeft
      else call("pubkeyParse")(native.pubkeyParse((ByteVector(0x02) ++ bytes).toArray)).as(VerifyingKey[S](bytes))
    def signature(bytes: ByteVector): Either[CryptoError, Signature[S]] =
      Either.cond(
        bytes.size == 64L,
        Signature[S](bytes),
        CryptoError.InvalidEncoding(s"$scheme signature", s"expected 64 bytes, got ${bytes.size}")
      )
  }

  private def xOnly[S](key: SigningKey[_]): VerifyingKey[S] =
    VerifyingKey[S](ByteVector.view(native.pubKeyCompress(native.pubkeyCreate(key.bytes.toArray))).drop(1))

  private def signRaw[S](key: SigningKey[_], message32: ByteVector, aux: ByteVector): Either[CryptoError, Signature[S]] =
    Either
      .cond(aux.size == 32L, aux, CryptoError.InvalidEncoding("BIP-340 auxiliary randomness", s"expected 32 bytes, got ${aux.size}"))
      .flatMap(a => call("signSchnorr")(native.signSchnorr(message32.toArray, key.bytes.toArray, a.toArray)))
      .map(b => Signature[S](ByteVector.view(b)))

  private def verifyRaw(key: VerifyingKey[_], message32: ByteVector, signature: Signature[_]): Boolean =
    signature.bytes.size == 64L &&
    call("verifySchnorr")(native.verifySchnorr(signature.bytes.toArray, message32.toArray, key.bytes.toArray)).getOrElse(false)

  // standard

  val bip340Codec: SchemeCodec[Bip340] = schnorrCodecFor[Bip340]("BIP-340")
  val bip340PublicKeyOf: PublicKeyOf[Bip340] = (key: SigningKey[Bip340]) => xOnly[Bip340](key)

  /** Standard BIP-340 with caller-supplied 32-byte auxiliary randomness (known answers, external RNGs). */
  def bip340SignWithAux(key: SigningKey[Bip340], message: Message32, aux: ByteVector): Either[CryptoError, Signature[Bip340]] =
    signRaw[Bip340](key, message.bytes, aux)

  /** Standard BIP-340 with fresh auxiliary randomness, as BIP-340 recommends. Total for the same reasons as the tagged signer. */
  def bip340Sign[F[_]: Functor](key: SigningKey[Bip340], message: Message32)(implicit R: SecureRandom[F]): F[Signature[Bip340]] =
    R.nextBytes(32).map(aux => Signature[Bip340](ByteVector.view(native.signSchnorr(message.bytes.toArray, key.bytes.toArray, aux))))

  def bip340Verify(key: VerifyingKey[Bip340], message: Message32, signature: Signature[Bip340]): Boolean =
    verifyRaw(key, message.bytes, signature)

  val bip340SeedDerivation: SeedDerivation[Bip340] = new SeedDerivation[Bip340] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Bip340]] =
      bip340Codec.signingKey(seed).map(sk => KeyPair(sk, bip340PublicKeyOf.verifyingKey(sk)))
  }

  // tagged (dilf4s)

  val taggedSchnorrCodec: SchemeCodec[Secp256k1TaggedSchnorr] = schnorrCodecFor[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr")
  val taggedSchnorrPublicKeyOf: PublicKeyOf[Secp256k1TaggedSchnorr] = (key: SigningKey[Secp256k1TaggedSchnorr]) =>
    xOnly[Secp256k1TaggedSchnorr](key)

  /** Tagged Schnorr with caller-supplied 32-byte auxiliary randomness: for known answers. */
  def taggedSchnorrSignWithAux(
    key:     SigningKey[Secp256k1TaggedSchnorr],
    message: ByteVector,
    aux:     ByteVector
  ): Either[CryptoError, Signature[Secp256k1TaggedSchnorr]] =
    signRaw[Secp256k1TaggedSchnorr](key, schnorrTag(message).bytes, aux)

  /**
   * Fresh auxiliary randomness per signature. Total: the key passed the codec and the digest and randomness are always
   * 32 bytes, the only inputs libsecp256k1 rejects.
   */
  def taggedSchnorrSigner[F[_]: Functor](implicit R: SecureRandom[F]): RandomizedSigner[F, Secp256k1TaggedSchnorr] =
    (key: SigningKey[Secp256k1TaggedSchnorr], message: ByteVector) =>
      R.nextBytes(32)
        .map(aux =>
          Signature[Secp256k1TaggedSchnorr](ByteVector.view(native.signSchnorr(schnorrTag(message).bytes.toArray, key.bytes.toArray, aux)))
        )

  val taggedSchnorrVerifier: Verifier[Secp256k1TaggedSchnorr] =
    (key: VerifyingKey[Secp256k1TaggedSchnorr], message: ByteVector, signature: Signature[Secp256k1TaggedSchnorr]) =>
      verifyRaw(key, schnorrTag(message).bytes, signature)

  val taggedSchnorrSeedDerivation: SeedDerivation[Secp256k1TaggedSchnorr] = new SeedDerivation[Secp256k1TaggedSchnorr] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Secp256k1TaggedSchnorr]] =
      taggedSchnorrCodec.signingKey(seed).map(sk => KeyPair(sk, taggedSchnorrPublicKeyOf.verifyingKey(sk)))
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
    implicit val bip340Codec: SchemeCodec[Bip340] = backend.bip340Codec
    implicit val bip340PublicKeyOf: PublicKeyOf[Bip340] = backend.bip340PublicKeyOf
    implicit val bip340SeedDerivation: SeedDerivation[Bip340] = backend.bip340SeedDerivation
    implicit val taggedSchnorrCodec: SchemeCodec[Secp256k1TaggedSchnorr] = backend.taggedSchnorrCodec
    implicit val taggedSchnorrVerifier: Verifier[Secp256k1TaggedSchnorr] = backend.taggedSchnorrVerifier
    implicit val taggedSchnorrPublicKeyOf: PublicKeyOf[Secp256k1TaggedSchnorr] = backend.taggedSchnorrPublicKeyOf
    implicit val taggedSchnorrSeedDerivation: SeedDerivation[Secp256k1TaggedSchnorr] = backend.taggedSchnorrSeedDerivation
    implicit def taggedSchnorrSigner[F[_]: Functor: SecureRandom]: RandomizedSigner[F, Secp256k1TaggedSchnorr] =
      backend.taggedSchnorrSigner[F]
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
