package xyz.kd5ujc.crypto.signing

import org.bouncycastle.math.ec.rfc8032.{Ed25519 => Rfc8032}
import scodec.bits.ByteVector

/**
 * RFC 8032 Ed25519 (pure, deterministic). Signing and key derivation use BouncyCastle's constant-time implementation;
 * each call allocates its own buffers, which never escape.
 */
sealed trait Ed25519

object Ed25519 {

  val SeedLength: Long = 32
  val KeyLength: Long = 32
  val SignatureLength: Long = 64

  implicit val codec: SchemeCodec[Ed25519] =
    SchemeCodec.fixed[Ed25519]("Ed25519", KeyLength, KeyLength, SignatureLength, vk => Rfc8032.validatePublicKeyFull(vk.toArray, 0))

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

  /** RFC 8032 §5.1.7 verification (cofactorless, canonical encodings required). */
  implicit val verifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) => {
    val m = message.toArray
    Rfc8032.verify(signature.bytes.toArray, 0, key.bytes.toArray, 0, m, 0, m.length)
  }

  /** The RFC 8032 secret key is the 32-byte seed itself. */
  implicit val seedDerivation: SeedDerivation[Ed25519] = new SeedDerivation[Ed25519] {
    val seedLength: Long = SeedLength
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[Ed25519]] =
      codec.signingKey(seed).map(sk => KeyPair(sk, publicKeyOf.verifyingKey(sk)))
  }
}
