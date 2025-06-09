package xyz.kd5ujc.signature

import java.security.SecureRandom

import cats.data.ValidatedNec
import cats.effect.{Resource, Sync}
import cats.syntax.all._

import xyz.kd5ujc.signature.ecdsa.Ed25519
import xyz.kd5ujc.signature.ecdsa.api.SignatureScheme

object signature {
  case class SigningKey(bytes: Array[Byte]) extends AnyVal
  case class VerificationKey(bytes: Array[Byte]) extends AnyVal
  case class SignatureProof(bytes: Array[Byte]) extends AnyVal
  case class Message(bytes: Array[Byte]) extends AnyVal

  sealed trait CryptoError
  object CryptoError {
    case class InvalidKeySize(expected: Int, got: Int) extends CryptoError
    case class InvalidSignatureSize(expected: Int, got: Int) extends CryptoError
    case class InvalidKey(msg: String) extends CryptoError
    case class InvalidSignature(msg: String) extends CryptoError
    case class SystemError(throwable: Throwable) extends CryptoError
  }

  // Type aliases for validation results
  type CryptoValidation[A] = ValidatedNec[CryptoError, A]

  // Typeclasses for key/signature validation
  trait KeyValidator[A] {
    def validate(bytes: Array[Byte]): CryptoValidation[A]
  }

  object KeyValidator {
    def apply[A](implicit KV: KeyValidator[A]): KeyValidator[A] = KV

    // Validate byte length and any additional constraints
    def validateKey(bytes: Array[Byte], expectedLength: Int): CryptoValidation[Array[Byte]] =
      if (bytes.length != expectedLength)
        CryptoError.InvalidKeySize(expectedLength, bytes.length).invalidNec
      else
        bytes.validNec

    implicit val secretKeyValidator: KeyValidator[SigningKey] =
      (bytes: Array[Byte]) => validateKey(bytes, 32).map(SigningKey.apply)

    implicit val publicKeyValidator: KeyValidator[VerificationKey] =
      (bytes: Array[Byte]) => validateKey(bytes, 32).map(VerificationKey.apply)

    implicit val signatureValidator: KeyValidator[SignatureProof] =
      (bytes: Array[Byte]) => validateKey(bytes, 64).map(SignatureProof.apply)
  }

  // Ed25519 implementation
  object Ed25519SignatureScheme {
    def apply[F[_]](implicit F: Sync[F]): SignatureScheme[F] = new SignatureScheme[F] {
      private val ed25519 = new Ed25519()

      def generateKeyPair: F[(SigningKey, VerificationKey)] = F.delay {
        val secretKeyBytes = new Array[Byte](32)
        val publicKeyBytes = new Array[Byte](32)
        val random = new SecureRandom()

        ed25519.generatePrivateKey(random, secretKeyBytes)
        ed25519.generatePublicKey(secretKeyBytes, 0, publicKeyBytes, 0)

        (SigningKey(secretKeyBytes), VerificationKey(publicKeyBytes))
      }

      def sign(secretKey: SigningKey, message: Message): F[SignatureProof] = F.delay {
        val signatureBytes = new Array[Byte](ed25519.SIGNATURE_SIZE)
        ed25519.sign(
          secretKey.bytes,
          0,
          message.bytes,
          0,
          message.bytes.length,
          signatureBytes,
          0
        )
        SignatureProof(signatureBytes)
      }

      def verify(publicKey: VerificationKey, message: Message, signature: SignatureProof): F[Boolean] = F.delay {
        ed25519.verify(
          signature.bytes,
          0,
          publicKey.bytes,
          0,
          message.bytes,
          0,
          message.bytes.length
        )
      }
    }

    // Resource-safe version that handles cleanup
    def resource[F[_]: Sync]: Resource[F, SignatureScheme[F]] =
      Resource.make(
        Sync[F].delay(Ed25519SignatureScheme[F])
      )(_ => Sync[F].unit)
  }
}
