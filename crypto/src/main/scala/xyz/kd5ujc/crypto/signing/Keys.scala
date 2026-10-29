package xyz.kd5ujc.crypto.signing

import java.security.MessageDigest

import cats.{Eq, Show}

import scodec.bits.ByteVector

sealed trait CryptoError extends Product with Serializable

object CryptoError {
  final case class InvalidEncoding(what: String, reason: String) extends CryptoError
  final case class InvalidKey(reason: String) extends CryptoError
  final case class BackendFailure(operation: String, reason: String) extends CryptoError
}

/**
 * A secret signing key for scheme `S`. Its bytes never appear in `toString`, and equality is constant-time. Built only
 * through the scheme's [[SchemeCodec]] or key derivation.
 */
final class SigningKey[S] private[crypto] (val bytes: ByteVector) {
  override def toString: String = "SigningKey(<redacted>)"
  override def equals(other: Any): Boolean = other match {
    case that: SigningKey[_] => MessageDigest.isEqual(bytes.toArray, that.bytes.toArray)
    case _                   => false
  }
  override def hashCode: Int = bytes.size.toInt
}

final case class VerifyingKey[S] private[crypto] (bytes: ByteVector)
final case class Signature[S] private[crypto] (bytes: ByteVector)
final case class KeyPair[S](signing: SigningKey[S], verifying: VerifyingKey[S])

object VerifyingKey {
  implicit def show[S]: Show[VerifyingKey[S]] = Show.show(k => s"VerifyingKey(${k.bytes.toHex})")
  implicit def eq[S]: Eq[VerifyingKey[S]] = Eq.fromUniversalEquals
}

object Signature {
  implicit def show[S]: Show[Signature[S]] = Show.show(s => s"Signature(${s.bytes.toHex})")
  implicit def eq[S]: Eq[Signature[S]] = Eq.fromUniversalEquals
}

/** Validating decoders for a scheme's keys and signatures (lengths, and curve membership where checkable). */
trait SchemeCodec[S] {
  def name: String
  def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[S]]
  def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[S]]
  def signature(bytes: ByteVector): Either[CryptoError, Signature[S]]
}

object SchemeCodec {

  def apply[S](implicit C: SchemeCodec[S]): SchemeCodec[S] = C

  /** Fixed-length codec with optional extra validation of verifying keys. */
  def fixed[S](scheme: String, skLength: Long, vkLength: Long, sigLength: Long, validVk: ByteVector => Boolean = _ => true): SchemeCodec[S] =
    new SchemeCodec[S] {
      val name: String = scheme
      private def length(what: String, expected: Long, b: ByteVector): Either[CryptoError, ByteVector] =
        Either.cond(b.size == expected, b, CryptoError.InvalidEncoding(s"$scheme $what", s"expected $expected bytes, got ${b.size}"))
      def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[S]] = length("signing key", skLength, bytes).map(new SigningKey[S](_))
      def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[S]] =
        length("verifying key", vkLength, bytes)
          .flatMap(b => Either.cond(validVk(b), VerifyingKey[S](b), CryptoError.InvalidKey(s"$scheme verifying key is not a valid point")))
      def signature(bytes: ByteVector): Either[CryptoError, Signature[S]] = length("signature", sigLength, bytes).map(Signature[S](_))
    }
}
