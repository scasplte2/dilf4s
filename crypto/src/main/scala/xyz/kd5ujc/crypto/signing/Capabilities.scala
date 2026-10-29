package xyz.kd5ujc.crypto.signing

import cats.Monad
import cats.effect.std.SecureRandom
import cats.mtl.Raise
import cats.syntax.all._

import scodec.bits.ByteVector

/**
 * Small, orthogonal capabilities (`.workspace/crypto-architecture.md` §4). Consumers ask only for what they use, so no
 * curve or library type reaches higher layers. Deterministic operations are pure functions; only randomness is in `F`.
 */
trait Verifier[S] {
  def verify(key: VerifyingKey[S], message: ByteVector, signature: Signature[S]): Boolean
}

/** Deterministic signing: the same key and message always give the same signature. */
trait Signer[S] {
  def sign(key: SigningKey[S], message: ByteVector): Signature[S]
}

/** Signing that consumes fresh randomness (randomised nonces, auxiliary randomness). */
trait RandomizedSigner[F[_], S] {
  def sign(key: SigningKey[S], message: ByteVector): F[Signature[S]]
}

trait PublicKeyOf[S] {
  def verifyingKey(key: SigningKey[S]): VerifyingKey[S]
}

/** Deterministic key derivation from seed material. */
trait SeedDerivation[S] {
  def seedLength: Long
  def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[S]]
}

trait KeyGen[F[_], S] {
  def generate: F[KeyPair[S]]
}

object Verifier { def apply[S](implicit V: Verifier[S]): Verifier[S] = V }
object Signer { def apply[S](implicit S: Signer[S]): Signer[S] = S }
object PublicKeyOf { def apply[S](implicit P: PublicKeyOf[S]): PublicKeyOf[S] = P }
object SeedDerivation { def apply[S](implicit D: SeedDerivation[S]): SeedDerivation[S] = D }

object RandomizedSigner {

  def apply[F[_], S](implicit R: RandomizedSigner[F, S]): RandomizedSigner[F, S] = R

  /** Any deterministic signer is trivially a randomised one. */
  implicit def fromSigner[F[_], S](implicit S: Signer[S], F: cats.Applicative[F]): RandomizedSigner[F, S] =
    (key: SigningKey[S], message: ByteVector) => F.pure(S.sign(key, message))
}

object KeyGen {

  def apply[F[_], S](implicit K: KeyGen[F, S]): KeyGen[F, S] = K

  /** Key generation from seed derivation and a cryptographically secure source; invalid seeds raise a typed error. */
  implicit def fromSeed[F[_]: Monad, S](implicit D: SeedDerivation[S], R: SecureRandom[F], E: Raise[F, CryptoError]): KeyGen[F, S] =
    new KeyGen[F, S] {
      def generate: F[KeyPair[S]] =
        R.nextBytes(D.seedLength.toInt).flatMap(b => D.fromSeed(ByteVector.view(b)).fold(E.raise[CryptoError, KeyPair[S]], _.pure[F]))
    }
}
