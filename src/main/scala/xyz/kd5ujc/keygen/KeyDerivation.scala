package xyz.kd5ujc.keygen

import cats.MonadThrow
import cats.syntax.all._
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter
import java.nio.charset.StandardCharsets

/**
 * Interface for converting entropy to a seed using a password
 */
trait KeyDerivation[F[_]] {
  def deriveKey(entropy: Entropy, password: Option[String], seedLength: Int): F[Array[Byte]]
}

object KeyDerivation {

  /**
   * PBKDF2-SHA512 implementation of key derivation
   */
  def pbkdf2Sha512[F[_]: MonadThrow](iterations: Int = 4096): KeyDerivation[F] =
    new KeyDerivation[F] {
      def deriveKey(entropy: Entropy, password: Option[String], seedLength: Int): F[Array[Byte]] =
        MonadThrow[F].catchNonFatal {
          val generator = new PKCS5S2ParametersGenerator(new SHA512Digest())

          val pass = password.getOrElse("").getBytes(StandardCharsets.UTF_8)
          generator.init(pass, entropy.bytes, iterations)

          generator.generateDerivedParameters(seedLength * 8)
            .asInstanceOf[KeyParameter]
            .getKey
        }.adaptErr(KeyDerivationError.DerivationFailed(_))
    }
}

sealed trait KeyDerivationError extends RuntimeException
object KeyDerivationError {
  case class DerivationFailed(cause: Throwable) extends KeyDerivationError {
    override def getMessage: String = s"Key derivation failed: ${cause.getMessage}"
  }
}