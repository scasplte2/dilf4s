package xyz.kd5ujc.signature.keygen

import java.nio.charset.StandardCharsets

import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all._

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter

/**
 * Interface for converting entropy to a seed using a password
 */
trait KeyDerivation[F[_]] {
  def derive(entropy: Entropy, password: Option[String])(outputByteLength: Int): F[Array[Byte]]
}

object KeyDerivation {

  def pbkdf2Sha512[F[_]: Sync](iterations: Int = 4096): KeyDerivation[F] =
    new KeyDerivation[F] {
      def derive(entropy: Entropy, password: Option[String])(outputByteLength: Int): F[Array[Byte]] = {
        val pass = password.getOrElse("").getBytes(StandardCharsets.UTF_8)

        Resource
          .make(
            acquire = Sync[F].delay {
              val generator = new PKCS5S2ParametersGenerator(new SHA512Digest())
              generator.init(pass, entropy.bytes, iterations)
              generator
            }
          )(
            release = _ =>
              Sync[F].delay {
                java.util.Arrays.fill(pass, 0: Byte)
              }
          )
          .use { generator =>
            Sync[F].delay {
              generator
                .generateDerivedParameters(outputByteLength * 8)
                .asInstanceOf[KeyParameter]
                .getKey
            }
          }
          .adaptErr(KeyDerivationError.DerivationFailed(_))
      }
    }
}

sealed trait KeyDerivationError extends RuntimeException
object KeyDerivationError {
  case class DerivationFailed(cause: Throwable) extends KeyDerivationError {
    override def getMessage: String = s"Key derivation failed: ${cause.getMessage}"
  }
}
