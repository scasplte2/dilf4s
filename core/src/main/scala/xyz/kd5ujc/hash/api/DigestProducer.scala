package xyz.kd5ujc.hash.api

import cats.effect.Sync
import cats.syntax.flatMap._

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.Encoder

/**
 * Produces hash digests in a standardized way across different algorithms,
 * handling type-level operations and serialization
 */
trait DigestProducer[F[_]] {

  /**
   * Hash data to produce a digest
   *
   * @param data The data to hash
   * @param prefix Optional prefix bytes to prepend
   * @return The resulting digest
   */
  def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest]
}

object DigestProducer {
  def apply[F[_]](implicit producer: DigestProducer[F]): DigestProducer[F] = producer

  /**
   * Create a DigestProducer that uses JSON serialization for structured data
   */
  def json[F[_]: Sync: JsonSerializer](implicit hasher: Hasher[F]): DigestProducer[F] = new DigestProducer[F] {
    def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest] =
      JsonSerializer[F].serialize(data).flatMap(bytes => hasher.hashBytes(bytes, prefix))
  }

  object syntax {
    implicit class DigestProducerOps[A](val data: A) extends AnyVal {
      def hash[F[_]](prefix: Array[Byte] = Array())(implicit producer: DigestProducer[F], enc: Encoder[A]): F[Digest] =
        producer.hash(data, prefix)
    }
  }
}
