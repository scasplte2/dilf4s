package xyz.kd5ujc.hash.impl

import cats.effect.Sync
import cats.syntax.flatMap._

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.api.DigestProducer
import xyz.kd5ujc.hash.{Digest, Hasher, l256, l512}

import io.circe.Encoder
import org.bouncycastle.crypto.digests.Blake2bDigest

/**
 * Implementation of Blake2b hash algorithm with 256 and 512 bit variants
 */
sealed abstract class Blake2bHasher[F[_]: Sync: JsonSerializer] extends DigestProducer[F] with Hasher[F] {
  protected val digestSize: Int
  protected def createDigest(bytes: Array[Byte]): Digest

  def hashBytes(bytes: Array[Byte], prefix: Array[Byte]): F[Digest] = Sync[F].delay {
    val digest = new Blake2bDigest(digestSize * 8)
    if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
    digest.update(bytes, 0, bytes.length)
    val output = new Array[Byte](digest.getDigestSize)
    digest.doFinal(output, 0)
    createDigest(output)
  }

  def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest] =
    JsonSerializer[F].serialize(data).flatMap(hashBytes(_, prefix))

}

class Blake2b256Hasher[F[_]: Sync: JsonSerializer] extends Blake2bHasher[F] {
  protected val digestSize: Int = 32
  protected def createDigest(bytes: Array[Byte]): Digest = l256.unsafe(bytes)
}

class Blake2b512Hasher[F[_]: Sync: JsonSerializer] extends Blake2bHasher[F] {
  protected val digestSize: Int = 64
  protected def createDigest(bytes: Array[Byte]): Digest = l512.unsafe(bytes)
}

object Blake2bHasher {
  def b256[F[_]: Sync: JsonSerializer]: Blake2bHasher[F] = new Blake2b256Hasher[F]
  def b512[F[_]: Sync: JsonSerializer]: Blake2bHasher[F] = new Blake2b512Hasher[F]
}
