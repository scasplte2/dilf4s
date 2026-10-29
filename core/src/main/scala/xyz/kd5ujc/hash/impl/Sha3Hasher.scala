package xyz.kd5ujc.hash.impl

import cats.effect.Sync
import cats.syntax.flatMap._

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.api.DigestProducer
import xyz.kd5ujc.hash.{Digest, Hasher, l256, l512}

import io.circe.Encoder
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3

/**
 * Implementation of SHA3 hash algorithm with 256 and 512 bit variants
 */
sealed abstract class Sha3Hasher[F[_]: Sync: JsonSerializer] extends DigestProducer[F] with Hasher[F] {
  protected val digestSize: Int
  protected def createDigest(bytes: Array[Byte]): Digest

  def hashBytes(bytes: Array[Byte], prefix: Array[Byte]): F[Digest] = Sync[F].delay {
    val sha3 = new DigestSHA3(digestSize * 8)
    if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
    sha3.update(bytes, 0, bytes.length)
    createDigest(sha3.digest())
  }

  def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest] =
    JsonSerializer[F].serialize(data).flatMap(hashBytes(_, prefix))
}

class Sha3_256Hasher[F[_]: Sync: JsonSerializer] extends Sha3Hasher[F] {
  protected val digestSize: Int = 32
  protected def createDigest(bytes: Array[Byte]): Digest = l256.unsafe(bytes)
}

class Sha3_512Hasher[F[_]: Sync: JsonSerializer] extends Sha3Hasher[F] {
  protected val digestSize: Int = 64
  protected def createDigest(bytes: Array[Byte]): Digest = l512.unsafe(bytes)
}

object Sha3Hasher {
  def sha3_256[F[_]: Sync: JsonSerializer]: Sha3Hasher[F] = new Sha3_256Hasher[F]
  def sha3_512[F[_]: Sync: JsonSerializer]: Sha3Hasher[F] = new Sha3_512Hasher[F]
}
