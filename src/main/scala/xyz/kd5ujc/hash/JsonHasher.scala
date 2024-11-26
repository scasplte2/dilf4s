package xyz.kd5ujc.hash

import cats.effect.Sync
import cats.implicits.{toFlatMapOps, toFunctorOps}
import io.circe.Encoder
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import xyz.kd5ujc.binary.JsonSerializer

private[hash] trait Hasher[F[_]] {
  def hashBytes[A](data: A, prefix: Array[Byte])(f: A => F[Array[Byte]]): F[Digest]
}

sealed abstract class JsonHasher[F[_]: Sync: JsonSerializer] extends Hasher[F] {
  def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest] =
    hashBytes(data, prefix)(JsonSerializer[F].serialize)

  def compare[A: Encoder](data: A, expectedHash: Digest): F[Boolean] =
    hash(data).map(_.value.sameElements(expectedHash.value))
}

object JsonHasher {
  def apply[F[_]: JsonHasher]: JsonHasher[F] = implicitly
}

class Blake2b256Hasher[F[_]: Sync: JsonSerializer] extends JsonHasher[F] {
  override def hashBytes[A](data: A, prefix: Array[Byte])(f: A => F[Array[Byte]]): F[Digest] =
    f(data).flatMap { _bytes =>
      val digest = new Blake2bDigest(256)
      val output = new Array[Byte](digest.getDigestSize)

      if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
      digest.update(_bytes, 0, _bytes.length)
      digest.doFinal(output, 0)

      Sync[F].delay(l256.from(output).valueOr(err => throw new Error(s"Error creating Blake2b256 hash! $err")))
    }
}

class Blake2b512Hasher[F[_]: Sync: JsonSerializer] extends JsonHasher[F] {

  override def hashBytes[A](data: A, prefix: Array[Byte])(f: A => F[Array[Byte]]): F[Digest] =
    f(data).flatMap { _bytes =>
      val digest = new Blake2bDigest(512)
      val output = new Array[Byte](digest.getDigestSize)

      if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
      digest.update(_bytes, 0, _bytes.length)
      digest.doFinal(output, 0)

      Sync[F].delay(l512.from(output).valueOr(err => throw new Error(s"Error creating Blake2b512 hash! $err")))
    }
}

class Sha3256Hasher[F[_]: Sync: JsonSerializer] extends JsonHasher[F] {

  override def hashBytes[A](data: A, prefix: Array[Byte])(f: A => F[Array[Byte]]): F[Digest] =
    f(data).flatMap { _bytes =>
      val sha3 = new DigestSHA3(256)

      if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
      sha3.update(_bytes, 0, _bytes.length)

      Sync[F].delay(l256.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-256 hash! $err")))
    }
}

class Sha3512Hasher[F[_]: Sync: JsonSerializer] extends JsonHasher[F] {

  override def hashBytes[A](data: A, prefix: Array[Byte])(f: A => F[Array[Byte]]): F[Digest] =
    f(data).flatMap { _bytes =>
      val sha3 = new DigestSHA3(512)

      if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
      sha3.update(_bytes, 0, _bytes.length)

      Sync[F].delay(l512.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-512 hash! $err")))
    }
}
