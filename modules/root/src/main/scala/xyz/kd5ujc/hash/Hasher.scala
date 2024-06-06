package xyz.kd5ujc.hash

import cats.effect.Sync
import cats.implicits.toFunctorOps
import io.circe.Encoder
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import xyz.kd5ujc.binary.JsonSerializer

trait Hasher[F[_], L] {
  def hash[A: Encoder](data:    A, prefix:       Array[Byte] = Array()): F[Digest[L]]
  def compare[A: Encoder](data: A, expectedHash: Digest[L]): F[Boolean]
}

class Blake2b256Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l256] {
  override def compare[A: Encoder](data: A, expectedHash: Digest[l256]): F[Boolean] =
    hash(data).map(_.value.sameElements(expectedHash.value))

  override def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest[l256]] =
    JsonSerializer[F].serialize(data).map { msg =>
      val digest = new Blake2bDigest(256)
      val output = new Array[Byte](digest.getDigestSize)

      if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
      digest.update(msg, 0, msg.length)
      digest.doFinal(output, 0)

      l256.from(output).valueOr(err => throw new Error(s"Error creating Blake2b256 hash! $err"))
    }
}

class Blake2b512Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l512] {
  override def compare[A: Encoder](data: A, expectedHash: Digest[l512]): F[Boolean] =
    hash(data).map(_.value.sameElements(expectedHash.value))

  override def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest[l512]] =
    JsonSerializer[F].serialize(data).map { msg =>
      val digest = new Blake2bDigest(512)
      val output = new Array[Byte](digest.getDigestSize)

      if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
      digest.update(msg, 0, msg.length)
      digest.doFinal(output, 0)

      l512.from(output).valueOr(err => throw new Error(s"Error creating Blake2b512 hash! $err"))
    }
}

class Sha3256Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l256] {
  override def compare[A: Encoder](data: A, expectedHash: Digest[l256]): F[Boolean] =
    hash(data).map(_.value.sameElements(expectedHash.value))

  override def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest[l256]] =
    JsonSerializer[F].serialize(data).map { msg =>
      val sha3 = new DigestSHA3(256)

      if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
      sha3.update(msg, 0, msg.length)

      l256.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-256 hash! $err"))
    }
}

class Sha3512Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l512] {
  override def compare[A: Encoder](data: A, expectedHash: Digest[l512]): F[Boolean] =
    hash(data).map(_.value.sameElements(expectedHash.value))

  override def hash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest[l512]] =
    JsonSerializer[F].serialize(data).map { msg =>
      val sha3 = new DigestSHA3(512)

      if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
      sha3.update(msg, 0, msg.length)

      l512.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-512 hash! $err"))
    }
}
