package xyz.kd5ujc.hash

import cats.effect.Sync
import cats.implicits.{toFlatMapOps, toFunctorOps}

import xyz.kd5ujc.binary.JsonSerializer

import io.circe.Encoder
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3

sealed abstract class Hasher[F[_]: Sync: JsonSerializer, L] {
  def hashBytes(data: Array[Byte], prefix: Array[Byte] = Array()): F[Digest[L]]

  def hashJson[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[Digest[L]] =
    JsonSerializer[F].serialize(data).flatMap(msg => hashBytes(msg, prefix))

  def compare[A: Encoder](data: A, expectedHash: Digest[L]): F[Boolean] =
    hashJson(data).map(_.value.sameElements(expectedHash.value))
}

class Blake2b256Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l256] {

  override def hashBytes(data: Array[Byte], prefix: Array[Byte]): F[Digest[l256]] = {
    val digest = new Blake2bDigest(256)
    val output = new Array[Byte](digest.getDigestSize)

    if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
    digest.update(data, 0, data.length)
    digest.doFinal(output, 0)

    Sync[F].delay(l256.from(output).valueOr(err => throw new Error(s"Error creating Blake2b256 hash! $err")))
  }
}

class Blake2b512Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l512] {

  override def hashBytes(data: Array[Byte], prefix: Array[Byte]): F[Digest[l512]] = {
    val digest = new Blake2bDigest(512)
    val output = new Array[Byte](digest.getDigestSize)

    if (prefix.nonEmpty) digest.update(prefix, 0, prefix.length)
    digest.update(data, 0, data.length)
    digest.doFinal(output, 0)

    Sync[F].delay(l512.from(output).valueOr(err => throw new Error(s"Error creating Blake2b512 hash! $err")))
  }
}

class Sha3256Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l256] {

  override def hashBytes(data: Array[Byte], prefix: Array[Byte]): F[Digest[l256]] = {
    val sha3 = new DigestSHA3(256)

    if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
    sha3.update(data, 0, data.length)

    Sync[F].delay(l256.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-256 hash! $err")))
  }
}

class Sha3512Hasher[F[_]: Sync: JsonSerializer] extends Hasher[F, l512] {

  override def hashBytes(data: Array[Byte], prefix: Array[Byte]): F[Digest[l512]] = {
    val sha3 = new DigestSHA3(512)

    if (prefix.nonEmpty) sha3.update(prefix, 0, prefix.length)
    sha3.update(data, 0, data.length)

    Sync[F].delay(l512.from(sha3.digest()).valueOr(err => throw new Error(s"Error creating Sha3-512 hash! $err")))
  }
}
