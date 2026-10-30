package xyz.kd5ujc.core.hash

import cats.syntax.either._
import cats.{Eq, Show}

import org.bouncycastle.crypto.ExtendedDigest
import org.bouncycastle.crypto.digests.{Blake2bDigest, SHA256Digest, SHA3Digest, SHA512Digest}
import scodec.bits.ByteVector

/** Hash algorithm tags: uninhabited types that index [[Digest]] and [[Hasher]]. */
sealed trait Sha256
sealed trait Sha512
sealed trait Sha3_256
sealed trait Sha3_512
sealed trait Blake2b256
sealed trait Blake2b512

/** A digest produced by algorithm `H`; digests of different algorithms never compare or mix. */
sealed abstract case class Digest[H](bytes: ByteVector) {
  def hex: String = bytes.toHex
}

object Digest {

  def fromBytes[H](bytes: ByteVector)(implicit H: Hasher[H]): Either[HashError, Digest[H]] =
    Either.cond(bytes.size == H.outputSize, new Digest[H](bytes) {}, HashError.InvalidLength(H.name, H.outputSize, bytes.size))

  def fromHex[H: Hasher](hex: String): Either[HashError, Digest[H]] =
    ByteVector.fromHexDescriptive(hex).leftMap(HashError.InvalidHex(_)).flatMap(fromBytes[H])

  private[hash] def unsafe[H](bytes: ByteVector): Digest[H] = new Digest[H](bytes) {}

  /** Digests are byte strings in the canonical data model. */
  implicit def digestCanonical[H]: xyz.kd5ujc.core.canonical.Canonical[Digest[H]] =
    xyz.kd5ujc.core.canonical.Canonical.total(d => xyz.kd5ujc.core.canonical.CanonicalValue.bytes(d.bytes))

  implicit def digestEq[H]: Eq[Digest[H]] = Eq.fromUniversalEquals
  implicit def digestShow[H]: Show[Digest[H]] = Show.show(_.hex)
}

/** A pure hash function. Instances wrap BouncyCastle digests: each call uses a fresh digest object and fresh arrays. */
trait Hasher[H] {
  def name: String
  def outputSize: Long

  /** The compression function's input block size in bytes (HMAC, `expand_message_xmd`). */
  def blockSize: Long
  def digest(data: ByteVector): Digest[H]
}

object Hasher {

  def apply[H](implicit H: Hasher[H]): Hasher[H] = H

  implicit val sha256: Hasher[Sha256] = bouncyCastle("SHA-256", () => new SHA256Digest())
  implicit val sha512: Hasher[Sha512] = bouncyCastle("SHA-512", () => new SHA512Digest())
  implicit val sha3_256: Hasher[Sha3_256] = bouncyCastle("SHA3-256", () => new SHA3Digest(256))
  implicit val sha3_512: Hasher[Sha3_512] = bouncyCastle("SHA3-512", () => new SHA3Digest(512))
  implicit val blake2b256: Hasher[Blake2b256] = bouncyCastle("BLAKE2b-256", () => new Blake2bDigest(256))
  implicit val blake2b512: Hasher[Blake2b512] = bouncyCastle("BLAKE2b-512", () => new Blake2bDigest(512))

  /**
   * Primitive boundary: BouncyCastle digests are mutable, but each call builds its own instance and buffers, which
   * never escape. `update` and `doFinal` cannot throw here: bounds come from the arrays themselves, and the output
   * buffer is sized by `getDigestSize`.
   */
  private def bouncyCastle[H](algorithm: String, make: () => ExtendedDigest): Hasher[H] =
    new Hasher[H] {
      val name: String = algorithm
      val outputSize: Long = make().getDigestSize.toLong
      val blockSize: Long = make().getByteLength.toLong

      def digest(data: ByteVector): Digest[H] = {
        val d = make()
        val in = data.toArray
        val out = new Array[Byte](d.getDigestSize)
        d.update(in, 0, in.length)
        d.doFinal(out, 0)
        Digest.unsafe[H](ByteVector.view(out))
      }
    }
}

sealed trait HashError extends Product with Serializable

object HashError {
  final case class InvalidLength(algorithm: String, expected: Long, actual: Long) extends HashError
  final case class InvalidHex(reason: String) extends HashError
  final case class InvalidDomainTag(tag: String) extends HashError
  final case class InvalidExpansion(reason: String) extends HashError
}
