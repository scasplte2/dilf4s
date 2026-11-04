package xyz.kd5ujc.core.hash

import java.nio.ByteBuffer

import cats.syntax.either._
import cats.{Eq, Show}

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{ByteSink, ByteSource, FlatBytes}

import org.bouncycastle.crypto.ExtendedDigest
import org.bouncycastle.crypto.digests.{Blake2bDigest, KeccakDigest, SHA256Digest, SHA3Digest, SHA512Digest}
import scodec.bits.ByteVector

/** Hash algorithm tags: uninhabited types that index [[Digest]] and [[Hasher]]. */
sealed trait Sha256
sealed trait Sha512
sealed trait Sha3_256

/** Keccak-256 as Ethereum uses it: the pre-standard padding, not SHA3-256's. */
sealed trait Keccak256
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

  /**
   * Unsigned bytewise lexicographic order, read in place up to the first difference. Collections keyed by digests use
   * it and never a hash code: a digest's JVM hash code is 32 bits of its bytes, which a peer choosing content can grind.
   */
  implicit def digestOrdering[H]: Ordering[Digest[H]] = (a, b) => xyz.kd5ujc.core.canonical.Cbor.compareBytes(a.bytes, b.bytes)
  implicit def digestShow[H]: Show[Digest[H]] = Show.show(_.hex)
}

/** A pure hash function. Instances wrap BouncyCastle digests: each call uses a fresh digest object and fresh arrays. */
trait Hasher[H] {
  def name: String
  def outputSize: Long

  /** The compression function's input block size in bytes (HMAC, `expand_message_xmd`). */
  def blockSize: Long

  def digest(data: ByteVector): Digest[H] = digestOf(ByteSource.bytes(data))

  /**
   * The digest of `source`'s bytes, absorbed as they are written: an encoding hashed this way is never built, so the
   * memory is the hash's state and a window of at most [[Hasher.Window]] bytes, whatever the length.
   */
  def digestOf(source: ByteSource): Digest[H]
}

object Hasher {

  def apply[H](implicit H: Hasher[H]): Hasher[H] = H

  implicit val sha256: Hasher[Sha256] = bouncyCastle("SHA-256", () => new SHA256Digest())
  implicit val sha512: Hasher[Sha512] = bouncyCastle("SHA-512", () => new SHA512Digest())
  implicit val sha3_256: Hasher[Sha3_256] = bouncyCastle("SHA3-256", () => new SHA3Digest(256))
  implicit val sha3_512: Hasher[Sha3_512] = bouncyCastle("SHA3-512", () => new SHA3Digest(512))
  implicit val keccak256: Hasher[Keccak256] = bouncyCastle("Keccak-256", () => new KeccakDigest(256))
  implicit val blake2b256: Hasher[Blake2b256] = bouncyCastle("BLAKE2b-256", () => new Blake2bDigest(256))
  implicit val blake2b512: Hasher[Blake2b512] = bouncyCastle("BLAKE2b-512", () => new Blake2bDigest(512))

  /** The most of an encoding staged, or of a byte string copied out, at once to absorb it: nothing is copied whole. */
  val Window: Int = 512

  /**
   * Primitive boundary: BouncyCastle digests are mutable, but each call builds its own instance and window, which
   * never escape: a source is polymorphic in its sink's state, so it can only pass the state on, and it uses each
   * state once ([[ByteSink]]). `update` and `doFinal` cannot throw here: bounds come from the arrays themselves, every
   * copy is at most the window's length, and the output buffer is sized by `getDigestSize`.
   */
  private[hash] def bouncyCastle[H](algorithm: String, make: () => ExtendedDigest): Hasher[H] =
    new Hasher[H] {
      val name: String = algorithm
      val outputSize: Long = make().getDigestSize.toLong
      val blockSize: Long = make().getByteLength.toLong

      /** Bytes no longer than the window are hashed in one `update`, as before streaming; longer ones through it. */
      override def digest(data: ByteVector): Digest[H] =
        if (data.size > Window.toLong) digestOf(ByteSource.bytes(data))
        else {
          val d = make()
          val in = data.toArray
          val out = new Array[Byte](d.getDigestSize)
          d.update(in, 0, in.length)
          d.doFinal(out, 0)
          Digest.unsafe[H](ByteVector.view(out))
        }

      def digestOf(source: ByteSource): Digest[H] = {
        val absorbed = source.writeTo(new Absorbing(make(), ByteBuffer.allocate(Window)))
        absorbed.flush()
        val out = new Array[Byte](absorbed.digest.getDigestSize)
        absorbed.digest.doFinal(out, 0)
        Digest.unsafe[H](ByteVector.view(out))
      }
    }

  /**
   * A digest absorbing as bytes are written, the sink state of [[Hasher.digestOf]], local to one call. Bytes are staged
   * in `window` (its position counts them) and absorbed a full window at a time, so small pieces (heads, keys) cost no
   * call each. A byte string is flat ([[FlatBytes]]: one chunk), so it is copied through the window a window at a time
   * from any offset, in O(1) per copy: nothing longer than the window is copied out or materialised, whatever its
   * length, and a lazy chunk (`ByteVector.fill`) is read a window at a time. Both mutable parts are created per call
   * and never escape.
   */
  final private class Absorbing(val digest: ExtendedDigest, window: ByteBuffer) {

    def flush(): Unit = {
      digest.update(window.array, 0, window.position)
      window.clear()
      ()
    }

    def byte(b: Int): Unit = {
      if (!window.hasRemaining) flush()
      window.put(b.toByte)
      ()
    }

    def bytes(b: FlatBytes): Unit = from(b, 0L)

    /** `b` from `offset` on, the window's room at a time. */
    @tailrec
    private def from(b: FlatBytes, offset: Long): Unit =
      if (offset < b.size) {
        val n = math.min(b.size - offset, window.remaining.toLong).toInt
        b.copyToArray(window.array, window.position, offset, n)
        window.position(window.position + n)
        if (!window.hasRemaining) flush()
        from(b, offset + n)
      }
  }

  private object Absorbing {

    implicit val sink: ByteSink[Absorbing] = new ByteSink[Absorbing] {
      def byte(s: Absorbing, b: Int): Absorbing = {
        s.byte(b)
        s
      }

      def bytes(s: Absorbing, b: FlatBytes): Absorbing = {
        s.bytes(b)
        s
      }
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
