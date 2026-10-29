package xyz.kd5ujc.storage

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{Canonical, CanonicalValue, Cbor, Utf8}
import xyz.kd5ujc.core.hash.{Digest, Hasher}

import io.circe.{Decoder, Encoder}
import scodec.bits.ByteVector

/** Unsigned lexicographic order on bytes: the order every backend iterates in. */
object ByteOrder {
  implicit val unsigned: Ordering[ByteVector] = (a: ByteVector, b: ByteVector) => Cbor.compareBytes(a, b)
}

/**
 * Encodes keys to bytes **preserving order**: `compare(a, b)` and the unsigned comparison of `encode(a)` and `encode(b)`
 * have the same sign, so range scans follow the key type's natural order.
 */
trait KeyCodec[K] {
  def encode(key:   K): ByteVector
  def decode(bytes: ByteVector): Either[StoreError, K]
}

object KeyCodec {

  def apply[K](implicit K: KeyCodec[K]): KeyCodec[K] = K

  /** A codec whose encodings all have the same width, so it can lead a composite key without a terminator. */
  trait Fixed[K] extends KeyCodec[K] { def width: Long }

  def instance[K](enc: K => ByteVector, dec: ByteVector => Either[StoreError, K]): KeyCodec[K] =
    new KeyCodec[K] {
      def encode(key:   K): ByteVector = enc(key)
      def decode(bytes: ByteVector): Either[StoreError, K] = dec(bytes)
    }

  def fixed[K](w: Long, enc: K => ByteVector, dec: ByteVector => Either[StoreError, K]): Fixed[K] =
    new Fixed[K] {
      val width: Long = w
      def encode(key: K): ByteVector = enc(key)
      def decode(bytes: ByteVector): Either[StoreError, K] =
        Either.cond(bytes.size == w, bytes, StoreError.Codec("key", s"expected $w bytes, got ${bytes.size}")).flatMap(dec)
    }

  implicit val bytesKey: KeyCodec[ByteVector] = instance(identity, _.asRight)

  /** UTF-8 byte order is code-point order. */
  implicit val stringKey: KeyCodec[String] = instance(
    s => ByteVector(s.getBytes(UTF_8)),
    b => Either.cond(Utf8.isWellFormed(b.toArray), new String(b.toArray, UTF_8), StoreError.Codec("key", "invalid UTF-8"))
  )

  /** Big-endian with the sign bit flipped, so negative values sort before positive ones. */
  implicit val longKey: Fixed[Long] = fixed(8L, l => ByteVector.fromLong(l ^ Long.MinValue), b => (b.toLong() ^ Long.MinValue).asRight)

  implicit val intKey: Fixed[Int] = fixed(4L, i => ByteVector.fromInt(i ^ Int.MinValue), b => (b.toInt() ^ Int.MinValue).asRight)

  implicit def digestKey[H](implicit H: Hasher[H]): Fixed[Digest[H]] =
    fixed(H.outputSize, _.bytes, b => Digest.fromBytes[H](b).leftMap(e => StoreError.Codec("key", e.toString)))

  /** Composite key led by a fixed-width component: plain concatenation preserves the lexicographic order of pairs. */
  implicit def tuple2Key[A, B](implicit A: Fixed[A], B: KeyCodec[B]): KeyCodec[(A, B)] =
    instance(
      { case (a, b) => A.encode(a) ++ B.encode(b) },
      bytes => (A.decode(bytes.take(A.width)), B.decode(bytes.drop(A.width))).tupled
    )

  /**
   * Order-preserving, prefix-free encoding for variable-length bytes (the FoundationDB tuple-layer technique): `0x00`
   * becomes `0x00 0xff`, and the encoding ends with `0x00 0x00`. Lets a variable-length component lead a composite
   * key.
   */
  object Terminated {

    def encode(bytes: ByteVector): ByteVector = escape(bytes, 0L, ByteVector.empty) ++ ByteVector(0, 0)

    /** Splits `bytes` into the unescaped first component and the remainder after its terminator. */
    def decode(bytes: ByteVector): Either[StoreError, (ByteVector, ByteVector)] = unescape(bytes, 0L, ByteVector.empty)

    @tailrec
    private def escape(in: ByteVector, i: Long, acc: ByteVector): ByteVector =
      in.indexOfSlice(ByteVector(0), i) match {
        case -1 => acc ++ in.drop(i)
        case j  => escape(in, j + 1, acc ++ in.slice(i, j) ++ ByteVector(0, 0xff))
      }

    @tailrec
    private def unescape(in: ByteVector, i: Long, acc: ByteVector): Either[StoreError, (ByteVector, ByteVector)] =
      in.indexOfSlice(ByteVector(0), i) match {
        case -1                                               => StoreError.Codec("key", "unterminated component").asLeft
        case j if j + 1 < in.size && in(j + 1) == 0xff.toByte => unescape(in, j + 2, acc ++ in.slice(i, j) ++ ByteVector(0))
        case j if j + 1 < in.size && in(j + 1) == 0           => (acc ++ in.slice(i, j), in.drop(j + 2)).asRight
        case _                                                => StoreError.Codec("key", "invalid escape").asLeft
      }
  }
}

/** Encodes values as canonical CBOR (docs/specs/canonical-encoding.md), or passes opaque bytes through. */
trait ValueCodec[V] {
  def encode(value: V): Either[StoreError, ByteVector]
  def decode(bytes: ByteVector): Either[StoreError, V]
}

object ValueCodec {

  def apply[V](implicit V: ValueCodec[V]): ValueCodec[V] = V

  implicit val bytesValue: ValueCodec[ByteVector] = new ValueCodec[ByteVector] {
    def encode(value: ByteVector): Either[StoreError, ByteVector] = value.asRight
    def decode(bytes: ByteVector): Either[StoreError, ByteVector] = bytes.asRight
  }

  implicit val canonicalValue: ValueCodec[CanonicalValue] = new ValueCodec[CanonicalValue] {
    def encode(value: CanonicalValue): Either[StoreError, ByteVector] = Cbor.encode(value).asRight
    def decode(bytes: ByteVector): Either[StoreError, CanonicalValue] =
      Cbor.decode(bytes).leftMap(e => StoreError.Codec("value", e.toString))
  }

  /** Canonical CBOR through the type's circe codecs. */
  def fromCirce[V: Encoder: Decoder]: ValueCodec[V] = new ValueCodec[V] {
    private val canonical = Canonical.fromEncoder[V]
    def encode(value: V): Either[StoreError, ByteVector] =
      canonical.canonical(value).bimap(e => StoreError.Codec("value", e.toString), Cbor.encode)
    def decode(bytes: ByteVector): Either[StoreError, V] =
      Cbor.decode(bytes).flatMap(Canonical.decode[V]).leftMap(e => StoreError.Codec("value", e.toString))
  }
}
