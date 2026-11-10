package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.KeyMode

import scodec.bits.ByteVector

/**
 * How a version becomes a history-trie key (docs/specs/versioned-trie.md §3): a fixed `width` and an unsigned big-endian,
 * order-preserving encoding. The law is that comparing versions and comparing their encodings as unsigned bytes always
 * agree, so the store orders versions by their keys and a range of keys is a range of versions.
 *
 * The default, for non-negative `Long`, is 8 bytes: the JLVM's `int_to_bytes [v, 8]`. It is not `KeyCodec.longKey`,
 * which flips the sign bit so that negative values sort first, and so would give a program a different key.
 */
trait VersionKey[V] {
  def width: Int
  def encode(v:     V): Either[VersionedTrieError, ByteVector]
  def decode(bytes: ByteVector): Either[VersionedTrieError, V]

  /**
   * `vkey(v)`, checked: [[encode]]'s bytes, refused as `InvalidVersion` unless they are exactly `width` bytes and the
   * width is one the history trie keys (1 to `KeyMode.MaxFixedBytes`). Every path of this package encodes through it,
   * so a version key of the caller's own never lengthens the key a refusal names (its [[versionHex]], at most twice
   * `KeyMode.MaxFixedBytes` characters).
   */
  final def keyOf(v: V): Either[VersionedTrieError, ByteVector] =
    if (width < 1 || width > KeyMode.MaxFixedBytes)
      VersionedTrieError.InvalidVersion(s"a version key's width must be 1..${KeyMode.MaxFixedBytes}, not $width").asLeft
    else encode(v).ensureOr(k => VersionedTrieError.InvalidVersion(s"a version key is $width bytes, got ${k.size}"))(_.size == width.toLong)
}

object VersionKey {

  def apply[V](implicit V: VersionKey[V]): VersionKey[V] = V

  /** Non-negative `Long` as `width` unsigned big-endian bytes, `1 <= width <= 8`; values must fit. */
  def unsignedLong(width: Int): Either[VersionedTrieError, VersionKey[Long]] =
    Either.cond(width >= 1 && width <= 8, longOf(width), VersionedTrieError.InvalidVersion(s"a Long version key is 1..8 bytes, not $width"))

  /**
   * A version that is its own key: `width` bytes (1 to `KeyMode.MaxFixedBytes`), in unsigned byte order. The Rust SDK's
   * `BytesKey` and the TypeScript SDK's `VersionKey.bytes`; the conformance vectors' wide version keys.
   */
  def bytes(width: Int): Either[VersionedTrieError, VersionKey[ByteVector]] =
    Either.cond(
      width >= 1 && width <= KeyMode.MaxFixedBytes,
      bytesOf(width),
      VersionedTrieError.InvalidVersion(s"a bytes version key is 1..${KeyMode.MaxFixedBytes} bytes, not $width")
    )

  private def bytesOf(w: Int): VersionKey[ByteVector] = new VersionKey[ByteVector] {
    val width: Int = w

    def encode(v: ByteVector): Either[VersionedTrieError, ByteVector] =
      Either.cond(v.size == w.toLong, v, VersionedTrieError.InvalidVersion(s"a version key is $w bytes, got ${v.size}"))

    def decode(bytes: ByteVector): Either[VersionedTrieError, ByteVector] = encode(bytes)
  }

  private def longOf(w: Int): VersionKey[Long] = new VersionKey[Long] {
    val width: Int = w
    // the largest version that fits: 2^(8w) - 1, capped at Long.MaxValue for w = 8
    private val max: Long = if (w == 8) Long.MaxValue else (1L << (8 * w)) - 1L

    def encode(v: Long): Either[VersionedTrieError, ByteVector] =
      Either.cond(
        v >= 0L && v <= max,
        ByteVector.fromLong(v).takeRight(w.toLong),
        VersionedTrieError.InvalidVersion(s"version $v is outside 0..$max")
      )

    def decode(bytes: ByteVector): Either[VersionedTrieError, Long] =
      if (bytes.size != w.toLong) VersionedTrieError.InvalidVersion(s"a version key is $w bytes, got ${bytes.size}").asLeft
      else
        bytes
          .toLong(signed = false)
          .asRight[VersionedTrieError]
          .ensure(VersionedTrieError.InvalidVersion(s"version ${bytes.toHex} exceeds $max"))(v => v >= 0L && v <= max)
  }

  implicit val long: VersionKey[Long] = longOf(8)
}
