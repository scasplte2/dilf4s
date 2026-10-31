package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

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
}

object VersionKey {

  def apply[V](implicit V: VersionKey[V]): VersionKey[V] = V

  /** Non-negative `Long` as `width` unsigned big-endian bytes, `1 <= width <= 8`; values must fit. */
  def unsignedLong(width: Int): Either[VersionedTrieError, VersionKey[Long]] =
    Either.cond(width >= 1 && width <= 8, longOf(width), VersionedTrieError.InvalidVersion(s"a Long version key is 1..8 bytes, not $width"))

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
