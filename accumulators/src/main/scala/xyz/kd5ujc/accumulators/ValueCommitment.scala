package xyz.kd5ujc.accumulators

import cats.syntax.bifunctor._

import xyz.kd5ujc.core.canonical.{ByteSink, ByteSource, CanonicalValue, Cbor, FlatBytes}

import scodec.bits.ByteVector

/**
 * The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2, trie.md section 3):
 * `0x00 ‖ b` for a byte string `b`, `0x01 ‖ cbor(v)` for any other value. The tag keeps the mapping injective: without
 * it, the bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) would commit to the same leaf, and a proof of one
 * would verify a claim of the other. The byte-level APIs (`MerkleHashing.leaf`, `Trie.putValue`) take bytes as they
 * are, for structures whose leaves are opaque bytes, such as RFC 9162 logs.
 */
object ValueCommitment {

  val BytesTag: Byte = 0x00
  val CborTag: Byte = 0x01

  /** The commitment, not yet built: hashing it (`MerkleHashing.leafOf`, `TrieHashing.valueOf`) streams it. */
  def source(v: CanonicalValue): ByteSource =
    v match {
      case CanonicalValue.CBytes(b) => ByteSource.byte(BytesTag.toInt) ++ ByteSource.bytes(b)
      case other                    => ByteSource.byte(CborTag.toInt) ++ Cbor.source(other)
    }

  /**
   * The commitment, built: [[bytesWithin]] at `Cbor.MaxEncodedBytes`, the longest array a value's encoding is built
   * into. O(its length) memory: to hash it, stream it ([[source]]).
   */
  def bytes(v: CanonicalValue): Either[Cbor.TooLarge, ByteVector] = bytesWithin(v, Cbor.MaxEncodedBytes)

  /**
   * The commitment, if it is at most `maxBytes` long, tag included (a negative limit is 0, and no limit is above
   * `Cbor.MaxEncodedBytes`), with the bounds of `Cbor.encodeWithin`: refused before a byte is written, and the refusal
   * names the limit as applied. A byte string's commitment is its bytes and the tag; any other value's body is budgeted
   * a byte short, for the tag.
   */
  def bytesWithin(v: CanonicalValue, maxBytes: Long): Either[Cbor.TooLarge, ByteVector] = {
    val limit = maxBytes.max(0L).min(Cbor.MaxEncodedBytes)
    val refused = Cbor.TooLarge(limit)
    v match {
      case CanonicalValue.CBytes(b) => Either.cond(b.size < limit, FlatBytes(ByteVector(BytesTag) ++ b), refused)
      case other =>
        if (limit == 0L) Left(refused)
        else Cbor.encodeInto(other, limit - 1, ByteSink.emptyBuffer :+ CborTag).bimap(_ => refused, FlatBytes(_))
    }
  }
}
