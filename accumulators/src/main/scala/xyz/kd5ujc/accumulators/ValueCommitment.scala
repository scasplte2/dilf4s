package xyz.kd5ujc.accumulators

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

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

  def bytes(v: CanonicalValue): ByteVector =
    v match {
      case CanonicalValue.CBytes(b) => BytesTag +: b
      case other                    => CborTag +: Cbor.encode(other)
    }
}
