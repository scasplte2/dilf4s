package xyz.kd5ujc.core.canonical

import scodec.bits.ByteVector

/**
 * Bytes held as one chunk, so reading any byte is O(1), slicing never rebuilds anything, and a traversal visits one
 * chunk: scodec's `ByteVector` is otherwise a rope, and a rope built by prepending (`x ++ acc` in a loop) is as deep as
 * it has pieces, so every read, slice or traversal of it walks every level. Canonical byte strings hold only these
 * ([[CanonicalValue.CBytes]] compacts what it is given), so no reader, encoder or hash of a value depends on how its
 * bytes were built.
 *
 * "One chunk" is scodec's notion: an array or a slice of one, which is what decoding, the encoder and the JLVM make, or
 * a host's lazy view (`ByteVector.fill`, `low`, `view(at, ...)`), whose reads run host code and which stays lazy.
 */
opaque type FlatBytes <: ByteVector = ByteVector

object FlatBytes {

  /**
   * `b` itself when it is one chunk (O(1)); otherwise one stack-safe copy into a fresh array, linear in its size and
   * its number of chunks, which is at most its size. A producer flattens only what it already paid for by size.
   */
  def apply(b: ByteVector): FlatBytes = b.compact

  /** A fresh array's bytes, which are one chunk as they are: O(1), no copy. */
  def view(a: Array[Byte]): FlatBytes = ByteVector.view(a)

  /** The parts end to end, copied once into a fresh array. The caller has charged, and bounded, their total size. */
  def concat(parts: Vector[ByteVector]): FlatBytes =
    parts match {
      case Vector(one) => apply(one)
      case _           => ByteVector.view(Array.concat(parts.map(_.toArrayUnsafe)*))
    }

  /** Whether `b` is already one chunk, which [[apply]] then returns unchanged. */
  def isFlat(b: ByteVector): Boolean = b.compact eq b
}
