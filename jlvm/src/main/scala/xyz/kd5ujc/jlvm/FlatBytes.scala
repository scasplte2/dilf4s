package xyz.kd5ujc.jlvm

import scodec.bits.ByteVector

/**
 * Bytes held as one contiguous chunk, so reading any byte is O(1) and never recurses. scodec's `ByteVector` is a rope:
 * a program that prepends in a loop (`reduce` over `cat`) would otherwise build one about `sqrt(2 * gas)` levels deep,
 * and every indexed read (`in`, `==`) would then walk, and recurse through, every level, work no size charge prices.
 *
 * Every byte string an operation produces is made through [[Values.bytes]] or [[Values.bytesValue]], which take only
 * `FlatBytes`, and every operation that reads bytes by content takes them as `FlatBytes` too. The type cannot reach
 * further: values are core's `CBytes(ByteVector)`, which any code may build, and core's equality compares the bytes
 * nested in arrays and objects as they are. Bytes from the host (data, bindings, decoded programs) are flat when decoded
 * from a flat buffer, as `Cbor.decode` and `0x` literals are (`Jlvm.evaluate` states the precondition).
 *
 * "One chunk" is scodec's notion, and covers an array (or a slice of one), which is what every operation, the parser and
 * the decoders make. It also covers a host's lazy view (`ByteVector.viewAt`, `map`, `reverse`), whose reads run host
 * code that no public scodec API can tell apart from an array, or copy without running: such bytes are outside the
 * precondition, and nothing a program does can make them.
 */
opaque type FlatBytes <: ByteVector = ByteVector

object FlatBytes {

  /**
   * `b` itself when it is one chunk (O(1)); otherwise one stack-safe copy, linear in its size and its number of
   * segments, which is at most its size. Callers flatten only what the gas schedule already charged by size, or what
   * they checked to have a fixed width.
   */
  def apply(b: ByteVector): FlatBytes = b.compact

  /** The parts end to end, copied once into a fresh array. The caller has charged, and bounded, their total size. */
  def concat(parts: Vector[ByteVector]): FlatBytes =
    parts match {
      case Vector(one) => apply(one)
      case _           => ByteVector.view(Array.concat(parts.map(_.toArrayUnsafe)*))
    }

  /** Whether `b` is already one chunk, which [[apply]] then returns unchanged. */
  def isFlat(b: ByteVector): Boolean = b.compact eq b
}
