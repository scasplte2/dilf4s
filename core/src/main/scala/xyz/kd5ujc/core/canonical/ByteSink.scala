package xyz.kd5ujc.core.canonical

import scodec.bits.ByteVector

/**
 * Where bytes written piece by piece go, as a state `S` threaded through every write: a buffer that keeps them, or a
 * hash that absorbs them. A writer uses each state once, passing on the one a write returns, so a sink may absorb in
 * place; [[ByteSource]] is the only writer, and its writers are this library's. Byte strings arrive flat
 * ([[FlatBytes]]): one chunk, which a sink can copy from at any offset in O(1).
 */
trait ByteSink[S] {
  def byte(s:  S, b: Int): S
  def bytes(s: S, b: FlatBytes): S
}

object ByteSink {

  /** Keeps the bytes: appending is amortised O(1) per byte, and a byte string is appended without copying. */
  implicit val buffer: ByteSink[ByteVector] = new ByteSink[ByteVector] {
    def byte(s:  ByteVector, b: Int): ByteVector = s :+ b.toByte
    def bytes(s: ByteVector, b: FlatBytes): ByteVector = s ++ b
  }

  /** The buffer to start from: appends share a chunk this long instead of building a node per piece. */
  private[kd5ujc] def emptyBuffer: ByteVector = ByteVector.empty.bufferBy(BufferChunk)

  private val BufferChunk = 4096
}

/**
 * Bytes not yet built, written piece by piece into any [[ByteSink]]: an encoding streamed into a hash never exists
 * whole. A source is a sequence of pieces, written in order by one loop, so concatenating any number of sources stays
 * stack-safe. Each piece uses each sink state once; only this library makes pieces (their constructor is
 * package-private), so every source keeps to that, which lets a hash absorb in place.
 */
final class ByteSource private (private val pieces: Vector[ByteSource.Piece]) {

  def writeTo[S](s: S)(implicit sink: ByteSink[S]): S = pieces.foldLeft(s)((acc, piece) => piece.writeTo(acc))

  /** This source's bytes, then `next`'s. */
  def ++(next: ByteSource): ByteSource = new ByteSource(pieces ++ next.pieces)

  /**
   * The bytes, built into one array: O(their length) memory, so a hash takes the source itself instead. Unbounded: a
   * source has no weight to refuse from, so only this library's tests build one whole; a value's encoding is built
   * within a limit by `Cbor.encodeWithin`.
   */
  private[canonical] def toByteVector: FlatBytes = FlatBytes(writeTo(ByteSink.emptyBuffer))
}

object ByteSource {

  /** One run of bytes, written by one call: a byte, a byte string, or an encoding walked by its own explicit stack. */
  abstract class Piece private[kd5ujc] () {
    def writeTo[S](s: S)(implicit sink: ByteSink[S]): S
  }

  private[kd5ujc] def of(piece: Piece): ByteSource = new ByteSource(Vector(piece))

  val empty: ByteSource = new ByteSource(Vector.empty)

  def byte(b: Int): ByteSource =
    of(new Piece {
      def writeTo[S](s: S)(implicit sink: ByteSink[S]): S = sink.byte(s, b)
    })

  /** The bytes of `b`, flattened once, here ([[FlatBytes]]: O(1) when `b` already is, as every canonical byte string is). */
  def bytes(b: ByteVector): ByteSource = {
    val flat = FlatBytes(b)
    of(new Piece {
      def writeTo[S](s: S)(implicit sink: ByteSink[S]): S = sink.bytes(s, flat)
    })
  }
}
