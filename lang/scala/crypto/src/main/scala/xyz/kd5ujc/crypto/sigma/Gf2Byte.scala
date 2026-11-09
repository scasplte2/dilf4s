package xyz.kd5ujc.crypto.sigma

/**
 * An element of GF(2^192) of degree below 8, i.e. one byte `0..255`: the points at which threshold children's
 * challenges are evaluated. Distinct from [[Gf2_192]] so that `Gf2_192 * Gf2Byte`, which takes 8 shift-and-add steps
 * instead of 192, can only be given an operand that fits in a byte. Sums of bytes stay bytes.
 */
opaque type Gf2Byte = Int

object Gf2Byte {

  /** The parent's point: a threshold node's own challenge is its polynomial's value here. */
  val Zero: Gf2Byte = 0

  /** `b` when it is a byte `0..255`; nothing wider is truncated into one. */
  def from(b: Int): Option[Gf2Byte] = Option.when((b & ~0xff) == 0)(b)

  /** The unsigned value of `b`. */
  def fromByte(b: Byte): Gf2Byte = b & 0xff

  extension (b: Gf2Byte) {
    def value: Int = b
    def +(c: Gf2Byte): Gf2Byte = b ^ c
  }
}

/**
 * A threshold child's point, `1..255`: its 1-based index among its siblings. Never zero, the parent's point, so a
 * child's challenge can never be the parent's; the only way to get one is [[ChildPoint.number]], which refuses a 256th
 * child rather than wrap it around to zero.
 */
opaque type ChildPoint <: Gf2Byte = Int

object ChildPoint {

  /** The most children a connective may have: the nonzero bytes. */
  val Max: Int = xyz.kd5ujc.models.limits.SigmaMaxChildren

  /** `i` when it is a child's point, `1..`[[Max]]. */
  def from(i: Int): Option[ChildPoint] = Option.when(i >= 1 && i <= Max)(i)

  /** Each child with its point `1..n`, or `Left` when there are more than [[Max]]. */
  def number[A](children: Vector[A]): Either[String, Vector[(A, ChildPoint)]] =
    Either.cond(
      children.size <= Max,
      children.zipWithIndex.map { case (c, i) => (c, i + 1) },
      s"a connective has at most $Max children"
    )
}
