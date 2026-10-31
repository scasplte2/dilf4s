package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import scala.annotation.tailrec

import scodec.bits.ByteVector

/**
 * An element of GF(2^192), modulo the irreducible pentanomial x^192 + x^7 + x^2 + x + 1: the field in which sigma
 * protocol challenges are split among threshold children. Bit `i` of the element is the coefficient of `x^i`; the
 * 24-byte encoding is little-endian, so byte `j` holds bits `8j .. 8j+7` (Ergo's `GF2_192`, by Leonid Reyzin).
 *
 * The same 24 bytes are also a challenge, read as a big-endian scalar by the leaves: the bytes, not either reading, are
 * the canonical object.
 */
final case class Gf2_192 private (w0: Long, w1: Long, w2: Long) {

  def +(that: Gf2_192): Gf2_192 = Gf2_192(w0 ^ that.w0, w1 ^ that.w1, w2 ^ that.w2)

  def isZero: Boolean = w0 == 0L && w1 == 0L && w2 == 0L

  /** Shift-and-add over the 192 bits of `that`, from the highest; each step multiplies the accumulator by x. */
  def *(that: Gf2_192): Gf2_192 = {
    @tailrec
    def go(bit: Int, acc: Gf2_192): Gf2_192 =
      if (bit < 0) acc
      else {
        val shifted = acc.timesX
        go(bit - 1, if (that.testBit(bit)) shifted + this else shifted)
      }
    go(191, Gf2_192.Zero)
  }

  /** The multiplicative inverse, `this^(2^192 - 2)`; zero has none and maps to zero. */
  def inverse: Gf2_192 = {
    // 2^192 - 2 = 2 + 4 + ... + 2^191: the product of the squares this^(2^i) for i in 1..191
    @tailrec
    def go(i: Int, square: Gf2_192, acc: Gf2_192): Gf2_192 =
      if (i > 191) acc
      else {
        val next = square * square
        go(i + 1, next, acc * next)
      }
    go(1, this, Gf2_192.One)
  }

  def bytes: ByteVector = Gf2_192.word(w0) ++ Gf2_192.word(w1) ++ Gf2_192.word(w2)

  private def testBit(i: Int): Boolean = {
    val w = if (i < 64) w0 else if (i < 128) w1 else w2
    ((w >>> (i & 63)) & 1L) == 1L
  }

  /** Multiplication by x: shift left one bit, folding x^192 back in as x^7 + x^2 + x + 1. */
  private def timesX: Gf2_192 = {
    val carry = w2 >>> 63
    Gf2_192((w0 << 1) ^ (Gf2_192.Pentanomial * carry), (w1 << 1) | (w0 >>> 63), (w2 << 1) | (w1 >>> 63))
  }
}

object Gf2_192 {

  val Size: Int = 24

  val Zero: Gf2_192 = Gf2_192(0L, 0L, 0L)
  val One: Gf2_192 = Gf2_192(1L, 0L, 0L)

  private val Pentanomial: Long = (1L << 7) | (1L << 2) | (1L << 1) | 1L

  /** The element with value `b` (`0..255`): the points at which threshold children's challenges are evaluated. */
  def byte(b: Int): Gf2_192 = Gf2_192((b & 0xff).toLong, 0L, 0L)

  /** Exactly 24 bytes, little-endian. */
  def fromBytes(b: ByteVector): Option[Gf2_192] =
    Option.when(b.size == Size.toLong)(Gf2_192(little(b.take(8)), little(b.slice(8, 16)), little(b.drop(16))))

  private def little(b: ByteVector): Long = b.reverse.toLong(signed = false)

  private def word(w: Long): ByteVector = ByteVector.fromLong(w).reverse
}

/**
 * A polynomial over GF(2^192), lowest-degree coefficient first. A threshold node's challenges are its evaluations at
 * 1, 2, ..., n; the node's own challenge is the value at 0.
 */
final case class Gf2Poly(coefficients: Vector[Gf2_192]) {

  def degree: Int = math.max(coefficients.size - 1, 0)

  /** Horner's rule at the point `x` (`0..255`). */
  def apply(x: Int): Gf2_192 = {
    val point = Gf2_192.byte(x)
    coefficients.foldRight(Gf2_192.Zero)((c, acc) => acc * point + c)
  }

  /** Coefficients 1 and up, 24 bytes each, lowest first: the proof form (coefficient 0 is the parent's challenge). */
  def higherBytes: ByteVector = coefficients.drop(1).foldLeft(ByteVector.empty)(_ ++ _.bytes)
}

object Gf2Poly {

  /** Coefficient 0 and the 24-byte coefficients from degree 1 up. */
  def fromBytes(coefficient0: Gf2_192, higher: ByteVector): Option[Gf2Poly] =
    Option
      .when(higher.size % Gf2_192.Size == 0)(higher.grouped(Gf2_192.Size.toLong).toVector)
      .flatMap(_.traverse(Gf2_192.fromBytes))
      .map(cs => Gf2Poly(coefficient0 +: cs))

  /**
   * The unique polynomial of degree at most `points.size` through `(0, valueAt0)` and the given `(x, y)` points, with
   * distinct nonzero `x`. Its representation always has `points.size + 1` coefficients (leading zeros kept), as the
   * proof form fixes the count.
   */
  def interpolate(valueAt0: Gf2_192, points: Vector[(Int, Gf2_192)]): Gf2Poly = {
    val all = (0, valueAt0) +: points
    val size = all.size
    // Lagrange: sum over j of y_j * prod over m != j of (X + x_m) / (x_j + x_m); in characteristic 2, minus is plus
    val sum = all.zipWithIndex.foldLeft(Vector.fill(size)(Gf2_192.Zero)) {
      case (acc, ((xj, yj), j)) =>
        val others = all.zipWithIndex.collect { case ((xm, _), m) if m != j => xm }
        val numerator = others.foldLeft(Vector(Gf2_192.One))((p, xm) => timesMonomial(p, Gf2_192.byte(xm)))
        val scale = yj * others.foldLeft(Gf2_192.One)((d, xm) => d * (Gf2_192.byte(xj) + Gf2_192.byte(xm))).inverse
        acc.zipAll(numerator.map(_ * scale), Gf2_192.Zero, Gf2_192.Zero).map { case (a, b) => a + b }
    }
    Gf2Poly(sum.padTo(size, Gf2_192.Zero))
  }

  /** `p * (X + r)`. */
  private def timesMonomial(p: Vector[Gf2_192], r: Gf2_192): Vector[Gf2_192] =
    (Gf2_192.Zero +: p).zipAll(p.map(_ * r), Gf2_192.Zero, Gf2_192.Zero).map { case (a, b) => a + b }

}
