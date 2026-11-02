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

  /**
   * Shift-and-add over the 192 bits of `that`, from the highest. Always 192 steps, and each bit selects `this` by mask,
   * not by branch: no branch depends on either operand's value (the prover multiplies values derived from its secrets).
   */
  def *(that: Gf2_192): Gf2_192 = shiftAndAdd(that.w0, that.w1, that.w2, 191)

  /**
   * The same shift-and-add over only the 8 bits of `b`: the general multiply's first 184 steps shift a zero
   * accumulator, so skipping them changes nothing. Always 8 steps, selecting by mask in the same way.
   */
  def *(b: Gf2Byte): Gf2_192 = shiftAndAdd(b.value.toLong, 0L, 0L, 7)

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

  /**
   * `this` times the multiplier `m2:m1:m0`, over its bits `top` down to 0 (the bits above `top` must be zero). Each step
   * multiplies the accumulator by x, folding x^192 back in as x^7 + x^2 + x + 1, and adds `this` masked by the bit; the
   * word read is chosen by the step, not by any value. On bare words, so no step allocates.
   */
  private def shiftAndAdd(m0: Long, m1: Long, m2: Long, top: Int): Gf2_192 = {
    @tailrec
    def go(bit: Int, a0: Long, a1: Long, a2: Long): Gf2_192 =
      if (bit < 0) Gf2_192(a0, a1, a2)
      else {
        val word = if (bit < 64) m0 else if (bit < 128) m1 else m2
        val mask = -((word >>> (bit & 63)) & 1L)
        val carry = a2 >>> 63
        go(
          bit - 1,
          (a0 << 1) ^ (Gf2_192.Pentanomial * carry) ^ (w0 & mask),
          ((a1 << 1) | (a0 >>> 63)) ^ (w1 & mask),
          ((a2 << 1) | (a1 >>> 63)) ^ (w2 & mask)
        )
      }
    go(top, 0L, 0L, 0L)
  }
}

object Gf2_192 {

  val Size: Int = 24

  val Zero: Gf2_192 = Gf2_192(0L, 0L, 0L)
  val One: Gf2_192 = Gf2_192(1L, 0L, 0L)

  private val Pentanomial: Long = (1L << 7) | (1L << 2) | (1L << 1) | 1L

  /** The element with value `b`, widened: `x * byte(b) == x * b`, the 192-step way. */
  def byte(b: Gf2Byte): Gf2_192 = Gf2_192(b.value.toLong, 0L, 0L)

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

  /** Horner's rule at the point `x`. */
  def apply(x: Gf2Byte): Gf2_192 = coefficients.foldRight(Gf2_192.Zero)((c, acc) => acc * x + c)

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
   * The unique polynomial of degree at most `points.size` through `(0, valueAt0)` and the given points. The points'
   * types make them nonzero and the map makes them distinct, so every denominator below is nonzero. Its representation
   * always has `points.size + 1` coefficients (leading zeros kept), as the proof form fixes the count.
   *
   * Lagrange's form `sum_j w_j * M(X) / (X + x_j)`, with `M(X) = prod_m (X + x_m)` and weights
   * `w_j = y_j / prod_{m != j} (x_j + x_m)` (in characteristic 2, minus is plus), regrouped so its cost is quadratic,
   * not cubic. Coefficient `i` of `M(X) / (X + x_j)` is `sum_{t > i} M_t x_j^(t-1-i)`, so coefficient `i` of the sum is
   * `sum_s M_(i+1+s) P_s` with the power sums `P_s = sum_j w_j x_j^s`. `M`, the denominators and the power sums take
   * only byte multiplies; the denominators are inverted together, with one inversion (Montgomery's trick).
   */
  def interpolate(valueAt0: Gf2_192, points: Map[ChildPoint, Gf2_192]): Gf2Poly = {
    val all: Vector[(Gf2Byte, Gf2_192)] = (Gf2Byte.Zero, valueAt0) +: points.toVector
    val xs = all.map(_._1)
    val size = all.size
    val master = xs.foldLeft(Vector(Gf2_192.One))(timesMonomial)
    val denominators = xs.map(xj => xs.foldLeft(Gf2_192.One)((d, xm) => if (xm == xj) d else d * (xj + xm)))
    val weights = all.map(_._2).zip(invertAll(denominators)).map { case (y, inv) => y * inv }
    val powerSums = Vector.iterate(weights, size)(_.zip(xs).map { case (w, x) => w * x }).map(_.foldLeft(Gf2_192.Zero)(_ + _))
    Gf2Poly(Vector.tabulate(size) { i =>
      master.drop(i + 1).zip(powerSums).foldLeft(Gf2_192.Zero) { case (acc, (m, p)) => acc + m * p }
    })
  }

  /** Every inverse with one inversion: prefix products, then back down (Montgomery's trick); none may be zero. */
  private def invertAll(ds: Vector[Gf2_192]): Vector[Gf2_192] = {
    val prefixes = ds.scanLeft(Gf2_192.One)(_ * _)
    val all = prefixes.lastOption.getOrElse(Gf2_192.One).inverse
    // walking down, `acc` is the inverse of d_0 * ... * d_i; times d_0 * ... * d_(i-1) it is the inverse of d_i
    ds.zip(prefixes)
      .foldRight((all, List.empty[Gf2_192])) { case ((d, before), (acc, out)) => (acc * d, (acc * before) :: out) }
      ._2
      .toVector
  }

  /** `p * (X + r)`. */
  private def timesMonomial(p: Vector[Gf2_192], r: Gf2Byte): Vector[Gf2_192] =
    (Gf2_192.Zero +: p).zipAll(p.map(_ * r), Gf2_192.Zero, Gf2_192.Zero).map { case (a, b) => a + b }

}
