package xyz.kd5ujc.crypto.curve25519

import scala.annotation.tailrec

import cafe.cryptography.curve25519.Scalar
import scodec.bits.{ByteOrdering, ByteVector}

/**
 * A point of edwards25519 in extended coordinates `(X : Y : Z : T)`, `x = X/Z`, `y = Y/Z`, `xy = T/Z`; immutable.
 *
 * Verification only: everything here is **variable time** (wNAF digits, early exits, data-dependent table indices),
 * which is safe because only public data (keys, signatures, VRF proofs, hash outputs) ever reaches it. Secret scalars
 * stay on elisabeth's and BouncyCastle's constant-time code.
 */
final private[crypto] class Point(val x: Fe, val y: Fe, val z: Fe, val t: Fe) {

  def +(q: Point): Point = add(q.cached).toPoint
  def -(q: Point): Point = sub(q.cached).toPoint
  def negate: Point = new Point(x.negate, y, z, t.negate)

  /** `[8]P`. */
  def multiplyByCofactor(): Point = projective.double.toProjective.double.toProjective.double.toPoint

  def isIdentity: Boolean = x.isZero && y.isEqual(z)

  /** Of order dividing 8. */
  def isSmallOrder: Boolean = multiplyByCofactor().isIdentity

  /** RFC 8032 encoding: the canonical little-endian `y`, with the sign of `x` in bit 255. */
  def compress: ByteVector = encode(z.invert)

  private[curve25519] def encode(zInverse: Fe): ByteVector = {
    val b = (y * zInverse).bytes
    if ((x * zInverse).isNegative) b.update(31, (b(31) | 0x80).toByte) else b
  }

  private[curve25519] def projective: Projective = new Projective(x, y, z)
  private[curve25519] def cached: Cached = new Cached(y + x, y - x, z, t * Fe.D2)

  private[curve25519] def add(q: Cached): Completed = {
    val a = (y - x) * q.yMinusX
    val b = (y + x) * q.yPlusX
    val c = q.t2d * t
    val zz = z * q.z
    val d = zz + zz
    new Completed(b - a, b + a, d + c, d - c)
  }

  private[curve25519] def sub(q: Cached): Completed = {
    val a = (y - x) * q.yPlusX
    val b = (y + x) * q.yMinusX
    val c = q.t2d * t
    val zz = z * q.z
    val d = zz + zz
    new Completed(b - a, b + a, d - c, d + c)
  }

  private[curve25519] def addAffine(q: Niels): Completed = {
    val a = (y - x) * q.yMinusX
    val b = (y + x) * q.yPlusX
    val c = q.xy2d * t
    val d = z + z
    new Completed(b - a, b + a, d + c, d - c)
  }

  private[curve25519] def subAffine(q: Niels): Completed = {
    val a = (y - x) * q.yPlusX
    val b = (y + x) * q.yMinusX
    val c = q.xy2d * t
    val d = z + z
    new Completed(b - a, b + a, d - c, d + c)
  }
}

/** `((X : Z), (Y : T))`: the result of an addition or doubling, before it is converted (ref10's `p1p1`). */
final private[curve25519] class Completed(val x: Fe, val y: Fe, val z: Fe, val t: Fe) {
  def toPoint: Point = new Point(x * t, y * z, z * t, x * y)
  def toProjective: Projective = new Projective(x * t, y * z, z * t)
}

/** `(X : Y : Z)`, without `T`: what a doubling needs (ref10's `p2`). */
final private[curve25519] class Projective(val x: Fe, val y: Fe, val z: Fe) {

  def double: Completed = {
    val xx = x.square
    val yy = y.square
    val zz2 = z.square
    val b = zz2 + zz2
    val a = (x + y).square
    val sum = yy + xx
    val diff = yy - xx
    new Completed(a - sum, sum, diff, b - diff)
  }
}

/** `(Y + X, Y - X, Z, 2dT)`: a point prepared to be added (ref10's `cached`). */
final private[curve25519] class Cached(val yPlusX: Fe, val yMinusX: Fe, val z: Fe, val t2d: Fe)

/** `(y + x, y - x, 2dxy)`, affine: a point of the fixed basepoint table (ref10's `precomp`). */
final private[curve25519] class Niels(val yPlusX: Fe, val yMinusX: Fe, val xy2d: Fe)

private[crypto] object Point {

  val Identity: Point = new Point(Fe.Zero, Fe.One, Fe.One, Fe.Zero)

  /** RFC 8032's `B`: `y = 4/5`, `x` even. */
  val Basepoint: Point = {
    val x = Fe.fromBigInt(BigInt("15112221349535400772501151409588531511454012693041857206046113283949847762202"))
    val y = Fe.fromBigInt(BigInt("46316835694926478169428394003475163141307993866256225615783033603165251855960"))
    new Point(x, y, Fe.One, x * y)
  }

  /**
   * ZIP-215 decoding, exactly as curve25519-elisabeth's `CompressedEdwardsY.decompress` (and dalek's): bit 255 is the
   * sign of `x`, the rest is `y`, and `y` from p to 2^255 - 1 is reduced, not refused; the encoding is refused only
   * when `(y^2 - 1) / (d y^2 + 1)` has no square root. `x = 0` with the sign bit set decodes (to `x = 0`).
   */
  def decode(bytes: ByteVector): Option[Point] =
    Option.when(bytes.size == 32L)(bytes).flatMap { b =>
      val y = Fe.fromBytes(b)
      val yy = y.square
      sqrtRatio(yy - Fe.One, yy * Fe.D + Fe.One).map { r =>
        val x = if (r.isNegative == ((b(31) & 0x80) != 0)) r else r.negate
        new Point(x, y, Fe.One, x * y)
      }
    }

  /** A root `r` of `r^2 = u / v`, if there is one (`v` is never 0 here: `-1/d` is not a square). */
  private def sqrtRatio(u: Fe, v: Fe): Option[Fe] = {
    val v3 = v.square * v
    val r = (u * v3) * (u * v3.square * v).pow22523
    val check = v * r.square
    if (check.isEqual(u)) Some(r) else Option.when(check.isEqual(u.negate))(r * Fe.SqrtM1)
  }

  /** The encodings of four points, with one inversion (Montgomery's trick). */
  def compressBatch(p0: Point, p1: Point, p2: Point, p3: Point): (ByteVector, ByteVector, ByteVector, ByteVector) = {
    val z01 = p0.z * p1.z
    val z012 = z01 * p2.z
    val inverse = (z012 * p3.z).invert
    val inverse012 = inverse * p3.z
    val inverse01 = inverse012 * p2.z
    (p0.encode(inverse01 * p1.z), p1.encode(inverse01 * p0.z), p2.encode(inverse012 * z01), p3.encode(inverse * z012))
  }

  // ------------------------------------------------------------------ variable-time multi-scalar multiplication

  /**
   * The width-`w` NAF of a scalar (dalek's `non_adjacent_form`), one digit per bit position, index 0 the lowest: each
   * digit is 0 or odd, below `2^(w-1)` in magnitude, and nonzero digits are at least `w` apart. A [[Scalar]] holds a
   * value below `L < 2^253` (its reduced constructors; the unreduced bit constructor is refused by the build), so no carry is left
   * over at the top.
   */
  private def naf(scalar: Scalar, w: Int): IArray[Int] = {
    val bytes = ByteVector.view(scalar.toByteArray)
    def word(i: Int): Long =
      if (i < 4) bytes.slice(8L * i, 8L * i + 8).toLong(signed = false, ordering = ByteOrdering.LittleEndian) else 0L
    val words = IArray.tabulate(5)(word)
    val width = 1L << w
    // digits are prepended, so `digits` runs from the highest position down; a digit's next w - 1 positions are zero
    @tailrec def go(position: Int, carry: Long, digits: List[Int]): List[Int] =
      if (position >= 256) digits
      else {
        val i = position / 64
        val bit = position % 64
        val buffer = if (bit < 64 - w) words(i) >>> bit else (words(i) >>> bit) | (words(i + 1) << (64 - bit))
        val window = carry + (buffer & (width - 1))
        val zeros = math.min(w - 1, 255 - position)
        if ((window & 1L) == 0L) go(position + 1, carry, 0 :: digits)
        else if (window < width / 2) go(position + w, 0L, List.fill(zeros)(0) ++ (window.toInt :: digits))
        else go(position + w, 1L, List.fill(zeros)(0) ++ ((window - width).toInt :: digits))
      }
    IArray.from(go(0, 0L, Nil).reverse)
  }

  /** `[P, 3P, ..., 15P]`: the odd multiples a width-5 NAF digit selects. */
  private def oddMultiples(p: Point): IArray[Cached] = {
    val twice = p.projective.double.toPoint.cached
    IArray.iterate(p, 8)(q => q.add(twice).toPoint).map(_.cached)
  }

  /** `[B, 3B, ..., 127B]`, affine, for width-8 digits of the basepoint scalar; built once. */
  private val basepointTable: IArray[Niels] = {
    val twice = Basepoint.projective.double.toPoint.cached
    IArray.iterate(Basepoint, 64)(q => q.add(twice).toPoint).map { q =>
      val zi = q.z.invert
      val ax = q.x * zi
      val ay = q.y * zi
      new Niels(ay + ax, ay - ax, ax * ay * Fe.D2)
    }
  }

  private def withVariable(c: Completed, table: IArray[Cached], digit: Int): Completed =
    if (digit == 0) c else if (digit > 0) c.toPoint.add(table(digit / 2)) else c.toPoint.sub(table(-digit / 2))

  private def withBasepoint(c: Completed, digit: Int): Completed =
    if (digit == 0) c
    else if (digit > 0) c.toPoint.addAffine(basepointTable(digit / 2))
    else c.toPoint.subAffine(basepointTable(-digit / 2))

  private val IdentityCompleted: Completed = new Completed(Fe.Zero, Fe.One, Fe.One, Fe.One)

  /** The highest position where either digit array is nonzero, or -1. */
  @tailrec private def top(as: IArray[Int], bs: IArray[Int], position: Int): Int =
    if (position < 0 || as(position) != 0 || bs(position) != 0) position else top(as, bs, position - 1)

  /** `aP + bQ`: Straus, width-5 NAF for both. */
  def doubleScalarMultiply(a: Scalar, p: Point, b: Scalar, q: Point): Point = {
    val ta = oddMultiples(p)
    val tb = oddMultiples(q)
    val as = naf(a, 5)
    val bs = naf(b, 5)
    @tailrec def go(position: Int, c: Completed): Point =
      if (position < 0) c.toPoint
      else go(position - 1, withVariable(withVariable(c.toProjective.double, ta, as(position)), tb, bs(position)))
    go(top(as, bs, 255), IdentityCompleted)
  }

  /** `aP + bB`: width-5 NAF for `a`, width 8 on the fixed table for `b`. */
  def doubleScalarMultiplyBasepoint(a: Scalar, p: Point, b: Scalar): Point = {
    val ta = oddMultiples(p)
    val as = naf(a, 5)
    val bs = naf(b, 8)
    @tailrec def go(position: Int, c: Completed): Point =
      if (position < 0) c.toPoint
      else go(position - 1, withBasepoint(withVariable(c.toProjective.double, ta, as(position)), bs(position)))
    go(top(as, bs, 255), IdentityCompleted)
  }
}
