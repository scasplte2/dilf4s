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

  /** A nonzero digit of a NAF: `value · 2^position`. */
  final private case class Digit(position: Int, value: Int)

  /**
   * The width-`w` NAF of a scalar (dalek's `non_adjacent_form`): its nonzero digits, highest position first. Each is
   * odd, below `2^(w-1)` in magnitude, and they are at least `w` positions apart. A [[Scalar]] holds a value below
   * `L < 2^253` (its reduced constructors; the unreduced bit constructor is refused by the build), so no carry is left
   * over at the top.
   */
  private def naf(scalar: Scalar, w: Int): List[Digit] = {
    val bytes = ByteVector.view(scalar.toByteArray)
    def word(i: Int): Long =
      if (i < 4) bytes.slice(8L * i, 8L * i + 8).toLong(signed = false, ordering = ByteOrdering.LittleEndian) else 0L
    val words = IArray.tabulate(5)(word)
    val width = 1L << w
    @tailrec def go(position: Int, carry: Long, digits: List[Digit]): List[Digit] =
      if (position >= 256) digits
      else {
        val i = position / 64
        val bit = position % 64
        val buffer = if (bit < 64 - w) words(i) >>> bit else (words(i) >>> bit) | (words(i + 1) << (64 - bit))
        val window = carry + (buffer & (width - 1))
        if ((window & 1L) == 0L) go(position + 1, carry, digits)
        else if (window < width / 2) go(position + w, 0L, Digit(position, window.toInt) :: digits)
        else go(position + w, 1L, Digit(position, (window - width).toInt) :: digits)
      }
    go(0, 0L, Nil)
  }

  /** The digit at `position` (0 if none), for digits consumed from the highest position down. */
  private def digit(digits: List[Digit], position: Int): Int =
    digits match {
      case Digit(`position`, v) :: _ => v
      case _                         => 0
    }

  /** The digits below `position`. */
  private def below(digits: List[Digit], position: Int): List[Digit] =
    digits match {
      case Digit(`position`, _) :: rest => rest
      case _                            => digits
    }

  /** The highest position with a digit, or -1. */
  private def top(digits: List[Digit]*): Int = digits.foldLeft(-1)((m, d) => d.headOption.fold(m)(x => math.max(m, x.position)))

  /** `[P, 3P, ..., 15P]`: the odd multiples a width-5 NAF digit selects. */
  private def oddMultiples(p: Point): IArray[Cached] = {
    val twice = p.projective.double.toPoint.cached
    IArray.iterate(p, 8)(q => q.add(twice).toPoint).map(_.cached)
  }

  /** `[P, 3P, ..., 127P]`, affine, for width-8 digits on a fixed point. */
  private def affineOddMultiples(p: Point): IArray[Niels] = {
    val twice = p.projective.double.toPoint.cached
    IArray.iterate(p, 64)(q => q.add(twice).toPoint).map { q =>
      val zi = q.z.invert
      val ax = q.x * zi
      val ay = q.y * zi
      new Niels(ay + ax, ay - ax, ax * ay * Fe.D2)
    }
  }

  /** The tables for `B` and `2^128 B`, built once. */
  private val basepointTable: IArray[Niels] = affineOddMultiples(Basepoint)
  private val basepoint128Table: IArray[Niels] = {
    @tailrec def doubled(q: Point, n: Int): Point = if (n == 0) q else doubled(q.projective.double.toPoint, n - 1)
    affineOddMultiples(doubled(Basepoint, 128))
  }

  private def withVariable(c: Completed, table: IArray[Cached], digit: Int): Completed =
    if (digit == 0) c else if (digit > 0) c.toPoint.add(table(digit / 2)) else c.toPoint.sub(table(-digit / 2))

  private def withFixed(c: Completed, table: IArray[Niels], digit: Int): Completed =
    if (digit == 0) c else if (digit > 0) c.toPoint.addAffine(table(digit / 2)) else c.toPoint.subAffine(table(-digit / 2))

  private val IdentityCompleted: Completed = new Completed(Fe.Zero, Fe.One, Fe.One, Fe.One)

  /** `aP + bQ`: Straus, width-5 NAF for both. */
  def doubleScalarMultiply(a: Scalar, p: Point, b: Scalar, q: Point): Point = {
    val ta = oddMultiples(p)
    val tb = oddMultiples(q)
    @tailrec def go(position: Int, c: Completed, as: List[Digit], bs: List[Digit]): Point =
      if (position < 0) c.toPoint
      else {
        val next = withVariable(withVariable(c.toProjective.double, ta, digit(as, position)), tb, digit(bs, position))
        go(position - 1, next, below(as, position), below(bs, position))
      }
    val (as, bs) = (naf(a, 5), naf(b, 5))
    go(top(as, bs), IdentityCompleted, as, bs)
  }

  /** `aP + bB`: width-5 NAF for `a`, width 8 on the fixed table for `b`. */
  def doubleScalarMultiplyBasepoint(a: Scalar, p: Point, b: Scalar): Point = {
    val ta = oddMultiples(p)
    @tailrec def go(position: Int, c: Completed, as: List[Digit], bs: List[Digit]): Point =
      if (position < 0) c.toPoint
      else {
        val next = withFixed(withVariable(c.toProjective.double, ta, digit(as, position)), basepointTable, digit(bs, position))
        go(position - 1, next, below(as, position), below(bs, position))
      }
    val (as, bs) = (naf(a, 5), naf(b, 8))
    go(top(as, bs), IdentityCompleted, as, bs)
  }

  /**
   * ZIP-215's cofactored equation: whether `[8]([s]B - [k]A - R)` is the identity, for any `A` and `R` (mixed-order ones
   * included).
   *
   * With `k = v0 / v1 (mod L)` from [[Lattice.split]] (both about 127 bits, `v1` not 0 mod L), it checks
   * `W = [v1 s mod L]B - [v0]A - [v1]R` instead, with every multi-scalar product about 128 bits long (the `B` scalar is
   * split at bit 128 over the tables of `B` and `2^128 B`), so half the doublings. Exact, for any `A` and `R`: let
   * `Q = [s]B - [k]A - R` and `v1 k = v0 + mL`. Then `W - [v1]Q = [v1 s mod L - v1 s]B + [mL]A = [mL]A` (B has order L),
   * and `[8][mL]A = [m][L]([8]A) = 0`, since `[8]A` lies in the subgroup of order L. So `[8]W = [v1]([8]Q)`, and as
   * `[8]Q` lies in that subgroup and `v1` is not 0 mod L, `[8]W` is the identity exactly when `[8]Q` is. Should no split be
   * found (the bounds of [[Lattice]] rule it out, but its arithmetic is checked rather than trusted), the equation is
   * evaluated directly, as `[8]([-k mod L]A + [s]B - R)`: `[8][L]A = 0` again.
   */
  def cofactoredEquation(s: Scalar, k: Scalar, a: Point, r: Point): Boolean =
    Lattice
      .split(k)
      .fold((doubleScalarMultiplyBasepoint(Scalar.ZERO.subtract(k), a, s) - r).multiplyByCofactor().isIdentity) { split =>
        val v1s = split.magnitude.multiply(s)
        val t = ByteVector.view((if (split.negative) Scalar.ZERO.subtract(v1s) else v1s).toByteArray)
        def half(b: ByteVector): Scalar = Scalar.fromBytesModOrder(b.padRight(32).toArray)
        // -[v1]R: [|v1|](-R) for a positive v1, [|v1|]R for a negative one
        val w = straus4(split.v0, a.negate, split.magnitude, if (split.negative) r else r.negate, half(t.take(16)), half(t.drop(16)))
        w.multiplyByCofactor().isIdentity
      }

  /** `aP + bQ + cB + d 2^128 B`: width-5 NAF for `a` and `b`, width 8 on the fixed tables for `c` and `d`. */
  private def straus4(a: Scalar, p: Point, b: Scalar, q: Point, c: Scalar, d: Scalar): Point = {
    val ta = oddMultiples(p)
    val tb = oddMultiples(q)
    @tailrec def go(position: Int, acc: Completed, as: List[Digit], bs: List[Digit], cs: List[Digit], ds: List[Digit]): Point =
      if (position < 0) acc.toPoint
      else {
        val variables = withVariable(withVariable(acc.toProjective.double, ta, digit(as, position)), tb, digit(bs, position))
        val next = withFixed(withFixed(variables, basepointTable, digit(cs, position)), basepoint128Table, digit(ds, position))
        go(position - 1, next, below(as, position), below(bs, position), below(cs, position), below(ds, position))
      }
    val (as, bs, cs, ds) = (naf(a, 5), naf(b, 5), naf(c, 8), naf(d, 8))
    go(top(as, bs, cs, ds), IdentityCompleted, as, bs, cs, ds)
  }
}
