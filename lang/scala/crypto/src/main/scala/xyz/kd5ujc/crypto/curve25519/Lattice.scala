package xyz.kd5ujc.crypto.curve25519

import java.lang.Long.{compareUnsigned, divideUnsigned, numberOfLeadingZeros}

import scala.annotation.tailrec

import cafe.cryptography.curve25519.Scalar
import scodec.bits.{ByteOrdering, ByteVector}

/** An unsigned integer below 2^256: four 64-bit limbs, little-endian, each read as unsigned; immutable. */
final private class U4(val w0: Long, val w1: Long, val w2: Long, val w3: Long) {

  import U4.carry

  /** Limb `i`, and 0 outside 0 to 3. */
  def limb(i: Int): Long = i match {
    case 0 => w0
    case 1 => w1
    case 2 => w2
    case 3 => w3
    case _ => 0L
  }

  def bitLength: Int =
    if (w3 != 0L) 256 - numberOfLeadingZeros(w3)
    else if (w2 != 0L) 192 - numberOfLeadingZeros(w2)
    else if (w1 != 0L) 128 - numberOfLeadingZeros(w1)
    else 64 - numberOfLeadingZeros(w0)

  /** Bits `s` to `s + 63`, for `s >= 0`. */
  def bits64(s: Int): Long = {
    val i = s / 64
    val o = s % 64
    if (o == 0) limb(i) else (limb(i) >>> o) | (limb(i + 1) << (64 - o))
  }

  /** `this · 2^j` modulo 2^256 (callers check it fits). */
  def <<(j: Int): U4 = {
    val w = j / 64
    val b = j % 64
    def at(n: Int): Long = if (b == 0) limb(n - w) else (limb(n - w) << b) | (limb(n - w - 1) >>> (64 - b))
    new U4(at(0), at(1), at(2), at(3))
  }

  def >=(b: U4): Boolean = {
    val c =
      if (w3 != b.w3) compareUnsigned(w3, b.w3)
      else if (w2 != b.w2) compareUnsigned(w2, b.w2)
      else if (w1 != b.w1) compareUnsigned(w1, b.w1)
      else compareUnsigned(w0, b.w0)
    c >= 0
  }

  /** `this - b`, for `this >= b`. */
  def -(b: U4): U4 = {
    val c0 = U4.borrow(w0, b.w0, 0L)
    val c1 = U4.borrow(w1, b.w1, c0)
    val c2 = U4.borrow(w2, b.w2, c1)
    new U4(w0 - b.w0, w1 - b.w1 - c0, w2 - b.w2 - c1, w3 - b.w3 - c2)
  }

  /** `this + b`, if it is below 2^256. */
  def plus(b: U4): Option[U4] = {
    val s0 = w0 + b.w0
    val k0 = carry(s0, b.w0)
    val a1 = w1 + b.w1
    val s1 = a1 + k0
    val k1 = carry(a1, b.w1) + carry(s1, k0)
    val a2 = w2 + b.w2
    val s2 = a2 + k1
    val k2 = carry(a2, b.w2) + carry(s2, k1)
    val a3 = w3 + b.w3
    val s3 = a3 + k2
    Option.when(carry(a3, b.w3) + carry(s3, k2) == 0L)(new U4(s0, s1, s2, s3))
  }

  /** `this · q` for `q` below 2^32, if it is below 2^256. */
  def times(q: Long): Option[U4] = {
    val p1 = w1 * q
    val p2 = w2 * q
    val p3 = w3 * q
    val r1 = p1 + U4.mulHigh(w0, q)
    val k1 = carry(r1, p1) + U4.mulHigh(w1, q)
    val r2 = p2 + k1
    val k2 = carry(r2, p2) + U4.mulHigh(w2, q)
    val r3 = p3 + k2
    Option.when(carry(r3, p3) + U4.mulHigh(w3, q) == 0L)(new U4(w0 * q, r1, r2, r3))
  }

  def isZero: Boolean = (w0 | w1 | w2 | w3) == 0L

  def bytes: ByteVector = {
    def le(w: Long) = ByteVector.fromLong(w, ordering = ByteOrdering.LittleEndian)
    le(w0) ++ le(w1) ++ le(w2) ++ le(w3)
  }
}

private object U4 {

  val Zero: U4 = new U4(0L, 0L, 0L, 0L)
  val One: U4 = new U4(1L, 0L, 0L, 0L)

  /** The high 64 bits of `a q`, `a` read as unsigned and `0 <= q < 2^63`: the signed product's, plus `q` if `a`'s top bit is set. */
  def mulHigh(a: Long, q: Long): Long = Math.multiplyHigh(a, q) + ((a >> 63) & q)

  /** 1 when the sum `sum = a + b` wrapped past 2^64. */
  def carry(sum: Long, b: Long): Long = if (compareUnsigned(sum, b) < 0) 1L else 0L

  /** 1 when `a - b - c` borrows (`c` is 0 or 1). */
  def borrow(a: Long, b: Long, c: Long): Long = if (compareUnsigned(a, b) < 0 || (a == b && c == 1L)) 1L else 0L

  /** 32 little-endian bytes. */
  def from(bytes: ByteVector): U4 = {
    def word(i: Int): Long = bytes.slice(8L * i, 8L * i + 8).toLong(signed = false, ordering = ByteOrdering.LittleEndian)
    new U4(word(0), word(1), word(2), word(3))
  }
}

/**
 * `k` written as `v0 / v1 (mod L)` with `v0` and `v1` about half its length: the split that lets a verifier check
 * `[s]B - [k]A - R` with half the doublings (BouncyCastle's `Scalar25519.reduceBasisVar` does the same job).
 *
 * `v0 = r_i` and `v1 = t_i` come from the extended Euclidean algorithm on `(L, k)`, stopped at the first remainder
 * below 2^126: `r_i = s_i L + t_i k`, so `v0 = v1 k (mod L)`; `|t_i| r_(i-1) <= L` bounds `|v1|` by `L / 2^126`, below
 * 2^127; and `t_i` is positive for odd `i`, negative for even. Each quotient is found 31 bits at a time from the
 * leading 63 bits of the operands (an estimate never above the quotient, so the remainder never goes negative), however
 * large it is. Euclid takes at most about 183 steps here (Lamé), and the congruence is checked before a split is
 * returned. Variable time: `k` is public.
 */
private[crypto] object Lattice {

  /**
   * `v0 = v1 k (mod L)`, `0 <= v0 < 2^126`, `0 < |v1| < 2^128`, `v1 = -magnitude` when `negative`; each the exact
   * integer.
   */
  final case class Split(v0: Scalar, magnitude: Scalar, negative: Boolean)

  private val L: U4 = U4.from(
    ByteVector.view((BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")).toByteArray).padLeft(32).reverse
  )

  private val Bound: Int = 126

  /**
   * `(r mod r1, t + floor(r / r1) · t1)` for `r1 >= 2^126`: each pass subtracts `q 2^j r1` (and adds `q 2^j t1`), with
   * `q` below 2^32 estimated low from the leading bits, and at least 1 (`r >= 2^j r1`). A pass takes about 31 bits off
   * `r`, or, at `j = 0`, leaves it within two multiples of `r1`.
   */
  @tailrec private def reduce(r: U4, t: U4, r1: U4, t1: U4, passes: Int): Option[(U4, U4)] =
    if (!(r >= r1)) Some((r, t))
    else if (passes > 16) None
    else {
      val j = math.max(r.bitLength - r1.bitLength - 31, 0)
      val shifted = if (j == 0) r1 else r1 << j
      val shift = r.bitLength - 63
      val q = math.max(divideUnsigned(r.bits64(shift), shifted.bits64(shift) + 1), 1L)
      val next = for {
        product <- shifted.times(q).filter(r >= _)
        added   <- (if (j == 0) t1 else t1 << j).times(q).filter(_ => j == 0 || t1.bitLength + j <= 256).flatMap(t.plus)
      } yield (r - product, added)
      next match {
        case None           => None
        case Some((r2, t2)) => reduce(r2, t2, r1, t1, passes + 1)
      }
    }

  def split(k: Scalar): Option[Split] = {
    @tailrec def go(r0: U4, r1: U4, t0: U4, t1: U4, odd: Boolean, steps: Int): Option[(U4, U4, Boolean)] =
      if (r1.bitLength <= Bound) Some((r1, t1, !odd))
      else if (steps > 256) None
      else
        reduce(r0, t0, r1, t1, 0) match {
          case None           => None
          case Some((r2, t2)) => go(r1, r2, t1, t2, !odd, steps + 1)
        }
    go(L, U4.from(ByteVector.view(k.toByteArray)), U4.Zero, U4.One, odd = true, 0).flatMap {
      case (v0, v1, negative) =>
        val (r, m) = (scalar(v0), scalar(v1))
        val v1k = m.multiply(k)
        Option.when(!v1.isZero && v1.bitLength <= 128 && same(r, if (negative) Scalar.ZERO.subtract(v1k) else v1k))(
          Split(r, m, negative)
        )
    }
  }

  private def same(a: Scalar, b: Scalar): Boolean = ByteVector.view(a.toByteArray) == ByteVector.view(b.toByteArray)

  /** An integer below 2^252 as the [[Scalar]] that holds it exactly. */
  private def scalar(n: U4): Scalar = Scalar.fromBytesModOrder(n.bytes.toArray)
}
