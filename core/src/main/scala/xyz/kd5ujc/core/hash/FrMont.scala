package xyz.kd5ujc.core.hash

import java.lang.Long.compareUnsigned
import java.lang.Math.unsignedMultiplyHigh

/**
 * An element of the BN254 scalar field in Montgomery form, `x · 2^256 mod r`, as four little-endian 64-bit limbs. Each
 * operation is a pure function of immutable values: no arrays, no mutation, so the JIT can keep the limbs in registers.
 * Multiplication is CIOS Montgomery multiplication (Koç, Acar and Kaliski). Every value is fully reduced, below `r`, so
 * equal elements have equal limbs.
 *
 * Variable time: for public data only, as [[Poseidon]].
 */
final private[hash] case class FrMont(l0: Long, l1: Long, l2: Long, l3: Long) {

  import FrMont._

  def +(that: FrMont): FrMont = {
    val s0 = l0 + that.l0
    val c0 = carry(s0, l0)
    val s1 = l1 + that.l1 + c0
    val c1 = carry3(s1, l1, c0)
    val s2 = l2 + that.l2 + c1
    val c2 = carry3(s2, l2, c1)
    val s3 = l3 + that.l3 + c2
    // r < 2^254, so the sum of two reduced elements is below 2^255: no carry out of the top limb
    reduce(FrMont(s0, s1, s2, s3))
  }

  /** CIOS: four rounds, each adding `this · b_i` and then a multiple of `r` that clears the low limb. */
  def *(that: FrMont): FrMont = {
    val t = round(round(round(round(Acc(0L, 0L, 0L, 0L, 0L), that.l0), that.l1), that.l2), that.l3)
    reduce(FrMont(t.t0, t.t1, t.t2, t.t3))
  }

  private def round(t: Acc, b: Long): Acc = {
    val w0 = mac(t.t0, l0, b, 0L)
    val w1 = mac(t.t1, l1, b, w0.hi)
    val w2 = mac(t.t2, l2, b, w1.hi)
    val w3 = mac(t.t3, l3, b, w2.hi)
    val top = t.t4 + w3.hi
    val top1 = carry(top, t.t4)
    val m = w0.lo * NInv
    val r0 = mac(w0.lo, m, N0, 0L)
    val r1 = mac(w1.lo, m, N1, r0.hi)
    val r2 = mac(w2.lo, m, N2, r1.hi)
    val r3 = mac(w3.lo, m, N3, r2.hi)
    val t3 = top + r3.hi
    Acc(r1.lo, r2.lo, r3.lo, t3, top1 + carry(t3, top))
  }

  def toBigInt: BigInt = {
    val plain = this * FrMont(1L, 0L, 0L, 0L)
    List(plain.l3, plain.l2, plain.l1, plain.l0).foldLeft(BigInt(0))((acc, l) => (acc << 64) + unsigned(l))
  }
}

private[hash] object FrMont {

  // the modulus r, -r^-1 mod 2^64, and 2^512 mod r (to enter Montgomery form)
  private val N0 = 4891460686036598785L
  private val N1 = 2896914383306846353L
  private val N2 = -5165552122434856867L
  private val N3 = 3486998266802970665L
  private val NInv = -4403968944856104961L
  private val R2 = FrMont(1997599621687373223L, 6052339484930628067L, -8337988935678721915L, 150537098327114917L)

  val Zero: FrMont = FrMont(0L, 0L, 0L, 0L)

  /** `value` must be in `[0, r)`. */
  def apply(value: BigInt): FrMont = {
    def limb(i: Int): Long = (value >> (64 * i)).toLong
    FrMont(limb(0), limb(1), limb(2), limb(3)) * R2
  }

  private def unsigned(l: Long): BigInt = if (l >= 0) BigInt(l) else BigInt(l) + (BigInt(1) << 64)

  final private case class Wide(hi: Long, lo: Long)
  final private case class Acc(t0: Long, t1: Long, t2: Long, t3: Long, t4: Long)

  /** Whether `sum = a + b` carried out of 64 bits. */
  private def carry(sum: Long, a: Long): Long = if (compareUnsigned(sum, a) < 0) 1L else 0L

  /** The carry out of `a + b + c`, `c` in {0, 1}, given `sum`. */
  private def carry3(sum: Long, a: Long, c: Long): Long =
    if (compareUnsigned(sum, a) < 0 || (c == 1L && sum == a)) 1L else 0L

  /** `t + a · b + c` as 128 bits, every input below 2^64: the result fits. */
  private def mac(t: Long, a: Long, b: Long, c: Long): Wide = {
    val lo = a * b
    val hi = unsignedMultiplyHigh(a, b)
    val lo1 = lo + t
    val hi1 = hi + carry(lo1, lo)
    val lo2 = lo1 + c
    Wide(hi1 + carry(lo2, lo1), lo2)
  }

  private def geqModulus(x: FrMont): Boolean = {
    def cmp(a: Long, b: Long) = compareUnsigned(a, b)
    val c3 = cmp(x.l3, N3)
    if (c3 != 0) c3 > 0
    else {
      val c2 = cmp(x.l2, N2)
      if (c2 != 0) c2 > 0
      else {
        val c1 = cmp(x.l1, N1)
        if (c1 != 0) c1 > 0 else cmp(x.l0, N0) >= 0
      }
    }
  }

  /** `x - r` if `x >= r`: every operation's result is below `2r`. */
  private def reduce(x: FrMont): FrMont =
    if (!geqModulus(x)) x
    else {
      val d0 = x.l0 - N0
      val b0 = if (compareUnsigned(x.l0, N0) < 0) 1L else 0L
      val d1 = x.l1 - N1 - b0
      val b1 = if (compareUnsigned(x.l1, N1) < 0 || (b0 == 1L && x.l1 == N1)) 1L else 0L
      val d2 = x.l2 - N2 - b1
      val b2 = if (compareUnsigned(x.l2, N2) < 0 || (b1 == 1L && x.l2 == N2)) 1L else 0L
      FrMont(d0, d1, d2, x.l3 - N3 - b2)
    }
}
