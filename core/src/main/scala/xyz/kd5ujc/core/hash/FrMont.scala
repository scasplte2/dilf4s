package xyz.kd5ujc.core.hash

import java.lang.Long.compareUnsigned
import java.lang.Math.unsignedMultiplyHigh

/**
 * An element of the BN254 scalar field in Montgomery form, `x · 2^256 mod r`, as four little-endian 64-bit limbs. Each
 * operation is a pure function of immutable values: no arrays, no mutation, so the JIT can keep the limbs in registers.
 * Multiplication is CIOS Montgomery multiplication (Koç, Acar and Kaliski). Every value is fully reduced, below `r`, so
 * equal elements have equal limbs.
 *
 * The helpers that produce several limbs pass them to an inline continuation instead of returning a tuple: the compiler
 * substitutes the continuation's body, so intermediate limbs stay in local `Long`s and an operation allocates only its
 * result.
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
    // r < 2^254, so the sum of two reduced elements is below 2^255: no carry out of the top limb
    reduced(s0, s1, s2, l3 + that.l3 + c2)
  }

  /**
   * CIOS: four rounds, each adding `this · b_i` and then a multiple of `r` that clears the low limb. Since `r`'s top
   * limb is below `2^63 - 1`, the accumulator never needs a fifth word ("no-carry" CIOS: Botrel and El Housni, EdMSM,
   * eprint 2022/1400).
   */
  def *(that: FrMont): FrMont =
    round(0L, 0L, 0L, 0L, that.l0) { (a0, a1, a2, a3) =>
      round(a0, a1, a2, a3, that.l1) { (b0, b1, b2, b3) =>
        round(b0, b1, b2, b3, that.l2) { (c0, c1, c2, c3) =>
          round(c0, c1, c2, c3, that.l3)(reduced)
        }
      }
    }

  /** One round over the accumulator `t0..t3`; passes the next accumulator to `k`. */
  inline private def round[A](t0: Long, t1: Long, t2: Long, t3: Long, b: Long)(inline k: (Long, Long, Long, Long) => A): A =
    mac(t0, l0, b, 0L) { (a0, u0) =>
      val m = u0 * NInv
      mac(u0, m, N0, 0L) { (c0, _) =>
        mac(t1, l1, b, a0) { (a1, u1) =>
          mac(u1, m, N1, c0) { (c1, v0) =>
            mac(t2, l2, b, a1) { (a2, u2) =>
              mac(u2, m, N2, c1) { (c2, v1) =>
                mac(t3, l3, b, a2) { (a3, u3) =>
                  mac(u3, m, N3, c2)((c3, v2) => k(v0, v1, v2, c3 + a3))
                }
              }
            }
          }
        }
      }
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

  /** Whether `sum = a + b` carried out of 64 bits. */
  inline private def carry(sum: Long, a: Long): Long = if (compareUnsigned(sum, a) < 0) 1L else 0L

  /** The carry out of `a + b + c`, `c` in {0, 1}, given `sum`. */
  inline private def carry3(sum: Long, a: Long, c: Long): Long =
    if (compareUnsigned(sum, a) < 0 || (c == 1L && sum == a)) 1L else 0L

  /** `t + a · b + c` as 128 bits, every input below 2^64 so the result fits: `k(high, low)`. */
  inline private def mac[A](t: Long, a: Long, b: Long, c: Long)(inline k: (Long, Long) => A): A = {
    val lo = a * b
    val hi = unsignedMultiplyHigh(a, b)
    val lo1 = lo + t
    val hi1 = hi + carry(lo1, lo)
    val lo2 = lo1 + c
    k(hi1 + carry(lo2, lo1), lo2)
  }

  private def geqModulus(x0: Long, x1: Long, x2: Long, x3: Long): Boolean = {
    val c3 = compareUnsigned(x3, N3)
    if (c3 != 0) c3 > 0
    else {
      val c2 = compareUnsigned(x2, N2)
      if (c2 != 0) c2 > 0
      else {
        val c1 = compareUnsigned(x1, N1)
        if (c1 != 0) c1 > 0 else compareUnsigned(x0, N0) >= 0
      }
    }
  }

  /** `x - r` if `x >= r`, else `x`: every operation's result is below `2r`. */
  private def reduced(x0: Long, x1: Long, x2: Long, x3: Long): FrMont =
    if (!geqModulus(x0, x1, x2, x3)) FrMont(x0, x1, x2, x3)
    else {
      val b0 = if (compareUnsigned(x0, N0) < 0) 1L else 0L
      val b1 = if (compareUnsigned(x1, N1) < 0 || (b0 == 1L && x1 == N1)) 1L else 0L
      val b2 = if (compareUnsigned(x2, N2) < 0 || (b1 == 1L && x2 == N2)) 1L else 0L
      FrMont(x0 - N0, x1 - N1 - b0, x2 - N2 - b1, x3 - N3 - b2)
    }
}
