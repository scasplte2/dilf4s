package xyz.kd5ujc.crypto.curve25519

import scala.annotation.tailrec

import scodec.bits.{ByteOrdering, ByteVector}

/**
 * An element of GF(2^255 - 19): five unsigned 51-bit limbs, immutable. Every operation returns a carried element (each
 * limb below 2^52), so products never overflow: a limb product is below 2^104, and a column of five, split at bit 51
 * as it is summed, stays below 2^61. Variable time: only public data (keys, signatures, proofs, messages) is ever
 * computed with it.
 */
final private[crypto] class Fe(val l0: Long, val l1: Long, val l2: Long, val l3: Long, val l4: Long) {

  import Fe._

  def +(b: Fe): Fe = carried(l0 + b.l0, l1 + b.l1, l2 + b.l2, l3 + b.l3, l4 + b.l4)

  /** `a + 2p - b`: every limb of `b` is below 2^52, so no limb goes negative. */
  def -(b: Fe): Fe = carried(l0 + TwoP0 - b.l0, l1 + TwoP - b.l1, l2 + TwoP - b.l2, l3 + TwoP - b.l3, l4 + TwoP - b.l4)

  def negate: Fe = carried(TwoP0 - l0, TwoP - l1, TwoP - l2, TwoP - l3, TwoP - l4)

  def *(b: Fe): Fe = {
    // separate vals, not a tuple: a tuple of Longs would box them
    val c0 = b.l0
    val c1 = b.l1
    val c2 = b.l2
    val c3 = b.l3
    val c4 = b.l4
    val n1 = c1 * 19
    val n2 = c2 * 19
    val n3 = c3 * 19
    val n4 = c4 * 19
    reduced(
      lo(l0, c0) + lo(l1, n4) + lo(l2, n3) + lo(l3, n2) + lo(l4, n1),
      hi(l0, c0) + hi(l1, n4) + hi(l2, n3) + hi(l3, n2) + hi(l4, n1),
      lo(l0, c1) + lo(l1, c0) + lo(l2, n4) + lo(l3, n3) + lo(l4, n2),
      hi(l0, c1) + hi(l1, c0) + hi(l2, n4) + hi(l3, n3) + hi(l4, n2),
      lo(l0, c2) + lo(l1, c1) + lo(l2, c0) + lo(l3, n4) + lo(l4, n3),
      hi(l0, c2) + hi(l1, c1) + hi(l2, c0) + hi(l3, n4) + hi(l4, n3),
      lo(l0, c3) + lo(l1, c2) + lo(l2, c1) + lo(l3, c0) + lo(l4, n4),
      hi(l0, c3) + hi(l1, c2) + hi(l2, c1) + hi(l3, c0) + hi(l4, n4),
      lo(l0, c4) + lo(l1, c3) + lo(l2, c2) + lo(l3, c1) + lo(l4, c0),
      hi(l0, c4) + hi(l1, c3) + hi(l2, c2) + hi(l3, c1) + hi(l4, c0)
    )
  }

  def square: Fe = {
    val d0 = l0 * 2
    val d1 = l1 * 2
    val n3 = l3 * 19
    val n4 = l4 * 19
    val m3 = l3 * 38
    val m4 = l4 * 38
    reduced(
      lo(l0, l0) + lo(l1, m4) + lo(l2, m3),
      hi(l0, l0) + hi(l1, m4) + hi(l2, m3),
      lo(d0, l1) + lo(l2, m4) + lo(l3, n3),
      hi(d0, l1) + hi(l2, m4) + hi(l3, n3),
      lo(d0, l2) + lo(l1, l1) + lo(l3, m4),
      hi(d0, l2) + hi(l1, l1) + hi(l3, m4),
      lo(d0, l3) + lo(d1, l2) + lo(l4, n4),
      hi(d0, l3) + hi(d1, l2) + hi(l4, n4),
      lo(d0, l4) + lo(d1, l3) + lo(l2, l2),
      hi(d0, l4) + hi(d1, l3) + hi(l2, l2)
    )
  }

  /** `k` successive squarings: `a^(2^k)`. */
  def squareTimes(k: Int): Fe = {
    @tailrec def go(x: Fe, n: Int): Fe = if (n <= 0) x else go(x.square, n - 1)
    go(this, k)
  }

  /** The canonical little-endian encoding, below p. */
  def bytes: ByteVector = {
    val c = Fe.canonical(this)
    val w0 = c.l0 | (c.l1 << 51)
    val w1 = (c.l1 >>> 13) | (c.l2 << 38)
    val w2 = (c.l2 >>> 26) | (c.l3 << 25)
    val w3 = (c.l3 >>> 39) | (c.l4 << 12)
    ByteVector.fromLong(w0, ordering = ByteOrdering.LittleEndian) ++
    ByteVector.fromLong(w1, ordering = ByteOrdering.LittleEndian) ++
    ByteVector.fromLong(w2, ordering = ByteOrdering.LittleEndian) ++
    ByteVector.fromLong(w3, ordering = ByteOrdering.LittleEndian)
  }

  def isZero: Boolean = { val c = Fe.canonical(this); (c.l0 | c.l1 | c.l2 | c.l3 | c.l4) == 0L }

  /** The low bit of the canonical value (RFC 8032's sign of `x`). */
  def isNegative: Boolean = (Fe.canonical(this).l0 & 1L) == 1L

  def isEqual(b: Fe): Boolean = (this - b).isZero

  /** `a^-1` (and 0 for 0), as `a^(p - 2)`. */
  def invert: Fe = {
    val (t, z11) = pow2501
    t.squareTimes(5) * z11
  }

  /** `a^((p - 5) / 8) = a^(2^252 - 3)`, the exponent of the square-root ratio. */
  def pow22523: Fe = pow2501._1.squareTimes(2) * this

  /** `(a^(2^250 - 1), a^11)`: the shared prefix of the inversion and square-root chains (ref10). */
  private def pow2501: (Fe, Fe) = {
    val z2 = square
    val z9 = z2.squareTimes(2) * this
    val z11 = z9 * z2
    val z5 = z11.square * z9 // 2^5 - 1
    val z10 = z5.squareTimes(5) * z5
    val z20 = z10.squareTimes(10) * z10
    val z40 = z20.squareTimes(20) * z20
    val z50 = z40.squareTimes(10) * z10
    val z100 = z50.squareTimes(50) * z50
    val z200 = z100.squareTimes(100) * z100
    (z200.squareTimes(50) * z50, z11)
  }
}

private[crypto] object Fe {

  private val Mask: Long = (1L << 51) - 1
  private val TwoP0: Long = (1L << 52) - 38
  private val TwoP: Long = (1L << 52) - 2

  val Zero: Fe = new Fe(0, 0, 0, 0, 0)
  val One: Fe = new Fe(1, 0, 0, 0, 0)

  /** The low 51 bits of `a b`. */
  inline private def lo(a: Long, b: Long): Long = (a * b) & Mask

  /** `a b >> 51`, for `a b` below 2^115. */
  inline private def hi(a: Long, b: Long): Long = (Math.multiplyHigh(a, b) << 13) | ((a * b) >>> 51)

  /** Columns split at bit 51 (`lk` the low parts, `hk` the high), folded and carried to five limbs. */
  inline private def reduced(l0: Long, h0: Long, l1: Long, h1: Long, l2: Long, h2: Long, l3: Long, h3: Long, l4: Long, h4: Long): Fe =
    carried(l0 + h4 * 19, l1 + h0, l2 + h1, l3 + h2, l4 + h3)

  /** One carry pass: limbs 1 to 4 below 2^51, limb 0 below 2^51 + 19 · 2^13. Inputs are non-negative, below 2^63. */
  private def carried(a0: Long, a1: Long, a2: Long, a3: Long, a4: Long): Fe = {
    val b1 = a1 + (a0 >>> 51)
    val b2 = a2 + (b1 >>> 51)
    val b3 = a3 + (b2 >>> 51)
    val b4 = a4 + (b3 >>> 51)
    new Fe((a0 & Mask) + (b4 >>> 51) * 19, b1 & Mask, b2 & Mask, b3 & Mask, b4 & Mask)
  }

  /** The fully reduced representative, below p. */
  private def canonical(a: Fe): Fe = {
    val c = carried(a.l0, a.l1, a.l2, a.l3, a.l4)
    // c is below 2^255 + 19 · 2^13; q is 1 exactly when c >= p
    val q = (c.l4 + ((c.l3 + ((c.l2 + ((c.l1 + ((c.l0 + 19) >>> 51)) >>> 51)) >>> 51)) >>> 51)) >>> 51
    val b0 = c.l0 + 19 * q
    val b1 = c.l1 + (b0 >>> 51)
    val b2 = c.l2 + (b1 >>> 51)
    val b3 = c.l3 + (b2 >>> 51)
    val b4 = c.l4 + (b3 >>> 51)
    new Fe(b0 & Mask, b1 & Mask, b2 & Mask, b3 & Mask, b4 & Mask)
  }

  /** 32 little-endian bytes, bit 255 ignored; values from p to 2^255 - 1 are accepted (and reduced). */
  def fromBytes(b: ByteVector): Fe = {
    def word(i: Int): Long = b.slice(8L * i, 8L * i + 8).toLong(signed = false, ordering = ByteOrdering.LittleEndian)
    val (w0, w1, w2, w3) = (word(0), word(1), word(2), word(3))
    new Fe(
      w0 & Mask,
      ((w0 >>> 51) | (w1 << 13)) & Mask,
      ((w1 >>> 38) | (w2 << 26)) & Mask,
      ((w2 >>> 25) | (w3 << 39)) & Mask,
      (w3 >>> 12) & Mask
    )
  }

  def fromBigInt(n: BigInt): Fe = fromBytes(ByteVector.view(n.mod(P).toByteArray).takeRight(32).padLeft(32).reverse)

  val P: BigInt = BigInt(2).pow(255) - 19

  /** `d = -121665 / 121666`. */
  val D: Fe = fromBigInt(BigInt(-121665) * BigInt(121666).modPow(P - 2, P))
  val D2: Fe = D + D
  val SqrtM1: Fe = fromBigInt(BigInt(2).modPow((P - 1) / 4, P))
}
