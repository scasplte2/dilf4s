package xyz.kd5ujc.crypto.vrf

import xyz.kd5ujc.core.hash.{ExpandMessage, Hasher, Sha512}
import xyz.kd5ujc.crypto.curve25519.{Fe, Point}

import scodec.bits.ByteVector

/**
 * edwards25519 point decoding profiles and the hash-to-curve maps the VRF suites use.
 *
 * Everything here operates on **public data only** (public keys, proofs, messages), so it is variable time, on the
 * variable-time field [[Fe]] and points [[Point]]. Secret scalars never reach this object.
 */
private[crypto] object Curve25519Maps {

  // ------------------------------------------------------------------ field GF(2^255 - 19)

  val p: BigInt = BigInt(2).pow(255) - 19
  val L: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")
  private val J: Fe = Fe.fromBigInt(486662)
  private val MinusOne: Fe = Fe.One.negate

  /** `a^((p - 1) / 2) = a^(2^254 - 10)`: 0, 1 or -1. */
  private def legendre(a: Fe): Fe = a.pow22523.squareTimes(2) * a.square
  private def isSquare(a: Fe): Boolean = { val l = legendre(a); l.isZero || l.isEqual(Fe.One) }

  /** A square root of `a` (p = 5 mod 8): `a^((p + 3) / 8)`, times `sqrt(-1)` if that squares to `-a`. */
  private def sqrt(a: Fe): Option[Fe] = {
    val c = a.pow22523 * a
    if (c.square.isEqual(a)) Some(c) else Option.when(c.square.isEqual(a.negate))(c * Fe.SqrtM1)
  }

  /** The root whose `sgn0` (low bit) is `sign`. */
  private def sqrtWithSign(a: Fe, sign: Boolean): Option[Fe] = sqrt(a).map(r => if (r.isNegative == sign) r else r.negate)

  /** `c1 = sqrt(-486664)` with `sgn0(c1) == 0` (RFC 9380 Appendix G.2.2). */
  private val c1: Fe = sqrtWithSign(Fe.fromBigInt(-486664), sign = false).getOrElse(Fe.Zero)

  def le(b:               ByteVector): BigInt = BigInt(1, b.reverse.toArray)
  def le32(i:             BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32).reverse
  private def toBigInt(a: Fe): BigInt = le(a.bytes)

  /** Compressed encoding of affine `(x, y)`: little-endian `y` with the sign of `x` in bit 255. */
  private def encode(x: Fe, y: Fe): ByteVector = {
    val b = y.bytes
    if (x.isNegative) b.update(31, (b(31) | 0x80).toByte) else b
  }

  // ------------------------------------------------------------------ decoding profiles

  /** `y` below p: bits 0 to 254 are not one of `p` to `2^255 - 1` (`0xed..0xff`, then `0xff` to bit 254). */
  private def canonicalY(b: ByteVector): Boolean =
    (b(31) & 0x7f) != 0x7f || (b(0) & 0xff) < 0xed || b.slice(1, 31) != AllOnes30

  private val AllOnes30: ByteVector = ByteVector.fill(30)(0xff)

  /**
   * RFC 8032 §5.1.3: canonical `y` and no negative zero, so the encoding is the point's own (`x = 0` decodes to the
   * point whose sign bit is clear).
   */
  def decodeStrict(b: ByteVector): Option[Point] =
    Option.when(b.size == 32L && canonicalY(b))(b).flatMap(Point.decode).filterNot(pt => (b(31) & 0x80) != 0 && pt.x.isZero)

  /** libsodium (Cardano draft-03) `ge25519_frombytes` with a canonicity check: canonical `y`, negative zero allowed. */
  def decodeDraft03(b: ByteVector): Option[Point] = Option.when(b.size == 32L && canonicalY(b))(b).flatMap(Point.decode)

  def isSmallOrder(pt: Point): Boolean = pt.isSmallOrder

  def bytes(pt: Point): ByteVector = pt.compress

  // ------------------------------------------------------------------ Elligator 2, libsodium draft-03 profile

  /**
   * libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 of the input selects the sign of `x`, the rest is
   * the Elligator 2 representative; the point is mapped through `y = (u - 1) / (u + 1)` and the cofactor cleared.
   */
  def fromUniformDraft03(r32: ByteVector): Option[Point] =
    Option.when(r32.size == 32L)(r32).flatMap { in =>
      val r = Fe.fromBytes(in)
      val u0 = (J * (Fe.One + (r.square + r.square)).invert).negate
      def w(u: Fe) = u * (u.square + J * u + Fe.One)
      val u = if (legendre(w(u0)).isEqual(MinusOne)) (u0 + J).negate else u0
      val y = if ((u + Fe.One).isZero || w(u).isZero) Fe.One else (u - Fe.One) * (u + Fe.One).invert
      val enc = y.bytes
      decodeDraft03(enc.update(31, (enc(31) | (in(31) & 0x80)).toByte)).map(_.multiplyByCofactor())
    }

  /** Cardano draft-03 hash-to-curve: `SHA-512(0x04 || 0x01 || PK || alpha)[0..32]` with bit 255 cleared. */
  def hashToCurveDraft03(pk: ByteVector, alpha: ByteVector): Option[Point] = {
    val h = Hasher[Sha512].digest(ByteVector(0x04, 0x01) ++ pk ++ alpha).bytes.take(32)
    fromUniformDraft03(h.update(31, (h(31) & 0x7f).toByte))
  }

  // ------------------------------------------------------------------ RFC 9380 edwards25519_XMD:SHA-512_ELL2_NU_

  /** `hash_to_field` with count 1: 48 uniform bytes, big-endian, reduced mod p. */
  def hashToField(msg: ByteVector, dst: ByteVector): Option[BigInt] =
    ExpandMessage.xmd[Sha512](msg, dst, 48).toOption.map(b => BigInt(1, b.toArray).mod(p))

  /** `map_to_curve_elligator2` on curve25519 (§6.7.1, J = 486662, K = 1, Z = 2) followed by the rational map (G.2.2). */
  def mapToCurveEll2(u: BigInt): Option[(BigInt, BigInt)] = mapToEdwards(Fe.fromBigInt(u)).map { case (x, y) => (toBigInt(x), toBigInt(y)) }

  private def mapToEdwards(u: Fe): Option[(Fe, Fe)] = {
    val x1a = (J * (Fe.One + (u.square + u.square)).invert).negate
    val x1 = if (x1a.isZero) J.negate else x1a
    def g(x: Fe) = x * (x.square + J * x + Fe.One)
    val x2 = (x1 + J).negate
    val (s, t) =
      if (isSquare(g(x1))) (x1, sqrtWithSign(g(x1), sign = true))
      else (x2, sqrtWithSign(g(x2), sign = false))
    t.map { tv =>
      if ((tv * (s + Fe.One)).isZero) (Fe.Zero, Fe.One)
      else (c1 * s * tv.invert, (s - Fe.One) * (s + Fe.One).invert)
    }
  }

  /** `encode_to_curve` for edwards25519_XMD:SHA-512_ELL2_NU_: map one field element, then clear the cofactor. */
  def encodeToCurveEll2(msg: ByteVector, dst: ByteVector): Option[Point] =
    hashToField(msg, dst)
      .flatMap(u => mapToEdwards(Fe.fromBigInt(u)))
      .flatMap { case (x, y) => decodeStrict(encode(x, y)) }
      .map(_.multiplyByCofactor())
}
