package xyz.kd5ujc.crypto.vrf

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{ExpandMessage, Hasher, Sha512}

import cafe.cryptography.curve25519.{CompressedEdwardsY, EdwardsPoint}
import scodec.bits.ByteVector

/**
 * edwards25519 point decoding profiles and the hash-to-curve maps the VRF suites use.
 *
 * Everything here operates on **public data only** (public keys, proofs, messages), so field arithmetic is plain
 * `BigInt`, variable time. Secret scalars never reach this object.
 */
private[crypto] object Curve25519Maps {

  // ------------------------------------------------------------------ field GF(2^255 - 19)

  val p: BigInt = BigInt(2).pow(255) - 19
  val L: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")
  private val J: BigInt = 486662
  private val sqrtM1: BigInt = BigInt(2).modPow((p - 1) / 4, p)

  private def mod(a: BigInt): BigInt = a.mod(p)

  /** `a^-1`, and `0` for `0` (inv0, RFC 9380 §4); never throws. */
  private def inv0(a:     BigInt): BigInt = mod(a).modPow(p - 2, p)
  private def isSquare(a: BigInt): Boolean = { val l = mod(a).modPow((p - 1) / 2, p); l == 0 || l == 1 }
  private def sgn0(a:     BigInt): Int = if (mod(a).testBit(0)) 1 else 0

  /** A square root of `a` (p = 5 mod 8), if one exists. */
  private def sqrt(a: BigInt): Option[BigInt] = {
    val c = mod(a).modPow((p + 3) / 8, p)
    if (mod(c * c) == mod(a)) Some(c)
    else Option.when(mod(c * c) == mod(-a))(mod(c * sqrtM1))
  }

  private def sqrtWithSign(a: BigInt, sign: Int): Option[BigInt] = sqrt(a).map(r => if (sgn0(r) == sign) r else mod(-r))

  /** `c1 = sqrt(-486664)` with `sgn0(c1) == 0` (RFC 9380 Appendix G.2.2). */
  private val c1: BigInt = sqrtWithSign(-486664, 0).getOrElse(BigInt(0))

  def le(b:   ByteVector): BigInt = BigInt(1, b.reverse.toArray)
  def le32(i: BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32).reverse

  /** Compressed encoding of affine `(x, y)`: little-endian `y` with the sign of `x` in bit 255. */
  private def encode(x: BigInt, y: BigInt): ByteVector = {
    val b = le32(mod(y))
    b.update(31, (b(31) | (if (sgn0(x) == 1) 0x80 else 0)).toByte)
  }

  // ------------------------------------------------------------------ decoding profiles

  private def lenient(b: ByteVector): Option[EdwardsPoint] =
    Option.when(b.size == 32L)(b).flatMap(bytes => Either.catchNonFatal(new CompressedEdwardsY(bytes.toArray).decompress()).toOption)

  private def canonicalY(b: ByteVector): Boolean = le(b.update(31, (b(31) & 0x7f).toByte)) < p

  /** RFC 8032 §5.1.3: canonical `y` and no negative zero (the re-encoding must equal the input). */
  def decodeStrict(b: ByteVector): Option[EdwardsPoint] =
    lenient(b).filter(pt => ByteVector.view(pt.compress().toByteArray) == b)

  /** libsodium (Cardano draft-03) `ge25519_frombytes` with a canonicity check: canonical `y`, negative zero allowed. */
  def decodeDraft03(b: ByteVector): Option[EdwardsPoint] = Option.when(b.size == 32L && canonicalY(b))(b).flatMap(lenient)

  def isSmallOrder(pt: EdwardsPoint): Boolean = pt.multiplyByCofactor().isIdentity

  def bytes(pt: EdwardsPoint): ByteVector = ByteVector.view(pt.compress().toByteArray)

  // ------------------------------------------------------------------ Elligator 2, libsodium draft-03 profile

  /**
   * libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 of the input selects the sign of `x`, the rest is
   * the Elligator 2 representative; the point is mapped through `y = (u - 1) / (u + 1)` and the cofactor cleared.
   */
  def fromUniformDraft03(r32: ByteVector): Option[EdwardsPoint] =
    Option.when(r32.size == 32L)(r32).flatMap { in =>
      val sign = (in(31) & 0x80) >>> 7
      val r = mod(le(in.update(31, (in(31) & 0x7f).toByte)))
      val u0 = mod(-J * inv0(1 + 2 * r * r))
      def w(u: BigInt) = mod(u * (u * u + J * u + 1))
      val u = if (mod(w(u0)).modPow((p - 1) / 2, p) == p - 1) mod(-u0 - J) else u0
      val y = if (mod(u + 1) == 0 || w(u) == 0) BigInt(1) else mod((u - 1) * inv0(u + 1))
      val enc = le32(y)
      decodeDraft03(enc.update(31, (enc(31) | (sign << 7)).toByte)).map(_.multiplyByCofactor())
    }

  /** Cardano draft-03 hash-to-curve: `SHA-512(0x04 || 0x01 || PK || alpha)[0..32]` with bit 255 cleared. */
  def hashToCurveDraft03(pk: ByteVector, alpha: ByteVector): Option[EdwardsPoint] = {
    val h = Hasher[Sha512].digest(ByteVector(0x04, 0x01) ++ pk ++ alpha).bytes.take(32)
    fromUniformDraft03(h.update(31, (h(31) & 0x7f).toByte))
  }

  // ------------------------------------------------------------------ RFC 9380 edwards25519_XMD:SHA-512_ELL2_NU_

  /** `hash_to_field` with count 1: 48 uniform bytes, big-endian, reduced mod p. */
  def hashToField(msg: ByteVector, dst: ByteVector): Option[BigInt] =
    ExpandMessage.xmd[Sha512](msg, dst, 48).toOption.map(b => BigInt(1, b.toArray).mod(p))

  /** `map_to_curve_elligator2` on curve25519 (§6.7.1, J = 486662, K = 1, Z = 2) followed by the rational map (G.2.2). */
  def mapToCurveEll2(u: BigInt): Option[(BigInt, BigInt)] = {
    val x1a = mod(-J * inv0(1 + 2 * u * u))
    val x1 = if (x1a == 0) mod(-J) else x1a
    def g(x: BigInt) = mod(x * x * x + J * x * x + x)
    val x2 = mod(-x1 - J)
    val (s, t) =
      if (isSquare(g(x1))) (x1, sqrtWithSign(g(x1), 1))
      else (x2, sqrtWithSign(g(x2), 0))
    t.map { tv =>
      if (mod(tv * (s + 1)) == 0) (BigInt(0), BigInt(1))
      else (mod(c1 * s * inv0(tv)), mod((s - 1) * inv0(s + 1)))
    }
  }

  /** `encode_to_curve` for edwards25519_XMD:SHA-512_ELL2_NU_: map one field element, then clear the cofactor. */
  def encodeToCurveEll2(msg: ByteVector, dst: ByteVector): Option[EdwardsPoint] =
    hashToField(msg, dst).flatMap(mapToCurveEll2).flatMap { case (x, y) => decodeStrict(encode(x, y)) }.map(_.multiplyByCofactor())
}
