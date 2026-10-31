package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.{ECPoint, FixedPointCombMultiplier}
import scodec.bits.ByteVector

/** A point of secp256k1, including the identity. */
final case class GroupElement private[sigma] (point: ECPoint) {
  def +(that: GroupElement): GroupElement = GroupElement(point.add(that.point).normalize())
  def *(k:    BigInt): GroupElement = GroupElement(point.multiply(k.mod(Secp256k1Group.Order).bigInteger).normalize())
  def unary_- : GroupElement = GroupElement(point.negate().normalize())
  def isIdentity: Boolean = point.isInfinity

  /** 33 bytes: SEC1 compressed, or 33 zero bytes for the identity (Ergo's encoding). */
  def bytes: ByteVector =
    if (point.isInfinity) Secp256k1Group.IdentityBytes else ByteVector.view(point.getEncoded(true))

}

/**
 * The secp256k1 group for sigma protocols, over BouncyCastle (pure Java, verification only needs public data). Only
 * canonical encodings decode: 33 bytes, prefix 02 or 03 with an x below the field modulus, or 33 zero bytes for the
 * identity. Programs compare points as bytes, so a second encoding of the same point must never be accepted.
 */
object Secp256k1Group {

  private val curve = CustomNamedCurves.getByName("secp256k1")

  val Order: BigInt = BigInt(curve.getN)

  /** Bytes of an encoded element. */
  val ElementSize: Int = 33

  /** Bytes of a scalar (a response `z`). */
  val ScalarSize: Int = 32

  private[sigma] val IdentityBytes: ByteVector = ByteVector.fill(ElementSize.toLong)(0)

  val Generator: GroupElement = GroupElement(curve.getG.normalize())
  val Identity: GroupElement = GroupElement(curve.getCurve.getInfinity)

  def decode(b: ByteVector): Either[String, GroupElement] =
    if (b.size != ElementSize.toLong) s"a group element is $ElementSize bytes, got ${b.size}".asLeft
    else if (b == IdentityBytes) Identity.asRight
    else if (b(0) != 0x02 && b(0) != 0x03) "a group element is compressed: prefix 02 or 03".asLeft
    else
      Either
        .catchNonFatal(curve.getCurve.decodePoint(b.toArray).normalize())
        .leftMap(_ => "not a point of secp256k1")
        .map(GroupElement(_))
        .filterOrElse(_.bytes == b, "not the canonical encoding of a point")

  /** `k·G`, with the fixed-base multiplier. */
  def base(k: BigInt): GroupElement =
    GroupElement(new FixedPointCombMultiplier().multiply(curve.getG, k.mod(Order).bigInteger).normalize())

  /** A 32-byte big-endian scalar. */
  def scalarBytes(k: BigInt): ByteVector = ByteVector.view(k.toByteArray).takeRight(ScalarSize.toLong).padLeft(ScalarSize.toLong)
}
