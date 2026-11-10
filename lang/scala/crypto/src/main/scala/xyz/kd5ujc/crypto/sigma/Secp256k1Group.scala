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

  enum DecodeError {
    case WrongSize(actual: Long, expected: Int)
    case InvalidPrefix(actual: Int)
    case InvalidPoint
    case NonCanonicalEncoding

    def message: String = this match {
      case WrongSize(actual, expected) => s"a group element is $expected bytes, got $actual"
      case InvalidPrefix(_)            => "a group element is compressed: prefix 02 or 03"
      case InvalidPoint                => "not a point of secp256k1"
      case NonCanonicalEncoding        => "not the canonical encoding of a point"
    }
  }

  private val curve = CustomNamedCurves.getByName("secp256k1")

  val Order: BigInt = xyz.kd5ujc.crypto.signing.Secp256k1.Order

  /** Bytes of an encoded element. */
  val ElementSize: Int = 33

  /** Bytes of a scalar (a response `z`). */
  val ScalarSize: Int = 32

  private[sigma] val IdentityBytes: ByteVector = ByteVector.fill(ElementSize.toLong)(0)

  val Generator: GroupElement = GroupElement(curve.getG.normalize())
  val Identity: GroupElement = GroupElement(curve.getCurve.getInfinity)

  def decode(b: ByteVector): Either[DecodeError, GroupElement] =
    if (b.size != ElementSize.toLong) DecodeError.WrongSize(b.size, ElementSize).asLeft
    else if (b == IdentityBytes) Identity.asRight
    else if (b(0) != 0x02 && b(0) != 0x03) DecodeError.InvalidPrefix(b(0) & 0xff).asLeft
    else
      Either
        .catchNonFatal(curve.getCurve.decodePoint(b.toArray).normalize())
        .leftMap(_ => DecodeError.InvalidPoint)
        .map(GroupElement(_))
        .filterOrElse(_.bytes == b, DecodeError.NonCanonicalEncoding)

  /**
   * Whether `p` is a point of this group's curve. Every element built here is; one built around the constructor (a
   * product mirror) may hold a point of another curve, or one off any curve.
   */
  def isPoint(p: GroupElement): Boolean = (p.point.getCurve eq curve.getCurve) && p.point.isValid

  /** `k·G`, with the fixed-base multiplier. */
  def base(k: BigInt): GroupElement =
    GroupElement(new FixedPointCombMultiplier().multiply(curve.getG, k.mod(Order).bigInteger).normalize())

  /** A 32-byte big-endian scalar. */
  def scalarBytes(k: BigInt): ByteVector = ByteVector.view(k.toByteArray).takeRight(ScalarSize.toLong).padLeft(ScalarSize.toLong)
}
