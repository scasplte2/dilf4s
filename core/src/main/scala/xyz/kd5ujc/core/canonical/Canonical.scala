package xyz.kd5ujc.core.canonical

import cats.syntax.either._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import scodec.bits.ByteVector

/** How a type maps into the canonical data model. */
trait Canonical[A] {
  def canonical(a: A): Either[CanonicalError, CanonicalValue]
}

object Canonical {

  def apply[A](implicit C: Canonical[A]): Canonical[A] = C

  def instance[A](f: A => Either[CanonicalError, CanonicalValue]): Canonical[A] = (a: A) => f(a)

  def total[A](f: A => CanonicalValue): Canonical[A] = (a: A) => f(a).asRight

  /**
   * Derive from a circe `Encoder`. Opt-in rather than implicit: an `Encoder[Double]` anywhere in the type would route a
   * number through its shortest decimal rendering, so binary floating point should stay out of canonical types.
   */
  def fromEncoder[A: Encoder]: Canonical[A] = instance(a => CanonicalJson.fromCirce(a.asJson))

  /** Decode a canonical value into typed data through its circe `Decoder`; the inverse of [[fromEncoder]]. */
  def decode[A: Decoder](value: CanonicalValue): Either[CanonicalError, A] =
    CanonicalJson.toCirce(value).flatMap(_.as[A].leftMap(e => CanonicalError.TypeMismatch(e.getMessage)))

  implicit val canonicalValue: Canonical[CanonicalValue] = total(identity)
  implicit val canonicalBoolean: Canonical[Boolean] = total(CBool(_))
  implicit val canonicalInt: Canonical[Int] = total(i => CInt(BigInt(i)))
  implicit val canonicalLong: Canonical[Long] = total(l => CInt(BigInt(l)))
  implicit val canonicalBigInt: Canonical[BigInt] = total(CInt(_))
  implicit val canonicalRatio: Canonical[Ratio] = total(CanonicalValue.number)
  implicit val canonicalString: Canonical[String] = instance(CanonicalValue.text)
  implicit val canonicalBytes: Canonical[ByteVector] = total(CBytes(_))
}
