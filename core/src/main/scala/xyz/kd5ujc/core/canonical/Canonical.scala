package xyz.kd5ujc.core.canonical

import cats.syntax.either._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/** How a type maps into the canonical data model. A circe `Encoder` gives one through `CanonicalCirce.fromEncoder`. */
trait Canonical[A] {
  def canonical(a: A): Either[CanonicalError, CanonicalValue]
}

object Canonical {

  def apply[A](implicit C: Canonical[A]): Canonical[A] = C

  def instance[A](f: A => Either[CanonicalError, CanonicalValue]): Canonical[A] = (a: A) => f(a)

  def total[A](f: A => CanonicalValue): Canonical[A] = (a: A) => f(a).asRight

  implicit val canonicalValue: Canonical[CanonicalValue] = total(identity)
  implicit val canonicalBoolean: Canonical[Boolean] = total(CBool(_))
  implicit val canonicalInt: Canonical[Int] = total(CInt(_))
  implicit val canonicalLong: Canonical[Long] = total(CInt(_))
  implicit val canonicalBigInt: Canonical[BigInt] = instance(CanonicalValue.int)
  implicit val canonicalRatio: Canonical[Ratio] = instance(CanonicalValue.number)
  implicit val canonicalString: Canonical[String] = instance(CanonicalValue.text)
  implicit val canonicalBytes: Canonical[ByteVector] = total(CBytes(_))
}
