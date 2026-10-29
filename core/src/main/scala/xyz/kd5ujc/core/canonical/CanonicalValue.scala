package xyz.kd5ujc.core.canonical

import cats.Show
import cats.syntax.either._

import scala.annotation.tailrec

import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * The canonical data model (docs/specs/canonical-encoding.md §1). Invariants are enforced by the smart constructors in
 * the companion: rationals are never integers, text holds no unpaired surrogates, maps hold unique keys and no null
 * values, and nesting never exceeds [[CanonicalValue.MaxDepth]] — so every constructible value is decodable.
 */
sealed trait CanonicalValue extends Product with Serializable {

  /** Nesting depth: 0 for scalars, 1 + the deepest child for arrays and maps. */
  def depth: Int
}

object CanonicalValue {

  val MaxDepth: Int = 256

  implicit val canonicalValueShow: Show[CanonicalValue] = Show.fromToString

  sealed trait Scalar extends CanonicalValue {
    final val depth: Int = 0
  }

  case object CNull extends Scalar
  final case class CBool(value: Boolean) extends Scalar
  final case class CInt(value: BigInt) extends Scalar
  sealed abstract case class CRational(value: Ratio) extends Scalar
  sealed abstract case class CText(value: String) extends Scalar
  final case class CBytes(value: ByteVector) extends Scalar

  sealed abstract case class CArray(values: Vector[CanonicalValue]) extends CanonicalValue {
    final val depth: Int = 1 + values.foldLeft(0)((d, v) => math.max(d, v.depth))
  }

  sealed abstract case class CMap(entries: Map[String, CanonicalValue]) extends CanonicalValue {
    final val depth: Int = 1 + entries.foldLeft(0) { case (d, (_, v)) => math.max(d, v.depth) }
  }

  val True: CanonicalValue = CBool(true)
  val False: CanonicalValue = CBool(false)
  val EmptyArray: CArray = new CArray(Vector.empty) {}
  val EmptyMap: CMap = new CMap(Map.empty) {}

  def int(value: BigInt): CanonicalValue = CInt(value)

  /** A number in the single numeric domain: integral values are always [[CInt]]. */
  def number(value: Ratio): CanonicalValue =
    value.toBigIntExact.fold[CanonicalValue](new CRational(value) {})(CInt(_))

  def text(value: String): Either[CanonicalError, CText] =
    Either.cond(Utf16.isWellFormed(value), new CText(value) {}, CanonicalError.UnpairedSurrogate(value))

  def bytes(value: ByteVector): CanonicalValue = CBytes(value)

  def array(values: Vector[CanonicalValue]): Either[CanonicalError, CArray] =
    withinDepth(new CArray(values) {})

  /** Builds a map, dropping null-valued entries; invalid or duplicate keys are rejected. */
  def map(entries: Iterable[(String, CanonicalValue)]): Either[CanonicalError, CMap] =
    collect(entries.toList, Set.empty, Map.empty).flatMap(m => withinDepth(new CMap(m) {}))

  /** For the strict decoder, which bounds depth and has verified unique, ordered text keys and non-null values. */
  private[canonical] def decodedArray(values: Vector[CanonicalValue]): CArray = new CArray(values) {}
  private[canonical] def decodedMap(entries: Vector[(String, CanonicalValue)]): CMap = new CMap(entries.toMap) {}

  private def withinDepth[A <: CanonicalValue](v: A): Either[CanonicalError, A] =
    Either.cond(v.depth <= MaxDepth, v, CanonicalError.DepthExceeded(MaxDepth))

  @tailrec
  private def collect(
    entries: List[(String, CanonicalValue)],
    seen:    Set[String],
    acc:     Map[String, CanonicalValue]
  ): Either[CanonicalError, Map[String, CanonicalValue]] =
    entries match {
      case Nil                                   => acc.asRight
      case (k, _) :: _ if seen.contains(k)       => CanonicalError.DuplicateKey(k).asLeft
      case (k, _) :: _ if !Utf16.isWellFormed(k) => CanonicalError.UnpairedSurrogate(k).asLeft
      case (k, CNull) :: rest                    => collect(rest, seen + k, acc)
      case (k, v) :: rest                        => collect(rest, seen + k, acc.updated(k, v))
    }
}

/** UTF-16 well-formedness: every surrogate is part of a high/low pair. */
private[canonical] object Utf16 {

  def isWellFormed(s: String): Boolean = check(s, 0)

  @tailrec
  private def check(s: String, i: Int): Boolean =
    if (i >= s.length) true
    else {
      val c = s.charAt(i)
      if (Character.isHighSurrogate(c))
        i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)) && check(s, i + 2)
      else !Character.isLowSurrogate(c) && check(s, i + 1)
    }
}
