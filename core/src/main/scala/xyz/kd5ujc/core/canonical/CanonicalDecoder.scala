package xyz.kd5ujc.core.canonical

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector

/** Why a canonical value is not an `A`, and where: `path` runs from the root (map keys and `[i]` array indices). */
final case class DecodeFailure(path: List[String], reason: String) {
  def at(segment: String): DecodeFailure = copy(path = segment :: path)
  def message: String = s"${if (path.isEmpty) "<root>" else path.mkString(".")}: $reason"
}

/**
 * Reads a typed value back out of the canonical data model. Decoders check **shape** (types, ranges, sizes, the
 * exact set of map keys); domain invariants that the shape cannot express belong to the domain decoder.
 */
trait CanonicalDecoder[A] {
  def decode(value: CanonicalValue): Either[DecodeFailure, A]

  final def emap[B](f: A => Either[String, B]): CanonicalDecoder[B] =
    (v: CanonicalValue) => decode(v).flatMap(a => f(a).leftMap(DecodeFailure(Nil, _)))
  final def map[B](f: A => B): CanonicalDecoder[B] = (v: CanonicalValue) => decode(v).map(f)
}

object CanonicalDecoder {

  def apply[A](implicit D: CanonicalDecoder[A]): CanonicalDecoder[A] = D

  private def fail[A](reason: String): Either[DecodeFailure, A] = DecodeFailure(Nil, reason).asLeft

  private def kind(v: CanonicalValue): String = v match {
    case CNull        => "null"
    case CBool(_)     => "bool"
    case CInt(_)      => "int"
    case CRational(_) => "rational"
    case CText(_)     => "text"
    case CBytes(_)    => "bytes"
    case CArray(_)    => "array"
    case CMap(_)      => "map"
  }

  private def expected[A](what: String, v: CanonicalValue): Either[DecodeFailure, A] = fail(s"expected $what, got ${kind(v)}")

  implicit val bool: CanonicalDecoder[Boolean] = {
    case CBool(b) => b.asRight
    case other    => expected("bool", other)
  }

  implicit val int: CanonicalDecoder[BigInt] = {
    case CInt(i) => i.asRight
    case other   => expected("int", other)
  }

  implicit val bytes: CanonicalDecoder[ByteVector] = {
    case CBytes(b) => b.asRight
    case other     => expected("bytes", other)
  }

  implicit val text: CanonicalDecoder[String] = {
    case CText(s) => s.asRight
    case other    => expected("text", other)
  }

  /** An integer in `lo..hi` inclusive (`uint` is `intRange(0, None)`). */
  def intRange(lo: Option[BigInt], hi: Option[BigInt]): CanonicalDecoder[BigInt] =
    int.emap(i =>
      Either.cond(lo.forall(i >= _) && hi.forall(i <= _), i, s"integer $i outside ${lo.fold("")(_.toString)}..${hi.fold("")(_.toString)}")
    )

  /** CDDL `uint` (RFC 8610 major type 0): 0..2^64-1. [[int]] stays the whole numeric domain's integers. */
  val uint: CanonicalDecoder[BigInt] = intRange(Some(BigInt(0)), Some((BigInt(1) << 64) - 1))
  def intIn(lo:  Int, hi:  Int): CanonicalDecoder[Int] = intRange(Some(BigInt(lo)), Some(BigInt(hi))).map(_.toInt)
  def longIn(lo: Long, hi: Long): CanonicalDecoder[Long] = intRange(Some(BigInt(lo)), Some(BigInt(hi))).map(_.toLong)

  def sizedBytes(lo: Long, hi: Option[Long]): CanonicalDecoder[ByteVector] =
    bytes.emap(b =>
      Either.cond(b.size >= lo && hi.forall(b.size <= _), b, s"byte string of ${b.size} bytes, expected $lo..${hi.fold("")(_.toString)}")
    )

  /** Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes). */
  def sizedText(lo: Int, hi: Option[Int]): CanonicalDecoder[String] =
    text.emap { s =>
      val n = Utf16.utf8Length(s)
      Either.cond(n >= lo && hi.forall(n <= _), s, s"text of $n UTF-8 bytes, expected $lo..${hi.fold("")(_.toString)}")
    }

  /** A homogeneous array with at least `min` and at most `max` elements; failures name the index. */
  def vector[A](min: Int, max: Option[Int])(implicit D: CanonicalDecoder[A]): CanonicalDecoder[Vector[A]] = {
    case CArray(values) =>
      if (values.size < min || max.exists(values.size > _))
        fail(s"array of ${values.size} elements, expected $min..${max.fold("")(_.toString)}")
      else values.zipWithIndex.traverse { case (v, i) => D.decode(v).leftMap(_.at(s"[$i]")) }
    case other => expected("array", other)
  }

  /** The fields of a map with exactly the given keys allowed; unknown keys are rejected. */
  final class Fields private[CanonicalDecoder] (entries: Map[String, CanonicalValue]) {
    def required[A](key: String)(implicit D: CanonicalDecoder[A]): Either[DecodeFailure, A] =
      entries.get(key).toRight(DecodeFailure(Nil, s"missing required key \"$key\"")).flatMap(v => D.decode(v).leftMap(_.at(key)))
    def optional[A](key: String)(implicit D: CanonicalDecoder[A]): Either[DecodeFailure, Option[A]] =
      entries.get(key).traverse(v => D.decode(v).leftMap(_.at(key)))
  }

  def record[A](allowed: Set[String])(f: Fields => Either[DecodeFailure, A]): CanonicalDecoder[A] = {
    case CMap(entries) =>
      val unknown = entries.keySet -- allowed
      if (unknown.nonEmpty) fail(s"unknown keys ${unknown.toList.sorted.mkString(", ")}")
      else f(new Fields(entries))
    case other => expected("map", other)
  }

  /** The keys present in a map value, for choosing between record alternatives. */
  def keysOf(value: CanonicalValue): Option[Set[String]] = value match {
    case CMap(entries) => Some(entries.keySet)
    case _             => None
  }
}
