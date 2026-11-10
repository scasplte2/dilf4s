package xyz.kd5ujc.core.canonical

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector

/**
 * Why a canonical value is not an `A`, and where: `path` runs from the root (map keys and `[i]` array indices). A key
 * may be the caller's, so each segment is a [[Preview]]: the path holds at most `MaxDepth` of them, however long the
 * keys. `reason` quotes caller input only through a [[Preview]] (docs/specs/conformance.md §8).
 */
final case class DecodeFailure(path: List[Preview], reason: DecodeReason) {
  def at(segment: String): DecodeFailure = copy(path = Preview.of(segment) :: path)
  def message: String = s"${if (path.isEmpty) "<root>" else path.map(_.short).mkString(".")}: ${reason.message}"
}

/**
 * Reads a typed value back out of the canonical data model. Decoders check **shape** (types, ranges, sizes, the
 * exact set of map keys); domain invariants that the shape cannot express belong to the domain decoder.
 */
trait CanonicalDecoder[A] {
  def decode(value: CanonicalValue): Either[DecodeFailure, A]

  final def emap[B](f: A => Either[DecodeReason, B]): CanonicalDecoder[B] =
    (v: CanonicalValue) => decode(v).flatMap(a => f(a).leftMap(DecodeFailure(Nil, _)))
  final def map[B](f: A => B): CanonicalDecoder[B] = (v: CanonicalValue) => decode(v).map(f)
}

object CanonicalDecoder {

  def apply[A](implicit D: CanonicalDecoder[A]): CanonicalDecoder[A] = D

  private def fail[A](reason: DecodeReason): Either[DecodeFailure, A] = DecodeFailure(Nil, reason).asLeft

  private def expected[A](what: ValueKind, v: CanonicalValue): Either[DecodeFailure, A] =
    fail(DecodeReason.ExpectedType(Vector(what), ValueKind.of(v)))

  implicit val bool: CanonicalDecoder[Boolean] = {
    case CBool(b) => b.asRight
    case other    => expected(ValueKind.Bool, other)
  }

  implicit val int: CanonicalDecoder[BigInt] = {
    case CInt(i) => i.asRight
    case other   => expected(ValueKind.Int, other)
  }

  implicit val bytes: CanonicalDecoder[ByteVector] = {
    case CBytes(b) => b.asRight
    case other     => expected(ValueKind.Bytes, other)
  }

  implicit val text: CanonicalDecoder[String] = {
    case CText(s) => s.asRight
    case other    => expected(ValueKind.Text, other)
  }

  /** An integer in `lo..hi` inclusive (`uint` is `intRange(0, None)`). A refusal names its size, never its digits. */
  def intRange(lo: Option[BigInt], hi: Option[BigInt]): CanonicalDecoder[BigInt] =
    int.emap(i =>
      Either.cond(
        lo.forall(i >= _) && hi.forall(i <= _),
        i,
        DecodeReason.IntegerRange(i.bitLength, lo, hi)
      )
    )

  /** CDDL `uint` (RFC 8610 major type 0): 0..2^64-1. [[int]] stays the whole numeric domain's integers. */
  val uint: CanonicalDecoder[BigInt] = intRange(Some(BigInt(0)), Some((BigInt(1) << 64) - 1))
  def intIn(lo:  Int, hi:  Int): CanonicalDecoder[Int] = intRange(Some(BigInt(lo)), Some(BigInt(hi))).map(_.toInt)
  def longIn(lo: Long, hi: Long): CanonicalDecoder[Long] = intRange(Some(BigInt(lo)), Some(BigInt(hi))).map(_.toLong)

  def sizedBytes(lo: Long, hi: Option[Long]): CanonicalDecoder[ByteVector] =
    bytes.emap(b => Either.cond(b.size >= lo && hi.forall(b.size <= _), b, DecodeReason.ByteLength(b.size, lo, hi)))

  /** Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes). */
  def sizedText(lo: Int, hi: Option[Int]): CanonicalDecoder[String] =
    text.emap { s =>
      val n = Utf16.utf8Length(s)
      Either.cond(n >= lo && hi.forall(n <= _), s, DecodeReason.TextLength(n, lo, hi))
    }

  /** A homogeneous array with at least `min` and at most `max` elements; failures name the index. */
  def vector[A](min: Int, max: Option[Int])(implicit D: CanonicalDecoder[A]): CanonicalDecoder[Vector[A]] = {
    case CArray(values) =>
      if (values.size < min || max.exists(values.size > _))
        fail(DecodeReason.ArrayLength(values.size, min, max))
      else values.zipWithIndex.traverse { case (v, i) => D.decode(v).leftMap(_.at(s"[$i]")) }
    case other => expected(ValueKind.Array, other)
  }

  /** The fields of a map with exactly the given keys allowed; unknown keys are rejected. */
  final class Fields private[CanonicalDecoder] (entries: CanonicalEntries[CanonicalValue]) {
    def required[A](key: String)(implicit D: CanonicalDecoder[A]): Either[DecodeFailure, A] =
      entries.get(key).toRight(DecodeFailure(Nil, DecodeReason.MissingKey(Preview.of(key)))).flatMap(v => D.decode(v).leftMap(_.at(key)))
    def optional[A](key: String)(implicit D: CanonicalDecoder[A]): Either[DecodeFailure, Option[A]] =
      entries.get(key).traverse(v => D.decode(v).leftMap(_.at(key)))
  }

  /**
   * The map's keys are only compared, never hashed: `allowed` becomes a key set in the map's order (a name listed twice
   * counts once), and every key of the map is looked up in it. A refusal counts the unknown keys and previews the first
   * in canonical order (shorter UTF-8 first, then by code point, as every SDK iterates a map), in linear passes; it never
   * lists them.
   */
  def record[A](allowed: Iterable[String])(f: Fields => Either[DecodeFailure, A]): CanonicalDecoder[A] = {
    case CMap(entries) =>
      val known = CanonicalKeys.like(entries, allowed)
      val unknown = entries.keysIterator.count(k => !known.contains(k))
      if (unknown > 0) {
        val first = entries.keysIterator.filterNot(k => known.contains(k)).minOption(using CanonicalKeyOrder).getOrElse("")
        fail(DecodeReason.UnknownKeys(unknown, Preview.of(first)))
      } else f(new Fields(entries))
    case other => expected(ValueKind.Map, other)
  }

  // the canonical order of encoded keys; each comparison measures both keys, so finding the least is linear in them
  private val CanonicalKeyOrder: Ordering[String] = Ordering.by[String, Long](Utf16.utf8Length).orElse(CodePointOrder)

  /** The keys present in a map value, for choosing between record alternatives. */
  def keysOf(value: CanonicalValue): Option[CanonicalKeys] = value match {
    case CMap(entries) => Some(entries.keySet)
    case _             => None
  }
}
