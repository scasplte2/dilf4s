package xyz.kd5ujc.core.canonical

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector

/**
 * Both directions between `A` and the canonical data model, with the same shape constraints: encoding refuses any value
 * the decoder would reject, so a schema-described type can never encode to bytes outside its schema. This is the
 * runtime of the code generated from the CDDL schemas in `shared/schemas`.
 */
final case class Codec[A](encode: A => Either[CanonicalError, CanonicalValue], decoder: CanonicalDecoder[A]) {
  def canonical: Canonical[A] = Canonical.instance(encode)
  def imap[B](to: A => B)(from: B => A): Codec[B] = Codec(b => encode(from(b)), decoder.map(to))
}

object Codec {

  private def mismatch(reason: String): CanonicalError = CanonicalError.TypeMismatch(reason)

  val bool: Codec[Boolean] = Codec(b => CBool(b).asRight, CanonicalDecoder.bool)
  val int: Codec[BigInt] = Codec(i => CInt(i).asRight, CanonicalDecoder.int)
  val bytes: Codec[ByteVector] = Codec(b => CBytes(b).asRight, CanonicalDecoder.bytes)
  val text: Codec[String] = Codec(s => CanonicalValue.text(s), CanonicalDecoder.text)

  def intRange(lo: Option[BigInt], hi: Option[BigInt]): Codec[BigInt] =
    Codec(
      i => Either.cond(lo.forall(i >= _) && hi.forall(i <= _), CInt(i), mismatch(s"integer $i outside the schema range")),
      CanonicalDecoder.intRange(lo, hi)
    )

  val uint: Codec[BigInt] = intRange(Some(BigInt(0)), None)

  def intIn(lo: Int, hi: Int): Codec[Int] =
    Codec(i => Either.cond(i >= lo && i <= hi, CInt(BigInt(i)), mismatch(s"integer $i outside $lo..$hi")), CanonicalDecoder.intIn(lo, hi))

  def longIn(lo: Long, hi: Long): Codec[Long] =
    Codec(i => Either.cond(i >= lo && i <= hi, CInt(BigInt(i)), mismatch(s"integer $i outside $lo..$hi")), CanonicalDecoder.longIn(lo, hi))

  def sizedBytes(lo: Long, hi: Option[Long]): Codec[ByteVector] =
    Codec(
      b =>
        Either.cond(b.size >= lo && hi.forall(b.size <= _), CBytes(b), mismatch(s"byte string of ${b.size} bytes outside the schema size")),
      CanonicalDecoder.sizedBytes(lo, hi)
    )

  def sizedText(lo: Int, hi: Option[Int]): Codec[String] =
    Codec(
      s =>
        Either
          .cond(s.length >= lo && hi.forall(s.length <= _), s, mismatch(s"text of length ${s.length} outside the schema size"))
          .flatMap(CanonicalValue.text),
      CanonicalDecoder.sizedText(lo, hi)
    )

  def vector[A](element: Codec[A], min: Int, max: Option[Int]): Codec[Vector[A]] =
    Codec(
      xs =>
        if (xs.size < min || max.exists(xs.size > _)) mismatch(s"array of ${xs.size} elements outside the schema size").asLeft
        else xs.traverse(element.encode).flatMap(CanonicalValue.array),
      CanonicalDecoder.vector(min, max)(using element.decoder)
    )

  /** One map entry of an encoded record; `None` (an absent optional field) is omitted, never written as null. */
  def field[A](key: String, codec: Codec[A], value: A): Either[CanonicalError, Option[(String, CanonicalValue)]] =
    codec.encode(value).map(v => Some(key -> v))

  def optionalField[A](key: String, codec: Codec[A], value: Option[A]): Either[CanonicalError, Option[(String, CanonicalValue)]] =
    value.flatTraverse(field(key, codec, _))

  def record(fields: List[Either[CanonicalError, Option[(String, CanonicalValue)]]]): Either[CanonicalError, CanonicalValue] =
    fields.sequence.flatMap(fs => CanonicalValue.map(fs.flatten))

  /**
   * The alternative of a record choice whose key set fits `value`: every required key present and no key outside the
   * alternative. The generator only accepts choices where at most one alternative can fit.
   */
  def selectAlternative[A](
    value:        CanonicalValue,
    alternatives: List[(Set[String], Set[String], CanonicalDecoder[A])]
  ): Either[DecodeFailure, A] =
    CanonicalDecoder.keysOf(value) match {
      case None => DecodeFailure(Nil, "expected a map").asLeft
      case Some(keys) =>
        alternatives.collectFirst { case (required, all, d) if required.subsetOf(keys) && keys.subsetOf(all) => d }
          .toRight(DecodeFailure(Nil, s"no alternative has keys ${keys.toList.sorted.mkString(", ")}"))
          .flatMap(_.decode(value))
    }
}
