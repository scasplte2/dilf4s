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

  /** Any canonical value, unchanged (CDDL `any`). */
  val any: Codec[CanonicalValue] = Codec(_.asRight, (v: CanonicalValue) => v.asRight)

  /**
   * A codec referenced by name and resolved on first use: recursive rules (an expression containing expressions)
   * would otherwise initialise in a cycle.
   */
  def defer[A](codec: => Codec[A]): Codec[A] = {
    lazy val c = codec
    Codec(a => c.encode(a), (v: CanonicalValue) => c.decoder.decode(v))
  }

  val bool: Codec[Boolean] = Codec(b => CBool(b).asRight, CanonicalDecoder.bool)
  val bytes: Codec[ByteVector] = Codec(b => CBytes(b).asRight, CanonicalDecoder.bytes)
  val text: Codec[String] = Codec(s => CanonicalValue.text(s), CanonicalDecoder.text)

  def intRange(lo: Option[BigInt], hi: Option[BigInt]): Codec[BigInt] =
    Codec(
      i =>
        Either
          .cond(lo.forall(i >= _) && hi.forall(i <= _), i, mismatch(s"integer of ${i.bitLength} bits outside the schema range"))
          .flatMap(CanonicalValue.int),
      CanonicalDecoder.intRange(lo, hi)
    )

  /**
   * CDDL integers follow RFC 8610: `uint` is major type 0 (0..2^64-1), `nint` major type 1 (-2^64..-1), and `int`
   * either. A wider integer must be an explicit range in the schema.
   */
  val UintMax: BigInt = (BigInt(1) << 64) - 1
  val NintMin: BigInt = -(BigInt(1) << 64)

  val uint: Codec[BigInt] = intRange(Some(BigInt(0)), Some(UintMax))
  val nint: Codec[BigInt] = intRange(Some(NintMin), Some(BigInt(-1)))
  val int: Codec[BigInt] = intRange(Some(NintMin), Some(UintMax))

  def intIn(lo: Int, hi: Int): Codec[Int] =
    Codec(i => Either.cond(i >= lo && i <= hi, CInt(i), mismatch(s"integer $i outside $lo..$hi")), CanonicalDecoder.intIn(lo, hi))

  def longIn(lo: Long, hi: Long): Codec[Long] =
    Codec(i => Either.cond(i >= lo && i <= hi, CInt(i), mismatch(s"integer $i outside $lo..$hi")), CanonicalDecoder.longIn(lo, hi))

  def sizedBytes(lo: Long, hi: Option[Long]): Codec[ByteVector] =
    Codec(
      b =>
        Either.cond(b.size >= lo && hi.forall(b.size <= _), CBytes(b), mismatch(s"byte string of ${b.size} bytes outside the schema size")),
      CanonicalDecoder.sizedBytes(lo, hi)
    )

  /** Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes). */
  def sizedText(lo: Int, hi: Option[Int]): Codec[String] =
    Codec(
      s =>
        CanonicalValue.text(s).flatMap { t =>
          val n = Utf16.utf8Length(s)
          Either.cond(n >= lo && hi.forall(n <= _), t, mismatch(s"text of $n UTF-8 bytes outside the schema size"))
        },
      CanonicalDecoder.sizedText(lo, hi)
    )

  /** Exactly the integer `a`; `A` is its singleton type, so the value is fixed by the type. */
  def literalInt[A <: Int & Singleton](a: A): Codec[A] =
    Codec(
      _ => CInt(a).asRight,
      CanonicalDecoder.int.emap(i => Either.cond(i == BigInt(a), a, s"expected $a, got an integer of ${i.bitLength} bits"))
    )

  /** Exactly the text `a`, as its singleton type. */
  def literalText[A <: String & Singleton](a: A): Codec[A] =
    Codec(_ => CanonicalValue.text(a), CanonicalDecoder.text.emap(t => Either.cond(t == a, a, s"expected \"$a\", got ${Preview.of(t)}")))

  /**
   * Text matching the whole of `pattern` (CDDL `.regexp`). The generator compiles every pattern before emitting it, so
   * the patterns reaching this point are valid.
   */
  def matching(pattern: scala.util.matching.Regex): Codec[String] =
    Codec(
      t => Either.cond(pattern.matches(t), t, mismatch(s"text does not match ${pattern.regex}")).flatMap(CanonicalValue.text),
      CanonicalDecoder.text.emap(t => Either.cond(pattern.matches(t), t, s"${Preview.of(t)} does not match ${pattern.regex}"))
    )

  /**
   * A map with arbitrary text keys and values of one type (CDDL `{ * text => T }`). Its keys are the caller's, so it is
   * ordered, never hashed: decoded, in the order of the map it is read from; to encode, in any order (the encoding sorts them).
   */
  def textMap[A](value: Codec[A]): Codec[CanonicalEntries[A]] =
    Codec(
      m => m.toList.traverse { case (k, v) => value.encode(v).map(k -> _) }.flatMap(CanonicalValue.map),
      {
        case CMap(entries) =>
          entries.toList.traverse { case (k, v) => value.decoder.decode(v).leftMap(_.at(k)).map(k -> _) }
            .map(CanonicalEntries.insertedLike(entries, _))
        case _ => DecodeFailure(Nil, "expected a map").asLeft
      }
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
    alternatives: List[(Iterable[String], Iterable[String], CanonicalDecoder[A])],
    texts:        List[(String, A)] = Nil
  ): Either[DecodeFailure, A] =
    CanonicalDecoder.keysOf(value) match {
      case None =>
        value match {
          case CText(t) => texts.collectFirst { case (`t`, a) => a }.toRight(DecodeFailure(Nil, s"${Preview.of(t)} is not an alternative"))
          case _        => DecodeFailure(Nil, "expected a map or one of the text alternatives").asLeft
        }
      case Some(keys) =>
        // the schema's names are looked up in the value's keys, and the value's keys in the alternative's names (a key
        // set in the same order, where a name listed twice counts once): no caller key is hashed
        alternatives.collectFirst {
          case (required, all, d) if required.forall(k => keys.contains(k)) && keys.iterator.forall(CanonicalKeys.within(keys, all)) => d
        }
          .toRight(DecodeFailure(Nil, s"no alternative fits a map of ${keys.size} keys"))
          .flatMap(_.decode(value))
    }
}
