package xyz.kd5ujc.core.circe

import cats.Eval
import cats.syntax.either._
import cats.syntax.traverse._

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical._
import xyz.kd5ujc.numerics.Ratio

import io.circe.DecodingFailure.Reason
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonNumber}

/**
 * Typed data with circe codecs, in and out of the canonical data model. This module is the only one that pairs core
 * with circe: core has no circe dependency, so nothing in it can reach circe's parser or its hashed `JsonObject`.
 * JSON text, and anything a caller chose, goes through [[CanonicalJson.parse]]; this bridge is for values a program
 * built, such as `a.asJson`. It uses core's public API only (by inspection: as a `kd5ujc` package it could reach
 * `private[kd5ujc]` members, and does not).
 */
object CanonicalCirce {

  // 10^MaxDigits has this many bits: a coefficient any wider has more than MaxDigits digits (limits.cddl)
  private val MaxDigitBits: Int = xyz.kd5ujc.models.limits.CanonicalMaxDigitBits

  /**
   * Converts circe JSON a program built; stack-safe. Numbers use their exact decimal value, within
   * [[CanonicalJson.parse]]'s bounds. Not a way in for untrusted text: circe parsing has already merged repeated
   * member names (and hashed the keys), so parse such text with [[CanonicalJson.parse]]. A number circe parsed is read
   * from its source text only so that it reads as the parser reads it.
   */
  def fromCirce(json: Json, maxScale: Int = CanonicalJson.MaxScale): Either[CanonicalError, CanonicalValue] =
    CanonicalJson.scaleBound(maxScale).flatMap(circe(json, _).value)

  private def circe(json: Json, maxScale: Int): Eval[Either[CanonicalError, CanonicalValue]] =
    json.fold(
      jsonNull = Eval.now(CNull.asRight),
      jsonBoolean = b => Eval.now(CBool(b).asRight),
      jsonNumber = n => Eval.now(circeNumber(n, maxScale)),
      jsonString = s => Eval.now(CanonicalValue.text(s)),
      jsonArray = vs => Eval.defer(vs.traverse(circe(_, maxScale))).map(_.sequence.flatMap(CanonicalValue.array)),
      jsonObject = o =>
        Eval
          .defer(o.toVector.traverse { case (k, v) => circe(v, maxScale).map(_.map(k -> _)) })
          .map(_.sequence.flatMap(CanonicalValue.map))
    )

  /**
   * A number circe parsed keeps its source text, which [[CanonicalJson.parse]] reads before circe converts anything:
   * so the bridge accepts and refuses each number exactly as the parser does. The parser's exponent budget is a whole
   * text's, for text a caller chose, which this bridge does not take. A number built from a converted value has its
   * coefficient's width checked before it is ever rendered.
   */
  private def circeNumber(n: JsonNumber, maxScale: Int): Either[CanonicalError, CanonicalValue] =
    io.circe.CirceNumberSource(n).fold(convertedNumber(n, maxScale)) { text =>
      CanonicalJson.parse(text, maxScale).flatMap {
        case v @ (CInt(_) | CRational(_)) => v.asRight
        case _                            => InvalidNumber(Some(text.length.toLong), Some(0L), None).asLeft
      }
    }

  private def convertedNumber(n: JsonNumber, maxScale: Int): Either[CanonicalError, CanonicalValue] =
    n.toBigDecimal
      .toRight(InvalidNumber(None, None, None))
      .flatMap { bd =>
        val unscaled = BigInt(bd.bigDecimal.unscaledValue)
        val digits = if (unscaled.bitLength > MaxDigitBits) CanonicalJson.MaxDigits + 1 else unscaled.abs.toString.length
        if (digits > CanonicalJson.MaxDigits)
          InvalidNumber(None, None, Some(NumberBound.Digits(digits.toLong, CanonicalJson.MaxDigits.toLong))).asLeft
        else Ratio.fromBigDecimal(bd, maxScale).leftMap(e => InvalidNumber(None, None, NumberBounds.of(e))).flatMap(CanonicalValue.number)
      }

  /**
   * The inverse bridge, for decoding into typed data. Numbers stay exact; fails where [[CanonicalJson.render]] fails,
   * since each number is checked by rendering it there first.
   */
  def toCirce(value: CanonicalValue): Either[CanonicalError, Json] = toCirceEval(value).value

  private def toCirceEval(value: CanonicalValue): Eval[Either[CanonicalError, Json]] =
    value match {
      case CNull        => Eval.now(Json.Null.asRight)
      case CBool(b)     => Eval.now(Json.fromBoolean(b).asRight)
      case CInt(n)      => Eval.now(Json.fromBigInt(n).asRight)
      case r: CRational => Eval.now(CanonicalJson.rationalText(r).map(d => Json.fromBigDecimal(BigDecimal(d))))
      case t: CText     => Eval.now(Json.fromString(t.value).asRight)
      case CBytes(_)    => Eval.now(NotJsonRepresentable("bytes").asLeft)
      case CArray(vs)   => Eval.defer(vs.traverse(toCirceEval)).map(_.sequence.map(Json.fromValues))
      case CMap(m) =>
        Eval
          .defer(m.toVector.traverse { case (k, v) => toCirceEval(v).map(_.map(k -> _)) })
          .map(_.sequence.map(Json.fromFields))
    }

  /**
   * Derive from a circe `Encoder`. Opt-in rather than implicit: an `Encoder[Double]` anywhere in the type would route a
   * number through its shortest decimal rendering, so binary floating point should stay out of canonical types.
   */
  def fromEncoder[A: Encoder]: Canonical[A] = Canonical.instance(a => fromCirce(a.asJson))

  /** Decode a canonical value into typed data through its circe `Decoder`; the inverse of [[fromEncoder]]. */
  def decode[A: Decoder](value: CanonicalValue): Either[CanonicalError, A] =
    toCirce(value).flatMap(_.as[A].leftMap(e => TypeMismatch(failure(e))))

  // circe's own message quotes the whole value it refused, and a path or custom reason may hold caller keys: the value
  // is named by its kind, the rest previewed
  private def failure(e: DecodingFailure): String = {
    val where = e.pathToRootString.fold("")(p => s" at ${Preview.of(p)}")
    e.reason match {
      case Reason.WrongTypeExpectation(expected, json) => s"expected $expected, got ${json.name}$where"
      case Reason.MissingField                         => s"missing field$where"
      case Reason.CustomReason(message)                => s"${Preview.of(message)}$where"
    }
  }

  /** Both directions through circe, as a [[Codec]]: for a store's values (`ValueCodec.fromCodec`) or a schema slot. */
  def codec[A: Encoder: Decoder]: Codec[A] =
    Codec(a => fromCirce(a.asJson), (v: CanonicalValue) => decode[A](v).leftMap(e => DecodeFailure(Nil, e.toString)))
}
