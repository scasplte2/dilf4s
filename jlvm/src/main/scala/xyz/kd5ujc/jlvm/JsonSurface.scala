package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}

import io.circe.Json
import scodec.bits.ByteVector

/**
 * Values in the JSON surface (program literals, input data, results). JSON has no byte strings, so a string of the
 * form `0x` followed by an even number of lowercase hex digits **is** bytes, and bytes render back to that form. Text
 * that would look like that must therefore not be written as a plain JSON string (docs/specs/jlvm.md).
 */
object JsonSurface {

  private val HexBytes = "0x((?:[0-9a-f]{2})*)".r

  /** The bytes a string denotes in the JSON surface, if it has the `0x`-hex form. */
  def hexBytes(s: String): Option[ByteVector] =
    s match {
      case HexBytes(hex) => ByteVector.fromHex(hex)
      case _             => None
    }

  /** Strings in `0x`-hex form become bytes, everywhere in the value; depth is bounded by the canonical model (256). */
  def fromCanonicalJson(v: CanonicalValue): CanonicalValue =
    v match {
      case CText(s)   => hexBytes(s).fold(v)(CBytes(_))
      case CArray(vs) => CanonicalValue.array(vs.map(fromCanonicalJson)).getOrElse(v)
      case CMap(m)    => CanonicalValue.map(m.toList.map { case (k, x) => k -> fromCanonicalJson(x) }).getOrElse(v)
      case other      => other
    }

  def toCanonicalJson(v: CanonicalValue): CanonicalValue =
    v match {
      case CBytes(b)  => CanonicalValue.text("0x" + b.toHex).getOrElse(v)
      case CArray(vs) => CanonicalValue.array(vs.map(toCanonicalJson)).getOrElse(v)
      case CMap(m)    => CanonicalValue.map(m.toList.map { case (k, x) => k -> toCanonicalJson(x) }).getOrElse(v)
      case other      => other
    }

  def value(j: Json): Either[JlvmError, CanonicalValue] =
    CanonicalJson.fromCirce(j).bimap(e => JlvmError.Malformed(e.toString), fromCanonicalJson)

  /** Input data from JSON text: the strict canonical parser (duplicate keys, depth, exact numbers), then bytes. */
  def parse(text: String): Either[JlvmError, CanonicalValue] =
    CanonicalJson.parse(text).bimap(e => JlvmError.Malformed(e.toString), fromCanonicalJson)

  /**
   * A value as JSON that reads back as the same value. Text in `0x`-hex form has no such rendering (it would read back
   * as bytes), and neither has a non-terminating rational; both are errors.
   */
  def json(v: CanonicalValue): Either[JlvmError, Json] =
    CanonicalJson
      .toCirce(toCanonicalJson(v))
      .leftMap(e => JlvmError.Malformed(e.toString))
      .flatMap(j => value(j).filterOrElse(_ == v, JlvmError.Malformed("a value with 0x-hex text has no JSON surface form")).as(j))

  /** A result as JSON; a non-terminating rational has no JSON rendering and is an error. */
  def render(v: CanonicalValue): Either[JlvmError, String] =
    CanonicalJson.render(toCanonicalJson(v)).leftMap(e => JlvmError.Malformed(e.toString))
}
