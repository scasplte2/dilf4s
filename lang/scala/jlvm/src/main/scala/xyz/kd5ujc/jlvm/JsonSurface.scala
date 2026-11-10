package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}
import xyz.kd5ujc.jlvm.JlvmError._

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

  /**
   * Strings in `0x`-hex form become bytes, everywhere in the value (map keys stay text). Total, and stack-safe at any
   * depth: an explicit stack, each container rebuilt once. O(weight): a part shared in `v` is rebuilt each time it
   * occurs, so this is for values read from text (which share nothing), or bounded by weight first.
   */
  def fromCanonicalJson(v: CanonicalValue): CanonicalValue =
    CanonicalValue.mapScalars(v) {
      case t: CText => hexBytes(t.value).fold[Scalar](t)(CBytes(_))
      case other    => other
    }

  /**
   * Bytes become text in `0x`-hex form, everywhere in the value: what [[render]] writes, as a value. Stack-safe at any
   * depth, as [[fromCanonicalJson]]. A value heavier than `CanonicalJson.MaxRenderBytes` has no rendering, and is
   * refused from its weight before anything is built: converting it would rebuild a shared part each time it occurs.
   */
  def toCanonicalJson(v: CanonicalValue): Either[JlvmError, CanonicalValue] =
    renderable(v).map(CanonicalValue.mapScalars(_) {
      case CBytes(b) => CanonicalValue.hexText(b)
      case other     => other
    })

  /** Input data from JSON text: the strict canonical parser (duplicate keys, depth, exact numbers), then bytes. */
  def parse(text: String): Either[JlvmError, CanonicalValue] =
    CanonicalJson.parse(text).bimap(e => JlvmError.Malformed(MalformedReason.Canonical(e)), fromCanonicalJson)

  /**
   * A value as JSON text that [[parse]] reads back as the same value: bytes are written in `0x`-hex form, and the value
   * is never converted or copied. Its rendering is bounded by `CanonicalJson.MaxRenderBytes`, refused from the weight
   * when the weight is over it; a non-terminating rational has none; and text in `0x`-hex form anywhere in the value (it
   * would read back as bytes) has none either, found by one walk once the rendering, which bounds the walk, is written.
   */
  def render(v: CanonicalValue): Either[JlvmError, String] =
    CanonicalJson
      .renderSurfaceWithin(v, CanonicalJson.MaxRenderBytes)
      .leftMap(e => JlvmError.Malformed(MalformedReason.Render(e)))
      .flatTap(_ => Either.cond(!hasHexText(List(v)), (), JlvmError.Malformed(MalformedReason.HexTextSurface)))

  // a value's rendering is never shorter than its weight
  private def renderable(v: CanonicalValue): Either[JlvmError, CanonicalValue] =
    Either.cond(
      v.weight <= CanonicalJson.MaxRenderBytes,
      v,
      JlvmError.Malformed(MalformedReason.Render(CanonicalJson.RenderError.TooLarge(CanonicalJson.MaxRenderBytes)))
    )

  /** Whether a text value (not a map key) in `todo` has the `0x`-hex form, which reads back as bytes. */
  @tailrec
  private def hasHexText(todo: List[CanonicalValue]): Boolean =
    todo match {
      case Nil                => false
      case CText(s) :: rest   => HexBytes.matches(s) || hasHexText(rest)
      case CArray(vs) :: rest => hasHexText(vs.toList ::: rest)
      case (m: CMap) :: rest  => hasHexText(m.entries.valuesIterator.toList ::: rest)
      case _ :: rest          => hasHexText(rest)
    }
}
