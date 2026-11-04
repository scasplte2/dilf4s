package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.numerics.ArithmeticError

/**
 * Why a canonical value, encoding or JSON text was refused. A refusal describes caller input by its size, or by a
 * [[Preview]], never by the input itself (docs/specs/conformance.md §8).
 */
sealed trait CanonicalError extends Product with Serializable

object CanonicalError {

  // data model
  case object UnpairedSurrogate extends CanonicalError
  final case class DuplicateKey(key: Preview) extends CanonicalError
  final case class RationalTooWide(limit: Int) extends CanonicalError

  // JSON mapping
  final case class JsonSyntax(offset: Int, reason: String) extends CanonicalError
  final case class InvalidNumber(source: NumberSource, cause: Option[ArithmeticError]) extends CanonicalError
  final case class NotJsonRepresentable(kind: String) extends CanonicalError
  final case class TypeMismatch(reason: String) extends CanonicalError

  // decoding
  final case class UnexpectedEnd(offset: Long) extends CanonicalError
  final case class TrailingBytes(offset: Long) extends CanonicalError
  final case class DepthExceeded(limit: Int) extends CanonicalError
  final case class NonCanonical(offset: Long, reason: String) extends CanonicalError
  final case class Unsupported(offset: Long, reason: String) extends CanonicalError
  final case class InvalidUtf8(offset: Long) extends CanonicalError
}

/** A refused JSON number, by its size: its text's length and offset, or a converted decimal's scale and width. */
sealed trait NumberSource extends Product with Serializable

object NumberSource {

  /** A number read from text: `length` characters at `offset`. */
  final case class Text(length: Int, offset: Int) extends NumberSource

  /** A decimal circe converted: its scale and its coefficient's bit length. */
  final case class Decimal(scale: Int, coefficientBits: Int) extends NumberSource

  /** A circe number with no decimal value. */
  case object NoDecimal extends NumberSource
}
