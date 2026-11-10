package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.numerics.ArithmeticError

/**
 * Why a canonical value, encoding or JSON text was refused, and its reasons: generated from
 * `shared/schemas/codec-errors.cddl`, with the Rust and TypeScript SDKs' kinds and fields. A refusal describes caller
 * input by its size, or by a [[Preview]], never by the input itself (docs/specs/conformance.md §8).
 */
export xyz.kd5ujc.models.codec_errors.{
  CanonicalError,
  CanonicalErrorKind,
  JsonSyntaxReason,
  NonCanonicalReason,
  NotJson,
  NumberBound,
  NumberBoundKind,
  UnsupportedItem,
  UnsupportedItemKind
}

object NumberBounds {

  /** The bound a decimal's arithmetic refusal names, if it names one (a division by zero names none). */
  def of(e: ArithmeticError): Option[NumberBound] = e match {
    case ArithmeticError.DigitsOutOfRange(digits, max) => Some(NumberBound.Digits(digits.toLong, max.toLong))
    case ArithmeticError.ScaleOutOfRange(scale, max)   => Some(NumberBound.Scale(scale, max.toLong))
    case ArithmeticError.DivisionByZero                => None
  }
}
