package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.numerics.ArithmeticError

sealed trait CanonicalError extends Product with Serializable

object CanonicalError {

  // data model
  final case class UnpairedSurrogate(value: String) extends CanonicalError
  final case class DuplicateKey(key: String) extends CanonicalError

  // JSON mapping
  final case class JsonSyntax(offset: Int, reason: String) extends CanonicalError
  final case class InvalidNumber(source: String, cause: Option[ArithmeticError]) extends CanonicalError
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
