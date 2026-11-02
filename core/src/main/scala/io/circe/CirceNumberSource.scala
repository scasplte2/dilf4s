package io.circe

/**
 * The source text circe keeps for a number it parsed. circe exposes it only through `toString`, which for a number
 * built from a `BigDecimal` renders the coefficient instead, a superlinear conversion; its number classes are
 * `private[circe]`, so this bridge lives in circe's package, and a change to those classes fails to compile here.
 */
object CirceNumberSource {

  /** The text `n` was parsed from, or `None` for a number built from a converted value. */
  def apply(n: JsonNumber): Option[String] =
    n match {
      case JsonDecimal(input)          => Some(input)
      case JsonBiggerDecimal(_, input) => Some(input)
      case _                           => None
    }
}
