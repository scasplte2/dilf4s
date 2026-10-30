/*
 * Adapted from Topl Bifrost (https://github.com/Topl/Bifrost) `co.topl.models.utility.Ratio`,
 * by way of Constellation tessellation and metakit. MPL-2.0.
 *
 * Changes: construction is only possible through a reducing smart constructor (no public apply/copy), partial
 * operations return ArithmeticError instead of throwing, and decimal conversion bounds the scale.
 */
package xyz.kd5ujc.numerics

import cats.syntax.either._
import cats.{Order, Show}

/** Exact rational `numerator / denominator`, always gcd-reduced with `denominator > 0`. */
sealed abstract case class Ratio(numerator: BigInt, denominator: BigInt) {

  def isInteger: Boolean = denominator == BigInt(1)

  def signum: Int = numerator.signum

  def toBigIntExact: Option[BigInt] = Option.when(isInteger)(numerator)

  def unary_- : Ratio = Ratio.reduced(-numerator, denominator)

  def abs: Ratio = Ratio.reduced(numerator.abs, denominator)

  def +(that: Ratio): Ratio =
    Ratio.reduced(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)

  def -(that: Ratio): Ratio = this + -that

  def *(that: Ratio): Ratio = Ratio.reduced(numerator * that.numerator, denominator * that.denominator)

  def /(that: Ratio): Either[ArithmeticError, Ratio] = that.inverse.map(this * _)

  def inverse: Either[ArithmeticError, Ratio] = Ratio.of(denominator, numerator)

  /** Integer power; negative exponents invert, so `0` to a negative power is a division by zero. */
  def pow(exponent: Int): Either[ArithmeticError, Ratio] =
    if (exponent >= 0) Ratio.reduced(numerator.pow(exponent), denominator.pow(exponent)).asRight
    else inverse.map(r => Ratio.reduced(r.numerator.pow(-exponent), r.denominator.pow(-exponent)))

  def compare(that: Ratio): Int = (numerator * that.denominator).compare(that.numerator * denominator)

  def <(that:   Ratio): Boolean = compare(that) < 0
  def <=(that:  Ratio): Boolean = compare(that) <= 0
  def >(that:   Ratio): Boolean = compare(that) > 0
  def >=(that:  Ratio): Boolean = compare(that) >= 0
  def min(that: Ratio): Ratio = if (this <= that) this else that
  def max(that: Ratio): Ratio = if (this >= that) this else that

  /** Round toward zero. */
  def truncate: BigInt = numerator / denominator

  /** Largest integer <= this. */
  def floor: BigInt = if (numerator.signum < 0 && numerator % denominator != BigInt(0)) truncate - 1 else truncate

  /** Smallest integer >= this. */
  def ceil: BigInt = if (numerator.signum > 0 && numerator % denominator != BigInt(0)) truncate + 1 else truncate

  /** Round half away from zero. */
  def roundHalfUp: BigInt = (numerator.abs * 2 + denominator) / (denominator * 2) * numerator.signum

  /** Round half to even (banker's rounding): ties go to the even neighbour, so rounding has no bias. */
  def roundHalfEven: BigInt = {
    val lo = floor
    val twiceFraction = (numerator - lo * denominator) * 2
    if (twiceFraction < denominator) lo
    else if (twiceFraction > denominator) lo + 1
    else if (lo % 2 == BigInt(0)) lo
    else lo + 1
  }

  /** Truncated remainder `a - b * truncate(a / b)`, the sign follows the dividend. */
  def rem(that: Ratio): Either[ArithmeticError, Ratio] = (this / that).map(q => this - that * Ratio.integer(q.truncate))

  /**
   * Exact decimal rendering when the value terminates (the reduced denominator has no prime factors other than 2 and
   * 5); `None` otherwise.
   */
  def toDecimalString: Option[String] =
    Ratio.decimalScale(denominator).map { scale =>
      val scaled = numerator * (BigInt(10).pow(scale) / denominator)
      BigDecimal(scaled, scale).bigDecimal.toPlainString
    }

  override def toString: String = if (isInteger) numerator.toString else s"$numerator/$denominator"
}

object Ratio {

  val Zero: Ratio = integer(0)
  val One: Ratio = integer(1)

  /** Largest accepted `|scale|` when converting a decimal, bounding the `10^scale` the conversion materialises. */
  val DefaultMaxDecimalScale: Int = 10000

  def integer(n: BigInt): Ratio = new Ratio(n, BigInt(1)) {}

  def of(numerator: BigInt, denominator: BigInt): Either[ArithmeticError, Ratio] =
    Either.cond(denominator.signum != 0, reduced(numerator, denominator), ArithmeticError.DivisionByZero)

  /** `unscaled * 10^-scale`, exactly; rejects `|scale| > maxScale`. */
  def fromDecimal(unscaled: BigInt, scale: Int, maxScale: Int = DefaultMaxDecimalScale): Either[ArithmeticError, Ratio] =
    if (math.abs(scale.toLong) > maxScale.toLong) ArithmeticError.ScaleOutOfRange(scale, maxScale).asLeft
    else if (scale >= 0) reduced(unscaled, BigInt(10).pow(scale)).asRight
    else integer(unscaled * BigInt(10).pow(-scale)).asRight

  def fromBigDecimal(value: BigDecimal, maxScale: Int = DefaultMaxDecimalScale): Either[ArithmeticError, Ratio] =
    fromDecimal(BigInt(value.bigDecimal.unscaledValue), value.bigDecimal.scale, maxScale)

  /** Callers guarantee `denominator != 0`. */
  private[numerics] def reduced(numerator: BigInt, denominator: BigInt): Ratio = {
    val g = numerator.gcd(denominator)
    val sign = if (denominator.signum < 0) BigInt(-1) else BigInt(1)
    new Ratio(sign * numerator / g, sign * denominator / g) {}
  }

  /** Number of decimal places needed to write `1/denominator` exactly, if finite. */
  private def decimalScale(denominator: BigInt): Option[Int] = {
    val twos = denominator.lowestSetBit
    val rest = denominator >> twos
    val (fives, remainder) = stripFives(rest, 0)
    Option.when(remainder == BigInt(1))(math.max(twos, fives))
  }

  @scala.annotation.tailrec
  private def stripFives(n: BigInt, count: Int): (Int, BigInt) =
    if (n % 5 == BigInt(0)) stripFives(n / 5, count + 1) else (count, n)

  implicit val ratioOrder: Order[Ratio] = Order.from(_ `compare` _)
  implicit val ratioShow: Show[Ratio] = Show.fromToString
  implicit val ratioOrdering: Ordering[Ratio] = ratioOrder.toOrdering
}

sealed trait ArithmeticError extends Product with Serializable

object ArithmeticError {
  case object DivisionByZero extends ArithmeticError
  final case class ScaleOutOfRange(scale: Int, maxScale: Int) extends ArithmeticError
}
