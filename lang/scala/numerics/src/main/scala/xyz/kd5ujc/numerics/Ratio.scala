/*
 * Adapted from Topl Bifrost (https://github.com/Topl/Bifrost) `co.topl.models.utility.Ratio`,
 * by way of Constellation tessellation and metakit. MPL-2.0.
 *
 * Changes: construction is only possible through a reducing smart constructor (no public apply/copy), partial
 * operations return ArithmeticError instead of throwing, and decimal conversion bounds the scale.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not
 * distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/. The rest of dilf4s is Apache-2.0
 * (NOTICE).
 */
package xyz.kd5ujc.numerics

import cats.syntax.either._
import cats.{Order, Show}

/** Exact rational `numerator / denominator`, always gcd-reduced with `denominator > 0`. */
sealed abstract case class Ratio(numerator: BigInt, denominator: BigInt) {

  def isInteger: Boolean = denominator == BigInt(1)

  def signum: Int = numerator.signum

  def toBigIntExact: Option[BigInt] = Option.when(isInteger)(numerator)

  // negation and absolute value keep a reduced fraction reduced: no gcd
  def unary_- : Ratio = new Ratio(-numerator, denominator) {}

  def abs: Ratio = if (numerator.signum >= 0) this else new Ratio(-numerator, denominator) {}

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
    if (isInteger) Option(numerator.toString)
    else
      Ratio.twosAndFives(denominator)._1.map {
        case (twos, fives) =>
          // 10^scale / (2^twos 5^fives), as a shift and a power rather than a division
          val scale = math.max(twos, fives)
          val scaled = (numerator * Ratio.Five.pow(scale - fives)) << (scale - twos)
          BigDecimal(scaled, scale).bigDecimal.toPlainString
      }

  /**
   * The scale of the terminating decimal, `max(a, b)` for a reduced denominator `2^a 5^b` (0 for an integer); `None`
   * when it does not terminate. Found from the denominator's factors alone, so a caller can refuse a scale past its bound
   * before [[toDecimalString]] builds any digit.
   */
  def decimalScale: Option[Int] =
    if (isInteger) Option(0) else Ratio.twosAndFives(denominator)._1.map { case (twos, fives) => math.max(twos, fives) }

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
    else if (scale >= 0) decimal(unscaled, scale).asRight
    else integer(unscaled * BigInt(10).pow(-scale)).asRight

  /**
   * `unscaled / 10^scale` in lowest terms. `10^scale`'s only primes are 2 and 5, so the common factor is
   * `2^min(v2, scale) 5^min(v5, scale)`: the twos go in one shift, and the fives by trial division by `5^(2^i)` from the
   * largest `2^i <= scale` down, which builds `min(v5, scale)` bit by bit (about 14 divisions for a scale of 10000).
   * The same result as [[reduced]] against `10^scale`, without its general gcd.
   */
  private def decimal(unscaled: BigInt, scale: Int): Ratio =
    if (unscaled.signum == 0) Zero
    else {
      val twos = math.min(unscaled.lowestSetBit, scale)
      val powers = Iterator.iterate((1, Five)) { case (e, p) => (e * 2, p * p) }.takeWhile(_._1 <= scale).toList
      val (rest, fives) = powers.reverse.foldLeft((unscaled >> twos, 0)) {
        case ((n, count), (e, _)) if count + e > scale => (n, count)
        case ((n, count), (e, p)) =>
          val (q, r) = n /% p
          if (r.signum == 0) (q, count + e) else (n, count)
      }
      val denominator = Five.pow(scale - fives) << (scale - twos)
      if (denominator == BigInt(1)) integer(rest) else new Ratio(rest, denominator) {}
    }

  def fromBigDecimal(value: BigDecimal, maxScale: Int = DefaultMaxDecimalScale): Either[ArithmeticError, Ratio] =
    fromDecimal(BigInt(value.bigDecimal.unscaledValue), value.bigDecimal.scale, maxScale)

  /** Callers guarantee `denominator != 0`. */
  private[numerics] def reduced(numerator: BigInt, denominator: BigInt): Ratio = {
    val g = numerator.gcd(denominator)
    val sign = if (denominator.signum < 0) BigInt(-1) else BigInt(1)
    new Ratio(sign * numerator / g, sign * denominator / g) {}
  }

  private val Five: BigInt = BigInt(5)

  /** log2(5): the bits per factor of five. */
  private val BitsPerFive: Double = math.log(5.0) / math.log(2.0)

  /**
   * `(a, b)` when `denominator = 2^a 5^b`, so that `1/denominator` terminates, and the big-number divisions and
   * exponentiations it took: at most four, whatever the denominator. The twos are the low zero bits; the power of five
   * is found from the bit length and checked by exponentiation, instead of one long division per factor (14,306 for
   * `5^14306`, the widest a canonical rational's denominator is).
   */
  private[numerics] def twosAndFives(denominator: BigInt): (Option[(Int, Int)], Int) = {
    val twos = denominator.lowestSetBit
    val rest = denominator >> twos
    if (rest == BigInt(1)) (Option((twos, 0)), 0)
    else if (rest % Five != BigInt(0)) (Option.empty, 1)
    else {
      // 5^b has floor(b log2 5) + 1 bits: b log2 5 lies in [bits - 1, bits), under half a factor wide, so b is the
      // estimate's ceiling but for rounding, which the neighbours absorb
      val estimate = ((rest.bitLength - 1) / BitsPerFive).toInt
      List(estimate + 1, estimate, estimate - 1).foldLeft((Option.empty[(Int, Int)], 1)) {
        case (done @ (scala.Some(_), _), _) => done
        case ((_, steps), b) if b > 0       => (Option.when(Five.pow(b) == rest)((twos, b)), steps + 1)
        case (skip, _)                      => skip
      }
    }
  }

  implicit val ratioOrder: Order[Ratio] = Order.from(_ `compare` _)
  implicit val ratioShow: Show[Ratio] = Show.fromToString
  implicit val ratioOrdering: Ordering[Ratio] = ratioOrder.toOrdering
}

sealed trait ArithmeticError extends Product with Serializable

object ArithmeticError {
  case object DivisionByZero extends ArithmeticError
  final case class ScaleOutOfRange(scale: Int, maxScale: Int) extends ArithmeticError

  /** A decimal with more significant digits than its reader accepts (counted on its text, before conversion). */
  final case class DigitsOutOfRange(digits: Int, maxDigits: Int) extends ArithmeticError
}
