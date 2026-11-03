package xyz.kd5ujc.core.canonical

import cats.Eval
import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.{ArithmeticError, Ratio}

/**
 * The JSON mapping (docs/specs/canonical-encoding.md §3). [[parse]] is a strict, stack-safe RFC 8259 parser that reads
 * numbers exactly from their source text and rejects duplicate member names; [[render]] is a display view and fails
 * for values JSON cannot represent. Core has no circe dependency: typed data with circe codecs crosses into the model
 * through the `circeInterop` module (`xyz.kd5ujc.core.circe.CanonicalCirce`), which uses this object's public API.
 */
object CanonicalJson {

  /**
   * The most significant digits a JSON number's significand (integer and fraction digits, leading zeros aside) or
   * exponent may have, read or written. Decimal conversion is superlinear, so the bound is checked on the text before
   * any conversion; it equals the largest scale, so every decimal of an accepted scale has a form within it.
   */
  val MaxDigits: Int = xyz.kd5ujc.models.limits.CanonicalMaxDigits

  /** The largest accepted `|fraction digits - exponent|` of a JSON number, bounding the `10^scale` it materialises. */
  val MaxScale: Int = xyz.kd5ujc.models.limits.CanonicalMaxScale

  // 10^MaxDigits has 33220 bits: a number any wider has more than MaxDigits digits
  private val MaxDigitBits: Int = xyz.kd5ujc.models.limits.CanonicalMaxDigitBits

  // ------------------------------------------------------------------ parse

  def parse(text: String, maxScale: Int = MaxScale): Either[CanonicalError, CanonicalValue] =
    parse(text, maxScale, KeyOrder.Default)

  /** [[parse]] building maps, and checking for repeated member names, in `keys`'s order: production passes [[KeyOrder.Default]]. */
  private[canonical] def parse(text: String, maxScale: Int, keys: KeyOrder): Either[CanonicalError, CanonicalValue] =
    run(text, maxScale, keys, ExpectValue(0, Nil, 0))

  sealed private trait Frame
  final private case class InArray(acc: Vector[CanonicalValue]) extends Frame
  // the members in input order, and their names ordered (never hashed) to find a repeat as it is read
  final private case class InObject(acc: Vector[(String, CanonicalValue)], seen: CanonicalKeys, key: String) extends Frame

  sealed private trait Step
  final private case class ExpectValue(pos: Int, stack: List[Frame], depth: Int) extends Step
  final private case class ExpectKey(pos: Int, acc: Vector[(String, CanonicalValue)], seen: CanonicalKeys, stack: List[Frame], depth: Int)
      extends Step
  final private case class Deliver(value: CanonicalValue, pos: Int, stack: List[Frame], depth: Int) extends Step

  @tailrec
  private def run(s: String, maxScale: Int, keys: KeyOrder, step: Step): Either[CanonicalError, CanonicalValue] =
    step match {
      case ExpectValue(p0, stack, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some('{') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains('}')) run(s, maxScale, keys, Deliver(EmptyMap, q + 1, stack, depth))
            else if (depth >= MaxDepth) DepthExceeded(MaxDepth).asLeft
            else run(s, maxScale, keys, ExpectKey(q, Vector.empty, CanonicalKeys.emptyIn(keys), stack, depth + 1))
          case Some('[') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains(']')) run(s, maxScale, keys, Deliver(EmptyArray, q + 1, stack, depth))
            else if (depth >= MaxDepth) DepthExceeded(MaxDepth).asLeft
            else run(s, maxScale, keys, ExpectValue(q, InArray(Vector.empty) :: stack, depth + 1))
          case Some(_) =>
            scalar(s, p, maxScale) match {
              case Right((v, next)) => run(s, maxScale, keys, Deliver(v, next, stack, depth))
              case Left(e)          => e.asLeft
            }
          case None => syntax(p, "expected a value").asLeft
        }

      case ExpectKey(p0, acc, seen, stack, depth) =>
        val p = skipWs(s, p0)
        if (!charAt(s, p).contains('"')) syntax(p, "expected a member name").asLeft
        else
          string(s, p) match {
            case Left(e)                               => e.asLeft
            case Right((key, _)) if seen.contains(key) => DuplicateKey(Preview.of(key)).asLeft
            case Right((key, next)) =>
              val colon = skipWs(s, next)
              if (!charAt(s, colon).contains(':')) syntax(colon, "expected ':'").asLeft
              else run(s, maxScale, keys, ExpectValue(colon + 1, InObject(acc, seen.added(key), key) :: stack, depth))
          }

      case Deliver(v, p0, Nil, _) =>
        val p = skipWs(s, p0)
        Either.cond(p == s.length, v, syntax(p, "unexpected trailing content"))

      case Deliver(v, p0, InArray(acc) :: rest, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some(',') => run(s, maxScale, keys, ExpectValue(p + 1, InArray(acc :+ v) :: rest, depth))
          case Some(']') =>
            CanonicalValue.array(acc :+ v) match {
              case Right(arr) => run(s, maxScale, keys, Deliver(arr, p + 1, rest, depth - 1))
              case Left(e)    => e.asLeft
            }
          case _ => syntax(p, "expected ',' or ']'").asLeft
        }

      case Deliver(v, p0, InObject(acc, seen, key) :: rest, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some(',') => run(s, maxScale, keys, ExpectKey(p + 1, acc :+ (key -> v), seen, rest, depth))
          case Some('}') =>
            CanonicalValue.map(acc :+ (key -> v), keys) match {
              case Right(m) => run(s, maxScale, keys, Deliver(m, p + 1, rest, depth - 1))
              case Left(e)  => e.asLeft
            }
          case _ => syntax(p, "expected ',' or '}'").asLeft
        }
    }

  private def scalar(s: String, p: Int, maxScale: Int): Either[CanonicalError, (CanonicalValue, Int)] =
    charAt(s, p) match {
      case Some('"')                             => string(s, p).flatMap { case (v, next) => text(v).map(t => (t, next)) }
      case Some('t') if s.startsWith("true", p)  => (True, p + 4).asRight
      case Some('f') if s.startsWith("false", p) => (False, p + 5).asRight
      case Some('n') if s.startsWith("null", p)  => (CNull, p + 4).asRight
      case Some(c) if c == '-' || isDigit(c)     => parseNumber(s, p, maxScale)
      case _                                     => syntax(p, "expected a value").asLeft
    }

  /**
   * `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`, converted exactly. Its digits are counted, then its scale
   * bounded, before any conversion. A refusal describes the number by its length and offset, never its text, whose
   * size the caller chose.
   */
  private def parseNumber(s: String, p: Int, maxScale: Int): Either[CanonicalError, (CanonicalValue, Int)] = {
    val negative = charAt(s, p).contains('-')
    val intStart = if (negative) p + 1 else p
    val intEnd = digits(s, intStart)
    val hasFrac = charAt(s, intEnd).contains('.')
    val fracEnd = if (hasFrac) digits(s, intEnd + 1) else intEnd
    val hasExp = charAt(s, fracEnd).exists(c => c == 'e' || c == 'E')
    val expSignEnd = if (hasExp && charAt(s, fracEnd + 1).exists(c => c == '+' || c == '-')) fracEnd + 2 else fracEnd + 1
    val end = if (hasExp) digits(s, expSignEnd) else fracEnd
    val described = NumberSource.Text(end - p, p)
    val intDigits = s.substring(intStart, intEnd)
    val fracDigits = if (hasFrac) s.substring(intEnd + 1, fracEnd) else ""
    val expDigits = if (hasExp) s.substring(expSignEnd, end) else "0"
    val malformed =
      intDigits.isEmpty || (intDigits.length > 1 && intDigits.startsWith("0")) ||
      (hasFrac && fracDigits.isEmpty) || (hasExp && expDigits.isEmpty)
    // the integer digits have no leading zeros but a lone 0, after which the fraction's leading zeros are not counted
    val significand = if (intDigits == "0") fracDigits.dropWhile(_ == '0').length else intDigits.length + fracDigits.length
    val exponentDigits = expDigits.dropWhile(_ == '0').length
    def tooLong(digits: Int): CanonicalError = InvalidNumber(described, Some(ArithmeticError.DigitsOutOfRange(digits, MaxDigits)))
    if (malformed) InvalidNumber(described, None).asLeft
    else if (significand > MaxDigits) tooLong(significand).asLeft
    else if (exponentDigits > MaxDigits) tooLong(exponentDigits).asLeft
    else {
      // an exponent of more than 18 digits is past every scale a text this long can reach: saturate rather than convert
      val magnitude = if (exponentDigits > 18) Long.MaxValue else expDigits.toLong
      val exponent = if (hasExp && charAt(s, fracEnd + 1).contains('-')) -BigInt(magnitude) else BigInt(magnitude)
      val scale = BigInt(fracDigits.length) - exponent
      if (scale.abs > BigInt(maxScale)) {
        val saturated = scale.max(BigInt(Int.MinValue)).min(BigInt(Int.MaxValue)).toInt
        InvalidNumber(described, Some(ArithmeticError.ScaleOutOfRange(saturated, maxScale))).asLeft
      } else {
        val unscaled = BigInt(intDigits + fracDigits) * (if (negative) -1 else 1)
        // the digit and scale bounds keep both parts within CanonicalValue.MaxRationalBits, so number never refuses here
        Ratio
          .fromDecimal(unscaled, scale.toInt, maxScale)
          .leftMap(e => InvalidNumber(described, Some(e)))
          .flatMap(r => number(r).map((_, end)))
      }
    }
  }

  /** Parses a JSON string starting at the opening quote; escapes may produce unpaired surrogates, rejected by the caller. */
  private def string(s: String, open: Int): Either[CanonicalError, (String, Int)] = {
    @tailrec
    def go(i: Int, segment: Int, acc: Vector[String]): Either[CanonicalError, (String, Int)] =
      charAt(s, i) match {
        case None               => syntax(i, "unterminated string").asLeft
        case Some('"')          => ((acc :+ s.substring(segment, i)).mkString, i + 1).asRight
        case Some(c) if c < ' ' => syntax(i, "unescaped control character in string").asLeft
        case Some('\\') =>
          val prefix = acc :+ s.substring(segment, i)
          charAt(s, i + 1) match {
            case Some('"')  => go(i + 2, i + 2, prefix :+ "\"")
            case Some('\\') => go(i + 2, i + 2, prefix :+ "\\")
            case Some('/')  => go(i + 2, i + 2, prefix :+ "/")
            case Some('b')  => go(i + 2, i + 2, prefix :+ "\b")
            case Some('f')  => go(i + 2, i + 2, prefix :+ "\f")
            case Some('n')  => go(i + 2, i + 2, prefix :+ "\n")
            case Some('r')  => go(i + 2, i + 2, prefix :+ "\r")
            case Some('t')  => go(i + 2, i + 2, prefix :+ "\t")
            case Some('u') if i + 6 <= s.length && s.substring(i + 2, i + 6).forall(isHex) =>
              go(i + 6, i + 6, prefix :+ Integer.parseInt(s.substring(i + 2, i + 6), 16).toChar.toString)
            case _ => syntax(i, "invalid escape").asLeft
          }
        case Some(_) => go(i + 1, segment, acc)
      }
    go(open + 1, open + 1, Vector.empty)
  }

  @tailrec
  private def skipWs(s: String, i: Int): Int =
    charAt(s, i) match {
      case Some(' ') | Some('\t') | Some('\n') | Some('\r') => skipWs(s, i + 1)
      case _                                                => i
    }

  @tailrec
  private def digits(s: String, i: Int): Int = if (charAt(s, i).exists(isDigit)) digits(s, i + 1) else i

  private def charAt(s:      String, i:   Int): Option[Char] = Option.when(i >= 0 && i < s.length)(s.charAt(i))
  private def isDigit(c:     Char): Boolean = c >= '0' && c <= '9'
  private def isHex(c:       Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  private def syntax(offset: Int, reason: String): CanonicalError = JsonSyntax(offset, reason)

  // its size, never its digits: a decoded rational can be far larger than the cost of rendering it allows
  private def nonTerminating(r: CRational): CanonicalError =
    NotJsonRepresentable(
      s"non-terminating rational of ${r.value.numerator.bitLength}/${r.value.denominator.bitLength} bits"
    )

  private val overLong: CanonicalError =
    NotJsonRepresentable(s"a number of more than $MaxDigits digits or a scale above $MaxScale")

  /**
   * A number's JSON text, within the bounds [[parse]] accepts, so that rendering always reads back. A wider numerator
   * or denominator is refused before any conversion: `2^a 5^b` with scale `max(a, b)` within bounds is at most
   * `10^MaxDigits`, and the significand is at least the numerator.
   */
  private def numberText(v: CanonicalValue): Either[CanonicalError, String] =
    v match {
      case CInt(n) if n.bitLength <= MaxDigitBits => within(n.toString)
      case r: CRational if r.value.numerator.bitLength <= MaxDigitBits && r.value.denominator.bitLength <= MaxDigitBits =>
        r.value.toDecimalString.toRight(nonTerminating(r)).flatMap(within)
      case _ => overLong.asLeft
    }

  // the digits of `text`, a plain decimal, leading zeros aside, and the digits after its point
  private def within(text: String): Either[CanonicalError, String] = {
    val unsigned = text.stripPrefix("-")
    val point = unsigned.indexOf('.')
    val scale = if (point < 0) 0 else unsigned.length - point - 1
    val significand = unsigned.filter(_ != '.').dropWhile(_ == '0').length
    Either.cond(significand <= MaxDigits && scale <= MaxScale, text, overLong)
  }

  // ------------------------------------------------------------------ render (display view)

  /** Compact JSON with keys in code-point order. Fails for bytes, non-terminating rationals and over-long numbers. */
  def render(value: CanonicalValue): Either[CanonicalError, String] = renderEval(value).value

  private def renderEval(value: CanonicalValue): Eval[Either[CanonicalError, String]] =
    value match {
      case CNull        => Eval.now("null".asRight)
      case CBool(b)     => Eval.now(b.toString.asRight)
      case CInt(_)      => Eval.now(numberText(value))
      case r: CRational => Eval.now(numberText(r))
      case t: CText     => Eval.now(quote(t.value).asRight)
      case CBytes(_)    => Eval.now(NotJsonRepresentable("bytes").asLeft)
      case CArray(vs)   => Eval.defer(vs.traverse(renderEval)).map(_.sequence.map(_.mkString("[", ",", "]")))
      case CMap(m) =>
        Eval
          .defer(m.toVector.sortBy(_._1)(using CodePointOrder).traverse { case (k, v) => renderEval(v).map(_.map(quote(k) + ":" + _)) })
          .map(_.sequence.map(_.mkString("{", ",", "}")))
    }

  private def quote(s: String): String =
    s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case '\b'         => "\\b"
      case '\f'         => "\\f"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    }.prepended('"').appended('"')

  private[canonical] object CodePointOrder extends Ordering[String] {
    def compare(a: String, b: String): Int = {
      @tailrec
      def go(i: Int, j: Int): Int =
        if (i >= a.length || j >= b.length) (a.length - i).compare(b.length - j)
        else {
          val (x, y) = (a.codePointAt(i), b.codePointAt(j))
          if (x != y) x.compare(y) else go(i + Character.charCount(x), j + Character.charCount(y))
        }
      go(0, 0)
    }
  }
}
