package xyz.kd5ujc.core.canonical

import cats.Eval
import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import io.circe.{Json, JsonNumber}

/**
 * The JSON mapping (docs/specs/canonical-encoding.md §3). [[parse]] is a strict, stack-safe RFC 8259 parser that reads
 * numbers exactly from their source text and rejects duplicate member names; [[fromCirce]] bridges typed data that
 * already has a circe `Encoder`; [[render]] is a display view and fails for values JSON cannot represent.
 */
object CanonicalJson {

  // ------------------------------------------------------------------ parse

  def parse(text: String, maxScale: Int = Ratio.DefaultMaxDecimalScale): Either[CanonicalError, CanonicalValue] =
    run(text, maxScale, ExpectValue(0, Nil, 0))

  sealed private trait Frame
  final private case class InArray(acc: Vector[CanonicalValue]) extends Frame
  final private case class InObject(acc: Vector[(String, CanonicalValue)], seen: Set[String], key: String) extends Frame

  sealed private trait Step
  final private case class ExpectValue(pos: Int, stack: List[Frame], depth: Int) extends Step
  final private case class ExpectKey(pos: Int, acc: Vector[(String, CanonicalValue)], seen: Set[String], stack: List[Frame], depth: Int) extends Step
  final private case class Deliver(value: CanonicalValue, pos: Int, stack: List[Frame], depth: Int) extends Step

  @tailrec
  private def run(s: String, maxScale: Int, step: Step): Either[CanonicalError, CanonicalValue] =
    step match {
      case ExpectValue(p0, stack, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some('{') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains('}')) run(s, maxScale, Deliver(EmptyMap, q + 1, stack, depth))
            else if (depth >= MaxDepth) DepthExceeded(MaxDepth).asLeft
            else run(s, maxScale, ExpectKey(q, Vector.empty, Set.empty, stack, depth + 1))
          case Some('[') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains(']')) run(s, maxScale, Deliver(EmptyArray, q + 1, stack, depth))
            else if (depth >= MaxDepth) DepthExceeded(MaxDepth).asLeft
            else run(s, maxScale, ExpectValue(q, InArray(Vector.empty) :: stack, depth + 1))
          case Some(_) =>
            scalar(s, p, maxScale) match {
              case Right((v, next)) => run(s, maxScale, Deliver(v, next, stack, depth))
              case Left(e)          => e.asLeft
            }
          case None => syntax(p, "expected a value").asLeft
        }

      case ExpectKey(p0, acc, seen, stack, depth) =>
        val p = skipWs(s, p0)
        if (!charAt(s, p).contains('"')) syntax(p, "expected a member name").asLeft
        else
          string(s, p) match {
            case Left(e) => e.asLeft
            case Right((key, _)) if seen.contains(key) => DuplicateKey(key).asLeft
            case Right((key, next)) =>
              val colon = skipWs(s, next)
              if (!charAt(s, colon).contains(':')) syntax(colon, "expected ':'").asLeft
              else run(s, maxScale, ExpectValue(colon + 1, InObject(acc, seen + key, key) :: stack, depth))
          }

      case Deliver(v, p0, Nil, _) =>
        val p = skipWs(s, p0)
        Either.cond(p == s.length, v, syntax(p, "unexpected trailing content"))

      case Deliver(v, p0, InArray(acc) :: rest, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some(',') => run(s, maxScale, ExpectValue(p + 1, InArray(acc :+ v) :: rest, depth))
          case Some(']') =>
            CanonicalValue.array(acc :+ v) match {
              case Right(arr) => run(s, maxScale, Deliver(arr, p + 1, rest, depth - 1))
              case Left(e)    => e.asLeft
            }
          case _ => syntax(p, "expected ',' or ']'").asLeft
        }

      case Deliver(v, p0, InObject(acc, seen, key) :: rest, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some(',') => run(s, maxScale, ExpectKey(p + 1, acc :+ (key -> v), seen, rest, depth))
          case Some('}') =>
            CanonicalValue.map(acc :+ (key -> v)) match {
              case Right(m) => run(s, maxScale, Deliver(m, p + 1, rest, depth - 1))
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

  /** `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`, converted exactly; the scale is bounded before `10^scale`. */
  private def parseNumber(s: String, p: Int, maxScale: Int): Either[CanonicalError, (CanonicalValue, Int)] = {
    val negative = charAt(s, p).contains('-')
    val intStart = if (negative) p + 1 else p
    val intEnd = digits(s, intStart)
    val hasFrac = charAt(s, intEnd).contains('.')
    val fracEnd = if (hasFrac) digits(s, intEnd + 1) else intEnd
    val hasExp = charAt(s, fracEnd).exists(c => c == 'e' || c == 'E')
    val expSignEnd = if (hasExp && charAt(s, fracEnd + 1).exists(c => c == '+' || c == '-')) fracEnd + 2 else fracEnd + 1
    val end = if (hasExp) digits(s, expSignEnd) else fracEnd
    val source = s.substring(p, end)
    val intDigits = s.substring(intStart, intEnd)
    val fracDigits = if (hasFrac) s.substring(intEnd + 1, fracEnd) else ""
    val expDigits = if (hasExp) s.substring(expSignEnd, end) else "0"
    val malformed =
      intDigits.isEmpty || (intDigits.length > 1 && intDigits.startsWith("0")) ||
        (hasFrac && fracDigits.isEmpty) || (hasExp && expDigits.isEmpty)
    if (malformed) InvalidNumber(source, None).asLeft
    else {
      val exponent = BigInt(expDigits) * (if (hasExp && charAt(s, fracEnd + 1).contains('-')) -1 else 1)
      val scale = BigInt(fracDigits.length) - exponent
      val unscaled = BigInt(intDigits + fracDigits) * (if (negative) -1 else 1)
      if (scale.abs > BigInt(maxScale)) InvalidNumber(source, Some(xyz.kd5ujc.numerics.ArithmeticError.ScaleOutOfRange(scale.toInt, maxScale))).asLeft
      else Ratio.fromDecimal(unscaled, scale.toInt, maxScale).bimap(e => InvalidNumber(source, Some(e)), r => (number(r), end))
    }
  }

  /** Parses a JSON string starting at the opening quote; escapes may produce unpaired surrogates, rejected by the caller. */
  private def string(s: String, open: Int): Either[CanonicalError, (String, Int)] = {
    @tailrec
    def go(i: Int, segment: Int, acc: Vector[String]): Either[CanonicalError, (String, Int)] =
      charAt(s, i) match {
        case None                => syntax(i, "unterminated string").asLeft
        case Some('"')           => ((acc :+ s.substring(segment, i)).mkString, i + 1).asRight
        case Some(c) if c < ' '  => syntax(i, "unescaped control character in string").asLeft
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
      case _                                                 => i
    }

  @tailrec
  private def digits(s: String, i: Int): Int = if (charAt(s, i).exists(isDigit)) digits(s, i + 1) else i

  private def charAt(s: String, i: Int): Option[Char] = Option.when(i >= 0 && i < s.length)(s.charAt(i))
  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  private def isHex(c: Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  private def syntax(offset: Int, reason: String): CanonicalError = JsonSyntax(offset, reason)

  // ------------------------------------------------------------------ circe bridge

  /** Converts circe JSON built from typed data; stack-safe. Numbers use their exact decimal value. */
  def fromCirce(json: Json, maxScale: Int = Ratio.DefaultMaxDecimalScale): Either[CanonicalError, CanonicalValue] =
    circe(json, maxScale).value

  private def circe(json: Json, maxScale: Int): Eval[Either[CanonicalError, CanonicalValue]] =
    json.fold(
      jsonNull = Eval.now(CNull.asRight),
      jsonBoolean = b => Eval.now(CBool(b).asRight),
      jsonNumber = n => Eval.now(circeNumber(n, maxScale)),
      jsonString = s => Eval.now(text(s)),
      jsonArray = vs =>
        Eval.defer(vs.traverse(circe(_, maxScale))).map(_.sequence.flatMap(CanonicalValue.array)),
      jsonObject = o =>
        Eval
          .defer(o.toVector.traverse { case (k, v) => circe(v, maxScale).map(_.map(k -> _)) })
          .map(_.sequence.flatMap(CanonicalValue.map))
    )

  private def circeNumber(n: JsonNumber, maxScale: Int): Either[CanonicalError, CanonicalValue] =
    n.toBigDecimal
      .toRight(InvalidNumber(n.toString, None))
      .flatMap(bd => Ratio.fromBigDecimal(bd, maxScale).bimap(e => InvalidNumber(n.toString, Some(e)), number))

  /** The inverse bridge, for decoding into typed data. Numbers stay exact; fails where [[render]] fails. */
  def toCirce(value: CanonicalValue): Either[CanonicalError, Json] = toCirceEval(value).value

  private def toCirceEval(value: CanonicalValue): Eval[Either[CanonicalError, Json]] =
    value match {
      case CNull        => Eval.now(Json.Null.asRight)
      case CBool(b)     => Eval.now(Json.fromBoolean(b).asRight)
      case CInt(n)      => Eval.now(Json.fromBigInt(n).asRight)
      case r: CRational =>
        Eval.now(
          r.value.toDecimalString
            .map(d => Json.fromBigDecimal(BigDecimal(d)))
            .toRight(NotJsonRepresentable(s"non-terminating rational ${r.value}"))
        )
      case t: CText   => Eval.now(Json.fromString(t.value).asRight)
      case CBytes(_)  => Eval.now(NotJsonRepresentable("bytes").asLeft)
      case CArray(vs) => Eval.defer(vs.traverse(toCirceEval)).map(_.sequence.map(Json.fromValues))
      case CMap(m) =>
        Eval
          .defer(m.toVector.traverse { case (k, v) => toCirceEval(v).map(_.map(k -> _)) })
          .map(_.sequence.map(Json.fromFields))
    }

  // ------------------------------------------------------------------ render (display view)

  /** Compact JSON with keys in code-point order. Fails for bytes and non-terminating rationals. */
  def render(value: CanonicalValue): Either[CanonicalError, String] = renderEval(value).value

  private def renderEval(value: CanonicalValue): Eval[Either[CanonicalError, String]] =
    value match {
      case CNull        => Eval.now("null".asRight)
      case CBool(b)     => Eval.now(b.toString.asRight)
      case CInt(n)      => Eval.now(n.toString.asRight)
      case r: CRational => Eval.now(r.value.toDecimalString.toRight(NotJsonRepresentable(s"non-terminating rational ${r.value}")))
      case t: CText     => Eval.now(quote(t.value).asRight)
      case CBytes(_)    => Eval.now(NotJsonRepresentable("bytes").asLeft)
      case CArray(vs)   => Eval.defer(vs.traverse(renderEval)).map(_.sequence.map(_.mkString("[", ",", "]")))
      case CMap(m) =>
        Eval
          .defer(m.toVector.sortBy(_._1)(CodePointOrder).traverse { case (k, v) => renderEval(v).map(_.map(quote(k) + ":" + _)) })
          .map(_.sequence.map(_.mkString("{", ",", "}")))
    }

  private def quote(s: String): String =
    s.flatMap {
      case '"'           => "\\\""
      case '\\'          => "\\\\"
      case '\n'          => "\\n"
      case '\r'          => "\\r"
      case '\t'          => "\\t"
      case '\b'          => "\\b"
      case '\f'          => "\\f"
      case c if c < ' '  => f"\\u${c.toInt}%04x"
      case c             => c.toString
    }.prepended('"').appended('"')

  private object CodePointOrder extends Ordering[String] {
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
