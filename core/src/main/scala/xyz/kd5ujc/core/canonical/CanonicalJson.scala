package xyz.kd5ujc.core.canonical

import cats.syntax.either._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

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

  // ------------------------------------------------------------------ parse

  /**
   * JSON text as a canonical value. `maxScale` may lower the scale bound, never raise it: the spec accepts no JSON number
   * of a scale past [[MaxScale]] (docs/specs/canonical-encoding.md §3), so a bound outside `0..MaxScale` is a
   * `TypeMismatch`, before anything is read, as in the Rust and TypeScript SDKs.
   */
  def parse(text: String, maxScale: Int = MaxScale): Either[CanonicalError, CanonicalValue] =
    scaleBound(maxScale).flatMap(parse(text, _, KeyOrder.Default))

  /** `maxScale`, if it is a bound [[parse]] accepts: from 0 to [[MaxScale]]. */
  private[kd5ujc] def scaleBound(maxScale: Int): Either[CanonicalError, Int] =
    Either.cond(maxScale >= 0 && maxScale <= MaxScale, maxScale, TypeMismatch(s"the scale bound is not an integer from 0 to $MaxScale"))

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
          // a container opened inside MaxDepth open ones is refused at its bracket, an empty one too
          // (canonical-encoding.md section 3): the first fault in reading order
          case Some('{' | '[') if depth >= MaxDepth => DepthExceeded(MaxDepth).asLeft
          case Some('{') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains('}')) run(s, maxScale, keys, Deliver(EmptyMap, q + 1, stack, depth))
            else run(s, maxScale, keys, ExpectKey(q, Vector.empty, CanonicalKeys.emptyIn(keys), stack, depth + 1))
          case Some('[') =>
            val q = skipWs(s, p + 1)
            if (charAt(s, q).contains(']')) run(s, maxScale, keys, Deliver(EmptyArray, q + 1, stack, depth))
            else run(s, maxScale, keys, ExpectValue(q, InArray(Vector.empty) :: stack, depth + 1))
          case Some(_) =>
            scalar(s, p, maxScale) match {
              case Right((v, next)) => run(s, maxScale, keys, Deliver(v, next, stack, depth))
              case Left(e)          => e.asLeft
            }
          case None => syntax(p, "expectedValue").asLeft
        }

      case ExpectKey(p0, acc, seen, stack, depth) =>
        val p = skipWs(s, p0)
        if (!charAt(s, p).contains('"')) syntax(p, "expectedMemberName").asLeft
        else
          string(s, p) match {
            case Left(e)                               => e.asLeft
            case Right((key, _)) if seen.contains(key) => DuplicateKey(Preview.of(key)).asLeft
            case Right((key, next)) =>
              val colon = skipWs(s, next)
              if (!charAt(s, colon).contains(':')) syntax(colon, "expectedColon").asLeft
              else run(s, maxScale, keys, ExpectValue(colon + 1, InObject(acc, seen.added(key), key) :: stack, depth))
          }

      case Deliver(v, p0, Nil, _) =>
        val p = skipWs(s, p0)
        Either.cond(p == s.length, v, syntax(p, "trailingContent"))

      case Deliver(v, p0, InArray(acc) :: rest, depth) =>
        val p = skipWs(s, p0)
        charAt(s, p) match {
          case Some(',') => run(s, maxScale, keys, ExpectValue(p + 1, InArray(acc :+ v) :: rest, depth))
          case Some(']') =>
            CanonicalValue.array(acc :+ v) match {
              case Right(arr) => run(s, maxScale, keys, Deliver(arr, p + 1, rest, depth - 1))
              case Left(e)    => e.asLeft
            }
          case _ => syntax(p, "expectedCommaOrBracket").asLeft
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
          case _ => syntax(p, "expectedCommaOrBrace").asLeft
        }
    }

  private def scalar(s: String, p: Int, maxScale: Int): Either[CanonicalError, (CanonicalValue, Int)] =
    charAt(s, p) match {
      case Some('"')                             => string(s, p).flatMap { case (v, next) => text(v).map(t => (t, next)) }
      case Some('t') if s.startsWith("true", p)  => (True, p + 4).asRight
      case Some('f') if s.startsWith("false", p) => (False, p + 5).asRight
      case Some('n') if s.startsWith("null", p)  => (CNull, p + 4).asRight
      case Some(c) if c == '-' || isDigit(c)     => parseNumber(s, p, maxScale)
      case _                                     => syntax(p, "expectedValue").asLeft
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
    def described(bound: Option[NumberBound]): CanonicalError = InvalidNumber(Some((end - p).toLong), Some(p.toLong), bound)
    val intDigits = s.substring(intStart, intEnd)
    val fracDigits = if (hasFrac) s.substring(intEnd + 1, fracEnd) else ""
    val expDigits = if (hasExp) s.substring(expSignEnd, end) else "0"
    val malformed =
      intDigits.isEmpty || (intDigits.length > 1 && intDigits.startsWith("0")) ||
      (hasFrac && fracDigits.isEmpty) || (hasExp && expDigits.isEmpty)
    // the integer digits have no leading zeros but a lone 0, after which the fraction's leading zeros are not counted
    val significand = if (intDigits == "0") fracDigits.dropWhile(_ == '0').length else intDigits.length + fracDigits.length
    val exponentDigits = expDigits.dropWhile(_ == '0').length
    def tooLong(digits: Int): CanonicalError = described(Some(NumberBound.Digits(digits.toLong, MaxDigits.toLong)))
    if (malformed) described(None).asLeft
    else if (significand > MaxDigits) tooLong(significand).asLeft
    else if (exponentDigits > MaxDigits) tooLong(exponentDigits).asLeft
    else {
      // an exponent of more than 18 digits is past every scale a text this long can reach: saturate rather than convert
      val magnitude = if (exponentDigits > 18) Long.MaxValue else expDigits.toLong
      val exponent = if (hasExp && charAt(s, fracEnd + 1).contains('-')) -BigInt(magnitude) else BigInt(magnitude)
      val scale = BigInt(fracDigits.length) - exponent
      if (scale.abs > BigInt(maxScale)) {
        val saturated = scale.max(BigInt(Int.MinValue)).min(BigInt(Int.MaxValue)).toInt
        described(Some(NumberBound.Scale(saturated, maxScale.toLong))).asLeft
      } else {
        val unscaled = BigInt(intDigits + fracDigits) * (if (negative) -1 else 1)
        // the digit and scale bounds keep the work bounded, not the value: an integer of 10,000 digits can be wider than
        // CanonicalValue.MaxIntegerBits (9e9999, 10,000 nines), and so can a rational's parts (0.000...1 at scale
        // 10,000, whose denominator is 10^10000), and number refuses them
        Ratio
          .fromDecimal(unscaled, scale.toInt, maxScale)
          .leftMap(e => described(NumberBounds.of(e)))
          .flatMap(r => number(r).map((_, end)))
      }
    }
  }

  /** Parses a JSON string starting at the opening quote; escapes may produce unpaired surrogates, rejected by the caller. */
  private def string(s: String, open: Int): Either[CanonicalError, (String, Int)] = {
    @tailrec
    def go(i: Int, segment: Int, acc: Vector[String]): Either[CanonicalError, (String, Int)] =
      charAt(s, i) match {
        case None               => syntax(i, "unterminatedString").asLeft
        case Some('"')          => ((acc :+ s.substring(segment, i)).mkString, i + 1).asRight
        case Some(c) if c < ' ' => syntax(i, "controlCharacter").asLeft
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
            case _ => syntax(i, "invalidEscape").asLeft
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
  private def syntax(offset: Int, reason: JsonSyntaxReason): CanonicalError = JsonSyntax(offset.toLong, reason)

  private val nonTerminating: CanonicalError = NotJsonRepresentable("non-terminating rational")

  private val overLong: CanonicalError = NotJsonRepresentable("over-long number")

  /**
   * An integer's JSON text, which [[parse]] reads back: total, since every integer is within
   * [[CanonicalValue.MaxIntegerBits]], so it has at most [[MaxDigits]] digits and converting it is bounded work.
   */
  def integerText(i: CInt): String = i.value.toString

  /**
   * A rational's JSON text, within the bounds [[parse]] accepts, so that rendering always reads back. Its scale comes
   * from the denominator's factors first, so a scale past the bound builds no digit.
   */
  private[kd5ujc] def rationalText(r: CRational): Either[CanonicalError, String] =
    r.value.decimalScale match {
      case scala.None                    => nonTerminating.asLeft
      case scala.Some(s) if s > MaxScale => overLong.asLeft
      case scala.Some(_)                 => r.value.toDecimalString.toRight(nonTerminating).flatMap(within)
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

  /**
   * The longest JSON [[render]] writes, in UTF-8 bytes: 2^29 - 24, the TypeScript SDK's ceiling too. Each UTF-16 unit
   * of the text is at least one UTF-8 byte; the builder may hold twice the text's units (it doubles as it grows) at two
   * bytes each once a unit past Latin-1 arrives, so it never asks for more than about 2^31 - 92 bytes, within the JVM's
   * largest array: rendering never throws for want of an array that size.
   */
  val MaxRenderBytes: Long = (1L << 29) - 24

  /** Why a value was not rendered within a limit: JSON cannot represent it, or its JSON is longer than the limit. */
  sealed trait RenderError extends Product with Serializable

  object RenderError {

    /** Bytes, a rational with no terminating decimal, or a number past the bounds [[parse]] accepts. */
    final case class Canonical(error: CanonicalError) extends RenderError

    /** The JSON is longer than `limit` UTF-8 bytes (docs/specs/conformance.md §4: the limit alone, nothing measured). */
    final case class TooLarge(limit: Long) extends RenderError
  }

  /**
   * Compact JSON with keys in code-point order: [[renderWithin]] at [[MaxRenderBytes]]. Fails for bytes, for rationals
   * that do not terminate or are too long (`Canonical`), and for a value whose JSON is longer than [[MaxRenderBytes]]
   * (`TooLarge`, with that limit), which no JVM string could be relied on to hold (docs/specs/conformance.md §4).
   */
  def render(value: CanonicalValue): Either[RenderError, String] = renderWithin(value, MaxRenderBytes)

  /**
   * [[render]], if the JSON is at most `maxBytes` long in UTF-8 (a negative limit is 0, and no limit is above
   * [[MaxRenderBytes]]). Fails with `TooLarge` at the first part found past the limit, or for what JSON cannot
   * represent, whichever comes first, as the Rust and TypeScript SDKs do.
   *
   * O(min(weight, `maxBytes`)) apart from sorting map keys, in one pass: a value's JSON is at least as long in UTF-8 as
   * its weight (each node writes at least a byte, a text or key its UTF-8 and quotes, a number of `l` whole limbs more
   * than `l` digits), so each part is refused in O(1) when its weight is over what is left, and each character of the
   * text is written once, into one builder. The builder is local to this call and never escapes it: each of its states
   * is used once ([[TextSink]]) and only the finished text leaves, so the function is pure.
   */
  def renderWithin(value: CanonicalValue, maxBytes: Long): Either[RenderError, String] =
    renderInto(value, maxBytes, new java.lang.StringBuilder)(using TextSink.builder).map(_.toString)

  /**
   * [[renderWithin]], but a byte string is written as the text `"0x"` and its lowercase hex (the JLVM's JSON surface,
   * docs/specs/jlvm.md), checked against the limit before its hex is built.
   */
  private[kd5ujc] def renderSurfaceWithin(value: CanonicalValue, maxBytes: Long): Either[RenderError, String] =
    write(List(Pending.Node(value)), Written(new java.lang.StringBuilder, 0L, budget(maxBytes)), hexBytes = true)(using TextSink.builder)
      .map(_.toString)

  /** A caller's limit as it is applied: from 0 to [[MaxRenderBytes]]. */
  private def budget(maxBytes: Long): Long = maxBytes.max(0L).min(MaxRenderBytes)

  /**
   * Where rendered text goes, as a state `S` threaded through every write, as [[ByteSink]] is for bytes: the renderer
   * uses each state once, passing on the one a write returns, so a sink may append in place.
   */
  private[canonical] trait TextSink[S] {

    /** `s` with the characters of `text` from `from` until `until` appended. */
    def chars(s: S, text: String, from: Int, until: Int): S
  }

  private[canonical] object TextSink {
    val builder: TextSink[java.lang.StringBuilder] = (s, text, from, until) => s.append(text, from, until)
  }

  /** [[renderWithin]] into any [[TextSink]]: each character of the rendering reaches the sink once, and nothing else does. */
  private[canonical] def renderInto[S](value: CanonicalValue, maxBytes: Long, s: S)(using TextSink[S]): Either[RenderError, S] =
    write(List(Pending.Node(value)), Written(s, 0L, budget(maxBytes)), hexBytes = false)

  /** What is still to be written, innermost first: a value, or the rest of an open array or map. */
  sealed private trait Pending

  private object Pending {
    final case class Node(value: CanonicalValue) extends Pending
    final case class Items(items: Vector[CanonicalValue], next: Int) extends Pending
    final case class Members(members: Vector[(String, CanonicalValue)], next: Int) extends Pending
  }

  /** The sink's state, the UTF-8 bytes written to it, and the limit. */
  final private case class Written[S](s: S, used: Long, max: Long) {
    def left: Long = max - used
    def refused: RenderError = RenderError.TooLarge(max)

    /** `piece`, ASCII, if it fits. */
    def ascii(piece: String)(using sink: TextSink[S]): Either[RenderError, Written[S]] =
      Either.cond(piece.length <= left, Written(sink.chars(s, piece, 0, piece.length), used + piece.length, max), refused)

    /** `t` quoted and escaped, if it fits: its length is counted before any of it is written. */
    def quoted(t: String)(using TextSink[S]): Either[RenderError, Written[S]] = {
      val length = quotedLength(t)
      Either.cond(length <= left, Written(escaped(t), used + length, max), refused)
    }

    // runs of characters that need no escape are written straight from the text
    private def escaped(t: String)(using sink: TextSink[S]): S = {
      def unescaped(acc: S, from: Int, until: Int): S = if (from < until) sink.chars(acc, t, from, until) else acc
      @tailrec
      def go(acc: S, run: Int, i: Int): S =
        if (i >= t.length) sink.chars(unescaped(acc, run, i), "\"", 0, 1)
        else
          escape(t.charAt(i)) match {
            case Some(e) => go(sink.chars(unescaped(acc, run, i), e, 0, e.length), i + 1, i + 1)
            case None    => go(acc, run, i + 1)
          }
      go(sink.chars(s, "\"", 0, 1), 0, 0)
    }
  }

  @tailrec
  private def write[S](todo: List[Pending], out: Written[S], hexBytes: Boolean)(using TextSink[S]): Either[RenderError, S] = {
    val step: Either[RenderError, (List[Pending], Written[S])] =
      todo match {
        case Nil                     => (Nil, out).asRight
        case Pending.Node(v) :: rest => node(v, out, hexBytes).map { case (opened, next) => (opened.fold(rest)(_ :: rest), next) }
        case Pending.Items(items, i) :: rest =>
          if (i >= items.size) out.ascii("]").map((rest, _))
          else separated(out, i).map((Pending.Node(items(i)) :: Pending.Items(items, i + 1) :: rest, _))
        case Pending.Members(members, i) :: rest =>
          if (i >= members.size) out.ascii("}").map((rest, _))
          else {
            val (key, v) = members(i)
            separated(out, i)
              .flatMap(_.quoted(key))
              .flatMap(_.ascii(":"))
              .map((Pending.Node(v) :: Pending.Members(members, i + 1) :: rest, _))
          }
      }
    step match {
      case Right((Nil, done))     => done.s.asRight
      case Right((next, written)) => write(next, written, hexBytes)
      case Left(e)                => e.asLeft
    }
  }

  // a comma before every part but the first
  private def separated[S](out: Written[S], i: Int)(using TextSink[S]): Either[RenderError, Written[S]] =
    if (i == 0) out.asRight else out.ascii(",")

  /** One value: a scalar, written whole, or a container's opening bracket, its parts now pending. */
  private def node[S](v: CanonicalValue, out: Written[S], hexBytes: Boolean)(
    using TextSink[S]
  ): Either[RenderError, (Option[Pending], Written[S])] =
    if (v.weight > out.left) out.refused.asLeft
    else
      v match {
        case CNull        => out.ascii("null").map((None, _))
        case CBool(b)     => out.ascii(b.toString).map((None, _))
        case i: CInt      => out.ascii(integerText(i)).map((None, _))
        case r: CRational => rationalText(r).leftMap(RenderError.Canonical(_)).flatMap(out.ascii).map((None, _))
        case t: CText     => out.quoted(t.value).map((None, _))
        // "0x" and two hex digits a byte, quoted: its length is known before the hex is built
        case CBytes(b) if hexBytes =>
          if (out.left < 4L || b.size > (out.left - 4L) / 2) out.refused.asLeft else out.ascii("\"0x" + b.toHex + "\"").map((None, _))
        case CBytes(_)  => RenderError.Canonical(NotJsonRepresentable("bytes")).asLeft
        case CArray(vs) => out.ascii("[").map((Some(Pending.Items(vs, 0)), _))
        case CMap(m)    => out.ascii("{").map((Some(Pending.Members(m.toVector.sortBy(_._1)(using CodePointOrder), 0)), _))
      }

  private def escape(c: Char): Option[String] =
    c match {
      case '"'          => Some("\\\"")
      case '\\'         => Some("\\\\")
      case '\n'         => Some("\\n")
      case '\r'         => Some("\\r")
      case '\t'         => Some("\\t")
      case '\b'         => Some("\\b")
      case '\f'         => Some("\\f")
      case c if c < ' ' => Some(f"\\u${c.toInt}%04x")
      case _            => None
    }

  /** The UTF-8 length of `t` quoted and escaped; a surrogate is half of a 4-byte pair, as text has no lone surrogates. */
  private def quotedLength(t: String): Long =
    t.foldLeft(2L) { (n, c) =>
      n + escape(c).fold(if (c < 0x80) 1L else if (c < 0x800 || c.isSurrogate) 2L else 3L)(_.length.toLong)
    }

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
