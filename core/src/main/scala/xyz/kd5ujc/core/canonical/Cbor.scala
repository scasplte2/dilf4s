package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8

import cats.Eval
import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/** The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md §2). */
object Cbor {

  val DefaultMaxDepth: Int = CanonicalValue.MaxDepth

  private val TwoTo64: BigInt = BigInt(1) << 64
  private val TagPositiveBignum: Int = 2
  private val TagNegativeBignum: Int = 3
  private val TagRational: Int = 30

  // ------------------------------------------------------------------ encoding

  /** Total: every canonical value has exactly one encoding. Stack-safe for any nesting depth. */
  def encode(value: CanonicalValue): ByteVector = encodeEval(value).value

  private def encodeEval(value: CanonicalValue): Eval[ByteVector] =
    value match {
      case CNull        => Eval.now(ByteVector(0xf6))
      case CBool(false) => Eval.now(ByteVector(0xf4))
      case CBool(true)  => Eval.now(ByteVector(0xf5))
      case CInt(n)      => Eval.now(integer(n))
      case r: CRational =>
        Eval.now(head(6, TagRational.toLong) ++ head(4, 2L) ++ integer(r.value.numerator) ++ integer(r.value.denominator))
      case t: CText   => Eval.now(text(t.value))
      case CBytes(b)  => Eval.now(head(2, b.size) ++ b)
      case CArray(vs) => Eval.defer(vs.traverse(encodeEval)).map(parts => parts.foldLeft(head(4, vs.size.toLong))(_ ++ _))
      case m: CMap =>
        Eval
          .defer(m.entries.toVector.traverse { case (k, v) => encodeEval(v).map(ev => (text(k), ev)) })
          .map { entries =>
            entries.sortWith { case ((ka, _), (kb, _)) => compareBytes(ka, kb) < 0 }
              .foldLeft(head(5, entries.size.toLong)) { case (acc, (k, v)) => acc ++ k ++ v }
          }
    }

  private def text(s: String): ByteVector = {
    val utf8 = ByteVector(s.getBytes(UTF_8))
    head(3, utf8.size) ++ utf8
  }

  private def integer(n: BigInt): ByteVector =
    if (n.signum >= 0) { if (n < TwoTo64) head(0, n) else bignum(TagPositiveBignum, n) }
    else {
      val m = -1 - n
      if (m < TwoTo64) head(1, m) else bignum(TagNegativeBignum, m)
    }

  private def bignum(tag: Int, magnitude: BigInt): ByteVector = {
    val bytes = ByteVector(magnitude.toByteArray).dropWhile(_ == 0)
    head(6, tag.toLong) ++ head(2, bytes.size) ++ bytes
  }

  private def head(major: Int, argument: Long): ByteVector = head(major, BigInt(argument))

  /** Shortest-form initial byte plus argument; callers guarantee `0 <= argument < 2^64`. */
  private def head(major: Int, argument: BigInt): ByteVector = {
    val mt = major << 5
    def fixed(len: Int): ByteVector = ByteVector(argument.toByteArray).takeRight(len.toLong).padLeft(len.toLong)
    if (argument < 24) ByteVector(mt | argument.toInt)
    else if (argument < 0x100) ByteVector(mt | 24, argument.toInt)
    else if (argument < 0x10000) ByteVector(mt | 25) ++ fixed(2)
    else if (argument < (BigInt(1) << 32)) ByteVector(mt | 26) ++ fixed(4)
    else ByteVector(mt | 27) ++ fixed(8)
  }

  /** Unsigned bytewise lexicographic order (RFC 8949 §4.2.1). */
  def compareBytes(a: ByteVector, b: ByteVector): Int = {
    @tailrec
    def go(i: Long): Int =
      if (i >= a.size || i >= b.size) a.size.compare(b.size)
      else {
        val (x, y) = (a(i) & 0xff, b(i) & 0xff)
        if (x != y) x.compare(y) else go(i + 1)
      }
    go(0)
  }

  // ------------------------------------------------------------------ decoding

  /**
   * Strict decoder: accepts exactly the canonical encodings, so `decode(b).map(encode) == Right(b)` whenever it
   * succeeds. An explicit frame stack bounds nesting by `maxDepth` instead of the JVM call stack.
   */
  def decode(bytes: ByteVector, maxDepth: Int = DefaultMaxDepth): Either[CanonicalError, CanonicalValue] =
    run(bytes, math.min(maxDepth, CanonicalValue.MaxDepth), Read(0L, Nil, 0))

  sealed private trait Frame
  final private case class ArrayFrame(remaining: Long, acc: Vector[CanonicalValue]) extends Frame
  final private case class MapFrame(
    remaining:  Long,
    acc:        Vector[(String, CanonicalValue)],
    lastKey:    Option[ByteVector],
    pendingKey: Option[(String, ByteVector)]
  ) extends Frame

  sealed private trait Step
  final private case class Read(pos: Long, stack: List[Frame], depth: Int) extends Step
  final private case class Deliver(value: CanonicalValue, start: Long, end: Long, stack: List[Frame], depth: Int) extends Step

  sealed private trait Item
  final private case class Scalar(value: CanonicalValue, end: Long) extends Item
  final private case class OpenArray(count: Long, end: Long) extends Item
  final private case class OpenMap(count: Long, end: Long) extends Item

  final private case class Head(major: Int, argument: BigInt, end: Long)

  @tailrec
  private def run(in: ByteVector, maxDepth: Int, step: Step): Either[CanonicalError, CanonicalValue] =
    step match {
      case Read(pos, stack, depth) =>
        readItem(in, pos) match {
          case Left(e)                       => e.asLeft
          case Right(Scalar(v, end))         => run(in, maxDepth, Deliver(v, pos, end, stack, depth))
          case Right(OpenArray(0L, end))     => run(in, maxDepth, Deliver(EmptyArray, pos, end, stack, depth))
          case Right(OpenMap(0L, end))       => run(in, maxDepth, Deliver(EmptyMap, pos, end, stack, depth))
          case Right(_) if depth >= maxDepth => DepthExceeded(maxDepth).asLeft
          case Right(OpenArray(n, end))      => run(in, maxDepth, Read(end, ArrayFrame(n, Vector.empty) :: stack, depth + 1))
          case Right(OpenMap(n, end))        => run(in, maxDepth, Read(end, MapFrame(n, Vector.empty, None, None) :: stack, depth + 1))
        }

      case Deliver(v, _, end, Nil, _) =>
        Either.cond(end == in.size, v, TrailingBytes(end))

      case Deliver(v, _, end, ArrayFrame(remaining, acc) :: rest, depth) =>
        if (remaining == 1) run(in, maxDepth, Deliver(CanonicalValue.decodedArray(acc :+ v), -1, end, rest, depth - 1))
        else run(in, maxDepth, Read(end, ArrayFrame(remaining - 1, acc :+ v) :: rest, depth))

      case Deliver(key: CText, start, end, MapFrame(remaining, acc, lastKey, None) :: rest, depth) =>
        val keyBytes = in.slice(start, end)
        if (lastKey.exists(compareBytes(_, keyBytes) >= 0))
          NonCanonical(start, "map keys must be unique and in ascending encoded order").asLeft
        else run(in, maxDepth, Read(end, MapFrame(remaining, acc, lastKey, Some((key.value, keyBytes))) :: rest, depth))

      case Deliver(_, start, _, MapFrame(_, _, _, None) :: _, _) =>
        Unsupported(start, "map keys must be text").asLeft

      case Deliver(CNull, start, _, MapFrame(_, _, _, Some(_)) :: _, _) =>
        NonCanonical(start, "map values must not be null (null and absent are the same value)").asLeft

      case Deliver(v, _, end, MapFrame(remaining, acc, _, Some((key, keyBytes))) :: rest, depth) =>
        if (remaining == 1) run(in, maxDepth, Deliver(CanonicalValue.decodedMap(acc :+ (key -> v)), -1, end, rest, depth - 1))
        else run(in, maxDepth, Read(end, MapFrame(remaining - 1, acc :+ (key -> v), Some(keyBytes), None) :: rest, depth))
    }

  private def readItem(in: ByteVector, pos: Long): Either[CanonicalError, Item] =
    readHead(in, pos).flatMap {
      case Head(0, n, end) => Scalar(CInt(n), end).asRight
      case Head(1, n, end) => Scalar(CInt(-1 - n), end).asRight
      case Head(2, n, end) => readSpan(in, end, n).map { case (b, e) => Scalar(CBytes(b), e) }
      case Head(3, n, end) => readText(in, end, n).map { case (t, e) => Scalar(t, e) }
      case Head(4, n, end) => count(in, end, n, 1).map(OpenArray(_, end))
      case Head(5, n, end) => count(in, end, n, 2).map(OpenMap(_, end))
      case Head(6, t, end) => readTagged(in, pos, t, end).map { case (v, e) => Scalar(v, e) }
      case Head(_, n, end) =>
        n.toInt match {
          case 20 => Scalar(False, end).asRight
          case 21 => Scalar(True, end).asRight
          case 22 => Scalar(CNull, end).asRight
          case s  => Unsupported(pos, s"simple value $s").asLeft
        }
    }

  private def readHead(in: ByteVector, pos: Long): Either[CanonicalError, Head] =
    byteAt(in, pos).flatMap { initial =>
      val (major, info) = (initial >>> 5, initial & 0x1f)
      def sized(len: Int, minimum: BigInt): Either[CanonicalError, Head] =
        readSpan(in, pos + 1, BigInt(len)).flatMap {
          case (b, end) =>
            val n = BigInt(1, b.toArray)
            Either.cond(n >= minimum, Head(major, n, end), NonCanonical(pos, s"argument $n not in shortest form"))
        }
      if (info < 24) Head(major, BigInt(info), pos + 1).asRight
      else if (major == 7) Unsupported(pos, s"floating point or simple value (additional information $info)").asLeft
      else
        info match {
          case 24 => sized(1, 24)
          case 25 => sized(2, 0x100)
          case 26 => sized(4, 0x10000)
          case 27 => sized(8, BigInt(1) << 32)
          case 31 => Unsupported(pos, "indefinite length").asLeft
          case _  => Unsupported(pos, s"reserved additional information $info").asLeft
        }
    }

  private def readTagged(in: ByteVector, pos: Long, tag: BigInt, end: Long): Either[CanonicalError, (CanonicalValue, Long)] =
    tag.toInt match {
      case TagPositiveBignum | TagNegativeBignum if tag.isValidInt =>
        readBignum(in, pos, tag.toInt, end).map { case (n, e) => (CInt(n), e) }
      case TagRational if tag.isValidInt => readRational(in, pos, end)
      case _                             => Unsupported(pos, s"tag $tag").asLeft
    }

  private def readBignum(in: ByteVector, pos: Long, tag: Int, end: Long): Either[CanonicalError, (BigInt, Long)] =
    readHead(in, end).flatMap {
      case Head(2, len, bodyStart) =>
        readSpan(in, bodyStart, len).flatMap {
          case (b, e) =>
            if (b.headOption.forall(_ == 0)) NonCanonical(pos, "bignum magnitude must be non-empty without leading zeros").asLeft
            else if (b.size <= 8) NonCanonical(pos, "bignum used for a value that fits major type 0/1").asLeft
            else {
              val magnitude = BigInt(1, b.toArray)
              (if (tag == TagPositiveBignum) magnitude else -1 - magnitude, e).asRight
            }
        }
      case _ => NonCanonical(pos, "bignum tag must wrap a byte string").asLeft
    }

  private def readInteger(in: ByteVector, pos: Long): Either[CanonicalError, (BigInt, Long)] =
    readHead(in, pos).flatMap {
      case Head(0, n, end)                                                     => (n, end).asRight
      case Head(1, n, end)                                                     => (-1 - n, end).asRight
      case Head(6, t, end) if t == TagPositiveBignum || t == TagNegativeBignum => readBignum(in, pos, t.toInt, end)
      case _                                                                   => NonCanonical(pos, "expected an integer").asLeft
    }

  private def readRational(in: ByteVector, pos: Long, end: Long): Either[CanonicalError, (CanonicalValue, Long)] =
    for {
      pair <- readHead(in, end)
      _    <- Either.cond(pair.major == 4 && pair.argument == BigInt(2), (), NonCanonical(pos, "rational must wrap a 2-element array"))
      num  <- readInteger(in, pair.end)
      den  <- readInteger(in, num._2)
      _    <- Either.cond(den._1 >= 2, (), NonCanonical(pos, "rational denominator must be >= 2"))
      _    <- Either.cond(num._1.gcd(den._1) == BigInt(1), (), NonCanonical(pos, "rational must be reduced"))
      r    <- Ratio.of(num._1, den._1).leftMap(_ => NonCanonical(pos, "rational denominator must be non-zero"))
    } yield (CanonicalValue.number(r), den._2)

  private def readText(in: ByteVector, pos: Long, len: BigInt): Either[CanonicalError, (CText, Long)] =
    readSpan(in, pos, len).flatMap {
      case (b, end) =>
        val raw = b.toArray
        if (!Utf8.isWellFormed(raw)) InvalidUtf8(pos).asLeft
        else CanonicalValue.text(new String(raw, UTF_8)).map(t => (t, end))
    }

  private def readSpan(in: ByteVector, pos: Long, len: BigInt): Either[CanonicalError, (ByteVector, Long)] =
    Either.cond(len <= BigInt(in.size - pos), (in.slice(pos, pos + len.toLong), pos + len.toLong), UnexpectedEnd(pos))

  /** Every element needs at least one byte, so a count beyond the remaining input is truncated, not allocated. */
  private def count(in: ByteVector, pos: Long, n: BigInt, perEntry: Int): Either[CanonicalError, Long] =
    Either.cond(n * perEntry <= BigInt(in.size - pos), n.toLong, UnexpectedEnd(pos))

  private def byteAt(in: ByteVector, pos: Long): Either[CanonicalError, Int] =
    Either.cond(pos >= 0 && pos < in.size, in(pos) & 0xff, UnexpectedEnd(pos))
}

/** RFC 3629 well-formed UTF-8: shortest form, no surrogates, at most U+10FFFF. */
private[canonical] object Utf8 {

  def isWellFormed(a: Array[Byte]): Boolean = check(a, 0)

  private def at(a:     Array[Byte], i: Int): Int = if (i < a.length) a(i) & 0xff else -1
  private def cont(a:   Array[Byte], i: Int): Boolean = (at(a, i) & 0xc0) == 0x80 && at(a, i) >= 0
  private def within(a: Array[Byte], i: Int, lo: Int, hi: Int): Boolean = at(a, i) >= lo && at(a, i) <= hi

  @tailrec
  private def check(a: Array[Byte], i: Int): Boolean =
    if (i >= a.length) true
    else {
      val b = at(a, i)
      val width =
        if (b < 0x80) 1
        else if (b >= 0xc2 && b <= 0xdf && cont(a, i + 1)) 2
        else if (b == 0xe0 && within(a, i + 1, 0xa0, 0xbf) && cont(a, i + 2)) 3
        else if (((b >= 0xe1 && b <= 0xec) || b == 0xee || b == 0xef) && cont(a, i + 1) && cont(a, i + 2)) 3
        else if (b == 0xed && within(a, i + 1, 0x80, 0x9f) && cont(a, i + 2)) 3
        else if (b == 0xf0 && within(a, i + 1, 0x90, 0xbf) && cont(a, i + 2) && cont(a, i + 3)) 4
        else if (b >= 0xf1 && b <= 0xf3 && cont(a, i + 1) && cont(a, i + 2) && cont(a, i + 3)) 4
        else if (b == 0xf4 && within(a, i + 1, 0x80, 0x8f) && cont(a, i + 2) && cont(a, i + 3)) 4
        else 0
      width > 0 && check(a, i + width)
    }
}
