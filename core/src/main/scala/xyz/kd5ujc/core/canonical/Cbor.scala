package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.either._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/** The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md §2). */
object Cbor {

  val DefaultMaxDepth: Int = CanonicalValue.MaxDepth

  /**
   * `n/d` (`d >= 2`, both within [[CanonicalValue.MaxRationalBits]]) if it is in lowest terms: one gcd. A test passes
   * one that counts its calls.
   */
  private[kd5ujc] type LowestTerms = (BigInt, BigInt) => Option[Ratio]

  /** [[LowestTerms]] with one gcd: reducing `n/d` leaves its denominator unchanged exactly when `gcd(n, d) = 1`. */
  private[kd5ujc] val lowestTerms: LowestTerms = (n, d) => Ratio.of(n, d).toOption.filter(_.denominator == d)

  /**
   * How a decoder builds numbers: the one gcd of a rational ([[LowestTerms]]), and a bignum's magnitude from its bytes,
   * built only once its form and width are checked. Production passes [[Numbers.Default]]; a test passes ones that
   * count their calls.
   */
  final private[kd5ujc] case class Numbers(lowestTerms: LowestTerms, magnitude: ByteVector => BigInt)

  private[kd5ujc] object Numbers {
    val Default: Numbers = Numbers(Cbor.lowestTerms, b => BigInt(1, b.toArray))
  }

  private val TwoTo64: BigInt = BigInt(1) << 64
  private val TagPositiveBignum: Int = 2
  private val TagNegativeBignum: Int = 3
  private val TagRational: Int = 30

  // ------------------------------------------------------------------ encoding

  /**
   * Total: every canonical value has exactly one encoding. Stack-safe for any nesting depth. The result is flat: one
   * array ([[FlatBytes]]), so reading or slicing it never walks a rope.
   */
  def encode(value: CanonicalValue): FlatBytes = FlatBytes(write(value, ByteSink.emptyBuffer))

  /**
   * The encoding of `value`, not yet built: written into a buffer it is [[encode]]; into a hash, it is hashed as it is
   * written, and never held whole (`Hasher.digestOf`).
   */
  def source(value: CanonicalValue): ByteSource =
    ByteSource.of(new ByteSource.Piece {
      def writeTo[S](s: S)(implicit sink: ByteSink[S]): S = write(value, s)
    })

  /**
   * The one encoder: an explicit stack of the containers still open (one per level of nesting) instead of the JVM's,
   * writing each node's head before its children. What it writes itself is at most a head (9 bytes) at a time; a byte
   * string goes to the sink as it is, a text or map key in UTF-8 windows of at most [[TextWindow]] characters, and a
   * bignum's magnitude whole (a copy of that one integer, which the value holds whole). A map's entries are sorted into
   * encoded-key order when it is opened, from each key's UTF-8 length and its code points ([[keyBefore]]), without
   * encoding any key: the walk holds a reference and a length per entry of each map open on the current path.
   */
  private def write[S](value: CanonicalValue, s: S)(implicit out: ByteSink[S]): S = walk(List(Node(value)), s)

  sealed private trait Open
  final private case class Node(value: CanonicalValue) extends Open
  final private case class Items(items: Vector[CanonicalValue], next: Int) extends Open
  final private case class Entries(entries: Vector[(String, Long, CanonicalValue)], next: Int) extends Open

  @tailrec
  private def walk[S](stack: List[Open], s: S)(implicit out: ByteSink[S]): S =
    stack match {
      case Nil                      => s
      case Node(CArray(vs)) :: rest => walk(Items(vs, 0) :: rest, head(s, 4, vs.size.toLong))
      case Node(m: CMap) :: rest =>
        val sorted = m.entries.toVector.map { case (k, v) => (k, Utf16.utf8Length(k), v) }.sortWith(keyBefore)
        walk(Entries(sorted, 0) :: rest, head(s, 5, sorted.size.toLong))
      case Node(scalar: CanonicalValue.Scalar) :: rest     => walk(rest, writeScalar(s, scalar))
      case Items(items, next) :: rest if next < items.size => walk(Node(items(next)) :: Items(items, next + 1) :: rest, s)
      case Entries(entries, next) :: rest if next < entries.size =>
        val (key, length, v) = entries(next)
        walk(Node(v) :: Entries(entries, next + 1) :: rest, utf8(head(s, 3, length), key, 0))
      case Items(_, _) :: rest   => walk(rest, s) // the array is done
      case Entries(_, _) :: rest => walk(rest, s) // the map is done
    }

  private def writeScalar[S](s: S, v: CanonicalValue.Scalar)(implicit out: ByteSink[S]): S =
    v match {
      case CBool(false) => out.byte(s, 0xf4)
      case CBool(true)  => out.byte(s, 0xf5)
      case CInt(n)      => integer(s, n)
      case r: CRational =>
        integer(integer(head(head(s, 6, TagRational.toLong), 4, 2L), r.value.numerator), r.value.denominator)
      case t: CText  => utf8(head(s, 3, t.weight - 1), t.value, 0) // a text weighs 1 plus its UTF-8 length
      case CBytes(b) => out.bytes(head(s, 2, b.size), b)
      case CNull     => out.byte(s, 0xf6)
    }

  /** The longest run of a text's characters converted to UTF-8 at once: a text's own bytes are never copied whole. */
  private[kd5ujc] val TextWindow: Int = 4096

  /** Where the window from `from` ends in a text of `length` characters: never past it, and no `Int` overflow near 2^31. */
  private[canonical] def windowEnd(length: Int, from: Int): Int = from + math.min(TextWindow, length - from)

  /** `t`'s UTF-8 from character `from`, a window at a time; a window never ends between the halves of a surrogate pair. */
  @tailrec
  private def utf8[S](s: S, t: String, from: Int)(implicit out: ByteSink[S]): S =
    if (from >= t.length) s
    else {
      val end = windowEnd(t.length, from)
      val cut = if (end < t.length && Character.isHighSurrogate(t.charAt(end - 1))) end - 1 else end
      utf8(out.bytes(s, FlatBytes.view(t.substring(from, cut).getBytes(UTF_8))), t, cut)
    }

  /**
   * `encode(value).size`, without building the encoding: one stack-safe pass over the value, saturating at
   * `Long.MaxValue`. Its cost is the value's [[CanonicalValue.weight]], which counts shared sub-values every time, so
   * bound the weight first. The length is at least the weight: every node has a head, text and byte strings add their
   * length, and an integer of `b` bits takes at least `1 + b/64` bytes.
   */
  def encodedLength(value: CanonicalValue): Long = {
    @tailrec
    def go(todo: List[CanonicalValue], acc: Long): Long =
      todo match {
        case Nil                => acc
        case CArray(vs) :: rest => go(vs.toList ::: rest, Weight.add(acc, headLength(vs.size.toLong)))
        case (m: CMap) :: rest =>
          val keys = m.entries.keysIterator.foldLeft(headLength(m.entries.size.toLong))((n, k) => Weight.add(n, textLength(k)))
          go(m.entries.values.toList ::: rest, Weight.add(acc, keys))
        case v :: rest => go(rest, Weight.add(acc, scalarLength(v)))
      }
    go(List(value), 0L)
  }

  private def scalarLength(v: CanonicalValue): Long =
    v match {
      case CInt(n) => integerLength(n)
      case r: CRational =>
        headLength(TagRational.toLong) + headLength(2L) + integerLength(r.value.numerator) + integerLength(r.value.denominator)
      case CText(s)  => textLength(s)
      case CBytes(b) => Weight.add(headLength(b.size), b.size)
      case _         => 1L
    }

  private def textLength(s: String): Long = {
    val n = Utf16.utf8Length(s)
    Weight.add(headLength(n), n)
  }

  private def integerLength(n: BigInt): Long = {
    val m = if (n.signum >= 0) n else -1 - n
    if (m.isValidLong) headLength(m.toLong)
    else if (m < TwoTo64) 9L
    else {
      val bytes = (m.bitLength + 7) / 8
      1L + headLength(bytes.toLong) + bytes
    }
  }

  /** The length of [[head]]'s output, for `0 <= argument < 2^63`. */
  private def headLength(argument: Long): Long =
    if (argument < 24) 1L
    else if (argument < 0x100) 2L
    else if (argument < 0x10000) 3L
    else if (argument < (1L << 32)) 5L
    else 9L

  /**
   * Whether entry `a`'s encoded key sorts before `b`'s, bytewise (RFC 8949 §4.2.1), from their UTF-8 lengths alone and
   * their code points: a key's head grows with its length, so a shorter key sorts first; keys of one length have one
   * head, and their UTF-8 bytes order as their code points.
   */
  private def keyBefore(a: (String, Long, CanonicalValue), b: (String, Long, CanonicalValue)): Boolean =
    a._2 < b._2 || (a._2 == b._2 && codePointCompare(a._1, b._1) < 0)

  /**
   * Code-point order of two strings, read as UTF-16 units: a surrogate (a code point past U+FFFF) sorts after every
   * unit from U+E000 up, which UTF-16 order would put after it.
   */
  private[canonical] def codePointCompare(a: String, b: String): Int = {
    def rank(c: Char): Int = if (c >= 0xe000) c - 0x800 else if (c >= 0xd800) c + 0x2000 else c.toInt
    @tailrec
    def go(i: Int): Int =
      if (i >= a.length || i >= b.length) a.length.compare(b.length)
      else if (a.charAt(i) == b.charAt(i)) go(i + 1)
      else rank(a.charAt(i)).compare(rank(b.charAt(i)))
    go(0)
  }

  private def integer[S](s: S, n: BigInt)(implicit out: ByteSink[S]): S =
    if (n.signum >= 0) { if (n < TwoTo64) head(s, 0, n.toLong) else bignum(s, TagPositiveBignum, n) }
    else {
      val m = -1 - n
      if (m < TwoTo64) head(s, 1, m.toLong) else bignum(s, TagNegativeBignum, m)
    }

  private def bignum[S](s: S, tag: Int, magnitude: BigInt)(implicit out: ByteSink[S]): S = {
    val bytes = FlatBytes(ByteVector.view(magnitude.toByteArray).dropWhile(_ == 0))
    out.bytes(head(head(s, 6, tag.toLong), 2, bytes.size), bytes)
  }

  /**
   * Shortest-form initial byte plus argument, the argument read as an unsigned 64-bit number: `BigInt.toLong` of an
   * argument below 2^64 keeps exactly its bits.
   */
  private def head[S](s: S, major: Int, argument: Long)(implicit out: ByteSink[S]): S = {
    val mt = major << 5
    def below(bound: Long): Boolean = java.lang.Long.compareUnsigned(argument, bound) < 0
    @tailrec
    def bigEndian(s: S, byte: Int): S = if (byte < 0) s else bigEndian(out.byte(s, ((argument >>> (8 * byte)) & 0xff).toInt), byte - 1)
    if (below(24)) out.byte(s, mt | argument.toInt)
    else if (below(0x100)) bigEndian(out.byte(s, mt | 24), 0)
    else if (below(0x10000)) bigEndian(out.byte(s, mt | 25), 1)
    else if (below(1L << 32)) bigEndian(out.byte(s, mt | 26), 3)
    else bigEndian(out.byte(s, mt | 27), 7)
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
   * succeeds. An explicit frame stack bounds nesting by `maxDepth` instead of the JVM call stack. The input is made
   * flat once on entry ([[FlatBytes]]: O(1) when it already is, one copy otherwise), so every byte string decoded from
   * it is a view of that one array, and every read of the input is O(1).
   */
  def decode(bytes: ByteVector, maxDepth: Int = DefaultMaxDepth): Either[CanonicalError, CanonicalValue] =
    decode(bytes, maxDepth, KeyOrder.Default)

  /** [[decode]] building maps in `keys`'s order: production passes [[KeyOrder.Default]]. */
  private[canonical] def decode(bytes: ByteVector, maxDepth: Int, keys: KeyOrder): Either[CanonicalError, CanonicalValue] =
    decode(bytes, maxDepth, keys, Numbers.Default)

  /** [[decode]] building numbers with `numbers`: production passes [[Numbers.Default]]. */
  private[canonical] def decode(
    bytes:    ByteVector,
    maxDepth: Int,
    keys:     KeyOrder,
    numbers:  Numbers
  ): Either[CanonicalError, CanonicalValue] =
    run(FlatBytes(bytes), math.min(maxDepth, CanonicalValue.MaxDepth), keys, numbers, Read(0L, Nil, 0))

  sealed private trait Frame
  final private case class ArrayFrame(remaining: Long, acc: Vector[CanonicalValue]) extends Frame
  final private case class MapFrame(
    remaining:  Long,
    acc:        Vector[(CText, CanonicalValue)],
    lastKey:    Option[ByteVector],
    pendingKey: Option[(CText, ByteVector)]
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
  private def run(
    in:       ByteVector,
    maxDepth: Int,
    keys:     KeyOrder,
    numbers:  Numbers,
    step:     Step
  ): Either[CanonicalError, CanonicalValue] =
    step match {
      case Read(pos, stack, depth) =>
        readItem(in, pos, numbers) match {
          case Left(e)                       => e.asLeft
          case Right(Scalar(v, end))         => run(in, maxDepth, keys, numbers, Deliver(v, pos, end, stack, depth))
          case Right(_) if depth >= maxDepth => DepthExceeded(maxDepth).asLeft // an empty array or map is one level too
          case Right(OpenArray(0L, end))     => run(in, maxDepth, keys, numbers, Deliver(EmptyArray, pos, end, stack, depth))
          case Right(OpenMap(0L, end))       => run(in, maxDepth, keys, numbers, Deliver(EmptyMap, pos, end, stack, depth))
          case Right(OpenArray(n, end))      => run(in, maxDepth, keys, numbers, Read(end, ArrayFrame(n, Vector.empty) :: stack, depth + 1))
          case Right(OpenMap(n, end)) =>
            run(in, maxDepth, keys, numbers, Read(end, MapFrame(n, Vector.empty, None, None) :: stack, depth + 1))
        }

      case Deliver(v, _, end, Nil, _) =>
        Either.cond(end == in.size, v, TrailingBytes(end))

      case Deliver(v, _, end, ArrayFrame(remaining, acc) :: rest, depth) =>
        if (remaining == 1) run(in, maxDepth, keys, numbers, Deliver(CanonicalValue.decodedArray(acc :+ v), -1, end, rest, depth - 1))
        else run(in, maxDepth, keys, numbers, Read(end, ArrayFrame(remaining - 1, acc :+ v) :: rest, depth))

      case Deliver(key: CText, start, end, MapFrame(remaining, acc, lastKey, None) :: rest, depth) =>
        val keyBytes = in.slice(start, end)
        if (lastKey.exists(compareBytes(_, keyBytes) >= 0))
          NonCanonical(start, "keyOrder").asLeft
        else run(in, maxDepth, keys, numbers, Read(end, MapFrame(remaining, acc, lastKey, Some((key, keyBytes))) :: rest, depth))

      case Deliver(_, start, _, MapFrame(_, _, _, None) :: _, _) =>
        Unsupported(start, UnsupportedItem.NonTextKey).asLeft

      case Deliver(CNull, start, _, MapFrame(_, _, _, Some(_)) :: _, _) =>
        NonCanonical(start, "nullMapValue").asLeft

      case Deliver(v, _, end, MapFrame(remaining, acc, _, Some((key, keyBytes))) :: rest, depth) =>
        if (remaining == 1)
          run(in, maxDepth, keys, numbers, Deliver(CanonicalValue.decodedMap(acc :+ (key -> v), keys), -1, end, rest, depth - 1))
        else run(in, maxDepth, keys, numbers, Read(end, MapFrame(remaining - 1, acc :+ (key -> v), Some(keyBytes), None) :: rest, depth))
    }

  private def readItem(in: ByteVector, pos: Long, numbers: Numbers): Either[CanonicalError, Item] =
    readHead(in, pos).flatMap {
      case Head(0, n, end) => CanonicalValue.int(n).map(Scalar(_, end))
      case Head(1, n, end) => CanonicalValue.int(-1 - n).map(Scalar(_, end))
      case Head(2, n, end) => readSpan(in, end, n).map { case (b, e) => Scalar(CBytes(b), e) }
      case Head(3, n, end) => readText(in, end, n).map { case (t, e) => Scalar(t, e) }
      case Head(4, n, end) => count(in, end, n, 1).map(OpenArray(_, end))
      case Head(5, n, end) => count(in, end, n, 2).map(OpenMap(_, end))
      case Head(6, t, end) => readTagged(in, pos, t, end, numbers).map { case (v, e) => Scalar(v, e) }
      case Head(_, n, end) =>
        n.toInt match {
          case 20 => Scalar(False, end).asRight
          case 21 => Scalar(True, end).asRight
          case 22 => Scalar(CNull, end).asRight
          case s  => Unsupported(pos, UnsupportedItem.SimpleValue(s)).asLeft
        }
    }

  private def readHead(in: ByteVector, pos: Long): Either[CanonicalError, Head] =
    byteAt(in, pos).flatMap { initial =>
      val (major, info) = (initial >>> 5, initial & 0x1f)
      def sized(len: Int, minimum: BigInt): Either[CanonicalError, Head] =
        readSpan(in, pos + 1, BigInt(len)).flatMap {
          case (b, end) =>
            val n = BigInt(1, b.toArray)
            Either.cond(n >= minimum, Head(major, n, end), NonCanonical(pos, "nonShortestArgument"))
        }
      if (info < 24) Head(major, BigInt(info), pos + 1).asRight
      else if (major == 7) Unsupported(pos, UnsupportedItem.FloatOrSimple(info)).asLeft
      else
        info match {
          case 24 => sized(1, 24)
          case 25 => sized(2, 0x100)
          case 26 => sized(4, 0x10000)
          case 27 => sized(8, BigInt(1) << 32)
          case 31 => Unsupported(pos, UnsupportedItem.IndefiniteLength).asLeft
          case _  => Unsupported(pos, UnsupportedItem.ReservedInfo(info)).asLeft
        }
    }

  private def readTagged(
    in:      ByteVector,
    pos:     Long,
    tag:     BigInt,
    end:     Long,
    numbers: Numbers
  ): Either[CanonicalError, (CanonicalValue, Long)] =
    tag.toInt match {
      case TagPositiveBignum | TagNegativeBignum if tag.isValidInt =>
        val tooWide = IntegerTooWide(CanonicalValue.MaxIntegerBits)
        readBignum(in, pos, tag.toInt, end, CanonicalValue.MaxIntegerBits, tooWide, numbers).flatMap {
          case (n, e) => CanonicalValue.int(n).map((_, e))
        }
      case TagRational if tag.isValidInt => readRational(in, pos, end, numbers)
      case _                             => Unsupported(pos, UnsupportedItem.Tag(tag)).asLeft
    }

  /**
   * A bignum's integer, in order: a byte string (`NonCanonical`), present in full (`UnexpectedEnd`), in canonical form
   * (`NonCanonical`), and at most `limit` wide (`tooWide`), all before the integer is built. Java's `bitLength` of `m`
   * and of `-1 - m` are both `m`'s, so either tag's width is its magnitude's: 8 bits per byte after the first, and the
   * first byte's own.
   */
  private def readBignum(
    in:      ByteVector,
    pos:     Long,
    tag:     Int,
    end:     Long,
    limit:   Int,
    tooWide: CanonicalError,
    numbers: Numbers
  ): Either[CanonicalError, (BigInt, Long)] =
    readHead(in, end).flatMap {
      case Head(2, len, bodyStart) =>
        readSpan(in, bodyStart, len).flatMap {
          case (b, e) =>
            if (b.headOption.forall(_ == 0)) NonCanonical(pos, "bignumLeadingZero").asLeft
            else if (b.size <= 8) NonCanonical(pos, "bignumInRange").asLeft
            else if (8L * (b.size - 1) + (32 - Integer.numberOfLeadingZeros(b(0) & 0xff)) > limit) tooWide.asLeft
            else {
              val magnitude = numbers.magnitude(b)
              (if (tag == TagPositiveBignum) magnitude else -1 - magnitude, e).asRight
            }
        }
      case _ => NonCanonical(pos, "bignumNotBytes").asLeft
    }

  /**
   * A rational's numerator or denominator, refused when wider than [[CanonicalValue.MaxRationalBits]]: a bignum's width
   * is checked from its length, before it is built and before any gcd, so a wide part costs no more than reading it.
   */
  private def readPart(in: ByteVector, pos: Long, numbers: Numbers): Either[CanonicalError, (BigInt, Long)] =
    readHead(in, pos).flatMap {
      case Head(0, n, end) => (n, end).asRight
      case Head(1, n, end) => (-1 - n, end).asRight
      case Head(6, t, end) if t == TagPositiveBignum || t == TagNegativeBignum =>
        val tooWide = RationalTooWide(CanonicalValue.MaxRationalBits)
        readBignum(in, pos, t.toInt, end, CanonicalValue.MaxRationalBits, tooWide, numbers)
      case _ => NonCanonical(pos, "rationalNotInteger").asLeft
    }

  /**
   * In order: the pair, the numerator and its width, the denominator and its width, the denominator's range, and last
   * the one gcd that checks lowest terms (docs/specs/conformance.md §5).
   */
  private def readRational(
    in:      ByteVector,
    pos:     Long,
    end:     Long,
    numbers: Numbers
  ): Either[CanonicalError, (CanonicalValue, Long)] =
    for {
      pair <- readHead(in, end)
      _    <- Either.cond(pair.major == 4 && pair.argument == BigInt(2), (), NonCanonical(pos, "rationalNotPair"))
      num  <- readPart(in, pair.end, numbers)
      den  <- readPart(in, num._2, numbers)
      _    <- Either.cond(den._1 >= 2, (), NonCanonical(pos, "rationalDenominator"))
      r    <- numbers.lowestTerms(num._1, den._1).toRight(NonCanonical(pos, "rationalNotReduced"))
      v    <- CanonicalValue.number(r)
    } yield (v, den._2)

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
object Utf8 {

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
