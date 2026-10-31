package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.Searching

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/** Value-level semantics shared by every operation (docs/specs/jlvm.md): truthiness, numbers, equality, ordering. */
object Values {

  /** JsonLogic truthiness as metakit defines it: null, false, 0, "", empty bytes, [] and {} are falsy. */
  def truthy(v: CanonicalValue): Boolean = v match {
    case CNull        => false
    case CBool(b)     => b
    case CInt(i)      => i != 0
    case CRational(_) => true
    case CText(s)     => s.nonEmpty
    case CBytes(b)    => b.nonEmpty
    case CArray(vs)   => vs.nonEmpty
    case CMap(m)      => m.nonEmpty
  }

  /**
   * The largest number, in bits of the numerator and of the denominator. Every number an operation reads or produces is
   * checked, so arithmetic (including the gcd that keeps rationals reduced) works on bounded operands and its cost is
   * a constant the gas schedule can price.
   */
  val MaxNumberBits: Int = 4096

  /** The largest [[CanonicalValue.weight]] an operation may produce: bounds memory and the cost of later traversals. */
  val MaxWeight: Long = 1L << 24

  def number(v: CanonicalValue): Option[Ratio] = v match {
    case CInt(i)      => Some(Ratio.integer(i))
    case CRational(r) => Some(r)
    case _            => None
  }

  private def fits(r: Ratio): Boolean = r.numerator.bitLength <= MaxNumberBits && r.denominator.bitLength <= MaxNumberBits

  private def tooLarge(op: Op): JlvmError = JlvmError.Limit(s"${op.wire}: a number exceeds $MaxNumberBits bits")

  /** A number operand, within [[MaxNumberBits]]. */
  def numeric(op: Op, v: CanonicalValue): Either[JlvmError, Ratio] =
    number(v)
      .toRight(JlvmError.Type(op.wire, s"expected a number, got ${kind(v)}"))
      .flatMap(r => Either.cond(fits(r), r, tooLarge(op)))

  /** An integer operand, within [[MaxNumberBits]]. */
  def integer(op: Op, v: CanonicalValue): Either[JlvmError, BigInt] =
    v match {
      case CInt(i) => Either.cond(i.bitLength <= MaxNumberBits, i, tooLarge(op))
      case other   => JlvmError.Type(op.wire, s"expected an integer, got ${kind(other)}").asLeft
    }

  /** A number result, within [[MaxNumberBits]]. */
  def num(op: Op, r: Ratio): Either[JlvmError, CanonicalValue] =
    Either.cond(fits(r), CanonicalValue.number(r), tooLarge(op))

  def int(op: Op, i: BigInt): Either[JlvmError, CanonicalValue] =
    Either.cond(i.bitLength <= MaxNumberBits, CInt(i), tooLarge(op))

  /** A count or a length: always small. */
  def count(n: Long): CanonicalValue = CInt(BigInt(n))

  def bool(b: Boolean): CanonicalValue = CBool(b)

  def bounded(op: Op, v: CanonicalValue): Either[JlvmError, CanonicalValue] = bounded(op.wire, v)

  /** `where` names the construct in error messages: an operation's wire name, or `array` / `object` for literals. */
  def bounded(where: String, v: CanonicalValue): Either[JlvmError, CanonicalValue] =
    Either.cond(v.weight <= MaxWeight, v, JlvmError.Limit(s"$where: the result exceeds weight $MaxWeight"))

  def text(op: Op, s: String): Either[JlvmError, CanonicalValue] =
    CanonicalValue.text(s).leftMap(_ => JlvmError.Type(op.wire, "the result is not well-formed text")).flatMap(bounded(op, _))

  def array(op: Op, vs: Vector[CanonicalValue]): Either[JlvmError, CanonicalValue] = array(op.wire, vs)

  def array(where: String, vs: Vector[CanonicalValue]): Either[JlvmError, CanonicalValue] =
    CanonicalValue.array(vs).leftMap(e => JlvmError.Limit(s"$where: $e")).flatMap(bounded(where, _))

  def map(op: Op, entries: Iterable[(String, CanonicalValue)]): Either[JlvmError, CanonicalValue] = map(op.wire, entries)

  def map(where: String, entries: Iterable[(String, CanonicalValue)]): Either[JlvmError, CanonicalValue] =
    CanonicalValue.map(entries).leftMap(e => JlvmError.Limit(s"$where: $e")).flatMap(bounded(where, _))

  /** A map from keys checked once (a program's literal keys), distinct: no key is rescanned. */
  def textKeyed(where: String, entries: Vector[(CText, CanonicalValue)]): Either[JlvmError, CanonicalValue] =
    CanonicalValue.textKeyed(entries).leftMap(e => JlvmError.Limit(s"$where: $e")).flatMap(bounded(where, _))

  /**
   * The value at `path`: an object segment is a key; an array segment is a decimal index without leading zeros. Anything
   * absent is null.
   */
  @tailrec
  def at(v: CanonicalValue, path: Vector[String]): CanonicalValue =
    (v, path) match {
      case (_, seg +: rest) =>
        v match {
          case CMap(m) =>
            m.get(seg) match {
              case Some(x) => at(x, rest)
              case None    => CNull
            }
          case CArray(vs) =>
            index(seg).filter(_ < vs.size) match {
              case Some(i) => at(vs(i), rest)
              case None    => CNull
            }
          case _ => CNull
        }
      case _ => v
    }

  private val Decimal = "0|[1-9][0-9]{0,8}".r

  private def index(seg: String): Option[Int] = Option.when(Decimal.matches(seg))(seg.toInt)

  def bytesValue(op: Op, b: ByteVector): Either[JlvmError, CanonicalValue] = bounded(op, CBytes(b))

  /** The kind name reported by `typeof`. There is one number kind. */
  def kind(v: CanonicalValue): String = v match {
    case CNull                  => "null"
    case CBool(_)               => "boolean"
    case CInt(_) | CRational(_) => "number"
    case CText(_)               => "string"
    case CBytes(_)              => "bytes"
    case CArray(_)              => "array"
    case CMap(_)                => "object"
  }

  /** Strict equality is structural equality of canonical values (`2.0` and `2` are the same number). */
  def equal(a: CanonicalValue, b: CanonicalValue): Boolean = a == b

  /** Text compares by Unicode code point; numbers numerically; anything else cannot be ordered. */
  def compare(op: Op, a: CanonicalValue, b: CanonicalValue): Either[JlvmError, Int] =
    (a, b) match {
      case (CText(x), CText(y))                            => compareCodePoints(x, y).asRight
      case _ if number(a).isDefined && number(b).isDefined => (numeric(op, a), numeric(op, b)).mapN(_.compare(_))
      case _                                               => JlvmError.Type(op.wire, s"cannot order ${kind(a)} and ${kind(b)}").asLeft
    }

  def compareCodePoints(x: String, y: String): Int = {
    @tailrec
    def go(i: Int): Int =
      if (i >= x.length || i >= y.length) Integer.compare(x.length, y.length)
      else if (x.charAt(i) == y.charAt(i)) go(i + 1)
      else Integer.compare(codePointRank(x.charAt(i)), codePointRank(y.charAt(i)))
    go(0)
  }

  /**
   * UTF-16 units ranked so that comparing them at the first difference gives code-point order (ICU's fix-up): only a
   * surrogate, which starts a code point above U+FFFF, can meet a unit in U+E000..U+FFFF out of order, so surrogates
   * move above that range.
   */
  private def codePointRank(c: Char): Int = if (c < 0xd800) c.toInt else if (c < 0xe000) c + 0x2000 else c - 0x800

  def codePoints(s:       String): Vector[Int] = s.codePoints().toArray.toVector
  def fromCodePoints(cps: Vector[Int]): String = new String(cps.toArray, 0, cps.size)

  /**
   * ASCII case mapping (docs/specs/jlvm.md §2): `A`–`Z` and `a`–`z` swap, every other code point is unchanged. No UTF-16
   * unit of a supplementary code point is an ASCII letter, so mapping units maps code points.
   */
  def lower(s: String): String = s.map(c => if (c >= 'A' && c <= 'Z') (c + ('a' - 'A')).toChar else c)
  def upper(s: String): String = s.map(c => if (c >= 'a' && c <= 'z') (c - ('a' - 'A')).toChar else c)

  /**
   * The Unicode `White_Space` property (PropList.txt), listed explicitly: Java's `strip` omits no-break spaces, and
   * JavaScript's `trim` adds U+FEFF and omits U+0085, so neither is a portable definition.
   */
  val WhiteSpace: Set[Int] =
    Set(0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x20, 0x85, 0xa0, 0x1680, 0x2028, 0x2029, 0x202f, 0x205f, 0x3000) ++ (0x2000 to 0x200a)

  /** Trims Unicode `White_Space` from both ends. */
  def trim(s: String): String = {
    @tailrec def start(i: Int): Int =
      if (i < s.length && WhiteSpace(s.codePointAt(i))) start(i + Character.charCount(s.codePointAt(i))) else i
    @tailrec def end(j: Int, floor: Int): Int =
      if (j > floor && WhiteSpace(s.codePointBefore(j))) end(j - Character.charCount(s.codePointBefore(j)), floor) else j
    val from = start(0)
    s.substring(from, end(s.length, from))
  }

  /** Map keys in Unicode code-point order (the order of `keys`, `values` and `entries`). */
  def sortedKeys(m: scala.collection.immutable.Map[String, CanonicalValue]): List[String] =
    m.keys.toList.sortWith((a, b) => compareCodePoints(a, b) < 0)

  /**
   * The bits of a number's decimal expansion, bounded from its size alone: the numerator's bits for an integer; for a
   * terminating `p/q` with `q = 2^a 5^b`, the expansion is `p 2^(s-a) 5^(s-b)` with `s = max(a, b)`, at most
   * `bits(p) + log2(5) bits(q)` bits, bounded here by `bits(p) + 7 bits(q) / 3`. 0 for anything else.
   */
  def renderedBits(v: CanonicalValue): Long = v match {
    case CInt(i)      => i.bitLength.toLong
    case CRational(r) => r.numerator.bitLength.toLong + (7L * r.denominator.bitLength + 2) / 3
    case _            => 0L
  }

  // ---- an order for deduplicating

  /**
   * A sort key for deduplicating: kinds first; scalars then compare structurally (numbers by numerator and
   * denominator, text by UTF-16 unit, bytes bytewise), and arrays and maps by their canonical encoding, computed once.
   * The order is total and consistent with equality, and nothing is hashed, so values a program chooses cannot be made
   * to collide.
   */
  final case class SortKey(rank: Int, value: CanonicalValue, encoding: ByteVector)

  private def unsigned(a: ByteVector, b: ByteVector): Int = java.util.Arrays.compareUnsigned(a.toArrayUnsafe, b.toArrayUnsafe)

  given Ordering[SortKey] = (a, b) =>
    if (a.rank != b.rank) Integer.compare(a.rank, b.rank)
    else
      (a.value, b.value) match {
        case (CBool(x), CBool(y))         => x.compare(y)
        case (CInt(x), CInt(y))           => x.compare(y)
        case (CRational(x), CRational(y)) => Ordering[(BigInt, BigInt)].compare((x.numerator, x.denominator), (y.numerator, y.denominator))
        case (CText(x), CText(y))         => x.compareTo(y)
        case (CBytes(x), CBytes(y))       => unsigned(x, y)
        case _                            => unsigned(a.encoding, b.encoding)
      }

  private def key(v: CanonicalValue): SortKey = v match {
    case CNull        => SortKey(0, v, ByteVector.empty)
    case CBool(_)     => SortKey(1, v, ByteVector.empty)
    case CInt(_)      => SortKey(2, v, ByteVector.empty)
    case CRational(_) => SortKey(3, v, ByteVector.empty)
    case CText(_)     => SortKey(4, v, ByteVector.empty)
    case CBytes(_)    => SortKey(5, v, ByteVector.empty)
    case CArray(_)    => SortKey(6, v, Cbor.encode(v).compact)
    case CMap(_)      => SortKey(7, v, Cbor.encode(v).compact)
  }

  /** `vs` without repeats, keeping each value's first occurrence, in order: a stable sort and a scan, O(n log n). */
  def distinct(vs: Vector[CanonicalValue]): Vector[CanonicalValue] = {
    val sorted = vs.map(key).zipWithIndex.sortBy(_._1)
    val firsts = sorted.zip(None +: sorted.map(Option(_))).collect {
      case ((e, i), previous) if !previous.exists(p => summon[Ordering[SortKey]].equiv(p._1, e)) => i
    }
    firsts.sorted.map(vs)
  }

  /** Whether every element of `xs` is in `ys`: a binary search of `ys` in canonical order, O((m + n) log n). */
  def subset(xs: Vector[CanonicalValue], ys: Vector[CanonicalValue]): Boolean = {
    val sorted = ys.map(key).sorted
    xs.forall(x =>
      sorted.search(key(x)) match {
        case Searching.Found(_)          => true
        case Searching.InsertionPoint(_) => false
      }
    )
  }

  /**
   * Keys and path segments are compared, and rendered in warnings, in time that grows with their length: each counts once
   * more per block of this many UTF-16 units. Names under a block cost what they always did.
   */
  val KeyBlock: Int = 32

  /** A path segment's count: 1, plus one per [[KeyBlock]] of its length. */
  def segmentCount(s: String): Long = 1L + s.length / KeyBlock

  /** The blocks of text used as a key ([[GasSchedule]]'s `Key` unit); 0 for anything else. */
  def keyBlocks(v: CanonicalValue): Long = v match {
    case CText(s) => (s.length / KeyBlock).toLong
    case _        => 0L
  }

  /** Top-level size, for operations that only touch the outermost collection. */
  def elements(v: CanonicalValue): Long = v match {
    case CArray(vs) => vs.size.toLong
    case CMap(m)    => m.size.toLong
    case _          => 1L
  }

  /**
   * The first index at or after `from` where `needle` occurs in `haystack`, or -1: Knuth-Morris-Pratt, so the work is
   * linear in both lengths (a naive search is quadratic, which no linear gas charge could price).
   */
  def indexOf(haystack: Int => Int, hayLength: Int, needle: Int => Int, needleLength: Int, from: Int): Int = {
    val fail = failure(needle, needleLength)

    @tailrec
    def search(i: Int, k: Int): Int =
      if (k == needleLength) i - k
      else if (i >= hayLength) -1
      else if (haystack(i) == needle(k)) search(i + 1, k + 1)
      else if (k > 0) search(i, fail(k - 1))
      else search(i + 1, 0)

    search(from, 0)
  }

  /** The KMP failure function: for each prefix, the length of its longest proper prefix that is also a suffix. */
  private def failure(needle: Int => Int, n: Int): Vector[Int] = {
    @tailrec
    def build(i: Int, k: Int, acc: Vector[Int]): Vector[Int] =
      if (i >= n) acc
      else if (needle(i) == needle(k)) build(i + 1, k + 1, acc :+ (k + 1))
      else if (k > 0) build(i, acc(k - 1), acc)
      else build(i + 1, 0, acc :+ 0)

    if (n == 0) Vector.empty else build(1, 0, Vector(0))
  }

  def indexOf(haystack: String, needle: String, from: Int): Int =
    indexOf(i => haystack.charAt(i).toInt, haystack.length, i => needle.charAt(i).toInt, needle.length, from)

  def indexOf(haystack: ByteVector, needle: ByteVector): Long =
    indexOf(i => haystack(i.toLong).toInt, haystack.size.toInt, i => needle(i.toLong).toInt, needle.size.toInt, 0).toLong
}
