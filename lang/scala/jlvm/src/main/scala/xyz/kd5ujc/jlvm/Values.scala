package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor, CodePointOrder}
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
  val MaxNumberBits: Int = xyz.kd5ujc.models.limits.JlvmMaxNumberBits

  /** The largest [[CanonicalValue.weight]] an operation may produce: bounds memory and the cost of later traversals. */
  val MaxWeight: Long = xyz.kd5ujc.models.limits.JlvmMaxValueWeight

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
      .toRight(JlvmError.Type(op, s"expected a number, got ${kind(v)}"))
      .flatMap(r => Either.cond(fits(r), r, tooLarge(op)))

  /** An integer operand, within [[MaxNumberBits]]. */
  def integer(op: Op, v: CanonicalValue): Either[JlvmError, BigInt] =
    v match {
      case CInt(i) => Either.cond(i.bitLength <= MaxNumberBits, i, tooLarge(op))
      case other   => JlvmError.Type(op, s"expected an integer, got ${kind(other)}").asLeft
    }

  /**
   * A number result, within [[MaxNumberBits]]: far inside the data model's integer and rational bounds (33219 bits),
   * so the constructor's refusal is never reached, and would be the same `Limit`.
   */
  def num(op: Op, r: Ratio): Either[JlvmError, CanonicalValue] =
    if (fits(r)) CanonicalValue.number(r).leftMap(_ => tooLarge(op)) else tooLarge(op).asLeft

  /** An integer result, within [[MaxNumberBits]], as [[num]]. */
  def int(op: Op, i: BigInt): Either[JlvmError, CanonicalValue] =
    if (i.bitLength <= MaxNumberBits) CanonicalValue.int(i).leftMap(_ => tooLarge(op)) else tooLarge(op).asLeft

  /** A count or a length: a `Long`, so always within every bound. */
  def count(n: Long): CanonicalValue = CInt(n)

  def bool(b: Boolean): CanonicalValue = CBool(b)

  def bounded(op: Op, v: CanonicalValue): Either[JlvmError, CanonicalValue] = bounded(op.wire, v)

  /** `where` names the construct in error messages: an operation's wire name, or `array` / `object` for literals. */
  def bounded(where: String, v: CanonicalValue): Either[JlvmError, CanonicalValue] =
    Either.cond(v.weight <= MaxWeight, v, JlvmError.Limit(s"$where: the result exceeds weight $MaxWeight"))

  def text(op: Op, s: String): Either[JlvmError, CanonicalValue] =
    CanonicalValue.text(s).leftMap(_ => JlvmError.Type(op, "the result is not well-formed text")).flatMap(bounded(op, _))

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

  /** A byte string an operation made: flat ([[FlatBytes]]), so what reads it later never depends on how it was built. */
  def bytes(b: FlatBytes): CanonicalValue = CBytes(b)

  def bytesValue(op: Op, b: FlatBytes): Either[JlvmError, CanonicalValue] = bounded(op, bytes(b))

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
      case _                                               => JlvmError.Type(op, s"cannot order ${kind(a)} and ${kind(b)}").asLeft
    }

  /** Code-point order, read as UTF-16 units ranked at the first difference (`CodePointOrder.rank`). */
  def compareCodePoints(x: String, y: String): Int = CodePointOrder.compare(x, y)

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
   * to collide. A key's `encoding` is one whole array (bytes' own, copied once if they are a slice or a rope), so a
   * comparison reads it in place and never copies.
   */
  final case class SortKey(rank: Int, value: CanonicalValue, encoding: ByteVector)

  private def unsigned(a: ByteVector, b: ByteVector): Int = java.util.Arrays.compareUnsigned(a.toArrayUnsafe, b.toArrayUnsafe)

  /** The production order on [[SortKey]]s ([[Kernels.Default]]'s `values`). */
  given sortKeyOrder: Ordering[SortKey] = (a, b) =>
    if (a.rank != b.rank) Integer.compare(a.rank, b.rank)
    else
      (a.value, b.value) match {
        case (CBool(x), CBool(y))         => x.compare(y)
        case (CInt(x), CInt(y))           => x.compare(y)
        case (CRational(x), CRational(y)) => Ordering[(BigInt, BigInt)].compare((x.numerator, x.denominator), (y.numerator, y.denominator))
        case (CText(x), CText(y))         => x.compareTo(y)
        case _                            => unsigned(a.encoding, b.encoding)
      }

  /**
   * `v`'s sort key. An array's or a map's encoding is budgeted by its weight, which the operation has paid for: no
   * encoding is longer than `Cbor.EncodingPerWeight` bytes per unit of weight, so it is written at once, unmeasured,
   * and only an encoding past `Cbor.MaxEncodedBytes`, the JVM's largest array, is refused, as `TooLarge`; so is a byte
   * string longer than that, which is its own key.
   */
  def sortKey(v: CanonicalValue): Either[Cbor.TooLarge, SortKey] = {
    def encoded(rank: Int): Either[Cbor.TooLarge, SortKey] =
      // no overflow: a weight past the ceiling is the ceiling, which budgets past the ceiling all the same
      Cbor.encodeWithin(v, v.weight.min(Cbor.MaxEncodedBytes) * Cbor.EncodingPerWeight).map(e => SortKey(rank, v, e))
    v match {
      case CNull        => SortKey(0, v, ByteVector.empty).asRight
      case CBool(_)     => SortKey(1, v, ByteVector.empty).asRight
      case CInt(_)      => SortKey(2, v, ByteVector.empty).asRight
      case CRational(_) => SortKey(3, v, ByteVector.empty).asRight
      case CText(_)     => SortKey(4, v, ByteVector.empty).asRight
      // a byte string is its own key, read as one array: one longer than the ceiling (a host's lazy view) is refused
      case CBytes(b) =>
        Either.cond(b.size <= Cbor.MaxEncodedBytes, SortKey(5, v, ByteVector.view(b.toArrayUnsafe)), Cbor.TooLarge(Cbor.MaxEncodedBytes))
      case CArray(_) => encoded(6)
      case CMap(_)   => encoded(7)
    }
  }

  /**
   * Keys and path segments are compared, and rendered in warnings, in time that grows with their length: each counts once
   * more per block of this many UTF-16 units. Names under a block cost what they always did.
   */
  val KeyBlock: Int = 32

  /** A path segment's count: 1, plus one per [[KeyBlock]] of its length. */
  def segmentCount(s: String): Long = 1L + s.length / KeyBlock

  /**
   * A map search ([[Search]]) pays once more per this many UTF-16 units of its key, at each level: a comparison reads
   * the keys' common prefix.
   */
  val SearchBlock: Int = 128

  /** Top-level size, for operations that only touch the outermost collection. */
  def elements(v: CanonicalValue): Long = v match {
    case CArray(vs) => vs.size.toLong
    case CMap(m)    => m.size.toLong
    case _          => 1L
  }

  /**
   * The first index at or after `from` where `needle` occurs in `haystack`, or -1: Knuth-Morris-Pratt, so the work is
   * linear in both lengths (a naive search is quadratic, which no linear gas charge could price). Inlined for each
   * representation, so every step is a direct read of a string or an array rather than a call through a function.
   */
  private inline def kmp(inline haystack: Int => Int, hayLength: Int, inline needle: Int => Int, needleLength: Int, from: Int): Int = {
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

  /**
   * The KMP failure function: for each prefix, the length of its longest proper prefix that is also a suffix. Each
   * entry reads earlier ones, so it is built in an array local to this call, which never escapes it (a persistent
   * vector of boxed integers made the search several times slower than its gas).
   */
  private inline def failure(inline needle: Int => Int, n: Int): Array[Int] = {
    val table = new Array[Int](n.max(1))

    @tailrec
    def build(i: Int, k: Int): Array[Int] =
      if (i >= n) table
      else if (needle(i) == needle(k)) { table(i) = k + 1; build(i + 1, k + 1) }
      else if (k > 0) build(i, table(k - 1))
      else { table(i) = 0; build(i + 1, 0) }

    build(1, 0)
  }

  def indexOf(haystack: String, needle: String, from: Int): Int =
    kmp(haystack.charAt(_).toInt, haystack.length, needle.charAt(_).toInt, needle.length, from)

  /** Flat operands, copied once into arrays: each step of the search is a direct array read. */
  def indexOf(haystack: FlatBytes, needle: FlatBytes): Long = {
    val (h, n) = (haystack.toArray, needle.toArray)
    kmp(h(_).toInt, h.length, n(_).toInt, n.length, 0).toLong
  }
}
