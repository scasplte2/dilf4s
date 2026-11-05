package xyz.kd5ujc.core.canonical

import cats.Show
import cats.syntax.either._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * The canonical data model (docs/specs/canonical-encoding.md §1). Invariants are enforced by the smart constructors in
 * the companion: integers are within [[CanonicalValue.MaxIntegerBits]], rationals are never integers and their parts
 * are within [[CanonicalValue.MaxRationalBits]], text
 * holds no unpaired surrogates, maps hold unique keys and no null values, and nesting never exceeds
 * [[CanonicalValue.MaxDepth]] — so every constructible value is decodable.
 */
sealed trait CanonicalValue extends Product with Serializable {

  /** Nesting depth: 0 for scalars, 1 + the deepest child for arrays and maps. */
  def depth: Int

  /**
   * A portable measure of size, cached at construction: one per node, plus the UTF-8 length of text and map keys, the
   * length of byte strings, and the 64-bit limbs of numbers (numerator and denominator). Shared sub-values count every
   * time they occur, so the weight bounds the work of any full traversal; it saturates at `Long.MaxValue`.
   */
  def weight: Long

  /** A readable form, bounded however often the value repeats a shared part ([[CanonicalValue.inspect]]). */
  final override def toString: String = CanonicalValue.inspect(this)
}

object CanonicalValue {

  val MaxDepth: Int = xyz.kd5ujc.models.limits.CanonicalMaxDepth

  /**
   * The widest numerator or denominator a rational has, in `bitLength` bits (docs/specs/canonical-encoding.md §1): the
   * integer bound, [[MaxIntegerBits]], and far past every JLVM number (4096 bits).
   * Checking lowest terms takes a gcd, superlinear in its operands, so a decoder refuses a wider part before any gcd: a
   * rational's decoding then costs at most one gcd of two numbers this wide, about 4 KiB each, and stays linear in its
   * input. [[number]] refuses a wider rational too, so no value is built that no decoder accepts.
   */
  val MaxRationalBits: Int = xyz.kd5ujc.models.limits.CanonicalMaxRationalBits

  /**
   * The widest integer, in `bitLength` bits (docs/specs/canonical-encoding.md §1): the widest whose every integer has
   * at most 10,000 digits (`2^33219 < 10^10000`), so every integer renders as JSON; `8e9999` is an integer and `9e9999`
   * is not. Far past every JLVM number (4096 bits), and the width of a rational's parts too. [[int]] refuses a
   * wider one, and a decoder refuses a wider bignum from its length, before it builds the integer: no value is built
   * that no decoder accepts, and none is so wide that what reads it (rendering, arithmetic, hashing) is unbounded.
   */
  val MaxIntegerBits: Int = xyz.kd5ujc.models.limits.CanonicalMaxIntegerBits

  implicit val canonicalValueShow: Show[CanonicalValue] = Show.fromToString

  /** How much of a value [[inspect]] shows, in units of weight; the rest is `…` (the SDKs' `DEBUG_BUDGET`). */
  val DebugBudget: Long = 1L << 16

  /** How many arrays and maps deep [[inspect]] shows; deeper ones are `…` (the SDKs' `DEBUG_DEPTH`). */
  val DebugDepth: Int = 16

  /** Numbers up to this many bits are shown in decimal, wider ones by their bit length: decimal conversion is superlinear. */
  private val ShownBits = 256

  /**
   * A readable form of `value` (CBOR diagnostic style, as the TypeScript SDK's `inspect`: `[1, "a", h'00ff', 1/3,
   * {"k": null}]`, a map's keys in its own order), bounded however often it repeats a shared part: scalars spend their
   * weight and containers and map keys theirs from a budget of [[DebugBudget]], nothing deeper than [[DebugDepth]] is
   * shown, and what is not shown is `…`. One stack-safe pass; a map's entries are read lazily, as they are shown.
   */
  def inspect(value: CanonicalValue): String = {
    @tailrec
    def go(todo: List[Inspecting], units: Long, out: Vector[String]): String =
      todo match {
        case Nil => out.mkString
        case Inspecting.Show(v, open) :: rest =>
          v match {
            case CArray(vs) if units >= 1 && open < DebugDepth => go(Inspecting.Items(vs, 0, open + 1) :: rest, units - 1, out :+ "[")
            case m: CMap if units >= 1 && open < DebugDepth =>
              go(Inspecting.Members(m.entries.iterator.to(LazyList), first = true, open + 1) :: rest, units - 1, out :+ "{")
            // an elided container still costs a unit, so the output stays within the budget
            case CArray(_) | CMap(_)    => go(rest, (units - 1).max(0L), out :+ Elided)
            case s if s.weight <= units => go(rest, units - s.weight, out :+ scalarText(s))
            case _                      => go(rest, 0L, out :+ Elided)
          }
        case Inspecting.Items(vs, i, open) :: rest =>
          if (i >= vs.size) go(rest, units, out :+ "]")
          else {
            val sep = if (i > 0) Vector(", ") else Vector.empty
            if (units == 0L) go(rest, units, out ++ sep :+ Elided :+ "]")
            else go(Inspecting.Show(vs(i), open) :: Inspecting.Items(vs, i + 1, open) :: rest, units, out ++ sep)
          }
        case Inspecting.Members(entries, first, open) :: rest =>
          entries match {
            case (k, v) #:: more =>
              val sep = if (first) Vector.empty else Vector(", ")
              // a key has at least as many UTF-8 bytes as UTF-16 units: one too long is refused before it is measured
              val key = if (units == 0L || k.length >= units) units + 1 else 1L + Utf16.utf8Length(k)
              if (key > units) go(rest, 0L, out ++ sep :+ Elided :+ "}")
              else
                go(
                  Inspecting.Show(v, open) :: Inspecting.Members(more, first = false, open) :: rest,
                  units - key,
                  out ++ sep :+ quotedText(k) :+ ": "
                )
            case _ => go(rest, units, out :+ "}")
          }
      }
    go(List(Inspecting.Show(value, 0)), DebugBudget, Vector.empty)
  }

  private val Elided = "…"

  /** What [[inspect]] has still to show, innermost first: a value under `open` containers, or the rest of one. */
  sealed private trait Inspecting

  private object Inspecting {
    final case class Show(value: CanonicalValue, open: Int) extends Inspecting
    final case class Items(values: Vector[CanonicalValue], next: Int, open: Int) extends Inspecting
    final case class Members(rest: LazyList[(String, CanonicalValue)], first: Boolean, open: Int) extends Inspecting
  }

  private def quotedText(s: String): String = CanonicalJson.render(new CText(s) {}).getOrElse(Elided)

  private def scalarText(v: CanonicalValue): String =
    v match {
      case CNull    => "null"
      case CBool(b) => b.toString
      case CInt(n)  => if (n.bitLength <= ShownBits) n.toString else s"<int of ${n.bitLength} bits>"
      case CRational(r) =>
        if (r.numerator.bitLength <= ShownBits && r.denominator.bitLength <= ShownBits) s"${r.numerator}/${r.denominator}"
        else s"<rational of ${r.numerator.bitLength}/${r.denominator.bitLength} bits>"
      case CText(s)  => quotedText(s)
      case CBytes(b) => s"h'${b.toHex}'"
      case _         => Elided
    }

  sealed trait Scalar extends CanonicalValue {
    final val depth: Int = 0
  }

  case object CNull extends Scalar {
    final val weight: Long = 1L
  }

  final case class CBool(value: Boolean) extends Scalar {
    final val weight: Long = 1L
  }

  /** An integer within [[MaxIntegerBits]]: built by [[int]], which checks the width, or from a `Long`, which fits. */
  sealed abstract case class CInt(value: BigInt) extends Scalar {
    final val weight: Long = 1L + value.bitLength / 64
  }

  object CInt {

    /** Any 64-bit integer, which is always within [[MaxIntegerBits]]. */
    def apply(value: Long): CInt = new CInt(BigInt(value)) {}
  }

  sealed abstract case class CRational(value: Ratio) extends Scalar {
    final val weight: Long = 2L + value.numerator.bitLength / 64 + value.denominator.bitLength / 64
  }

  sealed abstract case class CText(value: String) extends Scalar {
    final val weight: Long = 1L + Utf16.utf8Length(value)
  }

  /** A byte string, flat by construction ([[FlatBytes]]): no rope or buffer reaches a canonical value. */
  sealed abstract case class CBytes(value: FlatBytes) extends Scalar {
    final val weight: Long = 1L + value.size
  }

  object CBytes {

    /** `value`, flattened: O(1) when it is one chunk (as decoded, encoded and JLVM-made bytes are), one copy otherwise. */
    def apply(value: ByteVector): CBytes = new CBytes(FlatBytes(value)) {}
  }

  sealed abstract case class CArray(values: Vector[CanonicalValue]) extends CanonicalValue {
    final val depth: Int = 1 + values.foldLeft(0)((d, v) => math.max(d, v.depth))
    final val weight: Long = values.foldLeft(1L)((w, v) => Weight.add(w, v.weight))
  }

  /**
   * Besides its entries, a map keeps how many of its values have each depth, and its weight: [[updated]] and [[removed]]
   * then change one entry in O(log n) instead of a pass over all of them. Its entries are [[CanonicalEntries]], in
   * [[KeyOrder.Default]] (UTF-16 order) and no other: keys are compared, never hashed, so keys chosen to collide cannot
   * slow a map down. They iterate in that order, not the encoding's. Equality is on the entries alone.
   */
  sealed abstract case class CMap(entries: CanonicalEntries[CanonicalValue]) extends CanonicalValue {
    private[canonical] def childDepths: TreeMap[Int, Int]

    /** The order `entries` is in, which also measures keys. */
    private[canonical] def keys: KeyOrder
  }

  final private class MapNode(
    entries:         CanonicalEntries[CanonicalValue],
    val keys:        KeyOrder,
    val childDepths: TreeMap[Int, Int],
    val weight:      Long
  ) extends CMap(entries) {
    val depth: Int = 1 + childDepths.lastOption.fold(0)(_._1)
  }

  private def counted(h: TreeMap[Int, Int], depth: Int, by: Int): TreeMap[Int, Int] = {
    val n = h.getOrElse(depth, 0) + by
    if (n <= 0) h - depth else h.updated(depth, n)
  }

  /** The weight of `entries`, measuring each key once: for keys known to be well-formed. */
  private def recounted(entries: CanonicalEntries[CanonicalValue], keys: KeyOrder): Long =
    entries.foldLeft(1L) { case (w, (k, v)) => Weight.add(w, Weight.add(1L + keys.measure(k), v.weight)) }

  /** A map from entries in `keys`'s order with well-formed keys: one pass for the depth counts and the weight. */
  private def mapOf(entries: CanonicalEntries[CanonicalValue], keys: KeyOrder): CMap =
    new MapNode(
      entries,
      keys,
      entries.valuesIterator.foldLeft(TreeMap.empty[Int, Int])((h, v) => counted(h, v.depth, 1)),
      recounted(entries, keys)
    )

  /**
   * A map of unique text keys and non-null values: each key's weight is the one its [[CText]] cached, so no key is
   * rescanned; the keys are only compared, O(n log n) times.
   */
  private def ofText(entries: Vector[(CText, CanonicalValue)], keys: KeyOrder): CMap =
    new MapNode(
      CanonicalEntries.inserted(entries.iterator.map { case (k, v) => k.value -> v }, keys),
      keys,
      entries.foldLeft(TreeMap.empty[Int, Int]) { case (h, (_, v)) => counted(h, v.depth, 1) },
      entries.foldLeft(1L) { case (w, (k, v)) => Weight.add(w, Weight.add(k.weight, v.weight)) }
    )

  /**
   * `m` with entry `key` changed from `before` to `after`; a saturated weight is recounted. The key's weight is the one
   * its [[CText]] cached, so a change never rescans the key.
   */
  private def changed(m: CMap, key: CText, before: Option[CanonicalValue], after: Option[CanonicalValue]): CMap = {
    val entries = after.fold(m.entries.removed(key.value))(m.entries.updated(key.value, _))
    val without = before.fold(m.childDepths)(v => counted(m.childDepths, v.depth, -1))
    val depths = after.fold(without)(v => counted(without, v.depth, 1))
    def entry(v: CanonicalValue): Long = Weight.add(key.weight, v.weight)
    val weight =
      if (m.weight == Weight.Max) recounted(entries, m.keys)
      else Weight.add(m.weight - before.fold(0L)(entry), after.fold(0L)(entry))
    new MapNode(entries, m.keys, depths, weight)
  }

  val True: CanonicalValue = CBool(true)
  val False: CanonicalValue = CBool(false)
  val EmptyArray: CArray = new CArray(Vector.empty) {}
  val EmptyMap: CMap = mapOf(CanonicalEntries.empty, KeyOrder.Default)

  /** The integer `value`, refused when it is wider than [[MaxIntegerBits]] (the only constructor of a wider [[CInt]]). */
  def int(value: BigInt): Either[CanonicalError, CInt] =
    Either.cond(value.bitLength <= MaxIntegerBits, new CInt(value) {}, CanonicalError.IntegerTooWide(MaxIntegerBits))

  /**
   * A number in the single numeric domain: integral values are always [[CInt]], refused when wider than
   * [[MaxIntegerBits]], and a rational is refused when its numerator or denominator is wider than [[MaxRationalBits]]
   * (its only constructor).
   */
  def number(value: Ratio): Either[CanonicalError, CanonicalValue] =
    value.toBigIntExact.fold[Either[CanonicalError, CanonicalValue]](
      Either.cond(
        value.numerator.bitLength <= MaxRationalBits && value.denominator.bitLength <= MaxRationalBits,
        new CRational(value) {},
        CanonicalError.RationalTooWide(MaxRationalBits)
      )
    )(int)

  def text(value: String): Either[CanonicalError, CText] =
    Either.cond(Utf16.isWellFormed(value), new CText(value) {}, CanonicalError.UnpairedSurrogate)

  def bytes(value: ByteVector): CanonicalValue = CBytes(value)

  def array(values: Vector[CanonicalValue]): Either[CanonicalError, CArray] =
    withinDepth(new CArray(values) {})

  /**
   * Builds a map, dropping null-valued entries; invalid or duplicate keys are rejected, the first in input order. Each
   * key is measured once and compared O(log n) times.
   */
  def map(entries: Iterable[(String, CanonicalValue)]): Either[CanonicalError, CMap] = map(entries, KeyOrder.Default)

  /** [[map]] in `keys`'s order: production passes [[KeyOrder.Default]]. */
  private[canonical] def map(entries: Iterable[(String, CanonicalValue)], keys: KeyOrder): Either[CanonicalError, CMap] =
    collect(entries.toList, keys, CanonicalEntries.emptyIn(keys), TreeMap.empty, 1L).flatMap(withinDepth)

  /**
   * A map whose keys are text values already, each once: for keys checked once and reused, such as a program's
   * literal keys. Each key's weight is the one its [[CText]] cached, so no key is rescanned. Null values are dropped;
   * depth is checked.
   */
  private[kd5ujc] def textKeyed(entries: Vector[(CText, CanonicalValue)]): Either[CanonicalError, CMap] =
    textKeyed(entries, KeyOrder.Default)

  /**
   * [[textKeyed]] in `keys`'s order: production passes [[KeyOrder.Default]]. A repeated key, which would otherwise be
   * counted twice in the weight, is refused: the map then has fewer entries than it was given.
   */
  private[canonical] def textKeyed(entries: Vector[(CText, CanonicalValue)], keys: KeyOrder): Either[CanonicalError, CMap] = {
    val present = entries.filter { case (_, v) => v != CNull }
    val m = ofText(present, keys)
    if (m.entries.size == present.size) withinDepth(m)
    else {
      val sorted = present.map(_._1.value).sorted(using keys)
      sorted
        .zip(sorted.drop(1))
        .collectFirst { case (a, b) if keys.equiv(a, b) => CanonicalError.DuplicateKey(Preview.of(a)) }
        .toLeft(m)
        .flatMap(withinDepth)
    }
  }

  /**
   * `m` with `key` set to `value`, or removed when `value` is null, in O(log n). The key is already well-formed text, so
   * only depth is checked: no existing key is revalidated.
   */
  def updated(m: CMap, key: CText, value: CanonicalValue): Either[CanonicalError, CMap] =
    if (value == CNull) removed(m, key).asRight else withinDepth(changed(m, key, m.entries.get(key.value), Option(value)))

  /** `m` without `key`, in O(log n). */
  def removed(m: CMap, key: CText): CMap = m.entries.get(key.value).fold(m)(v => changed(m, key, Option(v), scala.None))

  /** Maps merged left to right, later keys winning; every key is already valid, and depth cannot grow. */
  def merged(maps: Vector[CMap]): CMap = merged(maps, KeyOrder.Default)

  /** [[merged]] in `keys`'s order: production passes [[KeyOrder.Default]]. Each key of the result is measured once. */
  private[canonical] def merged(maps: Vector[CMap], keys: KeyOrder): CMap =
    mapOf(maps.foldLeft(CanonicalEntries.emptyIn[CanonicalValue](keys))((acc, m) => acc.concat(m.entries)), keys)

  /**
   * For dilf4s's own fixed-shape records (e.g. trie node commitments): unique ASCII keys known at compile time and
   * shallow values. Null values are still dropped. Never use with external input.
   */
  private[kd5ujc] def trustedMap(entries: List[(String, CanonicalValue)]): CMap =
    mapOf(CanonicalEntries.inserted(entries.filter { case (_, v) => v != CNull }, KeyOrder.Default), KeyOrder.Default)

  /** For the strict decoder, which bounds depth and has verified unique, ordered text keys and non-null values. */
  private[canonical] def decodedArray(values: Vector[CanonicalValue]): CArray = new CArray(values) {}
  private[canonical] def decodedMap(entries:  Vector[(CText, CanonicalValue)], keys: KeyOrder): CMap = ofText(entries, keys)

  private def withinDepth[A <: CanonicalValue](v: A): Either[CanonicalError, A] =
    Either.cond(v.depth <= MaxDepth, v, CanonicalError.DepthExceeded(MaxDepth))

  /**
   * One insertion per entry, in input order: `acc` holds every key so far, null-valued ones too (a repeat of one is
   * still a duplicate), and the weight and depth counts of the rest. The nulls are dropped at the end.
   */
  @tailrec
  private def collect(
    entries: List[(String, CanonicalValue)],
    keys:    KeyOrder,
    acc:     CanonicalEntries[CanonicalValue],
    depths:  TreeMap[Int, Int],
    weight:  Long
  ): Either[CanonicalError, CMap] =
    entries match {
      case Nil => (new MapNode(acc.filter { case (_, v) => v != CNull }, keys, depths, weight): CMap).asRight
      case (k, v) :: rest =>
        val next = acc.updated(k, v)
        if (next.size == acc.size) CanonicalError.DuplicateKey(Preview.of(k)).asLeft
        else {
          val length = keys.measure(k)
          if (length < 0) CanonicalError.UnpairedSurrogate.asLeft
          else if (v == CNull) collect(rest, keys, next, depths, weight)
          else collect(rest, keys, next, counted(depths, v.depth, 1), Weight.add(weight, Weight.add(1L + length, v.weight)))
        }
    }
}

private[canonical] object Weight {

  /** Weights saturate here (`canonical-max-weight`, 2^63 - 1), never wrap. */
  val Max: Long = xyz.kd5ujc.models.limits.CanonicalMaxWeight

  def add(a: Long, b: Long): Long = if (a > Max - b) Max else a + b
}

/** UTF-16 well-formedness: every surrogate is part of a high/low pair. */
private[canonical] object Utf16 {

  def isWellFormed(s: String): Boolean = check(s, 0)

  /** The UTF-8 length of `s`, or -1 when it holds an unpaired surrogate: [[isWellFormed]] and [[utf8Length]] in one pass. */
  def measure(s: String): Long = measured(s, 0, 0L)

  @tailrec
  private def measured(s: String, i: Int, acc: Long): Long =
    if (i >= s.length) acc
    else {
      val c = s.charAt(i)
      if (Character.isHighSurrogate(c))
        if (i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))) measured(s, i + 2, acc + 4) else -1L
      else if (Character.isLowSurrogate(c)) -1L
      else measured(s, i + 1, acc + (if (c < 0x80) 1 else if (c < 0x800) 2 else 3))
    }

  /** The UTF-8 length of well-formed text, without encoding it: a surrogate pair is 4 bytes. */
  def utf8Length(s: String): Long = utf8(s, 0, 0L)

  @tailrec
  private def utf8(s: String, i: Int, acc: Long): Long =
    if (i >= s.length) acc
    else {
      val c = s.charAt(i)
      val n = if (c < 0x80) 1 else if (c < 0x800) 2 else if (Character.isHighSurrogate(c)) 4 else if (Character.isLowSurrogate(c)) 0 else 3
      utf8(s, i + 1, acc + n)
    }

  @tailrec
  private def check(s: String, i: Int): Boolean =
    if (i >= s.length) true
    else {
      val c = s.charAt(i)
      if (Character.isHighSurrogate(c))
        i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)) && check(s, i + 2)
      else !Character.isLowSurrogate(c) && check(s, i + 1)
    }
}
