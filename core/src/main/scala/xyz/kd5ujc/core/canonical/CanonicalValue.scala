package xyz.kd5ujc.core.canonical

import cats.Show
import cats.syntax.either._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * The canonical data model (docs/specs/canonical-encoding.md §1). Invariants are enforced by the smart constructors in
 * the companion: rationals are never integers, text holds no unpaired surrogates, maps hold unique keys and no null
 * values, and nesting never exceeds [[CanonicalValue.MaxDepth]] — so every constructible value is decodable.
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
}

object CanonicalValue {

  val MaxDepth: Int = 256

  implicit val canonicalValueShow: Show[CanonicalValue] = Show.fromToString

  sealed trait Scalar extends CanonicalValue {
    final val depth: Int = 0
  }

  case object CNull extends Scalar {
    final val weight: Long = 1L
  }

  final case class CBool(value: Boolean) extends Scalar {
    final val weight: Long = 1L
  }

  final case class CInt(value: BigInt) extends Scalar {
    final val weight: Long = 1L + value.bitLength / 64
  }

  sealed abstract case class CRational(value: Ratio) extends Scalar {
    final val weight: Long = 2L + value.numerator.bitLength / 64 + value.denominator.bitLength / 64
  }

  sealed abstract case class CText(value: String) extends Scalar {
    final val weight: Long = 1L + Utf16.utf8Length(value)
  }

  final case class CBytes(value: ByteVector) extends Scalar {
    final val weight: Long = 1L + value.size
  }

  sealed abstract case class CArray(values: Vector[CanonicalValue]) extends CanonicalValue {
    final val depth: Int = 1 + values.foldLeft(0)((d, v) => math.max(d, v.depth))
    final val weight: Long = values.foldLeft(1L)((w, v) => Weight.add(w, v.weight))
  }

  /**
   * Besides its entries, a map keeps how many of its values have each depth, and its weight: [[updated]] and [[removed]]
   * then change one entry in O(log n) instead of a pass over all of them. Equality is on the entries alone.
   */
  sealed abstract case class CMap(entries: Map[String, CanonicalValue]) extends CanonicalValue {
    private[canonical] def childDepths: TreeMap[Int, Int]
  }

  final private class MapNode(entries: Map[String, CanonicalValue], val childDepths: TreeMap[Int, Int], val weight: Long)
      extends CMap(entries) {
    val depth: Int = 1 + childDepths.lastOption.fold(0)(_._1)
  }

  private def entryWeight(key: String, value: CanonicalValue): Long = Weight.add(1L + Utf16.utf8Length(key), value.weight)

  private def counted(h: TreeMap[Int, Int], depth: Int, by: Int): TreeMap[Int, Int] = {
    val n = h.getOrElse(depth, 0) + by
    if (n <= 0) h - depth else h.updated(depth, n)
  }

  /** A map from its entries: one pass for the depth counts and the weight. */
  private def mapOf(entries: Map[String, CanonicalValue]): CMap =
    new MapNode(
      entries,
      entries.valuesIterator.foldLeft(TreeMap.empty[Int, Int])((h, v) => counted(h, v.depth, 1)),
      entries.foldLeft(1L) { case (w, (k, v)) => Weight.add(w, entryWeight(k, v)) }
    )

  /** `m` with entry `key` changed from `before` to `after`; a saturated weight is recounted. */
  private def changed(m: CMap, key: String, before: Option[CanonicalValue], after: Option[CanonicalValue]): CMap = {
    val entries = after.fold(m.entries - key)(m.entries.updated(key, _))
    val without = before.fold(m.childDepths)(v => counted(m.childDepths, v.depth, -1))
    val depths = after.fold(without)(v => counted(without, v.depth, 1))
    val weight =
      if (m.weight == Long.MaxValue) entries.foldLeft(1L) { case (w, (k, v)) => Weight.add(w, entryWeight(k, v)) }
      else Weight.add(m.weight - before.fold(0L)(entryWeight(key, _)), after.fold(0L)(entryWeight(key, _)))
    new MapNode(entries, depths, weight)
  }

  val True: CanonicalValue = CBool(true)
  val False: CanonicalValue = CBool(false)
  val EmptyArray: CArray = new CArray(Vector.empty) {}
  val EmptyMap: CMap = mapOf(Map.empty)

  def int(value: BigInt): CanonicalValue = CInt(value)

  /** A number in the single numeric domain: integral values are always [[CInt]]. */
  def number(value: Ratio): CanonicalValue =
    value.toBigIntExact.fold[CanonicalValue](new CRational(value) {})(CInt(_))

  def text(value: String): Either[CanonicalError, CText] =
    Either.cond(Utf16.isWellFormed(value), new CText(value) {}, CanonicalError.UnpairedSurrogate(value))

  def bytes(value: ByteVector): CanonicalValue = CBytes(value)

  def array(values: Vector[CanonicalValue]): Either[CanonicalError, CArray] =
    withinDepth(new CArray(values) {})

  /** Builds a map, dropping null-valued entries; invalid or duplicate keys are rejected. */
  def map(entries: Iterable[(String, CanonicalValue)]): Either[CanonicalError, CMap] =
    collect(entries.toList, Set.empty, Map.empty).flatMap(m => withinDepth(mapOf(m)))

  /**
   * `m` with `key` set to `value`, or removed when `value` is null, in O(log n). The key is already well-formed text, so
   * only depth is checked: no existing key is revalidated.
   */
  def updated(m: CMap, key: CText, value: CanonicalValue): Either[CanonicalError, CMap] =
    if (value == CNull) removed(m, key).asRight else withinDepth(changed(m, key.value, m.entries.get(key.value), Option(value)))

  /** `m` without `key`, in O(log n). */
  def removed(m: CMap, key: CText): CMap = m.entries.get(key.value).fold(m)(v => changed(m, key.value, Option(v), scala.None))

  /** Maps merged left to right, later keys winning; every key is already valid, and depth cannot grow. */
  def merged(maps: Vector[CMap]): CMap = mapOf(maps.foldLeft(Map.empty[String, CanonicalValue])(_ ++ _.entries))

  /**
   * For dilf4s's own fixed-shape records (e.g. trie node commitments): distinct ASCII keys known at compile time and
   * shallow values. Null values are still dropped. Never use with external input.
   */
  private[kd5ujc] def trustedMap(entries: List[(String, CanonicalValue)]): CMap =
    mapOf(entries.filter { case (_, v) => v != CNull }.toMap)

  /** For the strict decoder, which bounds depth and has verified unique, ordered text keys and non-null values. */
  private[canonical] def decodedArray(values: Vector[CanonicalValue]): CArray = new CArray(values) {}
  private[canonical] def decodedMap(entries:  Vector[(String, CanonicalValue)]): CMap = mapOf(entries.toMap)

  private def withinDepth[A <: CanonicalValue](v: A): Either[CanonicalError, A] =
    Either.cond(v.depth <= MaxDepth, v, CanonicalError.DepthExceeded(MaxDepth))

  @tailrec
  private def collect(
    entries: List[(String, CanonicalValue)],
    seen:    Set[String],
    acc:     Map[String, CanonicalValue]
  ): Either[CanonicalError, Map[String, CanonicalValue]] =
    entries match {
      case Nil                                   => acc.asRight
      case (k, _) :: _ if seen.contains(k)       => CanonicalError.DuplicateKey(k).asLeft
      case (k, _) :: _ if !Utf16.isWellFormed(k) => CanonicalError.UnpairedSurrogate(k).asLeft
      case (k, CNull) :: rest                    => collect(rest, seen + k, acc)
      case (k, v) :: rest                        => collect(rest, seen + k, acc.updated(k, v))
    }
}

private[canonical] object Weight {
  def add(a: Long, b: Long): Long = if (a > Long.MaxValue - b) Long.MaxValue else a + b
}

/** UTF-16 well-formedness: every surrogate is part of a high/low pair. */
private[canonical] object Utf16 {

  def isWellFormed(s: String): Boolean = check(s, 0)

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
