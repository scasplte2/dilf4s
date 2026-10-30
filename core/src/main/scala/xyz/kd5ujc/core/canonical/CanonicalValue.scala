package xyz.kd5ujc.core.canonical

import cats.Show
import cats.syntax.either._

import scala.annotation.tailrec

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

  sealed abstract case class CMap(entries: Map[String, CanonicalValue]) extends CanonicalValue {
    final val depth: Int = 1 + entries.foldLeft(0) { case (d, (_, v)) => math.max(d, v.depth) }
    final val weight: Long =
      entries.foldLeft(1L) { case (w, (k, v)) => Weight.add(Weight.add(w, 1L + Utf16.utf8Length(k)), v.weight) }
  }

  val True: CanonicalValue = CBool(true)
  val False: CanonicalValue = CBool(false)
  val EmptyArray: CArray = new CArray(Vector.empty) {}
  val EmptyMap: CMap = new CMap(Map.empty) {}

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
    collect(entries.toList, Set.empty, Map.empty).flatMap(m => withinDepth(new CMap(m) {}))

  /**
   * `m` with `key` set to `value`, or removed when `value` is null. The key is already well-formed text, so only depth is
   * checked: no existing key is revalidated.
   */
  def updated(m: CMap, key: CText, value: CanonicalValue): Either[CanonicalError, CMap] =
    if (value == CNull) removed(m, key).asRight else withinDepth(new CMap(m.entries.updated(key.value, value)) {})

  def removed(m: CMap, key: CText): CMap = new CMap(m.entries - key.value) {}

  /** Maps merged left to right, later keys winning; every key is already valid, and depth cannot grow. */
  def merged(maps: Vector[CMap]): CMap = new CMap(maps.foldLeft(Map.empty[String, CanonicalValue])(_ ++ _.entries)) {}

  /**
   * For dilf4s's own fixed-shape records (e.g. trie node commitments): distinct ASCII keys known at compile time and
   * shallow values. Null values are still dropped. Never use with external input.
   */
  private[kd5ujc] def trustedMap(entries: List[(String, CanonicalValue)]): CMap =
    new CMap(entries.filter { case (_, v) => v != CNull }.toMap) {}

  /** For the strict decoder, which bounds depth and has verified unique, ordered text keys and non-null values. */
  private[canonical] def decodedArray(values: Vector[CanonicalValue]): CArray = new CArray(values) {}
  private[canonical] def decodedMap(entries:  Vector[(String, CanonicalValue)]): CMap = new CMap(entries.toMap) {}

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
