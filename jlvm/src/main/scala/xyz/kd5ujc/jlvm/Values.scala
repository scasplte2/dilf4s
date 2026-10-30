package xyz.kd5ujc.jlvm

import java.util.Locale

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
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

  def number(v: CanonicalValue): Option[Ratio] = v match {
    case CInt(i)      => Some(Ratio.integer(i))
    case CRational(r) => Some(r)
    case _            => None
  }

  def num(r:  Ratio): CanonicalValue = CanonicalValue.number(r)
  def int(i:  BigInt): CanonicalValue = CInt(i)
  def bool(b: Boolean): CanonicalValue = CBool(b)

  def text(op: Op, s: String): Either[JlvmError, CanonicalValue] =
    CanonicalValue.text(s).leftMap(_ => JlvmError.Type(op.wire, "the result is not well-formed text"))

  def array(op: Op, vs: Vector[CanonicalValue]): Either[JlvmError, CanonicalValue] =
    CanonicalValue.array(vs).leftMap(e => JlvmError.Limit(s"${op.wire}: $e"))

  def map(op: Op, entries: Iterable[(String, CanonicalValue)]): Either[JlvmError, CanonicalValue] =
    CanonicalValue.map(entries).leftMap(e => JlvmError.Limit(s"${op.wire}: $e"))

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
      case (CText(x), CText(y)) => compareCodePoints(x, y).asRight
      case _ =>
        (number(a), number(b)) match {
          case (Some(x), Some(y)) => x.compare(y).asRight
          case _                  => JlvmError.Type(op.wire, s"cannot order ${kind(a)} and ${kind(b)}").asLeft
        }
    }

  def compareCodePoints(x: String, y: String): Int = {
    val (a, b) = (x.codePoints().toArray, y.codePoints().toArray)
    a.zip(b).collectFirst { case (p, q) if p != q => Integer.compare(p, q) }.getOrElse(Integer.compare(a.length, b.length))
  }

  def codePoints(s:       String): Vector[Int] = s.codePoints().toArray.toVector
  def fromCodePoints(cps: Vector[Int]): String = new String(cps.toArray, 0, cps.size)

  /** Unicode default full case mapping, locale-independent (docs/specs/jlvm.md). */
  def lower(s: String): String = s.toLowerCase(Locale.ROOT)
  def upper(s: String): String = s.toUpperCase(Locale.ROOT)

  /**
   * The Unicode `White_Space` property (PropList.txt), listed explicitly: Java's `strip` omits no-break spaces, and
   * JavaScript's `trim` adds U+FEFF and omits U+0085, so neither is a portable definition.
   */
  val WhiteSpace: Set[Int] =
    Set(0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x20, 0x85, 0xa0, 0x1680, 0x2028, 0x2029, 0x202f, 0x205f, 0x3000) ++ (0x2000 to 0x200a)

  /** Trims Unicode `White_Space` from both ends. */
  def trim(s: String): String =
    fromCodePoints(codePoints(s).dropWhile(WhiteSpace).reverse.dropWhile(WhiteSpace).reverse)

  /** Map keys in Unicode code-point order (the order of `keys`, `values` and `entries`). */
  def sortedKeys(m: scala.collection.immutable.Map[String, CanonicalValue]): List[String] =
    m.keys.toList.sortWith((a, b) => compareCodePoints(a, b) < 0)

  /**
   * Number size in 64-bit limbs, for gas: numerator plus denominator for a rational, at least 1. The size of a value
   * is the sum of its parts.
   */
  def limbs(v: CanonicalValue): Long = v match {
    case CInt(i)      => 1L + i.bitLength / 64
    case CRational(r) => 2L + r.numerator.bitLength / 64 + r.denominator.bitLength / 64
    case _            => 1L
  }

  def bytesOf(v: CanonicalValue): Long = v match {
    case CText(s)   => s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length.toLong
    case CBytes(b)  => b.size
    case CArray(vs) => vs.foldLeft(0L)(_ + bytesOf(_))
    case _          => 0L
  }

  def elements(v: CanonicalValue): Long = v match {
    case CArray(vs) => vs.size.toLong
    case CMap(m)    => m.size.toLong
    case _          => 1L
  }

  def bytesValue(b: ByteVector): CanonicalValue = CBytes(b)
}
