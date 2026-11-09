package xyz.kd5ujc.dilf4s.internal

import cats.syntax.all._

import scala.jdk.CollectionConverters._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.dilf4s.Result
import xyz.kd5ujc.dilf4s.error.{CanonicalError, EncodeError, RenderError, TooLarge, TypeMismatch}
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * The canonical data model for the Java API: the reference's smart constructors, accessors, encoder, decoder and JSON
 * mapping, with Java types in and out. Every byte string in is copied (`ByteVector(array)`), and every one out is a
 * fresh array (`toArray`), so no caller array aliases a value. Pure and total.
 */
object Values {

  /** `e` as a Java result. */
  def result[E, A](e: Either[E, A]): Result[A, E] = e.fold(x => new Result.Err[A, E](x), a => new Result.Ok[A, E](a))

  private def canonical[A](e: Either[xyz.kd5ujc.core.canonical.CanonicalError, A]): Result[A, CanonicalError] =
    result(e.left.map(Errors.canonicalError))

  def visit[R](v: CanonicalValue, k: ValueCases[R]): R =
    v match {
      case CNull        => k.nul()
      case b: CBool     => k.bool(b)
      case i: CInt      => k.integer(i)
      case r: CRational => k.rational(r)
      case t: CText     => k.text(t)
      case b: CBytes    => k.bytes(b)
      case a: CArray    => k.array(a)
      case m: CMap      => k.map(m)
    }

  // ---- accessors

  def boolOf(b:      CBool): Boolean = b.value
  def intOf(i:       CInt): java.math.BigInteger = i.value.bigInteger
  def numerator(r:   CRational): java.math.BigInteger = r.value.numerator.bigInteger
  def denominator(r: CRational): java.math.BigInteger = r.value.denominator.bigInteger
  def textOf(t:      CText): String = t.value
  def bytesOf(b:     CBytes): Array[Byte] = b.value.toArray
  def size(b:        CBytes): Long = b.value.size
  def size(a:        CArray): Int = a.values.size
  def size(m:        CMap): Int = m.entries.size

  def item(a: CArray, i: Int): java.util.Optional[CanonicalValue] =
    a.values.lift(i).fold(java.util.Optional.empty[CanonicalValue]())(java.util.Optional.of)

  def items(a: CArray): java.util.List[CanonicalValue] = a.values.asJava

  def get(m: CMap, key: String): java.util.Optional[CanonicalValue] =
    m.entries.get(key).fold(java.util.Optional.empty[CanonicalValue]())(java.util.Optional.of)

  /** The keys in the map's order: UTF-16 code unit order, `String.compareTo`. */
  def keys(m: CMap): java.util.List[String] = m.entries.keys.asJava

  def entries(m: CMap): java.util.List[java.util.Map.Entry[String, CanonicalValue]] =
    m.entries.toVector.map { case (k, v) => java.util.Map.entry(k, v) }.asJava

  def weight(v:  CanonicalValue): Long = v.weight
  def depth(v:   CanonicalValue): Int = v.depth
  def inspect(v: CanonicalValue): String = CanonicalValue.inspect(v)
  def same(a:    CanonicalValue, b: CanonicalValue): Boolean = a == b
  def hash(v:    CanonicalValue): Int = v.hashCode

  // ---- constructors

  val nul: CanonicalValue = CNull
  def ofBoolean(b: Boolean): CBool = CBool(b)
  def ofLong(l:    Long): CInt = CInt(l)
  def bytes(a:     Array[Byte]): CBytes = CBytes(ByteVector(a))

  def ofBigInteger(i: java.math.BigInteger): Result[CInt, CanonicalError] = canonical(CanonicalValue.int(BigInt(i)))

  /** `numerator / denominator` in the one numeric domain: an integer when it is integral; a zero denominator is refused. */
  def number(numerator: java.math.BigInteger, denominator: java.math.BigInteger): Result[CanonicalValue, CanonicalError] =
    Ratio
      .of(BigInt(numerator), BigInt(denominator))
      .fold(
        e => new Result.Err[CanonicalValue, CanonicalError](new TypeMismatch(s"not a rational: $e")),
        r => canonical(CanonicalValue.number(r))
      )

  def text(s: String): Result[CText, CanonicalError] = canonical(CanonicalValue.text(s))

  def array(items: java.util.List[CanonicalValue]): Result[CArray, CanonicalError] =
    canonical(CanonicalValue.array(items.asScala.toVector))

  /** A map from entries in the caller's order: a repeated key is refused, the first in that order; nulls are dropped. */
  def map(entries: java.util.List[java.util.Map.Entry[String, CanonicalValue]]): Result[CMap, CanonicalError] =
    canonical(CanonicalValue.map(entries.asScala.map(e => e.getKey -> e.getValue)))

  def updated(m: CMap, key: String, value: CanonicalValue): Result[CMap, CanonicalError] =
    canonical(CanonicalValue.text(key).flatMap(CanonicalValue.updated(m, _, value)))

  def removed(m: CMap, key: String): Result[CMap, CanonicalError] =
    canonical(CanonicalValue.text(key).map(CanonicalValue.removed(m, _)))

  // ---- CBOR and JSON

  private def tooLarge(e: Cbor.TooLarge): EncodeError = new TooLarge(e.limit)

  def encode(v: CanonicalValue): Result[Array[Byte], EncodeError] = result(Cbor.encode(v).bimap(tooLarge, _.toArray))

  def encodeWithin(v: CanonicalValue, maxBytes: Long): Result[Array[Byte], EncodeError] =
    result(Cbor.encodeWithin(v, maxBytes).bimap(tooLarge, _.toArray))

  def encodedLengthWithin(v: CanonicalValue, maxBytes: Long): Result[java.lang.Long, EncodeError] =
    result(Cbor.encodedLengthWithin(v, maxBytes).bimap(tooLarge, java.lang.Long.valueOf))

  def decode(bytes: Array[Byte], maxDepth: Int): Result[CanonicalValue, CanonicalError] =
    canonical(Cbor.decode(ByteVector(bytes), maxDepth))

  def parse(text: String, maxScale: Int): Result[CanonicalValue, CanonicalError] = canonical(CanonicalJson.parse(text, maxScale))

  private def rendered(e: CanonicalJson.RenderError): RenderError =
    e match {
      case CanonicalJson.RenderError.Canonical(c) => new RenderError.Canonical(Errors.canonicalError(c))
      case CanonicalJson.RenderError.TooLarge(l)  => new TooLarge(l)
    }

  def render(v: CanonicalValue, maxBytes: Long): Result[String, RenderError] = result(
    CanonicalJson.renderWithin(v, maxBytes).left.map(rendered)
  )

  val MaxDepth: Int = CanonicalValue.MaxDepth
  val MaxIntegerBits: Int = CanonicalValue.MaxIntegerBits
  val MaxRationalBits: Int = CanonicalValue.MaxRationalBits
  val MaxEncodedBytes: Long = Cbor.MaxEncodedBytes
  val MaxRenderBytes: Long = CanonicalJson.MaxRenderBytes
  val MaxScale: Int = CanonicalJson.MaxScale
  val MaxDigits: Int = CanonicalJson.MaxDigits
}
