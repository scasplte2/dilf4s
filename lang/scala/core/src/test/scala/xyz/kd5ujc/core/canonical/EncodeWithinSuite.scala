package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/** `Cbor.encodeWithin` and `Cbor.encode` at their limits (docs/specs/conformance.md §4). */
object EncodeWithinSuite extends SimpleIOSuite with Checkers {

  private def txt(s:  String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)
  private def arr(vs: CanonicalValue*): CanonicalValue = CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)

  /** `[x, x]` nested `k` deep over `leaf`: weight about `2^k`, a few hundred bytes in memory. */
  private def doubled(leaf: CanonicalValue, k: Int): CanonicalValue = (1 to k).foldLeft(leaf)((x, _) => arr(x, x))

  /** Counts what reaches it: the bytes, and the calls. */
  final private case class Count(bytes: Long, calls: Long)

  private val counting: ByteSink[Count] = new ByteSink[Count] {
    def byte(s:  Count, b: Int): Count = Count(s.bytes + 1, s.calls + 1)
    def bytes(s: Count, b: FlatBytes): Count = Count(s.bytes + b.size, s.calls + 1)
  }

  private def counted(v: CanonicalValue, maxBytes: Long): Either[Cbor.TooLarge, Count] =
    Cbor.encodeInto(v, maxBytes, Count(0L, 0L))(using counting)

  // every kind of node, a text past ASCII, a wide integer and a rational, a map with keys to sort
  private val sample: CanonicalValue = arr(
    CNull,
    True,
    CanonicalGens.int(BigInt(2).pow(70)),
    CanonicalValue
      .number(xyz.kd5ujc.numerics.Ratio.of(BigInt(-7), BigInt(3)).fold(e => sys.error(e.toString), identity))
      .fold(e => sys.error(e.toString), identity),
    txt("é😀\u0000"),
    CBytes(ByteVector.fill(300)(7)),
    CanonicalValue.map(List("bb" -> CInt(1), "a" -> txt("x"))).fold(e => sys.error(e.toString), identity)
  )

  pureTest("an encoding of exactly the limit is written; one byte under it is TooLarge with the limit") {
    val bytes = CanonicalGens.encoded(sample)
    expect
      .same(Right(bytes), Cbor.encodeWithin(sample, bytes.size))
      .and(expect.same(Left(Cbor.TooLarge(bytes.size - 1)), Cbor.encodeWithin(sample, bytes.size - 1)))
      .and(expect.same(Right(bytes.size), Cbor.encodedLengthWithin(sample, bytes.size)))
      .and(expect.same(Left(Cbor.TooLarge(bytes.size - 1)), Cbor.encodedLengthWithin(sample, bytes.size - 1)))
  }

  pureTest("a limit is applied from 0 to the ceiling: a negative one is 0, and none is above MaxEncodedBytes") {
    expect
      .same(Left(Cbor.TooLarge(0L)), Cbor.encodeWithin(CNull, -1L))
      .and(expect.same(Left(Cbor.TooLarge(0L)), Cbor.encodeWithin(CNull, 0L)))
      .and(expect.same(Right(ByteVector(0xf6)), Cbor.encodeWithin(CNull, Long.MaxValue)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), Cbor.encodeWithin(doubled(CNull, 62), Long.MaxValue)))
  }

  pureTest("the ceiling is the JVM's largest array, 2^31 - 9 bytes") {
    expect.same(Int.MaxValue.toLong - 8, Cbor.MaxEncodedBytes)
  }

  pureTest("encode refuses a value heavier than the ceiling as TooLarge, from its weight, before writing anything") {
    // weight 2^63 - 1 (saturated): encoding it would take longer than the universe has left, and the refusal is O(1)
    val heavy = doubled(txt("abc"), 70)
    expect
      .same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), Cbor.encode(heavy))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), counted(heavy, Cbor.MaxEncodedBytes)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), Cbor.encodedLengthWithin(heavy, Long.MaxValue)))
  }

  pureTest("a refusal writes nothing, by weight or measured; an encoding within the limit is written once, whole") {
    // 2^32 - 1 weighs 1 and encodes to 5 bytes; a 1000-byte text weighs 1001 and encodes to 1003
    val wide = CInt((1L << 32) - 1)
    val long = txt("a" * 1000)
    expect
      .same(Left(Cbor.TooLarge(4L)), counted(wide, 4L))
      .and(expect.same(Right(Count(5L, 5L)), counted(wide, 5L)))
      .and(expect.same(Left(Cbor.TooLarge(1002L)), counted(long, 1002L)))
      .and(expect.same(Left(Cbor.TooLarge(1000L)), counted(long, 1000L)))
      .and(expect.same(Right(1003L), counted(long, 1003L).map(_.bytes)))
      .and(expect.same(Left(Cbor.TooLarge(10L)), counted(sample, 10L)))
  }

  pureTest("a host's lazy byte string of Long.MaxValue bytes weighs Long.MaxValue, and is refused before anything is written") {
    // its weight would wrap to Long.MinValue unsaturated, and pass every budget
    val lazyBytes = CBytes(ByteVector.low(Long.MaxValue))
    expect
      .same(Long.MaxValue, lazyBytes.weight)
      .and(expect.same(Left(Cbor.TooLarge(0L)), Cbor.encodeWithin(lazyBytes, 0L)))
      .and(expect.same(Left(Cbor.TooLarge(0L)), counted(lazyBytes, 0L)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), Cbor.encode(lazyBytes)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), Cbor.encode(arr(CNull, lazyBytes))))
      .and(expect.same(Left(CanonicalJson.RenderError.TooLarge(100L)), CanonicalJson.renderSurfaceWithin(lazyBytes, 100L)))
  }

  pureTest("a byte string past the limit is never appended, however short the rest") {
    val trailing = arr(CNull, CBytes(ByteVector.fill(1 << 20)(1)))
    expect
      .same(Left(Cbor.TooLarge(1000L)), counted(trailing, 1000L))
      .and(expect.same(Right(Count(7L + (1 << 20), 8L)), counted(trailing, 7L + (1 << 20))))
  }

  /** Every canonical encoding is at most `EncodingPerWeight` bytes per unit of weight, so a light value is never measured. */
  test("an encoding is at most EncodingPerWeight bytes per unit of its weight") {
    forall(CanonicalGens.value(3)) { v =>
      val length = CanonicalGens.encoded(v).size
      expect(length <= Cbor.EncodingPerWeight * v.weight, s"$length bytes for weight ${v.weight}")
    }
  }

  pureTest("the densest values are within EncodingPerWeight: the widest integer and rational, and short texts") {
    val widest = BigInt(2).pow(CanonicalValue.MaxIntegerBits) - 1
    val dense = List(
      "the widest integer"          -> CanonicalGens.int(widest),
      "the widest negative integer" -> CanonicalGens.int(-widest - 1),
      "a 65-bit integer"            -> CanonicalGens.int(BigInt(2).pow(64)),
      "a 64-bit integer"            -> CanonicalGens.int(BigInt(2).pow(64) - 1),
      "the widest rational" -> CanonicalValue
        .number(xyz.kd5ujc.numerics.Ratio.of(widest, widest - 1).fold(e => sys.error(e.toString), identity))
        .fold(e => sys.error(e.toString), identity),
      "2^32, a 9-byte head" -> CInt(1L << 32),
      "a 4-byte character"  -> txt("😀"),
      "an empty map"        -> EmptyMap
    )
    dense.foldLeft(success) {
      case (acc, (name, v)) =>
        val length = CanonicalGens.encoded(v).size
        acc.and(expect(length <= Cbor.EncodingPerWeight * v.weight, s"$name: $length bytes for weight ${v.weight}"))
    }
  }
}
