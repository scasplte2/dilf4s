package xyz.kd5ujc.accumulators

import cats.Show

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ValueCommitmentSuite extends SimpleIOSuite with Checkers {

  private val value: Gen[CanonicalValue] = Gen.oneOf(
    Gen.alphaNumStr.map(s => CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)),
    Gen.long.map(l => CanonicalValue.CInt(l)),
    Gen.oneOf(CanonicalValue.True, CanonicalValue.False, CanonicalValue.CNull),
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => CanonicalValue.CBytes(ByteVector(bs)))
  )

  private given Show[CanonicalValue] = Show.fromToString

  pureTest("bytes and a value whose CBOR equals them commit differently: 0x6161 is not \"a\"") {
    val a = CanonicalValue.text("a").fold(e => sys.error(e.toString), identity)
    expect
      .same(ByteVector(0x61, 0x61), xyz.kd5ujc.core.canonical.CanonicalGens.encoded(a))
      .and(expect(ValueCommitment.bytes(CanonicalValue.CBytes(ByteVector(0x61, 0x61))) != ValueCommitment.bytes(a), "tagged apart"))
  }

  test("for any value v, the bytes cbor(v) commit differently from v") {
    forall(value) { v =>
      val asBytes = CanonicalValue.CBytes(xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v))
      expect(v == asBytes || ValueCommitment.bytes(v) != ValueCommitment.bytes(asBytes), s"$v collides with its encoding")
    }
  }

  pureTest("a commitment of exactly the limit, tag included, is built; one byte under it is TooLarge with the limit") {
    val t = CanonicalValue.text("abc").fold(e => sys.error(e.toString), identity)
    val b = CanonicalValue.CBytes(ByteVector(1, 2, 3))
    val heavy =
      (1 to 70).foldLeft[CanonicalValue](t)((x, _) => CanonicalValue.array(Vector(x, x)).fold(e => sys.error(e.toString), identity))
    expect
      .same(Right(ByteVector(0x01, 0x63, 0x61, 0x62, 0x63)), ValueCommitment.bytesWithin(t, 5L))
      .and(expect.same(Left(Cbor.TooLarge(4L)), ValueCommitment.bytesWithin(t, 4L)))
      .and(expect.same(Right(ByteVector(0x00, 1, 2, 3)), ValueCommitment.bytesWithin(b, 4L)))
      .and(expect.same(Left(Cbor.TooLarge(3L)), ValueCommitment.bytesWithin(b, 3L)))
      .and(expect.same(Left(Cbor.TooLarge(0L)), ValueCommitment.bytesWithin(t, -1L)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), ValueCommitment.bytes(heavy)))
      .and(expect.same(Right(ByteVector(0x01, 0xf6)), ValueCommitment.bytesWithin(CanonicalValue.CNull, Long.MaxValue)))
  }

  test("the commitment is injective on distinct values") {
    forall(Gen.zip(value, value)) {
      case (a, b) => expect((a == b) == (ValueCommitment.bytes(a) == ValueCommitment.bytes(b)))
    }
  }
}
