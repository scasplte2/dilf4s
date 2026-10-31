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
    Gen.long.map(l => CanonicalValue.CInt(BigInt(l))),
    Gen.oneOf(CanonicalValue.True, CanonicalValue.False, CanonicalValue.CNull),
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => CanonicalValue.CBytes(ByteVector(bs)))
  )

  private given Show[CanonicalValue] = Show.fromToString

  pureTest("bytes and a value whose CBOR equals them commit differently: 0x6161 is not \"a\"") {
    val a = CanonicalValue.text("a").fold(e => sys.error(e.toString), identity)
    expect
      .same(ByteVector(0x61, 0x61), Cbor.encode(a))
      .and(expect(ValueCommitment.bytes(CanonicalValue.CBytes(ByteVector(0x61, 0x61))) != ValueCommitment.bytes(a), "tagged apart"))
  }

  test("for any value v, the bytes cbor(v) commit differently from v") {
    forall(value) { v =>
      val asBytes = CanonicalValue.CBytes(Cbor.encode(v))
      expect(v == asBytes || ValueCommitment.bytes(v) != ValueCommitment.bytes(asBytes), s"$v collides with its encoding")
    }
  }

  test("the commitment is injective on distinct values") {
    forall(Gen.zip(value, value)) {
      case (a, b) => expect((a == b) == (ValueCommitment.bytes(a) == ValueCommitment.bytes(b)))
    }
  }
}
