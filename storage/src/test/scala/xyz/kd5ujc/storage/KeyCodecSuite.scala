package xyz.kd5ujc.storage

import xyz.kd5ujc.storage.ByteOrder.unsigned

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object KeyCodecSuite extends SimpleIOSuite with Checkers {

  private def preservesOrder[K](gen: Gen[K])(implicit K: KeyCodec[K], O: Ordering[K], S: cats.Show[K]) =
    forall(Gen.zip(gen, gen)) {
      case (a, b) =>
        val byKey = math.signum(O.compare(a, b))
        val byBytes = math.signum(unsigned.compare(K.encode(a), K.encode(b)))
        expect(byKey == byBytes, s"order of $a and $b: key says $byKey, bytes say $byBytes").and(
          expect.same(Right(a), K.decode(K.encode(a)))
        )
    }

  private val bytes: Gen[ByteVector] =
    Gen.listOf(Gen.oneOf(Gen.const(0.toByte), Gen.const(0xff.toByte), Gen.choose(Byte.MinValue, Byte.MaxValue))).map(ByteVector(_))
  implicit private val showBytes: cats.Show[ByteVector] = cats.Show.show(_.toHex)

  test("Long keys preserve order across the sign boundary")(
    preservesOrder(Gen.oneOf(Gen.long, Gen.oneOf(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)))
  )

  test("Int keys preserve order")(
    preservesOrder(Gen.oneOf(Gen.choose(Int.MinValue, Int.MaxValue), Gen.oneOf(Int.MinValue, -1, 0, 1, Int.MaxValue)))
  )

  test("String keys preserve code-point order") {
    val codePoint: Ordering[String] = (a, b) => unsigned.compare(ByteVector(a.getBytes("UTF-8")), ByteVector(b.getBytes("UTF-8")))
    val str = Gen.listOf(Gen.oneOf("a", "z", "é", "中", "😀", "￿", "\u0000")).map(_.mkString)
    forall(Gen.zip(str, str)) {
      case (a, b) =>
        val cp = math.signum(
          a.codePoints.toArray.toList
            .zipAll(b.codePoints.toArray.toList, -1, -1)
            .collectFirst { case (x, y) if x != y => x.compare(y) }
            .getOrElse(0)
        )
        expect
          .same(cp, math.signum(codePoint.compare(a, b)))
          .and(expect.same(Right(a), KeyCodec[String].decode(KeyCodec[String].encode(a))))
    }
  }

  test("terminated encoding round-trips and preserves order") {
    forall(Gen.zip(bytes, bytes)) {
      case (a, b) =>
        val (ea, eb) = (KeyCodec.Terminated.encode(a), KeyCodec.Terminated.encode(b))
        expect
          .same(Right((a, ByteVector.empty)), KeyCodec.Terminated.decode(ea))
          .and(expect(math.signum(unsigned.compare(a, b)) == math.signum(unsigned.compare(ea, eb)), s"order of ${a.toHex} and ${b.toHex}"))
    }
  }

  test("terminated encodings are prefix-free, so a suffix never changes the first component") {
    forall(Gen.zip(bytes, bytes, bytes)) {
      case (a, b, suffix) =>
        val ea = KeyCodec.Terminated.encode(a)
        expect
          .same(Right((a, suffix)), KeyCodec.Terminated.decode(ea ++ suffix))
          .and(expect(a == b || !KeyCodec.Terminated.encode(b).startsWith(ea), "one encoding is never a prefix of another"))
    }
  }

  test("composite keys led by a fixed-width component order lexicographically") {
    forall(Gen.zip(Gen.long, Gen.alphaStr, Gen.long, Gen.alphaStr)) {
      case (a1, a2, b1, b2) =>
        val K = KeyCodec[(Long, String)]
        val expected = math.signum(Ordering[(Long, String)].compare((a1, a2), (b1, b2)))
        expect
          .same(expected, math.signum(unsigned.compare(K.encode((a1, a2)), K.encode((b1, b2)))))
          .and(expect.same(Right((a1, a2)), K.decode(K.encode((a1, a2)))))
    }
  }

  pureTest("prefix successor handles 0xff runs") {
    expect
      .same(Some(ByteVector(0x02)), ByteRange.successor(ByteVector(0x01, 0xff)))
      .and(expect.same(None, ByteRange.successor(ByteVector(0xff, 0xff))))
      .and(expect.same(Some(ByteVector(0x01, 0x03)), ByteRange.successor(ByteVector(0x01, 0x02))))
  }

  pureTest("namespaces are length-prefixed and bounded") {
    expect(Namespace("a").map(_.prefix) != Namespace("ab").map(_.prefix).map(_.take(2)), "'a' is not a prefix of 'ab'")
      .and(expect(Namespace("").isLeft, "empty names are rejected"))
      .and(expect(Namespace("x" * 256).isLeft, "names over 255 bytes are rejected"))
  }
}
