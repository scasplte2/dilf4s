package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CborSuite extends SimpleIOSuite with Checkers {

  private def txt(s:   String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)
  private def arr(vs:  CanonicalValue*): CanonicalValue = CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)
  private def obj(kvs: (String, CanonicalValue)*): CanonicalValue = CanonicalValue.map(kvs).fold(e => sys.error(e.toString), identity)

  /** RFC 8949 Appendix A examples that fall inside the deterministic profile. */
  private val rfcExamples: List[(String, CanonicalValue, ByteVector)] = List(
    ("0", CInt(0), hex"00"),
    ("1", CInt(1), hex"01"),
    ("10", CInt(10), hex"0a"),
    ("23", CInt(23), hex"17"),
    ("24", CInt(24), hex"1818"),
    ("25", CInt(25), hex"1819"),
    ("100", CInt(100), hex"1864"),
    ("1000", CInt(1000), hex"1903e8"),
    ("1000000", CInt(1000000), hex"1a000f4240"),
    ("1000000000000", CInt(BigInt("1000000000000")), hex"1b000000e8d4a51000"),
    ("2^64-1", CInt(BigInt("18446744073709551615")), hex"1bffffffffffffffff"),
    ("2^64", CInt(BigInt("18446744073709551616")), hex"c249010000000000000000"),
    ("-2^64", CInt(BigInt("-18446744073709551616")), hex"3bffffffffffffffff"),
    ("-2^64-1", CInt(BigInt("-18446744073709551617")), hex"c349010000000000000000"),
    ("-1", CInt(-1), hex"20"),
    ("-10", CInt(-10), hex"29"),
    ("-100", CInt(-100), hex"3863"),
    ("-1000", CInt(-1000), hex"3903e7"),
    ("false", False, hex"f4"),
    ("true", True, hex"f5"),
    ("null", CNull, hex"f6"),
    ("h''", CBytes(ByteVector.empty), hex"40"),
    ("h'01020304'", CBytes(hex"01020304"), hex"4401020304"),
    ("\"\"", txt(""), hex"60"),
    ("\"a\"", txt("a"), hex"6161"),
    ("\"IETF\"", txt("IETF"), hex"6449455446"),
    ("\"\\\"\\\\\"", txt("\"\\"), hex"62225c"),
    ("\"ü\"", txt("ü"), hex"62c3bc"),
    ("\"水\"", txt("水"), hex"63e6b0b4"),
    ("\"𐅑\"", txt("𐅑"), hex"64f0908591"),
    ("[]", EmptyArray, hex"80"),
    ("[1, 2, 3]", arr(CInt(1), CInt(2), CInt(3)), hex"83010203"),
    ("[1, [2, 3], [4, 5]]", arr(CInt(1), arr(CInt(2), CInt(3)), arr(CInt(4), CInt(5))), hex"8301820203820405"),
    ("{}", EmptyMap, hex"a0"),
    ("{\"a\": 1, \"b\": [2, 3]}", obj("a" -> CInt(1), "b" -> arr(CInt(2), CInt(3))), hex"a26161016162820203"),
    ("[\"a\", {\"b\": \"c\"}]", arr(txt("a"), obj("b" -> txt("c"))), hex"826161a161626163")
  )

  rfcExamples.foreach {
    case (name, value, bytes) =>
      pureTest(s"RFC 8949 Appendix A: $name") {
        expect.same(bytes, Cbor.encode(value)).and(expect.same(Right(value), Cbor.decode(bytes)))
      }
  }

  pureTest("rationals are tag 30 over a reduced [numerator, denominator] pair") {
    val half = CanonicalValue.number(xyz.kd5ujc.numerics.Ratio.of(1, 2).toOption.get)
    val negThird = CanonicalValue.number(xyz.kd5ujc.numerics.Ratio.of(-2, 6).toOption.get)
    expect.same(hex"d81e820102", Cbor.encode(half)).and(expect.same(hex"d81e822003", Cbor.encode(negThird)))
  }

  pureTest("map keys order by encoded bytes: shorter keys first, then bytewise") {
    val m = obj("aa" -> CInt(1), "b" -> CInt(2), "a" -> CInt(3), "é" -> CInt(4))
    expect.same(hex"a46161036162026261610162c3a904", Cbor.encode(m))
  }

  pureTest("null map values are dropped, so absent and null hash identically") {
    expect
      .same(Cbor.encode(obj("a" -> CInt(1))), Cbor.encode(obj("a" -> CInt(1), "b" -> CNull)))
      .and(expect.same(Left(DuplicateKey("a")), CanonicalValue.map(List("a" -> CNull, "a" -> CInt(1)))))
  }

  test("decode inverts encode") {
    forall(CanonicalGens.value(4))(v => expect.same(Right(v), Cbor.decode(Cbor.encode(v))))
  }

  test("every accepted encoding is the canonical one") {
    forall(CanonicalGens.value(4)) { v =>
      val bytes = Cbor.encode(v)
      expect.same(Right(bytes), Cbor.decode(bytes).map(Cbor.encode))
    }
  }

  private val rejected: List[(String, ByteVector, CanonicalError => Boolean)] = List(
    ("non-shortest uint", hex"1817", { case NonCanonical(_, _) => true; case _ => false }),
    ("non-shortest length", hex"780161", { case NonCanonical(_, _) => true; case _ => false }),
    ("indefinite array", hex"9f01ff", { case Unsupported(_, _) => true; case _ => false }),
    ("half float", hex"f93c00", { case Unsupported(_, _) => true; case _ => false }),
    ("double", hex"fb3ff0000000000000", { case Unsupported(_, _) => true; case _ => false }),
    ("undefined", hex"f7", { case Unsupported(_, _) => true; case _ => false }),
    ("unknown tag", hex"c11a514b67b0", { case Unsupported(_, _) => true; case _ => false }),
    ("bignum in 64-bit range", hex"c24101", { case NonCanonical(_, _) => true; case _ => false }),
    ("bignum leading zero", hex"c2490001000000000000000000", { case NonCanonical(_, _) => true; case _ => false }),
    ("rational with denominator 1", hex"d81e820101", { case NonCanonical(_, _) => true; case _ => false }),
    ("unreduced rational", hex"d81e820204", { case NonCanonical(_, _) => true; case _ => false }),
    ("unsorted map keys", hex"a2616201616101", { case NonCanonical(_, _) => true; case _ => false }),
    ("duplicate map keys", hex"a2616101616102", { case NonCanonical(_, _) => true; case _ => false }),
    ("null map value", hex"a16161f6", { case NonCanonical(_, _) => true; case _ => false }),
    ("non-text key", hex"a10101", { case Unsupported(_, _) => true; case _ => false }),
    ("surrogate in UTF-8", hex"63eda080", { case InvalidUtf8(_) => true; case _ => false }),
    ("overlong UTF-8", hex"62c0af", { case InvalidUtf8(_) => true; case _ => false }),
    ("trailing bytes", hex"0000", { case TrailingBytes(_) => true; case _ => false }),
    ("truncated", hex"1a0001", { case UnexpectedEnd(_) => true; case _ => false }),
    ("huge claimed length", hex"9bffffffffffffffff", { case UnexpectedEnd(_) => true; case _ => false })
  )

  rejected.foreach {
    case (name, bytes, isExpected) =>
      pureTest(s"rejects non-canonical input: $name") {
        Cbor.decode(bytes) match {
          case Left(e)  => expect(isExpected(e), s"unexpected error $e for ${bytes.toHex}")
          case Right(v) => failure(s"accepted ${bytes.toHex} as $v")
        }
      }
  }

  pureTest("nesting beyond the limit is a typed error at any depth, never a stack overflow") {
    val bomb = ByteVector.fill(200000)(0x81) ++ hex"00"
    val atLimit = ByteVector.fill(CanonicalValue.MaxDepth.toLong)(0x81) ++ hex"00"
    val overLimit = (1 to CanonicalValue.MaxDepth).foldLeft(CanonicalValue.array(Vector(CInt(0)))) { (acc, _) =>
      acc.flatMap(a => CanonicalValue.array(Vector(a)))
    }
    expect
      .same(Left(DepthExceeded(CanonicalValue.MaxDepth)), Cbor.decode(bomb))
      .and(expect(Cbor.decode(atLimit).isRight, "a value exactly at the depth limit decodes"))
      .and(expect.same(Left(DepthExceeded(CanonicalValue.MaxDepth)), overLimit.map(_.depth)))
  }

  pureTest("a caller-supplied depth limit below the maximum is honoured") {
    expect
      .same(Left(DepthExceeded(2)), Cbor.decode(hex"81818100", maxDepth = 2))
      .and(expect(Cbor.decode(hex"818100", maxDepth = 2).isRight, "depth 2 fits a limit of 2"))
  }

  pureTest("the bytewise comparator is unsigned") {
    expect(Cbor.compareBytes(hex"01", hex"ff") < 0, "0x01 < 0xff")
      .and(expect(Cbor.compareBytes(hex"61", hex"6161") < 0, "prefix sorts first"))
      .and(expect.same(0, Cbor.compareBytes(hex"abcd", hex"abcd")))
  }

  pureTest("depth is tracked structurally") {
    expect.same(0, CInt(1).depth).and(expect.same(1, EmptyArray.depth)).and(expect.same(3, arr(obj("k" -> arr(CInt(1)))).depth))
  }

}
