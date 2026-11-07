package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CanonicalJsonSuite extends SimpleIOSuite with Checkers {

  private def ratio(n: Int, d: Int): CanonicalValue =
    CanonicalValue.number(Ratio.of(n, d).toOption.get).fold(e => sys.error(e.toString), identity)
  private def parse(s: String) = CanonicalJson.parse(s)

  pureTest("a refused rational is named by its size, never its digits") {
    def rational(n: BigInt, d: BigInt) =
      Ratio.of(n, d).fold(e => sys.error(e.toString), r => CanonicalValue.number(r).fold(e => sys.error(e.toString), identity))
    val third = rational(BigInt(1) << 30000, BigInt(3))
    val refusal = NotJsonRepresentable("non-terminating rational")
    // a rational that wide is no value (CanonicalValue.MaxRationalBits); the widest integer is, and renders
    val huge = CanonicalGens.int((BigInt(1) << CanonicalValue.MaxIntegerBits) - 1)
    expect
      .same(
        Left(RationalTooWide(CanonicalValue.MaxRationalBits)),
        Ratio.of(BigInt(1) << 1000000, BigInt(3)).toOption.map(CanonicalValue.number).get
      )
      .and(expect.same(Left(CanonicalJson.RenderError.Canonical(refusal)), CanonicalJson.render(third)))
      .and(expect.same(Right(huge.value.toString), CanonicalJson.render(huge)))
  }

  pureTest("numbers are read and written within 10,000 significant digits, counted before any conversion") {
    def digits(n: Int) = "9" * n
    val ones = "1" * 10000
    expect
      .same(Right(CanonicalGens.int(BigInt(ones))), parse(ones))
      .and(expect.same(Right(ones), parse(ones).flatMap(CanonicalJson.render)))
      .and(expect(parse("0." + "0" * 4000 + "1" * 5999 + "2").isRight, "leading zeros of a fraction are not counted"))
      .and(expect.same(Left(IntegerTooWide(CanonicalValue.MaxIntegerBits)), parse(digits(10000))))
      .and(expect.same(Right(CInt(100)), parse("1e" + "0" * 20000 + "2")))
      .and(parse(digits(10001)) match {
        case Left(e) => expect.same(InvalidNumber(Some(10001L), Some(0L), Some(NumberBound.Digits(10001L, 10000L))), e)
        case other   => failure(s"expected a digit refusal, got ${other.isRight}")
      })
      // both past the bound: the significand's count, as every SDK reports it
      .and(parse("1" * 10002 + "e" + "1" * 10001) match {
        case Left(InvalidNumber(_, _, Some(NumberBound.Digits(digits, _)))) => expect.same(10002L, digits)
        case other                                                          => failure(s"expected a digit refusal, got ${other.isRight}")
      })
      .and(expect(parse("0." + digits(10001)).isLeft, "10,001 fraction digits"))
      .and(expect(parse("1e" + digits(10001)).isLeft, "an exponent of 10,001 digits"))
      .and(expect(parse("1e" + digits(20)).isLeft, "an exponent past every scale, saturated rather than converted"))
      .and(expect.same(Left(IntegerTooWide(CanonicalValue.MaxIntegerBits)), CanonicalValue.int(BigInt(10).pow(10000))))
  }

  pureTest("a malformed or over-long number is described by its length and offset, never its text") {
    val nines = "9" * 10001
    List(
      "0" + nines        -> 10002,
      nines + "."        -> 10002,
      "1." + nines + "e" -> 10004,
      "-" + nines + "e+" -> 10004
    ).map {
      case (text, length) =>
        parse(s"[$text]") match {
          case Left(e @ InvalidNumber(len, offset, _)) =>
            expect
              .same((Some(length.toLong), Some(1L)), (len, offset))
              .and(expect(!e.toString.contains("99999"), s"its diagnostic keeps no digits: $e"))
          case other => failure(s"expected a refusal of a $length-character number, got ${other.isRight}")
        }
    }.reduce(_ and _)
  }

  pureTest("numbers are read exactly from source text") {
    expect
      .same(Right(ratio(1, 10)), parse("0.1"))
      .and(expect.same(Right(CInt(1)), parse("1.0")))
      .and(expect.same(Right(CInt(1)), parse("100e-2")))
      .and(expect.same(Right(CInt(0)), parse("-0")))
      .and(expect.same(Right(CInt(1200)), parse("1.2E3")))
      .and(expect.same(Right(ratio(-3, 8)), parse("-0.375")))
      .and(expect.same(Right(CanonicalGens.int(BigInt("123456789012345678901234567890"))), parse("123456789012345678901234567890")))
  }

  pureTest("integers above 2^53 stay distinct, with distinct encodings") {
    val a = parse("9007199254740992").map(CanonicalGens.encoded)
    val b = parse("9007199254740993").map(CanonicalGens.encoded)
    expect(a.isRight && b.isRight && a != b, s"$a and $b must differ")
  }

  pureTest("malformed numbers are rejected") {
    List("01", "1.", ".5", "1e", "-", "+1", "1e+", "0x10", "NaN", "Infinity").map { s =>
      expect(parse(s).isLeft, s"'$s' should be rejected")
    }.reduce(_ and _)
  }

  pureTest("extreme exponents are rejected by bound, without materialising 10^scale") {
    parse("1e-2000000000") match {
      case Left(InvalidNumber(_, _, Some(NumberBound.Scale(scale, 10000L)))) => expect.same(2000000000, scale)
      case other                                                             => failure(s"expected a bounded-scale rejection, got $other")
    }
  }

  pureTest("objects drop null members and reject duplicate names") {
    expect
      .same(parse("""{"a":1}"""), parse("""{"a":1,"b":null}"""))
      .and(expect.same(Left(DuplicateKey(Preview.of("a"))), parse("""{"a":1,"a":2}""")))
      .and(expect.same(Left(DuplicateKey(Preview.of("a"))), parse("""{"a":null,"a":2}""")))
      .and(expect(parse("[null, 1]").map(_.depth).contains(1), "array nulls are kept"))
  }

  pureTest("escaped surrogates must pair up") {
    expect
      .same(parse("\"\\ud83d\\ude00\""), CanonicalValue.text("\ud83d\ude00"))
      .and(expect(parse("\"\\ud800\"").swap.contains(UnpairedSurrogate), "a lone high surrogate is rejected"))
      .and(expect(parse("{\"\\udc00\":1}").isLeft, "a lone surrogate in a key is rejected"))
  }

  pureTest("strict RFC 8259 syntax") {
    List("", " ", "[1,]", "{\"a\":1,}", "[1 2]", "{\"a\" 1}", "{a:1}", "'x'", "\"\u0001\"", "[1]]", "tru", "nul").map { s =>
      expect(parse(s).isLeft, s"'${s.replace("\u0001", "\\u0001")}' should be rejected")
    }.reduce(_ and _)
  }

  pureTest("a scale bound may be lowered, never raised past the spec's, as in the Rust and TypeScript SDKs") {
    val refused = Left(TypeMismatch(s"the scale bound is not an integer from 0 to ${CanonicalJson.MaxScale}"))
    expect
      .same(refused, CanonicalJson.parse("1.5", CanonicalJson.MaxScale + 1))
      .and(expect.same(refused, CanonicalJson.parse("1.5", Int.MaxValue)))
      .and(expect.same(refused, CanonicalJson.parse("1.5", -1)))
      .and(expect(CanonicalJson.parse("1.5", CanonicalJson.MaxScale).isRight, "the spec's bound itself"))
      .and(expect(CanonicalJson.parse("1.5", 0).isLeft, "a scale of 1 past a bound of 0"))
  }

  pureTest("deeply nested JSON text is a typed error, never a stack overflow") {
    val bomb = "[" * 200000 + "]" * 200000
    expect.same(Left(DepthExceeded(CanonicalValue.MaxDepth)), parse(bomb))
  }

  test("render and parse are inverse for JSON-representable values") {
    forall(CanonicalGens.jsonValue(4)) { v =>
      expect.same(Right(v), CanonicalJson.render(v).flatMap(CanonicalJson.parse(_)))
    }
  }

  pureTest("render fails for values JSON cannot represent") {
    expect(CanonicalJson.render(CBytes(scodec.bits.ByteVector(1, 2))).isLeft, "bytes have no JSON form")
      .and(expect(CanonicalJson.render(ratio(1, 3)).isLeft, "1/3 has no exact decimal"))
      .and(expect.same(Right("0.1"), CanonicalJson.render(ratio(1, 10))))
  }

  pureTest("array nulls are positional: removing one changes the encoding") {
    expect(parse("[null,1]").map(CanonicalGens.encoded) != parse("[1]").map(CanonicalGens.encoded), "[null,1] and [1] must differ")
  }

  pureTest("nulls inside objects nested in arrays are still dropped") {
    expect.same(parse("""[{"a":null,"b":1}]""").map(CanonicalGens.encoded), parse("""[{"b":1}]""").map(CanonicalGens.encoded))
  }

  test("encoded-key order is a strict total order on distinct keys") {
    forall(org.scalacheck.Gen.zip(CanonicalGens.text, CanonicalGens.text)) {
      case (a, b) =>
        val enc = (s: String) => CanonicalGens.encoded(CanonicalValue.text(s).toOption.get)
        val (ab, ba) = (Cbor.compareBytes(enc(a), enc(b)), Cbor.compareBytes(enc(b), enc(a)))
        expect((a == b) == (ab == 0), s"compare($a, $b) = $ab must be 0 exactly when equal")
          .and(expect(math.signum(ab) == -math.signum(ba), "the order must be antisymmetric"))
    }
  }
}
