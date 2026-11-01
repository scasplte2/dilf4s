package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import io.circe.syntax._
import io.circe.{Json, parser}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CanonicalJsonSuite extends SimpleIOSuite with Checkers {

  private def ratio(n: Int, d: Int): CanonicalValue = CanonicalValue.number(Ratio.of(n, d).toOption.get)
  private def parse(s: String) = CanonicalJson.parse(s)

  pureTest("a refused rational is named by its size, never its digits") {
    val third = Ratio.of(BigInt(1) << 1000000, BigInt(3)).fold(e => sys.error(e.toString), CanonicalValue.number)
    val refusal = NotJsonRepresentable("non-terminating rational of 1000001/2 bits")
    expect.same(Left(refusal), CanonicalJson.render(third)).and(expect.same(Left(refusal), CanonicalJson.toCirce(third)))
  }

  pureTest("a number refused for its scale is named by its size, never its digits") {
    val wide = Json.fromBigDecimal(BigDecimal(new java.math.BigDecimal((BigInt(1) << 1000000).bigInteger, 10001)))
    CanonicalJson.fromCirce(wide) match {
      case Left(InvalidNumber(source, Some(_))) =>
        expect.same("a number of scale 10001 with a 1000001-bit coefficient", source)
      case other => failure(s"expected a scale refusal, got ${other.isRight}")
    }
  }

  pureTest("numbers are read exactly from source text") {
    expect
      .same(Right(ratio(1, 10)), parse("0.1"))
      .and(expect.same(Right(CInt(1)), parse("1.0")))
      .and(expect.same(Right(CInt(1)), parse("100e-2")))
      .and(expect.same(Right(CInt(0)), parse("-0")))
      .and(expect.same(Right(CInt(1200)), parse("1.2E3")))
      .and(expect.same(Right(ratio(-3, 8)), parse("-0.375")))
      .and(expect.same(Right(CInt(BigInt("123456789012345678901234567890"))), parse("123456789012345678901234567890")))
  }

  pureTest("integers above 2^53 stay distinct, with distinct encodings") {
    val a = parse("9007199254740992").map(Cbor.encode)
    val b = parse("9007199254740993").map(Cbor.encode)
    expect(a.isRight && b.isRight && a != b, s"$a and $b must differ")
  }

  pureTest("malformed numbers are rejected") {
    List("01", "1.", ".5", "1e", "-", "+1", "1e+", "0x10", "NaN", "Infinity").map { s =>
      expect(parse(s).isLeft, s"'$s' should be rejected")
    }.reduce(_ and _)
  }

  pureTest("extreme exponents are rejected by bound, without materialising 10^scale") {
    parse("1e-2000000000") match {
      case Left(InvalidNumber(_, Some(_))) => success
      case other                           => failure(s"expected a bounded-scale rejection, got $other")
    }
  }

  pureTest("objects drop null members and reject duplicate names") {
    expect
      .same(parse("""{"a":1}"""), parse("""{"a":1,"b":null}"""))
      .and(expect.same(Left(DuplicateKey("a")), parse("""{"a":1,"a":2}""")))
      .and(expect.same(Left(DuplicateKey("a")), parse("""{"a":null,"a":2}""")))
      .and(expect(parse("[null, 1]").map(_.depth).contains(1), "array nulls are kept"))
  }

  pureTest("escaped surrogates must pair up") {
    expect
      .same(parse("\"\\ud83d\\ude00\""), CanonicalValue.text("\ud83d\ude00"))
      .and(expect(parse("\"\\ud800\"").left.exists(_.isInstanceOf[UnpairedSurrogate]), "a lone high surrogate is rejected"))
      .and(expect(parse("{\"\\udc00\":1}").isLeft, "a lone surrogate in a key is rejected"))
  }

  pureTest("strict RFC 8259 syntax") {
    List("", " ", "[1,]", "{\"a\":1,}", "[1 2]", "{\"a\" 1}", "{a:1}", "'x'", "\"\u0001\"", "[1]]", "tru", "nul").map { s =>
      expect(parse(s).isLeft, s"'${s.replace("\u0001", "\\u0001")}' should be rejected")
    }.reduce(_ and _)
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

  test("the circe bridge agrees with the parser") {
    forall(CanonicalGens.jsonValue(4)) { v =>
      val viaCirce =
        CanonicalJson.render(v).flatMap(s => parser.parse(s).left.map(e => JsonSyntax(0, e.message))).flatMap(CanonicalJson.fromCirce(_))
      expect.same(Right(v), viaCirce)
    }
  }

  pureTest("render fails for values JSON cannot represent") {
    expect(CanonicalJson.render(CBytes(scodec.bits.ByteVector(1, 2))).isLeft, "bytes have no JSON form")
      .and(expect(CanonicalJson.render(ratio(1, 3)).isLeft, "1/3 has no exact decimal"))
      .and(expect.same(Right("0.1"), CanonicalJson.render(ratio(1, 10))))
  }

  pureTest("typed data derives through its circe Encoder") {
    final case class Payment(to: String, amount: BigInt, memo: Option[String])
    implicit val enc: io.circe.Encoder[Payment] = io.circe.generic.semiauto.deriveEncoder[Payment]
    val c = Canonical.fromEncoder[Payment]
    val withoutMemo = c.canonical(Payment("x", BigInt("9007199254740993"), None))
    val legacyShape = CanonicalJson.fromCirce(Json.obj("to" -> "x".asJson, "amount" -> BigInt("9007199254740993").asJson))
    expect.same(legacyShape, withoutMemo)
  }

  test("typed data round-trips through the canonical model and its encoding") {
    final case class Note(id: Long, label: String, amount: BigInt, rate: BigDecimal, tags: List[String], parent: Option[Long])
    implicit val enc: io.circe.Encoder[Note] = io.circe.generic.semiauto.deriveEncoder[Note]
    implicit val dec: io.circe.Decoder[Note] = io.circe.generic.semiauto.deriveDecoder[Note]
    val gen = for {
      id     <- org.scalacheck.Gen.long
      label  <- CanonicalGens.text
      amount <- CanonicalGens.bigInt
      rate   <- CanonicalGens.decimal.map(r => BigDecimal(r.toDecimalString.get))
      tags   <- org.scalacheck.Gen.listOf(CanonicalGens.text)
      parent <- org.scalacheck.Gen.option(org.scalacheck.Gen.long)
    } yield Note(id, label, amount, rate, tags, parent)
    implicit val show: cats.Show[Note] = cats.Show.fromToString
    forall(gen) { note =>
      val roundTrip = Canonical.fromEncoder[Note].canonical(note).map(Cbor.encode).flatMap(Cbor.decode(_)).flatMap(Canonical.decode[Note])
      expect.same(Right(note), roundTrip)
    }
  }

  pureTest("array nulls are positional: removing one changes the encoding") {
    expect(parse("[null,1]").map(Cbor.encode) != parse("[1]").map(Cbor.encode), "[null,1] and [1] must differ")
  }

  pureTest("nulls inside objects nested in arrays are still dropped") {
    expect.same(parse("""[{"a":null,"b":1}]""").map(Cbor.encode), parse("""[{"b":1}]""").map(Cbor.encode))
  }

  test("encoded-key order is a strict total order on distinct keys") {
    forall(org.scalacheck.Gen.zip(CanonicalGens.text, CanonicalGens.text)) {
      case (a, b) =>
        val enc = (s: String) => Cbor.encode(CanonicalValue.text(s).toOption.get)
        val (ab, ba) = (Cbor.compareBytes(enc(a), enc(b)), Cbor.compareBytes(enc(b), enc(a)))
        expect((a == b) == (ab == 0), s"compare($a, $b) = $ab must be 0 exactly when equal")
          .and(expect(math.signum(ab) == -math.signum(ba), "the order must be antisymmetric"))
    }
  }
}
