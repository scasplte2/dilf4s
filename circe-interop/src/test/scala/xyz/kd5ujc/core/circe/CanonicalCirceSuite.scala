package xyz.kd5ujc.core.circe

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalGens, CanonicalJson, CanonicalValue, Cbor, NumberSource}
import xyz.kd5ujc.core.vectors.CanonicalVectors
import xyz.kd5ujc.numerics.{ArithmeticError, Ratio}

import io.circe.syntax._
import io.circe.{Json, parser}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CanonicalCirceSuite extends SimpleIOSuite with Checkers {

  private def parse(s: String) = CanonicalJson.parse(s)

  private def viaCirce(text: String): Either[CanonicalError, CanonicalValue] =
    parser.parse(text).left.map(e => JsonSyntax(0, e.message)).flatMap(CanonicalCirce.fromCirce(_))

  pureTest("a refused rational is named by its size, never its digits, as render names it") {
    def rational(n: BigInt, d: BigInt) =
      Ratio.of(n, d).fold(e => sys.error(e.toString), r => CanonicalValue.number(r).fold(e => sys.error(e.toString), identity))
    val third = rational(BigInt(1) << 30000, BigInt(3))
    // a rational that wide is no value (CanonicalValue.MaxRationalBits); the widest integer is, and converts
    val huge = xyz.kd5ujc.core.canonical.CanonicalGens.int((BigInt(1) << CanonicalValue.MaxIntegerBits) - 1)
    expect
      .same(Left(NotJsonRepresentable("non-terminating rational of 30001/2 bits")), CanonicalCirce.toCirce(third))
      .and(expect.same(Right(Json.fromBigInt(huge.value)), CanonicalCirce.toCirce(huge)))
      .and(expect.same(CanonicalJson.render(third).map(_ => ()), CanonicalCirce.toCirce(third).map(_ => ())))
  }

  pureTest("a number refused for its scale is named by its size, never its digits") {
    val wide = Json.fromBigDecimal(BigDecimal(new java.math.BigDecimal((BigInt(1) << 1000000).bigInteger, 10001)))
    CanonicalCirce.fromCirce(wide) match {
      case Left(InvalidNumber(source, Some(_))) =>
        expect.same(NumberSource.Decimal(10001, 1000001), source)
      case other => failure(s"expected a scale refusal, got ${other.isRight}")
    }
  }

  pureTest("a converted number of 10,001 digits is refused by its width, before it is rendered") {
    val wide = Json.fromBigInt(BigInt(10).pow(10000))
    CanonicalCirce.fromCirce(wide) match {
      case Left(InvalidNumber(_, Some(ArithmeticError.DigitsOutOfRange(10001, 10000)))) => success
      case other                                                                        => failure(s"expected a digit refusal, got $other")
    }
  }

  pureTest("circe-parsed numbers are read from their source text, exactly as the parser reads them") {
    val numbers = CanonicalVectors.jsonInputs.filter(t => t.headOption.exists(c => c == '-' || c.isDigit))
    val agree = numbers
      .filter(t => parser.parse(t).isRight)
      .map(t => expect.same(parse(t), viaCirce(t)))
      .reduce(_ and _)
    expect
      .same(
        Left(InvalidNumber(NumberSource.Text(640000, 0), Some(ArithmeticError.DigitsOutOfRange(640000, 10000)))),
        viaCirce("9" * 640000)
      )
      .and(expect(viaCirce("0e" + "1" * 10001).isLeft, "a zero with a 10,001-digit exponent is refused, as the parser refuses it"))
      .and(agree)
  }

  test("the circe bridge agrees with the parser") {
    forall(CanonicalGens.jsonValue(4)) { v =>
      expect.same(Right(v), CanonicalJson.render(v).flatMap(viaCirce))
    }
  }

  test("toCirce is fromCirce's inverse on JSON-representable values") {
    forall(CanonicalGens.jsonValue(4)) { v =>
      expect.same(Right(v), CanonicalCirce.toCirce(v).flatMap(CanonicalCirce.fromCirce(_)))
    }
  }

  pureTest("typed data derives through its circe Encoder") {
    final case class Payment(to: String, amount: BigInt, memo: Option[String])
    implicit val enc: io.circe.Encoder[Payment] = io.circe.generic.semiauto.deriveEncoder[Payment]
    val c = CanonicalCirce.fromEncoder[Payment]
    val withoutMemo = c.canonical(Payment("x", BigInt("9007199254740993"), None))
    val legacyShape = CanonicalCirce.fromCirce(Json.obj("to" -> "x".asJson, "amount" -> BigInt("9007199254740993").asJson))
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
    val codec = CanonicalCirce.codec[Note]
    forall(gen) { note =>
      val viaDecode =
        CanonicalCirce.fromEncoder[Note].canonical(note).map(Cbor.encode).flatMap(Cbor.decode(_)).flatMap(CanonicalCirce.decode[Note])
      val viaCodec =
        codec.encode(note).map(Cbor.encode).flatMap(Cbor.decode(_)).flatMap(codec.decoder.decode(_).left.map(f => TypeMismatch(f.message)))
      expect.same(Right(note), viaDecode).and(expect.same(Right(note), viaCodec))
    }
  }

  pureTest("a circe decoding failure names the value by kind, never by its text (conformance.md §8)") {
    val huge = CanonicalValue.text("x" * (1 << 20)).toOption.get
    val r = CanonicalCirce.decode[Int](huge)
    val rendered = r.left.toOption.fold("")(_.toString)
    // a preview renders as at most 32 code points escaped, quotes, a mark and a length; the message adds a little
    expect(r.isLeft, "text is not an int").and(expect(rendered.length <= 2 * (32 * 6 + 64) + 256, s"renders ${rendered.length} characters"))
  }

  pureTest("a value that is not the type is a decoding failure") {
    final case class Point(x: Long, y: Long)
    implicit val dec: io.circe.Decoder[Point] = io.circe.generic.semiauto.deriveDecoder[Point]
    implicit val enc: io.circe.Encoder[Point] = io.circe.generic.semiauto.deriveEncoder[Point]
    val text = CanonicalValue.text("a point").toOption.get
    expect(CanonicalCirce.decode[Point](text).left.exists(_.isInstanceOf[TypeMismatch]), "decode refuses text")
      .and(expect(CanonicalCirce.codec[Point].decoder.decode(text).isLeft, "and so does the codec"))
      .and(
        expect(
          CanonicalCirce.decode[Point](CBytes(scodec.bits.ByteVector(1))).left.exists(_.isInstanceOf[NotJsonRepresentable]),
          "bytes have no JSON"
        )
      )
  }
}
