package xyz.kd5ujc.numerics

import cats.kernel.laws.discipline.OrderTests

import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers
import weaver.{FunSuite, SimpleIOSuite}

object RatioGens {

  val bigIntGen: Gen[BigInt] = Gen.oneOf(
    Gen.choose(-1000L, 1000L).map(BigInt(_)),
    Gen.choose(1, 300).flatMap(bits => Gen.long.map(l => BigInt(bits, new scala.util.Random(l)) * (if (l % 2 == 0) 1 else -1)))
  )

  val nonZeroGen: Gen[BigInt] = bigIntGen.filter(_ != BigInt(0))

  val ratioGen: Gen[Ratio] =
    for {
      n <- bigIntGen
      d <- nonZeroGen
    } yield Ratio.of(n, d).fold(e => sys.error(e.toString), identity)

  implicit val arbRatio: Arbitrary[Ratio] = Arbitrary(ratioGen)
  implicit val cogenRatio: Cogen[Ratio] = Cogen[(BigInt, BigInt)].contramap(r => (r.numerator, r.denominator))

}

object RatioLawsSuite extends FunSuite with Discipline {
  import RatioGens._

  checkAll("Order[Ratio]", OrderTests[Ratio].order)
}

object RatioSuite extends SimpleIOSuite with Checkers {
  import RatioGens._

  test("construction always yields the reduced form with a positive denominator") {
    forall(Gen.zip(bigIntGen, nonZeroGen)) {
      case (n, d) =>
        Ratio.of(n, d) match {
          case Right(r) =>
            expect(r.denominator > 0, s"denominator should be positive, got ${r.denominator}")
              .and(expect(r.numerator.gcd(r.denominator) == BigInt(1) || r.numerator == BigInt(0), s"$r should be reduced"))
              .and(expect(r.numerator * d == n * r.denominator, s"$r should equal $n/$d"))
          case Left(e) => failure(s"unexpected $e")
        }
    }
  }

  pureTest("half-even rounding: ties to the even neighbour, everything else to the nearest") {
    def r(n: Int, d: Int) = Ratio.of(BigInt(n), BigInt(d)).toOption.get.roundHalfEven
    expect
      .same(BigInt(2), r(5, 2))
      .and(expect.same(BigInt(4), r(7, 2)))
      .and(expect.same(BigInt(-2), r(-5, 2)))
      .and(expect.same(BigInt(-4), r(-7, 2)))
      .and(expect.same(BigInt(0), r(1, 2)))
      .and(expect.same(BigInt(3), r(8, 3)))
      .and(expect.same(BigInt(-3), r(-8, 3)))
      .and(expect.same(BigInt(7), r(7, 1)))
  }

  test("half-even rounding lands within 1/2 of the value") {
    forall(RatioGens.ratioGen) { x =>
      val diff = (Ratio.integer(x.roundHalfEven) - x).abs
      expect(diff <= Ratio.of(1, 2).toOption.get)
    }
  }

  test("zero has the single representation 0/1") {
    forall(nonZeroGen)(d => expect.same(Right(Ratio.Zero), Ratio.of(0, d)))
  }

  pureTest("a zero denominator is a typed error, not an exception") {
    expect
      .same(Left(ArithmeticError.DivisionByZero), Ratio.of(1, 0))
      .and(expect.same(Left(ArithmeticError.DivisionByZero), Ratio.One / Ratio.Zero))
      .and(expect.same(Left(ArithmeticError.DivisionByZero), Ratio.Zero.inverse))
      .and(expect.same(Left(ArithmeticError.DivisionByZero), Ratio.Zero.pow(-1)))
  }

  test("addition and subtraction are inverse") {
    forall(Gen.zip(ratioGen, ratioGen)) { case (a, b) => expect.same(a, (a + b) - b) }
  }

  test("multiplication and division are inverse for a non-zero divisor") {
    forall(Gen.zip(ratioGen, ratioGen.filter(_.signum != 0))) { case (a, b) => expect.same(Right(a), (a * b) / b) }
  }

  test("pow with a negative exponent equals the inverse of the positive power") {
    forall(Gen.zip(ratioGen.filter(_.signum != 0), Gen.choose(1, 6))) {
      case (a, k) =>
        expect.same(a.pow(k).flatMap(_.inverse), a.pow(-k))
    }
  }

  test("floor <= value <= ceil, differing by at most one") {
    forall(ratioGen) { r =>
      val (f, c) = (Ratio.integer(r.floor), Ratio.integer(r.ceil))
      expect(f <= r, s"floor $f should be <= $r")
        .and(expect(r <= c, s"ceil $c should be >= $r"))
        .and(expect(r.ceil - r.floor <= 1, s"floor/ceil of $r should differ by at most 1"))
    }
  }

  test("rem keeps the dividend's sign and is smaller than the divisor in magnitude") {
    forall(Gen.zip(ratioGen, ratioGen.filter(_.signum != 0))) {
      case (a, b) =>
        a.rem(b) match {
          case Right(r) =>
            expect(r.signum == 0 || r.signum == a.signum, s"rem($a, $b) = $r should follow the dividend's sign")
              .and(expect(r.abs < b.abs, s"|rem($a, $b)| = ${r.abs} should be < ${b.abs}"))
          case Left(e) => failure(s"unexpected $e")
        }
    }
  }

  pureTest("decimal conversion is exact and bounded") {
    expect
      .same(Ratio.of(1, 10), Ratio.fromBigDecimal(BigDecimal("0.1")))
      .and(expect.same(Right(Ratio.One), Ratio.fromBigDecimal(BigDecimal("1.000"))))
      .and(expect.same(Right(Ratio.integer(BigInt("9007199254740993"))), Ratio.fromBigDecimal(BigDecimal("9007199254740993"))))
      .and(expect.same(Right(Ratio.integer(1200)), Ratio.fromBigDecimal(BigDecimal("1.2E3"))))
      .and(
        expect.same(
          Left(ArithmeticError.ScaleOutOfRange(2000000000, Ratio.DefaultMaxDecimalScale)),
          Ratio.fromBigDecimal(BigDecimal("1e-2000000000"))
        )
      )
  }

  pureTest("decimal rendering is exact for terminating values and absent otherwise") {
    val third = Ratio.of(1, 3).toOption
    expect
      .same(Some("0.1"), Ratio.of(1, 10).toOption.flatMap(_.toDecimalString))
      .and(expect.same(Some("-0.375"), Ratio.of(-3, 8).toOption.flatMap(_.toDecimalString)))
      .and(expect.same(Some("12"), Ratio.integer(12).toDecimalString))
      .and(expect.same(None, third.flatMap(_.toDecimalString)))
  }

  test("terminating decimals round-trip through their rendering") {
    forall(Gen.zip(bigIntGen, Gen.choose(0, 40))) {
      case (unscaled, scale) =>
        val r = Ratio.fromDecimal(unscaled, scale)
        expect.same(
          r,
          r.flatMap(x => x.toDecimalString.toRight(ArithmeticError.DivisionByZero).flatMap(s => Ratio.fromBigDecimal(BigDecimal(s))))
        )
    }
  }
}
