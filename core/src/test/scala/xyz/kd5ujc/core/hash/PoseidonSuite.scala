package xyz.kd5ujc.core.hash

import cats.Show

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object PoseidonSuite extends SimpleIOSuite with Checkers {

  private def fr(i: BigInt): Fr = Fr.from(i).toOption.get
  private def h(xs: Int*): Either[PoseidonError, BigInt] = Poseidon.hash(xs.map(i => fr(BigInt(i)))).map(_.value)

  pureTest("circomlibjs known answers (widths 2..5, including r - 1)") {
    expect
      .same(Right(BigInt("18586133768512220936620570745912940619677854269274689475585506675881198879027")), h(1))
      .and(expect.same(Right(BigInt("115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a", 16)), h(1, 2)))
      .and(expect.same(Right(BigInt("16070431878087339506657234884858910435593423055199073760739081656581316900759")), h(3, 4, 5)))
      .and(expect.same(Right(BigInt("299c867db6c1fdd79dcefa40e4510b9837e60ebb1ce0663dbaa525df65250465", 16)), h(1, 2, 3, 4)))
      .and(
        expect.same(
          BigInt("3366645945435192953002076803303112651887535928162668198103357554665518664470"),
          Poseidon.hash1(fr(Fr.Modulus - 1)).value
        )
      )
  }

  pureTest("the fixed-arity functions agree with hash; only the input count can fail") {
    val (a, b, c, d) = (fr(11), fr(22), fr(33), fr(44))
    expect
      .same(Right(Poseidon.hash1(a)), Poseidon.hash(List(a)))
      .and(expect.same(Right(Poseidon.hash2(a, b)), Poseidon.hash(List(a, b))))
      .and(expect.same(Right(Poseidon.hash3(a, b, c)), Poseidon.hash(List(a, b, c))))
      .and(expect.same(Right(Poseidon.hash4(a, b, c, d)), Poseidon.hash(List(a, b, c, d))))
      .and(expect.same(Poseidon.hash2(a, b), Poseidon.compress(a, b)))
      .and(expect.same(Left(PoseidonError.InputCount(0, 4)), Poseidon.hash(Nil)))
      .and(expect.same(Left(PoseidonError.InputCount(5, 4)), Poseidon.hash(List(a, b, c, d, a))))
  }

  pureTest("field elements are checked once, at construction") {
    expect
      .same(Left(PoseidonError.NonCanonical(-1)), Fr.from(-1))
      .and(expect.same(Left(PoseidonError.NonCanonical(Fr.Modulus)), Fr.from(Fr.Modulus)))
      .and(expect.same(BigInt(1), Fr.reduce(Fr.Modulus + 1).value))
  }

  // ---- Montgomery arithmetic against BigInt

  private val element: Gen[BigInt] = Gen.oneOf(
    Gen.choose(0L, Long.MaxValue).map(BigInt(_)),
    Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => BigInt(1, bs.toArray).mod(Fr.Modulus)),
    Gen.oneOf(BigInt(0), BigInt(1), Fr.Modulus - 1, Fr.Modulus - 2, (BigInt(1) << 253) - 1)
  )

  private given Show[(BigInt, BigInt)] = Show.fromToString
  private given Show[List[BigInt]] = Show.fromToString

  test("Montgomery field arithmetic agrees with BigInt mod r, and round-trips") {
    forall(Gen.zip(element, element)) {
      case (a, b) =>
        val (x, y) = (FrMont(a), FrMont(b))
        expect
          .same((a * b).mod(Fr.Modulus), (x * y).toBigInt)
          .and(expect.same((a + b).mod(Fr.Modulus), (x + y).toBigInt))
          .and(expect.same(a, x.toBigInt))
    }
  }

  test("the Montgomery permutation agrees with the BigInteger reference at every width") {
    forall(Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, element))) { xs =>
      val params = PoseidonConstants.byWidth(xs.size + 1)
      expect.same(
        Poseidon.referencePermute(params, BigInt(0) +: xs.toVector).value,
        Poseidon.hash(xs.map(fr)).map(_.value).getOrElse(BigInt(-1))
      )
    }
  }
}
