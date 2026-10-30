package xyz.kd5ujc.core.hash

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object PoseidonSuite extends SimpleIOSuite with Checkers {

  private def h(xs: Int*) = Poseidon.hash(xs.map(BigInt(_)))

  pureTest("circomlibjs known answers (widths 2..5, including R - 1)") {
    expect
      .same(Right(BigInt("18586133768512220936620570745912940619677854269274689475585506675881198879027")), h(1))
      .and(expect.same(Right(BigInt("115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a", 16)), h(1, 2)))
      .and(expect.same(Right(BigInt("16070431878087339506657234884858910435593423055199073760739081656581316900759")), h(3, 4, 5)))
      .and(expect.same(Right(BigInt("299c867db6c1fdd79dcefa40e4510b9837e60ebb1ce0663dbaa525df65250465", 16)), h(1, 2, 3, 4)))
      .and(
        expect.same(
          Right(BigInt("3366645945435192953002076803303112651887535928162668198103357554665518664470")),
          Poseidon.hash(List(Poseidon.R - 1))
        )
      )
  }

  pureTest("empty, too many, negative and >= R inputs are typed errors") {
    expect(Poseidon.hash(Nil).isLeft)
      .and(expect(h(1, 2, 3, 4, 5).isLeft))
      .and(expect(Poseidon.hash(List(BigInt(-1))).isLeft))
      .and(expect(Poseidon.hash(List(Poseidon.R)).isLeft))
      .and(expect.same(Poseidon.compress(7, 9), h(7, 9)))
  }

}
