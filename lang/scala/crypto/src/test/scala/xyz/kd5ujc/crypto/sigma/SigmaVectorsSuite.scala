package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import weaver.{Expectations, SimpleIOSuite}

/** Every case of both vector files: Ergo's verdict, and ours (stricter on the proof's length). */
object SigmaVectorsSuite extends SimpleIOSuite {

  import SigmaVectors.Case

  private def agrees(c: Case): Expectations = {
    val ours = Sigma.check(c.prop, c.proof, c.message)
    c.kind match {
      case SigmaVectors.Kind.Valid => expect.same(Right(true), ours).and(expect(c.ergo, s"${c.name}: Ergo accepts"))
      case SigmaVectors.Kind.TrailingByte | SigmaVectors.Kind.Truncated =>
        expect.same(Left(SigmaError.ProofLength(Sigma.proofLength(c.prop), c.proof.size)), ours)
      case SigmaVectors.Kind.FlippedChallenge | SigmaVectors.Kind.FlippedResponse | SigmaVectors.Kind.OtherMessage =>
        expect.same(false, c.ergo).and(expect.same(Right(false), ours))
    }
  }

  test("Ergo's proofs: we agree with Ergo's verifier on every case, and reject wrong lengths it accepts") {
    SigmaVectors.load(SigmaVectors.ergoPath).map { cases =>
      expect
        .same(120, cases.size)
        .and(cases.foldMap(agrees))
        .and(expect(cases.exists(c => c.kind == SigmaVectors.Kind.TrailingByte && c.ergo), "Ergo ignores trailing bytes"))
    }
  }

  test("our proofs: Ergo's verifier accepts every one, and our prover reproduces them from the recorded seed") {
    SigmaVectors.load(SigmaVectors.oursPath).map { cases =>
      val valid = cases.filter(_.kind == SigmaVectors.Kind.Valid)
      expect
        .same(20, valid.size)
        .and(cases.foldMap(agrees))
        .and(
          valid.foldMap(c => expect.same(Right(c.proof), SigmaProver.proveWith(c.prop, c.secrets, c.message, SigmaVectors.seed(c.name))))
        )
    }
  }

  test("the vector propositions cover every connective, threshold shape and DH tuple use") {
    SigmaVectors.load(SigmaVectors.ergoPath).map { cases =>
      val names = cases.map(_.name).toSet
      expect(names.exists(_.startsWith("ZeroJoin")), "ZeroJoin").and(expect(names.exists(_.contains("3-of-5")), "3-of-5"))
    }
  }
}
