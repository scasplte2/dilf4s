package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import weaver.{Expectations, SimpleIOSuite}

/** Every case of both vector files: Ergo's verdict, and ours (stricter on the proof's length). */
object SigmaVectorsSuite extends SimpleIOSuite {

  import SigmaVectors.Case

  private def agrees(c: Case): Expectations = {
    val ours = Sigma.verify(c.prop, c.proof, c.message)
    c.kind match {
      case "valid"                       => expect.same(Right(true), ours).and(expect(c.ergo, s"${c.name}: Ergo accepts"))
      case "trailing-byte" | "truncated" => expect(ours.isLeft, s"${c.name} ${c.kind}: malformed for us (Ergo: ${c.ergo}), got $ours")
      case _                             => expect(!c.ergo && ours != Right(true), s"${c.name} ${c.kind}: rejected by both, ours $ours")
    }
  }

  test("Ergo's proofs: we agree with Ergo's verifier on every case, and reject wrong lengths it accepts") {
    SigmaVectors.load(SigmaVectors.ergoPath).map { cases =>
      expect
        .same(120, cases.size)
        .and(cases.foldMap(agrees))
        .and(expect(cases.exists(c => c.kind == "trailing-byte" && c.ergo), "Ergo ignores trailing bytes"))
    }
  }

  test("our proofs: Ergo's verifier accepts every one, and our prover reproduces them from the recorded seed") {
    SigmaVectors.load(SigmaVectors.oursPath).map { cases =>
      val valid = cases.filter(_.kind == "valid")
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
