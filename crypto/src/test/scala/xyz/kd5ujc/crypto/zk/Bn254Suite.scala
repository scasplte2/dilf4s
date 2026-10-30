package xyz.kd5ujc.crypto.zk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.Show
import cats.effect.IO
import cats.syntax.all._

import io.circe.parser
import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/** go-ethereum's EIP-196/197 precompile vectors (core/vm/testdata/precompiles). */
object Bn254Suite extends SimpleIOSuite with Checkers {

  private def vectors(name: String): List[(String, ByteVector, ByteVector)] =
    parser
      .parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "zk", s"$name.json")), UTF_8))
      .flatMap(_.as[List[Map[String, io.circe.Json]]])
      .toOption
      .get
      .map(m =>
        (m("Name").asString.get, ByteVector.fromValidHex(m("Input").asString.get), ByteVector.fromValidHex(m("Expected").asString.get))
      )

  private def replay(name: String, count: Int, on: String = "the precompile functions")(
    f: ByteVector => Either[Bn254.ZkError, ByteVector]
  ) =
    pureTest(s"$name: go-ethereum vectors, on $on") {
      val vs = vectors(name)
      expect.same(count, vs.size).and(vs.foldMap { case (n, in, out) => expect.same(Right(out), f(in)).and(expect(true, n)) })
    }

  private def word(ok: Boolean): ByteVector = ByteVector.low(31) :+ (if (ok) 1 else 0).toByte

  replay("bn256Add", 16)(Bn254.ecAdd)
  replay("bn256ScalarMul", 19)(Bn254.ecMul)
  replay("bn256Pairing", 14)(in => Bn254.ecPairing(in).map(word))

  // ---- the engines: both replay the same vectors (with the precompiles' zero-padding applied here), and agree

  private val gnark: Bn254Engine = Bn254Engine.gnark[IO].unsafeRunSync()(using cats.effect.unsafe.IORuntime.global)

  private def engine(label: String, e: Bn254Engine): Unit = {
    replay("bn256Add", 16, label) { in =>
      val x = in.take(128).padRight(128)
      e.add(x.take(64), x.drop(64))
    }
    replay("bn256ScalarMul", 19, label) { in =>
      val x = in.take(96).padRight(96)
      e.mul(x.take(64), x.drop(64))
    }
    replay("bn256Pairing", 14, label) { in =>
      if (in.size % 192 != 0) Left(Bn254.ZkError.Malformed("not whole pairs"))
      else e.pairing(in.grouped(192).map(c => c.take(64) -> c.drop(64)).toVector).map(word)
    }
  }

  engine("the reference engine", Bn254Engine.reference)
  engine("gnark", gnark)

  private val g1 =
    Bn254.encodeG1(Bn254.decodeG1(ByteVector.low(31) ++ ByteVector(1) ++ ByteVector.low(31) ++ ByteVector(2)).toOption.flatten)
  private val scalar: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private given Show[(ByteVector, ByteVector)] = Show.show { case (a, b) => s"${a.toHex}/${b.toHex}" }

  test("gnark agrees with the reference on k·G and on sums of multiples, for random 256-bit scalars") {
    forall(Gen.zip(scalar, scalar)) {
      case (j, k) =>
        val both = List(Bn254Engine.reference, gnark).map { e =>
          (e.mul(g1, j), e.mul(g1, k)).tupled.flatMap { case (p, q) => e.add(p, q).map((p, q, _)) }
        }
        expect.same(both.head, both(1)).and(expect(both.head.isRight, "valid points"))
    }
  }
}
