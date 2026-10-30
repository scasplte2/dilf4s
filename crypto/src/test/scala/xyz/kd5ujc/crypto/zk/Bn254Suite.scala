package xyz.kd5ujc.crypto.zk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import io.circe.parser
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** go-ethereum's EIP-196/197 precompile vectors (core/vm/testdata/precompiles). */
object Bn254Suite extends SimpleIOSuite {

  private def vectors(name: String): List[(String, ByteVector, ByteVector)] =
    parser
      .parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "zk", s"$name.json")), UTF_8))
      .flatMap(_.as[List[Map[String, io.circe.Json]]])
      .toOption
      .get
      .map(m =>
        (m("Name").asString.get, ByteVector.fromValidHex(m("Input").asString.get), ByteVector.fromValidHex(m("Expected").asString.get))
      )

  private def replay(name: String, count: Int)(f: ByteVector => Either[Bn254.ZkError, ByteVector]) =
    pureTest(s"$name: go-ethereum vectors") {
      val vs = vectors(name)
      expect.same(count, vs.size).and(vs.foldMap { case (n, in, out) => expect.same(Right(out), f(in)).and(expect(true, n)) })
    }

  replay("bn256Add", 16)(Bn254.ecAdd)
  replay("bn256ScalarMul", 19)(Bn254.ecMul)
  replay("bn256Pairing", 14)(in => Bn254.ecPairing(in).map(ok => ByteVector.low(31) :+ (if (ok) 1 else 0).toByte))
}
