package xyz.kd5ujc.accumulators.merkle

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.core.hash.Sha256

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/merkle.md, over the Certificate Transparency test leaves. */
object MerkleVectors {

  val path: Path = Paths.get("shared", "vectors", "merkle_v1.json")

  val leaves: Vector[ByteVector] = Vector(
    hex"",
    hex"00",
    hex"10",
    hex"2021",
    hex"3031",
    hex"40414243",
    hex"5051525354555657",
    hex"606162636465666768696a6b6c6d6e6f"
  )

  private def hexOrDie(e: Either[MerkleError, xyz.kd5ujc.core.canonical.CanonicalValue]): Json =
    e.fold(err => sys.error(err.toString), v => Cbor.encode(v).toHex.asJson)

  def build: Json = {
    implicit val m: MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]
    val ct = MerkleHashing.rfc6962[Sha256]
    val trees = (0 to leaves.size).map(n => MerkleTree.fromLeafHashes[Sha256](leaves.take(n).map(m.leaf)))
    Json.obj(
      "spec"       -> "docs/specs/merkle.md".asJson,
      "version"    -> 1.asJson,
      "hash"       -> "SHA-256".asJson,
      "leaves"     -> leaves.map(_.toHex).asJson,
      "leafHashes" -> leaves.map(l => m.leaf(l).hex).asJson,
      "trees" -> trees.map { t =>
        Json.obj(
          "size"        -> t.size.asJson,
          "root"        -> t.root.hex.asJson,
          "rfc6962Root" -> MerkleTree.fromLeafHashes[Sha256](leaves.take(t.size.toInt).map(ct.leaf))(ct).root.hex.asJson,
          "inclusion"   -> (0L until t.size).map(i => hexOrDie(t.inclusionProof(i).flatMap(MerkleCodec.encodeInclusion))).asJson,
          "consistency" -> (0L to t.size).map(o => hexOrDie(t.consistencyProof(o).flatMap(MerkleCodec.encodeConsistency))).asJson
        )
      }.asJson
    )
  }

  def render: String = build.spaces2 + "\n"
}

object MerkleVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    IO(Files.write(MerkleVectors.path, MerkleVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object MerkleVectorsSuite extends SimpleIOSuite {

  test("committed Merkle vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(MerkleVectors.path), UTF_8)).map { committed =>
      expect(
        committed == MerkleVectors.render,
        s"${MerkleVectors.path} is stale; regenerate with: sbt \"accumulators/Test/runMain xyz.kd5ujc.accumulators.merkle.MerkleVectorGen\""
      )
    }
  }

  pureTest("proof encodings round-trip") {
    implicit val m: MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]
    val t = MerkleTree.fromLeafHashes[Sha256](MerkleVectors.leaves.map(m.leaf))
    val inc = t.inclusionProof(5).toOption.get
    val con = t.consistencyProof(3).toOption.get
    expect
      .same(Right(inc), MerkleCodec.encodeInclusion(inc).flatMap(v => MerkleCodec.decodeInclusion[Sha256](v)))
      .and(expect.same(Right(con), MerkleCodec.encodeConsistency(con).flatMap(v => MerkleCodec.decodeConsistency[Sha256](v))))
      .and(
        expect(MerkleCodec.decodeInclusion[Sha256](xyz.kd5ujc.core.canonical.CanonicalValue.EmptyMap).isLeft, "missing fields are rejected")
      )
  }
}
