package xyz.kd5ujc.accumulators.merkle

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Sha256

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, hex}
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

  /**
   * Values committed as leaves (section 2): byte strings, and values whose CBOR equals those bytes, as JSON text. The
   * bytes `6161` and the text "a" (CBOR `6161`) must commit apart.
   */
  val committedBytes: Vector[ByteVector] = Vector(hex"", hex"6161", hex"f6")
  val committedJson: Vector[String] =
    Vector("\"a\"", "null", "0", "-1", "0.5", "[]", "[1,[2]]", "{\"b\":1,\"a\":[true,false]}", "18446744073709551616")

  private def commitment(v: CanonicalValue, m: MerkleHashing[Sha256]): Json = {
    val c = ValueCommitment.bytes(v)
    Json.obj("cbor" -> Cbor.encode(v).toHex.asJson, "commitment" -> c.toHex.asJson, "leafHash" -> m.leaf(c).hex.asJson)
  }

  private def hexOrDie(e: Either[MerkleError, xyz.kd5ujc.core.canonical.CanonicalValue]): Json =
    e.fold(err => sys.error(err.toString), v => Cbor.encode(v).toHex.asJson)

  private def proofValue(fields: (String, CanonicalValue)*): CanonicalValue =
    CanonicalValue.map(fields).fold(e => sys.error(e.toString), identity)

  private def digests(sizes: Long*): CanonicalValue =
    CanonicalValue.array(sizes.map(n => CanonicalValue.bytes(ByteVector.fill(n)(0x11))).toVector).fold(e => sys.error(e.toString), identity)

  private def int(i: BigInt): CanonicalValue = CanonicalValue.int(i)

  /**
   * Proofs the decoders refuse, with the kind of error each gives: an index not below the size is `IndexOutOfRange`,
   * an old size above the new is `InvalidSizes`, and a bad shape or digest is `Encoding`.
   */
  val malformed: List[(String, String, CanonicalValue, String)] = List(
    ("index-equals-size", "inclusion", proofValue("index" -> int(3), "size" -> int(3), "path" -> digests(32, 32)), "IndexOutOfRange"),
    ("index-above-size", "inclusion", proofValue("index" -> int(9), "size" -> int(3), "path" -> digests(32, 32)), "IndexOutOfRange"),
    ("empty-tree", "inclusion", proofValue("index" -> int(0), "size" -> int(0), "path" -> digests()), "IndexOutOfRange"),
    ("short-digest", "inclusion", proofValue("index" -> int(0), "size" -> int(2), "path" -> digests(31)), "Encoding"),
    ("negative-index", "inclusion", proofValue("index" -> int(-1), "size" -> int(2), "path" -> digests(32)), "Encoding"),
    ("index-2-63", "inclusion", proofValue("index" -> int(BigInt(2).pow(63)), "size" -> int(2), "path" -> digests(32)), "Encoding"),
    ("missing-size", "inclusion", proofValue("index" -> int(0), "path" -> digests(32)), "Encoding"),
    ("old-above-new", "consistency", proofValue("old" -> int(5), "new" -> int(3), "path" -> digests(32)), "InvalidSizes"),
    ("old-above-empty", "consistency", proofValue("old" -> int(1), "new" -> int(0), "path" -> digests()), "InvalidSizes"),
    ("long-digest", "consistency", proofValue("old" -> int(1), "new" -> int(3), "path" -> digests(33)), "Encoding"),
    ("negative-old", "consistency", proofValue("old" -> int(-1), "new" -> int(3), "path" -> digests(32)), "Encoding"),
    ("extra-field", "consistency", proofValue("old" -> int(1), "new" -> int(3), "path" -> digests(32), "x" -> int(0)), "Encoding")
  )

  private def malformedRow(name: String, kind: String, proof: CanonicalValue, expected: String): Json = {
    val decoded = kind match {
      case "inclusion" => MerkleCodec.decodeInclusion[Sha256](proof).map(_ => ())
      case _           => MerkleCodec.decodeConsistency[Sha256](proof).map(_ => ())
    }
    val result = decoded.fold(_.productPrefix, _ => "ok")
    require(result == expected, s"malformed $kind $name: expected $expected, got $result")
    Json.obj("name" -> name.asJson, "kind" -> kind.asJson, "cbor" -> Cbor.encode(proof).toHex.asJson, "error" -> result.asJson)
  }

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
        val r = MerkleTree.fromLeafHashes[Sha256](leaves.take(t.size.toInt).map(ct.leaf))(using ct)
        Json.obj(
          "size"               -> t.size.asJson,
          "root"               -> t.root.hex.asJson,
          "rfc6962Root"        -> r.root.hex.asJson,
          "inclusion"          -> (0L until t.size).map(i => hexOrDie(t.inclusionProof(i).flatMap(MerkleCodec.encodeInclusion))).asJson,
          "consistency"        -> (0L to t.size).map(o => hexOrDie(t.consistencyProof(o).flatMap(MerkleCodec.encodeConsistency))).asJson,
          "rfc6962Inclusion"   -> (0L until r.size).map(i => hexOrDie(r.inclusionProof(i).flatMap(MerkleCodec.encodeInclusion))).asJson,
          "rfc6962Consistency" -> (0L to r.size).map(o => hexOrDie(r.consistencyProof(o).flatMap(MerkleCodec.encodeConsistency))).asJson
        )
      }.asJson,
      "valueCommitments" -> (committedBytes.map(CanonicalValue.bytes) ++
      committedJson.map(j => CanonicalJson.parse(j).fold(e => sys.error(e.toString), identity))).map(commitment(_, m)).asJson,
      "malformed" -> malformed.map { case (name, kind, proof, error) => malformedRow(name, kind, proof, error) }.asJson
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
