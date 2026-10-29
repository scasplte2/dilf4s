package xyz.kd5ujc.accumulators.trie

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.core.hash.Sha256

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{BitVector, ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/trie.md: roots, encoded proofs, and the answers each proof must give. */
object TrieVectors {

  val path: Path = Paths.get("shared", "vectors", "trie_v1.json")

  private val T = TrieHashing[Sha256]

  private def orDie[A](e: Either[TrieError, A]): A = e.fold(err => throw new IllegalStateException(err.toString), identity)

  private val fixedEntries: List[(ByteVector, ByteVector)] =
    List(hex"0000", hex"0001", hex"00ff", hex"8000", hex"ffff", hex"7f80").zipWithIndex.map { case (k, i) => k -> ByteVector(s"value-$i".getBytes(UTF_8)) }

  private val variableEntries: List[(ByteVector, ByteVector)] =
    List(hex"", hex"61", hex"6162", hex"616263", hex"62", hex"6100", hex"ff").zipWithIndex.map { case (k, i) => k -> ByteVector(s"value-$i".getBytes(UTF_8)) }

  private def lookupJson(l: Lookup[Sha256]): Json = l match {
    case Lookup.Present(v) => Json.obj("present" -> v.hex.asJson)
    case Lookup.Absent     => "absent".asJson
    case Lookup.NotCovered => "notCovered".asJson
  }

  private def fixture(name: String, mode: KeyMode, entries: List[(ByteVector, ByteVector)], queries: List[ByteVector], prefixes: List[ByteVector], probes: List[ByteVector]): Json = {
    val trie = orDie(Trie.fromEntries[Sha256](mode, entries.map { case (k, v) => k -> T.value(v) }))
    val proof = orDie(trie.prove(queries, prefixes))
    val verified = orDie(TrieProofs.verify(mode, trie.root, proof))
    Json.obj(
      "name" -> name.asJson,
      "mode" -> (mode match {
        case f: KeyMode.Fixed    => Json.obj("fixed" -> f.bytes.asJson)
        case v: KeyMode.Variable => Json.obj("variable" -> v.maxBytes.asJson)
      }),
      "entries" -> entries.map { case (k, v) => Json.obj("key" -> k.toHex.asJson, "value" -> v.toHex.asJson, "valueHash" -> T.value(v).hex.asJson) }.asJson,
      "root" -> trie.root.hex.asJson,
      "proof" -> Json.obj(
        "keys" -> queries.map(_.toHex).asJson,
        "prefixes" -> prefixes.map(_.toHex).asJson,
        "cbor" -> orDie(TrieProofs.encode(proof)).pipe(Cbor.encode).toHex.asJson,
        "lookups" -> probes.map(k => Json.obj("key" -> k.toHex.asJson, "result" -> lookupJson(verified.lookup(k.bits)))).asJson,
        "prefixListings" -> prefixes.map { p =>
          Json.obj("prefix" -> p.toHex.asJson, "entries" -> verified.prefix(p.bits).map(_.map { case (b, v) => Json.obj("key" -> b.bytes.toHex.asJson, "valueHash" -> v.hex.asJson) }).asJson)
        }.asJson
      )
    )
  }

  def build: Json = {
    val fixed2 = orDie(KeyMode.fixed(2))
    val variable3 = orDie(KeyMode.variable(3))
    Json.obj(
      "spec" -> "docs/specs/trie.md".asJson,
      "version" -> 1.asJson,
      "hash" -> "SHA-256".asJson,
      "emptyRoot" -> T.empty.hex.asJson,
      "nodeHashes" -> Json.obj(
        "leaf" -> Json.obj("segment" -> Json.obj("bits" -> 11.asJson, "bytes" -> BitVector.high(11).bytes.toHex.asJson), "valueHash" -> T.value(hex"01").hex.asJson, "hash" -> T.leaf(BitVector.high(11), T.value(hex"01")).hex.asJson),
        "branch" -> Json.obj("segment" -> Json.obj("bits" -> 0.asJson, "bytes" -> "".asJson), "valueHash" -> T.value(hex"02").hex.asJson, "left" -> T.value(hex"03").hex.asJson, "hash" -> T.branch(BitVector.empty, Some(T.value(hex"02")), Some(T.value(hex"03")), None).hex.asJson)
      ),
      "fixtures" -> List(
        fixture("fixed-2", fixed2, fixedEntries, List(hex"0001", hex"8001"), Nil, List(hex"0001", hex"8001", hex"0000", hex"ffff")),
        fixture("fixed-2-prefix", fixed2, fixedEntries, Nil, List(hex"00"), List(hex"0000", hex"00ff", hex"ffff")),
        fixture("variable-3", variable3, variableEntries, List(hex"6162", hex"6163", hex""), Nil, List(hex"6162", hex"6163", hex"", hex"61", hex"ff")),
        fixture("variable-3-prefix", variable3, variableEntries, Nil, List(hex"61"), List(hex"61", hex"6100", hex"62"))
      ).asJson
    )
  }

  def render: String = build.spaces2 + "\n"

  implicit private class Pipe[A](a: A) {
    def pipe[B](f: A => B): B = f(a)
  }
}

object TrieVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] = IO(Files.write(TrieVectors.path, TrieVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object TrieVectorsSuite extends SimpleIOSuite {
  test("committed trie vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(TrieVectors.path), UTF_8)).map { committed =>
      expect(committed == TrieVectors.render, s"${TrieVectors.path} is stale; regenerate with: sbt \"accumulators/Test/runMain xyz.kd5ujc.accumulators.trie.TrieVectorGen\"")
    }
  }

  pureTest("vector fixtures exercise all three lookup answers") {
    val rendered = TrieVectors.render
    expect(rendered.contains("\"present\"") && rendered.contains("\"absent\"") && rendered.contains("\"notCovered\""), "present, absent and notCovered all appear")
  }
}
