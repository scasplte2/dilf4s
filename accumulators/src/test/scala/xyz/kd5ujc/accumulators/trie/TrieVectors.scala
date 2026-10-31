package xyz.kd5ujc.accumulators.trie

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{Digest, Sha256}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{BitVector, ByteVector, hex}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/trie.md: roots, encoded proofs, and the answers each proof must give. */
object TrieVectors {

  val path: Path = Paths.get("shared", "vectors", "trie_v1.json")

  private val T = TrieHashing[Sha256]

  private def orDie[A](e: Either[TrieError, A]): A = e.fold(err => throw new IllegalStateException(err.toString), identity)

  private val fixedEntries: List[(ByteVector, ByteVector)] =
    List(hex"0000", hex"0001", hex"00ff", hex"8000", hex"ffff", hex"7f80").zipWithIndex.map {
      case (k, i) => k -> ByteVector(s"value-$i".getBytes(UTF_8))
    }

  private val variableEntries: List[(ByteVector, ByteVector)] =
    List(hex"", hex"61", hex"6162", hex"616263", hex"62", hex"6100", hex"ff").zipWithIndex.map {
      case (k, i) => k -> ByteVector(s"value-$i".getBytes(UTF_8))
    }

  private def lookupJson(l: Lookup[Sha256]): Json = l match {
    case Lookup.Present(v) => Json.obj("present" -> v.hex.asJson)
    case Lookup.Absent     => "absent".asJson
    case Lookup.NotCovered => "notCovered".asJson
  }

  private def fixture(
    name:        String,
    mode:        KeyMode,
    entries:     List[(ByteVector, ByteVector)],
    queries:     List[ByteVector],
    prefixes:    List[ByteVector],
    probes:      List[ByteVector],
    ranges:      List[(ByteVector, Option[ByteVector])] = Nil,
    rangeProbes: List[(ByteVector, Option[ByteVector])] = Nil
  ): Json = {
    val trie = orDie(Trie.fromEntries[Sha256](mode, entries.map { case (k, v) => k -> T.value(v) }))
    val proof = orDie(trie.prove(queries, prefixes, ranges))
    def bounds(from: ByteVector, until: Option[ByteVector]): List[(String, Json)] =
      ("from" -> from.toHex.asJson) :: until.map(u => "until" -> u.toHex.asJson).toList
    val verified = orDie(TrieProofs.verify(mode, trie.root, proof))
    Json.obj(
      "name" -> name.asJson,
      "mode" -> modeJson(mode),
      "entries" -> entries.map {
        case (k, v) => Json.obj("key" -> k.toHex.asJson, "value" -> v.toHex.asJson, "valueHash" -> T.value(v).hex.asJson)
      }.asJson,
      "root" -> trie.root.hex.asJson,
      "proof" -> Json.obj(
        "keys"     -> queries.map(_.toHex).asJson,
        "prefixes" -> prefixes.map(_.toHex).asJson,
        "ranges"   -> ranges.map { case (f, u) => Json.fromFields(bounds(f, u)) }.asJson,
        "cbor"     -> orDie(TrieProofs.encode(proof)).pipe(Cbor.encode).toHex.asJson,
        "lookups"  -> probes.map(k => Json.obj("key" -> k.toHex.asJson, "result" -> lookupJson(verified.lookup(k.bits)))).asJson,
        "prefixListings" -> prefixes.map { p =>
          Json.obj(
            "prefix" -> p.toHex.asJson,
            "entries" -> verified
              .prefix(p.bits)
              .map(_.map { case (b, v) => Json.obj("key" -> b.bytes.toHex.asJson, "valueHash" -> v.hex.asJson) })
              .asJson
          )
        }.asJson,
        "rangeListings" -> (ranges ++ rangeProbes).map {
          case (f, u) =>
            val result = verified
              .range(f.bits, u.map(_.bits))
              .fold("notCovered".asJson)(l =>
                Json.obj("entries" -> l.map { case (b, v) => Json.obj("key" -> b.bytes.toHex.asJson, "valueHash" -> v.hex.asJson) }.asJson)
              )
            Json.fromFields(bounds(f, u) :+ ("result" -> result))
        }.asJson
      )
    )
  }

  // ---- refused proofs: decoding refuses what any reader sees (Encoding), verification what the mode decides (InvalidProof)

  import ProofNode._

  /** The root a partial trie hashes to with no structural check, so that only the mode can refuse it. */
  private def rawRoot(node: ProofNode[Sha256]): cats.Eval[Digest[Sha256]] =
    node match {
      case Stub(h)     => cats.Eval.now(h)
      case PLeaf(s, v) => cats.Eval.now(T.leaf(s, v))
      case PBranch(s, v, l, r) =>
        for {
          lh <- l.traverse(c => cats.Eval.defer(rawRoot(c)))
          rh <- r.traverse(c => cats.Eval.defer(rawRoot(c)))
        } yield T.branch(s, v, lh, rh)
    }

  private def wire(node: ProofNode[Sha256]): CanonicalValue = orDie(TrieProofs.encode(TrieProof(Some(node))))

  private def records(v: CanonicalValue): Vector[CanonicalValue] = v match {
    case CanonicalValue.CArray(rs) => rs
    case _                         => Vector.empty
  }

  private def arrayOf(rs: Vector[CanonicalValue]): CanonicalValue = CanonicalValue.array(rs).fold(e => sys.error(e.toString), identity)

  private def record(fields: (String, CanonicalValue)*): CanonicalValue =
    CanonicalValue.map(fields).fold(e => sys.error(e.toString), identity)

  private def seg(n: Long, b: ByteVector): CanonicalValue = record("n" -> CanonicalValue.int(n), "b" -> CanonicalValue.bytes(b))

  private val v0: Digest[Sha256] = T.value(hex"00")
  private val leaf7: ProofNode[Sha256] = PLeaf(BitVector.low(7), v0)
  private val leafRecord: CanonicalValue = record("s" -> seg(7, hex"00"), "v" -> CanonicalValue.bytes(v0.bytes))

  private def malformedRow(name: String, mode: KeyMode, proof: CanonicalValue, root: Digest[Sha256], expected: String): Json = {
    val result = TrieProofs.decode[Sha256](proof).flatMap(TrieProofs.verify(mode, root, _)).fold(_.productPrefix, _ => "ok")
    require(result == expected, s"malformed trie proof $name: expected $expected, got $result")
    Json.obj(
      "name"  -> name.asJson,
      "mode"  -> modeJson(mode),
      "root"  -> root.hex.asJson,
      "cbor"  -> Cbor.encode(proof).toHex.asJson,
      "error" -> result.asJson
    )
  }

  private def refusedByMode(name: String, mode: KeyMode, node: ProofNode[Sha256]): Json =
    malformedRow(name, mode, wire(node), rawRoot(node).value, "InvalidProof")

  private def refusedByDecoder(name: String, mode: KeyMode, proof: CanonicalValue): Json =
    malformedRow(name, mode, proof, T.empty, "Encoding")

  def malformed: List[Json] = {
    val fixed1 = orDie(KeyMode.fixed(1))
    val fixed2 = orDie(KeyMode.fixed(2))
    val variable1 = orDie(KeyMode.variable(1))
    val oneChild = wire(PBranch(BitVector.empty, None, Some(leaf7), None))
    val both = wire(PBranch(BitVector.empty, None, Some(leaf7), Some(leaf7)))
    List(
      // the branch invariant is a property of one record: the decoder refuses it, whatever the mode
      refusedByDecoder("branch-one-child-no-value", variable1, oneChild),
      refusedByDecoder("branch-one-child-no-value-fixed", fixed1, oneChild),
      refusedByDecoder(
        "branch-no-children",
        variable1,
        arrayOf(Vector(record("s" -> seg(0, ByteVector.empty), "c" -> CanonicalValue.int(0))))
      ),
      refusedByDecoder(
        "non-zero-padding",
        variable1,
        arrayOf(Vector(record("s" -> seg(7, hex"01"), "v" -> CanonicalValue.bytes(v0.bytes))))
      ),
      refusedByDecoder(
        "segment-length-mismatch",
        variable1,
        arrayOf(Vector(record("s" -> seg(9, hex"00"), "v" -> CanonicalValue.bytes(v0.bytes))))
      ),
      refusedByDecoder("short-digest", variable1, arrayOf(Vector(record("h" -> CanonicalValue.bytes(ByteVector.low(31)))))),
      refusedByDecoder("trailing-record", variable1, arrayOf(records(both) :+ leafRecord)),
      refusedByDecoder("unfinished-branch", variable1, arrayOf(records(both).dropRight(1))),
      // what the mode decides is refused at verification, against the root the records hash to
      refusedByMode("fixed-leaf-too-short", fixed2, PLeaf(BitVector.low(8), v0)),
      refusedByMode("fixed-leaf-too-long", fixed1, PLeaf(BitVector.low(16), v0)),
      refusedByMode("fixed-branch-value", fixed1, PBranch(BitVector.empty, Some(v0), Some(leaf7), None)),
      refusedByMode("fixed-branch-too-deep", fixed1, PBranch(BitVector.low(8), None, Some(leaf7), Some(leaf7))),
      refusedByMode("variable-leaf-unaligned", variable1, PLeaf(BitVector.low(4), v0)),
      refusedByMode("variable-leaf-too-long", variable1, PLeaf(BitVector.low(16), v0)),
      refusedByMode(
        "variable-branch-value-unaligned",
        variable1,
        PBranch(BitVector.low(4), Some(v0), Some(PLeaf(BitVector.low(3), v0)), None)
      )
    )
  }

  private def modeJson(mode: KeyMode): Json = mode match {
    case f: KeyMode.Fixed    => Json.obj("fixed" -> f.bytes.asJson)
    case v: KeyMode.Variable => Json.obj("variable" -> v.maxBytes.asJson)
  }

  def build: Json = {
    val fixed2 = orDie(KeyMode.fixed(2))
    val variable3 = orDie(KeyMode.variable(3))
    Json.obj(
      "spec"      -> "docs/specs/trie.md".asJson,
      "version"   -> 1.asJson,
      "hash"      -> "SHA-256".asJson,
      "emptyRoot" -> T.empty.hex.asJson,
      "nodeHashes" -> Json.obj(
        "leaf" -> Json.obj(
          "segment"   -> Json.obj("bits" -> 11.asJson, "bytes" -> BitVector.high(11).bytes.toHex.asJson),
          "valueHash" -> T.value(hex"01").hex.asJson,
          "hash"      -> T.leaf(BitVector.high(11), T.value(hex"01")).hex.asJson
        ),
        "branch" -> Json.obj(
          "segment"   -> Json.obj("bits" -> 0.asJson, "bytes" -> "".asJson),
          "valueHash" -> T.value(hex"02").hex.asJson,
          "left"      -> T.value(hex"03").hex.asJson,
          "hash"      -> T.branch(BitVector.empty, Some(T.value(hex"02")), Some(T.value(hex"03")), None).hex.asJson
        )
      ),
      "fixtures" -> List(
        fixture("fixed-2", fixed2, fixedEntries, List(hex"0001", hex"8001"), Nil, List(hex"0001", hex"8001", hex"0000", hex"ffff")),
        fixture("fixed-2-prefix", fixed2, fixedEntries, Nil, List(hex"00"), List(hex"0000", hex"00ff", hex"ffff")),
        fixture(
          "variable-3",
          variable3,
          variableEntries,
          List(hex"6162", hex"6163", hex""),
          Nil,
          List(hex"6162", hex"6163", hex"", hex"61", hex"ff")
        ),
        fixture("variable-3-prefix", variable3, variableEntries, Nil, List(hex"61"), List(hex"61", hex"6100", hex"62")),
        // Range proofs. Bounds are bit strings that need not be keys; a proper prefix sorts before its extensions.
        fixture(
          "fixed-2-range",
          fixed2,
          fixedEntries,
          Nil,
          Nil,
          List(hex"0001", hex"7f80", hex"8000"),
          ranges = List(hex"0001" -> Some(hex"7f81"), hex"ff00" -> None, hex"80" -> Some(hex"80")),
          rangeProbes = List(hex"0001" -> Some(hex"7f80"), hex"00" -> None, hex"" -> Some(hex"0000"))
        ),
        fixture(
          "variable-3-range",
          variable3,
          variableEntries,
          Nil,
          Nil,
          List(hex"61", hex"6162", hex"ff"),
          ranges = List(hex"61" -> Some(hex"6163"), hex"" -> Some(hex"00")),
          rangeProbes = List(hex"6162" -> Some(hex"616263"), hex"61" -> None, hex"62" -> Some(hex"ff"))
        ),
        fixture(
          "variable-3-everything",
          variable3,
          variableEntries,
          Nil,
          Nil,
          List(hex"", hex"616263", hex"6164"),
          ranges = List(hex"" -> None),
          rangeProbes = List(hex"6100" -> Some(hex"62"), hex"ff" -> None, hex"ff00" -> None)
        )
      ).asJson,
      "malformed" -> malformed.asJson
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
      expect(
        committed == TrieVectors.render,
        s"${TrieVectors.path} is stale; regenerate with: sbt \"accumulators/Test/runMain xyz.kd5ujc.accumulators.trie.TrieVectorGen\""
      )
    }
  }

  pureTest("vector fixtures exercise all three lookup answers") {
    val rendered = TrieVectors.render
    expect(
      rendered.contains("\"present\"") && rendered.contains("\"absent\"") && rendered.contains("\"notCovered\""),
      "present, absent and notCovered all appear"
    )
  }
}
