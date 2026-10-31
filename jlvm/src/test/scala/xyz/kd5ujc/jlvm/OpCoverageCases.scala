package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.MerkleCodec
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.signing._

import io.circe.Json
import io.circe.syntax._
import scodec.bits.ByteVector

import ProofFixtures._

/**
 * Writes `vectors/jlvm_op_cases.json`, so that every operation has vectors: a sweep of each operation over 0 to 3
 * arguments of each kind, which pins argument counts, type checks, the order they run in and what they cost; then
 * cases with real inputs for the operations no other vector file reaches. Every signature here is deterministic, so
 * the file regenerates identically: `sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.OpCoverageCases"`, then `JlvmVectorGen`.
 */
object OpCoverageCases extends IOApp {

  val path: Path = Paths.get("jlvm", "src", "test", "resources", "vectors", "jlvm_op_cases.json")

  final case class Case(name: String, program: String, data: CanonicalValue)

  /** `{"<op>": [{"var": "a0"}, ...]}` over the arguments as data, so a null argument stays null. */
  def probe(name: String, op: Op, args: CanonicalValue*): Case =
    Case(
      s"dilf4s ${op.wire}: $name",
      Json.obj(op.wire -> Json.fromValues(args.indices.map(i => Json.obj("var" -> s"a$i".asJson)))).noSpaces,
      m(args.zipWithIndex.map { case (v, i) => s"a$i" -> v }*)
    )

  private def surface(json: String): CanonicalValue = get(JsonSurface.parse(json))
  private def text(s:       String): CanonicalValue = get(CanonicalValue.text(s))
  private def int(n:        BigInt): CanonicalValue = CInt(n)
  private def bytes(h:      String): CanonicalValue = CBytes(hex(h))

  /** One value of each kind, and a rational. */
  val samples: List[(String, CanonicalValue)] = List(
    "null"     -> CNull,
    "boolean"  -> False,
    "integer"  -> int(7),
    "rational" -> surface("2.5"),
    "string"   -> text("ab"),
    "bytes"    -> bytes("01"),
    "array"    -> surface("[1, 2]"),
    "object"   -> surface("""{"k": 1}""")
  )

  val sweep: List[Case] =
    Op.values.toList.flatMap { op =>
      probe("no arguments", op) :: (1 to 3).toList.flatMap(n => samples.map { case (k, v) => probe(s"$n × $k", op, List.fill(n)(v)*) })
    }

  val bytesAndHashing: List[Case] = {
    val inputs = List("empty text" -> text(""), "text" -> text("abc"), "the same bytes" -> bytes("616263"), "empty bytes" -> bytes(""))
    List(Op.Sha256, Op.Sha3_256, Op.Keccak256, Op.Blake2b256).flatMap(op => inputs.map { case (n, v) => probe(n, op, v) }) ++
    List(
      probe("bytes", Op.Unhex, bytes("00ff")),
      probe("uppercase hex", Op.Unhex, text("0xAB")),
      probe("odd hex", Op.Unhex, text("0xabc")),
      probe("no prefix", Op.Unhex, text("ab")),
      probe("empty", Op.BytesToInt, bytes("")),
      probe("leading zero", Op.BytesToInt, bytes("000100")),
      probe("33 bytes", Op.BytesToInt, bytes("ff" * 33)),
      probe("zero", Op.IntToBytes, int(0)),
      probe("minimal", Op.IntToBytes, int(256)),
      probe("padded", Op.IntToBytes, int(256), int(4)),
      probe("too narrow", Op.IntToBytes, int(256), int(1)),
      probe("negative", Op.IntToBytes, int(-1)),
      probe("width above 1024", Op.IntToBytes, int(1), int(1025))
    ) ++
    List("2.5", "-2.5", "2.4", "-2.6", "7", "-0.5").flatMap(x => List(probe(x, Op.RoundHalfUp, surface(x)), probe(x, Op.Round, surface(x))))
  }

  private val message = ByteVector("transfer 10 to bob".getBytes(UTF_8))
  private val other = ByteVector("transfer 99 to eve".getBytes(UTF_8))
  private def seed(n: Int): ByteVector = ByteVector.fill(32)(n.toByte)

  /** A good signature, another message, a truncated signature, a number for a key. */
  private def scheme(op: Op, pk: ByteVector, sig: ByteVector, msg: ByteVector, wrong: ByteVector): List[Case] =
    List(
      probe("verifies", op, CBytes(pk), CBytes(msg), CBytes(sig)),
      probe("another message", op, CBytes(pk), CBytes(wrong), CBytes(sig)),
      probe("short signature", op, CBytes(pk), CBytes(msg), CBytes(sig.dropRight(1))),
      probe("a number for the key", op, int(1), CBytes(msg), CBytes(sig))
    )

  def signatures(secp: Secp256k1): List[Case] = {
    val ed = get(Ed25519.seedDerivation.fromSeed(seed(1)))
    val ec = get(secp.ecdsaSeedDerivation.fromSeed(seed(2)))
    val schnorr = get(secp.bip340SeedDerivation.fromSeed(seed(3)))
    val m32 = ByteVector.fill(32)(7)
    val bip340 = get(secp.bip340SignWithAux(schnorr.signing, get(Message32.from(m32)), ByteVector.fill(32)(0)))
    scheme(Op.Ed25519Verify, ed.verifying.bytes, Ed25519.signer.sign(ed.signing, message).bytes, message, other) ++
    scheme(Op.Secp256k1Verify, ec.verifying.bytes, secp.ecdsaSigner.sign(ec.signing, message).bytes, message, other) ++
    scheme(Op.Bip340Verify, schnorr.verifying.bytes, bip340.bytes, m32, ByteVector.fill(32)(8)) :+
    probe("a message not 32 bytes", Op.Bip340Verify, CBytes(schnorr.verifying.bytes), CBytes(message), CBytes(bip340.bytes))
  }

  def merkle(v: Json): List[Case] = {
    val leaves = get(v.hcursor.downField("leaves").as[List[String]]).map(hex)
    val trees = get(v.hcursor.downField("trees").as[List[Json]])
    val heads = trees.map(t => (get(t.hcursor.downField("size").as[Long]), str(t.hcursor.downField("root"))))
    val (n, root) = heads(5)
    val tree = committedTree(leaves, n)
    val head = m("root" -> CBytes(tree.root.bytes), "size" -> int(tree.size))
    val proof = get(tree.inclusionProof(2L).flatMap(MerkleCodec.encodeInclusion(_)))
    val consistency = cbor(get(trees(5).hcursor.downField("consistency").as[List[String]])(2))
    val (m0, oldRoot) = heads(2)
    List(
      probe(s"leaf 2 of $n", Op.MerkleVerify, head, CBytes(leaves(2)), proof),
      probe(s"another leaf", Op.MerkleVerify, head, CBytes(leaves(3)), proof),
      probe(s"a smaller size", Op.MerkleVerify, m("root" -> CBytes(tree.root.bytes), "size" -> int(n - 1)), CBytes(leaves(2)), proof),
      probe(s"$m0 to $n", Op.MerkleConsistencyVerify, headOf(oldRoot, m0), headOf(root, n), consistency),
      probe(s"$m0 to $n, wrong old root", Op.MerkleConsistencyVerify, headOf(root, m0), headOf(root, n), consistency),
      probe(s"$m0 to $n, malformed proof", Op.MerkleConsistencyVerify, headOf(oldRoot, m0), headOf(root, n), bytes("00"))
    )
  }

  def tries(v: Json): List[Case] =
    get(v.hcursor.downField("fixtures").as[List[Json]]).flatMap { fx =>
      val c = fx.hcursor
      val name = str(c.downField("name"))
      val (md, root, proof, vs) = (mode(get(c.downField("mode").as[Json])), rootOf(fx), proofOf(fx), values(fx))
      val entries = (ks: List[String]) => a(ks.map(k => a(bytes(k), CBytes(vs(k))))*)
      val lookups = get(c.downField("proof").downField("lookups").as[List[Json]])
        .groupBy(_.hcursor.downField("result").as[String].getOrElse("present"))
        .toList
        .sortBy(_._1)
        .flatMap {
          case (kind, l :: _) =>
            val key = str(l.hcursor.downField("key"))
            val claimed = if (kind == "present") CBytes(vs(key)) else CNull
            List(
              probe(s"$name, $kind $key", Op.TrieVerify, md, root, bytes(key), claimed, proof),
              probe(s"$name, $kind $key, another value", Op.TrieVerify, md, root, bytes(key), bytes("01"), proof)
            )
          case _ => Nil
        }
      val prefixes = get(c.downField("proof").downField("prefixListings").as[List[Json]]).take(1).flatMap { l =>
        val prefix = str(l.hcursor.downField("prefix"))
        val listed = get(l.hcursor.downField("entries").as[List[Json]]).map(e => str(e.hcursor.downField("key")))
        List(
          probe(s"$name, prefix $prefix", Op.TriePrefixVerify, md, root, bytes(prefix), entries(listed), proof),
          probe(s"$name, prefix $prefix, an entry left out", Op.TriePrefixVerify, md, root, bytes(prefix), entries(listed.drop(1)), proof)
        )
      }
      val ranges = get(c.downField("proof").downField("rangeListings").as[List[Json]]).take(1).flatMap { l =>
        val from = str(l.hcursor.downField("from"))
        val until = rangeEnd(l).fold[CanonicalValue](CNull)(CBytes(_))
        val listed =
          l.hcursor.downField("result").downField("entries").as[List[Json]].getOrElse(Nil).map(e => str(e.hcursor.downField("key")))
        List(
          probe(s"$name, range from $from", Op.TrieRangeVerify, md, root, bytes(from), until, entries(listed), proof),
          probe(
            s"$name, range from $from, an entry left out",
            Op.TrieRangeVerify,
            md,
            root,
            bytes(from),
            until,
            entries(listed.drop(1)),
            proof
          )
        )
      }
      lookups ++ prefixes ++ ranges
    }

  private def render(cases: List[Case]): String =
    Json
      .obj(
        "source" -> "dilf4s: every operation over 0 to 3 arguments of each kind, then real inputs for the operations no other file reaches (OpCoverageCases)".asJson,
        "cases" -> cases.map { c =>
          Json.obj("name" -> c.name.asJson, "program" -> c.program.asJson, "data" -> get(JsonSurface.render(c.data)).asJson)
        }.asJson
      )
      .spaces2 + "\n"

  def run(args: List[String]): IO[ExitCode] =
    (Secp256k1.load[IO], vector("merkle_v1.json"), vector("trie_v1.json"))
      .mapN((secp, mk, tr) => sweep ++ bytesAndHashing ++ signatures(secp) ++ merkle(mk) ++ tries(tr))
      .flatMap(cases => IO.blocking(Files.write(path, render(cases).getBytes(UTF_8))))
      .as(ExitCode.Success)
}
