package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleHashing, MerkleTree}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieHashing, TrieProofs}
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Sha256

import io.circe.{ACursor, Json}
import scodec.bits.ByteVector
import weaver.{Expectations, IOSuite}

/** The Merkle and trie proof operations, run over the committed conformance vectors (merkle_v1, trie_v1). */
object ProofOpsSuite extends IOSuite {

  final case class Fixtures(jlvm: Jlvm, merkle: Json, trie: Json)
  type Res = Fixtures

  private def read(name: String): IO[Json] =
    IO(new String(Files.readAllBytes(Paths.get("shared", "vectors", name)), UTF_8)).flatMap(t => io.circe.parser.parse(t).liftTo[IO])

  def sharedResource: Resource[IO, Fixtures] =
    Resource.eval((Jlvm.load[IO], read("merkle_v1.json"), read("trie_v1.json")).mapN(Fixtures.apply))

  /** `{"<op>": [{"var": "a0"}, ...]}` over the arguments as data, so null arguments stay null. */
  private def call(jlvm: Jlvm, op: Op, args: CanonicalValue*): Either[JlvmError, CanonicalValue] = {
    val names = args.indices.map(i => s"a$i").toVector
    val program =
      Program.from(Expr.Apply(op, names.map(n => Expr.Var(VarRef.Data(Vector(n)), scala.None)))).fold(e => sys.error(e.message), identity)
    val data = CanonicalValue.map(names.zip(args)).fold(e => sys.error(e.toString), identity)
    jlvm.evaluate(program, data, Map.empty, Config()).result
  }

  private def get[A](e:    Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def hex(s:       String): ByteVector = get(ByteVector.fromHexDescriptive(s))
  private def cbor(s:      String): CanonicalValue = get(Cbor.decode(hex(s)))
  private def str(c:       ACursor): String = get(c.as[String])
  private def m(kvs:       (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs))
  private def a(vs:        CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))
  private def headOf(root: String, size: Long): CanonicalValue = m("root" -> CBytes(hex(root)), "size" -> CInt(BigInt(size)))

  private def isCrypto(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Crypto(_, _)) => true
    case _                            => false
  }

  // ---- Merkle

  test("merkle_verify: every inclusion proof of merkle_v1 verifies against its head; other leaves and sizes do not") { f =>
    IO {
      val leaves = get(f.merkle.hcursor.downField("leaves").as[List[String]]).map(hex)
      val trees = get(f.merkle.hcursor.downField("trees").as[List[Json]])
      expect.same(9, trees.size) |+| trees.foldMap { t =>
        val c = t.hcursor
        val n = get(c.downField("size").as[Long])
        val root = str(c.downField("root"))
        get(c.downField("inclusion").as[List[String]]).zipWithIndex.foldMap {
          case (proof, i) =>
            val other = leaves((i + 1) % leaves.size)
            expect
              .same(Right(True), call(f.jlvm, Op.MerkleVerify, headOf(root, n), CBytes(leaves(i)), cbor(proof)))
              .and(
                expect(
                  other == leaves(i) || call(f.jlvm, Op.MerkleVerify, headOf(root, n), CBytes(other), cbor(proof)) == Right(False),
                  s"another leaf, size $n"
                )
              )
              .and(expect.same(Right(False), call(f.jlvm, Op.MerkleVerify, headOf(root, n + 1), CBytes(leaves(i)), cbor(proof))))
        }
      }
    }
  }

  test("merkle_consistency_verify: every consistency proof of merkle_v1, between the heads it names") { f =>
    IO {
      val trees = get(f.merkle.hcursor.downField("trees").as[List[Json]])
      val heads = trees.map(t => (get(t.hcursor.downField("size").as[Long]), str(t.hcursor.downField("root"))))
      expect.same(9, trees.size) |+| trees.foldMap { t =>
        val (n, root) = (get(t.hcursor.downField("size").as[Long]), str(t.hcursor.downField("root")))
        get(t.hcursor.downField("consistency").as[List[String]]).zipWithIndex.foldMap {
          case (proof, old) =>
            val (m0, oldRoot) = heads(old)
            expect
              .same(Right(True), call(f.jlvm, Op.MerkleConsistencyVerify, headOf(oldRoot, m0), headOf(root, n), cbor(proof)))
              .and(
                expect(
                  m0 == n || call(f.jlvm, Op.MerkleConsistencyVerify, headOf(root, m0), headOf(root, n), cbor(proof)) == Right(False),
                  s"wrong old root, $m0 of $n"
                )
              )
        }
      }
    }
  }

  test("merkle_verify: the rfc6962 profile, and malformed input") { f =>
    IO {
      val leaves = get(f.merkle.hcursor.downField("leaves").as[List[String]]).map(hex)
      val rfc = MerkleHashing.rfc6962[Sha256]
      val tree = MerkleTree.fromLeafHashes[Sha256](leaves.map(rfc.leaf))(using rfc)
      val proof = get(tree.inclusionProof(2).flatMap(MerkleCodec.encodeInclusion(_)))
      val head = m("root" -> CBytes(tree.root.bytes), "size" -> CInt(BigInt(tree.size)))
      val outOfRange = m("index" -> CInt(BigInt(9)), "size" -> CInt(BigInt(8)), "path" -> a())
      expect
        .same(Right(True), call(f.jlvm, Op.MerkleVerify, head, CBytes(leaves(2)), proof, CanonicalValue.text("rfc6962").toOption.get))
        .and(expect.same(Right(False), call(f.jlvm, Op.MerkleVerify, head, CBytes(leaves(2)), proof)))
        .and(expect(isCrypto(call(f.jlvm, Op.MerkleVerify, head, CBytes(leaves(2)), outOfRange)), "index out of range"))
        .and(
          expect(
            isCrypto(call(f.jlvm, Op.MerkleVerify, m("root" -> CBytes(ByteVector(1))), CBytes(leaves(2)), proof)),
            "a head without a size"
          )
        )
    }
  }

  // ---- tries

  private def mode(j: Json): CanonicalValue =
    j.hcursor
      .downField("fixed")
      .as[Int]
      .map(n => m("fixed" -> CInt(BigInt(n))))
      .orElse(j.hcursor.downField("variable").as[Int].map(n => m("variable" -> CInt(BigInt(n)))))
      .fold(e => sys.error(e.toString), identity)

  private def fixtures(f: Fixtures): List[Json] = get(f.trie.hcursor.downField("fixtures").as[List[Json]])

  private def values(fx: Json): Map[String, ByteVector] =
    get(fx.hcursor.downField("entries").as[List[Json]])
      .map(e => str(e.hcursor.downField("key")) -> hex(str(e.hcursor.downField("value"))))
      .toMap

  test("trie_verify: every lookup of trie_v1 (present, absent, not covered)") { f =>
    IO {
      expect.same(4, fixtures(f).size) |+| fixtures(f).foldMap { fx =>
        val c = fx.hcursor
        val (md, root, proof) = (
          mode(get(c.downField("mode").as[Json])),
          CBytes(hex(str(c.downField("root")))),
          cbor(str(c.downField("proof").downField("cbor")))
        )
        val vs = values(fx)
        get(c.downField("proof").downField("lookups").as[List[Json]]).foldMap { l =>
          val key = str(l.hcursor.downField("key"))
          val run = (v: CanonicalValue) => call(f.jlvm, Op.TrieVerify, md, root, CBytes(hex(key)), v, proof)
          l.hcursor.downField("result").as[String].toOption match {
            case Some("absent")     => expect.same(Right(True), run(CNull)).and(expect.same(Right(False), run(CBytes(ByteVector(1)))))
            case Some("notCovered") => expect(isCrypto(run(CNull)), s"${str(c.downField("name"))} $key is not covered")
            case _ =>
              expect
                .same(Right(True), run(CBytes(vs(key))))
                .and(expect.same(Right(False), run(CBytes(vs(key) :+ 0))))
                .and(expect.same(Right(False), run(CNull)))
          }
        }
      }
    }
  }

  test("trie_prefix_verify: every prefix listing of trie_v1 is complete, and a missing entry is caught") { f =>
    IO {
      fixtures(f).foldMap { fx =>
        val c = fx.hcursor
        val (md, root, proof) = (
          mode(get(c.downField("mode").as[Json])),
          CBytes(hex(str(c.downField("root")))),
          cbor(str(c.downField("proof").downField("cbor")))
        )
        val vs = values(fx)
        get(c.downField("proof").downField("prefixListings").as[List[Json]]).foldMap { l =>
          val prefix = CBytes(hex(str(l.hcursor.downField("prefix"))))
          val listed = get(l.hcursor.downField("entries").as[List[Json]]).map(e => str(e.hcursor.downField("key")))
          val entries = (ks: List[String]) => a(ks.map(k => a(CBytes(hex(k)), CBytes(vs(k))))*)
          expect
            .same(Right(True), call(f.jlvm, Op.TriePrefixVerify, md, root, prefix, entries(listed), proof))
            .and(
              expect(
                listed.isEmpty || call(f.jlvm, Op.TriePrefixVerify, md, root, prefix, entries(listed.drop(1)), proof) == Right(False),
                "an omitted entry"
              )
            )
            .and(
              expect(
                listed.size < 2 || call(f.jlvm, Op.TriePrefixVerify, md, root, prefix, entries(listed.reverse), proof) == Right(False),
                "out of key order"
              )
            )
        }
      }
    }
  }

  test("trie_range_verify: a range proof lists exactly [from, until); a key proof does not cover a range") { f =>
    IO {
      val fx = fixtures(f).find(x => str(x.hcursor.downField("name")) == "fixed-2").get
      val vs = values(fx)
      val km = get(KeyMode.fixed(2))
      val T = TrieHashing[Sha256]
      val trie = get(Trie.fromEntries[Sha256](km, vs.toList.map { case (k, v) => hex(k) -> T.value(v) }))
      val (from, until) = (hex("0001"), Option(hex("8000")))
      val proof = get(trie.prove(Nil, Nil, List(from -> until)).flatMap(TrieProofs.encode(_)))
      val keyProof = get(trie.prove(List(hex("0001"))).flatMap(TrieProofs.encode(_)))
      val inRange = vs.keys.toList.sorted.filter(k => k >= "0001" && k < "8000")
      val entries = (ks: List[String]) => a(ks.map(k => a(CBytes(hex(k)), CBytes(vs(k))))*)
      val md = m("fixed" -> CInt(BigInt(2)))
      val root = CBytes(trie.root.bytes)
      expect
        .same(List("0001", "00ff", "7f80"), inRange)
        .and(expect.same(Right(True), call(f.jlvm, Op.TrieRangeVerify, md, root, CBytes(from), CBytes(until.get), entries(inRange), proof)))
        .and(
          expect.same(
            Right(False),
            call(f.jlvm, Op.TrieRangeVerify, md, root, CBytes(from), CBytes(until.get), entries(inRange.drop(1)), proof)
          )
        )
        .and(
          expect(
            isCrypto(call(f.jlvm, Op.TrieRangeVerify, md, root, CBytes(from), CBytes(until.get), entries(inRange), keyProof)),
            "not covered"
          )
        )
        .and(expect(isCrypto(call(f.jlvm, Op.TrieVerify, md, root, CBytes(hex("000102")), CNull, keyProof)), "a key of the wrong length"))
    }
  }
}
