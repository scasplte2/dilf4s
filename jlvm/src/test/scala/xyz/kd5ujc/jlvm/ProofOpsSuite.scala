package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
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

  /** Not covered is a `Crypto` error (jlvm.md §6) carrying the typed `TrieError.NotCovered`. */
  private def notCovered(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Crypto(_, m)) => m.startsWith("NotCovered(")
    case _                            => false
  }

  // ---- Merkle

  /**
   * merkle_v1 and trie_v1 are built with the byte-level APIs. Programs verify value-committed structures, so the
   * Merkle and trie tests rebuild the vectors' trees from their leaves and entries with ValueCommitment, proving the
   * same queries; the consistency test uses the vectors' heads as they are.
   */
  private def committedTree(leaves: List[ByteVector], n: Long): MerkleTree[Sha256] =
    get(MerkleTree.fromLeaves[Sha256, CanonicalValue](leaves.take(n.toInt).map(CBytes(_))))

  test("merkle_verify: every inclusion proof of merkle_v1 verifies against its head; other leaves and sizes do not") { f =>
    IO {
      val leaves = get(f.merkle.hcursor.downField("leaves").as[List[String]]).map(hex)
      val trees = get(f.merkle.hcursor.downField("trees").as[List[Json]])
      expect.same(9, trees.size) |+| trees.foldMap { t =>
        val c = t.hcursor
        val n = get(c.downField("size").as[Long])
        val tree = committedTree(leaves, n)
        val root = tree.root.hex
        get(c.downField("inclusion").as[List[String]]).indices.toList.foldMap { i =>
          val proof = get(tree.inclusionProof(i.toLong).flatMap(MerkleCodec.encodeInclusion(_)))
          locally {
            val other = leaves((i + 1) % leaves.size)
            expect
              .same(Right(True), call(f.jlvm, Op.MerkleVerify, headOf(root, n), CBytes(leaves(i)), proof))
              .and(
                expect(
                  other == leaves(i) || call(f.jlvm, Op.MerkleVerify, headOf(root, n), CBytes(other), proof) == Right(False),
                  s"another leaf, size $n"
                )
              )
              .and(expect.same(Right(False), call(f.jlvm, Op.MerkleVerify, headOf(root, n + 1), CBytes(leaves(i)), proof)))
          }
        }
      }
    }
  }

  test("every malformed proof of merkle_v1 and trie_v1 is a Crypto error, whichever kind the decoder or verifier gives") { f =>
    IO {
      val emptyRoot = str(f.trie.hcursor.downField("emptyRoot"))
      val merkle = get(f.merkle.hcursor.downField("malformed").as[List[Json]]).foldMap { c =>
        val (name, proof) = (str(c.hcursor.downField("name")), cbor(str(c.hcursor.downField("cbor"))))
        val r = str(c.hcursor.downField("kind")) match {
          case "inclusion" => call(f.jlvm, Op.MerkleVerify, headOf(emptyRoot, 3), CBytes(ByteVector(1)), proof)
          case _           => call(f.jlvm, Op.MerkleConsistencyVerify, headOf(emptyRoot, 1), headOf(emptyRoot, 3), proof)
        }
        expect(isCrypto(r), s"merkle $name: $r")
      }
      val trie = get(f.trie.hcursor.downField("malformed").as[List[Json]]).foldMap { c =>
        val md = get(c.hcursor.downField("mode").as[Json])
        // a key the mode accepts: all zeros in fixed mode, empty in variable mode
        val key = md.hcursor.get[Int]("fixed").fold(_ => ByteVector.empty, n => ByteVector.low(n.toLong))
        val r = call(
          f.jlvm,
          Op.TrieVerify,
          mode(md),
          CBytes(hex(str(c.hcursor.downField("root")))),
          CBytes(key),
          CNull,
          cbor(str(c.hcursor.downField("cbor")))
        )
        expect(isCrypto(r), s"trie ${str(c.hcursor.downField("name"))}: $r")
      }
      merkle |+| trie
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

  test("merkle_verify commits leaves as MerkleTree.leaf does: bytes as they are, other values as canonical CBOR") { f =>
    IO {
      val leaves =
        List[CanonicalValue](CBytes(ByteVector(1, 2, 3)), m("to" -> CBytes(ByteVector(9)), "amount" -> CInt(BigInt(5))), CInt(BigInt(7)))
      val tree = get(MerkleTree.fromLeaves[Sha256, CanonicalValue](leaves))
      val head = m("root" -> CBytes(tree.root.bytes), "size" -> CInt(BigInt(tree.size)))
      leaves.zipWithIndex.foldMap {
        case (leaf, i) =>
          expect.same(
            Right(True),
            call(f.jlvm, Op.MerkleVerify, head, leaf, get(tree.inclusionProof(i.toLong).flatMap(MerkleCodec.encodeInclusion(_))))
          )
      }
    }
  }

  test("a proof of the bytes 0x6161 does not verify the text \"a\", whose CBOR is 0x6161, in a Merkle tree or a trie") { f =>
    IO {
      val bytes = CBytes(ByteVector(0x61, 0x61))
      val a = text("a")
      val tree = get(MerkleTree.fromLeaves[Sha256, CanonicalValue](List(bytes, CInt(BigInt(1)))))
      val head = m("root" -> CBytes(tree.root.bytes), "size" -> CInt(BigInt(tree.size)))
      val proof = get(tree.inclusionProof(0L).flatMap(MerkleCodec.encodeInclusion(_)))
      val key = ByteVector(0x00, 0x01)
      val km = get(KeyMode.fixed(2))
      val trie = get(Trie.empty[Sha256](km).putCanonical(key, bytes))
      val trieProof = get(trie.prove(List(key)).flatMap(TrieProofs.encode(_)))
      val md = m("fixed" -> CInt(BigInt(2)))
      def claim(v: CanonicalValue) = call(f.jlvm, Op.TrieVerify, md, CBytes(trie.root.bytes), CBytes(key), v, trieProof)
      expect
        .same(Right(True), call(f.jlvm, Op.MerkleVerify, head, bytes, proof))
        .and(expect.same(Right(False), call(f.jlvm, Op.MerkleVerify, head, a, proof)))
        .and(expect(call(f.jlvm, Op.MerkleVerify, head, a, proof, text("rfc6962")).isLeft, "rfc6962 leaves are bytes only"))
        .and(expect.same(Right(True), claim(bytes)))
        .and(expect.same(Right(False), claim(a)))
        .and(
          expect(
            call(
              f.jlvm,
              Op.TrieVerify,
              m("fixed" -> CInt(BigInt(2)), "raw" -> True),
              CBytes(trie.root.bytes),
              CBytes(key),
              bytes,
              trieProof
            ).isLeft,
            "there is no raw mode"
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

  private def keyMode(fx: Json): KeyMode =
    fx.hcursor
      .downField("mode")
      .downField("fixed")
      .as[Int]
      .map(KeyMode.fixed)
      .orElse(
        fx.hcursor.downField("mode").downField("variable").as[Int].map(KeyMode.variable)
      )
      .fold(e => sys.error(e.toString), r => get(r))

  /** A trie_v1 fixture rebuilt with its values committed as programs claim them, proving the fixture's own queries. */
  private def committedTrie(fx: Json): Trie[Sha256] =
    get(
      Trie.fromEntries[Sha256](
        keyMode(fx),
        values(fx).toList.map { case (k, v) => hex(k) -> TrieHashing[Sha256].value(ValueCommitment.bytes(CBytes(v))) }
      )
    )

  private def rootOf(fx: Json): CanonicalValue = CBytes(committedTrie(fx).root.bytes)

  private def proofOf(fx: Json): CanonicalValue = {
    val q = fx.hcursor.downField("proof")
    get(
      committedTrie(fx)
        .prove(
          get(q.downField("keys").as[List[String]]).map(hex),
          get(q.downField("prefixes").as[List[String]]).map(hex),
          get(q.downField("ranges").as[List[Json]]).map(r => hex(str(r.hcursor.downField("from"))) -> rangeEnd(r))
        )
        .flatMap(TrieProofs.encode(_))
    )
  }

  private def rangeEnd(r: Json): Option[ByteVector] = r.hcursor.downField("until").as[String].toOption.map(hex)

  private def values(fx: Json): Map[String, ByteVector] =
    get(fx.hcursor.downField("entries").as[List[Json]])
      .map(e => str(e.hcursor.downField("key")) -> hex(str(e.hcursor.downField("value"))))
      .toMap

  test("trie_verify: every lookup of trie_v1 (present, absent, not covered)") { f =>
    IO {
      expect.same(7, fixtures(f).size) |+| fixtures(f).foldMap { fx =>
        val c = fx.hcursor
        val (md, root, proof) = (
          mode(get(c.downField("mode").as[Json])),
          rootOf(fx),
          proofOf(fx)
        )
        val vs = values(fx)
        get(c.downField("proof").downField("lookups").as[List[Json]]).foldMap { l =>
          val key = str(l.hcursor.downField("key"))
          val run = (v: CanonicalValue) => call(f.jlvm, Op.TrieVerify, md, root, CBytes(hex(key)), v, proof)
          l.hcursor.downField("result").as[String].toOption match {
            case Some("absent")     => expect.same(Right(True), run(CNull)).and(expect.same(Right(False), run(CBytes(ByteVector(1)))))
            case Some("notCovered") => expect(notCovered(run(CNull)), s"${str(c.downField("name"))} $key is not covered: ${run(CNull)}")
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
          rootOf(fx),
          proofOf(fx)
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

  test("trie_range_verify: every range listing of trie_v1, complete or not covered, and a missing entry is caught") { f =>
    IO {
      fixtures(f).foldMap { fx =>
        val c = fx.hcursor
        val (md, root, proof) = (mode(get(c.downField("mode").as[Json])), rootOf(fx), proofOf(fx))
        val vs = values(fx)
        get(c.downField("proof").downField("rangeListings").as[List[Json]]).foldMap { l =>
          val from = CBytes(hex(str(l.hcursor.downField("from"))))
          val until = rangeEnd(l).fold[CanonicalValue](CNull)(CBytes(_))
          val entries = (ks: List[String]) => a(ks.map(k => a(CBytes(hex(k)), CBytes(vs(k))))*)
          val run = (ks: List[String]) => call(f.jlvm, Op.TrieRangeVerify, md, root, from, until, entries(ks), proof)
          l.hcursor.downField("result").as[String].toOption match {
            case Some(_) => expect(notCovered(run(Nil)), s"${str(c.downField("name"))} ${l.noSpaces} is not covered: ${run(Nil)}")
            case None =>
              val listed = get(l.hcursor.downField("result").downField("entries").as[List[Json]]).map(e => str(e.hcursor.downField("key")))
              expect
                .same(Right(True), run(listed))
                .and(expect(listed.isEmpty || run(listed.drop(1)) == Right(False), "an omitted entry"))
          }
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
      val trie = get(Trie.fromEntries[Sha256](km, vs.toList.map { case (k, v) => hex(k) -> T.value(ValueCommitment.bytes(CBytes(v))) }))
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

  private def fixture(f: Fixtures, name: String): Json =
    fixtures(f).find(x => str(x.hcursor.downField("name")) == name).getOrElse(sys.error(s"no fixture $name"))
  private def text(s: String): CanonicalValue = get(CanonicalValue.text(s))

  test("trie_verify: a wrong root is false; an undecodable proof or a short root is a crypto error; a text root a type error") { f =>
    IO {
      val fx = fixture(f, "fixed-2")
      val md = m("fixed" -> CInt(BigInt(2)))
      val key = CBytes(hex("0001"))
      val value = CBytes(values(fx)("0001"))
      val run = (root: CanonicalValue, v: CanonicalValue, proof: CanonicalValue) => call(f.jlvm, Op.TrieVerify, md, root, key, v, proof)
      expect
        .same(Right(True), run(rootOf(fx), value, proofOf(fx)))
        .and(expect.same(Right(False), run(CBytes(ByteVector.fill(32)(0)), value, proofOf(fx))))
        .and(expect.same(Right(False), run(CBytes(ByteVector.fill(32)(0xff)), CNull, proofOf(fx))))
        .and(expect(isCrypto(run(rootOf(fx), value, m("nope" -> text("not a proof")))), "an undecodable proof"))
        .and(expect(isCrypto(run(CBytes(ByteVector(1, 2, 3)), value, proofOf(fx))), "a 3-byte root"))
        .and(
          expect(
            run(text("not-hex"), value, proofOf(fx)) match {
              case Left(JlvmError.Type(_, _)) => true
              case _                          => false
            },
            "a text root is a type error"
          )
        )
    }
  }

  test("trie_prefix_verify: altered, swapped or extra entries are false; a partial proof is not covered; junk is malformed") { f =>
    IO {
      // fixed-2-prefix proves prefix 00 (keys 0000, 0001, 00ff); fixed-2 holds the same entries, so the same root, but
      // its key proof leaves 0000 as a stub: under prefix 00 it is not covered
      val full = fixture(f, "fixed-2-prefix")
      val partial = fixture(f, "fixed-2")
      val vs = values(full)
      val md = m("fixed" -> CInt(BigInt(2)))
      val entries = (es: List[(String, ByteVector)]) => a(es.map(e => a(CBytes(hex(e._1)), CBytes(e._2)))*)
      val listed = List("0000", "0001", "00ff").map(k => k -> vs(k))
      val run = (es: List[(String, ByteVector)], proof: CanonicalValue) =>
        call(f.jlvm, Op.TriePrefixVerify, md, rootOf(full), CBytes(hex("00")), entries(es), proof)
      expect
        .same(rootOf(full), rootOf(partial))
        .and(expect(vs("0000") != vs("0001"), "the swap below exchanges different values"))
        .and(expect.same(Right(True), run(listed, proofOf(full))))
        .and(expect.same(Right(False), run(listed.updated(1, "0001" -> (vs("0001") :+ 0)), proofOf(full))))
        .and(expect.same(Right(False), run(List("0000" -> vs("0001"), "0001" -> vs("0000"), "00ff" -> vs("00ff")), proofOf(full))))
        .and(expect.same(Right(False), run(listed.take(2) ++ List("0080" -> ByteVector(9)) ++ listed.drop(2), proofOf(full))))
        .and(expect(notCovered(run(listed.take(2), proofOf(partial))), "a key proof does not cover the prefix"))
        .and(expect(isCrypto(run(listed, m("nope" -> text("x")))), "an undecodable proof"))
    }
  }

  test("a revocation gate: denied when the key is proven present, allowed when proven absent, an error when not covered") { f =>
    IO {
      val fx = fixture(f, "fixed-2")
      val program = get(
        Parser.parse(
          """{"if": [{"trie_verify": [{"var": "mode"}, {"var": "root"}, {"var": "key"}, null, {"var": "proof"}]}, "allowed", "denied"]}"""
        )
      )
      def gate(key: String) = {
        val data =
          m("mode" -> m("fixed" -> CInt(BigInt(2))), "root" -> rootOf(fx), "key" -> CBytes(hex(key)), "proof" -> proofOf(fx))
        f.jlvm.evaluate(program, data, Map.empty, Config()).result
      }
      expect
        .same(Right(text("denied")), gate("0001"))
        .and(expect.same(Right(text("allowed")), gate("8001")))
        .and(expect(isCrypto(gate("0000")), "not covered is neither present nor absent"))
    }
  }
}
