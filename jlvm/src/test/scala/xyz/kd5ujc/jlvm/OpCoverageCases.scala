package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.ProofInput
import xyz.kd5ujc.accumulators.merkle.MerkleCodec
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalValue}
import xyz.kd5ujc.core.circe.CanonicalCirce
import xyz.kd5ujc.crypto.sigma.Secp256k1Group
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf.{EcVrfTai, Vrf}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.ByteVector

import ProofFixtures._

/**
 * Writes `vectors/jlvm_op_cases.json`, so that every operation has vectors: a sweep of each operation over 0 to 3
 * arguments of each kind, which pins argument counts, type checks, the order they run in and what they cost; then
 * cases with real inputs for the operations no other vector file reaches, ASCII-only case mapping on non-ASCII text,
 * the order in which `sigma_verify` refuses an oversized proposition, reads that search callback frames with long
 * keys, how program text is read (repeated names, numbers as paths and literals, null fields, depth), and each outcome
 * an operation gives at a program's root that no other row gives. Every signature here is deterministic, so the file
 * regenerates identically:
 * `sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.OpCoverageCases"`, then `JlvmVectorGen`.
 */
object OpCoverageCases extends IOApp {

  val path: Path = Paths.get("jlvm", "src", "test", "resources", "vectors", "jlvm_op_cases.json")

  /**
   * `gasLimit` replaces the default limit, for a case whose work the default does not cover; `built` binds host names to
   * values too heavy to write out (`BuiltValues` recipes); `env` binds host names to values as JSON surface text.
   */
  final case class Case(
    name:     String,
    program:  String,
    data:     CanonicalValue,
    gasLimit: Option[Long] = scala.None,
    built:    Map[String, BuiltValues.Built] = Map.empty,
    env:      Map[String, String] = Map.empty
  )

  /** `{"<op>": [{"var": "a0"}, ...]}` over the arguments as data, so a null argument stays null. */
  def probe(name: String, op: Op, args: CanonicalValue*): Case =
    Case(
      s"dilf4s ${op.wire}: $name",
      Json.obj(op.wire -> Json.fromValues(args.indices.map(i => Json.obj("var" -> s"a$i".asJson)))).noSpaces,
      m(args.zipWithIndex.map { case (v, i) => s"a$i" -> v }*)
    )

  private def surface(json: String): CanonicalValue = get(JsonSurface.parse(json))
  private def text(s:       String): CanonicalValue = get(CanonicalValue.text(s))
  private def int(n:        BigInt): CanonicalValue = xyz.kd5ujc.core.canonical.CanonicalGens.int(n)
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

  /** ASCII-only case mapping (jlvm.md §2): Unicode letters, one-to-many mappings and final sigma are left alone. */
  val casing: List[Case] = {
    val inputs = List(
      "ASCII"                              -> "HeLLo, World 1",
      "sharp s"                            -> "stra\u00dfe STRASSE",
      "dotted and dotless I"               -> "I \u0130 \u0131 i",
      "final sigma"                        -> "\u039f\u03a3 \u03a3\u039f \u03c3\u03c2",
      "precomposed and decomposed e acute" -> "\u00c9 \u00e9 E\u0301 e\u0301",
      "Garay (Unicode 16)"                 -> "\ud803\udd50 \ud803\udd70",
      "Cherokee"                           -> "\u13a0 \uab70"
    )
    inputs.flatMap { case (n, x) => List(probe(n, Op.Lower, text(x)), probe(n, Op.Upper, text(x))) }
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

  /**
   * ZIP-215's non-canonical encodings (`y` at or above `p`, reduced mod `p`), from `signing_v1.json`'s small-order
   * rows: a key and an `R` with one, and `x = 0` with the sign bit set. Each verifies; the challenge hashes the
   * encodings as given. SP1's curve25519-dalek patch refuses a non-canonical `y` unless it is reduced first.
   */
  val nonCanonicalEd25519: List[Case] = {
    val zcash = "5a63617368"
    val identityR = "01" + "00" * 63
    val pPlus1Signed = "ee" + "ff" * 31
    List(
      ("a key with y = p", "ed" + "ff" * 30 + "7f", identityR),
      ("a key with y = p + 1 and the sign bit", pPlus1Signed, identityR),
      ("an R with y = p + 1", "01" + "00" * 31, "ee" + "ff" * 30 + "7f" + "00" * 32),
      ("an R with y = p + 1 and the sign bit, a key with x = 0 and the sign bit", "01" + "00" * 30 + "80", pPlus1Signed + "00" * 32),
      ("a key and an R with y = p + 1 and the sign bit", pPlus1Signed, pPlus1Signed + "00" * 32)
    ).map { case (name, pk, sig) => probe(s"non-canonical: $name", Op.Ed25519Verify, bytes(pk), bytes(zcash), bytes(sig)) }
  }

  /**
   * The verifiers' root outcomes that `verdicts` leaves out: `sigma_verify` on `sigma_v1.json`'s valid proofs (true) and
   * on their proofs of another message (false) beyond a single key, from a Diffie-Hellman tuple to rings, thresholds and
   * a ZeroJoin mix; `schnorr_verify` on a flipped response; and `ecrecover` on the high-`s` twin of a signature, which
   * EIP-2 refuses. The differential fuzzer's coverage table found these missing (tools/difffuzz/README.md).
   */
  def rootVerifiers(sigma: Json): List[Case] = {
    val rows = get(sigma.hcursor.downField("cases").as[List[Json]])
    def row(name: String, kind: String): Option[Json] =
      rows.find(r => r.hcursor.get[String]("name").contains(name) && r.hcursor.get[String]("kind").contains(kind))
    def field(r: Json, k: String): CanonicalValue = get(JsonSurface.parse(get(r.hcursor.get[Json](k)).noSpaces))
    val sigmaRows = for {
      name <- List("dhtuple", "ring of three, the middle signs", "2-of-3, the outer two", "ZeroJoin half-mix, Bob spends")
      kind <- List("valid", "other-message")
      r    <- row(name, kind).toList
    } yield probe(s"$name, $kind", Op.SigmaVerify, field(r, "proposition"), field(r, "proof"), field(r, "message"))
    val schnorrRows = for {
      r  <- row("dlog", "flipped-response").toList
      pk <- r.hcursor.downField("proposition").get[String]("pk").toOption.toList
    } yield probe("dlog, flipped-response", Op.SchnorrVerify, surface(s"\"$pk\""), field(r, "message"), field(r, "proof"))
    val digest = EthKeys.keccak("transfer 10 to bob")
    val signature = EthKeys.sign(BigInt(7), digest)
    // (r, n - s, the other v) recovers the same key, so only the low-s rule refuses it
    val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
    val twinS = ByteVector((n - BigInt(1, signature.slice(32, 64).toArray)).toByteArray).takeRight(32).padLeft(32)
    val twin = signature.take(32) ++ twinS :+ (if (signature(64) == 27) 28 else 27).toByte
    sigmaRows ++ schnorrRows :+ probe("the high-s twin of a signature is refused", Op.Ecrecover, CBytes(digest), CBytes(twin))
  }

  /**
   * The order in which `sigma_verify` refuses an oversized proposition (sigma.md §3): its weight before anything is
   * decoded, a connective's width before its children, the leaf bound before a leaf's points. The JLVM reports each as
   * `Crypto` after charging for the proposition's weight, so these pin that each is refused, at what cost, and that the
   * refusal does not wait on decoding; the order itself is pinned by the unit tests of both implementations.
   */
  val sigmaBounds: List[Case] = {
    def v(name:      String): Json = Json.obj("var" -> name.asJson)
    def node(ty:     String, children: List[Json]): Json = Json.obj("type" -> ty.asJson, "children" -> Json.fromValues(children))
    def verify(prop: Json): Json = Json.obj("sigma_verify" -> Json.fromValues(List(prop, v("proof"), v("message"))))
    def let(bindings: List[(String, Json)], body: Json): Json =
      Json.obj("let" -> Json.arr(Json.fromValues(bindings.map { case (n, e) => Json.arr(n.asJson, e) }), body))
    def dlog(pk: ByteVector): CanonicalValue = m("type" -> text("dlog"), "pk" -> CBytes(pk))
    val offCurve = ByteVector(2) ++ ByteVector.fill(31)(0) :+ 5.toByte
    val data = m(
      "good"    -> dlog(Secp256k1Group.Generator.bytes),
      "bad"     -> dlog(offCurve),
      "proof"   -> bytes("00"),
      "message" -> bytes("00")
    )
    // 2^17 leaves under 17 levels of doubling, shared: weight 68·2^17 - 20 = 8,912,876, within the JLVM's 2^24
    val doubling = (2 to 17).toList.map(i => s"x$i" -> node("and", List.fill(2)(v(s"$$x${i - 1}"))))
    List(
      Case(
        "dilf4s sigma_verify: 256 children, child 0 off the curve: the width is checked before the children are decoded",
        verify(node("and", v("bad") :: List.fill(255)(v("good")))).noSpaces,
        data
      ),
      Case(
        "dilf4s sigma_verify: 4097 leaves, 17 ors of 241: refused at the 4097th leaf",
        let(List("o" -> node("or", List.fill(241)(v("good")))), verify(node("and", List.fill(17)(v("$o"))))).noSpaces,
        data,
        Option(1L << 27)
      ),
      Case(
        "dilf4s sigma_verify: heavier than the heaviest valid proposition: refused before it is decoded",
        let(("x1" -> node("and", List(v("bad"), v("good")))) :: doubling, verify(v("$x17"))).noSpaces,
        data,
        Option(1L << 32)
      )
    )
  }

  /**
   * Each outcome an operation gives at a program's root that no other row gives: `ecrecover` recovering a signer
   * (`EthKeys`, RFC 6979), `schnorr_verify` and `sigma_verify` true and false (sigma_v1.json's first valid Ergo-compatible
   * dlog and threshold proofs, then another message), and `>=` false. The SP1 guest picks its JLVM rows by what each
   * shows at the root or at an error's site, so every operation then runs there with a value, a negative verdict
   * where it gives one, and a refusal.
   */
  def verdicts(sigma: Json): List[Case] = {
    val cases = get(sigma.hcursor.downField("cases").as[List[Json]])
    def field(c: Json, name: String): String = str(c.hcursor.downField(name))
    def valid(ty: String): Json =
      cases
        .find(c =>
          field(c, "kind") == "valid" && str(c.hcursor.downField("proposition").downField("type")) == ty &&
          c.hcursor.downField("ergo").as[Boolean].contains(true)
        )
        .getOrElse(sys.error(s"sigma_v1.json: no valid Ergo-compatible $ty proof"))
    val dlog = valid("dlog")
    val threshold = valid("threshold")
    val pk = hex(str(dlog.hcursor.downField("proposition").downField("pk")))
    def proposition(c: Json): CanonicalValue = surface(get(c.hcursor.downField("proposition").as[Json]).noSpaces)
    val digest = EthKeys.keccak("dilf4s ecrecover")
    List(
      probe("recovers the signer", Op.Ecrecover, CBytes(digest), CBytes(EthKeys.sign(BigInt(7), digest))),
      probe("verifies", Op.SchnorrVerify, CBytes(pk), bytes(field(dlog, "message")), bytes(field(dlog, "proof"))),
      probe("another message", Op.SchnorrVerify, CBytes(pk), CBytes(other), bytes(field(dlog, "proof"))),
      probe("a dlog verifies", Op.SigmaVerify, proposition(dlog), bytes(field(dlog, "proof")), bytes(field(dlog, "message"))),
      probe(
        "a threshold verifies",
        Op.SigmaVerify,
        proposition(threshold),
        bytes(field(threshold, "proof")),
        bytes(field(threshold, "message"))
      ),
      probe("a dlog, another message", Op.SigmaVerify, proposition(dlog), bytes(field(dlog, "proof")), CBytes(other)),
      probe("less", Op.Ge, int(1), int(2))
    )
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

  /**
   * A proof over `ProofInput.MaxProofWeight` reaches the verifier only through a host binding (no program result is
   * heavier than 2^24), and only under a gas limit that pays for its weight: it is then `Crypto`, refused before it is
   * decoded (docs/specs/conformance.md §5). A well-formed Merkle proof of leaf 0 of a tree of one, its path padded with
   * copies of a digest: within the cap it is decoded, and its path is too long (`false`); past it, `Crypto`. At the
   * default limit the size charge, taken first, runs out of gas.
   */
  def proofWeight(v: Json): List[Case] = {
    val leaf = CBytes(hex(get(v.hcursor.downField("leaves").as[List[String]]).head))
    val tree = committedTree(List(leaf.value), 1L)
    val digest = CBytes(tree.root.bytes)
    val template = m("index" -> int(0), "size" -> int(1), "path" -> BuiltValues.Hole)
    val around = get(BuiltValues.fill(template, False)).weight - 1
    def padded(copies: Long) = BuiltValues.of(digest).copies(copies.toInt).wrap(template)
    val within = (ProofInput.MaxProofWeight - around - 1) / digest.weight
    val over = within + 1
    require(get(BuiltValues.build(padded(within))).weight <= ProofInput.MaxProofWeight, "within the cap")
    require(get(BuiltValues.build(padded(over))).weight > ProofInput.MaxProofWeight, "over the cap")
    val program = """{"merkle_verify": [{"var": "head"}, {"var": "leaf"}, {"var": "$proof"}]}"""
    val data = m("head" -> m("root" -> CBytes(tree.root.bytes), "size" -> int(1)), "leaf" -> leaf)
    val trie = """{"trie_verify": [{"var": "mode"}, {"var": "root"}, {"var": "key"}, null, {"var": "$proof"}]}"""
    val trieData = m("mode" -> m("fixed" -> int(1)), "root" -> CBytes(ByteVector.fill(32)(0)), "key" -> CBytes(ByteVector(1)))
    val claimed = """{"trie_verify": [{"var": "mode"}, {"var": "root"}, {"var": "key"}, {"var": "$value"}, {"var": "proof"}]}"""
    val claimData =
      m("mode" -> m("fixed" -> int(1)), "root" -> CBytes(ByteVector.fill(32)(0)), "key" -> CBytes(ByteVector(1)), "proof" -> a())
    val leafProgram = """{"merkle_verify": [{"var": "head"}, {"var": "$leaf"}, {"var": "proof"}]}"""
    val inclusion = get(tree.inclusionProof(0L).flatMap(MerkleCodec.encodeInclusion(_)))
    List(
      Case(
        s"dilf4s proof weight: merkle_verify, $within padded digests, within the cap, is decoded: false",
        program,
        data,
        Option(1L << 30),
        Map("proof" -> padded(within))
      ),
      Case(
        s"dilf4s proof weight: merkle_verify, $over padded digests, over the cap: Crypto",
        program,
        data,
        Option(1L << 30),
        Map("proof" -> padded(over))
      ),
      Case(
        s"dilf4s proof weight: merkle_verify, $over padded digests, at the default limit: out of gas",
        program,
        data,
        scala.None,
        Map("proof" -> padded(over))
      ),
      Case(
        "dilf4s proof weight: trie_verify, a proof one over the cap: Crypto",
        trie,
        trieData,
        Option(1L << 30),
        Map("proof" -> BuiltValues.ofWeight(ProofInput.MaxProofWeight + 1))
      ),
      // claims: bounded the same way, before they are hashed and before the proof is decoded
      Case(
        "dilf4s proof weight: trie_verify, a claimed value of weight exactly 2^24, within the cap: false",
        claimed,
        claimData,
        Option(1L << 30),
        Map("value" -> BuiltValues.ofWeight(ProofInput.MaxProofWeight))
      ),
      Case(
        "dilf4s proof weight: trie_verify, a claimed value one over the cap: Crypto",
        claimed,
        claimData,
        Option(1L << 30),
        Map("value" -> BuiltValues.ofWeight(ProofInput.MaxProofWeight + 1))
      ),
      Case(
        "dilf4s proof weight: merkle_verify, a leaf one over the cap: Crypto",
        leafProgram,
        m("head" -> m("root" -> CBytes(tree.root.bytes), "size" -> int(1)), "proof" -> inclusion),
        Option(1L << 30),
        Map("leaf" -> BuiltValues.ofWeight(ProofInput.MaxProofWeight + 1))
      ),
      // the head is checked before the claim: a root that is not bytes is Type, not the claim's Crypto
      Case(
        "dilf4s proof weight: merkle_verify, a text root and a leaf over the cap: the head first, Type",
        leafProgram,
        m("head" -> m("root" -> get(CanonicalValue.text("x")), "size" -> int(1)), "proof" -> inclusion),
        Option(1L << 30),
        Map("leaf" -> BuiltValues.ofWeight(ProofInput.MaxProofWeight + 1))
      )
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

  /**
   * Reads inside two nested callbacks whose elements hold 70-character keys that differ only in their last character:
   * each frame searched compares the whole shared prefix, so each pays the first segment's count (jlvm.md §7). A
   * static path (found in the outer frame, and absent), a computed text path, a computed array path and `missing`.
   */
  val frameLookups: List[Case] = {
    val prefix = "k" * 69
    val (outer, inner, absent) = (prefix + "a", prefix + "b", prefix + "c")
    val data = m("xs" -> surface(s"""[{"$outer": 1}]"""), "ys" -> surface(s"""[{"$inner": 2}]"""))
    def nested(read: Json): String =
      Json
        .obj("map" -> Json.arr(Json.obj("var" -> "xs".asJson), Json.obj("map" -> Json.arr(Json.obj("var" -> "ys".asJson), read))))
        .noSpaces
    def literal(v: Json): Json = Json.obj("literal" -> v)
    List(
      "a static path found in the outer frame" -> Json.obj("var" -> outer.asJson),
      "a static path no frame holds"           -> Json.obj("var" -> absent.asJson),
      "a computed text path"                   -> Json.obj("var" -> literal(outer.asJson)),
      "a computed array path"                  -> Json.obj("var" -> literal(Json.arr(outer.asJson))),
      "missing, which searches the frames too" -> Json.obj("missing" -> Json.arr(outer.asJson, absent.asJson))
    ).map { case (name, read) => Case(s"dilf4s frames: $name, with a 70-character first key", nested(read), data) }
  }

  /**
   * How program text is read (jlvm.md §1): by the strict canonical parser alone, so every implementation refuses a
   * repeated member name wherever it is, reads numbers by one grammar and its bounds, and keeps a null field in a map
   * expression. Each program is written out as text, since no JSON writer could produce some of them.
   */
  val surfaceReading: List[Case] = {
    // the most significant digits a number has, and within every bound
    val digits = "1" * 10000
    val keyed = surface("""{"1": "one", "0": "zero", "100": "hundred"}""")
    List(
      ("a null field keeps a map expression, not the operation +", """{"+": [1, 2], "x": null}""", CNull),
      ("a repeated member name is refused", """{"a": 1, "a": 2}""", CNull),
      ("a repeated member name is refused when its first value is null", """{"a": null, "a": 2}""", CNull),
      ("a repeated member name inside a literal is refused", """{"literal": {"a": 1, "a": 2}}""", CNull),
      ("a repeated operation name is refused", """{"+": [1], "+": [2]}""", CNull),
      ("a repeated member name in an object expression is refused", """{"object": {"k": 1, "k": {"var": "x"}}}""", CNull),
      ("an escaped member name is the name it spells", """{"\u002b": [1, 2]}""", CNull),
      ("an array headed by an escaped call name is refused", """["v\u0061r", "a"]""", CNull),
      ("a member name with an unpaired surrogate is refused", """{"\ud800": 1}""", CNull),
      ("whitespace around every token", " \t\n{ \"+\" :\r[ 1 , 2 ] } \n", CNull),
      ("trailing content is refused", """{"+": [1, 2]} 3""", CNull),
      ("a path written 1.0 is the integer path 1", """{"var": 1.0}""", keyed),
      ("a path written 1e2 is the integer path 100", """{"var": 1e2}""", keyed),
      ("a path written 100e-2 is the integer path 1", """{"var": 100e-2}""", keyed),
      ("a path written -0 is the integer path 0", """{"var": -0}""", keyed),
      ("a path written 0.5 is refused", """{"var": 0.5}""", keyed),
      ("a path written true is refused", """{"var": true}""", keyed),
      ("a literal of 10,000 significant digits is read", s"""{"if": [true, "read", $digits]}""", CNull),
      ("a literal of 10,001 significant digits is refused", s"""{"if": [true, "read", ${digits}9]}""", CNull),
      ("a path of 10,000 digits", s"""{"var": $digits}""", CNull),
      ("a path of 10,001 digits is refused", s"""{"var": ${digits}9}""", CNull),
      ("a literal with a 10,001-digit exponent is refused", s"""{"if": [true, "read", 1e${"1" * 10001}]}""", CNull),
      ("a literal 1e-7 is exact", """{"+": [1e-7, 0.0000001]}""", CNull),
      // an integer is at most 33219 bits wide (canonical-encoding.md §1), so it has at most 10,000 digits: 8e9999 is
      // one, 9e9999 and 10,000 nines are not, and neither is a 10,000-digit significand times 10^10000; the number bound
      // (4096 bits) then refuses arithmetic
      ("a literal 8e9999 is an integer of 33219 bits", """{"if": [true, 8e9999, 0]}""", CNull),
      ("a literal -8e9999 is an integer of 33219 bits", """{"if": [true, -8e9999, 0]}""", CNull),
      ("a literal 9e9999, of 33220 bits, is refused", """{"if": [true, 9e9999, 0]}""", CNull),
      ("a literal -9e9999, of 33220 bits, is refused", """{"if": [true, -9e9999, 0]}""", CNull),
      ("a literal 1e10000 is refused", """{"if": [true, 1e10000, 0]}""", CNull),
      ("a literal of 10,000 nines, of 33220 bits, is refused", s"""{"if": [true, 0, ${"9" * 10000}]}""", CNull),
      ("a literal of 10,000 digits times 10^10000 is refused", s"""{"if": [true, 0, ${digits}e10000]}""", CNull),
      ("a path written 9e9999 is refused", """{"var": 9e9999}""", CNull),
      ("a 33219-bit operand is past the number bound", """{"+": [8e9999, 0]}""", CNull),
      ("a 33219-bit operand is past the number bound when negative", """{"*": [-8e9999, 1]}""", CNull),
      ("text nested 257 levels deep is Limit", "[" * 256 + "[]" + "]" * 256, CNull)
    ).map { case (name, program, data) => Case(s"dilf4s surface: $name", program, data) }
  }

  /**
   * The charges of schedule v1 that grow with more than one argument, or with a number's limbs, pinned across languages
   * (docs/specs/jlvm.md §7): map searches over maps of 0 to 1000 entries with keys sharing a 300-unit prefix, merging maps,
   * rational arithmetic and comparison (the gcd's fixed cost), a substring search at KMP's worst, and a verified
   * `ecvrf_verify`.
   */
  val gasShapes: List[Case] = {
    def keyed(n: Int, prefix: Int): CanonicalValue = m((0 until n).map(i => ("k" * prefix + f"$i%04d") -> int(i))*)
    val long = "k" * 300
    val searches = for {
      n  <- List(0, 1, 64, 1000)
      op <- List(Op.Get, Op.Has, Op.Set, Op.Unset)
    } yield {
      val args = Vector(keyed(n, 300), text(long + "0007")) ++ Option.when(op == Op.Set)(int(1)).toVector
      probe(s"$n entries with a 300-unit common prefix", op, args*)
    }
    val merges = List(
      probe("three maps of 40, 10 and 3 entries", Op.Merge, keyed(40, 1), keyed(10, 200), keyed(3, 1)),
      probe("a map and an array: concatenated, not merged", Op.Merge, keyed(40, 1), a(int(1)))
    )
    def q(op: Op, name: String, n: BigInt, d: BigInt, k: BigInt, e: BigInt): Case =
      Case(
        s"dilf4s ${op.wire}: $name",
        s"""{"${op.wire}": [{"/": [{"var": "a0"}, {"var": "a1"}]}, {"/": [{"var": "a2"}, {"var": "a3"}]}]}""",
        m("a0" -> int(n), "a1" -> int(d), "a2" -> int(k), "a3" -> int(e))
      )
    val big = (BigInt(1) << 700) + 12345
    val rationals =
      List(Op.Add, Op.Mul, Op.Div, Op.Mod, Op.Lt, Op.Min, Op.Max).map(op =>
        q(op, "two 700-bit rationals", big, big * 3 + 2, big + 7, big * 5 + 4)
      ) ++
      List(
        Case(
          "dilf4s abs: a 700-bit rational",
          """{"abs": [{"/": [{"var": "a0"}, {"var": "a1"}]}]}""",
          m("a0" -> int(-big), "a1" -> int(big * 3 + 2))
        )
      )
    val kmp = List(probe("a needle of 1000 a's and a b, in 4000 a's", Op.In, text("a" * 1000 + "b"), text("a" * 4000)))
    val kp = get(Vrf.fromSeed[EcVrfTai](ByteVector.fill(32)(5)))
    val alpha = ByteVector("alpha".getBytes(UTF_8))
    val (proof, _) = get(Vrf[EcVrfTai].prove(kp.signing, alpha))
    val vrf = List(
      probe("verifies", Op.EcvrfVerify, CBytes(kp.verifying.bytes), CBytes(alpha), CBytes(proof.bytes)),
      probe("another alpha", Op.EcvrfVerify, CBytes(kp.verifying.bytes), bytes("00"), CBytes(proof.bytes))
    )
    searches ++ merges ++ rationals ++ kmp ++ vrf
  }

  /**
   * Sort keys past the encoding ceiling (jlvm.md §7): a collection heavier than 2^31 - 9 bytes, the JVM's largest array,
   * is a `Limit` in `unique` and `intersect`, refused from its weight on every backend, never an allocation. `intersect`
   * keys its second array whole, and its first only up to the first element missing from the second.
   */
  val sortKeyCeiling: List[Case] = {
    val heavy = BuiltValues.ofWeight(xyz.kd5ujc.core.canonical.Cbor.MaxEncodedBytes + 1)
    val limit = Option(1L << 40)
    List(
      Case(
        "dilf4s unique: an element past the sort-key ceiling is Limit",
        """{"unique": {"var": "$xs"}}""",
        CNull,
        limit,
        Map("xs" -> heavy.copies(1))
      ),
      Case(
        "dilf4s intersect: an element past the sort-key ceiling in the second array is Limit",
        """{"intersect": [{"var": "a"}, {"var": "$xs"}]}""",
        m("a" -> a(int(1))),
        limit,
        Map("xs" -> heavy.copies(1))
      ),
      Case(
        "dilf4s intersect: an element past the sort-key ceiling after one missing from the second array is not keyed: false",
        """{"intersect": [{"var": "$xs"}, {"var": "b"}]}""",
        m("b" -> a(int(1))),
        limit,
        Map("xs" -> heavy.wrap(a(int(0), BuiltValues.Hole)))
      ),
      Case(
        "dilf4s intersect: an element past the sort-key ceiling before one missing from the second array is Limit",
        """{"intersect": [{"var": "$xs"}, {"var": "b"}]}""",
        m("b" -> a(int(1))),
        limit,
        Map("xs" -> heavy.wrap(a(BuiltValues.Hole, int(0))))
      )
    )
  }

  /**
   * Error rows the sweep does not reach (jlvm.md §3): each refusal a computed `var` path gives, whose site is `var`
   * rather than an operation; a nested refusal, whose `op` is the inner operation's; and a spread that runs out of gas
   * while its elements are sized, where `OutOfGas` is pinned by its kind and the gas used (the whole limit) alone.
   */
  val errorSites: List[Case] = {
    def computed(name: String, path: Json): Case =
      Case(s"dilf4s var: $name", Json.obj("var" -> Json.obj("literal" -> path)).noSpaces, m("a" -> m("b" -> int(1))))
    List(
      computed("a computed text path with an empty segment is Domain", "a..b".asJson),
      computed("a computed path that is neither text nor an array is Type", true.asJson),
      computed("a computed integer segment past 64 bits is Domain", Json.arr(Json.fromBigInt(BigInt(1) << 64))),
      computed("a computed segment that is neither text nor an integer is Type", Json.arr(true.asJson)),
      Case("dilf4s op: the innermost operation that refuses is the op", """{"+": [1, {"abs": "x"}]}""", CNull),
      Case(
        "dilf4s spread: out of gas while its elements are sized, at 210",
        """{"+": {"var": "xs"}}""",
        m("xs" -> a((1 to 8).map(i => int(i))*)),
        gasLimit = Some(210L)
      )
    )
  }

  /**
   * The gas limit's range (jlvm.md §7): 2^53 - 1, the largest, is metered like any other, and 2^53 is `Limit` with no
   * gas used, checked before the program is compiled (a program that binds a name twice would be `Malformed`).
   */
  val gasLimits: List[Case] = {
    val max = GasLimit.Max.amount.value
    List(
      Case("dilf4s gas limit: 2^53 - 1, the largest, is metered like any other", """{"+": [1, 2]}""", CNull, Some(max)),
      Case("dilf4s gas limit: 2^53 is Limit, with no gas used", """{"+": [1, 2]}""", CNull, Some(max + 1)),
      Case(
        "dilf4s gas limit: 2^53 is Limit before the program is compiled",
        """{"let": [[["x", 1], ["x", 2]], 1]}""",
        CNull,
        Some(max + 1)
      ),
      // the program and bindings are read before the limit is checked
      Case("dilf4s gas limit: a program that is not JSON is Malformed, even at 2^53", "{", CNull, Some(max + 1)),
      Case(
        "dilf4s gas limit: a binding that is not JSON is Malformed, even at 2^53",
        """{"var": "$x"}""",
        CNull,
        Some(max + 1),
        env = Map("x" -> "{")
      )
    )
  }

  /**
   * Host bindings (jlvm.md §4.1): a name bound to null is bound, so it reads as null where an unbound name is
   * `Malformed`; a default replaces a null read either way, and a path into a null binding reads null. `let` shadows a
   * host binding, and binds null like any other value.
   */
  val hostBindings: List[Case] = {
    val x = Map("x" -> "null")
    def bound(name: String, program: String, env: Map[String, String]): Case =
      Case(s"dilf4s host bindings: $name", program, CNull, env = env)
    List(
      bound("a name bound to null reads as null", """{"var": "$x"}""", x),
      bound("an unbound name is Malformed", """{"var": "$x"}""", Map.empty),
      bound("a name bound to null with a default reads the default", """{"var": ["$x", 5]}""", x),
      bound("an unbound name with a default reads the default", """{"var": ["$x", 5]}""", Map.empty),
      bound("a path into a name bound to null reads as null", """{"var": "$x.a"}""", x),
      bound("a path into a name bound to null with a default reads the default", """{"var": ["$x.a", 5]}""", x),
      bound("a name bound to null is not missing to typeof", """{"typeof": {"var": "$x"}}""", x),
      bound("let shadows a host binding of null", """{"let": [[["x", 1]], {"var": "$x"}]}""", x),
      bound("let binds null, read as null", """{"let": [[["y", null]], {"var": "$y"}]}""", Map.empty),
      bound("let binds null, and a default replaces the read", """{"let": [[["y", null]], {"var": ["$y", 5]}]}""", Map.empty)
    )
  }

  /**
   * The result bound applies to what a lookup returns, never to the keys it reads (jlvm.md §2, §4.4): 4,100 absent
   * keys of 4,096 characters, one value shared, weigh past 2^24 together. `missing_some` whose minimum is met returns
   * `[]`; one whose minimum is not, and `missing`, return the keys, and are `Limit`.
   */
  val lookupBounds: List[Case] = {
    val keys = BuiltValues.of(text("k" * 4096)).copies(4100)
    val limit = Option(1L << 27)
    List(
      ("missing_some: absent keys past the result bound, the minimum met, is []", """{"missing_some": [0, {"var": "$ks"}]}"""),
      ("missing_some: absent keys past the result bound, the minimum not met, is Limit", """{"missing_some": [4100, {"var": "$ks"}]}"""),
      ("missing: absent keys past the result bound is Limit", """{"missing": {"var": "$ks"}}""")
    ).map { case (name, program) => Case(s"dilf4s lookups: $name", program, CNull, limit, Map("ks" -> keys)) }
  }

  /**
   * The result bound applies to the maps `unset` and `set` build, never to host data passed through (jlvm.md section
   * 2): a host map holding `[x, x]` nested 24 deep (weight 2^25 - 1, in 24 arrays) is read, asked its kind and its keys
   * as it is, but removing a key from it, either way, builds a map past the bound, and is `Limit`.
   */
  val resultBounds: List[Case] = {
    val heavy = BuiltValues
      .of(int(0))
      .wrap(surface("""["$", "$"]"""), 24)
      .wrap(surface("""{"keep": "$", "drop": 1}"""))
    List(
      ("unset of a key, leaving a map past the result bound, is Limit", """{"unset": [{"var": "$m"}, "drop"]}"""),
      ("set of a key to null, leaving a map past the result bound, is Limit", """{"set": [{"var": "$m"}, "drop", null]}"""),
      ("a host map past the result bound passes through a read, typeof", """{"typeof": {"var": "$m"}}"""),
      ("a host map past the result bound passes through get", """{"get": [{"var": "$m"}, "drop"]}"""),
      ("a host map past the result bound is searched by has", """{"has": [{"var": "$m"}, "keep"]}""")
    ).map { case (name, program) => Case(s"dilf4s result bound: $name", program, CNull, built = Map("m" -> heavy)) }
  }

  private def render(cases: List[Case]): String =
    Json
      .obj(
        "source" -> "dilf4s: every operation over 0 to 3 arguments of each kind, then real inputs for the operations no other file reaches (OpCoverageCases)".asJson,
        "cases" -> cases.map { c =>
          Json.fromFields(
            List("name" -> c.name.asJson, "program" -> c.program.asJson, "data" -> get(JsonSurface.render(c.data)).asJson) ++
            Option.when(c.built.nonEmpty)("built" -> Json.fromFields(c.built.toList.sortBy(_._1).map {
              case (k, b) => k -> get(CanonicalCirce.toCirce(BuiltValues.value(b)))
            })) ++
            Option.when(c.env.nonEmpty)("env" -> Json.fromFields(c.env.toList.sortBy(_._1).map { case (k, v) => k -> v.asJson })) ++
            c.gasLimit.map(l => "gasLimit" -> l.asJson)
          )
        }.asJson
      )
      .spaces2 + "\n"

  def run(args: List[String]): IO[ExitCode] =
    (Secp256k1.load[IO], vector("merkle_v1.json"), vector("trie_v1.json"), vector("sigma_v1.json"))
      .mapN((secp, mk, tr, sg) =>
        sweep ++ bytesAndHashing ++ casing ++ signatures(secp) ++ merkle(mk) ++ proofWeight(mk) ++ tries(tr) ++ sigmaBounds ++
        frameLookups ++ nonCanonicalEd25519 ++ surfaceReading ++ gasShapes ++ errorSites ++ gasLimits ++ sortKeyCeiling ++ hostBindings ++ lookupBounds ++ resultBounds ++
        verdicts(sg) ++ rootVerifiers(sg)
      )
      .flatMap(cases => IO.blocking(Files.write(path, render(cases).getBytes(UTF_8))))
      .as(ExitCode.Success)
}
