package xyz.kd5ujc.accumulators

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.{InclusionProof, MerkleCodec, MerkleError, MerkleHashing, MerkleTree, MerkleVerifier}
import xyz.kd5ujc.accumulators.poseidon.{PoseidonMerkleError, PoseidonMerkleProof}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieError, TrieHashing, TrieProofs}
import xyz.kd5ujc.accumulators.versioned.Harness.{attempt, commitAll, fresh, int, key, text}
import xyz.kd5ujc.accumulators.versioned._
import xyz.kd5ujc.core.canonical.BuiltValues.{Built, Pair}
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalValue}
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.storage.Batch

import io.circe.syntax._
import io.circe.{ACursor, Json}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * Conformance vectors for docs/specs/conformance.md (shared/vectors/refusals_v1.json): calls into the verifiers and the
 * versioned trie, each with its outcome, a refusal by kind and normative fields, or "accepted". A case is written as
 * JSON first, and its outcome is what [[run]] gives on that JSON, so the reference reads the cases exactly as every
 * SDK's loader does. Heavy inputs are recipes (`BuiltValues`), built with sharing and never encoded here.
 */
object RefusalVectors {

  type H = Sha256

  val path: Path = Paths.get("shared", "vectors", "refusals_v1.json")

  // ---------------------------------------------------------------------------------------------------- outcomes

  sealed trait Outcome extends Product with Serializable
  case object Accepted extends Outcome

  /** A refusal: its kind, and the fields conformance.md §3 names normative for it. */
  final case class Refused(kind: String, limit: Option[Long] = None, depth: Option[Int] = None, component: Option[String] = None)
      extends Outcome

  def outcomeJson(o: Outcome): Json = o match {
    case Accepted => "accepted".asJson
    case Refused(k, l, d, c) =>
      Json.fromFields(
        List("kind" -> k.asJson) ++ l.map("limit" -> _.asJson) ++ d.map("depth" -> _.asJson) ++ c.map("component" -> _.asJson)
      )
  }

  def versioned(e: VersionedTrieError): Refused = e match {
    case VersionedTrieError.TooLarge(_, limit)       => Refused("TooLarge", limit = Some(limit))
    case VersionedTrieError.TooDeep(_, depth, limit) => Refused("TooDeep", limit = Some(limit.toLong), depth = Some(depth))
    case VersionedTrieError.RootMismatch(c, _, _)    => Refused("RootMismatch", component = Some(c))
    case VersionedTrieError.Trie(t)                  => Refused(s"Trie.${t.productPrefix}")
    case other                                       => Refused(other.productPrefix)
  }

  def merkle(e: MerkleError): Refused = e match {
    case MerkleError.TooLarge(limit) => Refused("TooLarge", limit = Some(limit))
    case other                       => Refused(other.productPrefix)
  }

  def trie(e: TrieError): Refused = e match {
    case TrieError.TooLarge(limit) => Refused("TooLarge", limit = Some(limit))
    case other                     => Refused(other.productPrefix)
  }
  def poseidon(e: PoseidonMerkleError): Refused = e match {
    case PoseidonMerkleError.Position(_, depth) => Refused("Position", depth = Some(depth))
    case other                                  => Refused(other.productPrefix)
  }

  private def outcome[E, A](r: Either[E, A])(refused: E => Refused): Outcome = r.fold(refused, _ => Accepted)

  // ---------------------------------------------------------------------------------------------------- reading

  private def field[A: io.circe.Decoder](c: ACursor, name: String): Either[String, A] = c.get[A](name).leftMap(e => s"$name: $e")

  private def bytes(c: ACursor, name: String): Either[String, ByteVector] =
    field[String](c, name).flatMap(ByteVector.fromHexDescriptive(_))

  private def digest(c: ACursor, name: String): Either[String, Digest[H]] =
    bytes(c, name).flatMap(Digest.fromBytes[H](_).leftMap(_.toString))

  /** A `built-value` from its JSON. */
  def built(j: ACursor): Either[String, CanonicalValue] = BuiltValues.fromJsonValue(j)

  private def mode(c: ACursor): Either[String, KeyMode] = {
    val m = c.downField("mode")
    m.get[Int]("fixed")
      .toOption
      .map(KeyMode.fixed(_))
      .orElse(m.get[Int]("variable").toOption.map(KeyMode.variable(_)))
      .toRight("mode")
      .flatMap(_.leftMap(_.toString))
  }

  /** The outcome of one case, read from its JSON as an SDK loader reads it (conformance.md §6 defines each check). */
  def run(c: ACursor): Either[String, Outcome] = {
    def input = built(c.downField("input"))
    def stateKey = field[String](c, "stateKey").flatMap(StateKey.parse(_).leftMap(_.toString))
    def proof = input.map(VersionedTrieWire.decodeProof[Long, H](_))
    val M = MerkleHashing.tagged[H]
    val T = TrieHashing[H]
    field[String](c, "check").flatMap {
      case "vt-value" => (stateKey, input).mapN((k, v) => outcome(VersionedTrie.checkValue(k, v))(versioned))
      case "vt-chunk" =>
        field[Long](c, "zeros").map(n => outcome(VersionedTrieWire.decodeChunk[Long, H](ByteVector.fill(n)(0)))(versioned))
      case "vt-delta" => input.map(v => outcome(VersionedTrieWire.decodeDelta[Long, H](v))(versioned))
      case "vt-proof" => proof.map(outcome(_)(versioned))
      case "vt-state" =>
        (digest(c, "root"), proof, stateKey).mapN { (r, p, k) =>
          outcome(p.flatMap(VersionedTrieVerifier.verifyState(r, _)).flatMap(_.lookup(k)))(versioned)
        }
      case "vt-historical" =>
        (digest(c, "root"), proof, field[Long](c, "at"), stateKey).mapN { (r, p, u, k) =>
          outcome(p.flatMap(VersionedTrieVerifier.verifyHistoricalKey(r, _, u)).flatMap(_.lookup(k)))(versioned)
        }
      case "vt-replica" =>
        c.downField("inputs").as[List[Json]].leftMap(_.toString).flatMap(_.traverse(j => built(j.hcursor))).map { ds =>
          outcome(
            Replica
              .empty[Long, H]
              .flatMap(r0 => ds.foldLeftM(r0)((r, d) => VersionedTrieWire.decodeDelta[Long, H](d).flatMap(Replica.applyDelta(r, _))))
          )(versioned)
        }
      case "merkle-inclusion" =>
        (digest(c, "root"), digest(c, "leafHash"), input).mapN { (r, l, v) =>
          outcome(MerkleCodec.decodeInclusion[H](v).flatMap(MerkleVerifier.inclusion(r, l, _)(using M)))(merkle)
        }
      case "merkle-consistency" =>
        (digest(c, "oldRoot"), digest(c, "root"), input).mapN { (o, r, v) =>
          outcome(MerkleCodec.decodeConsistency[H](v).flatMap(MerkleVerifier.consistency(o, r, _)(using M)))(merkle)
        }
      case "trie-lookup" =>
        (mode(c), digest(c, "root"), bytes(c, "key"), input).mapN { (m, r, k, v) =>
          outcome(for {
            p        <- TrieProofs.decode[H](v)
            verified <- TrieProofs.verify(m, r, p)(using T)
            path     <- m.path(k)
            held     <- verified.holds(path, None)
          } yield held)(trie)
        }
      case "poseidon-proof" => input.map(v => outcome(PoseidonMerkleProof.fromCanonical(v))(poseidon))
      case "merkle-verify" =>
        (digest(c, "root"), field[Long](c, "size"), built(c.downField("claim")), input).mapN { (r, n, leaf, v) =>
          outcome(MerkleCodec.verifyLeaf[H](r, n, leaf, v)(using summon[xyz.kd5ujc.core.hash.Hasher[H]], M))(merkle)
        }
      case "trie-verify" =>
        val claim = c.downField("claim").success.traverse(built(_))
        (mode(c), bytes(c, "root"), bytes(c, "key"), claim, input).mapN { (m, r, k, cl, v) =>
          outcome(TrieProofs.verifyClaim[H](m, r, k, cl, v)(using summon[xyz.kd5ujc.core.hash.Hasher[H]], T))(trie)
        }
      case "trie-prefix-verify" =>
        val entries = c
          .downField("entries")
          .as[List[Json]]
          .leftMap(_.toString)
          .flatMap(_.traverse { e =>
            (bytes(e.hcursor, "key"), built(e.hcursor.downField("value"))).tupled
          })
        (mode(c), bytes(c, "root"), bytes(c, "prefix"), entries, input).mapN { (m, r, p, es, v) =>
          outcome(TrieProofs.verifyPrefixClaims[H](m, r, p, es, v)(using summon[xyz.kd5ujc.core.hash.Hasher[H]], T))(trie)
        }
      case other => s"unknown check $other".asLeft
    }
  }

  // ---------------------------------------------------------------------------------------------------- cases

  private def hex(b: ByteVector): Json = b.toHex.asJson

  /** A case: its fields; the outcome is computed from them. */
  final case class Case(name: String, check: String, fields: List[(String, Json)]) {
    def json: Json = Json.fromFields(List("name" -> name.asJson, "check" -> check.asJson) ++ fields)
  }

  private def input(b:   Built): (String, Json) = "input" -> BuiltValues.json(b)
  private def literal(v: CanonicalValue): Built = BuiltValues.of(v)

  private def get[A](e: Either[?, A]): A = e.fold(err => throw new IllegalStateException(err.toString), identity)
  private def arr(vs:   CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))
  private def obj(es:   (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(es.toList))
  private val Hole = BuiltValues.Hole

  /** `[u64max, u32max]`: weight 4, 15 bytes of CBOR. Doubled 16 times, it weighs 327,679 and encodes in 2^20 - 1. */
  private val dense: Built = BuiltValues.of(arr(CInt(BigInt(2).pow(64) - 1), CInt(BigInt(2).pow(32) - 1))).wrap(Pair, 16)

  /** Values against `VersionedTrie.MaxValueBytes` (2^20) and `MaxValueDepth` (253). */
  private def valueCases: List[Case] = {
    def c(name: String, b: Built) = Case(s"value: $name", "vt-value", List("stateKey" -> "a/1".asJson, input(b)))
    List(
      c("exactly 2^20 bytes, measured (weight 327,680)", dense.wrap(arr(Hole))),
      c("2^20 + 1 bytes, measured to the limit (weight 327,681)", dense.wrap(arr(Hole, CBytes(ByteVector.empty)))),
      c("weight 2^21 - 1, refused by weight alone", BuiltValues.of(CNull).wrap(Pair, 20)),
      c("a saturated weight (70 doublings)", BuiltValues.of(CNull).wrap(Pair, 70)),
      c("253 levels deep", BuiltValues.of(CNull).wrap(arr(Hole), VersionedTrie.MaxValueDepth)),
      c("254 levels deep", BuiltValues.of(CNull).wrap(arr(Hole), VersionedTrie.MaxValueDepth + 1))
    )
  }

  /** Byte inputs against `VersionedTrie.MaxChunkBytes` (2^22): the size is checked before anything is decoded. */
  private def chunkCases: List[Case] = List(
    Case("chunk: 2^22 zero bytes pass the size check and do not decode", "vt-chunk", List("zeros" -> (1L << 22).asJson)),
    Case("chunk: 2^22 + 1 zero bytes, refused by size", "vt-chunk", List("zeros" -> ((1L << 22) + 1).asJson))
  )

  private val head: CanonicalValue =
    obj("v" -> CBytes(ByteVector.fill(8)(0)), "s" -> CBytes(ByteVector.fill(32)(1)), "h" -> CBytes(ByteVector.fill(32)(2)))

  private def delta(k: String, v: CanonicalValue): CanonicalValue =
    obj("head" -> head, "upserts" -> arr(obj("k" -> get(CanonicalValue.text(k)), "v" -> v)), "removes" -> arr())

  /** A delta (well formed but for its weight) whose upserted value makes it weigh exactly `target`. */
  private def deltaOfWeight(target: Long): Built = {
    val template = delta("a/1", Hole)
    val around = get(BuiltValues.fill(template, False)).weight - 1
    val b = BuiltValues.ofWeight(target - around).wrap(template)
    require(get(BuiltValues.build(b)).weight == target, s"a delta of weight $target")
    b
  }

  /** Verifier inputs against `ProofInput.MaxProofWeight` (2^24), and decoding refusals by kind. */
  private def deltaCases: List[Case] = List(
    Case("delta: weight exactly 2^24 decodes", "vt-delta", List(input(deltaOfWeight(ProofInput.MaxProofWeight)))),
    Case("delta: weight 2^24 + 1, refused before decoding", "vt-delta", List(input(deltaOfWeight(ProofInput.MaxProofWeight + 1)))),
    Case(
      "delta: a saturated weight, refused before decoding",
      "vt-delta",
      List(input(BuiltValues.of(CNull).wrap(Pair, 70).wrap(delta("a/1", Hole))))
    ),
    Case(
      "delta: a key the schema pattern accepts but the grammar refuses (a 65-byte segment)",
      "vt-delta",
      List(input(literal(delta("x" * 65, int(1)))))
    )
  )

  /** A store with two commits: its proofs and deltas. */
  private final case class Store(
    root:    Digest[H],
    oldRoot: Digest[H],
    keys:    CanonicalValue,
    absent3: CanonicalValue,
    delta1:  CanonicalValue,
    replay1: CanonicalValue
  )

  private def store: IO[Store] =
    attempt { implicit h =>
      for {
        s <- fresh
        heads <- commitAll(
          s._2,
          List(1L -> (Batch.put(key("a/1"), int(1)) ++ Batch.put(key("a/2"), text("x"))), 2L -> Batch.put(key("a/1"), int(2)))
        )
        keys <- s._2.proveState(StateQuery.keys(key("a/1")))
        v3   <- s._2.proveVersion(3L)
        d1   <- s._2.delta(1L)
      } yield {
        def enc(p: VersionedTrieProof[Long, H]) = get(VersionedTrieWire.encodeProof(p))
        val first = heads.head
        // the first commit again, chaining from itself: its version is not above the replica's head
        val again = Delta[Long, H](Some(first), first, Nil, Nil)
        Store(
          get(VersionedTrie.root(heads.last)),
          get(VersionedTrie.root(first)),
          enc(keys),
          enc(v3),
          get(VersionedTrieWire.encodeDelta(d1)),
          get(VersionedTrieWire.encodeDelta(again))
        )
      }
    }.flatMap(_.leftMap(e => new IllegalStateException(e.toString)).liftTo[IO])

  /** `proof` with its top trie's first record repeated at the end: a trailing record. */
  private def trailingTop(proof: CanonicalValue): CanonicalValue = proof match {
    case CMap(m) =>
      m.get("top") match {
        case Some(CArray(rs)) => obj((m.toList.filterNot(_._1 == "top") :+ ("top" -> arr((rs :+ rs.head)*)))*)
        case _                => proof
      }
    case _ => proof
  }

  /** `proof` with its `at` version replaced by 2^63, which no version key decodes. */
  private def badAt(proof: CanonicalValue): CanonicalValue = proof match {
    case CMap(m) =>
      m.get("at") match {
        case Some(CMap(a)) =>
          val at = obj((a.toList.filterNot(_._1 == "v") :+ ("v" -> CBytes(ByteVector(0x80, 0, 0, 0, 0, 0, 0, 0))))*)
          obj((m.toList.filterNot(_._1 == "at") :+ ("at" -> at))*)
        case _ => proof
      }
    case _ => proof
  }

  private def storeCases(s: Store): List[Case] = List(
    Case("proof: a top trie with a trailing record", "vt-proof", List(input(literal(trailingTop(s.keys))))),
    Case("proof: an at version no version key decodes (2^63)", "vt-proof", List(input(literal(badAt(s.absent3))))),
    Case("proof: a saturated weight, refused before decoding", "vt-proof", List(input(BuiltValues.of(CNull).wrap(Pair, 70)))),
    Case("state: a/1 at the head", "vt-state", List("root" -> hex(s.root.bytes), input(literal(s.keys)), "stateKey" -> "a/1".asJson)),
    Case(
      "state: against another root",
      "vt-state",
      List("root" -> hex(s.oldRoot.bytes), input(literal(s.keys)), "stateKey" -> "a/1".asJson)
    ),
    Case(
      "state: a/2, behind a stub of a proof of a/1",
      "vt-state",
      List("root" -> hex(s.root.bytes), input(literal(s.keys)), "stateKey" -> "a/2".asJson)
    ),
    Case(
      "historical: version 3 was never committed",
      "vt-historical",
      List("root" -> hex(s.root.bytes), input(literal(s.absent3)), "at" -> 3.asJson, "stateKey" -> "a/1".asJson)
    ),
    Case("replica: the first delta", "vt-replica", List("inputs" -> List(BuiltValues.json(literal(s.delta1))).asJson)),
    Case(
      "replica: the first delta, then version 1 again",
      "vt-replica",
      List("inputs" -> List(BuiltValues.json(literal(s.delta1)), BuiltValues.json(literal(s.replay1))).asJson)
    )
  )

  /** Merkle proofs over three leaves, under the tagged profile. */
  private def merkleCases: List[Case] = {
    implicit val M: MerkleHashing[H] = MerkleHashing.tagged[H]
    val leaves = List(int(1), text("b"), CBytes(ByteVector(0x61, 0x61))).map(v => M.leaf(ValueCommitment.bytes(v)))
    val tree = MerkleTree.fromLeafHashes[H](leaves)
    val p0 = get(MerkleCodec.encodeInclusion(get(tree.inclusionProof(0L))))
    val c1 = get(MerkleCodec.encodeConsistency(get(tree.consistencyProof(1L))))
    val short = get(MerkleCodec.encodeInclusion(get(InclusionProof.from(0L, 3L, get(tree.inclusionProof(0L)).path.take(1)))))
    val other = MerkleTree.fromLeafHashes[H](leaves.take(2)).root
    // a well-formed proof of index 0 in a tree of one, its path padded with copies of a digest past the weight cap:
    // decoded, the path is too long (false); capped, it is never decoded
    val digestV = CBytes(leaves.head.bytes)
    val around = get(BuiltValues.fill(obj("index" -> int(0), "size" -> int(1), "path" -> Hole), False)).weight - 1
    val copies = ((ProofInput.MaxProofWeight - around) / digestV.weight + 1).toInt
    val padded = BuiltValues.of(digestV).copies(copies).wrap(obj("index" -> int(0), "size" -> int(1), "path" -> Hole))
    require(get(BuiltValues.build(padded)).weight > ProofInput.MaxProofWeight, "the padded proof is over the cap")
    def inc(name: String, root: Digest[H], leaf: Digest[H], b: Built) =
      Case(s"merkle inclusion: $name", "merkle-inclusion", List("root" -> hex(root.bytes), "leafHash" -> hex(leaf.bytes), input(b)))
    List(
      inc("leaf 0 of 3", tree.root, leaves.head, literal(p0)),
      inc("against another root", other, leaves.head, literal(p0)),
      inc("a path one digest short", tree.root, leaves.head, literal(short)),
      inc(s"a path of $copies digests, over the weight cap", MerkleTree.fromLeafHashes[H](leaves.take(1)).root, leaves.head, padded),
      Case(
        "merkle consistency: 1 to 3",
        "merkle-consistency",
        List("oldRoot" -> hex(MerkleTree.fromLeafHashes[H](leaves.take(1)).root.bytes), "root" -> hex(tree.root.bytes), input(literal(c1)))
      ),
      Case(
        "merkle consistency: a saturated weight",
        "merkle-consistency",
        List("oldRoot" -> hex(tree.root.bytes), "root" -> hex(tree.root.bytes), input(BuiltValues.of(CNull).wrap(Pair, 70)))
      )
    )
  }

  /** A fixed(1) trie of three keys; a proof of key 0x01, whose neighbours are stubs. */
  private def trieCases: List[Case] = {
    val T: TrieHashing[H] = TrieHashing[H]
    val m = get(KeyMode.fixed(1))
    val t = get(Trie.fromEntries[H](m, List(0x01, 0x02, 0x80).map(b => ByteVector(b) -> T.value(ByteVector(b)))))
    val proof = get(TrieProofs.encode(get(t.prove(List(ByteVector(0x01))))))
    def c(name: String, root: Digest[H], k: ByteVector, b: Built) =
      Case(
        s"trie: $name",
        "trie-lookup",
        List("mode" -> Json.obj("fixed" -> 1.asJson), "root" -> hex(root.bytes), "key" -> hex(k), input(b))
      )
    List(
      c("key 0x01", t.root, ByteVector(0x01), literal(proof)),
      c("against another root", T.empty, ByteVector(0x01), literal(proof)),
      c("key 0x02, behind a stub", t.root, ByteVector(0x02), literal(proof)),
      c("a two-byte key in a fixed(1) trie", t.root, ByteVector(0x01, 0x00), literal(proof)),
      c("a saturated weight", t.root, ByteVector(0x01), BuiltValues.of(CNull).wrap(Pair, 70))
    )
  }

  private def poseidonCases: List[Case] = {
    def proof(position: Int, siblings: Int) = obj("position" -> int(position), "siblings" -> arr(List.fill(siblings)(int(0))*))
    List(
      Case("poseidon: position 3 of depth 2", "poseidon-proof", List(input(literal(proof(3, 2))))),
      Case("poseidon: position 4 of depth 2", "poseidon-proof", List(input(literal(proof(4, 2))))),
      Case("poseidon: a saturated weight", "poseidon-proof", List(input(BuiltValues.of(CNull).wrap(Pair, 70))))
    )
  }

  /**
   * Claims against `ProofInput.MaxProofWeight`, together, refused before they are hashed: a Merkle leaf, a trie value,
   * and a listing whose values are each under the cap but together over it.
   */
  private def claimCases: List[Case] = {
    implicit val M: MerkleHashing[H] = MerkleHashing.tagged[H]
    val T: TrieHashing[H] = TrieHashing[H]
    val cap = ProofInput.MaxProofWeight
    val one = MerkleTree.fromLeafHashes[H](List(M.leaf(ValueCommitment.bytes(int(1)))))
    val inclusion = get(MerkleCodec.encodeInclusion(get(one.inclusionProof(0L))))
    def leaf(name: String, b: Built) =
      Case(
        s"merkle verify: $name",
        "merkle-verify",
        List("root" -> hex(one.root.bytes), "size" -> 1.asJson, "claim" -> BuiltValues.json(b), input(literal(inclusion)))
      )
    val m = get(KeyMode.fixed(1))
    val t = get(Trie.fromEntries[H](m, List(0x01, 0x02).map(b => ByteVector(b) -> T.value(ValueCommitment.bytes(int(b))))))
    val proof = get(TrieProofs.encode(get(t.prove(Nil, List(ByteVector.empty)))))
    def claim(name: String, b: Option[Built]) =
      Case(
        s"trie verify: $name",
        "trie-verify",
        List("mode" -> Json.obj("fixed" -> 1.asJson), "root" -> hex(t.root.bytes), "key" -> hex(ByteVector(0x01)), input(literal(proof))) ++
        b.map(x => "claim" -> BuiltValues.json(x))
      )
    def listing(name: String, w1: Long, w2: Long) =
      Case(
        s"trie prefix verify: $name",
        "trie-prefix-verify",
        List(
          "mode"   -> Json.obj("fixed" -> 1.asJson),
          "root"   -> hex(t.root.bytes),
          "prefix" -> hex(ByteVector.empty),
          "entries" -> List(0x01 -> w1, 0x02 -> w2).map {
            case (k, w) => Json.obj("key" -> hex(ByteVector(k)), "value" -> BuiltValues.json(BuiltValues.ofWeight(w)))
          }.asJson,
          input(literal(proof))
        )
      )
    List(
      leaf("a leaf of weight exactly 2^24", BuiltValues.ofWeight(cap)),
      leaf("a leaf of weight 2^24 + 1, refused before it is hashed", BuiltValues.ofWeight(cap + 1)),
      leaf("a leaf of saturated weight", BuiltValues.of(CNull).wrap(Pair, 70)),
      claim("absence claimed", None),
      claim("a value of weight exactly 2^24", Some(BuiltValues.ofWeight(cap))),
      claim("a value of weight 2^24 + 1, refused before it is hashed", Some(BuiltValues.ofWeight(cap + 1))),
      // competing refusals: the claim is checked before the root is read, so a 31-byte root does not answer first
      Case(
        "trie verify: a claim over the cap with a 31-byte root: the claim is refused first",
        "trie-verify",
        List(
          "mode" -> Json.obj("fixed" -> 1.asJson),
          "root" -> hex(t.root.bytes.take(31)),
          "key"  -> hex(ByteVector(0x01)),
          input(literal(proof)),
          "claim" -> BuiltValues.json(BuiltValues.ofWeight(cap + 1))
        )
      ),
      claim("a value within the cap with a 31-byte root", None).copy(
        name = "trie verify: a 31-byte root, with no claim to refuse first",
        fields = List(
          "mode" -> Json.obj("fixed" -> 1.asJson),
          "root" -> hex(t.root.bytes.take(31)),
          "key"  -> hex(ByteVector(0x01)),
          input(literal(proof))
        )
      ),
      listing("two values of 2^23, together 2^24", cap / 2, cap / 2),
      listing("values of 2^23 and 2^23 + 1, each under the cap, together over it", cap / 2, cap / 2 + 1)
    )
  }

  /**
   * The per-commit delta bound (versioned-trie.md §4): fifteen values of 2^20 bytes and a pad, so that the delta the
   * commit would serve weighs exactly `ProofInput.MaxProofWeight` (accepted) or one more (`TooLarge`, before anything is
   * written). Each value passes `checkValue`: `BuiltValues.ofWeight` encodes in exactly its weight.
   */
  private def commitCases: List[Case] = {
    val cap = ProofInput.MaxProofWeight
    val full = BuiltValues.ofWeight(VersionedTrie.MaxValueBytes)
    val keys = (1 to 15).map(i => s"wide/$i").toList
    val root = TrieHashing[H].empty
    def deltaWeight(pad: CanonicalValue): Long = {
      val upserts = keys.map(k => key(k) -> get(BuiltValues.build(full))) :+ (key("pad") -> pad)
      get(VersionedTrieWire.encodeDelta(Delta[Long, H](None, VersionedTrie.Head(1L, root, root), upserts, Nil))).weight
    }
    val around = deltaWeight(False) - 1
    def batch(target: Long) = {
      val pad = BuiltValues.ofWeight(target - around)
      // weights add, so the pad sets the delta's weight; within the cap the encoder confirms it
      require(target > cap || deltaWeight(get(BuiltValues.build(pad))) == target, s"a delta of weight $target")
      "batch" -> (keys.map(k => Json.obj("stateKey" -> k.asJson, "value" -> BuiltValues.json(full))) :+
      Json.obj("stateKey" -> "pad".asJson, "value" -> BuiltValues.json(pad))).asJson
    }
    List(
      Case("commit: a batch whose delta weighs exactly 2^24", "vt-commit", List(batch(cap))),
      Case("commit: a batch whose delta would weigh 2^24 + 1, refused before anything is written", "vt-commit", List(batch(cap + 1)))
    )
  }

  def cases: IO[List[Case]] =
    store.map(s =>
      valueCases ++ chunkCases ++ deltaCases ++ storeCases(s) ++ merkleCases ++ trieCases ++ poseidonCases ++ claimCases ++ commitCases
    )

  /**
   * `vt-commit`: the batch committed at version 1 into an empty store, the reference for the per-commit delta bound
   * (versioned-trie.md §4); every other check is [[run]].
   */
  def runIO(c: ACursor): IO[Either[String, Outcome]] =
    field[String](c, "check").flatTraverse {
      case "vt-commit" =>
        c.downField("batch")
          .as[List[Json]]
          .leftMap(_.toString)
          .flatMap(_.traverse { u =>
            (
              field[String](u.hcursor, "stateKey").flatMap(StateKey.parse(_).leftMap(_.toString)),
              built(u.hcursor.downField("value"))
            ).tupled
          })
          .traverse { upserts =>
            val batch = upserts.foldLeft(Batch.empty[StateKey, CanonicalValue]) { case (b, (k, v)) => b ++ Batch.put(k, v) }
            attempt(implicit h => fresh.flatMap(_._2.commit(None, 1L, batch))).map(outcome(_)(versioned))
          }
      case _ => IO.pure(run(c))
    }

  def build: IO[Json] =
    cases
      .flatMap(_.traverse { c =>
        runIO(c.json.hcursor).flatMap(
          _.map(o => c.json.deepMerge(Json.obj("outcome" -> outcomeJson(o))))
            .leftMap(e => new IllegalStateException(s"${c.name}: $e"))
            .liftTo[IO]
        )
      })
      .map { cs =>
        Json.obj("spec" -> "docs/specs/conformance.md".asJson, "version" -> 1.asJson, "hash" -> "SHA-256".asJson, "cases" -> cs.asJson)
      }

  def render: IO[String] = build.map(_.spaces2 + "\n")
}

object RefusalVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    RefusalVectors.render.flatMap(r => IO.blocking(Files.write(RefusalVectors.path, r.getBytes(UTF_8)))).as(ExitCode.Success)
}

object RefusalVectorsSuite extends SimpleIOSuite {

  test("committed refusal vectors match the reference implementation") {
    (IO.blocking(new String(Files.readAllBytes(RefusalVectors.path), UTF_8)), RefusalVectors.render).mapN { (committed, rendered) =>
      expect(
        committed == rendered,
        s"${RefusalVectors.path} is stale; regenerate with: sbt \"accumulators/Test/runMain xyz.kd5ujc.accumulators.RefusalVectorGen\""
      )
    }
  }
}
