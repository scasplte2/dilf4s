package xyz.kd5ujc.accumulators.versioned

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.storage.Batch

import io.circe.Json
import io.circe.syntax._
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * Conformance vectors for docs/specs/versioned-trie.md: the roots a sequence of commits produces, the head's top-trie
 * root, each commit's delta, and proofs against the final root with the answers they must give. Built by the store.
 */
object VersionedTrieVectors {

  val path: Path = Paths.get("shared", "vectors", "versioned_trie_v1.json")

  private def hex(b:      ByteVector): Json = b.toHex.asJson
  private def cbor(v:     CanonicalValue): Json = hex(Cbor.encode(v))
  private def orDie[A](e: Either[VersionedTrieError, A]): IO[A] = e.leftMap(err => new IllegalStateException(err.toString)).liftTo[IO]

  val commits: List[(Long, Batch[StateKey, CanonicalValue])] = List(
    1L -> (Batch.put(key("fiber/1"), int(1)) ++ Batch.put(key("fiber/2"), text("x")) ++ Batch.put(key("fiberx/1"), int(9)) ++
    Batch.put(key("oracle/a/b"), CanonicalValue.bytes(ByteVector(0x61, 0x61)))),
    2L -> (Batch.put(key("fiber/1"), int(2)) ++ Batch.delete(key("fiberx/1")) ++ Batch
      .put(key("meta"), get(CanonicalValue.map(List("k" -> int(1)))))),
    255L -> Batch.put(key("fiber/3"), get(CanonicalValue.array(Vector(int(1), CanonicalValue.CNull)))),
    256L -> Batch.delete(key("fiber/2")),
    300L -> Batch.put(key("oracle/a/b"), text("a"))
  )

  private def headJson(h: Head[Long, H]): IO[Json] =
    (
      orDie(VersionedTrie.root(h)),
      orDie(
        VersionedTrieWire
          .head(h)
          .flatMap(w =>
            xyz.kd5ujc.models.versioned_trie.VersionedTrieHead.codec.encode(w).leftMap(e => VersionedTrieError.Malformed(e.toString))
          )
      )
    ).mapN { (root, wire) =>
      Json.obj(
        "version"     -> h.version.asJson,
        "vkey"        -> hex(vk(h.version)),
        "stateRoot"   -> hex(h.stateRoot.bytes),
        "historyRoot" -> hex(h.historyRoot.bytes),
        "cbor"        -> cbor(wire),
        "root"        -> hex(root.bytes)
      )
    }

  /**
   * The default version key at its edges: `key` is `int_to_bytes [version, 8]` (a JLVM program builds those bytes for
   * any version below 2^64), and `valid` says whether the version key accepts it, encoding and decoding alike. It
   * stops at 2^63 - 1 (section 2).
   */
  val versionKeyCases: List[BigInt] =
    List(
      BigInt(0),
      BigInt(1),
      BigInt(255),
      BigInt(256),
      BigInt(2).pow(32),
      BigInt(Long.MaxValue),
      BigInt(2).pow(63),
      BigInt(2).pow(63) + 1,
      BigInt(2).pow(64) - 1
    )

  private def versionKeyCase(v: BigInt): Json = {
    val key = ByteVector.view(v.toByteArray).takeRight(8).padLeft(8)
    val decoded = VersionKey.long.decode(key)
    val encoded =
      Option.when(v.isValidLong)(v.toLong).toRight(VersionedTrieError.InvalidVersion("not a Long")).flatMap(VersionKey.long.encode)
    require(decoded.isRight == encoded.isRight, s"version $v: decoding and encoding disagree")
    require(decoded.forall(d => BigInt(d) == v) && encoded.forall(_ == key), s"version $v: the key does not round-trip")
    Json.obj("version" -> v.asJson, "key" -> hex(key), "valid" -> decoded.isRight.asJson)
  }

  private def entry(k: StateKey, v: CanonicalValue): Json = Json.obj("key" -> k.value.asJson, "value" -> cbor(v))

  private def claim(k: StateKey, v: Option[CanonicalValue]): Json =
    Json.obj("key" -> k.value.asJson).deepMerge(v.fold(Json.obj())(x => Json.obj("value" -> cbor(x))))

  private def answer(a: Option[(Long, xyz.kd5ujc.core.hash.Digest[H])]): Json =
    a.fold(Json.obj()) { case (v, s) => Json.obj("version" -> v.asJson, "stateRoot" -> hex(s.bytes)) }

  private def proofJson(name: String, kind: String, at: Option[Long], p: VersionedTrieProof[Long, H], extra: Json): IO[Json] =
    orDie(VersionedTrieWire.encodeProof(p)).map { w =>
      Json
        .obj("name" -> name.asJson, "kind" -> kind.asJson, "cbor" -> cbor(w))
        .deepMerge(at.fold(Json.obj())(u => Json.obj("at" -> u.asJson)))
        .deepMerge(extra)
    }

  /** The `.regexp` of `state-key` in shared/schemas/versioned-trie.cddl: the grammar without its length limits. */
  val StateKeyPattern: String = "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*"

  /** Candidate state keys: whether each matches the schema pattern, and whether StateKey (with the limits) accepts it. */
  val stateKeyCases: List[String] = List(
    "a",
    "0",
    "fiber/1",
    "oracle/a/b",
    "a.b_c-d/0.9",
    "x" * 64,
    "x" * 65,
    List.fill(16)("a").mkString("/"),
    List.fill(17)("a").mkString("/"),
    List("x" * 64, "x" * 64, "x" * 64, "x" * 61).mkString("/"),
    List("x" * 64, "x" * 64, "x" * 64, "x" * 62).mkString("/"),
    "",
    "/",
    "a/",
    "/a",
    "a//b",
    "A",
    "a/B",
    ".a",
    "-a",
    "_a",
    "a/.b",
    "a b",
    "\u00e9",
    "a\n"
  )

  private def stateKeyCase(k: String): Json =
    Json.obj("key" -> k.asJson, "schema" -> k.matches(StateKeyPattern).asJson, "valid" -> StateKey.parse(k).isRight.asJson)

  def build: IO[Json] =
    attempt { implicit h =>
      for {
        s <- fresh
        store = s._2
        heads <- commitAll(store, commits)
        root  <- orDie(VersionedTrie.root(heads.last))
        commitJson <- commits.zip(heads).traverse {
          case ((v, b), hd) =>
            (orDie(VersionedTrie.root(hd)), store.delta(v).flatMap(d => orDie(VersionedTrieWire.encodeDelta(d)))).mapN { (r, d) =>
              Json.obj(
                "version"     -> v.asJson,
                "upserts"     -> b.last.collect { case (k, Some(x)) => entry(k, x) }.asJson,
                "removes"     -> b.last.collect { case (k, None) => k.value.asJson }.asJson,
                "stateRoot"   -> hex(hd.stateRoot.bytes),
                "historyRoot" -> hex(hd.historyRoot.bytes),
                "root"        -> hex(r.bytes),
                "delta"       -> cbor(d)
              )
            }
        }
        current <- store.proveState(StateQuery.keys(key("fiber/1"), key("fiber/2"), key("oracle/a/b"), key("nope")))
        currentJ <- proofJson(
          "current-keys",
          "state",
          None,
          current,
          Json.obj(
            "claims" -> List(
              claim(key("fiber/1"), Some(int(2))),
              claim(key("fiber/2"), None),
              claim(key("oracle/a/b"), Some(text("a"))),
              claim(key("nope"), None)
            ).asJson
          )
        )
        nsProof <- store.proveState(StateQuery.namespaces(ns("fiber"), ns("absent")))
        nsJ <- proofJson(
          "namespace-fiber",
          "state",
          None,
          nsProof,
          Json.obj(
            "listings" -> List(
              Json.obj(
                "namespace" -> "fiber".asJson,
                "entries" -> List(
                  entry(key("fiber/1"), int(2)),
                  entry(key("fiber/3"), get(CanonicalValue.array(Vector(int(1), CanonicalValue.CNull))))
                ).asJson
              ),
              Json.obj("namespace" -> "absent".asJson, "entries" -> List.empty[Json].asJson)
            ).asJson
          )
        )
        past <- store.proveState(StateQuery.keys(key("fiber/2"), key("oracle/a/b"), key("meta")), Some(1L))
        pastJ <- proofJson(
          "key-at-version-1",
          "historical",
          Some(1L),
          past,
          Json.obj(
            "claims" -> List(
              claim(key("fiber/2"), Some(text("x"))),
              claim(key("oracle/a/b"), Some(CanonicalValue.bytes(ByteVector(0x61, 0x61)))),
              claim(key("meta"), None)
            ).asJson
          )
        )
        v255   <- store.proveVersion(255L)
        v255J  <- proofJson("version-255", "version", Some(255L), v255, Json.obj("answer" -> answer(Some(255L -> heads(2).stateRoot))))
        v3     <- store.proveVersion(3L)
        v3J    <- proofJson("version-3-never-committed", "version", Some(3L), v3, Json.obj("answer" -> answer(None)))
        as299  <- store.proveAsOf(299L)
        as299J <- proofJson("as-of-299", "asOf", Some(299L), as299, Json.obj("answer" -> answer(Some(256L -> heads(3).stateRoot))))
        as0    <- store.proveAsOf(0L)
        as0J   <- proofJson("as-of-0-none", "asOf", Some(0L), as0, Json.obj("answer" -> answer(None)))
        hp     <- store.proveHead
        hpJ    <- proofJson("head", "head", None, hp, Json.obj())
        vs     <- store.proveVersions(2L, Some(300L))
        vsJ <- proofJson(
          "versions-2-until-300",
          "versions",
          Some(2L),
          vs,
          Json.obj(
            "until" -> 300L.asJson,
            "versions" -> List(2L -> heads(1), 255L -> heads(2), 256L -> heads(3)).map {
              case (v, hd) => answer(Some(v -> hd.stateRoot))
            }.asJson
          )
        )
        vt <- store.proveVersions(256L, None)
        vtJ <- proofJson(
          "versions-from-256",
          "versions",
          Some(256L),
          vt,
          Json.obj("versions" -> List(256L -> heads(3), 300L -> heads(4)).map { case (v, hd) => answer(Some(v -> hd.stateRoot)) }.asJson)
        )
        kat    <- headJson(heads.last)
        chunks <- store.snapshot(2).compile.toList
        wire   <- orDie(chunks.traverse(VersionedTrieWire.encodeChunk[Long, H]))
      } yield
        Json.obj(
          "spec"           -> "docs/specs/versioned-trie.md".asJson,
          "version"        -> 1.asJson,
          "hash"           -> "SHA-256".asJson,
          "width"          -> VersionKey.long.width.asJson,
          "emptyStateRoot" -> hex(T.empty.bytes),
          "head"           -> kat,
          "commits"        -> commitJson.asJson,
          "root"           -> hex(root.bytes),
          "proofs"         -> List(currentJ, nsJ, pastJ, v255J, v3J, as299J, as0J, hpJ, vsJ, vtJ).asJson,
          "snapshot"       -> Json.obj("chunkEntries" -> 2.asJson, "chunks" -> wire.map(hex).asJson),
          "stateKeys"      -> stateKeyCases.map(stateKeyCase).asJson,
          "versionKeys"    -> versionKeyCases.map(versionKeyCase).asJson
        )
    }.flatMap(_.leftMap(e => new IllegalStateException(e.toString)).liftTo[IO])

  def render: IO[String] = build.map(_.spaces2 + "\n")
}

object VersionedTrieVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    VersionedTrieVectors.render.flatMap(r => IO(Files.write(VersionedTrieVectors.path, r.getBytes(UTF_8)))).as(ExitCode.Success)
}

object VersionedTrieVectorsSuite extends SimpleIOSuite {

  pureTest("the schema's state-key pattern is the grammar of StateKey without its length limits") {
    val patternOnly =
      VersionedTrieVectors.stateKeyCases.filter(k => k.matches(VersionedTrieVectors.StateKeyPattern) && StateKey.parse(k).isLeft)
    VersionedTrieVectors.stateKeyCases.map { k =>
      expect(StateKey.parse(k).isLeft || k.matches(VersionedTrieVectors.StateKeyPattern), s"$k is a StateKey but fails the schema pattern")
    }.reduce(_ and _)
      .and(
        expect(
          patternOnly.forall(k =>
            k.length > StateKey.MaxLength || k.split("/", -1).length > StateKey.MaxSegments || k
              .split("/", -1)
              .exists(_.length > StateKey.MaxSegmentLength)
          ),
          s"keys the pattern accepts and StateKey rejects break only a length limit: $patternOnly"
        )
      )
      .and(expect(patternOnly.size >= 3, "the cases cover each limit"))
  }

  test("committed versioned-trie vectors match the reference implementation") {
    (IO(new String(Files.readAllBytes(VersionedTrieVectors.path), UTF_8)), VersionedTrieVectors.render).mapN { (committed, rendered) =>
      expect(
        committed == rendered,
        s"${VersionedTrieVectors.path} is stale; regenerate with: sbt \"accumulators/Test/runMain xyz.kd5ujc.accumulators.versioned.VersionedTrieVectorGen\""
      )
    }
  }

  test("the vector snapshot's chunks decode, pass the reader against the final head, and restore a store with the final root") {
    IO(new String(Files.readAllBytes(VersionedTrieVectors.path), UTF_8)).flatMap { text =>
      val json = get(io.circe.parser.parse(text))
      val head = get(json.hcursor.downField("head").as[Json])
      val trusted = VersionedTrie.Head[Long, H](
        get(head.hcursor.get[Long]("version")),
        get(xyz.kd5ujc.core.hash.Digest.fromHex[H](get(head.hcursor.get[String]("stateRoot")))),
        get(xyz.kd5ujc.core.hash.Digest.fromHex[H](get(head.hcursor.get[String]("historyRoot"))))
      )
      val chunks = get(json.hcursor.downField("snapshot").downField("chunks").as[List[String]])
        .traverse(c => VersionedTrieWire.decodeChunk[Long, H](get(ByteVector.fromHexDescriptive(c))))
      attempt(implicit h => fresh.flatMap(s => s._2.restoreFrom(trusted, fs2.Stream.emits(get(chunks))) >> s._2.head)).map { restored =>
        expect
          .same(Right(Some(trusted)), restored)
          .and(expect.same(Right(get(json.hcursor.get[String]("root"))), restored.map(_.fold("")(hd => get(VersionedTrie.root(hd)).hex))))
          .and(expect(get(chunks).size > 3, "several chunks"))
      }
    }
  }

  test("every vector proof verifies natively against the final root and gives its recorded answer") {
    IO(new String(Files.readAllBytes(VersionedTrieVectors.path), UTF_8)).map { text =>
      val json = get(io.circe.parser.parse(text))
      val root = get(xyz.kd5ujc.core.hash.Digest.fromHex[H](get(json.hcursor.get[String]("root"))))
      val proofs = get(json.hcursor.downField("proofs").as[List[Json]])
      def bytes(s: String) = get(ByteVector.fromHexDescriptive(s))
      def value(s: String) = get(Cbor.decode(bytes(s)))
      proofs.foldMap { pj =>
        val c = pj.hcursor
        val proof = get(VersionedTrieWire.decodeProof[Long, H](value(get(c.get[String]("cbor")))))
        val at = c.get[Long]("at").toOption
        val answer = c.downField("answer").success.map { a =>
          (a.get[Long]("version").toOption, a.get[String]("stateRoot").toOption).mapN((v, s) =>
            v -> get(xyz.kd5ujc.core.hash.Digest.fromHex[H](s))
          )
        }
        val claims = c.downField("claims").as[List[Json]].getOrElse(Nil).map { cl =>
          key(get(cl.hcursor.get[String]("key"))) -> cl.hcursor.get[String]("value").toOption.map(value)
        }
        get(c.get[String]("kind")) match {
          case "state" =>
            val state = get(VersionedTrieVerifier.verifyState(root, proof))
            claims.foldMap { case (k, v) => expect.same(Right(true), state.holds(k, v)) } |+|
            c.downField("listings").as[List[Json]].getOrElse(Nil).foldMap { l =>
              val n = ns(get(l.hcursor.get[String]("namespace")))
              val es = get(l.hcursor.downField("entries").as[List[Json]])
                .map(e => key(get(e.hcursor.get[String]("key"))) -> VersionedTrie.valueHash[H](value(get(e.hcursor.get[String]("value")))))
              expect.same(Right(es), state.namespace(n))
            }
          case "historical" =>
            val state = get(at.toRight("at").flatMap(u => VersionedTrieVerifier.verifyHistoricalKey(root, proof, u).leftMap(_.toString)))
            claims.foldMap { case (k, v) => expect.same(Right(true), state.holds(k, v)) }
          case "version" =>
            expect.same(
              Right(answer.flatten.map(_._2)),
              at.toRight(VersionedTrieError.Malformed("at")).flatMap(VersionedTrieVerifier.verifyVersion(root, proof, _))
            )
          case "asOf" =>
            expect.same(
              Right(answer.flatten),
              at.toRight(VersionedTrieError.Malformed("at")).flatMap(VersionedTrieVerifier.verifyAsOf(root, proof, _))
            )
          case "versions" =>
            val versions = c.downField("versions").as[List[Json]].getOrElse(Nil).map { v =>
              get(v.hcursor.get[Long]("version")) -> get(xyz.kd5ujc.core.hash.Digest.fromHex[H](get(v.hcursor.get[String]("stateRoot"))))
            }
            val until = c.get[Long]("until").toOption
            expect
              .same(
                Right(true),
                at.toRight(VersionedTrieError.Malformed("at"))
                  .flatMap(VersionedTrieVerifier.verifyVersions(root, proof, _, until, versions))
              )
              .and(
                expect.same(
                  Right(false),
                  at.toRight(VersionedTrieError.Malformed("at"))
                    .flatMap(VersionedTrieVerifier.verifyVersions(root, proof, _, until, versions.drop(1)))
                )
              )
          case _ =>
            expect(VersionedTrieVerifier.verifyHead(root, proof).isRight, "the head proof verifies")
        }
      }.and(expect.same(10, proofs.size))
    }
  }
}
