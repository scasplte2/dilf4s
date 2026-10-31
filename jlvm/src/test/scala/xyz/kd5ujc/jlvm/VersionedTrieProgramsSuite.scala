package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.{IO, Resource}
import cats.mtl.Handle
import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.storage.{Batch, KeyCodec, RawStore}

import io.circe.Json
import scodec.bits.ByteVector
import weaver.{Expectations, IOSuite}

/**
 * JLVM programs verifying versioned-trie proofs (docs/specs/versioned-trie.md §9) with the plain trie operations: the
 * current state, absence, namespace slices, version inclusion and absence, a key at a past version, as-of, and epochs.
 * The proofs come from a live store and from the conformance vectors; the keys come from the programs.
 */
object VersionedTrieProgramsSuite extends IOSuite {

  private type H = Sha256
  type Res = Jlvm
  def sharedResource: Resource[IO, Jlvm] = Resource.eval(Jlvm.load[IO])

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def key(s:    String): StateKey = get(StateKey.parse(s))
  private def ns(s:     String): StateNamespace = get(StateNamespace.parse(s))
  private def int(i:    Long): CanonicalValue = CInt(BigInt(i))
  private def txt(s:    String): CanonicalValue = get(CanonicalValue.text(s))
  private def m(kvs:    (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs))
  private def a(vs:     CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))
  private def vk(v:     Long): ByteVector = get(VersionKey.long.encode(v))

  private def run(jlvm: Jlvm, program: String, data: (String, CanonicalValue)*): Either[JlvmError, CanonicalValue] =
    jlvm.evaluate(get(Parser.parse(program)), m(data*), Map.empty, Config()).result

  private def isCrypto(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Crypto(_, _)) => true
    case _                            => false
  }

  // ---- programs: the slot check binds an intermediate root to the trusted root, then the inner check uses it

  private def slot(n: String, witness: String) =
    s"""{"trie_verify": [{"object": {"fixed": 1}}, {"var": "root"}, "$n", {"var": "$witness"}, {"var": "top"}]}"""

  private val currentKey =
    s"""{"and": [${slot(
        "0x01",
        "s"
      )}, {"trie_verify": [{"object": {"variable": 256}}, {"var": "s"}, {"var": "k"}, {"var": "x"}, {"var": "state"}]}]}"""

  private val namespaceSlice =
    s"""{"and": [${slot(
        "0x01",
        "s"
      )}, {"trie_prefix_verify": [{"object": {"variable": 256}}, {"var": "s"}, {"var": "prefix"}, {"var": "entries"}, {"var": "state"}]}]}"""

  private val versionCommitted =
    s"""{"and": [${slot(
        "0x02",
        "h"
      )}, {"trie_verify": [{"object": {"fixed": 8}}, {"var": "h"}, {"int_to_bytes": [{"var": "u"}, 8]}, {"var": "su"}, {"var": "history"}]}]}"""

  private val asOf =
    s"""{"and": [${slot(
        "0x02",
        "h"
      )}, {"trie_range_verify": [{"object": {"fixed": 8}}, {"var": "h"}, {"int_to_bytes": [{"var": "p"}, 8]}, {"cat": [{"int_to_bytes": [{"var": "t"}, 8]}, "0x00"]}, [[{"int_to_bytes": [{"var": "p"}, 8]}, {"var": "sp"}]], {"var": "history"}]}]}"""

  private val asOfNone =
    s"""{"and": [${slot(
        "0x02",
        "h"
      )}, {"trie_range_verify": [{"object": {"fixed": 8}}, {"var": "h"}, "0x", {"cat": [{"int_to_bytes": [{"var": "t"}, 8]}, "0x00"]}, [], {"var": "history"}]}]}"""

  private val epoch =
    s"""{"and": [${slot(
        "0x02",
        "h"
      )}, {"trie_prefix_verify": [{"object": {"fixed": 8}}, {"var": "h"}, {"var": "prefix"}, {"var": "entries"}, {"var": "history"}]}]}"""

  /** The committed versions in `[from, until)`, with every history key built by the program from `{v, s}` pairs. */
  private def versionsIn(until: String) =
    s"""{"and": [${slot(
        "0x02",
        "h"
      )}, {"trie_range_verify": [{"object": {"fixed": 8}}, {"var": "h"}, {"int_to_bytes": [{"var": "from"}, 8]}, $until, {"map": [{"var": "vs"}, [{"int_to_bytes": [{"var": "v"}, 8]}, {"var": "s"}]]}, {"var": "history"}]}]}"""

  private val versionsBounded = versionsIn("""{"int_to_bytes": [{"var": "until"}, 8]}""")
  private val versionsFrom = versionsIn("null")

  private val versionSlot = slot("0x00", "v")

  /** The worked example of versioned-trie.md §9, taken verbatim from the spec. */
  private val specExample: IO[String] =
    IO(new String(Files.readAllBytes(Paths.get("docs", "specs", "versioned-trie.md")), UTF_8)).map { spec =>
      "(?s)```json\\n(.*?)```".r.findFirstMatchIn(spec).map(_.group(1)).getOrElse(sys.error("versioned-trie.md has no json example"))
    }

  // ---- proofs as program inputs: the wire record's parts are values the trie operations read directly

  final private case class Parts(
    root:    CanonicalValue,
    top:     CanonicalValue,
    s:       CanonicalValue,
    h:       CanonicalValue,
    v:       CanonicalValue,
    history: CanonicalValue,
    state:   CanonicalValue
  )

  private def parts(p: VersionedTrieProof[Long, H]): Parts = {
    val wire = get(VersionedTrieWire.encodeProof(p))
    def field(v: CanonicalValue, name: String): CanonicalValue = v match {
      case CMap(fs) => fs.getOrElse(name, CNull)
      case _        => CNull
    }
    val head = field(wire, "head")
    Parts(
      CBytes(get(VersionedTrie.root(p.head)).bytes),
      field(wire, "top"),
      field(head, "s"),
      field(head, "h"),
      field(head, "v"),
      field(wire, "history"),
      field(wire, "state")
    )
  }

  private def bytesOf(k: StateKey): CanonicalValue = CBytes(k.bytes)

  // ---- a live store: versions 1, 2, 4, 255, 256 (a byte carry), 300

  private val steps: List[(Long, Batch[StateKey, CanonicalValue])] = List(
    1L   -> (Batch.put(key("fiber/1"), int(1)) ++ Batch.put(key("fiber/2"), txt("x")) ++ Batch.put(key("fiberx/1"), int(9))),
    2L   -> Batch.put(key("oracle/a"), CBytes(ByteVector(0x61, 0x61))),
    4L   -> Batch.put(key("fiber/1"), int(4)),
    255L -> Batch.delete(key("fiber/2")),
    256L -> Batch.put(key("oracle/a"), txt("a")),
    300L -> Batch.put(key("meta"), m("k" -> int(1)))
  )

  private def withStore[A](f: VersionedTrieStore[IO, Long, H] => IO[A]): IO[A] =
    Handle
      .allowF[IO, VersionedTrieError] { implicit h =>
        for {
          raw   <- RawStore.inMemory[IO]
          store <- VersionedTrieStore.make[IO, Long, H](raw, "vt")
          _     <- steps.foldLeftM(Option.empty[Long]) { case (prev, (v, b)) => store.commit(prev, v, b).as(Option(v)) }
          a     <- f(store)
        } yield a
      }
      .rescue(e => IO.raiseError(new IllegalStateException(e.toString)))

  private def heads: IO[Map[Long, VersionedTrie.Head[Long, H]]] =
    withStore(s => steps.traverse(st => s.headAt(st._1).map(st._1 -> _)).map(_.collect { case (v, Some(h)) => v -> h }.toMap))

  test("the head's slots: version 0x00 is int_to_bytes [V, 8]; the wrong slot or a sign-flipped key is false") { jlvm =>
    withStore(_.proveHead).map { p =>
      val x = parts(p)
      expect
        .same(Right(True), run(jlvm, versionSlot, "root" -> x.root, "v" -> x.v, "top" -> x.top))
        .and(
          expect.same(
            Right(True),
            run(
              jlvm,
              s"""${slot("0x00", "v")}""".replace("""{"var": "v"}""", """{"int_to_bytes": [300, 8]}"""),
              "root" -> x.root,
              "top"  -> x.top
            )
          )
        )
        .and(
          expect.same(Right(False), run(jlvm, versionSlot, "root" -> x.root, "v" -> CBytes(KeyCodec.longKey.encode(300L)), "top" -> x.top))
        )
        .and(expect.same(Right(False), run(jlvm, slot("0x01", "s"), "root" -> x.root, "s" -> x.h, "top" -> x.top)))
        .and(expect.same(Right(True), run(jlvm, slot("0x03", "none"), "root" -> x.root, "top" -> x.top)))
    }
  }

  test("current keys: present values verify; a wrong value, or a value for an absent key, is false; an unproven key is a crypto error") {
    jlvm =>
      withStore(_.proveState(StateQuery.keys(key("fiber/1"), key("oracle/a"), key("fiber/2")))).map { p =>
        val x = parts(p)
        def check(k: String, v: CanonicalValue) =
          run(jlvm, currentKey, "root" -> x.root, "top" -> x.top, "s" -> x.s, "state" -> x.state, "k" -> bytesOf(key(k)), "x" -> v)
        expect
          .same(Right(True), check("fiber/1", int(4)))
          .and(expect.same(Right(True), check("oracle/a", txt("a"))))
          .and(expect.same(Right(False), check("oracle/a", CBytes(ByteVector(0x61, 0x61)))))
          .and(expect.same(Right(False), check("fiber/1", int(1))))
          .and(expect.same(Right(True), check("fiber/2", CNull)))
          .and(expect.same(Right(False), check("fiber/2", txt("x"))))
          .and(expect(isCrypto(check("meta", CNull)), "a key the proof does not reach"))
      }
  }

  test("a forged state root fails the slot check even when the inner proof is self-consistent") { jlvm =>
    (
      withStore(_.proveState(StateQuery.keys(key("fiber/1")))),
      withStore(_.proveState(StateQuery.keys(key("fiber/1")), Some(1L)))
    ).tupled.map {
      case (now, past) =>
        val x = parts(now)
        val old = parts(past)
        // the version-1 state proof proves fiber/1 = 1 against version 1's state root, which is not the head's
        val pastRoot = past.at.flatMap(_.stateRoot).fold[CanonicalValue](CNull)(r => CBytes(r.bytes))
        expect.same(
          Right(False),
          run(
            jlvm,
            currentKey,
            "root"  -> x.root,
            "top"   -> x.top,
            "s"     -> pastRoot,
            "state" -> old.state,
            "k"     -> bytesOf(key("fiber/1")),
            "x"     -> int(1)
          )
        )
    }
  }

  test("namespace slices: the fiber namespace lists exactly fiber/1, never fiberx/1; a missing entry is false") { jlvm =>
    withStore(_.proveState(StateQuery.namespaces(ns("fiber")))).map { p =>
      val x = parts(p)
      def check(entries: CanonicalValue) =
        run(
          jlvm,
          namespaceSlice,
          "root"    -> x.root,
          "top"     -> x.top,
          "s"       -> x.s,
          "state"   -> x.state,
          "prefix"  -> CBytes(ns("fiber").prefix),
          "entries" -> entries
        )
      expect
        .same(Right(True), check(a(a(bytesOf(key("fiber/1")), int(4)))))
        .and(expect.same(Right(False), check(a())))
        .and(expect.same(Right(False), check(a(a(bytesOf(key("fiber/1")), int(4)), a(bytesOf(key("fiberx/1")), int(9))))))
    }
  }

  test("version inclusion and absence, with keys built by int_to_bytes; the storage key codec's bytes do not verify") { jlvm =>
    (withStore(s => List(4L, 255L, 256L, 3L, 257L).traverse(u => s.proveVersion(u).map(u -> _))), heads).tupled.map {
      case (proofs, hs) =>
        proofs.foldMap {
          case (u, p) =>
            val x = parts(p)
            val su = hs.get(u).fold[CanonicalValue](CNull)(h => CBytes(h.stateRoot.bytes))
            val claim = (s: CanonicalValue) =>
              run(jlvm, versionCommitted, "root" -> x.root, "top" -> x.top, "h" -> x.h, "history" -> x.history, "u" -> int(u), "su" -> s)
            expect(claim(su) == Right(True), s"version $u")
              .and(expect(claim(if (su == CNull) CBytes(hs(4L).stateRoot.bytes) else CNull) != Right(True), s"the opposite claim for $u"))
        }.and {
          val x = parts(proofs.head._2)
          val flipped = run(
            jlvm,
            versionCommitted.replace("""{"int_to_bytes": [{"var": "u"}, 8]}""", """{"var": "uk"}"""),
            "root"    -> x.root,
            "top"     -> x.top,
            "h"       -> x.h,
            "history" -> x.history,
            "uk"      -> CBytes(KeyCodec.longKey.encode(4L)),
            "su"      -> CBytes(hs(4L).stateRoot.bytes)
          )
          expect(flipped != Right(True), s"KeyCodec.longKey(4) is not the history key: $flipped")
        }
    }
  }

  test("a key at a past version: the spec's worked example verifies, and rejects the current value") { jlvm =>
    (specExample, withStore(_.proveState(StateQuery.keys(key("fiber/1"), key("oracle/a")), Some(2L))), heads).tupled.map {
      case (program, p, hs) =>
        val x = parts(p)
        def check(k: String, v: CanonicalValue, u: Long = 2L) =
          run(
            jlvm,
            program,
            "root"    -> x.root,
            "top"     -> x.top,
            "h"       -> x.h,
            "history" -> x.history,
            "u"       -> int(u),
            "su"      -> CBytes(hs(2L).stateRoot.bytes),
            "k"       -> bytesOf(key(k)),
            "x"       -> v,
            "state"   -> x.state
          )
        expect
          .same(Right(True), check("fiber/1", int(1)))
          .and(expect.same(Right(True), check("oracle/a", CBytes(ByteVector(0x61, 0x61)))))
          .and(expect.same(Right(False), check("fiber/1", int(4))))
          .and(expect(check("fiber/1", int(1), u = 4L) != Right(True), "version 2's state root is not version 4's"))
    }
  }

  test("as-of: the latest version at or before t, through a range proof; a skipped version or a later one is false") { jlvm =>
    (withStore(s => List(3L, 255L, 299L, 0L).traverse(t => s.proveAsOf(t).map(t -> _))), heads).tupled.map {
      case (proofs, hs) =>
        def base(p: VersionedTrieProof[Long, H]) = {
          val x = parts(p)
          List("root" -> x.root, "top" -> x.top, "h" -> x.h, "history" -> x.history)
        }
        def at(t: Long, pv: Long, p: VersionedTrieProof[Long, H]) =
          run(jlvm, asOf, (base(p) ++ List("t" -> int(t), "p" -> int(pv), "sp" -> CBytes(hs(pv).stateRoot.bytes)))*)
        val byT = proofs.toMap
        expect
          .same(Right(True), at(3L, 2L, byT(3L)))
          .and(expect.same(Right(True), at(255L, 255L, byT(255L))))
          .and(expect.same(Right(True), at(299L, 256L, byT(299L))))
          .and(expect(at(299L, 255L, byT(299L)) != Right(True), "256 lies between 255 and 299"))
          .and(expect.same(Right(True), run(jlvm, asOfNone, (base(byT(0L)) :+ ("t" -> int(0L)))*)))
          .and(expect(run(jlvm, asOfNone, (base(byT(3L)) :+ ("t" -> int(3L)))*) != Right(True), "versions 1 and 2 are at or before 3"))
    }
  }

  test("an epoch of 256 versions is listed completely by a prefix proof over its first seven key bytes") { jlvm =>
    (withStore(_.proveVersions(0L, Some(256L))), heads).tupled.map {
      case (p, hs) =>
        val x = parts(p)
        val listed = List(1L, 2L, 4L, 255L).map(u => a(CBytes(vk(u)), CBytes(hs(u).stateRoot.bytes)))
        def check(es: List[CanonicalValue]) =
          run(
            jlvm,
            epoch,
            "root"    -> x.root,
            "top"     -> x.top,
            "h"       -> x.h,
            "history" -> x.history,
            "prefix"  -> CBytes(vk(0L).take(7)),
            "entries" -> a(es*)
          )
        expect.same(Right(True), check(listed)).and(expect.same(Right(False), check(listed.dropRight(1))))
    }
  }

  private def versionList(vs: List[(Long, ByteVector)]): CanonicalValue =
    a(vs.map { case (v, s) => m("v" -> int(v), "s" -> CBytes(s)) }*)

  test("versions [from, until): a range proof lists exactly the committed versions; one missing, extra or moved is false") { jlvm =>
    (withStore(s => (s.proveVersions(2L, Some(300L)), s.proveVersions(4L, None)).tupled), heads).tupled.map {
      case ((bounded, open), hs) =>
        def run2(program: String, p: VersionedTrieProof[Long, H], extra: (String, CanonicalValue)*) = {
          val x = parts(p)
          run(jlvm, program, (List("root" -> x.root, "top" -> x.top, "h" -> x.h, "history" -> x.history) ++ extra)*)
        }
        val inBounds = List(2L, 4L, 255L, 256L).map(v => v -> hs(v).stateRoot.bytes)
        val fromFour = List(4L, 255L, 256L, 300L).map(v => v -> hs(v).stateRoot.bytes)
        def bounded2(vs: List[(Long, ByteVector)]) =
          run2(versionsBounded, bounded, "from" -> int(2L), "until" -> int(300L), "vs" -> versionList(vs))
        expect
          .same(Right(True), bounded2(inBounds))
          .and(expect.same(Right(False), bounded2(inBounds.drop(1))))
          .and(expect.same(Right(False), bounded2(inBounds :+ (300L -> hs(300L).stateRoot.bytes))))
          .and(expect.same(Right(False), bounded2(inBounds.map { case (v, r) => (if (v == 4L) 3L else v) -> r })))
          .and(expect.same(Right(True), run2(versionsFrom, open, "from" -> int(4L), "vs" -> versionList(fromFour))))
          .and(expect.same(Right(False), run2(versionsFrom, open, "from" -> int(4L), "vs" -> versionList(fromFour.dropRight(1)))))
          // the proof from 4 leaves versions 1 and 2 as a stub: asking it for everything from 1 is no answer
          .and(expect(isCrypto(run2(versionsFrom, open, "from" -> int(1L), "vs" -> versionList(fromFour))), "not covered"))
    }
  }

  test("versions above 2^63 - 1: int_to_bytes still builds a key, which a head proof shows absent; 2^64 is a Domain error") { jlvm =>
    (withStore(_.proveHead), heads).tupled.map {
      case (p, hs) =>
        val x = parts(p)
        def claim(u: BigInt, su: CanonicalValue) =
          run(jlvm, versionCommitted, "root" -> x.root, "top" -> x.top, "h" -> x.h, "history" -> x.history, "u" -> CInt(u), "su" -> su)
        val cap = BigInt(Long.MaxValue)
        val head = CBytes(hs(300L).stateRoot.bytes)
        expect
          .same(Right(True), claim(300, head))
          .and(expect.same(Right(True), claim(cap, CNull)))
          .and(expect.same(Right(True), claim(cap + 1, CNull)))
          .and(expect.same(Right(False), claim(cap + 1, head)))
          .and(expect.same(Right(True), claim(BigInt(2).pow(64) - 1, CNull)))
          .and(
            expect(
              claim(BigInt(2).pow(64), CNull) match {
                case Left(JlvmError.Domain("int_to_bytes", _)) => true
                case _                                         => false
              },
              "2^64 does not fit 8 bytes"
            )
          )
    }
  }

  test("every proof in versioned_trie_v1.json verifies in the JLVM with the answers the vectors record") { jlvm =>
    (specExample, IO(new String(Files.readAllBytes(Paths.get("shared", "vectors", "versioned_trie_v1.json")), UTF_8))).tupled.map {
      case (pastKey, text) =>
        val json = get(io.circe.parser.parse(text))
        def bytes(s: String) = get(ByteVector.fromHexDescriptive(s))
        def value(s: String) = get(Cbor.decode(bytes(s)))
        val root = CBytes(bytes(get(json.hcursor.get[String]("root"))))
        get(json.hcursor.downField("proofs").as[List[Json]]).foldMap { pj =>
          val c = pj.hcursor
          val x = parts(get(VersionedTrieWire.decodeProof[Long, H](value(get(c.get[String]("cbor"))))))
          val at = c.get[Long]("at").toOption
          val answer =
            c.downField("answer").success.flatMap(a => (a.get[Long]("version").toOption, a.get[String]("stateRoot").toOption).tupled)
          val claims = c.downField("claims").as[List[Json]].getOrElse(Nil).map { cl =>
            key(get(cl.hcursor.get[String]("key"))) -> cl.hcursor.get[String]("value").toOption.fold[CanonicalValue](CNull)(value)
          }
          val common = List("root" -> root, "top" -> x.top, "s" -> x.s, "h" -> x.h, "history" -> x.history, "state" -> x.state)
          get(c.get[String]("kind")) match {
            case "state" =>
              claims.foldMap {
                case (k, v) => expect.same(Right(True), run(jlvm, currentKey, (common ++ List("k" -> bytesOf(k), "x" -> v))*))
              } |+|
              c.downField("listings").as[List[Json]].getOrElse(Nil).foldMap { l =>
                val es = get(l.hcursor.downField("entries").as[List[Json]])
                  .map(e => a(bytesOf(key(get(e.hcursor.get[String]("key")))), value(get(e.hcursor.get[String]("value")))))
                val prefix = CBytes(ns(get(l.hcursor.get[String]("namespace"))).prefix)
                expect.same(Right(True), run(jlvm, namespaceSlice, (common ++ List("prefix" -> prefix, "entries" -> a(es*)))*))
              }
            case "version" =>
              val su = answer.fold[CanonicalValue](CNull)(an => CBytes(bytes(an._2)))
              expect.same(Right(True), run(jlvm, versionCommitted, (common ++ List("u" -> int(at.getOrElse(-1L)), "su" -> su))*))
            case "asOf" =>
              answer.fold(expect.same(Right(True), run(jlvm, asOfNone, (common :+ ("t" -> int(at.getOrElse(-1L))))*))) { an =>
                expect.same(
                  Right(True),
                  run(jlvm, asOf, (common ++ List("t" -> int(at.getOrElse(-1L)), "p" -> int(an._1), "sp" -> CBytes(bytes(an._2))))*)
                )
              }
            case "historical" =>
              val proof = get(VersionedTrieWire.decodeProof[Long, H](value(get(c.get[String]("cbor")))))
              val su = proof.at.flatMap(_.stateRoot).fold[CanonicalValue](CNull)(r => CBytes(r.bytes))
              claims.foldMap {
                case (k, v) =>
                  expect.same(
                    Right(True),
                    run(jlvm, pastKey, (common ++ List("u" -> int(at.getOrElse(-1L)), "su" -> su, "k" -> bytesOf(k), "x" -> v))*)
                  )
              }
            case "versions" =>
              val vs = get(c.downField("versions").as[List[Json]]).map { vj =>
                get(vj.hcursor.get[Long]("version")) -> bytes(get(vj.hcursor.get[String]("stateRoot")))
              }
              val from = "from" -> int(at.getOrElse(-1L))
              expect.same(
                Right(True),
                c.get[Long]("until")
                  .toOption
                  .fold(run(jlvm, versionsFrom, (common ++ List(from, "vs" -> versionList(vs)))*))(u =>
                    run(jlvm, versionsBounded, (common ++ List(from, "until" -> int(u), "vs" -> versionList(vs)))*)
                  )
              )
            case "head" => expect.same(Right(True), run(jlvm, versionSlot, "root" -> root, "top" -> x.top, "v" -> x.v))
            case other  => failure(s"proof kind $other has no JLVM check")
          }
        }
    }
  }

  test("versionKeys in versioned_trie_v1.json: the JLVM's int_to_bytes [v, 8] is each key, valid or not") { jlvm =>
    IO(new String(Files.readAllBytes(Paths.get("shared", "vectors", "versioned_trie_v1.json")), UTF_8)).map { text =>
      val rows = get(get(io.circe.parser.parse(text)).hcursor.downField("versionKeys").as[List[Json]])
      expect(rows.exists(r => r.hcursor.get[Boolean]("valid").contains(false)), "the vectors cross the cap") |+|
      rows.foldMap { r =>
        val v = get(r.hcursor.get[BigInt]("version"))
        val key = get(ByteVector.fromHexDescriptive(get(r.hcursor.get[String]("key"))))
        expect.same(Right(CBytes(key)), run(jlvm, """{"int_to_bytes": [{"var": "v"}, 8]}""", "v" -> CInt(v)))
      }
    }
  }
}
