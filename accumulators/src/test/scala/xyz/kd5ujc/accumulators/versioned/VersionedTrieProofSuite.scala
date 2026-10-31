package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.ProofNode.{PBranch, PLeaf, Stub}
import xyz.kd5ujc.accumulators.trie.{KeyMode, Lookup, TrieProof, TrieProofs}
import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest
import xyz.kd5ujc.storage.Batch

import scodec.bits.ByteVector
import weaver.{Expectations, SimpleIOSuite}

/** Proofs from the store, checked by the pure verifier against the commitment root (docs/specs/versioned-trie.md §5, §6). */
object VersionedTrieProofSuite extends SimpleIOSuite {

  private type Store = VersionedTrieStore[IO, Long, H]

  private def ok[A](io: IO[Either[VersionedTrieError, A]])(f: A => Expectations): IO[Expectations] =
    io.map(_.fold(e => failure(s"unexpected $e"), f))

  private def rootOf(h: Head[Long, H]): Digest[H] = get(VersionedTrie.root(h))

  private val entries: List[(StateKey, CanonicalValue)] = List(
    key("fiber/1")    -> int(1),
    key("fiber/2")    -> text("x"),
    key("fiberx/1")   -> int(9),
    key("fiber")      -> int(0),
    key("oracle/a/b") -> CanonicalValue.bytes(ByteVector(0x61, 0x61))
  )

  /** Versions 1, 2, 4, 8, 16: each commit adds one entry; the gaps are versions never committed. */
  private def built(implicit h: cats.mtl.Handle[IO, VersionedTrieError]): IO[(Store, List[Head[Long, H]])] =
    fresh.flatMap(s => commitAll(s._2, entries.zipWithIndex.map { case (e, i) => (1L << i) -> Batch.put(e._1, e._2) }).map(s._2 -> _))

  test("top trie: slot 0x00 binds the version, 0x01 the state root, 0x02 the history root; unused slot 0x03 is provably absent") {
    ok(attempt(implicit h => built.flatMap(_._1.proveHead))) { proof =>
      val head = proof.head
      val verified = get(TrieProofs.verify(get(KeyMode.fixed(1)), rootOf(head), proof.top))
      val records = get(TrieProofs.encode(proof.top)) match {
        case CanonicalValue.CArray(rs) => rs.size
        case _                         => -1
      }
      expect
        .same(Lookup.Present(VersionedTrie.bytesHash[H](vk(head.version))), verified.lookup(VersionedTrie.VersionSlot.bits))
        .and(expect.same(Lookup.Present(VersionedTrie.bytesHash[H](head.stateRoot.bytes)), verified.lookup(VersionedTrie.StateSlot.bits)))
        .and(
          expect.same(Lookup.Present(VersionedTrie.bytesHash[H](head.historyRoot.bytes)), verified.lookup(VersionedTrie.HistorySlot.bits))
        )
        .and(expect.same(Lookup.Absent, verified.lookup(ByteVector(0x03).bits)))
        .and(expect.same(Lookup.Absent, verified.lookup(ByteVector(0xff).bits)))
        .and(expect.same(5, records))
    }
  }

  test("single key: a present key verifies against the root, an absent key is a verifiable absence, an unproven key is not covered") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => s.proveState(StateQuery.keys(key("fiber/1"), key("fiber/3"))).map(_ -> heads.last) }
    }) {
      case (proof, head) =>
        val state = get(VersionedTrieVerifier.verifyState(rootOf(head), proof))
        expect
          .same(Right(true), state.holds(key("fiber/1"), Some(int(1))))
          .and(expect.same(Right(false), state.holds(key("fiber/1"), Some(int(2)))))
          .and(expect.same(Right(true), state.holds(key("fiber/3"), None)))
          .and(expect.same(Left(VersionedTrieError.NotCovered("key oracle/a/b")), state.lookup(key("oracle/a/b"))))
    }
  }

  test("a batch proof covers every requested key and verifies once") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => s.proveState(StateQuery(entries.map(_._1) :+ key("nope"), Nil, Nil)).map(_ -> heads.last) }
    }) {
      case (proof, head) =>
        val state = get(VersionedTrieVerifier.verifyState(rootOf(head), proof))
        entries.foldMap { case (k, v) => expect.same(Right(true), state.holds(k, Some(v))) }
          .and(expect.same(Right(None), state.lookup(key("nope"))))
    }
  }

  test("empty state: any key is provably absent against the empty-trie state root") {
    ok(attempt { implicit h =>
      fresh.flatMap(s => s._2.commit(None, 0L, Batch.empty) >> s._2.proveState(StateQuery.keys(key("a"), key("b/c"))))
    }) { proof =>
      val state = get(VersionedTrieVerifier.verifyState(rootOf(proof.head), proof))
      expect
        .same(T.empty, proof.head.stateRoot)
        .and(expect.same(Right(None), state.lookup(key("a"))))
        .and(expect.same(Right(None), state.lookup(key("b/c"))))
    }
  }

  test("a namespace proof lists exactly the namespace's keys: `fiber` lists fiber/1 and fiber/2, never `fiber` or fiberx/1") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => s.proveState(StateQuery.namespaces(ns("fiber"), ns("absent"))).map(_ -> heads.last) }
    }) {
      case (proof, head) =>
        val state = get(VersionedTrieVerifier.verifyState(rootOf(head), proof))
        expect
          .same(
            Right(List(key("fiber/1") -> VersionedTrie.valueHash[H](int(1)), key("fiber/2") -> VersionedTrie.valueHash[H](text("x")))),
            state.namespace(ns("fiber"))
          )
          .and(expect.same(Right(Nil), state.namespace(ns("absent"))))
    }
  }

  test("a namespace proof is a plain prefix proof over the bytes utf8(ns + \"/\"), as trie_prefix_verify reads it") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, _) => s.proveState(StateQuery.namespaces(ns("fiber"))) }
    }) { proof =>
      val prefix = ByteVector("fiber/".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      val listing = proof.state
        .flatMap(p => TrieProofs.verify(get(VersionedTrie.stateMode), proof.head.stateRoot, p).toOption)
        .flatMap(_.prefix(prefix.bits))
      expect
        .same(Some(List(key("fiber/1").bytes, key("fiber/2").bytes)), listing.map(_.map(_._1.bytes)))
        .and(expect.same(ns("fiber").prefix, prefix))
    }
  }

  test("version attestations: every committed version is included, the head itself too; every gap is provably absent") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => (0L to 17L).toList.traverse(u => s.proveVersion(u).map(u -> _)).map(_ -> heads) }
    }) {
      case (proofs, heads) =>
        val root = rootOf(heads.last)
        val committed = heads.map(h => h.version -> h.stateRoot).toMap
        proofs.foldMap { case (u, p) => expect.same(Right(committed.get(u)), VersionedTrieVerifier.verifyVersion(root, p, u)) }
          .and(expect.same(Right(Some(heads.last.stateRoot)), VersionedTrieVerifier.verifyVersion(root, proofs(16)._2, 16L)))
    }
  }

  test("a tampered history proof, a wrong state root or a wrong commitment root is rejected") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => (s.proveVersion(4L), s.proveVersion(3L)).tupled.map(p => (p, heads)) }
    }) {
      case ((present, absent), heads) =>
        val root = rootOf(heads.last)
        val wrongRoot = present.copy(at = present.at.map(_.copy(stateRoot = Some(heads.head.stateRoot))))
        val claimsAbsent = present.copy(at = present.at.map(_.copy(stateRoot = None)))
        val claimsPresent = absent.copy(at = absent.at.map(_.copy(stateRoot = Some(heads.head.stateRoot))))
        val tampered = present.copy(history = present.history.map(p => TrieProof(p.top.map(stubbed))))
        expect(VersionedTrieVerifier.verifyVersion(root, wrongRoot, 4L).isLeft, "another state root")
          .and(expect(VersionedTrieVerifier.verifyVersion(root, claimsAbsent, 4L).isLeft, "a committed version claimed absent"))
          .and(expect(VersionedTrieVerifier.verifyVersion(root, claimsPresent, 3L).isLeft, "an absent version claimed present"))
          .and(expect(VersionedTrieVerifier.verifyVersion(root, tampered, 4L).isLeft, "a tampered history proof"))
          .and(expect(VersionedTrieVerifier.verifyVersion(rootOf(heads.head), present, 4L).isLeft, "another commitment root"))
    }
  }

  /** Replaces the value hash of the first leaf in pre-order: the proof no longer hashes to its root. */
  private def stubbed(n: xyz.kd5ujc.accumulators.trie.ProofNode[H]): xyz.kd5ujc.accumulators.trie.ProofNode[H] = {
    def go(n: xyz.kd5ujc.accumulators.trie.ProofNode[H]): Option[xyz.kd5ujc.accumulators.trie.ProofNode[H]] =
      n match {
        case PLeaf(s, _) => Some(PLeaf(s, T.value(ByteVector(0xee))))
        case PBranch(s, v, l, r) =>
          l.flatMap(go).map(x => PBranch(s, v, Some(x), r)).orElse(r.flatMap(go).map(x => PBranch(s, v, l, Some(x))))
        case Stub(_) => None
      }
    go(n).getOrElse(n)
  }

  test("the verifier supplies the keys: a proof about another version, or read with another key width, is rejected") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => s.proveVersion(4L).map(_ -> heads) }
    }) {
      case (proof, heads) =>
        val root = rootOf(heads.last)
        val narrow = get(VersionKey.unsignedLong(4))
        expect
          .same(
            Left(VersionedTrieError.Malformed(s"the proof is about version ${vk(4L).toHex}, not ${vk(8L).toHex}")),
            VersionedTrieVerifier.verifyVersion(root, proof, 8L)
          )
          .and(
            expect(VersionedTrieVerifier.verifyVersion(root, proof, 4L)(using narrow, T).isLeft, "a 4-byte reading of an 8-byte history")
          )
    }
  }

  test("an old version after many commits is still one history inclusion, of logarithmic size") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        heads <- commitAll(s._2, (0L until 600L).toList.map(v => v -> Batch.put(key("k"), int(v.toInt))))
        proof <- s._2.proveVersion(3L)
      } yield (heads, proof)
    }) {
      case (heads, proof) =>
        val records = proof.history.map(p => get(TrieProofs.encode(p))) match {
          case Some(CanonicalValue.CArray(rs)) => rs.size
          case _                               => -1
        }
        expect
          .same(Right(Some(heads(3).stateRoot)), VersionedTrieVerifier.verifyVersion(rootOf(heads.last), proof, 3L))
          .and(expect(records > 0 && records <= 2 * 64 + 1, s"$records records"))
    }
  }

  test("a key at a past version: history inclusion of its state root, then the key against that root") {
    ok(attempt { implicit h =>
      built.flatMap {
        case (s, heads) =>
          (
            s.proveState(StateQuery.keys(key("fiber/2"), key("fiber")), Some(2L)),
            s.proveState(StateQuery.keys(key("fiber")), Some(4L))
          ).tupled.map(p => (p, heads))
      }
    }) {
      case ((at2, at4), heads) =>
        val root = rootOf(heads.last)
        val s2 = get(VersionedTrieVerifier.verifyHistoricalKey(root, at2, 2L))
        val s4 = get(VersionedTrieVerifier.verifyHistoricalKey(root, at4, 4L))
        expect
          .same(Right(true), s2.holds(key("fiber/2"), Some(text("x"))))
          .and(expect.same(Right(None), s2.lookup(key("fiber"))))
          .and(expect.same(Right(true), s4.holds(key("fiber"), None)))
          .and(expect(VersionedTrieVerifier.verifyHistoricalKey(root, at2, 4L).isLeft, "a proof about version 2 is not about 4"))
          .and(expect(VersionedTrieVerifier.verifyState(root, at2).isLeft, "a historical proof is not about the current state"))
    }
  }

  test("as-of t: the latest version at or before t, or none before the first commit") {
    ok(attempt { implicit h =>
      built.flatMap { case (s, heads) => List(0L, 1L, 3L, 7L, 8L, 100L).traverse(t => s.proveAsOf(t).map(t -> _)).map(_ -> heads) }
    }) {
      case (proofs, heads) =>
        val root = rootOf(heads.last)
        val byVersion = heads.map(h => h.version -> h.stateRoot)
        proofs.foldMap {
          case (t, p) =>
            expect.same(Right(byVersion.filter(_._1 <= t).lastOption), VersionedTrieVerifier.verifyAsOf(root, p, t))
        }.and {
          val lying = proofs(2)._2.copy(at = Some(At(1L, Some(heads.head.stateRoot))))
          expect(VersionedTrieVerifier.verifyAsOf(root, lying, 3L).isLeft, "version 2 exists between 1 and 3")
        }
    }
  }

  test("the head proof shows the head invariant: the history ends at the head version with the head's state root") {
    ok(attempt(implicit h => built.flatMap { case (s, heads) => s.proveHead.map(_ -> heads) })) {
      case (proof, heads) =>
        val root = rootOf(heads.last)
        val noHistory = proof.copy(history = None)
        expect
          .same(Right(heads.last), VersionedTrieVerifier.verifyHead(root, proof))
          .and(expect(VersionedTrieVerifier.verifyHead(root, noHistory).isLeft, "the invariant needs the history tail"))
    }
  }

  test("an epoch of 256 versions is a key prefix: its listing is complete, and stops changing once the epoch is past") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        heads <- commitAll(s._2, (0L until 300L).toList.map(v => (2L * v) -> Batch.put(key("k"), int(v.toInt))))
        first <- s._2.proveVersions(0L, Some(256L))
        more  <- commitAll(s._2, (300L until 400L).toList.map(v => (2L * v) -> Batch.put(key("k"), int(v.toInt))), Some(598L))
        later <- s._2.proveVersions(0L, Some(256L))
      } yield (heads, first, more, later)
    }) {
      case (heads, first, more, later) =>
        val claimed = heads.filter(_.version < 256L).map(h => h.version -> h.stateRoot)
        val prefix = vk(0L).take(7)
        def listing(p: VersionedTrieProof[Long, H], root: Digest[H]) =
          p.history.flatMap(hp => TrieProofs.verify(get(KeyMode.fixed(8)), root, hp).toOption).flatMap(_.prefix(prefix.bits))
        expect
          .same(Right(true), VersionedTrieVerifier.verifyVersions(rootOf(heads.last), first, 0L, Some(256L), claimed))
          .and(expect.same(Right(false), VersionedTrieVerifier.verifyVersions(rootOf(heads.last), first, 0L, Some(256L), claimed.drop(1))))
          .and(expect.same(Right(true), VersionedTrieVerifier.verifyVersions(rootOf(more.last), later, 0L, Some(256L), claimed)))
          .and(expect.same(128, claimed.size))
          .and(expect(listing(first, heads.last.historyRoot).isDefined, "the epoch is listed by prefix"))
          .and(expect.same(listing(first, heads.last.historyRoot), listing(later, more.last.historyRoot)))
    }
  }

  test("proofs and deltas round-trip through the codecs generated from versioned-trie.cddl, as canonical CBOR") {
    ok(attempt { implicit h =>
      built.flatMap {
        case (s, _) =>
          (s.proveState(StateQuery.namespaces(ns("fiber")), Some(4L)), s.proveVersion(3L), s.proveAsOf(5L), s.proveHead, s.delta(4L)).tupled
      }
    }) {
      case (a, b, c, d, delta) =>
        def again(p: VersionedTrieProof[Long, H]) =
          VersionedTrieWire
            .encodeProof(p)
            .map(Cbor.encode)
            .flatMap(bs => Cbor.decode(bs).leftMap(e => VersionedTrieError.Malformed(e.toString)))
            .flatMap(VersionedTrieWire.decodeProof[Long, H])
        List(a, b, c, d)
          .foldMap(p => expect.same(Right(p), again(p)))
          .and(expect.same(Right(delta), VersionedTrieWire.encodeDelta(delta).flatMap(VersionedTrieWire.decodeDelta[Long, H])))
    }
  }

  test("before any commit there is no head to prove against") {
    attempt(implicit h => fresh.flatMap(_._2.proveHead)).map(r => expect.same(Left(VersionedTrieError.Empty), r))
  }
}
