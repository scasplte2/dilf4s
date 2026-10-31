package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.storage.Batch

import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/** Replication verified locally against a trusted head (docs/specs/versioned-trie.md §7). */
object ReplicaSuite extends SimpleIOSuite with Checkers {

  private type R = Replica[Long, H]

  private def ok[A](io: IO[Either[VersionedTrieError, A]])(f: A => Expectations): IO[Expectations] =
    io.map(_.fold(e => failure(s"unexpected $e"), f))

  private val empty: R = get(Replica.empty[Long, H])

  private def follow(r: R, deltas: List[Delta[Long, H]]): Either[VersionedTrieError, List[R]] =
    deltas.foldLeftM(List(r))((acc, d) => Replica.applyDelta(acc.last, d).map(acc :+ _)).map(_.tail)

  private def snapshotOf(s: VersionedTrieStore[IO, Long, H]): IO[Snapshot[Long, H]] =
    s.snapshot.compile.toList.map(cs => get(Snapshot.fromChunks(cs)))

  test("a replica applying the delta stream matches the source's head at every step") {
    forall(stepsGen) { steps =>
      ok(attempt { implicit h =>
        fresh.flatMap(s => commitAll(s._2, steps).flatMap(heads => steps.traverse(st => s._2.delta(st._1)).map(heads -> _)))
      }) {
        case (heads, deltas) =>
          val followed = follow(empty, deltas)
          expect
            .same(Right(heads.map(Option(_))), followed.map(_.map(_.head)))
            .and(expect.same(Right(model(steps).last._2), followed.map(_.last.values)))
      }
    }
  }

  test("a replica follows across version-key byte carries (255 -> 256, 65535 -> 65536), where the history trie restructures") {
    val versions = List(254L, 255L, 256L, 257L, 65534L, 65535L, 65536L, 65537L, 16777215L, 16777216L)
    ok(attempt { implicit h =>
      fresh.flatMap(s =>
        commitAll(s._2, versions.map(v => v -> Batch.put(key(s"v/$v"), int(v.toInt)))).flatMap(heads =>
          versions.traverse(s._2.delta).map(heads -> _)
        )
      )
    }) {
      case (heads, deltas) =>
        expect
          .same(Right(heads.map(Option(_))), follow(empty, deltas).map(_.map(_.head)))
          .and(expect.same(heads.last, model(versions.map(v => v -> Batch.put(key(s"v/$v"), int(v.toInt)))).last._1))
    }
  }

  private def threeDeltas(implicit h: cats.mtl.Handle[IO, VersionedTrieError]): IO[(List[Head[Long, H]], List[Delta[Long, H]])] =
    fresh.flatMap { s =>
      val steps = List(
        1L -> (Batch.put(key("a"), int(1)) ++ Batch.put(key("b"), int(2))),
        2L -> (Batch.put(key("a"), int(3)) ++ Batch.delete(key("b"))),
        3L -> Batch.put(key("c"), int(4))
      )
      commitAll(s._2, steps).flatMap(heads => steps.traverse(st => s._2.delta(st._1)).map(heads -> _))
    }

  test("a delta that does not chain from the replica's head is rejected") {
    ok(attempt(implicit h => threeDeltas)) {
      case (_, deltas) =>
        val r1 = get(Replica.applyDelta(empty, deltas.head))
        expect(Replica.applyDelta(r1, deltas(2)).left.exists(_.isInstanceOf[VersionedTrieError.RootMismatch]), "skips version 2")
          .and(expect(Replica.applyDelta(empty, deltas(1)).isLeft, "an empty replica takes only a first delta"))
    }
  }

  test("forged roots are rejected by the replica's own recomputation") {
    ok(attempt(implicit h => threeDeltas)) {
      case (heads, deltas) =>
        val r1 = get(Replica.applyDelta(empty, deltas.head))
        val d = deltas(1)
        val forgedState = d.copy(head = d.head.copy(stateRoot = heads.head.stateRoot))
        val forgedHistory = d.copy(head = d.head.copy(historyRoot = heads.head.historyRoot))
        val forgedVersion = d.copy(head = d.head.copy(version = 9L))
        expect(Replica.applyDelta(r1, forgedState).left.exists {
          case VersionedTrieError.RootMismatch("state", _, _) => true
          case _                                              => false
        })
          .and(expect(Replica.applyDelta(r1, forgedHistory).left.exists {
            case VersionedTrieError.RootMismatch("history", _, _) => true
            case _                                                => false
          }))
          .and(expect(Replica.applyDelta(r1, forgedVersion).isLeft, "the version is in the history key"))
    }
  }

  test("a tampered delta is rejected: a modified upsert, a dropped remove, an added write") {
    ok(attempt(implicit h => threeDeltas)) {
      case (_, deltas) =>
        val r1 = get(Replica.applyDelta(empty, deltas.head))
        val d = deltas(1)
        List(
          d.copy(upserts = d.upserts.map { case (k, _) => k -> int(99) }),
          d.copy(removes = Nil),
          d.copy(upserts = d.upserts :+ (key("z") -> int(0))),
          d.copy(upserts = d.upserts :+ (key("b") -> int(0)))
        ).foldMap(t => expect(Replica.applyDelta(r1, t).isLeft, s"$t"))
    }
  }

  test("bootstrap: a snapshot verified against the trusted head installs a replica that then follows deltas") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        heads <- threeDeltas.flatMap(_ => commitAll(s._2, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("b"), int(2)))))
        snap  <- snapshotOf(s._2)
        _     <- s._2.commit(Some(2L), 5L, Batch.delete(key("a")))
        d     <- s._2.delta(5L)
        now   <- s._2.head
      } yield (heads, snap, d, now)
    }) {
      case (heads, snap, d, now) =>
        val seeded = Replica.fromSnapshot(heads.last, snap)
        expect
          .same(Right(Some(heads.last)), seeded.map(_.head))
          .and(expect.same(Right(now), seeded.flatMap(Replica.applyDelta(_, d)).map(_.head)))
          .and(expect.same(Right(Map(key("a") -> int(1), key("b") -> int(2))), seeded.map(_.values)))
    }
  }

  test("a snapshot that does not reproduce the trusted head is rejected, whatever head it claims") {
    ok(attempt { implicit h =>
      for {
        a  <- fresh
        ha <- commitAll(a._2, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("b"), int(2))))
        sa <- snapshotOf(a._2)
        b  <- fresh
        hb <- commitAll(b._2, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("b"), int(3))))
      } yield (ha.last, sa, hb.last)
    }) {
      case (trusted, snap, other) =>
        val tamperedEntry = snap.copy(entries = snap.entries.map { case (k, _) => k -> int(7) })
        val claimsOther = snap.copy(head = other)
        val forgedHistory = snap.copy(history = snap.history.map { case (v, _) => v -> other.stateRoot })
        val reordered = snap.copy(history = snap.history.reverse)
        val repeated = snap.copy(entries = snap.entries ++ snap.entries.take(1))
        expect(Replica.fromSnapshot(other, snap).isLeft, "another trusted head")
          .and(expect(Replica.fromSnapshot(trusted, tamperedEntry).isLeft, "a tampered entry"))
          .and(expect.same(Replica.fromSnapshot(trusted, snap).map(_.head), Replica.fromSnapshot(trusted, claimsOther).map(_.head)))
          .and(expect(Replica.fromSnapshot(trusted, forgedHistory).isLeft, "forged history contents"))
          .and(expect(Replica.fromSnapshot(trusted, reordered).isLeft, "history out of order"))
          .and(expect(Replica.fromSnapshot(trusted, repeated).isLeft, "a repeated entry"))
    }
  }

  test("history contents round-trip: a snapshot's versions rebuild the history root; the replica verifies proofs against it") {
    forall(stepsGen) { steps =>
      ok(attempt { implicit h =>
        fresh.flatMap(s => (commitAll(s._2, steps), snapshotOf(s._2), s._2.proveHead).tupled)
      }) {
        case (heads, snap, proof) =>
          val replica = Replica.fromSnapshot(heads.last, snap)
          expect
            .same(heads.map(h => h.version -> h.stateRoot), snap.history)
            .and(expect.same(Right(Some(get(VersionedTrie.root(heads.last)))), replica.flatMap(_.root)))
            .and(
              expect.same(
                Right(heads.last),
                replica.flatMap(r => r.root.flatMap(rt => VersionedTrieVerifier.verifyHead(rt.getOrElse(T.empty), proof)))
              )
            )
      }
    }
  }

  test("a replica bootstrapped from state and head alone answers proofs against the trusted root") {
    ok(attempt { implicit h =>
      fresh.flatMap(s => (commitAll(s._2, List(3L -> Batch.put(key("a/b"), int(1)))), snapshotOf(s._2)).tupled)
    }) {
      case (heads, snap) =>
        val replica = get(Replica.fromSnapshot(heads.last, snap))
        val stateProof = get(replica.state.prove(List(key("a/b").bytes, key("a/c").bytes)))
        val proof = VersionedTrieProof(heads.last, get(VersionedTrie.topProof(heads.last)), None, None, Some(stateProof))
        val state = VersionedTrieVerifier.verifyState(get(VersionedTrie.root(heads.last)), proof)
        expect
          .same(Right(Right(true)), state.map(_.holds(key("a/b"), Some(int(1)))))
          .and(expect.same(Right(Right(true)), state.map(_.holds(key("a/c"), None))))
    }
  }

  test("below the prune floor a delta is NotRetained; a snapshot re-seeds the replica and the delta stream resumes") {
    ok(attempt { implicit h =>
      for {
        s       <- fresh
        heads   <- commitAll(s._2, (1L to 6L).toList.map(v => v -> Batch.put(key(s"k/$v"), int(v.toInt))))
        early   <- (1L to 2L).toList.traverse(s._2.delta)
        _       <- s._2.prune(5L)
        evicted <- h.attempt(s._2.delta(3L))
        snap    <- snapshotOf(s._2)
        _       <- s._2.commit(Some(6L), 7L, Batch.put(key("k/7"), int(7)))
        resumed <- s._2.delta(7L)
        head    <- s._2.head
      } yield (heads, early, evicted, snap, resumed, head)
    }) {
      case (heads, early, evicted, snap, resumed, head) =>
        val lagging = follow(empty, early).map(_.last)
        val reseeded = Replica.fromSnapshot(heads.last, snap)
        expect
          .same(Left(VersionedTrieError.NotRetained(vk(3L))), evicted)
          .and(expect(lagging.isRight, "the replica followed up to version 2"))
          .and(expect.same(Right(head), reseeded.flatMap(Replica.applyDelta(_, resumed)).map(_.head)))
    }
  }

  test("a snapshot whose pieces straddle a commit is refused rather than torn") {
    attempt { implicit h =>
      for {
        s <- fresh
        _ <- commitAll(s._2, List(1L -> Batch.put(key("a"), int(1))))
        _ <- s._2.snapshot.evalTap {
          case _: SnapshotChunk.Top[Long, H] => s._2.commit(Some(1L), 2L, Batch.put(key("b"), int(2))).void
          case _                             => IO.unit
        }.compile.drain
      } yield ()
    }.map(r => expect.same(Left(VersionedTrieError.VersionConflict(Some(vk(1L)), Some(vk(2L)))), r))
  }
}
