package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.storage.{Batch, Namespace, RawStore, WatchedRawStore}

import weaver.{Expectations, SimpleIOSuite}

/**
 * Stale guards and racing writers (docs/specs/versioned-trie.md §10): every write is guarded on a revision that no
 * rollback can bring back, read before anything the write depends on.
 */
object GuardSuite extends SimpleIOSuite {

  private def ok[A](io: IO[Either[VersionedTrieError, A]])(f: A => Expectations): IO[Expectations] =
    io.map(_.fold(e => failure(s"unexpected $e"), f))

  test("a draft staged before a rollback is refused after the store re-reaches its sequence") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        _     <- commitAll(s._2, List(1L -> Batch.put(key("b"), int(1)), 2L -> Batch.put(key("b"), int(2))))
        draft <- s._2.preview(3L, Batch.put(key("c"), int(3)))
        _     <- s._2.rollback(1L)
        h20   <- s._2.commit(Some(1L), 20L, Batch.put(key("b"), int(20)))
        stale <- h.attempt(s._2.commitDraft(draft))
        head  <- s._2.head
        b     <- s._2.get(key("b"))
        state <- s._2.proveState(StateQuery.keys(key("b")))
      } yield (h20, stale, head, b, state)
    }) {
      case (h20, stale, head, b, state) =>
        expect(stale.left.exists(_.isInstanceOf[VersionedTrieError.VersionConflict]), s"$stale")
          .and(expect.same(Some(h20), head))
          .and(expect.same(Some(int(20)), b))
          .and(
            expect.same(
              Right(Right(true)),
              VersionedTrieVerifier.verifyState(get(VersionedTrie.root(h20)), state).map(_.holds(key("b"), Some(int(20))))
            )
          )
    }
  }

  test("a rollback racing a commit fails cleanly, leaving the commit, the index and a reopenable store") {
    ok(attempt { implicit h =>
      for {
        base  <- RawStore.inMemory[IO]
        raw   <- WatchedRawStore(base)
        s     <- open(raw)
        other <- open(base)
        _     <- commitAll(s, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("a"), int(2))))
        index <- IO.fromEither(Namespace("c/vidx/s").leftMap(e => new IllegalStateException(e.toString)))
        // another writer commits version 3 while the rollback reads the index
        _      <- raw.beforeNextScanOf(index.prefix, other.commit(Some(2L), 3L, Batch.put(key("a"), int(3))).void)
        result <- h.attempt(s.rollback(1L))
        again  <- open(base)
        head   <- again.head
        heads  <- List(1L, 2L, 3L).traverse(v => h.attempt(again.headAt(v)))
        a      <- again.get(key("a"))
      } yield (result, head, heads, a)
    }) {
      case (result, head, heads, a) =>
        expect(result.left.exists(_.isInstanceOf[VersionedTrieError.VersionConflict]), s"$result")
          .and(expect.same(Some(3L), head.map(_.version)))
          .and(expect(heads.forall(_.exists(_.isDefined)), s"$heads"))
          .and(expect.same(Some(int(3)), a))
    }
  }

  test("a snapshot is refused if a rollback and recommit return the store to its sequence while it is read") {
    ok(attempt { implicit h =>
      for {
        base  <- RawStore.inMemory[IO]
        raw   <- WatchedRawStore(base)
        s     <- open(raw)
        other <- open(base)
        _     <- commitAll(s, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("a"), int(2))))
        index <- IO.fromEither(Namespace("c/vidx/s").leftMap(e => new IllegalStateException(e.toString)))
        // between the entries and the versions, the store goes back to sequence 1 and forward to sequence 2 again
        _      <- raw.beforeNextScanOf(index.prefix, other.rollback(1L) >> other.commit(Some(1L), 5L, Batch.put(key("a"), int(5))).void)
        result <- h.attempt(s.snapshot().compile.toList)
      } yield result
    })(result => expect(result.left.exists(_.isInstanceOf[VersionedTrieError.VersionConflict]), s"$result"))
  }

  test("a delta read while another writer rolls back and recommits its version is refused, not mixed") {
    ok(attempt { implicit h =>
      for {
        base  <- RawStore.inMemory[IO]
        raw   <- WatchedRawStore(base)
        s     <- open(raw)
        other <- open(base)
        _     <- commitAll(s, List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("b"), int(2))))
        log   <- IO.fromEither(Namespace("c/vals/log").leftMap(e => new IllegalStateException(e.toString)))
        // the delta has read version 2's heads when version 2 is replaced by a different commit at the same sequence
        _      <- raw.beforeNextScanOf(log.prefix, other.rollback(1L) >> other.commit(Some(1L), 2L, Batch.put(key("c"), int(3))).void)
        result <- h.attempt(s.delta(2L))
      } yield result
    })(result => expect(result.left.exists(_.isInstanceOf[VersionedTrieError.VersionConflict]), s"$result"))
  }
}
