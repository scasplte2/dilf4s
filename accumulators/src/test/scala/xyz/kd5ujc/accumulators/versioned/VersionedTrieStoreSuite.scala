package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{Trie, TrieProofs}
import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}
import xyz.kd5ujc.storage.{Batch, ByteRange, Namespace, RawStore, WatchedRawStore}

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/** The store against the from-scratch reference model, and its write discipline (docs/specs/versioned-trie.md §4). */
object VersionedTrieStoreSuite extends SimpleIOSuite with Checkers {

  private def ok[A](io: IO[Either[VersionedTrieError, A]])(f: A => weaver.Expectations): IO[weaver.Expectations] =
    io.map(_.fold(e => failure(s"unexpected $e"), f))

  test("PROPERTY: every commit's head and root equal a from-scratch build of the model state and history") {
    forall(stepsGen) { steps =>
      val expected = model(steps)
      ok(attempt { implicit h =>
        for {
          s     <- fresh
          heads <- commitAll(s._2, steps)
          state <- s._2.scan().compile.toList
        } yield (heads, state)
      }) {
        case (heads, state) =>
          expect
            .same(expected.map(_._1), heads)
            .and(expect.same(expected.last._2.toList.sortBy(_._1), state))
            .and(expect.same(heads.map(h => get(VersionedTrie.root(h))), expected.map(e => get(VersionedTrie.root(e._1)))))
      }
    }
  }

  test("the values store agrees with the state trie at every version: its entries rebuild that version's state root") {
    forall(stepsGen) { steps =>
      ok(attempt { implicit h =>
        for {
          s     <- fresh
          heads <- commitAll(s._2, steps)
          keys = steps.flatMap(_._2.last.map(_._1)).distinct
          states <- steps.traverse(st => keys.traverseFilter(k => s._2.get(k, Some(st._1)).map(_.map(k -> _))).map(_.toMap))
        } yield (heads, states)
      }) {
        case (heads, states) => expect.same(heads.map(_.stateRoot), states.map(stateRoot))
      }
    }
  }

  test("the state trie at the head stores exactly a from-scratch build once older versions are pruned") {
    forall(stepsGen) { steps =>
      val expected = model(steps).last
      ok(attempt { implicit h =>
        for {
          s     <- fresh
          _     <- commitAll(s._2, steps)
          _     <- s._2.prune(steps.last._1)
          nodes <- get(Namespace("c/state/node")).pipe(n => s._1.scan(ByteRange.prefix(n.prefix)).compile.count)
        } yield nodes
      }) { nodes =>
        val built = get(
          Trie.fromEntries[H](
            get(VersionedTrie.stateMode),
            expected._2.toList.map { case (k, v) => k.bytes -> VersionedTrie.valueHash[H](v) }
          )
        )
        expect.same(built.nodeCount.toLong, nodes)
      }
    }
  }

  test("null-containing values from JSON commit to the same root as the same entries without the null fields") {
    val withNulls = get(CanonicalJson.parse("""{"a": 1, "b": null, "c": {"d": null, "e": [1, null]}}"""))
    val without = get(CanonicalJson.parse("""{"a": 1, "c": {"e": [1, null]}}"""))
    ok(attempt { implicit h =>
      for {
        one <- fresh.flatMap(s => s._2.commit(None, 0L, Batch.put(key("k"), withNulls)))
        two <- fresh.flatMap(s => s._2.commit(None, 0L, Batch.put(key("k"), without)))
        nul <- fresh.flatMap(s => s._2.commit(None, 0L, Batch.put(key("k"), CanonicalValue.CNull) ++ Batch.put(key("j"), int(1))))
        abs <- fresh.flatMap(s => s._2.commit(None, 0L, Batch.put(key("j"), int(1))))
      } yield (one, two, nul, abs)
    }) {
      case (one, two, nul, abs) =>
        expect.same(one, two).and(expect.same(abs, nul)).and(expect.same(stateRoot(Map(key("k") -> without)), one.stateRoot))
    }
  }

  test("array nulls in values are significant: a different root") {
    val a = get(CanonicalJson.parse("[1, null]"))
    val b = get(CanonicalJson.parse("[1]"))
    expect(stateRoot(Map(key("k") -> a)) != stateRoot(Map(key("k") -> b))).pure[IO]
  }

  test("an explicit null field commits to the same root as an absent field") {
    val explicit = get(CanonicalJson.parse("""{"owner": "x", "note": null}"""))
    val absent = get(CanonicalJson.parse("""{"owner": "x"}"""))
    expect.same(stateRoot(Map(key("k") -> absent)), stateRoot(Map(key("k") -> explicit))).pure[IO]
  }

  test("empty <-> nonempty stays canonical: removing every key gives the empty-trie root again") {
    ok(attempt { implicit h =>
      for {
        s  <- fresh
        h0 <- s._2.commit(None, 0L, Batch.empty)
        h1 <- s._2.commit(Some(0L), 1L, Batch.put(key("a"), int(1)) ++ Batch.put(key("b/c"), int(2)))
        h2 <- s._2.commit(Some(1L), 2L, Batch.deletes(List(key("a"), key("b/c"))))
        h3 <- s._2.commit(Some(2L), 3L, Batch.put(key("a"), int(1)) ++ Batch.put(key("b/c"), int(2)))
      } yield (h0, h1, h2, h3)
    }) {
      case (h0, h1, h2, h3) =>
        expect
          .same(T.empty, h0.stateRoot)
          .and(expect.same(T.empty, h2.stateRoot))
          .and(expect.same(h1.stateRoot, h3.stateRoot))
          .and(expect(h1.historyRoot != h3.historyRoot, "history differs"))
    }
  }

  test("root determinism: the same entries written in any order and batching give the same state root") {
    forall(Gen.listOfN(8, Gen.zip(keyGen, valueGen)).flatMap(es => Gen.pick(es.size, es).map(es -> _.toList))) {
      case (entries, shuffled) =>
        val distinct = entries.toMap
        val inOrder = Batch.puts(distinct.toList.sortBy(_._1))
        val reordered = Batch.puts(shuffled.filter(e => distinct.get(e._1).contains(e._2)).distinctBy(_._1).reverse)
        ok(attempt { implicit h =>
          for {
            a <- fresh.flatMap(s => s._2.commit(None, 7L, inOrder))
            b <- fresh.flatMap { s =>
              s._2.commit(None, 3L, Batch.puts(reordered.ops.toList.take(3).collect { case xyz.kd5ujc.storage.Op.Put(k, v) => k -> v })) >>
              s._2.commit(Some(3L), 7L, reordered)
            }
          } yield (a, b)
        }) {
          case (a, b) => expect.same(a.stateRoot, b.stateRoot).and(expect.same(stateRoot(distinct), a.stateRoot))
        }
    }
  }

  test("the history is committed: the same state at different versions gives different roots") {
    ok(attempt { implicit h =>
      for {
        s  <- fresh
        h1 <- s._2.commit(None, 1L, Batch.put(key("a"), int(1)))
        h2 <- s._2.commit(Some(1L), 2L, Batch.empty)
        t  <- fresh
        h5 <- t._2.commit(None, 5L, Batch.put(key("a"), int(1)))
      } yield (h1, h2, h5)
    }) {
      case (h1, h2, h5) =>
        val roots = List(h1, h2, h5).map(h => get(VersionedTrie.root(h)))
        expect
          .same(h1.stateRoot, h2.stateRoot)
          .and(expect.same(h1.stateRoot, h5.stateRoot))
          .and(expect.same(3, roots.distinct.size))
    }
  }

  test("preview equals the commit it previews, for several alternative batches, and writes nothing") {
    forall(Gen.zip(stepsGen, Gen.listOfN(3, batchGen))) {
      case (steps, alternatives) =>
        val next = steps.last._1 + 1
        ok(attempt { implicit h =>
          for {
            raw    <- RawStore.inMemory[IO].flatMap(WatchedRawStore(_))
            store  <- open(raw)
            _      <- commitAll(store, steps)
            before <- (raw.contents, raw.writes).tupled
            drafts <- alternatives.traverse(b => store.preview(next, b))
            after  <- (raw.contents, raw.writes).tupled
            heads <- alternatives.traverse { b =>
              RawStore.inMemory[IO].flatMap(open).flatMap(fresh => commitAll(fresh, steps :+ (next -> b)).map(_.last))
            }
          } yield (before, after, drafts, heads)
        }) {
          case (before, after, drafts, heads) =>
            val expected = alternatives.map(b => model(steps :+ (next -> b)).last._1)
            expect(before == after, "previews write nothing")
              .and(expect.same(expected, drafts.map(_.head)))
              .and(expect.same(heads, drafts.map(_.head)))
              .and(expect.same(drafts.map(d => get(VersionedTrie.root(d.head))), drafts.map(_.root)))
        }
    }
  }

  test("chained drafts (a preview on a preview) equal sequential commits, and commit as one write") {
    forall(stepsGen) { steps =>
      val expected = model(steps).map(_._1)
      ok(attempt { implicit h =>
        for {
          raw    <- RawStore.inMemory[IO].flatMap(WatchedRawStore(_))
          store  <- open(raw)
          first  <- store.preview(steps.head._1, steps.head._2)
          drafts <- steps.tail.foldLeftM(List(first)) { case (acc, (v, b)) => store.previewOn(acc.last, v, b).map(acc :+ _) }
          empty  <- raw.contents
          head   <- store.commitDraft(drafts.last)
          writes <- raw.writes
          now    <- store.head
          all    <- steps.traverse(s => store.headAt(s._1))
        } yield (drafts.map(_.head), empty, head, writes, now, all)
      }) {
        case (drafts, empty, head, writes, now, all) =>
          expect
            .same(expected, drafts)
            .and(expect(empty.isEmpty, "drafts write nothing"))
            .and(expect.same(expected.last, head))
            .and(expect.same((0, 1), writes))
            .and(expect.same(Some(expected.last), now))
            .and(expect.same(expected.map(Option(_)), all))
      }
    }
  }

  test("re-committing the head version is idempotent for the same state and refused for a different one") {
    ok(attempt { implicit h =>
      for {
        s       <- fresh
        _       <- s._2.commit(None, 1L, Batch.put(key("a"), int(1)))
        h2      <- s._2.commit(Some(1L), 2L, Batch.put(key("b"), int(2)) ++ Batch.put(key("a"), int(1)))
        same    <- s._2.commit(Some(1L), 2L, Batch.put(key("b"), int(2)))
        rewrite <- h.attempt(s._2.commit(Some(1L), 2L, Batch.put(key("b"), int(3))))
        subset  <- h.attempt(s._2.commit(Some(1L), 2L, Batch.empty))
        older   <- h.attempt(s._2.commit(Some(2L), 1L, Batch.put(key("a"), int(1))))
        again   <- h.attempt(s._2.commit(Some(2L), 2L, Batch.put(key("b"), int(2))))
        stale   <- h.attempt(s._2.commit(Some(1L), 3L, Batch.empty))
        head    <- s._2.head
      } yield (h2, same, rewrite, subset, older, again, stale, head)
    }) {
      case (h2, same, rewrite, subset, older, again, stale, head) =>
        expect
          .same(h2, same)
          .and(expect.same(Left(VersionedTrieError.CommitRewrite(vk(2L))), rewrite))
          .and(expect.same(Left(VersionedTrieError.CommitRewrite(vk(2L))), subset))
          .and(expect.same(Left(VersionedTrieError.VersionNotIncreasing(vk(2L), vk(1L))), older))
          .and(expect.same(Left(VersionedTrieError.VersionNotIncreasing(vk(2L), vk(2L))), again))
          .and(expect.same(Left(VersionedTrieError.VersionConflict(Some(vk(1L)), Some(vk(2L)))), stale))
          .and(expect.same(Some(h2), head))
    }
  }

  test("versions are strictly increasing with gaps allowed; a negative version is rejected") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        _     <- s._2.commit(None, 10L, Batch.empty)
        gap   <- s._2.commit(Some(10L), 1000L, Batch.empty)
        neg   <- h.attempt(s._2.commit(Some(1000L), -1L, Batch.empty))
        first <- h.attempt(fresh.flatMap(t => t._2.commit(Some(3L), 4L, Batch.empty)))
      } yield (gap, neg, first)
    }) {
      case (gap, neg, first) =>
        expect
          .same(1000L, gap.version)
          .and(expect(neg.left.exists(_.isInstanceOf[VersionedTrieError.InvalidVersion]), s"$neg"))
          .and(expect.same(Left(VersionedTrieError.VersionConflict(Some(vk(3L)), None)), first))
    }
  }

  test("each commit is one compare-and-write; a crash during it leaves every store and the index as they were") {
    forall(stepsGen) { steps =>
      ok(attempt { implicit h =>
        for {
          raw   <- RawStore.inMemory[IO].flatMap(WatchedRawStore(_))
          store <- open(raw)
          counted <- steps.init.zipWithIndex.traverse {
            case (step, i) =>
              (raw.writes, commitAll(store, List(step), Option.when(i > 0)(steps(i - 1)._1)), raw.writes).tupled.map {
                case (w0, _, w1) => (w1._1 - w0._1, w1._2 - w0._2)
              }
          }
          prev     <- store.head
          before   <- raw.contents
          _        <- raw.crashNextWrites(true)
          crashed  <- store.commit(prev.map(_.version), steps.last._1, steps.last._2).attempt
          _        <- raw.crashNextWrites(false)
          after    <- raw.contents
          reopened <- open(raw)
          head     <- reopened.head
        } yield (counted, before, after, crashed, prev, head)
      }) {
        case (counted, before, after, crashed, prev, head) =>
          expect
            .same(counted.map(_ => (0, 1)), counted)
            .and(expect(crashed.isLeft, "the write failed"))
            .and(expect(before == after, "no record changed"))
            .and(expect.same(prev, head))
      }
    }
  }

  test("restart: a store reopened over the same backend has the same head, values, history and proofs") {
    forall(stepsGen) { steps =>
      ok(attempt { implicit h =>
        for {
          s        <- fresh
          heads    <- commitAll(s._2, steps)
          reopened <- open(s._1)
          head     <- reopened.head
          values   <- reopened.scan().compile.toList
          past     <- steps.traverse(st => reopened.headAt(st._1))
          proof    <- reopened.proveHead
        } yield (heads, head, values, past, proof)
      }) {
        case (heads, head, values, past, proof) =>
          expect
            .same(heads.lastOption, head)
            .and(expect.same(model(steps).last._2.toList.sortBy(_._1), values))
            .and(expect.same(heads.map(Option(_)), past))
            .and(expect.same(Right(heads.last), VersionedTrieVerifier.verifyHead(get(VersionedTrie.root(heads.last)), proof)))
      }
    }
  }

  test("reading the value of a key at a past version, at a gap, and before the prune floor") {
    ok(attempt { implicit h =>
      for {
        s      <- fresh
        _      <- commitAll(s._2, List(1L -> Batch.put(key("a"), int(1)), 5L -> Batch.put(key("a"), int(5)), 9L -> Batch.delete(key("a"))))
        at     <- List(1L, 5L, 9L).traverse(v => s._2.get(key("a"), Some(v)))
        now    <- s._2.get(key("a"))
        gap    <- h.attempt(s._2.get(key("a"), Some(3L)))
        _      <- s._2.prune(5L)
        pruned <- h.attempt(s._2.get(key("a"), Some(1L)))
        kept   <- s._2.get(key("a"), Some(5L))
      } yield (at, now, gap, pruned, kept)
    }) {
      case (at, now, gap, pruned, kept) =>
        expect
          .same(List(Some(int(1)), Some(int(5)), None), at)
          .and(expect.same(None, now))
          .and(expect.same(Left(VersionedTrieError.UnknownVersion(vk(3L))), gap))
          .and(expect.same(Left(VersionedTrieError.NotRetained(vk(1L))), pruned))
          .and(expect.same(Some(int(5)), kept))
    }
  }

  test("rollback restores an earlier head in every store and the index, as one write, and commits continue") {
    forall(
      Gen.zip(stepsGen, batchGen).map { case (st, b) => st :+ (st.last._1 + 1 -> b) }.flatMap(st => Gen.choose(0, st.size - 2).map(st -> _))
    ) {
      case (steps, cut) =>
        val expected = model(steps)
        ok(attempt { implicit h =>
          for {
            raw   <- RawStore.inMemory[IO].flatMap(WatchedRawStore(_))
            store <- open(raw)
            _     <- commitAll(store, steps)
            w0    <- raw.writes
            _     <- store.rollback(steps(cut)._1)
            w1    <- raw.writes
            head  <- store.head
            later <- steps.drop(cut + 1).traverse(st => store.headAt(st._1))
            redo  <- commitAll(store, steps.slice(cut + 1, cut + 2), Some(steps(cut)._1))
            fresh <- RawStore.inMemory[IO].flatMap(open).flatMap(f => commitAll(f, steps.take(cut + 2)))
          } yield (w1._2 - w0._2, head, later, redo, fresh)
        }) {
          case (writes, head, later, redo, fresh) =>
            expect
              .same(1, writes)
              .and(expect.same(Some(expected(cut)._1), head))
              .and(expect(later.forall(_.isEmpty), "later versions are forgotten"))
              .and(expect.same(fresh.lastOption, redo.lastOption))
              .and(expect.same(Some(expected(cut + 1)._1), redo.lastOption))
        }
    }
  }

  test("retention: deltas at or below the prune floor are NotRetained; later deltas, the head and old version proofs remain") {
    ok(attempt { implicit h =>
      for {
        s       <- fresh
        heads   <- commitAll(s._2, (1L to 8L).toList.map(v => v -> Batch.put(key(s"k/$v"), int(v.toInt))))
        before  <- s._2.proveVersion(2L)
        _       <- s._2.prune(5L)
        evicted <- List(2L, 5L).traverse(v => h.attempt(s._2.delta(v)))
        kept    <- s._2.delta(6L)
        oldHead <- h.attempt(s._2.headAt(3L))
        version <- s._2.proveVersion(2L)
        state   <- h.attempt(s._2.proveState(StateQuery.keys(key("k/1")), Some(3L)))
      } yield (heads, before, evicted, kept, oldHead, version, state)
    }) {
      case (heads, before, evicted, kept, oldHead, version, state) =>
        val root = get(VersionedTrie.root(heads.last))
        expect
          .same(List(Left(VersionedTrieError.NotRetained(vk(2L))), Left(VersionedTrieError.NotRetained(vk(5L)))), evicted)
          .and(expect.same(Some(heads(4)), kept.parent).and(expect.same(heads(5), kept.head)))
          .and(expect.same(Left(VersionedTrieError.NotRetained(vk(3L))), oldHead))
          .and(expect.same(Right(Some(heads(1).stateRoot)), VersionedTrieVerifier.verifyVersion(root, version, 2L)))
          .and(expect.same(Right(Some(heads(1).stateRoot)), VersionedTrieVerifier.verifyVersion(root, before, 2L)))
          .and(expect.same(Left(VersionedTrieError.NotRetained(vk(3L))), state))
    }
  }

  test("opening a store whose stores are out of step is refused as corruption") {
    attempt { implicit h =>
      for {
        s <- fresh
        _ <- s._2.commit(None, 1L, Batch.put(key("a"), int(1)))
        _ <- get(Namespace("c/vidx/s")).pipe(n =>
          s._1.write(xyz.kd5ujc.storage.RawBatch.delete(n.prefix ++ scodec.bits.ByteVector.fromLong(1L)))
        )
        _ <- open(s._1)
      } yield ()
    }.map {
      case Left(VersionedTrieError.Store(_: xyz.kd5ujc.storage.StoreError.Corrupt)) => success
      case other                                                                    => failure(s"expected corruption, got $other")
    }
  }

  test("the state proof at the head verifies with the trie verifier directly against the head's state root") {
    ok(attempt { implicit h =>
      for {
        s     <- fresh
        head  <- s._2.commit(None, 0L, Batch.put(key("a/b"), int(1)))
        proof <- s._2.proveState(StateQuery.keys(key("a/b")))
      } yield (head, proof)
    }) {
      case (head, proof) =>
        expect(proof.state.exists(p => TrieProofs.verify(get(VersionedTrie.stateMode), head.stateRoot, p).isRight))
    }
  }

  implicit private class Pipe[A](a: A) {
    def pipe[B](f: A => B): B = f(a)
  }
}
