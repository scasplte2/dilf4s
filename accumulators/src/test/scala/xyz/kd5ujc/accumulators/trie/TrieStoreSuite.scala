package xyz.kd5ujc.accumulators.trie

import cats.effect.IO
import cats.mtl.{Handle, Raise}
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.storage.{ByteRange, RawBatch, RawStore, StoreError, Version, WatchedRawStore}

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object TrieStoreSuite extends SimpleIOSuite with Checkers {

  private val T = TrieHashing[Sha256]
  private val mode = KeyMode.variable(3).toOption.get

  sealed private trait Failure
  final private case class Store(e: StoreError) extends Failure
  final private case class Tree(e: TrieError) extends Failure

  private def run[A](f: (Raise[IO, StoreError], Raise[IO, TrieError]) => IO[A]): IO[Either[Failure, A]] =
    Handle
      .allowF[IO, StoreError](rs =>
        Handle.allowF[IO, TrieError](rt => f(rs, rt).map(_.asRight[Failure])).rescue(e => IO.pure(Tree(e).asLeft[A]))
      )
      .rescue(e => IO.pure(Store(e).asLeft[A]))

  private def ok(f: (Raise[IO, StoreError], Raise[IO, TrieError]) => IO[Expectations]): IO[Expectations] =
    run(f).map(_.fold(e => failure(s"unexpected $e"), identity))

  private val byte = Gen.oneOf(0x00.toByte, 0x01.toByte, 0x80.toByte, 0xff.toByte)
  private val key = Gen.choose(0, 3).flatMap(Gen.listOfN(_, byte)).map(ByteVector(_))
  private def value(i: Int): Digest[Sha256] = T.value(ByteVector.fromInt(i))
  private type Ops = List[(ByteVector, Option[Digest[Sha256]])]
  private val batch: Gen[Ops] = Gen.listOf(Gen.zip(key, Gen.option(Gen.choose(0, 5).map(value)))).map(_.take(8))
  private val history: Gen[List[Ops]] = Gen.choose(1, 8).flatMap(Gen.listOfN(_, batch))
  implicit private val showHistory: cats.Show[List[Ops]] = cats.Show.fromToString
  implicit private val showHistoryInt: cats.Show[(List[Ops], Int)] = cats.Show.fromToString

  /** Reference: the pure trie after each version (index = version). */
  private def model(h: List[Ops]): Vector[Trie[Sha256]] =
    h.scanLeft(Trie.empty[Sha256](mode))((t, ops) => t.update(ops).fold(e => throw new AssertionError(e.toString), identity)).toVector

  private def commitAll(store: TrieStore[IO, Sha256], h: List[Ops]): IO[Version] =
    h.foldLeftM(Version.Genesis)((v, ops) => store.commit(v, ops))

  /** Distinct nodes reachable from the given roots in the reference tries. */
  private def reachable(tries: Seq[Trie[Sha256]]): Int = tries.flatMap(_.nodesForTest.keySet).distinct.size

  private def storedNodes(raw: RawStore[IO]): IO[Long] =
    xyz.kd5ujc.storage.Namespace("trie/node").fold(_ => IO.pure(-1L), ns => raw.scan(ByteRange.prefix(ns.prefix)).compile.count)

  test("every retained version reads, roots and proves like the reference trie") {
    forall(history) { h =>
      val snaps = model(h)
      ok { (rs, rt) =>
        given Raise[IO, StoreError] = rs
        given Raise[IO, TrieError] = rt
        for {
          raw   <- RawStore.inMemory[IO]
          store <- TrieStore.make[IO, Sha256](raw, "trie", mode)
          _     <- commitAll(store, h)
          checks <- snaps.indices.toList.traverse { v =>
            val version = Version.unsafe(v.toLong)
            val keys = snaps(v).entries.toOption.get.map(_._1) ++ h.flatten.map(_._1).take(4)
            (store.root(version), keys.traverse(k => store.get(k, version)), store.prove(keys, Nil, version)).mapN { (root, got, proof) =>
              val verified = TrieProofs.verify(mode, root, proof)
              expect
                .same(snaps(v).root, root)
                .and(expect.same(keys.map(k => snaps(v).get(k).toOption.get), got))
                .and(expect(verified.isRight, s"proof at v$v verifies"))
            }
          }
        } yield checks.reduce(_ and _)
      }
    }
  }

  test("node storage holds exactly what the retained versions reach, through prune and rollback") {
    forall(history.flatMap(h => Gen.choose(0, h.size).map(h -> _))) {
      case (h, cut) =>
        val snaps = model(h)
        ok { (rs, rt) =>
          given Raise[IO, StoreError] = rs
          given Raise[IO, TrieError] = rt
          for {
            raw     <- RawStore.inMemory[IO]
            store   <- TrieStore.make[IO, Sha256](raw, "trie", mode)
            _       <- commitAll(store, h)
            full    <- storedNodes(raw)
            _       <- store.prune(Version.unsafe(cut.toLong))
            pruned  <- storedNodes(raw)
            floor   <- store.floor
            _       <- store.rollback(Version.unsafe(cut.toLong))
            rolled  <- storedNodes(raw)
            latest  <- store.latest
            rootNow <- store.root(latest)
          } yield
            expect
              .same(reachable(snaps).toLong, full)
              .and(expect.same(reachable(snaps.drop(cut)).toLong, pruned))
              .and(expect.same(cut.toLong, floor.value))
              .and(expect.same(reachable(Seq(snaps(cut))).toLong, rolled))
              .and(expect.same(snaps(cut).root, rootNow))
        }
    }
  }

  test("staged commits, rollbacks and prunes write nothing until written, then land as one compare-and-write") {
    forall(history.flatMap(h => Gen.choose(0, h.size).map(h -> _))) {
      case (h, cut) =>
        val snaps = model(h)
        ok { (rs, rt) =>
          given Raise[IO, StoreError] = rs
          given Raise[IO, TrieError] = rt
          def step(raw: WatchedRawStore, stage: IO[TrieStore.Staged[Sha256]], write: TrieStore.Staged[Sha256] => IO[Version]) =
            for {
              before  <- raw.contents
              atStart <- raw.writes
              s       <- stage
              after   <- raw.contents
              staged  <- raw.writes
              _       <- write(s)
              written <- raw.writes
            } yield (before == after && staged == atStart, (written._1 - staged._1, written._2 - staged._2), s.root)
          for {
            base    <- RawStore.inMemory[IO]
            raw     <- WatchedRawStore(base)
            store   <- TrieStore.make[IO, Sha256](raw, "trie", mode)
            commits <- h.zipWithIndex.traverse { case (ops, v) => step(raw, store.stage(Version.unsafe(v.toLong), ops), store.write) }
            prune   <- step(raw, store.stagePrune(Version.unsafe(cut.toLong)), store.write)
            back    <- step(raw, store.stageRollback(Version.unsafe(cut.toLong)), store.write)
          } yield {
            val steps = commits :+ prune :+ back
            expect(steps.forall(_._1), "staging changed no record and reached no write")
              .and(expect.same(steps.map(_ => (0, 1)), steps.map(_._2)))
              .and(expect.same(snaps.tail.map(_.root).toList :+ snaps.last.root :+ snaps(cut).root, steps.map(_._3)))
          }
        }
    }
  }

  test("a crash during a staged write leaves node storage, roots and state as they were") {
    forall(history) { h =>
      ok { (rs, rt) =>
        given Raise[IO, StoreError] = rs
        given Raise[IO, TrieError] = rt
        for {
          base    <- RawStore.inMemory[IO]
          raw     <- WatchedRawStore(base)
          store   <- TrieStore.make[IO, Sha256](raw, "trie", mode)
          v       <- commitAll(store, h.init)
          before  <- raw.contents
          _       <- raw.crashNextWrites(true)
          crashed <- store.commit(v, h.last).attempt
          _       <- raw.crashNextWrites(false)
          after   <- raw.contents
          latest  <- store.latest
        } yield expect(crashed.isLeft, "the write failed").and(expect(before == after, "no record changed")).and(expect.same(v, latest))
      }
    }
  }

  test("chained drafts read nodes through the drafts below them and write what sequential commits write") {
    forall(history) { h =>
      val snaps = model(h)
      ok { (rs, rt) =>
        given Raise[IO, StoreError] = rs
        given Raise[IO, TrieError] = rt
        for {
          seqRaw   <- RawStore.inMemory[IO]
          seq      <- TrieStore.make[IO, Sha256](seqRaw, "trie", mode)
          _        <- commitAll(seq, h)
          expected <- seqRaw.scan(ByteRange.all).compile.toList
          base     <- RawStore.inMemory[IO]
          raw      <- WatchedRawStore(base)
          store    <- TrieStore.make[IO, Sha256](raw, "trie", mode)
          first    <- store.stage(Version.Genesis, h.head)
          drafts   <- h.tail.foldLeftM(List(first))((acc, ops) => store.stageOn(acc.last, ops).map(acc :+ _))
          empty    <- raw.contents
          v        <- store.write(drafts.last)
          all      <- raw.contents
          writes   <- raw.writes
        } yield
          expect(empty.isEmpty, "drafts write nothing")
            .and(expect.same(snaps.tail.map(_.root).toList, drafts.map(_.root)))
            .and(expect.same(h.size.toLong, v.value))
            .and(expect(expected == all, "the same records as committing one by one"))
            .and(expect.same((0, 1), writes))
      }
    }
  }

  test("reading a pruned or future version is a typed error") {
    run { (rs, rt) =>
      given Raise[IO, StoreError] = rs
      given Raise[IO, TrieError] = rt
      for {
        raw    <- RawStore.inMemory[IO]
        store  <- TrieStore.make[IO, Sha256](raw, "trie", mode)
        _      <- commitAll(store, List(List(ByteVector(1) -> Some(value(1))), List(ByteVector(2) -> Some(value(2)))))
        _      <- store.prune(Version.unsafe(2L))
        result <- store.root(Version.unsafe(1L))
      } yield result
    }.map(r => expect.same(Left(Store(StoreError.Pruned(1L, 2L))), r))
  }

  test("concurrent commits through separate instances: one wins per version") {
    ok { (rs, rt) =>
      given Raise[IO, StoreError] = rs
      given Raise[IO, TrieError] = rt
      for {
        raw <- RawStore.inMemory[IO]
        results <- (0 until 6).toList.parTraverse { i =>
          run((rs2, rt2) =>
            TrieStore
              .make[IO, Sha256](raw, "trie", mode)(using implicitly, implicitly, rs2, rt2)
              .flatMap(_.commit(Version.Genesis, List(ByteVector(i.toByte) -> Some(value(i)))))
          )
        }
        reader <- TrieStore.make[IO, Sha256](raw, "trie", mode)
        latest <- reader.latest
      } yield expect.same(1, results.count(_.isRight)).and(expect.same(1L, latest.value))
    }
  }

  test("tampered node storage is detected on read") {
    run { (rs, rt) =>
      given Raise[IO, StoreError] = rs
      given Raise[IO, TrieError] = rt
      for {
        raw   <- RawStore.inMemory[IO]
        store <- TrieStore.make[IO, Sha256](raw, "trie", mode)
        _     <- store.commit(Version.Genesis, List(ByteVector(1) -> Some(value(1)), ByteVector(2) -> Some(value(2))))
        nodes <- xyz.kd5ujc.storage
          .Namespace("trie/node")
          .fold(_ => IO.pure(List.empty[(ByteVector, ByteVector)]), ns => raw.scan(ByteRange.prefix(ns.prefix)).compile.toList)
        _ <- nodes.headOption.traverse_ { case (k, v) => raw.write(RawBatch.put(k, v.update(v.size - 1, (v(v.size - 1) ^ 0x01).toByte))) }
        _ <- store.get(ByteVector(1), Version.unsafe(1L))
        _ <- store.get(ByteVector(2), Version.unsafe(1L))
      } yield ()
    }.map {
      case Left(Store(StoreError.Corrupt(_))) => success
      case other                              => failure(s"expected corruption to be detected, got $other")
    }
  }
}
