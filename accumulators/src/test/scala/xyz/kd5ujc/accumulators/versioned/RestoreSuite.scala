package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.storage.{Batch, ByteRange, Generations, RawStore, WatchedRawStore}

import fs2.Stream
import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/**
 * Restore and the snapshot wire format (docs/specs/versioned-trie.md §4, §7). The reference for a restored store is a
 * store that committed the same versions and pruned through the head: the two must hold the same bytes, so that every
 * read, proof and later write behaves the same on both.
 */
object RestoreSuite extends SimpleIOSuite with Checkers {

  private type Store = VersionedTrieStore[IO, Long, H]

  private def ok[A](io: IO[Either[VersionedTrieError, A]])(f: A => Expectations): IO[Expectations] =
    io.map(_.fold(e => failure(s"unexpected $e"), f))

  private def contents(raw: RawStore[IO]): IO[List[(ByteVector, ByteVector)]] = raw.scan(ByteRange.all).compile.toList

  /** Store A commits `steps` and prunes through its head; store B restores A's snapshot. */
  private def pair(steps: List[(Long, Batch[StateKey, CanonicalValue])], chunkEntries: Int)(
    implicit h: Handle[IO, VersionedTrieError]
  ): IO[((RawStore[IO], Store), (RawStore[IO], Store), Head[Long, H])] =
    for {
      a      <- fresh
      heads  <- commitAll(a._2, steps)
      chunks <- a._2.snapshot(chunkEntries).compile.toList
      _      <- a._2.prune(steps.last._1)
      b      <- fresh
      _      <- b._2.restoreFrom(heads.last, Stream.emits(chunks))
    } yield (a, b, heads.last)

  /** Every read and proof a caller can ask of a store, each attempted, over every version and the gaps between. */
  private def observe(s: Store, versions: List[Long], keys: List[StateKey])(
    implicit h: Handle[IO, VersionedTrieError]
  ): IO[List[String]] = {
    val probes = (0L to versions.max + 2L).toList
    val whole = List(
      s.head.map(_.toString),
      s.scan().compile.toList.map(_.toString),
      s.proveHead.map(_.toString),
      s.proveState(StateQuery(keys, List(ns("a"), ns("b")), Nil)).map(_.toString)
    ).sequence
    val perVersion = probes.flatTraverse { u =>
      List(
        h.attempt(s.headAt(u)).map(_.toString),
        h.attempt(s.proveVersion(u)).map(_.toString),
        h.attempt(s.proveAsOf(u)).map(_.toString),
        h.attempt(s.proveState(StateQuery.keys(keys*), Some(u))).map(_.toString),
        h.attempt(s.delta(u)).map(_.toString),
        keys.traverse(k => h.attempt(s.get(k, Some(u)))).map(_.toString)
      ).sequence
    }
    (whole, perVersion).mapN(_ ++ _)
  }

  private val laterGen: Gen[List[Batch[StateKey, CanonicalValue]]] = Gen.choose(0, 4).flatMap(Gen.listOfN(_, batchGen))

  test("PROPERTY: a store restored from a snapshot holds the same bytes as one that committed and pruned to the same head, and stays so") {
    forall(Gen.zip(stepsGen, laterGen, Gen.choose(1, 4))) {
      case (steps, later, chunkEntries) =>
        val more = later.zipWithIndex.map { case (b, i) => (steps.last._1 + 1L + 3L * i) -> b }
        val keys = (steps ++ more).flatMap(_._2.last.map(_._1)).distinct.take(6)
        ok(attempt { implicit h =>
          for {
            p      <- pair(steps, chunkEntries)
            atBase <- (contents(p._1._1), contents(p._2._1)).tupled
            seen   <- (observe(p._1._2, steps.map(_._1), keys), observe(p._2._2, steps.map(_._1), keys)).tupled
            below  <- steps.init.lastOption.traverse(st => (h.attempt(p._1._2.rollback(st._1)), h.attempt(p._2._2.rollback(st._1))).tupled)
            headsA <- commitAll(p._1._2, more, Some(steps.last._1))
            headsB <- commitAll(p._2._2, more, Some(steps.last._1))
            afterAll <- (contents(p._1._1), contents(p._2._1)).tupled
          } yield (atBase, seen, below, headsA, headsB, afterAll, p._3)
        }) {
          case (atBase, seen, below, headsA, headsB, afterAll, base) =>
            expect(
              Generations.masked(atBase._1) == Generations.masked(atBase._2),
              "identical bytes at the restored head, but for generations"
            )
              .and(expect.same(Generations.of(atBase._1).keySet, Generations.of(atBase._2).keySet))
              .and(expect.same(seen._1, seen._2))
              .and(expect(below.forall(b => b._1 == b._2 && b._2.left.exists(_.isInstanceOf[VersionedTrieError.NotRetained])), s"$below"))
              .and(expect.same(headsA, headsB))
              .and(
                expect(
                  Generations.masked(afterAll._1) == Generations.masked(afterAll._2),
                  "identical bytes after further commits, but for generations"
                )
              )
              .and(expect.same(model(steps ++ more).map(_._1).drop(steps.size), headsB))
              .and(expect.same(model(steps).last._1, base))
        }
    }
  }

  test("restore is one compare-and-write; a crash leaves the store empty; a non-empty store is refused") {
    val steps = List(1L -> Batch.put(key("a"), int(1)), 4L -> Batch.put(key("b/c"), int(2)))
    ok(attempt { implicit h =>
      for {
        a        <- fresh
        heads    <- commitAll(a._2, steps)
        snap     <- a._2.snapshot().compile.toList.map(cs => get(assemble(cs)))
        raw      <- RawStore.inMemory[IO].flatMap(WatchedRawStore(_))
        b        <- open(raw)
        _        <- raw.crashNextWrites(true)
        crashed  <- b.restore(heads.last, snap).attempt
        _        <- raw.crashNextWrites(false)
        empty    <- raw.contents
        w0       <- raw.writes
        restored <- b.restore(heads.last, snap)
        w1       <- raw.writes
        again    <- h.attempt(b.restore(heads.last, snap))
        busy     <- h.attempt(a._2.restore(heads.last, snap))
      } yield (crashed, empty, w1._2 - w0._2, w1._1 - w0._1, restored, again, busy, heads.last)
    }) {
      case (crashed, empty, writeIfs, writes, restored, again, busy, head) =>
        expect(crashed.isLeft, "the write failed")
          .and(expect(empty.isEmpty, "nothing landed"))
          .and(expect.same((1, 0), (writeIfs, writes)))
          .and(expect.same(head, restored))
          .and(expect.same(Left(VersionedTrieError.VersionConflict(None, Some(vk(4L)))), again))
          .and(expect.same(Left(VersionedTrieError.VersionConflict(None, Some(vk(4L)))), busy))
    }
  }

  test("a snapshot is checked against the trusted head: another head, tampered contents or a truncated history restore nothing") {
    val steps = List(1L -> Batch.put(key("a"), int(1)), 2L -> Batch.put(key("b"), int(2)), 3L -> Batch.delete(key("a")))
    ok(attempt { implicit h =>
      for {
        a      <- fresh
        heads  <- commitAll(a._2, steps)
        chunks <- a._2.snapshot(1).compile.toList
        snap = get(assemble(chunks))
        tries <- List(
          (heads(1), snap),
          (heads.last, snap.copy(entries = snap.entries.map { case (k, _) => k -> int(7) })),
          (heads.last, snap.copy(history = snap.history.init))
        ).traverse { case (trusted, s) => fresh.flatMap(b => h.attempt(b._2.restore(trusted, s)).flatMap(r => contents(b._1).map(r -> _))) }
      } yield tries
    }) { tries =>
      tries.foldMap { case (r, c) => expect(r.isLeft, s"$r").and(expect(c.isEmpty, "nothing written")) }
    }
  }

  test("the reader refuses a bad chunk when it arrives: no later chunk is read") {
    val steps = List(
      1L -> (Batch.put(key("a"), int(1)) ++ Batch.put(key("b"), int(2)) ++ Batch.put(key("c"), int(3))),
      2L -> Batch.empty[StateKey, CanonicalValue]
    )
    ok(attempt { implicit h =>
      for {
        a      <- fresh
        heads  <- commitAll(a._2, steps)
        chunks <- a._2.snapshot(1).compile.toList
      } yield (heads, chunks)
    }) {
      case (heads, chunks) =>
        val head = heads.last
        val top = chunks.head
        val states = chunks.collect { case s: SnapshotChunk.State[Long, H] => s: SnapshotChunk[Long, H] }
        val hist = chunks.collect { case s: SnapshotChunk.History[Long, H] => s: SnapshotChunk[Long, H] }
        def readUpTo(cs: List[SnapshotChunk[Long, H]]) = cs.foldM(SnapshotReader.start(head))(_.feed(_))
        val bad: List[(String, List[SnapshotChunk[Long, H]])] = List(
          "another head first"  -> List(SnapshotChunk.Top(heads.head)),
          "no head first"       -> states.take(1),
          "keys out of order"   -> (top :: states.reverse),
          "a repeated key"      -> (top :: states.head :: states.head :: Nil),
          "state after history" -> (top :: hist.head :: states),
          "a null value"        -> List(top, SnapshotChunk.State(Vector(key("a") -> CanonicalValue.CNull))),
          "an empty chunk"      -> List(top, SnapshotChunk.State(Vector.empty)),
          "too many entries" -> List(
            top,
            SnapshotChunk.State(Vector.tabulate(VersionedTrie.MaxChunkEntries + 1)(i => key(s"k$i") -> int(i)))
          ),
          "a version after the head" -> (top :: states ++ List(SnapshotChunk.History(Vector(3L -> head.stateRoot)))),
          "the head with another state root" -> (top :: states ++ List(
            SnapshotChunk.History(Vector(1L -> heads.head.stateRoot, 2L -> T.value(ByteVector(1))))
          )),
          "a chunk after the head version" -> (chunks :+ states.head)
        )
        bad.foldMap { case (what, cs) => expect(readUpTo(cs).isLeft, what) }
          .and(expect(readUpTo(chunks).flatMap(_.finish).isRight, "the honest snapshot"))
          .and(expect(readUpTo(chunks.init).flatMap(_.finish).isLeft, "a snapshot cut before the head version"))
    }
  }

  test("a restore from a stream stops at the first bad chunk without reading on") {
    ok(attempt { implicit h =>
      for {
        a      <- fresh
        heads  <- commitAll(a._2, List(1L -> (Batch.put(key("a"), int(1)) ++ Batch.put(key("b"), int(2)))))
        chunks <- a._2.snapshot(1).compile.toList
        b      <- fresh
        // the second state chunk repeats the first; anything after it would raise a different error
        r <- h.attempt(
          b._2.restoreFrom(
            heads.last,
            Stream.emits(chunks.take(2) :+ chunks(1)) ++ Stream.raiseError[IO](new IllegalStateException("read on"))
          )
        )
      } yield r
    })(r => expect(r.left.exists(_.isInstanceOf[VersionedTrieError.Malformed]), s"$r"))
  }

  test("snapshot chunks round-trip through the generated codec; an oversized chunk is refused before decoding") {
    ok(attempt { implicit h =>
      for {
        a <- fresh
        _ <- commitAll(a._2, List(1L -> (Batch.put(key("a"), text("x")) ++ Batch.put(key("b/c"), CanonicalValue.bytes(ByteVector(1, 2))))))
        chunks <- a._2.snapshot(1).compile.toList
      } yield chunks
    }) { chunks =>
      val again = chunks.traverse(c => VersionedTrieWire.encodeChunk(c).flatMap(VersionedTrieWire.decodeChunk[Long, H]))
      val huge = ByteVector.fill(VersionedTrie.MaxChunkBytes + 1L)(0)
      expect
        .same(Right(chunks), again)
        .and(
          expect.same(
            Left(VersionedTrieError.TooLarge("a snapshot chunk", huge.size, VersionedTrie.MaxChunkBytes)),
            VersionedTrieWire.decodeChunk[Long, H](huge)
          )
        )
    }
  }

  test("a chunk must be canonical CBOR: reordered map keys or a widened length header are refused") {
    ok(attempt { implicit h =>
      fresh.flatMap(s => commitAll(s._2, List(1L -> Batch.put(key("a"), int(1)))) >> s._2.snapshot().compile.toList)
    }) { chunks =>
      val historyChunk = get(chunks.collectFirst { case c: SnapshotChunk.History[Long, H] => c }.toRight("no history chunk"))
      val bytes = get(VersionedTrieWire.encodeChunk(historyChunk))
      // {"history": [{"s": h'..32..', "v": h'..8..'}]}: keys in canonical order are "s" then "v"
      val entry = bytes.drop(bytes.size - (1L + 2L + 2L + 32L + 2L + 1L + 8L))
      val prefix = bytes.take(bytes.size - entry.size)
      val s = entry.slice(1L, 1L + 2L + 2L + 32L)
      val v = entry.drop(1L + 2L + 2L + 32L)
      val reordered = prefix ++ entry.take(1L) ++ v ++ s
      // the 8-byte version key's header 0x48 widened to 0x58 0x08: the same value, not minimally encoded
      val widened = prefix ++ entry.take(1L) ++ s ++ v.take(2L) ++ ByteVector(0x58, 0x08) ++ v.drop(3L)
      expect
        .same(Right(historyChunk), VersionedTrieWire.decodeChunk[Long, H](bytes))
        .and(expect(VersionedTrieWire.decodeChunk[Long, H](reordered).isLeft, s"reordered keys: ${reordered.toHex}"))
        .and(expect(VersionedTrieWire.decodeChunk[Long, H](widened).isLeft, s"a widened header: ${widened.toHex}"))
        .and(expect.same(bytes.size, reordered.size))
    }
  }

  pureTest("Snapshot.chunks groups by bytes as well as count: large valid values never make a chunk the wire refuses") {
    val big = CanonicalValue.bytes(ByteVector.fill(900L * 1024L)(7))
    val entries = (1 to 5).toList.map(i => key(s"k$i") -> big)
    val root = xyz.kd5ujc.core.hash.Hasher[H].digest(ByteVector.empty)
    val head = Head[Long, H](1L, root, root)
    val states = Snapshot.chunks(Snapshot(head, entries, Nil)).collect { case c: SnapshotChunk.State[Long, H] => c }
    expect(states.size > 1, s"five 900 KiB values need more than one chunk, got ${states.size}")
      .and(expect.same(entries, states.flatMap(_.entries)))
      .and(expect(states.forall(c => VersionedTrieWire.encodeChunk[Long, H](c).isRight), "every chunk encodes within the wire limit"))
  }

  private def nested(depth: Int): CanonicalValue =
    (1 to depth).foldLeft(int(0))((v, _) => get(CanonicalValue.array(Vector(v))))

  test("the deepest accepted value round-trips through snapshot chunks and deltas; one level deeper is refused at commit") {
    val deepest = nested(VersionedTrie.MaxValueDepth)
    ok(attempt { implicit h =>
      for {
        a       <- fresh
        head    <- a._2.commit(None, 1L, Batch.put(key("deep"), deepest))
        chunks  <- a._2.snapshot(1).compile.toList
        delta   <- a._2.delta(1L)
        tooDeep <- h.attempt(a._2.commit(Some(1L), 2L, Batch.put(key("deeper"), nested(VersionedTrie.MaxValueDepth + 1))))
        preview <- h.attempt(a._2.preview(2L, Batch.put(key("deeper"), nested(VersionedTrie.MaxValueDepth + 1))))
        b       <- fresh
        copied <- chunks.traverse(c =>
          IO.fromEither(
            VersionedTrieWire
              .encodeChunk(c)
              .flatMap(VersionedTrieWire.decodeChunk[Long, H])
              .leftMap(e => new IllegalStateException(e.toString))
          )
        )
        _     <- b._2.restoreFrom(head, Stream.emits(copied))
        value <- b._2.get(key("deep"))
      } yield (copied == chunks, delta, tooDeep, preview, value)
    }) {
      case (sameChunks, delta, tooDeep, preview, value) =>
        val refused = Left(VersionedTrieError.TooDeep("the value of deeper", VersionedTrie.MaxValueDepth + 1, VersionedTrie.MaxValueDepth))
        val overWrapped =
          VersionedTrieWire.encodeChunk[Long, H](SnapshotChunk.State(Vector(key("x") -> nested(VersionedTrie.MaxValueDepth + 1))))
        expect
          .same(253, VersionedTrie.MaxValueDepth)
          .and(expect(overWrapped.isLeft, "one level deeper no longer fits a chunk, so 253 is exactly the limit"))
          .and(expect(sameChunks, "every chunk encodes and decodes"))
          .and(
            expect.same(
              Right(delta),
              VersionedTrieWire
                .encodeDelta(delta)
                .map(xyz.kd5ujc.core.canonical.Cbor.encode)
                .flatMap(b => xyz.kd5ujc.core.canonical.Cbor.decode(b).leftMap(e => VersionedTrieError.Malformed(e.toString)))
                .flatMap(VersionedTrieWire.decodeDelta[Long, H])
            )
          )
          .and(expect.same(Some(deepest), value))
          .and(expect.same(refused, tooDeep))
          .and(expect.same(refused, preview))
    }
  }

  test("values over 1 MiB are refused at commit; large values are chunked within the byte limit") {
    val big = (i: Int) => CanonicalValue.bytes(ByteVector.fill(VersionedTrie.MaxValueBytes - 16L)(i.toByte))
    ok(attempt { implicit h =>
      for {
        a    <- fresh
        over <- h.attempt(a._2.commit(None, 1L, Batch.put(key("x"), CanonicalValue.bytes(ByteVector.fill(VersionedTrie.MaxValueBytes)(0)))))
        _    <- a._2.commit(None, 1L, Batch.puts((0 until 9).map(i => key(s"v$i") -> big(i))))
        chunks <- a._2.snapshot().compile.toList
      } yield (over, chunks)
    }) {
      case (over, chunks) =>
        val sizes = chunks.map(c => VersionedTrieWire.encodeChunk(c).map(_.size))
        expect(over.left.exists(_.isInstanceOf[VersionedTrieError.TooLarge]), s"$over")
          .and(expect(sizes.forall(_.exists(_ <= VersionedTrie.MaxChunkBytes)), s"$sizes"))
          .and(expect(chunks.count(_.isInstanceOf[SnapshotChunk.State[?, ?]]) >= 3, "nine 1 MiB values need at least three 4 MiB chunks"))
    }
  }
}
