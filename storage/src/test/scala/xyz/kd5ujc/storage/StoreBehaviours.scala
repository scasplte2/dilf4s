package xyz.kd5ujc.storage

import cats.effect.IO
import cats.mtl.{Handle, Raise}
import cats.syntax.all._

import io.circe.{Decoder, Encoder}
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/** Backend-independent behaviour of every store layer; each interpreter's suite mixes this in. */
trait StoreBehaviours { self: SimpleIOSuite & Checkers =>

  def backend: String

  /** A fresh, empty raw store for one test. */
  def withRaw[A](use: RawStore[IO] => IO[A])(implicit R: Raise[IO, StoreError]): IO[A]

  /**
   * Runs a program with a `Handle` for store errors, installed once at the edge. Inside, recover locally through that
   * same instance (`h.attempt(fa)`), so algebras built with it report into it.
   */
  def attempt[A](f: Handle[IO, StoreError] => IO[A]): IO[Either[StoreError, A]] =
    Handle.allowF[IO, StoreError](h => f(h).map(_.asRight[StoreError])).rescue(e => IO.pure(e.asLeft[A]))

  def expectOk(f: Handle[IO, StoreError] => IO[Expectations]): IO[Expectations] =
    attempt(f).map(_.fold(e => failure(s"unexpected store error $e"), identity))

  implicit private val intValue: ValueCodec[Int] = ValueCodec.fromCirce[Int](using Encoder[Int], Decoder[Int])

  private def ns(name: String): Namespace = Namespace(name).fold(e => sys.error(e.toString), identity)

  // ------------------------------------------------------------------ raw

  test(s"$backend: batches apply in order, last write wins") {
    expectOk { implicit r =>
      withRaw { raw =>
        val batch = RawBatch.put(hex"01", hex"aa") ++ RawBatch.put(hex"02", hex"bb") ++ RawBatch.delete(hex"01") ++
          RawBatch.put(hex"02", hex"cc")
        raw.write(batch) >> (raw.get(hex"01"), raw.get(hex"02"), raw.getAll(List(hex"02", hex"03"))).mapN { (a, b, all) =>
          expect.same(None, a).and(expect.same(Some(hex"cc"), b)).and(expect.same(List(Some(hex"cc"), None), all))
        }
      }
    }
  }

  test(s"$backend: scans are ordered by unsigned bytes and honour range bounds") {
    expectOk { implicit r =>
      withRaw { raw =>
        val keys = List(hex"7f", hex"80", hex"ff", hex"00", hex"0100", hex"01", hex"ffff")
        raw.write(keys.foldMap(k => RawBatch.put(k, k))) >>
        (
          raw.scan(ByteRange.all).map(_._1).compile.toList,
          raw.scan(ByteRange.prefix(hex"01")).map(_._1).compile.toList,
          raw.scan(ByteRange.prefix(hex"ff")).map(_._1).compile.toList,
          raw.scan(ByteRange(Some(hex"01"), Some(hex"80"))).map(_._1).compile.toList
        ).mapN { (all, p01, pff, mid) =>
          expect
            .same(List(hex"00", hex"01", hex"0100", hex"7f", hex"80", hex"ff", hex"ffff"), all)
            .and(expect.same(List(hex"01", hex"0100"), p01))
            .and(expect.same(List(hex"ff", hex"ffff"), pff))
            .and(expect.same(List(hex"01", hex"0100", hex"7f"), mid))
        }
      }
    }
  }

  test(s"$backend: a scan sees one snapshot even if writes happen while it runs") {
    expectOk { implicit r =>
      withRaw { raw =>
        raw.write((1 to 600).toList.foldMap(i => RawBatch.put(ByteVector.fromInt(i), ByteVector.fromInt(i)))) >>
        raw
          .scan(ByteRange.all)
          .zipWithIndex
          .evalTap { case (_, i) => raw.write(RawBatch.put(ByteVector.fromInt(10000 + i.toInt), hex"00")).whenA(i == 0L) }
          .compile
          .count
          .map(n => expect.same(600L, n))
      }
    }
  }

  // ------------------------------------------------------------------ typed

  test(s"$backend: typed put, get, contains, getAll, remove") {
    expectOk { implicit r =>
      withRaw { raw =>
        val kv = KeyValueStore[IO, String, Int](raw, ns("kv"))
        kv.put("a", 1) >> kv.put("b", 2) >> kv.remove("a") >>
        (kv.get("a"), kv.get("b"), kv.contains("b"), kv.getAll(List("a", "b", "c"))).mapN { (a, b, cb, all) =>
          expect
            .same(None, a)
            .and(expect.same(Some(2), b))
            .and(expect(cb))
            .and(expect.same(List("a" -> None, "b" -> Some(2), "c" -> None), all))
        }
      }
    }
  }

  test(s"$backend: getOrRaise raises a typed NotFound") {
    attempt { implicit r =>
      withRaw(raw => KeyValueStore[IO, String, Int](raw, ns("kv")).getOrRaise("missing"))
    }.map {
      case Left(StoreError.NotFound(_)) => success
      case other                        => failure(s"expected NotFound, got $other")
    }
  }

  test(s"$backend: scans follow the key type's order, including negative numbers") {
    expectOk { implicit r =>
      withRaw { raw =>
        val kv = KeyValueStore[IO, Long, Int](raw, ns("longs"))
        val keys = List(5L, -1L, Long.MinValue, 0L, Long.MaxValue, -100L, 42L)
        kv.commit(Batch.puts(keys.map(k => k -> 0))) >>
        (kv.scan().map(_._1).compile.toList, kv.scan(KeyRange.between(-100L, 42L)).map(_._1).compile.toList).mapN { (all, mid) =>
          expect.same(keys.sorted, all).and(expect.same(List(-100L, -1L, 0L, 5L), mid))
        }
      }
    }
  }

  test(s"$backend: namespaces are isolated") {
    expectOk { implicit r =>
      withRaw { raw =>
        val a = KeyValueStore[IO, String, Int](raw, ns("a"))
        val ab = KeyValueStore[IO, String, Int](raw, ns("ab"))
        a.put("k", 1) >> ab.put("k", 2) >>
        (a.get("k"), ab.get("k"), a.scan().compile.toList).mapN { (x, y, scanned) =>
          expect.same(Some(1), x).and(expect.same(Some(2), y)).and(expect.same(List("k" -> 1), scanned))
        }
      }
    }
  }

  test(s"$backend: a batch that fails to encode writes nothing") {
    val refuse13: ValueCodec[Int] = new ValueCodec[Int] {
      def encode(v: Int): Either[StoreError, ByteVector] =
        if (v == 13) StoreError.Codec("value", "13 is refused").asLeft else intValue.encode(v)
      def decode(b: ByteVector): Either[StoreError, Int] = intValue.decode(b)
    }
    expectOk { implicit h =>
      withRaw { raw =>
        val kv = KeyValueStore[IO, String, Int](raw, ns("kv"))(using implicitly, implicitly, refuse13, h)
        h.attempt(kv.commit(Batch.puts(List("a" -> 1, "b" -> 13))))
          .flatMap(result => kv.get("a").map(a => expect(result.isLeft, "the commit fails").and(expect.same(None, a))))
      }
    }
  }

  // ------------------------------------------------------------------ versioned

  implicit private val showBatches: cats.Show[List[Batch[Int, Int]]] = cats.Show.fromToString
  implicit private val showBatchesAndTarget: cats.Show[(List[Batch[Int, Int]], Int)] = cats.Show.fromToString

  private val keyGen: Gen[Int] = Gen.choose(0, 6)
  private val batchGen: Gen[Batch[Int, Int]] =
    Gen
      .listOf(
        Gen.oneOf(
          Gen.zip(keyGen, Gen.choose(-50, 50)).map { case (k, v) => Op.Put(k, v): Op[Int, Int] },
          keyGen.map(Op.Delete(_): Op[Int, Int])
        )
      )
      .map(ops => Batch(ops.toVector))

  /** The reference model: the state after each version, `snapshots(v)`. */
  private def model(batches: List[Batch[Int, Int]]): Vector[Map[Int, Int]] =
    batches
      .scanLeft(Map.empty[Int, Int]) { (state, b) =>
        b.last.foldLeft(state) { case (s, (k, v)) => v.fold(s - k)(x => s.updated(k, x)) }
      }
      .toVector

  private def readAll(store: VersionedStore[IO, Int, Int], v: Long): IO[Map[Int, Int]] =
    (0 to 6).toList.traverseFilter(k => store.getAt(k, Version.unsafe(v)).map(_.map(k -> _))).map(_.toMap)

  test(s"$backend: versioned history matches the reference model") {
    forall(Gen.choose(1, 8).flatMap(Gen.listOfN(_, batchGen))) { batches =>
      val snapshots = model(batches)
      expectOk { implicit r =>
        withRaw { raw =>
          for {
            store   <- VersionedStore.make[IO, Int, Int](raw, "vs")
            _       <- batches.foldLeftM(Version.Genesis)((v, b) => store.commit(v, b))
            latest  <- store.latest
            current <- store.scan().compile.toList.map(_.toMap)
            history <- snapshots.indices.toList.traverse(v => readAll(store, v.toLong))
            diffs   <- (1 to batches.size).toList.traverse(v => store.changes(Version.unsafe(v.toLong)))
          } yield
            expect
              .same(batches.size.toLong, latest.value)
              .and(expect.same(snapshots.last, current))
              .and(expect.same(snapshots.toList, history))
              .and(
                expect.same(
                  batches.indices.toList
                    .map(i => batches(i).last.map { case (k, after) => Change(k, snapshots(i).get(k), after) }.toList.sortBy(_.key)),
                  diffs.map(_.sortBy(_.key))
                )
              )
        }
      }
    }
  }

  test(s"$backend: rollback restores an earlier version and history continues from it") {
    forall(Gen.choose(2, 8).flatMap(Gen.listOfN(_, batchGen)).flatMap(bs => Gen.choose(0, bs.size - 1).map(bs -> _))) {
      case (batches, to) =>
        val snapshots = model(batches)
        expectOk { implicit r =>
          withRaw { raw =>
            for {
              store    <- VersionedStore.make[IO, Int, Int](raw, "vs")
              _        <- batches.foldLeftM(Version.Genesis)((v, b) => store.commit(v, b))
              _        <- store.rollback(Version.unsafe(to.toLong))
              restored <- store.scan().compile.toList.map(_.toMap)
              latest   <- store.latest
              next     <- store.commit(latest, Batch.put(99, 1))
              after    <- store.get(99)
            } yield
              expect
                .same(snapshots(to), restored)
                .and(expect.same(to.toLong, latest.value))
                .and(expect.same(to.toLong + 1, next.value))
                .and(expect.same(Some(1), after))
          }
        }
    }
  }

  test(s"$backend: prune discards history at or below the floor and keeps the rest") {
    val batches = List(Batch.put[Int, Int](1, 10), Batch.put[Int, Int](1, 20), Batch.put[Int, Int](2, 5), Batch.delete[Int, Int](1))
    attempt { implicit h =>
      withRaw { raw =>
        for {
          store  <- VersionedStore.make[IO, Int, Int](raw, "vs")
          _      <- batches.foldLeftM(Version.Genesis)((v, b) => store.commit(v, b))
          _      <- store.prune(Version.unsafe(2L))
          floor  <- store.floor
          at2    <- store.getAt(1, Version.unsafe(2L))
          at3    <- store.getAt(1, Version.unsafe(3L))
          pruned <- h.attempt(store.getAt(1, Version.unsafe(1L)))
        } yield (floor, at2, at3, pruned)
      }
    }.map {
      case Right((floor, at2, at3, pruned)) =>
        expect
          .same(2L, floor.value)
          .and(expect.same(Some(20), at2))
          .and(expect.same(Some(20), at3))
          .and(expect.same(Left(StoreError.Pruned(1L, 2L)), pruned))
      case Left(e) => failure(s"unexpected $e")
    }
  }

  test(s"$backend: the change into the floor version is pruned with the history below it") {
    attempt { implicit h =>
      withRaw { raw =>
        for {
          store <- VersionedStore.make[IO, Int, Int](raw, "vs")
          _ <- List(Batch.put[Int, Int](1, 10), Batch.put[Int, Int](1, 20), Batch.put[Int, Int](2, 5))
            .foldLeftM(Version.Genesis)(store.commit)
          _     <- store.prune(Version.unsafe(2L))
          above <- store.changes(Version.unsafe(3L))
          at    <- h.attempt(store.changes(Version.unsafe(2L)))
        } yield (above, at)
      }
    }.map {
      case Right((above, at)) =>
        expect
          .same(List(Change(2, None, Some(5))), above)
          .and(expect.same(Left(StoreError.Pruned(2L, 2L)), at))
      case Left(e) => failure(s"unexpected $e")
    }
  }

  test(s"$backend: a staged commit writes nothing until written, then lands as exactly one compare-and-write") {
    forall(Gen.choose(1, 6).flatMap(Gen.listOfN(_, batchGen))) { batches =>
      val snapshots = model(batches)
      expectOk { implicit r =>
        withRaw { base =>
          for {
            raw   <- WatchedRawStore(base)
            store <- VersionedStore.make[IO, Int, Int](raw, "vs")
            steps <- batches.traverse { b =>
              for {
                v       <- store.latest
                before  <- raw.contents
                atStart <- raw.writes
                s       <- store.stage(v, b)
                after   <- raw.contents
                staged  <- raw.writes
                _       <- store.write(s)
                written <- raw.writes
              } yield (before == after, staged == atStart, (written._1 - staged._1, written._2 - staged._2))
            }
            current <- store.scan().compile.toList.map(_.toMap)
          } yield
            expect(steps.forall(_._1), "staging changed no record")
              .and(expect(steps.forall(_._2), "staging reached no write"))
              .and(expect.same(batches.map(_ => (0, 1)), steps.map(_._3)))
              .and(expect.same(snapshots.last, current))
        }
      }
    }
  }

  test(s"$backend: a crash during the write leaves every record as it was") {
    forall(Gen.choose(1, 4).flatMap(Gen.listOfN(_, batchGen)).flatMap(bs => batchGen.map(bs :+ _))) { batches =>
      expectOk { implicit r =>
        withRaw { base =>
          for {
            raw     <- WatchedRawStore(base)
            store   <- VersionedStore.make[IO, Int, Int](raw, "vs")
            v       <- batches.init.foldLeftM(Version.Genesis)(store.commit)
            before  <- raw.contents
            _       <- raw.crashNextWrites(true)
            crashed <- store.commit(v, batches.last).attempt
            _       <- raw.crashNextWrites(false)
            after   <- raw.contents
            latest  <- store.latest
          } yield expect(crashed.isLeft, "the write failed").and(expect(before == after, "no record changed")).and(expect.same(v, latest))
        }
      }
    }
  }

  test(s"$backend: chained drafts read through the drafts below them and equal sequential commits") {
    forall(Gen.choose(1, 6).flatMap(Gen.listOfN(_, batchGen))) { batches =>
      expectOk { implicit r =>
        for {
          sequential <- withRaw { raw =>
            VersionedStore
              .make[IO, Int, Int](raw, "vs")
              .flatMap(s => batches.foldLeftM(Version.Genesis)(s.commit) >> raw.scan(ByteRange.all).compile.toList)
          }
          chained <- withRaw { base =>
            for {
              raw   <- WatchedRawStore(base)
              store <- VersionedStore.make[IO, Int, Int](raw, "vs")
              first <- store.stage(Version.Genesis, batches.head)
              draft <- batches.tail.foldLeftM(first)(store.stageOn)
              empty <- raw.contents
              v     <- store.write(draft)
              all   <- raw.contents
              n     <- raw.writes
            } yield (empty, v, all, n)
          }
        } yield
          expect(chained._1.isEmpty, "drafts write nothing")
            .and(expect.same(batches.size.toLong, chained._2.value))
            .and(expect(sequential == chained._3, "the same records as committing one by one"))
            .and(expect.same((0, 1), chained._4))
      }
    }
  }

  test(s"$backend: a restored store holds the same records as one that committed and pruned through the same version") {
    forall(Gen.choose(1, 6).flatMap(Gen.listOfN(_, batchGen))) { batches =>
      expectOk { implicit r =>
        for {
          pruned <- withRaw { raw =>
            VersionedStore.make[IO, Int, Int](raw, "vs").flatMap { s =>
              batches.foldLeftM(Version.Genesis)(s.commit).flatMap(v => s.prune(v)) >> raw.scan(ByteRange.all).compile.toList
            }
          }
          restored <- withRaw { raw =>
            VersionedStore.make[IO, Int, Int](raw, "vs").flatMap { s =>
              s.stageRestore(model(batches).last.toList, Version.unsafe(batches.size.toLong)).flatMap(s.write) >>
              (raw.scan(ByteRange.all).compile.toList, r.attempt(s.stageRestore(Nil, Version.unsafe(1L)))).tupled
            }
          }
        } yield
          expect(pruned == restored._1, "the same records")
            .and(expect.same(Left(StoreError.VersionConflict(0L, batches.size.toLong)), restored._2))
      }
    }
  }

  test(s"$backend: a draft staged against a version that moved on does not write") {
    attempt { implicit r =>
      withRaw { raw =>
        for {
          store <- VersionedStore.make[IO, Int, Int](raw, "vs")
          draft <- store.stage(Version.Genesis, Batch.put(1, 1))
          _     <- store.commit(Version.Genesis, Batch.put(2, 2))
          _     <- store.write(draft)
        } yield ()
      }
    }.map(result => expect.same(Left(StoreError.VersionConflict(0L, 1L)), result))
  }

  test(s"$backend: optimistic concurrency rejects a stale expected version") {
    attempt { implicit r =>
      withRaw { raw =>
        VersionedStore.make[IO, Int, Int](raw, "vs").flatMap { store =>
          store.commit(Version.Genesis, Batch.put(1, 1)) >> store.commit(Version.Genesis, Batch.put(1, 2))
        }
      }
    }.map(result => expect.same(Left(StoreError.VersionConflict(0L, 1L)), result))
  }

  test(s"$backend: compare-and-write applies only when every guard matches") {
    expectOk { implicit r =>
      withRaw { raw =>
        raw.write(RawBatch.put(hex"01", hex"aa")) >>
        (
          raw.writeIf(List(hex"01" -> Some(hex"aa"), hex"02" -> None), RawBatch.put(hex"03", hex"cc")),
          raw.writeIf(List(hex"01" -> Some(hex"bb")), RawBatch.put(hex"04", hex"dd"))
        ).tupled.flatMap {
          case (first, second) =>
            (raw.get(hex"03"), raw.get(hex"04")).mapN { (c, d) =>
              expect(first).and(expect(!second)).and(expect.same(Some(hex"cc"), c)).and(expect.same(None, d))
            }
        }
      }
    }
  }

  test(s"$backend: concurrent commits through separate instances: exactly one wins per version") {
    expectOk { implicit r =>
      withRaw { raw =>
        for {
          // each racer builds its own instance, raising through its own handler
          results <- (0 until 8).toList.parTraverse { i =>
            attempt(r2 =>
              VersionedStore
                .make[IO, Int, Int](raw, "vs")(using implicitly, implicitly, implicitly, r2)
                .flatMap(_.commit(Version.Genesis, Batch.put(i, i)))
            )
          }
          reader  <- VersionedStore.make[IO, Int, Int](raw, "vs")
          latest  <- reader.latest
          content <- reader.scan().compile.toList
        } yield {
          val wins = results.collect { case Right(v) => v }
          val conflicts = results.collect { case Left(StoreError.VersionConflict(0L, 1L)) => () }
          expect.same(1, wins.size).and(expect.same(7, conflicts.size)).and(expect.same(1L, latest.value)).and(expect.same(1, content.size))
        }
      }
    }
  }

  test(s"$backend: reading a future version is a typed error") {
    attempt { implicit r =>
      withRaw(raw => VersionedStore.make[IO, Int, Int](raw, "vs").flatMap(_.getAt(1, Version.unsafe(5L))))
    }.map(result => expect.same(Left(StoreError.UnknownVersion(5L, 0L)), result))
  }
}
