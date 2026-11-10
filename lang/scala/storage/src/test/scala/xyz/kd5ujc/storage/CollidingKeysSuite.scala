package xyz.kd5ujc.storage

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.mtl.{Handle, Raise}
import cats.syntax.all._

import scala.collection.immutable.TreeMap

import xyz.kd5ujc.storage.ByteOrder.unsigned

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for the stores. A batch's keys, and the encoded keys a stage collects, are the callers', and
 * a hashed collection of keys chosen to collide degrades to linear scans: quadratic work for one batch. Keys are only
 * compared instead. Nothing here measures time: a [[Colliding]] key has one hash code for every key and counts each
 * `hashCode` and `equals` call, so a hashed path shows up as calls; and every order counts its comparisons, bounded by
 * the input's size (and at least one per key: a hashed path makes none).
 */
object CollidingKeysSuite extends SimpleIOSuite with Checkers {

  private val K = 12
  private val N = 1 << K

  /** `ceil(log2 (n + 1))`: a red-black tree of `n` entries is at most twice this high. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  /** The most comparisons `n` lookups or insertions make in a red-black tree of at most `n` entries. */
  private def nLogN(n: Int): Long = n.toLong * (2L * log2(n) + 1L)

  /** A key whose hash code is the same for every key; it counts the calls a hashed collection would make on it. */
  final private class Colliding(val id: Int, calls: Calls) {
    override def hashCode: Int = { calls.hashes.incrementAndGet(); 0 }
    override def equals(other: Any): Boolean = {
      calls.equalities.incrementAndGet()
      other match {
        case c: Colliding => c.id == id
        case _            => false
      }
    }
    override def toString: String = s"Colliding($id)"
  }

  final private class Calls {
    val hashes: AtomicLong = new AtomicLong
    val equalities: AtomicLong = new AtomicLong
    val comparisons: AtomicLong = new AtomicLong
    val order: Ordering[Colliding] = (a, b) => { comparisons.incrementAndGet(); Integer.compare(a.id, b.id) }
  }

  /** The unsigned byte order, counting its comparisons. */
  final private class CountingBytes extends Ordering[ByteVector] {
    val comparisons: AtomicLong = new AtomicLong
    def compare(a: ByteVector, b: ByteVector): Int = { comparisons.incrementAndGet(); unsigned.compare(a, b) }
  }

  private def within(what: String, n: Int, comparisons: Long, upper: Long): Expectations =
    expect(comparisons >= n - 1L, s"$what: $comparisons comparisons for $n keys; a hashed path makes none")
      .and(expect(comparisons <= upper, s"$what: $comparisons comparisons for $n keys, above $upper"))

  /** The reference: each key's last write, keys in the order first written, by linear scans. */
  private def reference[A, V](ops: Vector[Op[A, V]]): Vector[(A, Option[V])] = {
    val writes = ops.map {
      case Op.Put(k, v) => k -> Option(v)
      case Op.Delete(k) => k -> Option.empty[V]
    }
    writes.map(_._1).foldLeft(Vector.empty[A])((seen, k) => if (seen.contains(k)) seen else seen :+ k).map { k =>
      k -> writes.findLast(_._1 == k).flatMap(_._2)
    }
  }

  test("the watch works: a hashed map of colliding keys hashes each and compares them pairwise") {
    IO {
      val calls = new Calls
      val n = 256
      val keys = Vector.tabulate(n)(new Colliding(_, calls))
      val m = keys.foldLeft(Map.empty[Colliding, Int])((acc, k) => acc.updated(k, k.id))
      expect(calls.hashes.get >= n.toLong, s"${calls.hashes.get} hash calls")
        .and(expect(calls.equalities.get >= n.toLong * (n - 1) / 2, s"${calls.equalities.get} equality calls for $n colliding keys"))
        .and(expect.same(n, m.size))
    }
  }

  test("Batch.last: O(n log n) comparisons and no key hashed or tested for equality, on keys that all collide") {
    IO {
      val calls = new Calls
      val keys = Vector.tabulate(N)(new Colliding(_, calls))
      // every key written, then every third deleted, then every fifth written again
      val ops = keys.map(k => Op.Put(k, k.id): Op[Colliding, Int]) ++
        keys.filter(_.id % 3 == 0).map(Op.Delete(_)) ++
        keys.filter(_.id % 5 == 0).map(k => Op.Put(k, -k.id))
      val finals = Batch(ops).last(using calls.order)
      val (hashes, equalities, comparisons) = (calls.hashes.get, calls.equalities.get, calls.comparisons.get)
      within("last", ops.size, comparisons, 2L * nLogN(ops.size))
        .and(expect.same(0L, hashes))
        .and(expect.same(0L, equalities))
        .and(expect.same(keys.map(_.id), finals.map(_._1.id)))
        .and(
          expect.same(
            keys.map(k => if (k.id % 5 == 0) Some(-k.id) else if (k.id % 3 == 0) None else Some(k.id)),
            finals.map(_._2)
          )
        )
    }
  }

  test("Batch.last: writes to one key compare it on every write, and the last one wins") {
    IO {
      val calls = new Calls
      val key = new Colliding(7, calls)
      val ops = Vector.tabulate(N)(i => if (i % 2 == 0) Op.Put(key, i) else Op.Delete(key): Op[Colliding, Int])
      val finals = Batch(ops).last(using calls.order)
      within("one key", N, calls.comparisons.get, 2L * N)
        .and(expect.same(0L, calls.hashes.get))
        .and(expect.same(Vector(7 -> Option.empty[Int]), finals.map { case (k, v) => k.id -> v }))
    }
  }

  test("Batch.last agrees with a reference of linear scans: first-touched order, last write wins") {
    val op = Gen.choose(0, 9).flatMap(k => Gen.oneOf(Gen.choose(0, 99).map(v => Op.Put(k, v): Op[Int, Int]), Gen.const(Op.Delete(k))))
    implicit val showOps: cats.Show[Vector[Op[Int, Int]]] = cats.Show.fromToString
    forall(Gen.listOf(op).map(_.toVector)) { ops =>
      expect.same(reference(ops), Batch(ops).last)
    }
  }

  // ------------------------------------------------------------------ VersionedStore

  private def key(i:   Int): ByteVector = ByteVector.fromInt(i)
  private def value(i: Int): ByteVector = ByteVector.fromInt(i).reverse

  private def attempt[A](f: Handle[IO, StoreError] => IO[A]): IO[Either[StoreError, A]] =
    Handle.allowF[IO, StoreError](h => f(h).map(_.asRight[StoreError])).rescue(e => IO.pure(e.asLeft[A]))

  private def run(f: Handle[IO, StoreError] => IO[Expectations]): IO[Expectations] =
    attempt(f).map(_.fold(e => failure(s"unexpected store error $e"), identity))

  /** A fresh in-memory store whose stages compare encoded keys through a counting order. */
  private def store(order: CountingBytes)(implicit R: Raise[IO, StoreError]): IO[VersionedStore[IO, ByteVector, ByteVector]] =
    RawStore.inMemory[IO].flatMap(raw => VersionedStore.ordered[IO, ByteVector, ByteVector](raw, "vs", order))

  /** `f`'s result and the comparisons it made. */
  private def counted[A](order: CountingBytes)(f: IO[A]): IO[(A, Long)] =
    IO(order.comparisons.get).flatMap(before => f.map(a => a -> (order.comparisons.get - before)))

  private def orderedBy(what: String, staged: VersionedStore.Staged, order: Ordering[ByteVector]): Expectations =
    expect(staged.data.ordering eq order, s"$what: the draft's values are ordered by ${staged.data.ordering}")

  test("stage: n keys collapse and are drafted in O(n log n) comparisons, in the store's order") {
    val order = new CountingBytes
    run { implicit r =>
      for {
        s <- store(order)
        batch = Batch.puts((0 until N).map(i => key(i) -> value(i))) ++ Batch.deletes((0 until N by 2).map(key))
        (staged, comparisons) <- counted(order)(s.stage(Version.Genesis, batch))
        _                     <- s.write(staged)
        changes               <- s.changes(Version.unsafe(1L))
      } yield
        within("stage", N, comparisons, 4L * nLogN(N + N / 2))
          .and(orderedBy("stage", staged, order))
          .and(expect.same(N, staged.data.size))
          .and(expect.same(N / 2, staged.data.values.count(_.isDefined)))
          .and(expect.same(N, changes.size))
    }
  }

  test("stage: a key written n times is compared on every write, and its last write wins") {
    val order = new CountingBytes
    run { implicit r =>
      for {
        s <- store(order)
        batch = Batch(Vector.tabulate(N)(i => Op.Put(key(1), value(i)): Op[ByteVector, ByteVector]))
        (staged, comparisons) <- counted(order)(s.stage(Version.Genesis, batch))
        _                     <- s.write(staged)
        now                   <- s.get(key(1))
      } yield
        within("one key", N, comparisons, 2L * N + 16L)
          .and(expect.same(Some(value(N - 1)), now))
          .and(expect.same(TreeMap(key(1) -> Option(value(N - 1)))(using unsigned), staged.data))
    }
  }

  test("stageOn: a draft over a draft reads through it in O(n log n) comparisons and keeps its order") {
    val order = new CountingBytes
    run { implicit r =>
      for {
        s     <- store(order)
        first <- s.stage(Version.Genesis, Batch.puts((0 until N).map(i => key(i) -> value(i))))
        // half the keys the draft wrote, and as many new ones
        batch = Batch.puts((N / 2 until N + N / 2).map(i => key(i) -> value(-i)))
        (second, comparisons) <- counted(order)(s.stageOn(first, batch))
        _                     <- s.write(second)
        changes               <- s.changes(Version.unsafe(2L))
      } yield
        within("stageOn", N, comparisons, 4L * nLogN(N + N / 2))
          .and(orderedBy("stageOn", second, order))
          .and(expect.same(N + N / 2, second.data.size))
          .and(expect.same(N / 2, changes.count(_.before.isDefined)))
    }
  }

  test("stageRollback: a key changed in n versions is compared once per change, and returns to its value then") {
    val order = new CountingBytes
    val versions = 1024
    run { implicit r =>
      for {
        s                     <- store(order)
        _                     <- (1 to versions).toList.foldLeftM(Version.Genesis)((v, i) => s.commit(v, Batch.put(key(1), value(i))))
        (staged, comparisons) <- counted(order)(s.stageRollback(Version.unsafe(1L)))
        _                     <- s.write(staged)
        now                   <- s.get(key(1))
      } yield
        within("rollback", versions - 1, comparisons, 2L * versions)
          .and(orderedBy("rollback", staged, order))
          .and(expect.same(Some(value(1)), now))
    }
  }

  test("stageRestore: n entries in O(n log n) comparisons, in the store's order, a later duplicate winning") {
    val order = new CountingBytes
    run { implicit r =>
      for {
        s                     <- store(order)
        entries = (0 until N).toList.map(i => key(i) -> value(i)) :+ (key(5) -> value(-5))
        (staged, comparisons) <- counted(order)(s.stageRestore(entries, Version.unsafe(3L)))
        next                  <- s.stageOn(staged, Batch.put(key(N), value(N)))
        _                     <- s.write(next)
        restored              <- s.getAt(key(5), Version.unsafe(3L))
      } yield
        within("restore", N, comparisons, nLogN(N + 1))
          .and(orderedBy("restore", staged, order))
          .and(expect.same(Some(Option(value(-5))), staged.data.get(key(5))))
          .and(expect.same(Some(value(-5)), restored))
    }
  }
}
