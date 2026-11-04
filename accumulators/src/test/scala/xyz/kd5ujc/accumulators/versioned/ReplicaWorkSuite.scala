package xyz.kd5ujc.accumulators.versioned

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{Trie, TrieHashing}
import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Digest

import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * A replica's work per delta (docs/specs/versioned-trie.md §7): its tries are updated incrementally, equal to a full
 * rebuild after every delta, and a delta re-hashes only its changed keys' paths, whatever the state's size or however
 * its keys are chosen; its values are compared, never hashed. Nothing here measures time: a counting [[TrieHashing]]
 * reports the node hashes, and a counting key order the comparisons.
 */
object ReplicaWorkSuite extends SimpleIOSuite with Checkers {

  private type R = Replica[Long, H]

  /** The reference: the head of `state` and `history`, both rebuilt from scratch, over sorted lists. */
  private def headOf(version: Long, state: List[(StateKey, CanonicalValue)], history: List[(Long, Digest[H])]): Head[Long, H] = {
    val stateRoot = get(
      Trie.fromEntries[H](get(VersionedTrie.stateMode), state.map { case (k, v) => k.bytes -> VersionedTrie.valueHash[H](v) })
    ).root
    Head(version, stateRoot, historyRoot(history :+ (version -> stateRoot)))
  }

  /** The model after a delta: the state as a sorted map (so no reference structure hashes a key either). */
  final private case class Model(
    state:   scala.collection.immutable.TreeMap[StateKey, CanonicalValue],
    history: List[(Long, Digest[H])],
    head:    Option[Head[Long, H]]
  ) {

    def delta(version: Long, upserts: List[(StateKey, CanonicalValue)], removes: List[StateKey]): (Delta[Long, H], Model) = {
      val next = upserts.foldLeft(state -- removes)(_ + _)
      val h = headOf(version, next.toList, history)
      (Delta(head, h, upserts, removes), Model(next, history :+ (version -> h.stateRoot), Some(h)))
    }

    def snapshot: Snapshot[Long, H] = Snapshot(head.getOrElse(sys.error("no head")), state.toList, history)
  }

  private val start = Model(scala.collection.immutable.TreeMap.empty[StateKey, CanonicalValue](using Replica.keyOrder), Nil, None)

  // ---- incremental roots equal a rebuild

  private val universe: Vector[StateKey] = Vector(
    "a",
    "a/b",
    "a/b/c",
    "a/b/c/d",
    "a/c",
    "ab",
    "ab/c",
    "b",
    "b0",
    "b1",
    "b2",
    "c",
    "fiber/1",
    "fiber/2",
    "fiber/10",
    "fiberx/1",
    "z",
    "z/z",
    "0",
    "00",
    "01",
    "1",
    "q.r",
    "q-r",
    "q_r"
  ).map(key)

  /** One delta's writes: `None` empties the state; otherwise keys with a value to write or none to remove. */
  private val writesGen: Gen[Option[List[(StateKey, Option[CanonicalValue])]]] =
    Gen.frequency(
      1 -> Gen.const(None),
      12 -> Gen
        .choose(1, 6)
        .flatMap(n => Gen.listOfN(n, Gen.zip(Gen.oneOf(universe), Gen.option(Gen.choose(0, 999).map(int)))))
        .map(Some(_))
    )

  private val forged: Digest[H] = xyz.kd5ujc.core.hash.Hasher[H].digest(ByteVector(0xab))

  test("incremental roots equal a full rebuild after every delta; a refused delta changes nothing") {
    forall(Gen.choose(1, 40).flatMap(Gen.listOfN(_, writesGen))) { steps =>
      val result = steps.zipWithIndex.foldLeftM((get(Replica.empty[Long, H]), start)) {
        case ((r, m), (writes, i)) =>
          // a remove may name an absent key, which changes nothing; a key named twice keeps its last write
          val chosen = writes.fold(m.state.keys.toList.map(_ -> Option.empty[CanonicalValue]))(ws =>
            scala.collection.immutable.TreeMap.from(ws)(using Replica.keyOrder).toList
          )
          val (d, next) = m.delta(3L * (i + 1), chosen.collect { case (k, Some(v)) => k -> v }, chosen.collect { case (k, None) => k })
          for {
            _ <- Replica
              .applyDelta(r, d.copy(head = d.head.copy(stateRoot = forged)))
              .swap
              .leftMap(_ => s"step $i: a forged state root applied")
            _ <- Replica
              .applyDelta(r, d.copy(head = d.head.copy(historyRoot = forged)))
              .swap
              .leftMap(_ => s"step $i: a forged history root applied")
            r2 <- Replica.applyDelta(r, d).leftMap(e => s"step $i: $e")
            _  <- Either.cond(r2.head == next.head, (), s"step $i: head ${r2.head} is not ${next.head}")
            _  <- Either.cond(r2.values.toList == next.state.toList, (), s"step $i: values differ")
            _  <- Either.cond(writes.isDefined || r2.state.isEmpty, (), s"step $i: the state was not emptied")
          } yield (r2, next)
      }
      result.fold(
        failure,
        {
          case (r, m) =>
            val restored = m.head.traverse(h => Replica.fromSnapshot(h, m.snapshot))
            expect.same(Right(Some(r)), restored).and(expect.same(m.state.toList, r.values.toList))
        }
      )
    }
  }

  // ---- work per delta

  /** The trie hashing, counting the node hashes (leaves and branches) it makes. */
  final private class Counting extends TrieHashing[H] {
    val nodes: AtomicLong = new AtomicLong
    def hasher: xyz.kd5ujc.core.hash.Hasher[H] = T.hasher
    def empty: Digest[H] = T.empty
    def valueOf(bytes: xyz.kd5ujc.core.canonical.ByteSource): Digest[H] = T.valueOf(bytes)
    def leaf(segment: BitVector, value: Digest[H]): Digest[H] = {
      nodes.incrementAndGet()
      T.leaf(segment, value)
    }
    def branch(segment: BitVector, value: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]]): Digest[H] = {
      nodes.incrementAndGet()
      T.branch(segment, value, left, right)
    }
  }

  /** [[Replica.keyOrder]], counting its comparisons. */
  final private class Comparisons extends Ordering[StateKey] {
    val count: AtomicLong = new AtomicLong
    def compare(a: StateKey, b: StateKey): Int = {
      count.incrementAndGet()
      Replica.keyOrder.compare(a, b)
    }
  }

  private val K = 12
  private val N = 1 << K

  /** `ceil(log2 (n + 1))`: a red-black tree of `n` entries is at most twice this high. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  /** Bit positions at which `keys` do not all agree (past a key's end counts): every branch on a path sits at one. */
  private def varyingBits(keys: List[ByteVector]): Long = {
    val bits = keys.map(_.bits)
    val longest = bits.map(_.size).maxOption.getOrElse(0L)
    def at(b: BitVector, i: Long): Option[Boolean] = Option.when(i < b.size)(b(i))
    (0L until longest).count(i => bits.exists(b => at(b, i) != at(bits.head, i))).toLong
  }

  /**
   * The most node hashes one delta may make: two top-trie roots (the parent check and the claimed parent, five node
   * hashes each), and for each changed key and the new version its path re-hashed with at most one lowered or merged
   * node and one new leaf.
   */
  private def bound(changes: Long, stateDepth: Long, historyDepth: Long): Long =
    2L * 5 + changes * (stateDepth + 1 + 2) + (historyDepth + 1 + 2)

  /** A replica of `keys`, each holding its index, restored from a snapshot at version 1 through `hashing` and `order`. */
  private def seeded(keys: List[StateKey], hashing: Counting, order: Comparisons): (R, Model) = {
    val (_, m) = start.delta(1L, keys.zipWithIndex.map { case (k, i) => k -> int(i) }, Nil)
    (get(Replica.fromSnapshotIn[Long, H](m.head.getOrElse(sys.error("no head")), m.snapshot, order)(using VersionKey.long, hashing)), m)
  }

  /** Every mix of `k` blocks `a` and `b`: with "an" and "c0" all share one Java hash code (97·31 + 110 = 99·31 + 48). */
  private def mixes(k: Int, a: String, b: String): List[String] =
    (1 to k).foldLeft(List(""))((acc, _) => acc.flatMap(s => List(s + a, s + b)))

  private val families: List[(String, List[StateKey])] = List(
    "ordinary"       -> (0 until N).toList.map(i => key(f"k/$i%04d")),
    "java-colliding" -> mixes(K, "an", "c0").map(key),
    "v8-colliding"   -> (0 until N).toList.map(i => key((i.toLong * 65536).toString))
  )

  pureTest("the Java-colliding keys do collide, and every family is distinct keys") {
    val java = families(1)._2
    expect
      .same(1, java.map(_.value.hashCode).distinct.size)
      .and(expect.same(1, java.map(_.hashCode).distinct.size))
      .and(families.foldMap { case (name, ks) => expect.same(N, ks.map(_.value).distinct.size).and(expect(name.nonEmpty)) })
  }

  /** `a`, with the node hashes and the comparisons it made. */
  private def counted[A](hashing: Counting, order: Comparisons)(a: => A): IO[(A, Long, Long)] = {
    val read = IO((hashing.nodes.get, order.count.get))
    read.flatMap { case (n0, c0) => IO(a).flatMap(x => read.map { case (n1, c1) => (x, n1 - n0, c1 - c0) }) }
  }

  /**
   * A replica of `keys` restored, then a delta changing three of them (one updated, one removed, one new): the
   * restore's node hashes and comparisons, the delta's, and the delta's bound on node hashes.
   */
  private def threeChanges(keys: List[StateKey]): IO[((Long, Long), (Long, Long), Long)] =
    IO((new Counting, new Comparisons)).flatMap {
      case (hashing, order) =>
        val fresh = key("fresh/key")
        val limit = bound(3, varyingBits((fresh :: keys).map(_.bytes)), varyingBits(List(vk(1L), vk(2L))))
        for {
          restored <- counted(hashing, order)(seeded(keys, hashing, order))
          ((r, m), built, restoreComparisons) = restored
          (d, _) = m.delta(2L, List(keys(keys.size / 3) -> int(-1), fresh -> int(-2)), List(keys(2 * keys.size / 3)))
          applied <- counted(hashing, order)(get(Replica.applyDelta(r, d)(using VersionKey.long, hashing)))
        } yield ((built, restoreComparisons), (applied._2, applied._3), limit)
    }

  families.foreach {
    case (name, keys) =>
      test(s"a delta hashes only its changed keys' paths, and compares keys O(log n) times: $name keys") {
        threeChanges(keys).map {
          case ((built, restoreComparisons), (hashes, comparisons), limit) =>
            // the reference model builds its own trie with the plain hashing: these are the restore's alone
            expect(built >= N, s"a restore of $N keys made $built node hashes")
              .and(expect(hashes <= limit, s"a 3-key delta made $hashes node hashes, over $limit"))
              .and(expect(hashes * 40 < built, s"a 3-key delta made $hashes node hashes, against $built for a restore"))
              // a hashed map makes no comparisons: at least n - 1 shows the values went into the ordered map
              .and(expect(restoreComparisons >= N - 1, s"restoring made $restoreComparisons comparisons"))
              .and(expect(restoreComparisons <= N * (2L * log2(N) + 1), s"restoring made $restoreComparisons comparisons"))
              // three removals or insertions in the values and three in the delta's own key set, each one tree walk
              .and(expect(comparisons >= 3, s"a delta made $comparisons comparisons"))
              .and(expect(comparisons <= 6L * (2L * log2(N) + 1), s"a delta made $comparisons comparisons"))
        }
      }
  }

  test("the same delta on a state 64 times smaller makes about as many node hashes") {
    val ordinary = families.head._2
    (threeChanges(ordinary.take(N / 64)), threeChanges(ordinary)).mapN {
      case ((_, (small, _), _), (_, (large, _), _)) => expect(large <= small + 3 * 8, s"$small node hashes at ${N / 64} keys, $large at $N")
    }
  }

  test("a deep key costs its depth, and a key beside it only its own") {
    // every prefix of a 255-character key that is itself a key: each a branch value above the next
    val long = List.fill(4)("a" * 63).mkString("/")
    val chain = (1 to long.length).toList.flatMap(i => StateKey.parse(long.take(i)).toOption)
    val bulk = families.head._2
    IO((new Counting, new Comparisons)).flatMap {
      case (hashing, order) =>
        def apply(r: R, d: Delta[Long, H]): IO[(R, Long)] =
          counted(hashing, order)(get(Replica.applyDelta(r, d)(using VersionKey.long, hashing))).map { case (r2, nodes, _) => (r2, nodes) }
        for {
          seed <- IO(seeded(chain ++ bulk, hashing, order))
          (d2, m2) = seed._2.delta(2L, List(chain.last -> int(7)), Nil)
          deep    <- apply(seed._1, d2)
          shallow <- apply(deep._1, m2.delta(3L, List(bulk(7) -> int(7)), Nil)._1)
        } yield
          expect(deep._2 > chain.size.toLong, s"the deepest key's path has ${chain.size} nodes, re-hashed: ${deep._2}")
            .and(expect(deep._2 <= bound(1, chain.size.toLong + 1, 2), s"${deep._2} node hashes for one deep key"))
            .and(expect(shallow._2 <= bound(1, varyingBits(bulk.map(_.bytes)) + 1, 2), s"${shallow._2} node hashes for one shallow key"))
    }
  }

  test("the deliberate regression: a rebuild per delta is over the bound") {
    val keys = families.head._2
    val (_, m) = start.delta(1L, keys.zipWithIndex.map { case (k, i) => k -> int(i) }, Nil)
    val entries = m.state.toList.map { case (k, v) => k.bytes -> VersionedTrie.valueHash[H](v) }
    IO((new Counting, new Comparisons)).flatMap {
      case (hashing, order) =>
        counted(hashing, order)(get(Trie.fromEntries[H](get(VersionedTrie.stateMode), entries)(using hashing))).map {
          case (_, rebuilt, _) => expect(rebuilt > bound(3, varyingBits(keys.map(_.bytes)), 2), s"a rebuild makes $rebuilt node hashes")
        }
    }
  }
}
