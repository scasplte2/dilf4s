package xyz.kd5ujc.core.canonical

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.TreeMap

import xyz.kd5ujc.core.canonical.CanonicalValue._

import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for canonical maps. Java's `String.hashCode` is trivially collidable (every combination of
 * "Aa" and "BB" blocks has one hash code), and a hashed map degrades to linear scans on colliding keys. A map is a
 * `TreeMap` in [[KeyOrder]] instead. Nothing here measures time or looks inside a key: every path runs through a
 * counting [[KeyOrder]], whose comparisons are bounded below (a path that hashes in place of comparing compares
 * nothing) and above (n log n), and, where a path is given its keys in a known order, counted exactly (a step that
 * reorders them changes the count). These are complexity checks: a hashed step that keeps its input's order compares
 * exactly as often, and these counts cannot see it. What holds structurally: the opaque [[CanonicalEntries]] and
 * [[CanonicalKeys]] types keep every canonical map and key set in one key order, and core does not depend on circe,
 * so circe's hashed `JsonObject` is out of its reach.
 */
object CollidingKeysSuite extends SimpleIOSuite {

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:      String): CText = get(CanonicalValue.text(s))

  /** `2^k` distinct keys of `2k` units with one Java hash code: every combination of "Aa" and "BB". */
  private def colliding(k: Int): Vector[String] = combinations(k, "Aa", "BB")

  /** The control: as many keys, as long, from "Aa" and "Ac", whose hash codes differ. */
  private def control(k: Int): Vector[String] = combinations(k, "Aa", "Ac")

  private def combinations(k: Int, a: String, b: String): Vector[String] =
    (1 to k).foldLeft(Vector(""))((acc, _) => acc.flatMap(s => Vector(s + a, s + b)))

  private val K = 12
  private val N = 1 << K

  /** `ceil(log2 (n + 1))`: a red-black tree of `n` entries is at most twice this high. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  /** The most comparisons `n` insertions or lookups make in a red-black tree of at most `n` entries. */
  private def nLogN(n: Int): Long = n.toLong * (2L * log2(n) + 1L)

  /**
   * [[KeyOrder.Default]], counting its comparisons, the key units they read, and the keys it measures. `compareTo` reads
   * both keys up to their first difference: the common prefix, and one unit more of each unless a key ends there.
   */
  final private class Counting extends KeyOrder {
    val comparisons: AtomicLong = new AtomicLong
    val units: AtomicLong = new AtomicLong
    val measures: AtomicLong = new AtomicLong
    def compare(a: String, b: String): Int = {
      val n = math.min(a.length, b.length)
      val common = (0 until n).find(i => a.charAt(i) != b.charAt(i)).getOrElse(n)
      comparisons.incrementAndGet()
      units.addAndGet(math.min(common + 1, n).toLong)
      KeyOrder.Default.compare(a, b)
    }
    def measure(key: String): Long = {
      measures.incrementAndGet()
      KeyOrder.Default.measure(key)
    }
    def counts: (Long, Long, Long) = (comparisons.get, units.get, measures.get)
  }

  /** `f` under a fresh counting order: its result, and the comparisons, units and measures it made. */
  private def counting[A](f: Counting => A): IO[(A, Long, Long, Long)] =
    IO(new Counting).flatMap(keys => IO(f(keys)).map(a => (a, keys.comparisons.get, keys.units.get, keys.measures.get)))

  /**
   * The comparisons of inserting `keys` one at a time, in this order, into a tree in [[KeyOrder]]: exactly what a path
   * that inserts the keys it is given, as it is given them, must make.
   */
  private def inserting(keys: Vector[String]): Long = {
    val o = new Counting
    val _ = keys.foldLeft(TreeMap.empty[String, Int](using o))(_.updated(_, 1))
    o.comparisons.get
  }

  /**
   * The comparisons of merging, left to right, trees each built by [[inserting]] one part: exactly what a merge of the
   * parts' maps must make, as their trees' union.
   */
  private def unioning(parts: Vector[Vector[String]]): Long = {
    val o = new Counting
    val trees = parts.map(_.foldLeft(TreeMap.empty[String, Int](using o))(_.updated(_, 1)))
    val before = o.comparisons.get
    val _ = trees.foldLeft(TreeMap.empty[String, Int](using o))(_ ++ _)
    o.comparisons.get - before
  }

  /** The hash code `String.hashCode` computes. */
  private def javaHash(s: String): Int = s.foldLeft(0)((h, c) => 31 * h + c)

  private def keysOf(v: CanonicalValue): Vector[String] = v match {
    case CMap(m) => m.keysIterator.toVector
    case _       => Vector.empty
  }

  private def ordered(what: String, v: CanonicalValue, keys: KeyOrder): Expectations = v match {
    case CMap(m) => expect(CanonicalEntries.ordering(m) eq keys, s"$what: the entries are not in the order passed")
    case other   => failure(s"$what: not a map: $other")
  }

  private def within(what: String, n: Int, comparisons: Long, upper: Long): Expectations =
    expect(comparisons >= n - 1L, s"$what: $comparisons comparisons for $n keys; a hashed path makes none")
      .and(expect(comparisons <= upper, s"$what: $comparisons comparisons for $n keys, above $upper"))

  test("every colliding key collides, and the exact count tells a hash-ordered insertion apart") {
    IO(
      expect
        .same(1, colliding(K).map(javaHash).distinct.size)
        .and(expect.same(N, colliding(K).distinct.size))
        .and(expect(control(K).map(javaHash).distinct.size > N / 2, "the control's keys mostly differ in hash code"))
        .and(expect(inserting(control(K)) != inserting(control(K).sortBy(javaHash)), "hash order inserts with another count"))
        .and(expect(inserting(control(K)) != inserting(control(K).map(_ -> 1).toMap.keysIterator.toVector), "so does a HashMap's order"))
    )
  }

  test("map: one insertion per key in O(n log n) comparisons, each key measured once") {
    val keys = colliding(K)
    counting(o => CanonicalValue.map(keys.map(_ -> CInt(1)), o).map(m => (m, o))).map {
      case (result, comparisons, _, measures) =>
        val m = get(result)._1
        within("map", N, comparisons, nLogN(N))
          .and(expect.same(inserting(keys), comparisons))
          .and(expect.same(N.toLong, measures))
          .and(ordered("map", m, get(result)._2))
          .and(expect.same(N, keysOf(m).size))
    }
  }

  test("the control costs the same: colliding keys take no more comparisons than keys that do not collide") {
    (
      counting(o => CanonicalValue.map(colliding(K).map(_ -> CInt(1)), o)),
      counting(o => CanonicalValue.map(control(K).map(_ -> CInt(1)), o))
    ).mapN {
      case ((_, adversarial, _, _), (_, plain, _, _)) =>
        expect(adversarial <= plain + plain / 10, s"colliding: $adversarial comparisons, control: $plain")
    }
  }

  test("decode: one insertion per key, in encoded order, and no key measured again") {
    def decoding(keys: Vector[String]) = {
      val bytes = Cbor.encode(get(CanonicalValue.map(keys.map(_ -> CInt(1)))))
      counting(o => (Cbor.decode(bytes, Cbor.DefaultMaxDepth, o), o, bytes))
    }
    (decoding(colliding(K)), decoding(control(K))).mapN {
      case (((result, o, bytes), comparisons, _, measures), ((plain, _, _), plainComparisons, _, _)) =>
        val v = get(result)
        // a sorted map iterates in encoded order here: every key has the same UTF-8 length
        within("decode", N, comparisons, nLogN(N))
          .and(expect.same(inserting(keysOf(v)), comparisons))
          .and(expect.same(inserting(keysOf(get(plain))), plainComparisons))
          .and(expect.same(0L, measures))
          .and(ordered("decode", v, o))
          .and(expect.same(bytes, Cbor.encode(v)))
    }
  }

  test("JSON: a repeated member name is found by comparing names as they are read, in O(n log n)") {
    val keys = colliding(K)
    val members = keys.map(k => s"\"$k\":1")
    val json = members.mkString("{", ",", "}")
    val repeated = members.appended(s"\"${keys(N / 2)}\":2").mkString("{", ",", "}")
    // every member read, then the input ends where the next name would start: no lookup of a next name is made
    val prefix = members.mkString("{", ",", ",")
    (
      counting(o => (CanonicalJson.parse(json, 0, o), o)),
      counting(o => CanonicalJson.parse(repeated, 0, o)),
      counting(o => CanonicalJson.parse(prefix, 0, o))
    ).mapN {
      case (((result, o), comparisons, _, measures), (dup, dupComparisons, _, _), (cut, prefixComparisons, _, _)) =>
        val v = get(result)
        val lookup = dupComparisons - prefixComparisons
        within("parse", N, comparisons, 2L * nLogN(N))
          .and(expect.same(N.toLong, measures))
          .and(ordered("parse", v, o))
          .and(
            expect(dup == Left(CanonicalError.DuplicateKey(Preview.of(keys(N / 2)))), s"the repeat is found: ${dup.leftMap(_.toString)}")
          )
          .and(expect(cut.isLeft, "the cut input ends before a name"))
          // the repeat ends the parse before any map is built: only the check of the names read so far compared them
          .and(within("the names before the repeat", N, prefixComparisons, 2L * nLogN(N)))
          // and the repeat itself is found by one descent of the names read so far, never by scanning them
          .and(expect(lookup >= 1L && lookup <= 2L * log2(N) + 1L, s"finding the repeat among $N names made $lookup comparisons"))
    }
  }

  test("textKeyed: one insertion per key, in the order given, each key's cached weight reused: none measured") {
    def keyed(keys: Vector[String]) = counting(o => (CanonicalValue.textKeyed(keys.map(k => t(k) -> CInt(1)), o), o))
    (keyed(colliding(K)), keyed(control(K))).mapN {
      case (((result, o), comparisons, _, measures), (_, plainComparisons, _, _)) =>
        within("textKeyed", N, comparisons, nLogN(N))
          .and(expect.same(inserting(colliding(K)), comparisons))
          .and(expect.same(inserting(control(K)), plainComparisons))
          .and(expect.same(0L, measures))
          .and(ordered("textKeyed", get(result), o))
    }
  }

  test("updated and removed: O(log n) comparisons, each reading O(the key), the key never measured again") {
    val keys = colliding(K)
    IO(new Counting).flatMap { o =>
      IO {
        val m = get(CanonicalValue.map(keys.map(_ -> CInt(1)), o))
        val (c0, u0, m0) = o.counts
        val added = t(("BB" * K).updated(0, 'C'))
        val updates = Vector(added, t(keys(7)), t(keys(N - 1)))
        val (after, costs) = updates.foldLeft((m, Vector.empty[(Long, Long, Long)])) {
          case ((acc, cs), k) =>
            val before = o.counts
            val next = get(CanonicalValue.updated(acc, k, CInt(2)))
            val removed = CanonicalValue.removed(next, t(keys(N / 3)))
            val now = o.counts
            (removed, cs :+ ((now._1 - before._1, now._2 - before._2, now._3 - before._3)))
        }
        // a lookup and an insertion, then a lookup and a deletion: each one descent of a tree at most 2 log2(n + 2) high
        val perOp = 4L * (2L * log2(N + 2) + 1L)
        val perUnit = 2L * K + 1L
        costs
          .foldLeft(success) {
            case (acc, (comparisons, units, measures)) =>
              acc
                .and(expect(comparisons >= log2(N), s"an update and a removal made $comparisons comparisons; a hashed path makes none"))
                .and(expect(comparisons <= perOp, s"an update and a removal made $comparisons comparisons, above $perOp"))
                .and(expect(units <= comparisons * perUnit, s"$units units read in $comparisons comparisons of ${2 * K}-unit keys"))
                .and(expect.same(0L, measures))
          }
          .and(expect.same(N.toLong, m0))
          .and(expect(c0 > 0L && u0 > 0L, "building the map compared keys"))
          .and(ordered("updated", after, o))
          .and(expect.same(N, keysOf(after).size))
      }
    }
  }

  test("merged: the union of its maps' trees, in O(n log n) comparisons, each key of the result measured once") {
    // interleaved halves, so the union compares throughout, and a repeat of a few keys
    def parts(keys: Vector[String]) = {
      val (evens, odds) = keys.zipWithIndex.partition(_._2 % 2 == 0)
      Vector(evens.map(_._1), odds.map(_._1), keys.take(10))
    }
    def merging(keys: Vector[String]) = counting { o =>
      val maps = parts(keys).map(ks => get(CanonicalValue.map(ks.map(_ -> CInt(1)), o)))
      val (c, _, ms) = o.counts
      (CanonicalValue.merged(maps, o), o, c, ms)
    }
    (merging(colliding(K)), merging(control(K))).mapN {
      case (((m, o, c0, m0), comparisons, _, measures), ((_, _, p0, _), plain, _, _)) =>
        expect(comparisons - c0 <= 2L * nLogN(N), s"merged: ${comparisons - c0} comparisons for $N keys")
          .and(expect.same(unioning(parts(colliding(K))), comparisons - c0))
          .and(expect.same(unioning(parts(control(K))), plain - p0))
          .and(expect.same(N.toLong, measures - m0))
          .and(ordered("merged", m, o))
          .and(expect.same(N, keysOf(m).size))
    }
  }

  test("equality, typed decoding and record checks compare keys in the map's own order") {
    val keys = colliding(K)
    IO(new Counting).flatMap { o =>
      IO {
        val m = get(CanonicalValue.map(keys.map(_ -> CInt(1)), o))
        val same = get(CanonicalValue.map(keys.map(_ -> CInt(1)), o))
        val other = get(CanonicalValue.map(keys.updated(5, "x").map(_ -> CInt(1)), o))
        def cost[A](f: => A): (A, Long) = {
          val before = o.comparisons.get
          val a = f
          (a, o.comparisons.get - before)
        }
        val (equal, equality) = cost(m == same && m != other)
        val (decoded, decoding) = cost(Codec.textMap(Codec.uint).decoder.decode(m))
        val plain = get(CanonicalValue.map(control(K).map(_ -> CInt(1)), o))
        val (_, plainDecoding) = cost(Codec.textMap(Codec.uint).decoder.decode(plain))
        val (record, recording) = cost(CanonicalDecoder.record(Set("a", "b"))(f => f.optional[BigInt]("a")).decode(m))
        val (alternative, choosing) =
          cost(Codec.selectAlternative(m, List((Set("a"), Set("a", "b"), CanonicalDecoder.record(Set("a", "b"))(_ => 1.asRight)))))
        expect(equal, "equal maps are equal, and a different key is not")
          // two trees in one order are walked side by side, their keys compared by `equals`: the order is barely asked
          .and(expect(equality <= 2L * log2(N), s"equality made $equality comparisons"))
          .and(expect(decoded.map(_.size) == Right(N), s"textMap decodes: ${decoded.map(_.size)}"))
          .and(expect(decoded.exists(e => CanonicalEntries.ordering(e) eq o), "textMap decodes in the order of the map it reads"))
          .and(within("textMap", N, decoding, nLogN(N)))
          // decoded entries are inserted in the map's order: a hashed step on the way would reorder the control's keys
          .and(expect.same(inserting(keysOf(m)), decoding))
          .and(expect.same(inserting(keysOf(plain)), plainDecoding))
          .and(expect(record.isLeft && alternative.isLeft, "4096 unknown keys are refused"))
          // the refusal names the unknown keys: each is looked up among the allowed ones, in the map's order
          .and(within("record", N, recording, 4L * nLogN(N)))
          .and(expect(choosing <= 4L * log2(N), s"choosing an alternative made $choosing comparisons"))
      }
    }
  }

  pureTest("measure is a key's UTF-8 length, and -1 when it holds an unpaired surrogate") {
    val keys = List("", "a", "é", "中", "😀", "a😀é中", "a\ud800", "\udc00a", "\ud800\ud800", "\ud800")
    expect.same(List(0L, 1L, 2L, 3L, 4L, 10L, -1L, -1L, -1L, -1L), keys.map(KeyOrder.Default.measure))
  }
}
