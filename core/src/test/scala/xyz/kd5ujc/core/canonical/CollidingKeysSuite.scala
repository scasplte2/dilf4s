package xyz.kd5ujc.core.canonical

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.syntax.all._

import scala.annotation.nowarn

import xyz.kd5ujc.core.canonical.CanonicalValue._

import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for canonical maps. Java's `String.hashCode` is trivially collidable (every combination of
 * "Aa" and "BB" blocks has one hash code), and a hashed map degrades to linear scans on colliding keys. A map is a
 * `TreeMap` in [[KeyOrder]] instead. Nothing here measures time: a map built through a counting [[KeyOrder]] reports the
 * comparisons, the key units they read and the keys it measured, bounded by the input's size; and each `String` caches
 * its hash code once computed, so a key whose cache is still empty afterwards was never hashed, by anything.
 */
object CollidingKeysSuite extends SimpleIOSuite {

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:      String): CText = get(CanonicalValue.text(s))

  /** `2^k` distinct keys of `2k` units with one Java hash code: every combination of "Aa" and "BB". */
  private def colliding(k: Int): Vector[String] = combinations(k, "Aa", "BB")

  /** The control: as many keys, as long, from "Aa" and "Ac", whose hash codes differ. */
  private def control(k: Int): Vector[String] = combinations(k, "Aa", "Ac")

  // each key a fresh String, so no hash code is cached in it yet
  private def combinations(k: Int, a: String, b: String): Vector[String] =
    (1 to k).foldLeft(Vector(""))((acc, _) => acc.flatMap(s => Vector(s + a, s + b))).map(s => new String(s.toCharArray))

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
   * Whether `String.hashCode` was ever called on `s`: reads its cached hash, through `sun.misc.Unsafe` because `java.lang`
   * is not open to reflection. A test-only probe: if a JDK ever withdraws the call, the suite fails rather than passes.
   */
  private object HashProbe {
    private val unsafe: sun.misc.Unsafe = {
      val f = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
      f.setAccessible(true)
      f.get(null).asInstanceOf[sun.misc.Unsafe]
    }
    @nowarn("cat=deprecation")
    private val offset: Long = unsafe.objectFieldOffset(classOf[String].getDeclaredField("hash"))
    def hashed(s: String): Boolean = unsafe.getInt(s, offset) != 0
  }

  /** The hash code `String.hashCode` would compute, without computing it there (so nothing is cached). */
  private def javaHash(s: String): Int = s.foldLeft(0)((h, c) => 31 * h + c)

  private def neverHashed(what: String, keys: Iterable[String]): Expectations = {
    val hashed = keys.count(HashProbe.hashed)
    expect(hashed == 0, s"$what: $hashed of ${keys.size} keys were hashed")
  }

  private def keysOf(v: CanonicalValue): Vector[String] = v match {
    case CMap(m) => m.keysIterator.toVector
    case _       => Vector.empty
  }

  private def ordered(what: String, v: CanonicalValue, keys: KeyOrder): Expectations = v match {
    case CMap(m) => expect(m.ordering eq keys, s"$what: the entries are ordered by ${m.ordering}, not the order passed")
    case other   => failure(s"$what: not a map: $other")
  }

  private def within(what: String, n: Int, comparisons: Long, upper: Long): Expectations =
    expect(comparisons >= n - 1L, s"$what: $comparisons comparisons for $n keys; a hashed path makes none")
      .and(expect(comparisons <= upper, s"$what: $comparisons comparisons for $n keys, above $upper"))

  test("the probe sees a hash code once one is computed, and every colliding key really collides") {
    IO {
      val keys = colliding(K)
      val probe = new String("probe".toCharArray)
      val before = HashProbe.hashed(probe)
      val _ = probe.hashCode
      expect(!before && HashProbe.hashed(probe), "the probe reads String's cached hash code")
        .and(expect.same(1, keys.map(javaHash).distinct.size))
        .and(expect(javaHash(keys.head) != 0, "a zero hash code would read as never computed"))
        .and(expect.same(N, keys.distinct.size))
        .and(expect(control(K).map(javaHash).distinct.size > N / 2, "the control's keys mostly differ in hash code"))
    }
  }

  test("map: one insertion per key in O(n log n) comparisons, each key measured once, none hashed") {
    val keys = colliding(K)
    counting(o => CanonicalValue.map(keys.map(_ -> CInt(1)), o).map(m => (m, o))).map {
      case (result, comparisons, _, measures) =>
        val m = get(result)._1
        within("map", N, comparisons, nLogN(N))
          .and(expect.same(N.toLong, measures))
          .and(ordered("map", m, get(result)._2))
          .and(neverHashed("map", keys))
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

  test("decode: O(n log n) comparisons, no key measured again or hashed, in the order passed") {
    val bytes = Cbor.encode(get(CanonicalValue.map(colliding(K).map(_ -> CInt(1)))))
    counting(o => (Cbor.decode(bytes, Cbor.DefaultMaxDepth, o), o)).map {
      case ((result, o), comparisons, _, measures) =>
        val v = get(result)
        within("decode", N, comparisons, nLogN(N))
          .and(expect.same(0L, measures))
          .and(ordered("decode", v, o))
          .and(neverHashed("decode", keysOf(v)))
          .and(expect.same(bytes, Cbor.encode(v)))
    }
  }

  test("JSON: repeated member names are found in O(n log n) comparisons, and no member name is hashed") {
    val keys = colliding(K)
    val json = keys.map(k => s"\"$k\":1").mkString("{", ",", "}")
    val repeated = keys.map(k => s"\"$k\":1").appended(s"\"${keys(N / 2)}\":2").mkString("{", ",", "}")
    (counting(o => (CanonicalJson.parse(json, 0, o), o)), counting(o => CanonicalJson.parse(repeated, 0, o))).mapN {
      case (((result, o), comparisons, _, measures), (dup, dupComparisons, _, _)) =>
        val v = get(result)
        within("parse", N, comparisons, 2L * nLogN(N))
          .and(expect.same(N.toLong, measures))
          .and(ordered("parse", v, o))
          .and(neverHashed("parse", keysOf(v)))
          .and(
            expect(dup == Left(CanonicalError.DuplicateKey(Preview.of(keys(N / 2)))), s"the repeat is found: ${dup.leftMap(_.toString)}")
          )
          .and(expect(dupComparisons <= 2L * nLogN(N + 1), s"$dupComparisons comparisons to find the repeat"))
    }
  }

  test("textKeyed: O(n log n) comparisons, each key's cached weight reused: no key measured or hashed") {
    val keys = colliding(K).map(t)
    counting(o => (CanonicalValue.textKeyed(keys.map(_ -> CInt(1)), o), o)).map {
      case ((result, o), comparisons, _, measures) =>
        within("textKeyed", N, comparisons, nLogN(N))
          .and(expect.same(0L, measures))
          .and(ordered("textKeyed", get(result), o))
          .and(neverHashed("textKeyed", keys.map(_.value)))
    }
  }

  test("updated and removed: O(log n) comparisons, each reading O(the key), the key never measured again or hashed") {
    val keys = colliding(K)
    IO(new Counting).flatMap { o =>
      IO {
        val m = get(CanonicalValue.map(keys.map(_ -> CInt(1)), o))
        val (c0, u0, m0) = o.counts
        val added = t(new String(("BB" * K).updated(0, 'C').toCharArray))
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
                .and(expect(comparisons <= perOp, s"an update and a removal made $comparisons comparisons, above $perOp"))
                .and(expect(units <= comparisons * perUnit, s"$units units read in $comparisons comparisons of ${2 * K}-unit keys"))
                .and(expect.same(0L, measures))
          }
          .and(expect.same(N.toLong, m0))
          .and(expect(c0 > 0L && u0 > 0L, "building the map compared keys"))
          .and(ordered("updated", after, o))
          .and(expect.same(N, keysOf(after).size))
          .and(neverHashed("updated", keysOf(after) ++ updates.map(_.value)))
      }
    }
  }

  test("merged: O(n log n) comparisons, each key of the result measured once, none hashed") {
    val (left, right) = colliding(K).splitAt(N / 2)
    counting { o =>
      val maps = Vector(left, right, left.take(10)).map(ks => get(CanonicalValue.map(ks.map(_ -> CInt(1)), o)))
      val (c, _, ms) = o.counts
      (CanonicalValue.merged(maps, o), o, c, ms)
    }.map {
      case ((m, o, c0, m0), comparisons, _, measures) =>
        // a union of disjoint key ranges compares little, so only the upper bound applies
        expect(comparisons - c0 <= 2L * nLogN(N), s"merged: ${comparisons - c0} comparisons for $N keys")
          .and(expect.same(N.toLong, measures - m0))
          .and(ordered("merged", m, o))
          .and(expect.same(N, keysOf(m).size))
          .and(neverHashed("merged", keysOf(m)))
    }
  }

  test("equality, typed decoding and record shapes look keys up in the map's order: none is hashed") {
    val keys = colliding(K)
    val m = get(CanonicalValue.map(keys.map(_ -> CInt(1))))
    val same = get(CanonicalValue.map(colliding(K).map(_ -> CInt(1))))
    val other = get(CanonicalValue.map(colliding(K).updated(5, new String("x".toCharArray)).map(_ -> CInt(1))))
    val decoded = Codec.textMap(Codec.uint).decoder.decode(m)
    val record = CanonicalDecoder.record(Set("a", "b"))(f => f.optional[BigInt]("a")).decode(m)
    val alternative = Codec.selectAlternative(m, List((Set("a"), Set("a", "b"), CanonicalDecoder.record(Set("a", "b"))(_ => 1.asRight))))
    IO(
      expect(m == same && m != other, "equal maps are equal, and a different key is not")
        .and(expect(decoded.map(_.size) == Right(N), s"textMap decodes: ${decoded.map(_.size)}"))
        .and(expect(decoded.exists(_.ordering eq KeyOrder.Default), "textMap decodes in the default key order"))
        .and(expect(record.isLeft && alternative.isLeft, "4096 unknown keys are refused"))
        .and(neverHashed("equality, textMap, record and selectAlternative", keys ++ keysOf(same) ++ keysOf(other)))
    )
  }

  pureTest("measure is a key's UTF-8 length, and -1 when it holds an unpaired surrogate") {
    val keys = List("", "a", "é", "中", "😀", "a😀é中", "a\ud800", "\udc00a", "\ud800\ud800", "\ud800")
    expect.same(List(0L, 1L, 2L, 3L, 4L, 10L, -1L, -1L, -1L, -1L), keys.map(KeyOrder.Default.measure))
  }
}
