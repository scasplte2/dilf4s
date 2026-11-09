package xyz.kd5ujc.accumulators.trie

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO

import xyz.kd5ujc.core.hash.{Digest, Sha256}

import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for building a trie from entries. A key's path is the caller's bytes, and a `BitVector`'s
 * hash code is a seedless MurmurHash3, so paths are only compared. Nothing here measures time: preparing the entries
 * compares paths through a counting order, at most O(n log n) times and at least once per entry (a hashed path makes
 * none).
 */
object CollidingPathsSuite extends SimpleIOSuite with Checkers {

  private val T = TrieHashing[Sha256]
  private val variable = KeyMode.variable(4).fold(e => sys.error(e.toString), identity)
  private val N = 1 << 12

  private def value(i: Int): Digest[Sha256] = T.value(ByteVector.fromInt(i))

  /** `ceil(log2 (n + 1))`. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  final private class Counting extends Ordering[BitVector] {
    val comparisons: AtomicLong = new AtomicLong
    def compare(a: BitVector, b: BitVector): Int = { comparisons.incrementAndGet(); Trie.PathOrder.compare(a, b) }
  }

  private def within(what: String, n: Int, comparisons: Long, upper: Long): Expectations =
    expect(comparisons >= n - 1L, s"$what: $comparisons comparisons for $n entries; a hashed path makes none")
      .and(expect(comparisons <= upper, s"$what: $comparisons comparisons for $n entries, above $upper"))

  /** The reference: each key's last value by a linear scan, sorted by the paths' bit strings (a prefix first). */
  private def reference(entries: List[(ByteVector, Digest[Sha256])]): Vector[(BitVector, Digest[Sha256])] =
    entries
      .map(_._1)
      .distinct
      .map(k => k.bits -> entries.findLast(_._1 == k).map(_._2).getOrElse(T.empty))
      .sortBy(_._1.toBin)
      .toVector

  test("prepare: O(n log n) comparisons, each key's last value, in path order") {
    IO {
      // keys of 0 to 4 bytes, many of them prefixes of others, then a rewrite of every seventh
      val keys = (0 until N).toList.map(i => ByteVector.fromInt(i).drop((i % 5).toLong))
      val entries = keys.zipWithIndex.map { case (k, i) => k -> value(i) } ++ keys.zipWithIndex.collect {
        case (k, i) if i % 7 == 0 => k -> value(-i)
      }
      val order = new Counting
      val prepared = Trie.prepare(variable, entries, order)
      within("prepare", entries.size, order.comparisons.get, entries.size.toLong * (log2(entries.size) + 2L))
        .and(expect.same(Right(reference(entries.take(300))), Trie.prepare(variable, entries.take(300), Trie.PathOrder)))
        .and(expect.same(Right(reference(entries).size), prepared.map(_.size)))
    }
  }

  test("prepare: a key given n times is compared on every entry, and its last value stands") {
    IO {
      val entries = List.tabulate(N)(i => ByteVector(1, 2) -> value(i))
      val order = new Counting
      val prepared = Trie.prepare(variable, entries, order)
      within("one key", N, order.comparisons.get, 3L * N)
        .and(expect.same(Right(Vector(ByteVector(1, 2).bits -> value(N - 1))), prepared))
    }
  }

  test("PathOrder is the order of the paths' bit strings, a proper prefix first") {
    val bits = Gen.choose(0, 20).flatMap(n => Gen.listOfN(n, Gen.oneOf(true, false))).map(BitVector.bits)
    implicit val show: cats.Show[(BitVector, BitVector)] = cats.Show.fromToString
    forall(Gen.zip(bits, bits)) {
      case (a, b) =>
        expect.same(Integer.signum(a.toBin.compareTo(b.toBin)), Integer.signum(Trie.PathOrder.compare(a, b)))
    }
  }

  test("fromEntries equals the trie built by updates, whatever the order and repeats") {
    IO {
      val entries = (0 until 512).toList.map(i => ByteVector.fromInt(i * 7919).drop((i % 5).toLong) -> value(i))
      val repeated = entries ++ entries.take(40).map { case (k, _) => k -> value(-1) }
      val updated = Trie.empty[Sha256](variable).update(repeated.map { case (k, v) => k -> Some(v) })
      expect.same(updated.map(_.root), Trie.fromEntries[Sha256](variable, repeated).map(_.root))
    }
  }
}
