package xyz.kd5ujc.accumulators.versioned

import java.util.concurrent.atomic.AtomicLong

import scala.collection.immutable.TreeMap

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.storage.{Batch, Op}

import weaver.SimpleIOSuite

/**
 * A state key is a string, and Java's `String.hashCode` is trivially collidable (every combination of "ak" and "c-"
 * blocks has one hash code), so a view's states are sorted maps. Nothing here measures time: the states compare keys
 * through a counting order, and diffing them stays within O(n log n) comparisons.
 */
object ProjectionDiffSuite extends SimpleIOSuite {

  private val K = 10

  private def key(s: String): StateKey = StateKey.parse(s).fold(e => sys.error(e.toString), identity)

  /** `2^K` keys whose strings share one Java hash code. */
  private val keys: Vector[StateKey] =
    (1 to K).foldLeft(Vector(""))((acc, _) => acc.flatMap(s => Vector(s + "ak", s + "c-"))).map(s => key(s"k/k$s"))

  final private class Counting extends Ordering[StateKey] {
    val comparisons: AtomicLong = new AtomicLong
    def compare(a: StateKey, b: StateKey): Int = { comparisons.incrementAndGet(); StateKey.stateKeyOrdering.compare(a, b) }
  }

  /** `ceil(log2 (n + 1))`. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  pureTest("diff: O(n log n) comparisons over colliding keys, and exactly the removes and puts between the states") {
    val order = new Counting
    val prev = TreeMap.from(keys.zipWithIndex.map { case (k, i) => k -> CanonicalValue.int(i) })(using order)
    // a third removed, a third changed, a third kept, and as many new keys
    val next = TreeMap.from(keys.zipWithIndex.collect {
      case (k, i) if i % 3 == 1 => k -> CanonicalValue.int(-i)
      case (k, i) if i % 3 == 2 => k -> CanonicalValue.int(i)
    } ++ keys.map(k => key(s"${k.value}/new") -> CanonicalValue.int(0)))(using order)
    val before = order.comparisons.get
    val batch = Projection.diff(prev, next)
    val comparisons = order.comparisons.get - before
    val n = prev.size + next.size
    val removed = keys.zipWithIndex.collect { case (k, i) if i % 3 == 0 => k }.sorted
    val changed = keys.zipWithIndex.collect { case (k, i) if i % 3 == 1 => k } ++ keys.map(k => key(s"${k.value}/new"))
    expect(keys.map(_.value.hashCode).distinct.size == 1, "the keys' strings share one hash code")
      .and(expect(comparisons >= n.toLong, s"$comparisons comparisons for $n entries; a hashed lookup makes none"))
      .and(expect(comparisons <= 2L * n * (2L * log2(n) + 1L), s"$comparisons comparisons for $n entries"))
      .and(expect.same(removed.toList, batch.ops.collect { case Op.Delete(k) => k }.toList))
      .and(expect.same(changed.sorted.toList, batch.ops.collect { case Op.Put(k, _) => k }.toList))
      .and(expect.same(Batch.empty[StateKey, CanonicalValue], Projection.diff(prev, prev)))
  }
}
