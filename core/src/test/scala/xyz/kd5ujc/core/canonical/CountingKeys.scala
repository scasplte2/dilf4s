package xyz.kd5ujc.core.canonical

import java.util.concurrent.atomic.AtomicLong

/**
 * [[KeyOrder.Default]], counting its comparisons and the key units each reads (up to and including the first
 * difference, as `compareTo` does), for tests in other modules that bound the work of map operations by counting.
 */
final class CountingKeys extends KeyOrder {
  val comparisons: AtomicLong = new AtomicLong
  val units: AtomicLong = new AtomicLong

  def compare(a: String, b: String): Int = {
    val n = math.min(a.length, b.length)
    val common = (0 until n).find(i => a.charAt(i) != b.charAt(i)).getOrElse(n)
    comparisons.incrementAndGet()
    units.addAndGet(math.min(common + 1, n).toLong)
    KeyOrder.Default.compare(a, b)
  }

  def measure(key: String): Long = KeyOrder.Default.measure(key)

  /** The map of `entries` in this order: every later update of it compares through this order too. */
  def map(entries: Iterable[(String, CanonicalValue)]): Either[CanonicalError, CanonicalValue.CMap] =
    CanonicalValue.map(entries, this)
}
