package xyz.kd5ujc.core.canonical

import scala.annotation.tailrec

/**
 * Code-point order on text, the order of its UTF-8 bytes alone: the order of JSON object members
 * (docs/specs/canonical-encoding.md), of the JLVM's `keys`, `values` and `entries` (docs/specs/jlvm.md §5.4), and,
 * among keys of one UTF-8 length, of a CBOR map's encoded keys. A JVM string holds UTF-16 units, and it is the
 * lexicographic order of their [[rank]]s, so [[sorted]] puts a map's entries in it by merging rather than by a
 * comparison sort, in time linear in the map's weight.
 */
private[kd5ujc] object CodePointOrder extends Ordering[String] {

  /**
   * UTF-16 units ranked so that comparing them at the first difference gives code-point order (ICU's fix-up): only a
   * surrogate, which starts a code point above U+FFFF, can meet a unit in U+E000..U+FFFF out of order, so surrogates
   * move above that range. The ranks are a permutation of the units.
   */
  def rank(c: Char): Int = if (c < 0xd800) c.toInt else if (c < 0xe000) c + 0x2000 else c - 0x800

  def compare(a: String, b: String): Int = {
    val l = common(a, b, 0)
    if (l < a.length && l < b.length) Integer.compare(rank(a.charAt(l)), rank(b.charAt(l))) else Integer.compare(a.length, b.length)
  }

  /** The length of the common prefix of `a` and `b`, which agree on their first `from` units. */
  private def common(a: String, b: String, from: Int): Int = {
    val n = math.min(a.length, b.length)
    @tailrec
    def go(i: Int): Int = if (i < n && a.charAt(i) == b.charAt(i)) go(i + 1) else i
    go(from)
  }

  /** The pairs of units read finding that `a` and `b` share `l` units from `from`: those that match, and the one that differs unless either ends first. */
  private def read(a: String, b: String, from: Int, l: Int): Long =
    (l - from).toLong + (if (l < a.length && l < b.length) 1L else 0L)

  /** Whether `a`, which shares its first `l` units with `b`, comes first: keys are distinct, and a prefix comes first. */
  private def before(a: String, b: String, l: Int): Boolean =
    l == a.length || (l < b.length && rank(a.charAt(l)) < rank(b.charAt(l)))

  /** `entries`, whose keys are distinct, in code-point order of their keys, in time linear in their weight ([[counted]]). */
  def sorted[A](entries: Vector[(String, A)]): Vector[(String, A)] = counted(entries)._1

  /**
   * [[sorted]], and the pairs of units it read. The entries are cut into their maximal runs already in code-point
   * order, each with every key's common prefix with the one before it, and the runs are merged, pairwise in rounds, by
   * [[merge]]. A map's entries come in UTF-16 order, which is code-point order unless a surrogate meets a unit in
   * U+E000..U+FFFF at a key's first difference from the next, so they are usually one run, found in one pass.
   *
   * Linear in the weight `W` of the map they come from, whatever the keys. With `n` keys of `S` UTF-8 bytes in all, and
   * at most `S` UTF-16 units, in `k` runs: finding the runs and their prefixes reads at most `S + n` pairs (the prefix
   * a key shares with the one before it is no longer than the key, and then one pair differs). The rounds output
   * `n ceil(log2 k)` entries, each after at most one pair of units that differ; and every pair that matches raises one
   * entry's common prefix with its predecessor, which no round lowers, so each key's units match at most once over all
   * rounds (`S`). `n` distinct keys are `n` distinct byte strings in UTF-8, which have `n log2 n < 8 S + 2.45 n` (fewer
   * than `2 * 256^L` strings are `L` bytes or shorter), and every entry adds at least `2 +` its key's UTF-8 length to `W`
   * (`W > S + 2 n`), so the pairs read are at most `2 S + n (ceil(log2 k) + 1) <= 2 S + n (log2 n + 2) < 10 W`, and so
   * are the entries moved. One run is the answer as it is, after the one pass.
   *
   * The positions and prefixes being merged are held in arrays local to this call, which never escape it: a round
   * writes each entry's position once, and only the finished order leaves, as a vector.
   */
  def counted[A](entries: Vector[(String, A)]): (Vector[(String, A)], Long) =
    if (entries.sizeIs <= 1) (entries, 0L) else runs(entries)

  /** [[counted]] of two entries or more. */
  private def runs[A](entries: Vector[(String, A)]): (Vector[(String, A)], Long) = {
    val n = entries.size
    val keys = entries.iterator.map(_._1).toArray
    // lcp(i): key i's common prefix with key i - 1 in its run; 0 where a run starts
    val lcp = new Array[Int](n)
    @tailrec
    def scan(i: Int, starts: List[Int], reads: Long): (List[Int], Long) =
      if (i >= n) (starts, reads)
      else {
        val (a, b) = (keys(i - 1), keys(i))
        val l = common(a, b, 0)
        val ascending = before(a, b, l)
        lcp(i) = if (ascending) l else 0
        scan(i + 1, if (ascending) starts else i :: starts, reads + read(a, b, 0, l))
      }
    val (starts, scanned) = scan(1, List(0), 0L)
    if (starts.sizeIs <= 1) (entries, scanned)
    else {
      val (order, reads) =
        rounds(keys, Array.range(0, n), lcp, new Array[Int](n), new Array[Int](n), (n :: starts).reverse.toVector, scanned)
      (order.iterator.map(entries).toVector, reads)
    }
  }

  /**
   * The runs of `order` starting at `bounds` (then `n`), merged pairwise into `to` until one is left; the buffers
   * change places each round. The order, and the pairs read.
   */
  @tailrec
  private def rounds(
    keys:   Array[String],
    order:  Array[Int],
    lcp:    Array[Int],
    to:     Array[Int],
    toLcp:  Array[Int],
    bounds: Vector[Int],
    reads:  Long
  ): (Array[Int], Long) =
    if (bounds.sizeIs <= 2) (order, reads)
    else {
      @tailrec
      def pairs(r: Int, starts: List[Int], reads: Long): (List[Int], Long) =
        if (r + 2 < bounds.size)
          pairs(r + 2, bounds(r) :: starts, reads + merge(keys, order, lcp, to, toLcp, bounds(r), bounds(r + 1), bounds(r + 2)))
        else if (r + 1 < bounds.size) {
          // a run left over: carried into the next round as it is
          val (from, until) = (bounds(r), bounds(r + 1))
          System.arraycopy(order, from, to, from, until - from)
          System.arraycopy(lcp, from, toLcp, from, until - from)
          (from :: starts, reads)
        } else (starts, reads)
      val (starts, merged) = pairs(0, Nil, reads)
      rounds(keys, to, toLcp, order, lcp, (order.length :: starts).reverse.toVector, merged)
    }

  /**
   * The runs `[a0, a1)` and `[a1, b1)` of `order` merged into `to` from `a0`, by their common prefixes (Ng and Kakehi's
   * LCP merge, "Merging String Sequences by Longest Common Prefixes", IPSJ Digital Courier 4, 2008); the pairs read.
   * With `o` the last entry output, `ha` and `hb` are each head's common prefix with `o`. The head sharing more with
   * `o` comes first, read without a unit: both follow `o`, and the other differs from `o` earlier, upward. Only when
   * they share as much is a unit read, from that common length on, and the one not taken then shares exactly the units
   * read with the new `o`. So a unit that matches raises an entry's common prefix with the entry before it, which never
   * falls from one merge to the next, and is read once per entry over every merge; a unit that differs is read once
   * per entry output.
   */
  private def merge(keys: Array[String], order: Array[Int], lcp: Array[Int], to: Array[Int], toLcp: Array[Int], a0: Int, a1: Int, b1: Int)
    : Long = {
    def put(o:     Int, at:  Int, h: Int): Unit = { to(o) = order(at); toLcp(o) = h }
    def prefix(at: Int, end: Int): Int = if (at < end) lcp(at) else 0
    // the rest of one run follows: its head shares h with the last entry output, each after it its own prefix
    def rest(at: Int, end: Int, o: Int, h: Int): Unit = {
      System.arraycopy(order, at, to, o, end - at)
      System.arraycopy(lcp, at, toLcp, o, end - at)
      if (at < end) toLcp(o) = h
    }
    @tailrec
    def go(i: Int, j: Int, ha: Int, hb: Int, o: Int, reads: Long): Long =
      if (i >= a1) { rest(j, b1, o, hb); reads }
      else if (j >= b1) { rest(i, a1, o, ha); reads }
      else if (ha > hb) { put(o, i, ha); go(i + 1, j, prefix(i + 1, a1), hb, o + 1, reads) }
      else if (hb > ha) { put(o, j, hb); go(i, j + 1, ha, prefix(j + 1, b1), o + 1, reads) }
      else {
        val (x, y) = (keys(order(i)), keys(order(j)))
        val l = common(x, y, ha)
        val r = reads + read(x, y, ha, l)
        if (before(x, y, l)) { put(o, i, ha); go(i + 1, j, prefix(i + 1, a1), l, o + 1, r) }
        else { put(o, j, ha); go(i, j + 1, l, prefix(j + 1, b1), o + 1, r) }
      }
    go(a0, a1, 0, 0, a0, 0L)
  }
}
