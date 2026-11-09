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
   * Runs retain entries and their common prefixes in immutable lists. Each merge prepends its output and reverses it
   * once, so list construction adds only constant work per entry moved.
   */
  def counted[A](entries: Vector[(String, A)]): (Vector[(String, A)], Long) =
    entries.toList match {
      case Nil => (entries, 0L)
      case first :: tail =>
        val (runs, reads) = scan(tail, first, List(Item(first, 0)), Nil, 0L)
        if (runs.sizeIs <= 1) (entries, reads)
        else {
          val (ordered, total) = rounds(runs, reads)
          (ordered.map(_.entry).toVector, total)
        }
    }

  private final case class Item[A](entry: (String, A), lcp: Int)

  @tailrec
  private def scan[A](
    remaining: List[(String, A)],
    previous:  (String, A),
    reversed:  List[Item[A]],
    runs:      List[List[Item[A]]],
    reads:     Long
  ): (List[List[Item[A]]], Long) = remaining match {
    case Nil => ((reversed.reverse :: runs).reverse, reads)
    case next :: tail =>
      val (a, b) = (previous._1, next._1)
      val l = common(a, b, 0)
      val total = reads + read(a, b, 0, l)
      if (before(a, b, l)) scan(tail, next, Item(next, l) :: reversed, runs, total)
      else scan(tail, next, List(Item(next, 0)), reversed.reverse :: runs, total)
  }

  @tailrec
  private def rounds[A](runs: List[List[Item[A]]], reads: Long): (List[Item[A]], Long) = runs match {
    case Nil         => (Nil, reads)
    case only :: Nil => (only, reads)
    case _ =>
      val (next, total) = pairs(runs, Nil, reads)
      rounds(next, total)
  }

  @tailrec
  private def pairs[A](
    runs:     List[List[Item[A]]],
    reversed: List[List[Item[A]]],
    reads:    Long
  ): (List[List[Item[A]]], Long) = runs match {
    case left :: right :: tail =>
      val (merged, total) = merge(left, right, 0, 0, Nil, reads)
      pairs(tail, merged :: reversed, total)
    case last :: Nil => ((last :: reversed).reverse, reads)
    case Nil         => (reversed.reverse, reads)
  }

  /**
   * LCP merge (Ng and Kakehi, "Merging String Sequences by Longest Common Prefixes", 2008). Each head shares
   * `ha` or `hb` units with the last output. The greater prefix comes first without rereading it. Equal prefixes
   * compare from that offset; the other head retains the extended prefix for its next comparison.
   */
  @tailrec
  private def merge[A](
    left:     List[Item[A]],
    right:    List[Item[A]],
    ha:       Int,
    hb:       Int,
    reversed: List[Item[A]],
    reads:    Long
  ): (List[Item[A]], Long) = (left, right) match {
    case (Nil, head :: tail) => (reversed.reverse ::: head.copy(lcp = hb) :: tail, reads)
    case (head :: tail, Nil) => (reversed.reverse ::: head.copy(lcp = ha) :: tail, reads)
    case (Nil, Nil)          => (reversed.reverse, reads)
    case (a :: as, b :: bs) =>
      if (ha > hb) merge(as, right, as.headOption.fold(0)(_.lcp), hb, a.copy(lcp = ha) :: reversed, reads)
      else if (hb > ha) merge(left, bs, ha, bs.headOption.fold(0)(_.lcp), b.copy(lcp = hb) :: reversed, reads)
      else {
        val (x, y) = (a.entry._1, b.entry._1)
        val l = common(x, y, ha)
        val total = reads + read(x, y, ha, l)
        if (before(x, y, l)) merge(as, right, as.headOption.fold(0)(_.lcp), l, a.copy(lcp = ha) :: reversed, total)
        else merge(left, bs, l, bs.headOption.fold(0)(_.lcp), b.copy(lcp = hb) :: reversed, total)
      }
  }
}
