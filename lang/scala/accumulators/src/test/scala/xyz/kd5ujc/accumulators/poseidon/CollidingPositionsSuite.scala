package xyz.kd5ujc.accumulators.poseidon

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO

import xyz.kd5ujc.core.hash.Fr

import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for the Poseidon tree. Positions are the caller's, and a `BigInt`'s hash code is trivially
 * collidable: every `(x << 32) | x` hashes to 0, as a `Long` does. A tree's maps are ordered instead. Nothing here
 * measures time: the tree compares positions through a counting order, and each map's work is bounded by its size, and
 * is at least one comparison per lookup (a hashed path makes none).
 */
object CollidingPositionsSuite extends SimpleIOSuite {

  private val N = 256
  private val Depth = 45

  /** `N` distinct positions below `2^Depth` with one hash code. */
  private val positions: Vector[BigInt] = Vector.tabulate(N)(i => (BigInt(i + 1) << 32) | BigInt(i + 1))

  private def fr(i: Int): Fr = Fr.from(BigInt(i)).fold(e => sys.error(e.toString), identity)

  /** `ceil(log2 (n + 1))`: a red-black tree of `n` entries is at most twice this high. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong)

  /** The most comparisons one lookup or insertion makes in a red-black tree of at most `n` entries. */
  private def perOp(n: Int): Long = 2L * log2(n) + 1L

  final private class Counting extends Ordering[BigInt] {
    val comparisons: AtomicLong = new AtomicLong
    def compare(a: BigInt, b: BigInt): Int = { comparisons.incrementAndGet(); a.compare(b) }
  }

  private def build(order: Counting): PoseidonMerkleTree =
    PoseidonMerkleTree
      .empty(Depth, order)
      .flatMap(_.insertAll(positions.zipWithIndex.map { case (p, i) => p -> fr(i + 1) }))
      .fold(e => sys.error(e.toString), identity)

  private def within(what: String, lower: Long, comparisons: Long, upper: Long): Expectations =
    expect(comparisons >= lower, s"$what: $comparisons comparisons, below $lower; a hashed path makes none")
      .and(expect(comparisons <= upper, s"$what: $comparisons comparisons, above $upper"))

  pureTest("the positions collide, are distinct and fit the tree") {
    expect
      .same(1, positions.map(_.##).distinct.size)
      .and(expect.same(N, positions.distinct.size))
      .and(expect(positions.forall(_ < (BigInt(1) << Depth))))
  }

  test("insertAll: O(n · depth · log n) comparisons for colliding positions, and the root of the default order") {
    IO {
      val order = new Counting
      val tree = build(order)
      val plain = PoseidonMerkleTree
        .empty(Depth)
        .flatMap(_.insertAll(positions.zipWithIndex.map { case (p, i) => p -> fr(i + 1) }))
        .map(_.root)
      // per insertion: the leaf, then at each level an update and a sibling lookup
      within("insertAll", N.toLong, order.comparisons.get, N.toLong * (1L + 2L * Depth) * perOp(N))
        .and(expect.same(Right(tree.root), plain))
    }
  }

  test("leafAt: each lookup compares in the leaves' map, O(log n) times") {
    IO {
      val order = new Counting
      val tree = build(order)
      val before = order.comparisons.get
      val found = positions.map(tree.leafAt)
      within("leafAt", N.toLong, order.comparisons.get - before, N.toLong * perOp(N))
        .and(expect.same(positions.indices.map(i => Right(fr(i + 1))).toVector, found))
    }
  }

  test("proof: one sibling lookup per level, each O(log n) comparisons, and the proof verifies") {
    IO {
      val order = new Counting
      val tree = build(order)
      val before = order.comparisons.get
      val p = positions(N / 3)
      val proof = tree.proof(p)
      // the levels below 32 hold every position's node; the ones above hold fewer, and the top ones one
      within("proof", 32L, order.comparisons.get - before, Depth.toLong * perOp(N))
        .and(expect(proof.exists(PoseidonMerkleTree.verifyInclusion(fr(N / 3 + 1), _, tree.root)), s"the proof verifies: $proof"))
    }
  }
}
