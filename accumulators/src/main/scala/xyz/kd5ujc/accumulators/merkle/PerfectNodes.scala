package xyz.kd5ujc.accumulators.merkle

import cats.Monad
import cats.syntax.all._

import xyz.kd5ujc.core.hash.Digest

/**
 * RFC 9162 tree hashes and proofs computed from **perfect** subtrees only: `perfect(level, index)` is the hash of the
 * complete block of `2^level` leaves starting at `index * 2^level`. Every MTH of a range splits into aligned perfect
 * blocks, so a store never needs the partial nodes on the right edge, and appends never rewrite stored nodes.
 * Recursion depth is the tree height (at most 64).
 */
object PerfectNodes {

  type Perfect[F[_], H] = (Int, Long) => F[Digest[H]]

  /** MTH of `length >= 1` leaves starting at `start`. */
  def subtree[F[_]: Monad, H](perfect: Perfect[F, H], start: Long, length: Long)(implicit M: MerkleHashing[H]): F[Digest[H]] =
    if (java.lang.Long.bitCount(length) == 1 && start % length == 0)
      perfect(java.lang.Long.numberOfTrailingZeros(length), start / length)
    else {
      val k = MerkleTree.splitPoint(length)
      (subtree(perfect, start, k), subtree(perfect, start + k, length - k)).mapN(M.node)
    }

  def root[F[_]: Monad, H](perfect: Perfect[F, H], size: Long)(implicit M: MerkleHashing[H]): F[Digest[H]] =
    if (size == 0) M.empty.pure[F] else subtree(perfect, 0L, size)

  /** RFC 9162 §2.1.3.1 PATH(m, D[start : start + n]), leaf to root. */
  def auditPath[F[_]: Monad, H](perfect: Perfect[F, H], m: Long, start: Long, n: Long)(implicit M: MerkleHashing[H]): F[Vector[Digest[H]]] =
    if (n <= 1) Vector.empty[Digest[H]].pure[F]
    else {
      val k = MerkleTree.splitPoint(n)
      if (m < k) (auditPath(perfect, m, start, k), subtree(perfect, start + k, n - k)).mapN(_ :+ _)
      else (auditPath(perfect, m - k, start + k, n - k), subtree(perfect, start, k)).mapN(_ :+ _)
    }

  /** RFC 9162 §2.1.4.1 SUBPROOF(m, D[start : start + n], complete). */
  def consistencyPath[F[_]: Monad, H](perfect: Perfect[F, H], m: Long, start: Long, n: Long, complete: Boolean)(implicit
    M: MerkleHashing[H]
  ): F[Vector[Digest[H]]] =
    if (m == n) (if (complete) Vector.empty[Digest[H]].pure[F] else subtree(perfect, start, n).map(Vector(_)))
    else {
      val k = MerkleTree.splitPoint(n)
      if (m <= k) (consistencyPath(perfect, m, start, k, complete), subtree(perfect, start + k, n - k)).mapN(_ :+ _)
      else (consistencyPath(perfect, m - k, start + k, n - k, complete = false), subtree(perfect, start, k)).mapN(_ :+ _)
    }

  /** The perfect nodes completed when leaf `index` is placed: `(level, blockIndex)` pairs above level 0, lowest first. */
  def completedBy(index: Long): List[(Int, Long)] =
    LazyList
      .from(1)
      .takeWhile(level => level < 63 && ((index + 1) % (1L << level)) == 0)
      .map(level => (level, (index + 1) / (1L << level) - 1))
      .toList

  /** The perfect ancestors of leaf `index` that lie fully inside a tree of `size` leaves, lowest first. */
  def ancestorsWithin(index: Long, size: Long): List[(Int, Long)] =
    LazyList
      .from(1)
      .takeWhile(level => level < 63 && (1L << level) <= size)
      .map(level => (level, index >> level))
      .filter { case (level, block) => (block + 1) * (1L << level) <= size }
      .toList
}
