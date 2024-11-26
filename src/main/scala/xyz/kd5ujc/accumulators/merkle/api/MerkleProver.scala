package xyz.kd5ujc.accumulators.merkle.api

import cats.Monad
import cats.implicits.toTraverseOps
import xyz.kd5ujc.accumulators.merkle.MerkleInclusionProof.Side
import xyz.kd5ujc.accumulators.merkle.{MerkleInclusionProof, MerkleNode, MerkleTree}
import xyz.kd5ujc.hash.Digest

import scala.annotation.tailrec

trait MerkleProver[F[_]] {
  def fromLeafNode(leaf: MerkleNode.Leaf): F[Option[MerkleInclusionProof]]

  def fromLeafDigest(digest: Digest): F[Option[MerkleInclusionProof]]
}

object MerkleProver {
  def make[F[_]: Monad](tree: MerkleTree): MerkleProver[F] =
    new MerkleProver[F] {
      def fromLeafNode(leaf: MerkleNode.Leaf): F[Option[MerkleInclusionProof]] =
        fromLeafDigest(leaf.digest)

      def fromLeafDigest(digest: Digest): F[Option[MerkleInclusionProof]] =
        tree.leafDigestIndex.get(digest).flatTraverse(proofByIndex)

      private def proofByIndex(index: Int): F[Option[MerkleInclusionProof]] = {

        // bitwise shift operation to calculate log2 by counting number of divisions by 2
        @tailrec
        def log2(n: Int, acc: Int = 0): Int =
          if (n <= 1) acc
          else log2(n >> 1, acc + 1)

        val maxDepth = log2(tree.leafDigestIndex.size)

        @tailrec
        def loop(
          node:  MerkleNode,
          acc:   Seq[Option[(Digest, Side)]],
          depth: Int
        ): Option[(MerkleNode.Leaf, Seq[Option[(Digest, Side)]])] =
          node match {
            case MerkleNode.Internal(left, right, _) =>
              if (((index >> (maxDepth - depth)) & 1) == 0) {
                loop(left, right.map(_.digest).map((_, MerkleInclusionProof.rightSide)) +: acc, depth + 1)
              } else {
                loop(right.get, Some((left.digest, MerkleInclusionProof.leftSide)) +: acc, depth + 1)
              }

            case n: MerkleNode.Leaf =>
              Some((n, acc))
          }

        Monad[F].pure(
          if (index < 0 || index >= tree.leafDigestIndex.size) None
          else {
            loop(tree.rootNode, Seq(), 0).flatMap {
              case (leaf, witness) =>
                witness.sequence.map(MerkleInclusionProof(leaf.digest, _))
            }
          }
        )
      }
    }
}
