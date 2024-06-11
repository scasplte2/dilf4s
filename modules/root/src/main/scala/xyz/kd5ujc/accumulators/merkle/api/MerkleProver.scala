package xyz.kd5ujc.accumulators.merkle.api

import cats.Monad
import cats.data.OptionT
import cats.implicits.{catsSyntaxOptionId, toTraverseOps}

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.merkle.MerkleInclusionProof.Side
import xyz.kd5ujc.accumulators.merkle.nodes.{InternalNode, LeafNode}
import xyz.kd5ujc.accumulators.merkle.{MerkleInclusionProof, MerkleTree}
import xyz.kd5ujc.hash.Digest

trait MerkleProver[F[_], L] {
  def fromLeafNode[A](leaf: LeafNode[A, L]): F[Option[MerkleInclusionProof[L]]]

  def fromLeafDigest(digest: Digest[L]): F[Option[MerkleInclusionProof[L]]]
}

object MerkleProver {
  def make[F[_]: Monad, L](tree: MerkleTree[L]): MerkleProver[F, L] =
    new MerkleProver[F, L] {
      def fromLeafNode[A](leaf: LeafNode[A, L]): F[Option[MerkleInclusionProof[L]]] = fromLeafDigest(leaf.digest)

      def fromLeafDigest(digest: Digest[L]): F[Option[MerkleInclusionProof[L]]] =
        OptionT
          .fromOption[F](tree.leafDigestIndex.get(digest))
          .flatMapF(index => proofByIndex(index))
          .value

      private def proofByIndex(index: Int): F[Option[MerkleInclusionProof[L]]] = {

        // bitwise shift operation to calculate log2 by counting number of divisions by 2
        @tailrec
        def log2(n: Int, acc: Int = 0): Int =
          if (n <= 1) acc
          else log2(n >> 1, acc + 1)

        val maxDepth = log2(tree.leafDigestIndex.size)

        @tailrec
        def loop(
          node:  Option[Node[L]],
          acc:   Seq[Option[(Digest[L], Side)]],
          depth: Int
        ): Option[(LeafNode[_, L], Seq[Option[(Digest[L], Side)]])] =
          node match {
            case Some(n: InternalNode[_]) if ((index >> (maxDepth - depth)) & 1) == 0 =>
              loop(Some(n.left), n.right.map(_.digest).map((_, MerkleInclusionProof.leftSide)) +: acc, depth + 1)

            case Some(n: InternalNode[_]) =>
              loop(n.right, Some((n.left.digest, MerkleInclusionProof.rightSide)) +: acc, depth + 1)

            case Some(n: LeafNode[_, _]) =>
              Some((n, acc))

            case _ =>
              None
          }

        Monad[F].pure(
          if (index < 0 || index >= tree.leafDigestIndex.size) None
          else {
            loop(tree.rootNode.some, Seq(), 0).flatMap { lp =>
              lp._2.sequence.map(w => MerkleInclusionProof(lp._1.digest, w))
            }
          }
        )
      }
    }
}
