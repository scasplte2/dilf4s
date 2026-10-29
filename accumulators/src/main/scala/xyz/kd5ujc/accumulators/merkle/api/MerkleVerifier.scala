package xyz.kd5ujc.accumulators.merkle.api

import cats.syntax.applicative._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.{Applicative, Monad}

import xyz.kd5ujc.accumulators.merkle.{MerkleInclusionProof, MerkleNode}
import xyz.kd5ujc.hash.Digest
import xyz.kd5ujc.hash.api.DigestProducer

/**
 * Type class for verifying Merkle inclusion proofs
 */
trait MerkleVerifier[F[_]] {

  /**
   * Confirm that a Merkle inclusion proof is valid
   *
   * @param proof The inclusion proof to verify
   * @return True if the proof is valid for this tree's root
   */
  def confirm(proof: MerkleInclusionProof): F[Boolean]

  /**
   * Confirm that a leaf exists in the tree
   *
   * @param leaf The leaf node to verify
   * @param proof The inclusion proof for this leaf
   * @return True if the leaf exists and the proof is valid
   */
  def confirmLeaf(leaf: MerkleNode.Leaf, proof: MerkleInclusionProof)(implicit app: Applicative[F]): F[Boolean] =
    if (leaf.digest == proof.leafDigest) confirm(proof)
    else false.pure[F]
}

object MerkleVerifier {
  def apply[F[_]](implicit verifier: MerkleVerifier[F]): MerkleVerifier[F] = verifier

  /**
   * Create a verifier for a specific root digest
   */
  private def fromRoot[F[_]: Monad: DigestProducer](root: Digest): MerkleVerifier[F] =
    new MerkleVerifier[F] {
      def confirm(proof: MerkleInclusionProof): F[Boolean] = {
        def combine(a: Digest, b: Digest): F[Digest] =
          MerkleNode.Internal.nodeCommitment(a, b.some)

        proof.witness
          .foldLeftM(proof.leafDigest) {
            case (acc, (digest, MerkleInclusionProof.LeftSide))  => combine(digest, acc)
            case (acc, (digest, MerkleInclusionProof.RightSide)) => combine(acc, digest)
            case (acc, _)                                        => acc.pure[F]
          }
          .map(_.value.sameElements(root.value))
      }
    }

  /**
   * Create a verifier instance from a DigestProducer and root
   */
  def make[F[_]: Monad](root: Digest)(implicit producer: DigestProducer[F]): MerkleVerifier[F] =
    fromRoot(root)

  /**
   * Provides syntax extensions for more ergonomic Merkle verification
   *
   * Import xyz.kd5ujc.accumulators.merkle.api.MerkleVerifier.syntax._ to use these extensions
   */
  object syntax {
    implicit class MerkleVerifierOps(val proof: MerkleInclusionProof) extends AnyVal {

      /**
       * Confirm this proof is valid
       *
       * @return True if the proof is valid for the tree's root
       */
      def confirm[F[_]](implicit V: MerkleVerifier[F]): F[Boolean] =
        V.confirm(proof)
    }

    implicit class MerkleLeafOps(val leaf: MerkleNode.Leaf) extends AnyVal {

      /**
       * Confirm this leaf exists in the tree
       *
       * @param proof The inclusion proof for this leaf
       * @return True if the leaf exists and the proof is valid
       */
      def confirmInclusion[F[_]](proof: MerkleInclusionProof)(implicit V: MerkleVerifier[F], app: Applicative[F]): F[Boolean] =
        V.confirmLeaf(leaf, proof)
    }
  }
}
