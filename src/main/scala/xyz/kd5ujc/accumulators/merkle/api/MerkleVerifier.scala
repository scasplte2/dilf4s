package xyz.kd5ujc.accumulators.merkle.api

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._

import xyz.kd5ujc.accumulators.merkle.{MerkleInclusionProof, MerkleNode}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

trait MerkleVerifier[F[_]] {
  def confirm(proof: MerkleInclusionProof): F[Boolean]
}

object MerkleVerifier {
  def make[F[_]: Monad: JsonHasher](root: Digest): MerkleVerifier[F] =
    new MerkleVerifier[F] {
      override def confirm(proof: MerkleInclusionProof): F[Boolean] = {

        def combine(a: Digest, b: Digest): F[Digest] =
          MerkleNode.Internal.nodeCommitment(a, b.some)

        proof.witness
          .foldLeftM(proof.leafDigest) {
            case (acc, (digest, MerkleInclusionProof.leftSide))  => combine(digest, acc)
            case (acc, (digest, MerkleInclusionProof.rightSide)) => combine(acc, digest)
            case (acc, _)                                        => acc.pure[F]
          }
          .map(_.value.sameElements(root.value))
      }
    }
}
