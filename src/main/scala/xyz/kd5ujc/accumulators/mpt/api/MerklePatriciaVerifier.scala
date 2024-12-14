package xyz.kd5ujc.accumulators.mpt.api

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaCommitment, MerklePatriciaInclusionProof, MerklePatriciaNode, Nibble}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.syntax.EncoderOps

trait MerklePatriciaVerifier[F[_]] {
  def isValid(proof: MerklePatriciaInclusionProof): F[Boolean]
}

object MerklePatriciaVerifier {
  def make[F[_]: Monad: JsonHasher](root: Digest): MerklePatriciaVerifier[F] =
    new MerklePatriciaVerifier[F] {
      override def isValid(proof: MerklePatriciaInclusionProof): F[Boolean] = {
        def validateCommitment(
          commitments:   List[MerklePatriciaCommitment],
          currentDigest: Digest,
          remainingPath: Seq[Nibble]
        ): F[Boolean] =
          commitments match {
            case (nodeCommit: MerklePatriciaCommitment.Leaf) :: Nil =>
              JsonHasher[F]
                .hash(nodeCommit.asJson, MerklePatriciaNode.LeafPrefix)
                .map(_ == currentDigest && remainingPath == nodeCommit.remaining)

            case (nodeCommit: MerklePatriciaCommitment.Extension) :: tail =>
              JsonHasher[F]
                .hash(nodeCommit.asJson, MerklePatriciaNode.ExtensionPrefix)
                .flatMap { digest =>
                  if (digest == currentDigest)
                    validateCommitment(tail, nodeCommit.childDigest, remainingPath.drop(nodeCommit.shared.length))
                  else false.pure[F]
                }

            case (nodeCommit: MerklePatriciaCommitment.Branch) :: tail =>
              nodeCommit.pathsDigest.get(remainingPath.head) match {
                case Some(childDigest) =>
                  JsonHasher[F]
                    .hash(nodeCommit.asJson, MerklePatriciaNode.BranchPrefix)
                    .flatMap { digest =>
                      if (digest == currentDigest) validateCommitment(tail, childDigest, remainingPath.tail)
                      else false.pure[F]
                    }

                case None => false.pure[F]
              }

            case _ => false.pure[F]
          }

        validateCommitment(proof.witness.reverse, root, Nibble(proof.path))
      }
    }
}
