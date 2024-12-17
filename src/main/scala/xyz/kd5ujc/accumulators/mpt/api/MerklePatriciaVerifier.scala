package xyz.kd5ujc.accumulators.mpt.api

import cats.Monad
import cats.syntax.either._
import cats.syntax.functor._

import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaCommitment, MerklePatriciaInclusionProof, MerklePatriciaNode, Nibble}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.syntax.EncoderOps

trait MerklePatriciaVerifier[F[_]] {
  def confirm(proof: MerklePatriciaInclusionProof): F[Boolean]
}

object MerklePatriciaVerifier {
  def make[F[_]: Monad: JsonHasher](root: Digest): MerklePatriciaVerifier[F] =
    new MerklePatriciaVerifier[F] {
      type Continue = (List[MerklePatriciaCommitment], Digest, Seq[Nibble])
      type Return = Boolean

      override def confirm(proof: MerklePatriciaInclusionProof): F[Boolean] =
        Monad[F].tailRecM[Continue, Return]((proof.witness.reverse, root, Nibble(proof.path))) {
          case (commitments, currentDigest, remainingPath) =>
            commitments match {
              case (nodeCommit: MerklePatriciaCommitment.Leaf) :: Nil =>
                JsonHasher[F]
                  .hash(nodeCommit.asJson, MerklePatriciaNode.LeafPrefix)
                  .map(_ == currentDigest && remainingPath == nodeCommit.remaining)
                  .map(_.asRight)

              case (nodeCommit: MerklePatriciaCommitment.Extension) :: tail =>
                JsonHasher[F]
                  .hash(nodeCommit.asJson, MerklePatriciaNode.ExtensionPrefix)
                  .map { digest =>
                    if (digest == currentDigest) (tail, nodeCommit.childDigest, remainingPath.drop(nodeCommit.shared.length)).asLeft
                    else Right(false)
                  }

              case (nodeCommit: MerklePatriciaCommitment.Branch) :: tail =>
                nodeCommit.pathsDigest.get(remainingPath.head) match {
                  case Some(childDigest) =>
                    JsonHasher[F]
                      .hash(nodeCommit.asJson, MerklePatriciaNode.BranchPrefix)
                      .map { digest =>
                        if (digest == currentDigest) (tail, childDigest, remainingPath.tail).asLeft
                        else Right(false)
                      }

                  case None => Monad[F].pure(Right(false))
                }

              case _ => Monad[F].pure(Right(false))
            }
        }
    }
}
