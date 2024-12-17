package xyz.kd5ujc.accumulators.mpt.api

import cats.Monad
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._

import xyz.kd5ujc.accumulators.mpt._
import xyz.kd5ujc.hash.{Digest, JsonHasher}

trait MerklePatriciaProver[F[_]] {
  def attest(path: Digest): F[Option[MerklePatriciaInclusionProof]]
}

object MerklePatriciaProver {
  def make[F[_]: Monad: JsonHasher](trie: MerklePatriciaTrie): MerklePatriciaProver[F] =
    new MerklePatriciaProver[F] {
      type Continue = (MerklePatriciaNode, Seq[Nibble], List[MerklePatriciaCommitment])
      type Return = Option[List[MerklePatriciaCommitment]]

      override def attest(path: Digest): F[Option[MerklePatriciaInclusionProof]] =
        Monad[F]
          .tailRecM[Continue, Return]((trie.rootNode, Nibble(path), List.empty[MerklePatriciaCommitment])) {
            case (currentNode, remainingPath, acc) =>
              currentNode match {
                case leaf: MerklePatriciaNode.Leaf if leaf.remaining == remainingPath =>
                  JsonHasher[F]
                    .hash(leaf.data)
                    .map(dataDigest => MerklePatriciaCommitment.Leaf(leaf.remaining, dataDigest) :: acc)
                    .map(_.some)
                    .map(_.asRight)

                case extension: MerklePatriciaNode.Extension if remainingPath.startsWith(extension.shared) =>
                  Monad[F].pure(
                    (
                      extension.child,
                      remainingPath.drop(extension.shared.length),
                      MerklePatriciaCommitment.Extension(extension.shared, extension.child.digest) :: acc
                    ).asLeft
                  )

                case branch: MerklePatriciaNode.Branch if remainingPath.nonEmpty =>
                  branch.paths.get(remainingPath.head) match {
                    case Some(child) =>
                      Monad[F].pure(
                        (
                          child,
                          remainingPath.tail,
                          MerklePatriciaCommitment.Branch(
                            branch.paths.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap
                          ) :: acc
                        ).asLeft
                      )

                    case None => Monad[F].pure(Right(None))
                  }

                case _ => Monad[F].pure(Right(None))
              }
          }
          .map(_.map(MerklePatriciaInclusionProof(path, _)))
    }
}
