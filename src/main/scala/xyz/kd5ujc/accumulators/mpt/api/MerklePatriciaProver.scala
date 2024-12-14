package xyz.kd5ujc.accumulators.mpt.api

import cats.Monad
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.mpt._
import xyz.kd5ujc.hash.{Digest, JsonHasher}

trait MerklePatriciaProver[F[_]] {
  def fromPath(path: Digest): F[Option[MerklePatriciaInclusionProof]]
}

object MerklePatriciaProver {
  def make[F[_]: Monad: JsonHasher](trie: MerklePatriciaTrie): MerklePatriciaProver[F] =
    new MerklePatriciaProver[F] {
      override def fromPath(path: Digest): F[Option[MerklePatriciaInclusionProof]] = {

        @tailrec
        def traverse(
          currentNode:   MerklePatriciaNode,
          remainingPath: Seq[Nibble],
          pathAcc:       List[MerklePatriciaCommitment]
        ): F[Either[(MerklePatriciaNode, Seq[Nibble], List[MerklePatriciaCommitment]), Option[MerklePatriciaInclusionProof]]] =
          currentNode match {
            case leaf: MerklePatriciaNode.Leaf if leaf.remaining == remainingPath =>
              JsonHasher[F]
                .hash(leaf.data)
                .map(dataDigest => MerklePatriciaInclusionProof(path, MerklePatriciaCommitment.Leaf(leaf.remaining, dataDigest) :: pathAcc))
                .map(_.some)
                .map(_.asRight)

            case extension: MerklePatriciaNode.Extension if remainingPath.startsWith(extension.shared) =>
              traverse(
                extension.child,
                remainingPath.drop(extension.shared.length),
                MerklePatriciaCommitment.Extension(extension.shared, extension.digest) :: pathAcc
              )

            case branch: MerklePatriciaNode.Branch if remainingPath.nonEmpty =>
              branch.paths.get(remainingPath.head) match {
                case Some(child) =>
                  traverse(
                    child,
                    remainingPath.tail,
                    MerklePatriciaCommitment.Branch(
                      branch.paths.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap
                    ) :: pathAcc
                  )

                case None => Monad[F].pure(Right(None))
              }

            case _ => Monad[F].pure(Right(None))
          }

        Monad[F].tailRecM((trie.rootNode, Nibble(path), List.empty[MerklePatriciaCommitment])) {
          case (node, path, acc) => traverse(node, path, acc)
        }
      }
    }
}
