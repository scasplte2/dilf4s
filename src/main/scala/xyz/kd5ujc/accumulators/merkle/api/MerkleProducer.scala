package xyz.kd5ujc.accumulators.merkle.api

import cats.effect.{Ref, Sync}
import cats.implicits.{toFlatMapOps, toFunctorOps}

import xyz.kd5ujc.accumulators.merkle.{MerkleNode, MerkleTree}
import xyz.kd5ujc.hash.JsonHasher

trait MerkleProducer[F[_]] {
  val leaves: F[List[MerkleNode.Leaf]]

  def build: F[MerkleTree]

  def update(index: Int, leaf: MerkleNode.Leaf): F[Unit]

  def append(leaves: List[MerkleNode.Leaf]): F[Unit]

  def prepend(leaves: List[MerkleNode.Leaf]): F[Unit]

  def remove(index: Int): F[Unit]
}

object MerkleProducer {
  def make[F[_]: Sync, A](
    initial:         List[MerkleNode.Leaf]
  )(implicit hasher: JsonHasher[F]): F[MerkleProducer[F]] =
    Ref.of[F, Vector[MerkleNode.Leaf]](Vector.from(initial)).map { ref =>
      new MerkleProducer[F] {

        override val leaves: F[List[MerkleNode.Leaf]] = ref.get.map(_.toList)

        override def build: F[MerkleTree] =
          ref.get.flatMap { leaves =>
            MerkleTree.create(leaves.toList)
          }

        def update(index: Int, leaf: MerkleNode.Leaf): F[Unit] =
          ref.update { leaves =>
            if (index >= 0 && index < leaves.size) leaves.updated(index, leaf)
            else leaves
          }

        def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
          ref.update { leaves =>
            leaves.appendedAll(newLeaves)
          }

        def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
          ref.update { leaves =>
            leaves.prependedAll(newLeaves)
          }

        def remove(index: Int): F[Unit] =
          ref.update { leaves =>
            if (index >= 0 && index < leaves.size) leaves.patch(index, Vector(), 1)
            else leaves
          }
      }
    }
}
