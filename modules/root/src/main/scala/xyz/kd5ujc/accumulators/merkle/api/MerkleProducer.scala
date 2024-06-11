package xyz.kd5ujc.accumulators.merkle.api

import cats.effect.{Ref, Sync}
import cats.implicits.{toFlatMapOps, toFunctorOps}

import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.accumulators.merkle.nodes.LeafNode
import xyz.kd5ujc.hash.Hasher

import io.circe.Encoder

trait MerkleProducer[F[_], A, L] {
  val leaves: F[List[LeafNode[A, L]]]

  def build: F[MerkleTree[L]]

  def update(index: Int, leaf: LeafNode[A, L]): F[Unit]

  def append(leaves: List[LeafNode[A, L]]): F[Unit]

  def prepend(leaves: List[LeafNode[A, L]]): F[Unit]

  def remove(index: Int): F[Unit]
}

object MerkleProducer {
  def make[F[_]: Sync, A: Encoder, L](
    initial:         List[LeafNode[A, L]]
  )(implicit hasher: Hasher[F, L]): F[MerkleProducer[F, A, L]] =
    Ref.of[F, Vector[LeafNode[A, L]]](Vector.from(initial)).map { ref =>
      new MerkleProducer[F, A, L] {

        override val leaves: F[List[LeafNode[A, L]]] = ref.get.map(_.toList)

        override def build: F[MerkleTree[L]] =
          ref.get.flatMap { leaves =>
            MerkleTree.create(leaves.toList)
          }

        def update(index: Int, leaf: LeafNode[A, L]): F[Unit] =
          ref.update { leaves =>
            if (index >= 0 && index < leaves.size) leaves.updated(index, leaf)
            else leaves
          }

        def append(newLeaves: List[LeafNode[A, L]]): F[Unit] =
          ref.update { leaves =>
            leaves.appendedAll(newLeaves)
          }

        def prepend(newLeaves: List[LeafNode[A, L]]): F[Unit] =
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
