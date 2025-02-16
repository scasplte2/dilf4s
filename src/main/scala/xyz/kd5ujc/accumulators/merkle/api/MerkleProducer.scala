package xyz.kd5ujc.accumulators.merkle.api

import cats.effect.{Ref, Sync}
import cats.syntax.all._
import xyz.kd5ujc.accumulators.merkle.{MerkleNode, MerkleTree}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import scala.annotation.tailrec

trait MerkleProducer[F[_]] {
  def leaves: F[List[MerkleNode.Leaf]]

  def build: F[MerkleTree]

  def update(index: Int, leaf: MerkleNode.Leaf): F[Unit]

  def append(leaves: List[MerkleNode.Leaf]): F[Unit]

  def prepend(leaves: List[MerkleNode.Leaf]): F[Unit]

  def remove(index: Int): F[Unit]
}

object MerkleProducer {
  def make[F[_]: Sync: JsonHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    Ref
      .of[F, ProducerState](
        ProducerState(
          leaves = Vector.from(initial),
          nodeCache = Map.empty,
          dirtyNodes = Set.empty,
          currentRoot = None
        )
      )
      .map(new OptimizedMerkleProducer[F](_))

  class SimpleMerkleProducer[F[_]: Sync: JsonHasher](
    stateRef: Ref[F, Vector[MerkleNode.Leaf]]
  ) extends MerkleProducer[F] {
    override def leaves: F[List[MerkleNode.Leaf]] =
      stateRef.get.map(_.toList)

    override def build: F[MerkleTree] =
      stateRef.get.flatMap { leaves =>
        MerkleTree.create(leaves.toList)
      }

    def update(index: Int, leaf: MerkleNode.Leaf): F[Unit] =
      stateRef.update { leaves =>
        if (index >= 0 && index < leaves.size) leaves.updated(index, leaf)
        else leaves
      }

    def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
      stateRef.update { leaves =>
        leaves.appendedAll(newLeaves)
      }

    def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
      stateRef.update { leaves =>
        leaves.prependedAll(newLeaves)
      }

    def remove(index: Int): F[Unit] =
      stateRef.update { leaves =>
        if (index >= 0 && index < leaves.size) leaves.patch(index, Vector(), 1)
        else leaves
      }
  }

  class OptimizedMerkleProducer[F[_]: Sync: JsonHasher](
    stateRef: Ref[F, ProducerState]
  ) extends MerkleProducer[F] {

    def leaves: F[List[MerkleNode.Leaf]] =
      stateRef.get.map(_.leaves.toList)

    def build: F[MerkleTree] =
      stateRef.get.flatMap { state =>
        state.currentRoot match {
          case Some(root) if state.dirtyNodes.isEmpty =>
            root.pure[F]
          case _ =>
            rebuildTree(state).flatMap { tree =>
              stateRef
                .update(
                  _.copy(
                    currentRoot = Some(tree),
                    dirtyNodes = Set.empty
                  )
                )
                .as(tree)
            }
        }
      }

    private def rebuildTree(state: ProducerState): F[MerkleTree] = {
      def getOrBuildNode(left: MerkleNode, rightOpt: Option[MerkleNode]): F[MerkleNode] =
        state.nodeCache.get(left.digest) match {
          case Some(cached) if !state.dirtyNodes.contains(left.digest) => cached.pure[F]
          case _ =>
            MerkleNode
              .Internal(left, rightOpt)
              .flatTap { node =>
                stateRef.update { s =>
                  s.copy(nodeCache = s.nodeCache + (left.digest -> node))
                }
              }
              .widen
        }

      def buildLevel(nodes: List[MerkleNode]): F[MerkleNode] =
        Sync[F].tailRecM(nodes) { currentLevel =>
          if (currentLevel.length <= 1) currentLevel.head.asRight[List[MerkleNode]].pure[F]
          else {
            currentLevel
              .grouped(2)
              .toList
              .traverse[F, MerkleNode] {
                case left :: right :: Nil => getOrBuildNode(left, Some(right))
                case left :: Nil          => getOrBuildNode(left, None)
                case _                    => new RuntimeException("Unexpected grouping").raiseError
              }
              .map(_.asLeft[MerkleNode])
          }
        }

      buildLevel(state.leaves.toList).map { rootNode =>
        MerkleTree(
          rootNode,
          state.leaves.zipWithIndex.map {
            case (leaf, idx) => (leaf.digest, idx)
          }.toMap
        )
      }
    }

    def update(index: Int, leaf: MerkleNode.Leaf): F[Unit] =
      stateRef.get.flatMap { state =>
        if (index < 0 || index >= state.leaves.size) {
          new IndexOutOfBoundsException(s"Index $index out of bounds for size ${state.leaves.size}").raiseError
        } else {
          val dirtyPath = getPathToRoot(state, index)
          stateRef.update { s =>
            s.copy(
              leaves = s.leaves.updated(index, leaf),
              dirtyNodes = s.dirtyNodes ++ dirtyPath,
              currentRoot = None
            )
          }
        }
      }

    def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
      stateRef.update { state =>
        val startIdx = state.leaves.size
        val dirtyPath = (startIdx until startIdx + newLeaves.size).flatMap(getPathToRoot(state, _))
        state.copy(
          leaves = state.leaves ++ newLeaves,
          dirtyNodes = state.dirtyNodes ++ dirtyPath,
          currentRoot = None
        )
      }

    def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
      stateRef.update { state =>
        state.copy(
          leaves = Vector.from(newLeaves) ++ state.leaves,
          nodeCache = Map.empty,
          dirtyNodes = Set.empty,
          currentRoot = None
        )
      }

    def remove(index: Int): F[Unit] =
      stateRef.get.flatMap { state =>
        if (index < 0 || index >= state.leaves.size) {
          new IndexOutOfBoundsException(s"Index $index out of bounds for size ${state.leaves.size}").raiseError
        } else {
          val dirtyPath = getPathToRoot(state, index)
          stateRef.update { s =>
            s.copy(
              leaves = s.leaves.patch(index, Vector(), 1),
              dirtyNodes = s.dirtyNodes ++ dirtyPath,
              currentRoot = None
            )
          }
        }
      }

    private def getPathToRoot(state: ProducerState, index: Int): Set[Digest] = {

      @tailrec
      def loop(idx: Int, acc: Set[Digest]): Set[Digest] =
        if (idx == 0) acc
        else {
          val parentIdx = (idx - 1) / 2
          state.nodeCache.get(state.leaves(parentIdx).digest) match {
            case Some(parent) => loop(parentIdx, acc + parent.digest)
            case None         => acc
          }
        }

      loop(index, Set.empty)
    }
  }

  case class ProducerState(
    leaves:      Vector[MerkleNode.Leaf],
    nodeCache:   Map[Digest, MerkleNode],
    dirtyNodes:  Set[Digest],
    currentRoot: Option[MerkleTree]
  )
}
