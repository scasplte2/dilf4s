package xyz.kd5ujc.accumulators.merkle

import cats.MonadError
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import cats.syntax.applicative._

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.merkle.nodes.{InternalNode, LeafNode}
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerkleTree[L](
  rootNode:        Node[L],
  leafDigestIndex: Map[Digest[L], Int]
)

object MerkleTree {
  val leafPrefix: Array[Byte] = Array(0: Byte)
  val internalPrefix: Array[Byte] = Array(1: Byte)

  implicit def merkleTreeEncoder[L]: Encoder[MerkleTree[L]] = (tree: MerkleTree[L]) =>
    Json.obj(
      "rootNode" -> tree.rootNode.asJson,
      "leafDigestIndex" -> tree.leafDigestIndex.toList
        .sortBy(_._2)
        .map {
          case (digest, index) =>
            Json.obj(
              "digest" -> digest.asJson,
              "index"  -> index.asJson
            )
        }
        .asJson
    )

  implicit def merkleTreeDecoder[L]: Decoder[MerkleTree[L]] = (c: HCursor) =>
    for {
      rootNode        <- c.downField("rootNode").as[Node[L]]
      leafDigestIndex <- c.downField("leafDigestIndex").as[List[(Digest[L], Int)]].map(_.toMap)
    } yield MerkleTree[L](rootNode, leafDigestIndex)

  def create[F[_], A: Encoder, L](data: List[A])(implicit me: MonadError[F, Throwable], hasher: Hasher[F, L]): F[MerkleTree[L]] = {
    def buildNodes(nodes: List[Node[L]]): F[Node[L]] =
      nodes match {
        case Nil               => MonadError[F, Throwable].raiseError(new RuntimeException("Input list must be non-empty"))
        case singleNode :: Nil => singleNode.pure[F]
        case _ =>
          nodes
            .grouped(2)
            .toList
            .traverse[F, InternalNode[L]] {
              case Seq(leftNode, rightNode) => InternalNode[F, L](leftNode, Some(rightNode))
              case Seq(singleNode)          => InternalNode[F, L](singleNode, None)
              case _                        => MonadError[F, Throwable].raiseError(new RuntimeException("Unexpected input"))
            }
            .flatMap(buildNodes)
      }

    for {
      leafNodes <- data.traverse(LeafNode(_))
      rootNode  <- buildNodes(leafNodes)
      leafDigestIndex = leafNodes.zipWithIndex.map { case (node, index) => (node.digest, index) }.toMap
    } yield MerkleTree(rootNode, leafDigestIndex)
  }
}
