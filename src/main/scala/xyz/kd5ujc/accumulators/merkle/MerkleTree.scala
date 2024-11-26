package xyz.kd5ujc.accumulators.merkle

import cats.MonadError
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import cats.syntax.applicative._
import cats.syntax.either._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

final case class MerkleTree(
  rootNode:        MerkleNode,
  leafDigestIndex: Map[Digest, Int]
)

object MerkleTree {

  implicit def merkleTreeEncoder: Encoder[MerkleTree] = (tree: MerkleTree) =>
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

  implicit def merkleTreeDecoder: Decoder[MerkleTree] = (c: HCursor) =>
    for {
      rootNode        <- c.downField("rootNode").as[MerkleNode]
      leafDigestIndex <- c.downField("leafDigestIndex").as[List[(Digest, Int)]].map(_.toMap)
    } yield MerkleTree(rootNode, leafDigestIndex)

  def create[F[_]: JsonHasher, A: Encoder](
    data: List[A]
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerkleTree] = {
    def buildNodes(nodes: List[MerkleNode]): F[MerkleNode] =
      if (nodes.isEmpty) me.raiseError(new RuntimeException("Input list must be non-empty"))
      else {
        me.tailRecM[List[MerkleNode], MerkleNode](nodes) {
          case singleNode :: Nil => singleNode.asRight[List[MerkleNode]].pure[F]
          case currentNodes @ _ =>
            currentNodes
              .grouped(2)
              .toList
              .traverse[F, MerkleNode.Internal] {
                case Seq(leftNode, rightNode) => MerkleNode.Internal(leftNode, Some(rightNode))
                case Seq(singleNode)          => MerkleNode.Internal(singleNode, None)
                case _                        => me.raiseError(new RuntimeException("Unexpected input"))
              }
              .map(_.asLeft[MerkleNode])
        }
      }

    for {
      leafNodes <- data.traverse(el => MerkleNode.Leaf(el.asJson))
      rootNode  <- buildNodes(leafNodes)
      leafDigestIndex = leafNodes.zipWithIndex.map { case (node, index) => (node.digest, index) }.toMap
    } yield MerkleTree(rootNode, leafDigestIndex)
  }
}
