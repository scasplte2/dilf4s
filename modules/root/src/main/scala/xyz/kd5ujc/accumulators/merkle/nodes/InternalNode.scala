package xyz.kd5ujc.accumulators.merkle.nodes

import cats.Functor
import cats.implicits.toFunctorOps

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

trait InternalNode[L] extends Node[L] {
  val left: Node[L]
  val right: Option[Node[L]]

  def digest: Digest[L]
}

object InternalNode {

  def apply[F[_]: Functor, L](_left: Node[L], _right: Option[Node[L]])(implicit h: Hasher[F, L]): F[InternalNode[L]] = {
    val hashableValue =
      if (_right.isEmpty) _left.digest.value
      else _left.digest.value ++ _right.get.digest.value

    h.hashBytes(hashableValue, MerkleTree.internalPrefix).map { _digest =>
      new InternalNode[L] {
        val left: Node[L] = _left
        val right: Option[Node[L]] = _right
        val digest: Digest[L] = _digest
      }
    }
  }

  implicit def encodeInternalNode[L](implicit nodeEncoder: Encoder[Node[L]]): Encoder[InternalNode[L]] = Encoder.instance { node =>
    Json.obj(
      "left"   -> nodeEncoder(node.left).asJson,
      "right"  -> node.right.map(nodeEncoder(_)).asJson,
      "digest" -> node.digest.asJson
    )
  }

  implicit def decodeInternalNode[L](implicit nodeDecoder: Decoder[Node[L]], digestDecoder: Decoder[Digest[L]]): Decoder[InternalNode[L]] =
    Decoder.instance { hCursor =>
      for {
        _left   <- hCursor.downField("left").as[Node[L]]
        _right  <- hCursor.downField("right").as[Option[Node[L]]]
        _digest <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new InternalNode[L] {
          override val left: Node[L] = _left
          override val right: Option[Node[L]] = _right
          override def digest: Digest[L] = _digest
        }
    }
}
