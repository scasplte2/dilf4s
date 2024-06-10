package xyz.kd5ujc.accumulators.merkle.nodes

import cats.Functor
import cats.implicits.toFunctorOps

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

trait LeafNode[A, L] extends Node[L] {
  val data: A
}

object LeafNode {
  def apply[F[_]: Functor, A: Encoder, L](_data: A)(implicit h: Hasher[F, L]): F[LeafNode[A, L]] =
    h.hashJson(_data, MerkleTree.leafPrefix).map { _digest =>
      new LeafNode[A, L] {
        val data: A = _data
        val digest: Digest[L] = _digest
      }
    }

  implicit def leafNodeEncoder[A: Encoder, L]: Encoder[LeafNode[A, L]] = Encoder.instance { node =>
    Json.obj(
      "data"   -> node.data.asJson,
      "digest" -> node.digest.asJson
    )
  }

  implicit def leafNodeDecoder[A: Decoder, L](implicit digestDecoder: Decoder[Digest[L]]): Decoder[LeafNode[A, L]] =
    Decoder.instance { hCursor =>
      for {
        _data   <- hCursor.downField("data").as[A]
        _digest <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new LeafNode[A, L] {
          override val data: A = _data
          override def digest: Digest[L] = _digest
        }
    }
}
