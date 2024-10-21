package xyz.kd5ujc.accumulators.mpt.nodes

import cats.Functor
import cats.implicits._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaTrie, Nibble}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

trait BranchNode[L] extends Node[L] {
  val children: Map[Nibble, Node[L]]
}

object BranchNode {
  def apply[F[_]: Functor, L](_children: Map[Nibble, Node[L]])(implicit h: JsonHasher[F, L]): F[BranchNode[L]] = {
    val hashableJson = _children.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap.asJson

    h.hash(hashableJson, MerklePatriciaTrie.BranchPrefix).map { _digest =>
      new BranchNode[L] {
        val children: Map[Nibble, Node[L]] = _children
        val digest: Digest[L] = _digest
      }
    }
  }

  implicit def encodeBranchNode[L](implicit nodeEncoder: Encoder[Node[L]]): Encoder[BranchNode[L]] =
    Encoder.instance { node =>
      Json.obj(
        "children" -> node.children.toSeq.sortBy(_._1.value).map { case (k, v) => k -> nodeEncoder(v).asJson }.toMap.asJson,
        "digest"   -> node.digest.asJson
      )
    }

  implicit def decodeBranchNode[L](implicit nodeDecoder: Decoder[Node[L]], digestDecoder: Decoder[Digest[L]]): Decoder[BranchNode[L]] =
    Decoder.instance { hCursor =>
      for {
        _children <- hCursor.downField("children").as[Map[Nibble, Node[L]]]
        _digest   <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new BranchNode[L] {
          override val children: Map[Nibble, Node[L]] = _children
          override def digest: Digest[L] = _digest
        }
    }
}
