package xyz.kd5ujc.accumulators.mpt.nodes

import cats.Functor
import cats.implicits._

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.mpt.MerklePatriciaTrie
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

trait BranchNode[L] extends Node[L] {
  val children: Map[Byte, Node[L]]
}

object BranchNode {
  def apply[F[_]: Functor, L](_children: Map[Byte, Node[L]])(implicit h: Hasher[F, L]): F[BranchNode[L]] = {
    val hashableValue = _children.toSeq.sortBy(_._1).flatMap { case (k, v) => k +: v.digest.value }.toArray

    h.hashBytes(hashableValue, MerklePatriciaTrie.branchPrefix).map { _digest =>
      new BranchNode[L] {
        val children: Map[Byte, Node[L]] = _children
        val digest: Digest[L] = _digest
      }
    }
  }

  implicit def encodeBranchNode[L](implicit nodeEncoder: Encoder[Node[L]]): Encoder[BranchNode[L]] = Encoder.instance { node =>
    Json.obj(
      "children" -> node.children.map { case (k, v) => k.toString -> nodeEncoder(v).asJson }.asJson,
      "digest"   -> node.digest.asJson
    )
  }

  implicit def decodeBranchNode[L](implicit nodeDecoder: Decoder[Node[L]], digestDecoder: Decoder[Digest[L]]): Decoder[BranchNode[L]] =
    Decoder.instance { hCursor =>
      for {
        _children <- hCursor.downField("children").as[Map[String, Node[L]]].map(_.map { case (k, v) => k.toByte -> v })
        _digest   <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new BranchNode[L] {
          override val children: Map[Byte, Node[L]] = _children
          override def digest: Digest[L] = _digest
        }
    }
}