package xyz.kd5ujc.accumulators.mpt.nodes

import cats.Monad
import cats.implicits.toFunctorOps

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaTrie, Nibble}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

trait MPTLeafNode[A, L] extends Node[L] {
  val suffix: Seq[Nibble]
  val data: A
}

object MPTLeafNode {
  def apply[F[_]: Monad, A: Encoder, L](_suffix: Seq[Nibble], _data: A)(implicit h: JsonHasher[F, L]): F[MPTLeafNode[A, L]] = {
    val hashableJson = Json.obj(
      "suffix" -> _suffix.asJson,
      "data"   -> _data.asJson
    )

    h.hash(hashableJson, MerklePatriciaTrie.LeafPrefix).map { _digest =>
      new MPTLeafNode[A, L] {
        val suffix: Seq[Nibble] = _suffix
        val data: A = _data
        val digest: Digest[L] = _digest
      }
    }
  }

  implicit def leafNodeEncoder[A: Encoder, L]: Encoder[MPTLeafNode[A, L]] =
    Encoder.instance { node =>
      Json.obj(
        "suffix" -> node.suffix.asJson,
        "data"   -> node.data.asJson,
        "digest" -> node.digest.asJson
      )
    }

  implicit def leafNodeDecoder[A: Decoder, L](implicit digestDecoder: Decoder[Digest[L]]): Decoder[MPTLeafNode[A, L]] =
    Decoder.instance { hCursor =>
      for {
        _suffix <- hCursor.downField("suffix").as[Seq[Nibble]]
        _data   <- hCursor.downField("data").as[A]
        _digest <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new MPTLeafNode[A, L] {
          override val suffix: Seq[Nibble] = _suffix
          override val data: A = _data
          override def digest: Digest[L] = _digest
        }
    }
}
