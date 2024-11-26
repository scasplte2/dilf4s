package xyz.kd5ujc.accumulators.merkle

import cats.Functor
import cats.syntax.functor._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.hash.{Digest, JsonHasher}

sealed trait MerkleNode extends Node

object MerkleNode {
  private val LeafPrefix: Array[Byte] = Array(0: Byte)
  private val InternalPrefix: Array[Byte] = Array(1: Byte)

  final case class Leaf private (data: Json, digest: Digest) extends MerkleNode
  final case class Internal private (left: MerkleNode, right: Option[MerkleNode], digest: Digest) extends MerkleNode

  object Leaf {
    def apply[F[_]: Functor: JsonHasher](data: Json): F[Leaf] =
      nodeCommitment(data).map { digest =>
        new Leaf(data, digest)
      }

    def nodeCommitment[F[_]: JsonHasher](data: Json): F[Digest] =
      JsonHasher[F].hash(data, LeafPrefix)

    implicit val leafNodeEncoder: Encoder[Leaf] = Encoder.instance { node =>
      Json.obj(
        "data"   -> node.data,
        "digest" -> node.digest.asJson
      )
    }

    implicit val leafNodeDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          data   <- hCursor.downField("data").as[Json]
          digest <- hCursor.downField("digest").as[Digest]
        } yield Leaf(data, digest)
      }
  }

  object Internal {

    def apply[F[_]: Functor: JsonHasher](
      left:  MerkleNode,
      right: Option[MerkleNode]
    ): F[Internal] =
      nodeCommitment(left.digest, right.map(_.digest)).map { digest =>
        new Internal(left, right, digest)
      }

    def nodeCommitment[F[_]: JsonHasher](
      leftDigest:     Digest,
      rightDigestOpt: Option[Digest]
    ): F[Digest] = {
      val hashableJson = rightDigestOpt match {
        case Some(rd) => Json.obj("leftDigest" -> leftDigest.asJson, "rightDigest" -> rd.asJson)
        case None     => Json.obj("leftDigest" -> leftDigest.asJson)
      }

      JsonHasher[F].hash(hashableJson, InternalPrefix)
    }

    implicit val encodeInternalNode: Encoder[Internal] = Encoder.instance { node =>
      Json.obj(
        "left"   -> node.left.asJson,
        "right"  -> node.right.asJson,
        "digest" -> node.digest.asJson
      )
    }

    implicit val decodeInternalNode: Decoder[Internal] =
      Decoder.instance { hCursor =>
        for {
          left   <- hCursor.downField("left").as[MerkleNode]
          right  <- hCursor.downField("right").as[Option[MerkleNode]]
          digest <- hCursor.downField("digest").as[Digest]
        } yield new Internal(left, right, digest)
      }
  }

  implicit val encodeMerkleNode: Encoder[MerkleNode] = Encoder.instance {
    case leaf: Leaf =>
      Json.obj(
        "type"     -> Json.fromString("Leaf"),
        "contents" -> leaf.asJson
      )
    case internal: Internal =>
      Json.obj(
        "type"     -> Json.fromString("Internal"),
        "contents" -> internal.asJson
      )
  }

  implicit val decodeMerkleNode: Decoder[MerkleNode] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"     => cursor.downField("contents").as[Leaf]
      case "Internal" => cursor.downField("contents").as[Internal]
      case other      => Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }
}
