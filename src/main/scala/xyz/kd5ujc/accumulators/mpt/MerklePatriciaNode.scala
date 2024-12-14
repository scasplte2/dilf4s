package xyz.kd5ujc.accumulators.mpt

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait MerklePatriciaNode extends Node

object MerklePatriciaNode {
  private[mpt] val LeafPrefix: Array[Byte] = Array(0: Byte)
  private[mpt] val BranchPrefix: Array[Byte] = Array(1: Byte)
  private[mpt] val ExtensionPrefix: Array[Byte] = Array(2: Byte)

  final case class Leaf private (remaining: Seq[Nibble], data: Json, digest: Digest) extends MerklePatriciaNode
  final case class Branch private (paths: Map[Nibble, MerklePatriciaNode], digest: Digest) extends MerklePatriciaNode
  final case class Extension private (shared: Seq[Nibble], child: Branch, digest: Digest) extends MerklePatriciaNode

  object Leaf {
    def apply[F[_]: Monad: JsonHasher](remaining: Seq[Nibble], data: Json): F[Leaf] = for {
      dataDigest <- JsonHasher[F].hash(data)
      commitment <- MerklePatriciaCommitment.Leaf(remaining, dataDigest).pure[F]
      nodeDigest <- JsonHasher[F].hash(commitment.asJson, LeafPrefix)
    } yield Leaf(remaining, data, nodeDigest)

    implicit val leafNodeEncoder: Encoder[Leaf] =
      Encoder.instance { node =>
        Json.obj(
          "remaining" -> node.remaining.asJson(Nibble.nibbleSeqEncoder),
          "data"      -> node.data.asJson,
          "digest"    -> node.digest.asJson
        )
      }

    implicit val leafNodeDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          remaining <- hCursor.downField("remaining").as[Seq[Nibble]](Nibble.nibbleSeqDecoder)
          data      <- hCursor.downField("data").as[Json]
          digest    <- hCursor.downField("digest").as[Digest]
        } yield new Leaf(remaining, data, digest)
      }
  }

  object Branch {
    def apply[F[_]: Monad: JsonHasher](paths: Map[Nibble, MerklePatriciaNode]): F[Branch] = for {
      pathDigests <- paths.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap.pure[F]
      commitment  <- MerklePatriciaCommitment.Branch(pathDigests).pure[F]
      nodeDigest  <- JsonHasher[F].hash(commitment.asJson, BranchPrefix)
    } yield Branch(paths, nodeDigest)

    implicit val encodeBranchNode: Encoder[Branch] =
      Encoder.instance { node =>
        Json.obj(
          "paths"  -> node.paths.toSeq.sortBy(_._1.value).toMap.asJson,
          "digest" -> node.digest.asJson
        )
      }

    implicit val decodeBranchNode: Decoder[Branch] =
      Decoder.instance { hCursor =>
        for {
          children <- hCursor.downField("paths").as[Map[Nibble, MerklePatriciaNode]]
          digest   <- hCursor.downField("digest").as[Digest]
        } yield new Branch(children, digest)
      }
  }

  object Extension {
    def apply[F[_]: Monad: JsonHasher](shared: Seq[Nibble], child: Branch): F[Extension] = for {
      commitment <- MerklePatriciaCommitment.Extension(shared, child.digest).pure[F]
      nodeDigest <- JsonHasher[F].hash(commitment.asJson, ExtensionPrefix)
    } yield Extension(shared, child, nodeDigest)

    implicit val encodeExtensionNode: Encoder[Extension] =
      Encoder.instance { node =>
        Json.obj(
          "shared" -> node.shared.asJson(Nibble.nibbleSeqEncoder),
          "child"  -> (node.child: MerklePatriciaNode).asJson,
          "digest" -> node.digest.asJson
        )
      }

    implicit val decodeExtensionNode: Decoder[Extension] =
      Decoder.instance { hCursor =>
        for {
          shared <- hCursor.downField("shared").as[Seq[Nibble]](Nibble.nibbleSeqDecoder)
          child  <- hCursor.downField("child").downField("contents").as[Branch]
          digest <- hCursor.downField("digest").as[Digest]
        } yield new Extension(shared, child, digest)
      }
  }

  implicit val encodeMptNode: Encoder[MerklePatriciaNode] = Encoder.instance {
    case node: Leaf =>
      Json.obj(
        "type"     -> Json.fromString("Leaf"),
        "contents" -> node.asJson
      )
    case node: Extension =>
      Json.obj(
        "type"     -> Json.fromString("Extension"),
        "contents" -> node.asJson
      )
    case node: Branch =>
      Json.obj(
        "type"     -> Json.fromString("Branch"),
        "contents" -> node.asJson
      )
  }

  implicit val decodeMptNode: Decoder[MerklePatriciaNode] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"      => cursor.downField("contents").as[Leaf]
      case "Extension" => cursor.downField("contents").as[Extension]
      case "Branch"    => cursor.downField("contents").as[Branch]
      case other       => Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }
}
