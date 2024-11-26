package xyz.kd5ujc.accumulators.mpt

import cats.{Functor, Monad}
import cats.syntax.functor._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.EncoderOps
import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.hash.{Digest, JsonHasher}

sealed trait MerklePatriciaNode extends Node

object MerklePatriciaNode {
  private val LeafPrefix: Array[Byte] = Array(0: Byte)
  private val BranchPrefix: Array[Byte] = Array(1: Byte)
  private val ExtensionPrefix: Array[Byte] = Array(2: Byte)

  final case class Leaf private (data: Json, remaining: Seq[Nibble], digest: Digest) extends MerklePatriciaNode
  final case class Branch private (paths: Map[Nibble, MerklePatriciaNode], digest: Digest) extends MerklePatriciaNode
  final case class Extension private (shared: Seq[Nibble], child: Branch, digest: Digest) extends MerklePatriciaNode

  object Leaf {
    def apply[F[_]: Monad: JsonHasher](data: Json, remaining: Seq[Nibble]): F[Leaf] =
      nodeCommitment(data, remaining).map(new Leaf(data, remaining, _))

    def nodeCommitment[F[_]: JsonHasher](
                                          data:      Json,
                                          remaining: Seq[Nibble]
                                        ): F[Digest] = {
      val hashableJson = Json.obj(
        "remaining" -> remaining.asJson,
        "data"      -> data.asJson
      )

      JsonHasher[F].hash(hashableJson, LeafPrefix)
    }

    implicit val leafNodeEncoder: Encoder[Leaf] =
      Encoder.instance { node =>
        Json.obj(
          "data"      -> node.data.asJson,
          "remaining" -> node.remaining.asJson,
          "digest"    -> node.digest.asJson
        )
      }

    implicit val leafNodeDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          data      <- hCursor.downField("data").as[Json]
          remaining <- hCursor.downField("remaining").as[Seq[Nibble]]
          digest    <- hCursor.downField("digest").as[Digest]
        } yield new Leaf(data, remaining, digest)
      }
  }

  object Branch {
    private implicit val nibbleOrdering: Ordering[Nibble] = (x: Nibble, y: Nibble) => x.value.compareTo(y.value)

    def apply[F[_]: Functor: JsonHasher](paths: Map[Nibble, MerklePatriciaNode]): F[Branch] = {
      val pathDigests = paths.toSeq.sortBy(_._1).map { case (k, v) => k -> v.digest }.toMap

      nodeCommitment(pathDigests).map(Branch(paths, _))
    }

    def nodeCommitment[F[_]: JsonHasher](pathsDigest: Map[Nibble, Digest]): F[Digest] = {
      val hashableJson = Json.obj("pathDigests" -> pathsDigest.asJson)

      JsonHasher[F].hash(hashableJson, BranchPrefix)
    }

    implicit val encodeBranchNode: Encoder[Branch] =
      Encoder.instance { node =>
        Json.obj(
          "paths"  -> node.paths.toSeq.sortBy(_._1).toMap.asJson,
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
    def apply[F[_]: Functor: JsonHasher](shared: Seq[Nibble], child: Branch): F[Extension] =
      nodeCommitment(shared, child.digest).map(Extension(shared, child, _))

    def nodeCommitment[F[_]: JsonHasher](
                                          shared:      Seq[Nibble],
                                          childDigest: Digest
                                        ): F[Digest] = {
      val hashableJson = Json.obj(
        "shared"      -> shared.asJson,
        "childDigest" -> childDigest.asJson
      )

      JsonHasher[F].hash(hashableJson, ExtensionPrefix)
    }

    implicit val encodeExtensionNode: Encoder[Extension] =
      Encoder.instance { node =>
        Json.obj(
          "shared" -> node.shared.asJson,
          "child"  -> node.child.asJson,
          "digest" -> node.digest.asJson
        )
      }

    implicit val decodeExtensionNode: Decoder[Extension] =
      Decoder.instance { hCursor =>
        for {
          shared <- hCursor.downField("shared").as[Seq[Nibble]]
          child  <- hCursor.downField("child").as[Branch]
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
