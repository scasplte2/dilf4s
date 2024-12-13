package xyz.kd5ujc.accumulators.mpt

import xyz.kd5ujc.hash.Digest

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait MerklePatriciaCommitment

object MerklePatriciaCommitment {
  final case class Leaf(remaining: Seq[Nibble], dataDigest: Digest) extends MerklePatriciaCommitment
  final case class Branch(pathsDigest: Map[Nibble, Digest]) extends MerklePatriciaCommitment
  final case class Extension(shared: Seq[Nibble], childDigest: Digest) extends MerklePatriciaCommitment

  object Leaf {
    implicit val leafCommitEncoder: Encoder[Leaf] =
      Encoder.instance { node =>
        Json.obj(
          "remaining"  -> node.remaining.asJson(Nibble.nibbleSeqEncoder),
          "dataDigest" -> node.dataDigest.asJson
        )
      }

    implicit val leafCommitDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          remaining  <- hCursor.downField("remaining").as[Seq[Nibble]](Nibble.nibbleSeqDecoder)
          dataDigest <- hCursor.downField("dataDigest").as[Digest]
        } yield Leaf(remaining, dataDigest)
      }
  }

  object Branch {
    implicit val branchCommitEncoder: Encoder[Branch] =
      Encoder.instance { node =>
        Json.obj(
          "pathsDigest" -> node.pathsDigest.asJson
        )
      }

    implicit val branchCommitDecoder: Decoder[Branch] =
      Decoder.instance { hCursor =>
        for {
          pathsDigest <- hCursor.downField("pathsDigest").as[Map[Nibble, Digest]]
        } yield Branch(pathsDigest)
      }
  }

  object Extension {
    implicit val extensionCommitEncoder: Encoder[Extension] =
      Encoder.instance { node =>
        Json.obj(
          "shared"      -> node.shared.asJson(Nibble.nibbleSeqEncoder),
          "childDigest" -> node.childDigest.asJson
        )
      }

    implicit val extensionCommitDecoder: Decoder[Extension] =
      Decoder.instance { hCursor =>
        for {
          shared      <- hCursor.downField("shared").as[Seq[Nibble]](Nibble.nibbleSeqDecoder)
          childDigest <- hCursor.downField("childDigest").as[Digest]
        } yield Extension(shared, childDigest)
      }
  }

  implicit val mpCommitEncoder: Encoder[MerklePatriciaCommitment] = Encoder.instance {
    case commit: Leaf =>
      Json.obj(
        "type"     -> Json.fromString("Leaf"),
        "contents" -> commit.asJson
      )
    case commit: Extension =>
      Json.obj(
        "type"     -> Json.fromString("Extension"),
        "contents" -> commit.asJson
      )
    case commit: Branch =>
      Json.obj(
        "type"     -> Json.fromString("Branch"),
        "contents" -> commit.asJson
      )
  }

  implicit val mpCommitDecoder: Decoder[MerklePatriciaCommitment] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"      => cursor.downField("contents").as[Leaf]
      case "Branch"    => cursor.downField("contents").as[Branch]
      case "Extension" => cursor.downField("contents").as[Extension]
      case other       => Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }
}
