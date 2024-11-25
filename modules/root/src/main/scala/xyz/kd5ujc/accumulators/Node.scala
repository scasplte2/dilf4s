package xyz.kd5ujc.accumulators

import xyz.kd5ujc.hash.Digest

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

trait Node {
  def digest: Digest
}

object Node {

  implicit def encodeNode(implicit digestEncoder: Encoder[Digest]): Encoder[Node] =
    Encoder.instance { node =>
      digestEncoder(node.digest).asJson
    }

  implicit def decodeNode(implicit digestDecoder: Decoder[Digest]): Decoder[Node] =
    Decoder.instance { hCursor =>
      for {
        d <- hCursor.as[Digest]
      } yield
        new Node {
          override def digest: Digest = d
        }
    }
}
