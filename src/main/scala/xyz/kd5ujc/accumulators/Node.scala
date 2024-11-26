package xyz.kd5ujc.accumulators

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import xyz.kd5ujc.hash.Digest

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
