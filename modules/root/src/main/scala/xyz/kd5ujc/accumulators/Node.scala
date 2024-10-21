package xyz.kd5ujc.accumulators

import xyz.kd5ujc.hash.Digest

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

trait Node[L] {
  def digest: Digest[L]
}

object Node {

  implicit def encodeNode[L](implicit digestEncoder: Encoder[Digest[L]]): Encoder[Node[L]] =
    Encoder.instance { node =>
      digestEncoder(node.digest).asJson
    }

  implicit def decodeNode[L](implicit digestDecoder: Decoder[Digest[L]]): Decoder[Node[L]] =
    Decoder.instance { hCursor =>
      for {
        d <- hCursor.as[Digest[L]]
      } yield
        new Node[L] {
          override def digest: Digest[L] = d
        }
    }
}
