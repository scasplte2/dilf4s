package xyz.kd5ujc.accumulators.patricia_merkle.nodes

import cats.Functor
import cats.implicits.toFunctorOps

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

trait ExtensionNode[L] extends Node[L] {
  val prefix: Array[Byte]
  val child: Node[L]
}

object ExtensionNode {
  def apply[F[_]: Functor, L](_prefix: Array[Byte], _child: Node[L])(implicit h: Hasher[F, L]): F[ExtensionNode[L]] = {
    val hashableValue = _prefix ++ _child.digest.value

    h.hashBytes(hashableValue, PatriciaMerkleTrie.extensionPrefix).map { _digest =>
      new ExtensionNode[L] {
        val prefix: Array[Byte] = _prefix
        val child: Node[L] = _child
        val digest: Digest[L] = _digest
      }
    }
  }

  implicit def encodeExtensionNode[L](implicit nodeEncoder: Encoder[Node[L]]): Encoder[ExtensionNode[L]] = Encoder.instance { node =>
    Json.obj(
      "prefix" -> node.prefix.asJson,
      "child"  -> nodeEncoder(node.child).asJson,
      "digest" -> node.digest.asJson
    )
  }

  implicit def decodeExtensionNode[L](
    implicit nodeDecoder: Decoder[Node[L]],
    digestDecoder:        Decoder[Digest[L]]
  ): Decoder[ExtensionNode[L]] =
    Decoder.instance { hCursor =>
      for {
        _prefix <- hCursor.downField("prefix").as[Array[Byte]]
        _child  <- hCursor.downField("child").as[Node[L]]
        _digest <- hCursor.downField("digest").as[Digest[L]]
      } yield
        new ExtensionNode[L] {
          override val prefix: Array[Byte] = _prefix
          override val child: Node[L] = _child
          override def digest: Digest[L] = _digest
        }
    }
}
