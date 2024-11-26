package xyz.kd5ujc.accumulators.merkle

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import xyz.kd5ujc.accumulators.merkle.MerkleInclusionProof.Side
import xyz.kd5ujc.hash.Digest

final case class MerkleInclusionProof(
  leafDigest: Digest,
  path:       Seq[(Digest, Side)]
)

object MerkleInclusionProof {
  val leftSide: Side = Side(0: Byte)
  val rightSide: Side = Side(1: Byte)

  implicit def proofEncoder: Encoder[MerkleInclusionProof] = (mp: MerkleInclusionProof) =>
    Json.obj(
      "leafDigest" -> mp.leafDigest.asJson,
      "path" -> mp.path.map {
        case (digest, side) =>
          Json.obj(
            "digest" -> digest.asJson,
            "side"   -> side.asJson
          )
      }.asJson
    )

  implicit def proofDecoder: Decoder[MerkleInclusionProof] = (c: HCursor) =>
    for {
      leafDigest <- c.downField("leafDigest").as[Digest]
      witness    <- c.downField("path").as[Seq[(Digest, Side)]]
    } yield MerkleInclusionProof(leafDigest, witness)

  final case class Side(value: Byte) extends AnyVal

  object Side {
    implicit val sideEncoder: Encoder[Side] = deriveEncoder
    implicit val sideDecoder: Decoder[Side] = deriveDecoder
  }
}
