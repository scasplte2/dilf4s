package xyz.kd5ujc.accumulators.mpt

import xyz.kd5ujc.hash.Digest

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerklePatriciaInclusionProof(
  path:    Digest,
  witness: List[MerklePatriciaCommitment]
)

object MerklePatriciaInclusionProof {
  implicit val mpInclusionProofEncoder: Encoder[MerklePatriciaInclusionProof] = (proof: MerklePatriciaInclusionProof) =>
    Json.obj(
      "path"    -> proof.path.asJson,
      "witness" -> proof.witness.asJson
    )

  implicit val mpInclusionProofDecoder: Decoder[MerklePatriciaInclusionProof] = (c: HCursor) =>
    for {
      path    <- c.downField("path").as[Digest]
      witness <- c.downField("witness").as[List[MerklePatriciaCommitment]]
    } yield MerklePatriciaInclusionProof(path, witness)
}
