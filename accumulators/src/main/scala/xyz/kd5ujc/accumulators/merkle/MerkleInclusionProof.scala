package xyz.kd5ujc.accumulators.merkle

import cats.data.Validated
import cats.syntax.either._

import xyz.kd5ujc.hash.Digest

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

sealed trait MerkleProofError
case class InvalidWitness(message: String) extends MerkleProofError
case class InvalidSide(value: Byte) extends MerkleProofError

/**
 * Proof of inclusion for a leaf in a Merkle tree
 *
 * @param leafDigest The digest of the leaf being proven
 * @param witness The path from leaf to root, with sibling digests and their positions
 */
final case class MerkleInclusionProof private (
  leafDigest: Digest,
  witness:    Seq[(Digest, MerkleInclusionProof.Side)]
)

object MerkleInclusionProof {

  /**
   * Side of a Merkle tree node (left or right child)
   */
  sealed trait Side {
    def value: Byte
  }
  case object LeftSide extends Side { val value: Byte = 0 }
  case object RightSide extends Side { val value: Byte = 1 }

  /**
   * Create a proof with validation
   *
   * @param leafDigest The digest of the leaf being proven
   * @param witness The path from leaf to root
   * @return A validated proof or error
   */
  def create(
    leafDigest: Digest,
    witness:    Seq[(Digest, Side)]
  ): Validated[MerkleProofError, MerkleInclusionProof] =
    if (witness.isEmpty) {
      Validated.invalid(InvalidWitness("Witness path cannot be empty"))
    } else {
      Validated.valid(new MerkleInclusionProof(leafDigest, witness))
    }

  /**
   * Create a proof for a single leaf with no siblings
   *
   * @param leafDigest The digest of the single leaf
   * @return A proof for a tree with one leaf
   */
  def forSingleLeaf(leafDigest: Digest): MerkleInclusionProof =
    new MerkleInclusionProof(leafDigest, Seq())

  implicit val sideEncoder: Encoder[Side] = Encoder.encodeByte.contramap(_.value)
  implicit val sideDecoder: Decoder[Side] = Decoder.decodeByte.emap {
    case 0     => Right(LeftSide)
    case 1     => Right(RightSide)
    case other => Left(s"Invalid side value: $other")
  }

  implicit val proofEncoder: Encoder[MerkleInclusionProof] = (mp: MerkleInclusionProof) =>
    Json.obj(
      "leafDigest" -> mp.leafDigest.asJson,
      "witness" -> mp.witness.map {
        case (digest, side) =>
          Json.obj(
            "digest" -> digest.asJson,
            "side"   -> side.asJson
          )
      }.asJson
    )

  implicit val proofDecoder: Decoder[MerkleInclusionProof] = (c: HCursor) =>
    for {
      leafDigest <- c.downField("leafDigest").as[Digest]
      witness    <- c.downField("witness").as[Seq[(Digest, Side)]]
      proof      <- create(leafDigest, witness).toEither.leftMap(err => DecodingFailure(s"Invalid proof: $err", c.history))
    } yield proof
}
