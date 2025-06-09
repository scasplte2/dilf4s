package xyz.kd5ujc.hash

import cats.data.Validated

import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.bouncycastle.util.encoders.Hex

/**
 * Proof that a hash computation was performed correctly or that a preimage exists
 *
 * @param digest The resulting digest
 * @param proofData Additional data needed for verification
 * @param proofType The type of proof (hash computation or preimage knowledge)
 */
case class HashProof(
  digest:    Digest,
  proofData: Array[Byte],
  proofType: HashProof.ProofType
) {
  override def toString: String = s"HashProof(${digest.toString}, ${Hex.toHexString(proofData)}, $proofType)"

  override def equals(obj: Any): Boolean = obj match {
    case other: HashProof =>
      this.digest == other.digest &&
      this.proofData.sameElements(other.proofData) &&
      this.proofType == other.proofType
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + digest.hashCode()
    result = prime * result + java.util.Arrays.hashCode(proofData)
    result = prime * result + proofType.hashCode()
    result
  }
}

object HashProof {
  sealed trait ProofType
  case object HashComputation extends ProofType
  case object PreimageKnowledge extends ProofType

  /**
   * Create a proof for simple digest verification
   *
   * @param digest The digest to verify against
   * @return A proof for verifying data hashes to this digest
   */
  def forDigest(digest: Digest): HashProof =
    HashProof(digest, Array(), HashComputation)

  /**
   * Create a proof of preimage knowledge
   *
   * @param digest The digest that the preimage hashes to
   * @param proofData Additional verification data
   * @return A proof of preimage knowledge
   */
  def forPreimage(digest: Digest, proofData: Array[Byte]): Validated[InvalidProof, HashProof] =
    if (proofData == null || proofData.isEmpty)
      Validated.invalid(InvalidProofData)
    else
      Validated.valid(HashProof(digest, proofData, PreimageKnowledge))

  /**
   * Create a proof from raw components with validation
   *
   * @param digest The resulting digest
   * @param proofData Additional verification data
   * @param proofType The type of proof
   * @return A validated proof instance
   */
  def from(digest: Digest, proofData: Array[Byte], proofType: ProofType): Validated[InvalidProof, HashProof] =
    proofType match {
      case HashComputation =>
        // Hash computation proofs don't require additional data
        Validated.valid(HashProof(digest, Array(), proofType))
      case PreimageKnowledge =>
        // Preimage proofs must have some verification data
        if (proofData == null || proofData.isEmpty)
          Validated.invalid(InvalidProofData)
        else
          Validated.valid(HashProof(digest, proofData, proofType))
    }

  implicit val proofTypeEncoder: Encoder[ProofType] = Encoder.encodeString.contramap {
    case HashComputation   => "hash"
    case PreimageKnowledge => "preimage"
  }

  implicit val proofTypeDecoder: Decoder[ProofType] = Decoder.decodeString.emap {
    case "hash"     => Right(HashComputation)
    case "preimage" => Right(PreimageKnowledge)
    case other      => Left(s"Invalid proof type: $other")
  }

  implicit val hashProofEncoder: Encoder[HashProof] = Encoder.instance { proof =>
    import io.circe.Json
    Json.obj(
      "digest"    -> proof.digest.asJson,
      "proofData" -> Hex.toHexString(proof.proofData).asJson,
      "proofType" -> proof.proofType.asJson
    )
  }

  implicit val hashProofDecoder: Decoder[HashProof] = Decoder.instance { cursor =>
    for {
      digest       <- cursor.get[Digest]("digest")
      proofDataHex <- cursor.get[String]("proofData")
      proofType    <- cursor.get[ProofType]("proofType")
    } yield
      HashProof(
        digest,
        Hex.decode(proofDataHex),
        proofType
      )
  }
}

sealed trait InvalidProof
case object InvalidProofData extends InvalidProof
case class InvalidProofFormat(message: String) extends InvalidProof
