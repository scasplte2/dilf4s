package xyz.kd5ujc.accumulators.merkle

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{Canonical, CanonicalError, CanonicalValue}
import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.models.merkle.{MerkleConsistency, MerkleInclusion}

/**
 * Canonical encodings of Merkle proofs: the generated codecs of shared/schemas/merkle.cddl fix the shape; this adds
 * what the schema cannot state (digest lengths for `H`, `index < size`, `old <= new`).
 */
object MerkleCodec {

  private def encoding(e: CanonicalError): MerkleError = MerkleError.Encoding(e.toString)

  private def digests[H: Hasher](path: Vector[scodec.bits.ByteVector]): Either[MerkleError, Vector[Digest[H]]] =
    path.traverse(Digest.fromBytes[H](_).leftMap(e => MerkleError.Encoding(e.toString)))

  def encodeInclusion[H](p: InclusionProof[H]): Either[MerkleError, CanonicalValue] =
    MerkleInclusion.codec.encode(MerkleInclusion(p.index, p.size, p.path.map(_.bytes))).leftMap(encoding)

  def encodeConsistency[H](p: ConsistencyProof[H]): Either[MerkleError, CanonicalValue] =
    MerkleConsistency.codec.encode(MerkleConsistency(p.oldSize, p.newSize, p.path.map(_.bytes))).leftMap(encoding)

  def decodeInclusion[H: Hasher](v: CanonicalValue): Either[MerkleError, InclusionProof[H]] =
    for {
      w     <- MerkleInclusion.codec.decoder.decode(v).leftMap(e => MerkleError.Encoding(e.message))
      path  <- digests[H](w.path)
      proof <- InclusionProof.from(w.index, w.size, path)
    } yield proof

  def decodeConsistency[H: Hasher](v: CanonicalValue): Either[MerkleError, ConsistencyProof[H]] =
    for {
      w     <- MerkleConsistency.codec.decoder.decode(v).leftMap(e => MerkleError.Encoding(e.message))
      path  <- digests[H](w.path)
      proof <- ConsistencyProof.from(w.old, w.`new`, path)
    } yield proof

  implicit def inclusionCanonical[H]: Canonical[InclusionProof[H]] =
    Canonical.instance(p => encodeInclusion(p).leftMap(e => CanonicalError.TypeMismatch(e.toString)))

  implicit def consistencyCanonical[H]: Canonical[ConsistencyProof[H]] =
    Canonical.instance(p => encodeConsistency(p).leftMap(e => CanonicalError.TypeMismatch(e.toString)))
}
