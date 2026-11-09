package xyz.kd5ujc.accumulators.merkle

import cats.syntax.all._

import xyz.kd5ujc.accumulators.{ProofInput, ValueCommitment}
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

  /** A proof heavier than `ProofInput.MaxProofWeight` is `Encoding`, refused before it is decoded. */
  def decodeInclusion[H: Hasher](v: CanonicalValue): Either[MerkleError, InclusionProof[H]] =
    for {
      _     <- ProofInput.bounded(v)(MerkleError.Encoding(_))
      w     <- MerkleInclusion.codec.decoder.decode(v).leftMap(e => MerkleError.Encoding(e.message))
      path  <- digests[H](w.path)
      proof <- InclusionProof.from(w.index, w.size, path)
    } yield proof

  /** A proof heavier than `ProofInput.MaxProofWeight` is `Encoding`, refused before it is decoded. */
  def decodeConsistency[H: Hasher](v: CanonicalValue): Either[MerkleError, ConsistencyProof[H]] =
    for {
      _     <- ProofInput.bounded(v)(MerkleError.Encoding(_))
      w     <- MerkleConsistency.codec.decoder.decode(v).leftMap(e => MerkleError.Encoding(e.message))
      path  <- digests[H](w.path)
      proof <- ConsistencyProof.from(w.old, w.`new`, path)
    } yield proof

  /**
   * `merkle_verify [head, leaf, proof]` under `M` (docs/specs/jlvm.md §6): whether `leaf`, committed by `commit(v)`, is
   * at the proof's index of the tree `(root, size)`. The leaf is a claim: heavier than `ProofInput.MaxProofWeight` it is
   * `TooLarge`, before the proof is decoded. A proof of another size or root is `false`; a malformed one an error.
   */
  def verifyLeaf[H](root: Digest[H], size: Long, leaf: CanonicalValue, proof: CanonicalValue)(
    implicit H: Hasher[H],
    M:          MerkleHashing[H]
  ): Either[MerkleError, Boolean] =
    for {
      // the head first, as every SDK checks it: a size outside the tree-index range is malformed, not a verdict
      _ <- Either.cond(size >= 0, (), MerkleError.Encoding("head.size is a tree size"))
      _ <- ProofInput.claimed(List(leaf))(MerkleError.TooLarge(_))
      p <- decodeInclusion[H](proof)
      c <- ValueCommitment.source(leaf).leftMap(e => MerkleError.TooLarge(e.limit))
      verdict <-
        if (p.size != size) false.asRight
        else
          MerkleVerifier.inclusion(root, M.leafOf(c), p) match {
            case Right(())                                                          => true.asRight
            case Left(MerkleError.InvalidProof(_) | MerkleError.RootMismatch(_, _)) => false.asRight
            case Left(other)                                                        => other.asLeft
          }
    } yield verdict

  implicit def inclusionCanonical[H]: Canonical[InclusionProof[H]] =
    Canonical.instance(p => encodeInclusion(p).leftMap(e => CanonicalError.TypeMismatch(e.toString)))

  implicit def consistencyCanonical[H]: Canonical[ConsistencyProof[H]] =
    Canonical.instance(p => encodeConsistency(p).leftMap(e => CanonicalError.TypeMismatch(e.toString)))
}
