package xyz.kd5ujc.accumulators.merkle

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{Canonical, CanonicalValue}
import xyz.kd5ujc.core.hash.{Digest, Hasher}

/** Canonical encodings of Merkle proofs (docs/specs/merkle.md §4). */
object MerkleCodec {

  private def path[H](p: Vector[Digest[H]]): Either[MerkleError, CanonicalValue] =
    CanonicalValue.array(p.map(d => CanonicalValue.bytes(d.bytes))).leftMap(e => MerkleError.Encoding(e.toString))

  private def obj(fields: (String, CanonicalValue)*): Either[MerkleError, CanonicalValue] =
    CanonicalValue.map(fields).leftMap(e => MerkleError.Encoding(e.toString))

  def encodeInclusion[H](p: InclusionProof[H]): Either[MerkleError, CanonicalValue] =
    path(p.path).flatMap(ps => obj("index" -> CInt(p.index), "size" -> CInt(p.size), "path" -> ps))

  def encodeConsistency[H](p: ConsistencyProof[H]): Either[MerkleError, CanonicalValue] =
    path(p.path).flatMap(ps => obj("old" -> CInt(p.oldSize), "new" -> CInt(p.newSize), "path" -> ps))

  def decodeInclusion[H: Hasher](v: CanonicalValue): Either[MerkleError, InclusionProof[H]] =
    fields(v, "index", "size").flatMap { case (index, size, p) => InclusionProof(index, size, p).asRight[MerkleError] }

  def decodeConsistency[H: Hasher](v: CanonicalValue): Either[MerkleError, ConsistencyProof[H]] =
    fields(v, "old", "new").map { case (m, n, p) => ConsistencyProof(m, n, p) }

  private def fields[H: Hasher](v: CanonicalValue, a: String, b: String): Either[MerkleError, (Long, Long, Vector[Digest[H]])] =
    v match {
      case CMap(m) if m.keySet == Set(a, b, "path") =>
        (uint(m(a), a), uint(m(b), b), digests[H](m("path"))).tupled
      case other => MerkleError.Encoding(s"expected a map with keys $a, $b, path: $other").asLeft
    }

  private def uint(v: CanonicalValue, name: String): Either[MerkleError, Long] =
    v match {
      case CInt(n) if n >= 0 && n.isValidLong => n.toLong.asRight
      case other                              => MerkleError.Encoding(s"$name must be a non-negative 64-bit integer: $other").asLeft
    }

  private def digests[H: Hasher](v: CanonicalValue): Either[MerkleError, Vector[Digest[H]]] =
    v match {
      case CArray(vs) =>
        vs.traverse {
          case CBytes(b) => Digest.fromBytes[H](b).leftMap(e => MerkleError.Encoding(e.toString))
          case other     => MerkleError.Encoding(s"path entries must be bytes: $other").asLeft
        }
      case other => MerkleError.Encoding(s"path must be an array: $other").asLeft
    }

  implicit def inclusionCanonical[H]: Canonical[InclusionProof[H]] =
    Canonical.instance(p => encodeInclusion(p).leftMap(e => xyz.kd5ujc.core.canonical.CanonicalError.TypeMismatch(e.toString)))

  implicit def consistencyCanonical[H]: Canonical[ConsistencyProof[H]] =
    Canonical.instance(p => encodeConsistency(p).leftMap(e => xyz.kd5ujc.core.canonical.CanonicalError.TypeMismatch(e.toString)))
}
