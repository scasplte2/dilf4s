package xyz.kd5ujc.accumulators.poseidon

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.hash.Poseidon

sealed trait PoseidonMerkleError extends Product with Serializable

object PoseidonMerkleError {
  final case class NonCanonical(role: String, value: BigInt) extends PoseidonMerkleError
  final case class Position(position: BigInt, capacity: BigInt) extends PoseidonMerkleError
  final case class Depth(depth: Int) extends PoseidonMerkleError
  final case class OccupiedPosition(position: BigInt) extends PoseidonMerkleError
}

/** A path of sibling digests, **root-first** (the order the SP1 shielded-transfer circuit consumes). */
final case class PoseidonMerkleProof(position: BigInt, siblings: Vector[BigInt]) {
  def depth: Int = siblings.size
}

/**
 * A sparse, fixed-depth Poseidon Merkle tree of field elements at positions `[0, 2^depth)`, compatible with metakit's
 * `PoseidonMerkleTree`: empty leaves are 0, a node is `poseidon([left, right])`, and empty subtrees have the
 * precomputed zero hashes. A leaf set to 0 is indistinguishable from an empty one, so an absence proof is an
 * inclusion proof of 0.
 */
final class PoseidonMerkleTree private (
  val depth:          Int,
  val zeroHashes:     Vector[BigInt],
  private val nodes:  Map[(Int, BigInt), BigInt],
  private val leaves: Map[BigInt, BigInt]
) {

  def capacity: BigInt = BigInt(1) << depth
  def root: BigInt = nodes.getOrElse((depth, BigInt(0)), zeroHashes(depth))

  private def checkPosition(position: BigInt): Either[PoseidonMerkleError, BigInt] =
    Either.cond(position >= 0 && position < capacity, position, PoseidonMerkleError.Position(position, capacity))

  def leafAt(position: BigInt): Either[PoseidonMerkleError, BigInt] = checkPosition(position).map(leaves.getOrElse(_, BigInt(0)))

  def insert(position: BigInt, leaf: BigInt): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    for {
      _ <- checkPosition(position)
      _ <- Either.cond(Poseidon.isCanonical(leaf), (), PoseidonMerkleError.NonCanonical(s"leaf at $position", leaf))
    } yield {
      @tailrec def climb(level: Int, idx: BigInt, digest: BigInt, acc: Map[(Int, BigInt), BigInt]): Map[(Int, BigInt), BigInt] = {
        val acc1 = if (digest == zeroHashes(level)) acc - ((level, idx)) else acc.updated((level, idx), digest)
        if (level == depth) acc1
        else {
          val right = position.testBit(level)
          val sibling = acc1.getOrElse((level, if (right) idx - 1 else idx + 1), zeroHashes(level))
          climb(
            level + 1,
            idx >> 1,
            if (right) Poseidon.compressCanonical(sibling, digest) else Poseidon.compressCanonical(digest, sibling),
            acc1
          )
        }
      }
      new PoseidonMerkleTree(
        depth,
        zeroHashes,
        climb(0, position, leaf, nodes),
        if (leaf == 0) leaves - position else leaves.updated(position, leaf)
      )
    }

  def insertAll(entries: Iterable[(BigInt, BigInt)]): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    entries.toList.foldM(this) { case (t, (p, l)) => t.insert(p, l) }

  def proof(position: BigInt): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    checkPosition(position).map { _ =>
      @tailrec def collect(level: Int, idx: BigInt, acc: List[BigInt]): List[BigInt] =
        if (level == depth) acc
        else
          collect(
            level + 1,
            idx >> 1,
            nodes.getOrElse((level, if (position.testBit(level)) idx - 1 else idx + 1), zeroHashes(level)) :: acc
          )
      PoseidonMerkleProof(position, collect(0, position, Nil).toVector)
    }

  def absenceProof(position: BigInt): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    leafAt(position).flatMap(l => Either.cond(l == 0, (), PoseidonMerkleError.OccupiedPosition(position))).flatMap(_ => proof(position))
}

object PoseidonMerkleTree {

  val DefaultDepth: Int = 32

  def zeroHashes(depth: Int): Vector[BigInt] =
    (1 to depth).scanLeft(BigInt(0))((prev, _) => Poseidon.compressCanonical(prev, prev)).toVector

  def empty(depth: Int = DefaultDepth): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    Either.cond(
      depth >= 1 && depth <= 256,
      new PoseidonMerkleTree(depth, zeroHashes(depth), Map.empty, Map.empty),
      PoseidonMerkleError.Depth(depth)
    )

  def computeRoot(leaf: BigInt, proof: PoseidonMerkleProof): Either[PoseidonMerkleError, BigInt] =
    for {
      _ <- Either.cond(Poseidon.isCanonical(leaf), (), PoseidonMerkleError.NonCanonical("leaf", leaf))
      _ <- proof.siblings.zipWithIndex.traverse_ {
        case (s, i) => Either.cond(Poseidon.isCanonical(s), (), PoseidonMerkleError.NonCanonical(s"sibling[$i]", s))
      }
      _ <- Either.cond(
        proof.position >= 0 && proof.position < (BigInt(1) << proof.depth),
        (),
        PoseidonMerkleError.Position(proof.position, BigInt(1) << proof.depth)
      )
    } yield
      proof.siblings.reverse.zipWithIndex.foldLeft(leaf) {
        case (cur, (sib, level)) =>
          if (proof.position.testBit(level)) Poseidon.compressCanonical(sib, cur) else Poseidon.compressCanonical(cur, sib)
      }

  def verifyInclusion(leaf: BigInt, proof:             PoseidonMerkleProof, root: BigInt): Boolean = computeRoot(leaf, proof).contains(root)
  def verifyAbsence(proof:  PoseidonMerkleProof, root: BigInt): Boolean = verifyInclusion(BigInt(0), proof, root)
}
