package xyz.kd5ujc.accumulators.poseidon

import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.accumulators.ProofInput
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.{Fr, Poseidon}
import xyz.kd5ujc.models.poseidon.{PoseidonMerkleProof => Wire}

sealed trait PoseidonMerkleError extends Product with Serializable

object PoseidonMerkleError {

  /**
   * A position outside `[0, 2^depth)`, or more than `MaxDepth` siblings. `depth` (the number of siblings, or the
   * tree's depth) is normative (docs/specs/conformance.md §3); the position echoes the caller's input and is not.
   */
  // a caller's position can be any size: it is shown by its bit length, never its digits
  final case class Position(position: BigInt, depth: Int) extends PoseidonMerkleError {
    override def toString: String = s"Position(a ${position.bitLength}-bit position, depth $depth)"
  }
  final case class Depth(depth: Int) extends PoseidonMerkleError
  final case class OccupiedPosition(position: BigInt) extends PoseidonMerkleError
  final case class Encoding(reason: String) extends PoseidonMerkleError
}

/**
 * A path of sibling digests, **root-first** (the order the SP1 shielded-transfer circuit consumes), valid by
 * construction: `0 <= position < 2^depth`, and every sibling is an [[Fr]]. Built by a tree, by [[PoseidonMerkleProof.from]],
 * or decoded from its wire form, so computing a root from one is total.
 */
final case class PoseidonMerkleProof private (position: BigInt, siblings: Vector[Fr]) {
  def depth: Int = siblings.size
}

object PoseidonMerkleProof {

  def from(position: BigInt, siblings: Vector[Fr]): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    Either.cond(
      siblings.size <= PoseidonMerkleTree.MaxDepth && position >= 0 && position < (BigInt(1) << siblings.size),
      new PoseidonMerkleProof(position, siblings),
      PoseidonMerkleError.Position(position, siblings.size)
    )

  /** The wire form of shared/schemas/poseidon.cddl, through its generated codec. */
  def toCanonical(p: PoseidonMerkleProof): Either[PoseidonMerkleError, CanonicalValue] =
    Wire.codec.encode(Wire(p.position, p.siblings.map(_.value))).leftMap(e => PoseidonMerkleError.Encoding(e.toString))

  /**
   * The schema fixes shape and field ranges; this adds the invariant it cannot state (`position < 2^depth`). A proof
   * heavier than `ProofInput.MaxProofWeight` is `Encoding`, refused before it is decoded.
   */
  def fromCanonical(v: CanonicalValue): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    for {
      _        <- ProofInput.bounded(v)(PoseidonMerkleError.Encoding(_))
      w        <- Wire.codec.decoder.decode(v).leftMap(e => PoseidonMerkleError.Encoding(e.message))
      siblings <- w.siblings.traverse(Fr.from).leftMap(e => PoseidonMerkleError.Encoding(e.toString))
      proof    <- from(w.position, siblings)
    } yield proof
}

/**
 * A sparse, fixed-depth Poseidon Merkle tree of field elements at positions `[0, 2^depth)`, compatible with metakit's
 * `PoseidonMerkleTree` and the SP1 shielded-transfer circuit: empty leaves are 0, a node is `poseidon([left, right])`,
 * and empty subtrees have the precomputed zero hashes. A leaf set to 0 is indistinguishable from an empty one, so an
 * absence proof is an inclusion proof of 0. Leaves and roots are [[Fr]], so only positions can be out of range.
 *
 * Positions are the caller's, and a `BigInt`'s hash code is trivially collidable (every `(x << 32) | x` hashes to 0), so
 * they are only ever compared: `nodes(level)` holds that level's non-empty nodes by index, and every map is a `TreeMap`.
 */
final class PoseidonMerkleTree private (
  val depth:          Int,
  val zeroHashes:     Vector[Fr],
  private val nodes:  Vector[TreeMap[BigInt, Fr]],
  private val leaves: TreeMap[BigInt, Fr]
) {

  def capacity: BigInt = BigInt(1) << depth
  def root: Fr = node(depth, BigInt(0))

  private def node(level: Int, idx: BigInt): Fr = nodes.lift(level).flatMap(_.get(idx)).getOrElse(zeroHashes(level))

  private def checkPosition(position: BigInt): Either[PoseidonMerkleError, BigInt] =
    Either.cond(position >= 0 && position < capacity, position, PoseidonMerkleError.Position(position, depth))

  def leafAt(position: BigInt): Either[PoseidonMerkleError, Fr] = checkPosition(position).map(leaves.getOrElse(_, Fr.Zero))

  def insert(position: BigInt, leaf: Fr): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    checkPosition(position).map { _ =>
      @tailrec def climb(level: Int, idx: BigInt, digest: Fr, acc: Vector[TreeMap[BigInt, Fr]]): Vector[TreeMap[BigInt, Fr]] = {
        val here = acc.lift(level).map(m => if (digest == zeroHashes(level)) m - idx else m.updated(idx, digest))
        val acc1 = here.fold(acc)(acc.updated(level, _))
        if (level == depth) acc1
        else {
          val right = position.testBit(level)
          val sibling = here.flatMap(_.get(if (right) idx - 1 else idx + 1)).getOrElse(zeroHashes(level))
          climb(level + 1, idx >> 1, if (right) Poseidon.compress(sibling, digest) else Poseidon.compress(digest, sibling), acc1)
        }
      }
      new PoseidonMerkleTree(
        depth,
        zeroHashes,
        climb(0, position, leaf, nodes),
        if (leaf == Fr.Zero) leaves - position else leaves.updated(position, leaf)
      )
    }

  def insertAll(entries: Iterable[(BigInt, Fr)]): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    entries.toList.foldM(this) { case (t, (p, l)) => t.insert(p, l) }

  def proof(position: BigInt): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    checkPosition(position).flatMap { _ =>
      @tailrec def collect(level: Int, idx: BigInt, acc: List[Fr]): List[Fr] =
        if (level == depth) acc
        else
          collect(
            level + 1,
            idx >> 1,
            node(level, if (position.testBit(level)) idx - 1 else idx + 1) :: acc
          )
      PoseidonMerkleProof.from(position, collect(0, position, Nil).toVector)
    }

  def absenceProof(position: BigInt): Either[PoseidonMerkleError, PoseidonMerkleProof] =
    leafAt(position)
      .flatMap(l => Either.cond(l == Fr.Zero, (), PoseidonMerkleError.OccupiedPosition(position)))
      .flatMap(_ => proof(position))
}

object PoseidonMerkleTree {

  val DefaultDepth: Int = 32
  val MaxDepth: Int = xyz.kd5ujc.models.limits.PoseidonMaxDepth

  def zeroHashes(depth: Int): Vector[Fr] =
    (1 to depth).scanLeft(Fr.Zero)((prev, _) => Poseidon.compress(prev, prev)).toVector

  def empty(depth: Int = DefaultDepth): Either[PoseidonMerkleError, PoseidonMerkleTree] = empty(depth, Ordering.BigInt)

  /** [[empty]], comparing positions in `positions`: production passes `Ordering.BigInt`, and a test counts its calls. */
  private[poseidon] def empty(depth: Int, positions: Ordering[BigInt]): Either[PoseidonMerkleError, PoseidonMerkleTree] =
    Either.cond(
      depth >= 1 && depth <= MaxDepth,
      new PoseidonMerkleTree(
        depth,
        zeroHashes(depth),
        Vector.fill(depth + 1)(TreeMap.empty[BigInt, Fr](using positions)),
        TreeMap.empty(using positions)
      ),
      PoseidonMerkleError.Depth(depth)
    )

  /** Total: the proof's position fits its depth and every value is a field element, by construction. */
  def computeRoot(leaf: Fr, proof: PoseidonMerkleProof): Fr =
    proof.siblings.reverse.zipWithIndex.foldLeft(leaf) {
      case (cur, (sib, level)) =>
        if (proof.position.testBit(level)) Poseidon.compress(sib, cur) else Poseidon.compress(cur, sib)
    }

  def verifyInclusion(leaf: Fr, proof:                 PoseidonMerkleProof, root: Fr): Boolean = computeRoot(leaf, proof) == root
  def verifyAbsence(proof:  PoseidonMerkleProof, root: Fr): Boolean = verifyInclusion(Fr.Zero, proof, root)
}
