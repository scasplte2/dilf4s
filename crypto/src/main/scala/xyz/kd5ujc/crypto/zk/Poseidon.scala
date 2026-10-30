package xyz.kd5ujc.crypto.zk

import cats.syntax.all._

import scala.annotation.tailrec

sealed trait PoseidonError extends Product with Serializable

object PoseidonError {
  final case class InputCount(count: Int, max: Int) extends PoseidonError
  final case class NonCanonical(role: String, value: BigInt) extends PoseidonError
  final case class Position(position: BigInt, capacity: BigInt) extends PoseidonError
  final case class Depth(depth: Int) extends PoseidonError
  final case class OccupiedPosition(position: BigInt) extends PoseidonError
}

/**
 * Poseidon over the BN254 scalar field, byte-compatible with circomlib / circomlibjs (`poseidon([...])`): x^5 S-box,
 * 8 full rounds, width `t = inputs + 1`, state `[0, in_0, ..]`, output `state[0]`. Widths 2..5 (1..4 inputs).
 *
 * **Public data only**: `BigInt` arithmetic is variable time. Never hash secrets (note values, nullifier keys) here.
 */
object Poseidon {

  val R: BigInt = Bn254.R
  val MaxInputs: Int = 4

  def isCanonical(x: BigInt): Boolean = x >= 0 && x < R

  def hash(inputs: Seq[BigInt]): Either[PoseidonError, BigInt] =
    for {
      params <- PoseidonConstants.byWidth.get(inputs.size + 1).toRight(PoseidonError.InputCount(inputs.size, MaxInputs))
      _ <- inputs.zipWithIndex.traverse_ { case (x, i) => Either.cond(isCanonical(x), (), PoseidonError.NonCanonical(s"input[$i]", x)) }
    } yield permute(params, BigInt(0) +: inputs.toVector)

  /** `hash([l, r])` for canonical `l`, `r` (width 3): total, for Merkle nodes whose inputs are already field elements. */
  private[zk] def compressCanonical(l: BigInt, r: BigInt): BigInt = permute(PoseidonConstants.Width3, Vector(BigInt(0), l, r))

  def compress(l: BigInt, r: BigInt): Either[PoseidonError, BigInt] = hash(List(l, r))

  private def pow5(a: BigInt): BigInt = { val a2 = a * a % R; a2 * a2 % R * a % R }

  private def permute(params: PoseidonConstants.Params, state: Vector[BigInt]): BigInt = {
    val t = state.size
    val (c, m) = (params.roundConstants, params.mds)
    val (rf, rp) = (PoseidonConstants.FullRounds, params.partialRounds)
    @tailrec def loop(r: Int, s: Vector[BigInt]): Vector[BigInt] =
      if (r >= rf + rp) s
      else {
        val ark = Vector.tabulate(t)(i => (s(i) + c(r * t + i)) % R)
        val sbox = if (r < rf / 2 || r >= rf / 2 + rp) ark.map(pow5) else ark.updated(0, pow5(ark(0)))
        loop(r + 1, m.map(row => row.zip(sbox).foldLeft(BigInt(0)) { case (acc, (mij, x)) => (acc + mij * x) % R }))
      }
    loop(0, state).headOption.getOrElse(BigInt(0))
  }
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

  private def checkPosition(position: BigInt): Either[PoseidonError, BigInt] =
    Either.cond(position >= 0 && position < capacity, position, PoseidonError.Position(position, capacity))

  def leafAt(position: BigInt): Either[PoseidonError, BigInt] = checkPosition(position).map(leaves.getOrElse(_, BigInt(0)))

  def insert(position: BigInt, leaf: BigInt): Either[PoseidonError, PoseidonMerkleTree] =
    for {
      _ <- checkPosition(position)
      _ <- Either.cond(Poseidon.isCanonical(leaf), (), PoseidonError.NonCanonical(s"leaf at $position", leaf))
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

  def insertAll(entries: Iterable[(BigInt, BigInt)]): Either[PoseidonError, PoseidonMerkleTree] =
    entries.toList.foldM(this) { case (t, (p, l)) => t.insert(p, l) }

  def proof(position: BigInt): Either[PoseidonError, PoseidonMerkleProof] =
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

  def absenceProof(position: BigInt): Either[PoseidonError, PoseidonMerkleProof] =
    leafAt(position).flatMap(l => Either.cond(l == 0, (), PoseidonError.OccupiedPosition(position))).flatMap(_ => proof(position))
}

object PoseidonMerkleTree {

  val DefaultDepth: Int = 32

  def zeroHashes(depth: Int): Vector[BigInt] =
    (1 to depth).scanLeft(BigInt(0))((prev, _) => Poseidon.compressCanonical(prev, prev)).toVector

  def empty(depth: Int = DefaultDepth): Either[PoseidonError, PoseidonMerkleTree] =
    Either.cond(
      depth >= 1 && depth <= 256,
      new PoseidonMerkleTree(depth, zeroHashes(depth), Map.empty, Map.empty),
      PoseidonError.Depth(depth)
    )

  def computeRoot(leaf: BigInt, proof: PoseidonMerkleProof): Either[PoseidonError, BigInt] =
    for {
      _ <- Either.cond(Poseidon.isCanonical(leaf), (), PoseidonError.NonCanonical("leaf", leaf))
      _ <- proof.siblings.zipWithIndex.traverse_ {
        case (s, i) => Either.cond(Poseidon.isCanonical(s), (), PoseidonError.NonCanonical(s"sibling[$i]", s))
      }
      _ <- Either.cond(
        proof.position >= 0 && proof.position < (BigInt(1) << proof.depth),
        (),
        PoseidonError.Position(proof.position, BigInt(1) << proof.depth)
      )
    } yield
      proof.siblings.reverse.zipWithIndex.foldLeft(leaf) {
        case (cur, (sib, level)) =>
          if (proof.position.testBit(level)) Poseidon.compressCanonical(sib, cur) else Poseidon.compressCanonical(cur, sib)
      }

  def verifyInclusion(leaf: BigInt, proof:             PoseidonMerkleProof, root: BigInt): Boolean = computeRoot(leaf, proof).contains(root)
  def verifyAbsence(proof:  PoseidonMerkleProof, root: BigInt): Boolean = verifyInclusion(BigInt(0), proof, root)
}
