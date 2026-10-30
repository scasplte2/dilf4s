package xyz.kd5ujc.accumulators.merkle

import cats.Eval
import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{Canonical, CanonicalError, Cbor}
import xyz.kd5ujc.core.hash.{Digest, DomainTag, Hasher, TaggedHash}

import scodec.bits.ByteVector

/** The three hash functions of a Merkle profile (docs/specs/merkle.md §2). */
trait MerkleHashing[H] {
  def empty: Digest[H]
  def leaf(data: ByteVector): Digest[H]
  def node(left: Digest[H], right: Digest[H]): Digest[H]
}

object MerkleHashing {

  def apply[H](implicit M: MerkleHashing[H]): MerkleHashing[H] = M

  private[kd5ujc] val LeafTag = "dilf4s/merkle/leaf/v1"
  private[kd5ujc] val NodeTag = "dilf4s/merkle/node/v1"
  private[kd5ujc] val EmptyTag = "dilf4s/merkle/empty/v1"
  private[kd5ujc] val Tags: List[String] = List(LeafTag, NodeTag, EmptyTag)

  private def tag(name: String): DomainTag = DomainTag.literal(name)

  /** The dilf4s profile: tagged hashes per role. */
  implicit def tagged[H: Hasher]: MerkleHashing[H] = new MerkleHashing[H] {
    private val leafTag = TaggedHash[H](tag(LeafTag))
    private val nodeTag = TaggedHash[H](tag(NodeTag))
    val empty: Digest[H] = TaggedHash[H](tag(EmptyTag)).apply(ByteVector.empty)
    def leaf(data: ByteVector): Digest[H] = leafTag(data)
    def node(left: Digest[H], right: Digest[H]): Digest[H] = nodeTag(left.bytes ++ right.bytes)
  }

  /** RFC 6962 one-byte prefixes, for interoperability tests against Certificate Transparency vectors. */
  def rfc6962[H](implicit H: Hasher[H]): MerkleHashing[H] = new MerkleHashing[H] {
    val empty: Digest[H] = H.digest(ByteVector.empty)
    def leaf(data: ByteVector): Digest[H] = H.digest(ByteVector(0) ++ data)
    def node(left: Digest[H], right: Digest[H]): Digest[H] = H.digest(ByteVector(1) ++ left.bytes ++ right.bytes)
  }
}

sealed trait MerkleError extends Product with Serializable

object MerkleError {
  final case class IndexOutOfRange(index: Long, size: Long) extends MerkleError
  final case class InvalidSizes(oldSize: Long, newSize: Long) extends MerkleError
  final case class InvalidProof(reason: String) extends MerkleError
  final case class RootMismatch(expected: String, computed: String) extends MerkleError
  final case class Encoding(reason: String) extends MerkleError
}

/** An audit path for leaf `index` of a tree of `size` leaves; `0 <= index < size` by construction. */
final case class InclusionProof[H] private[merkle] (index: Long, size: Long, path: Vector[Digest[H]])

object InclusionProof {
  def from[H](index: Long, size: Long, path: Vector[Digest[H]]): Either[MerkleError, InclusionProof[H]] =
    Either.cond(index >= 0 && index < size, InclusionProof(index, size, path), MerkleError.IndexOutOfRange(index, size))
}

/** A consistency path between tree sizes `oldSize <= newSize`; both non-negative by construction. */
final case class ConsistencyProof[H] private[merkle] (oldSize: Long, newSize: Long, path: Vector[Digest[H]])

object ConsistencyProof {
  def from[H](oldSize: Long, newSize: Long, path: Vector[Digest[H]]): Either[MerkleError, ConsistencyProof[H]] =
    Either.cond(oldSize >= 0 && oldSize <= newSize, ConsistencyProof(oldSize, newSize, path), MerkleError.IndexOutOfRange(oldSize, newSize))
}

/**
 * An immutable RFC 9162 Merkle tree (docs/specs/merkle.md), stored as levels of node hashes: `levels(0)` are the leaf
 * hashes, and each level pairs the one below, promoting an unpaired last node. Appending updates one node per level.
 */
final class MerkleTree[H] private (private val levels: Vector[Vector[Digest[H]]])(implicit M: MerkleHashing[H]) {

  def size: Long = levels.headOption.fold(0L)(_.size.toLong)

  def root: Digest[H] = levels.lastOption.flatMap(_.headOption).getOrElse(M.empty)

  def leafHash(index: Long): Either[MerkleError, Digest[H]] =
    Either.cond(index >= 0 && index < size, levels(0)(index.toInt), MerkleError.IndexOutOfRange(index, size))

  def append(leafHash: Digest[H]): MerkleTree[H] =
    new MerkleTree[H](MerkleTree.propagate(levels.headOption.fold(Vector(Vector(leafHash)))(l0 => levels.updated(0, l0 :+ leafHash)), 0))

  def appendAll(leafHashes: Iterable[Digest[H]]): MerkleTree[H] = leafHashes.foldLeft(this)(_ `append` _)

  /** All leaf hashes, in order. */
  def leafHashes: Vector[Digest[H]] = levels.headOption.getOrElse(Vector.empty)

  /** Every index holding `leafHash`; leaves may repeat. */
  def indicesOf(leafHash: Digest[H]): List[Long] =
    leafHashes.zipWithIndex.collect { case (h, i) if h == leafHash => i.toLong }.toList

  /** Replaces one leaf, recomputing only its path to the root. */
  def update(index: Long, leafHash: Digest[H]): Either[MerkleError, MerkleTree[H]] =
    Either.cond(
      index >= 0 && index < size,
      new MerkleTree[H](MerkleTree.repath(levels.updated(0, levels(0).updated(index.toInt, leafHash)), 0, index.toInt)),
      MerkleError.IndexOutOfRange(index, size)
    )

  /**
   * Inserts leaves at the front. Every index shifts, so the result is not an append-only extension and consistency
   * proofs from earlier sizes no longer apply. Rebuilds the tree.
   */
  def prependAll(leafHashes: Iterable[Digest[H]]): MerkleTree[H] = MerkleTree.fromLeafHashes[H](leafHashes ++ this.leafHashes)

  /** Removes one leaf, shifting later indices down (not append-only; rebuilds the tree). */
  def removeAt(index: Long): Either[MerkleError, MerkleTree[H]] =
    Either.cond(
      index >= 0 && index < size,
      MerkleTree.fromLeafHashes[H](leafHashes.patch(index.toInt, Nil, 1)),
      MerkleError.IndexOutOfRange(index, size)
    )

  /** RFC 9162 §2.1.3.1 audit path, read from the stored levels. */
  def inclusionProof(index: Long): Either[MerkleError, InclusionProof[H]] =
    Either.cond(
      index >= 0 && index < size,
      InclusionProof(index, size, auditPath(index.toInt, 0, Vector.empty)),
      MerkleError.IndexOutOfRange(index, size)
    )

  @tailrec
  private def auditPath(i: Int, level: Int, acc: Vector[Digest[H]]): Vector[Digest[H]] =
    if (level >= levels.size - 1) acc
    else {
      val nodes = levels(level)
      val sibling =
        if (i % 2 == 1) Some(nodes(i - 1))
        else Option.when(i + 1 < nodes.size)(nodes(i + 1))
      auditPath(i / 2, level + 1, acc ++ sibling)
    }

  /** RFC 9162 §2.1.4.1 consistency path from `oldSize` to this tree. */
  def consistencyProof(oldSize: Long): Either[MerkleError, ConsistencyProof[H]] =
    if (oldSize < 0 || oldSize > size) MerkleError.InvalidSizes(oldSize, size).asLeft
    else if (oldSize == 0 || oldSize == size) ConsistencyProof(oldSize, size, Vector.empty[Digest[H]]).asRight
    else ConsistencyProof(oldSize, size, subproof(oldSize, 0L, size, complete = true).value).asRight

  private def subproof(m: Long, start: Long, n: Long, complete: Boolean): Eval[Vector[Digest[H]]] =
    if (m == n) Eval.now(if (complete) Vector.empty else Vector(subtree(start, n)))
    else {
      val k = MerkleTree.splitPoint(n)
      if (m <= k) Eval.defer(subproof(m, start, k, complete)).map(_ :+ subtree(start + k, n - k))
      else Eval.defer(subproof(m - k, start + k, n - k, complete = false)).map(_ :+ subtree(start, k))
    }

  /** MTH of `length` leaves from `start`: aligned power-of-two blocks are stored nodes; the rest splits per RFC 9162. */
  private def subtree(start: Long, length: Long): Digest[H] = subtreeEval(start, length).value

  private def subtreeEval(start: Long, length: Long): Eval[Digest[H]] = {
    val level = java.lang.Long.numberOfTrailingZeros(length)
    if (java.lang.Long.bitCount(length) == 1 && start % length == 0) Eval.now(levels(level)((start >> level).toInt))
    else {
      val k = MerkleTree.splitPoint(length)
      (Eval.defer(subtreeEval(start, k)), Eval.defer(subtreeEval(start + k, length - k))).mapN(M.node)
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that: MerkleTree[_] => levels == that.levels
    case _                   => false
  }

  override def hashCode: Int = levels.hashCode

  override def toString: String = s"MerkleTree(size=$size, root=${root.hex})"
}

object MerkleTree {

  def empty[H: MerkleHashing]: MerkleTree[H] = new MerkleTree[H](Vector.empty)

  def fromLeafHashes[H: MerkleHashing](leafHashes: Iterable[Digest[H]]): MerkleTree[H] = empty[H].appendAll(leafHashes)

  /** Leaves as typed values: each leaf hashes the canonical encoding of its value. */
  def fromLeaves[H, A: Canonical](leaves: Iterable[A])(implicit M: MerkleHashing[H]): Either[CanonicalError, MerkleTree[H]] =
    leaves.toList.traverse(leaf[H, A](_)).map(fromLeafHashes[H])

  def leaf[H, A](value: A)(implicit M: MerkleHashing[H], C: Canonical[A]): Either[CanonicalError, Digest[H]] =
    C.canonical(value).map(v => M.leaf(Cbor.encode(v)))

  /** Largest power of two strictly below `n` (n >= 2). */
  private[merkle] def splitPoint(n: Long): Long = java.lang.Long.highestOneBit(n - 1)

  /** After node `i` of level `k` changed, recompute its ancestors. */
  @tailrec
  private def repath[H](levels: Vector[Vector[Digest[H]]], k: Int, i: Int)(implicit M: MerkleHashing[H]): Vector[Vector[Digest[H]]] =
    if (k + 1 >= levels.size) levels
    else {
      val nodes = levels(k)
      val left = i - (i % 2)
      val parent = if (left + 1 < nodes.size) M.node(nodes(left), nodes(left + 1)) else nodes(left)
      repath(levels.updated(k + 1, levels(k + 1).updated(i / 2, parent)), k + 1, i / 2)
    }

  /** After level `k` changed at its end, recompute the last node of every level above it. */
  @tailrec
  private def propagate[H](levels: Vector[Vector[Digest[H]]], k: Int)(implicit M: MerkleHashing[H]): Vector[Vector[Digest[H]]] = {
    val nodes = levels(k)
    if (nodes.size <= 1) levels.take(k + 1)
    else {
      val parentIndex = (nodes.size - 1) / 2
      val parent = if (nodes.size % 2 == 1) nodes(nodes.size - 1) else M.node(nodes(nodes.size - 2), nodes(nodes.size - 1))
      val above = levels.lift(k + 1).getOrElse(Vector.empty)
      val updated = if (parentIndex < above.size) above.updated(parentIndex, parent) else above :+ parent
      propagate(if (k + 1 < levels.size) levels.updated(k + 1, updated) else levels :+ updated, k + 1)
    }
  }
}

/** Pure verification of RFC 9162 proofs. */
object MerkleVerifier {

  /** RFC 9162 §2.1.3.2. */
  def inclusion[H](root: Digest[H], leafHash: Digest[H], proof: InclusionProof[H])(
    implicit M: MerkleHashing[H]
  ): Either[MerkleError, Unit] =
    if (proof.index < 0 || proof.index >= proof.size) MerkleError.IndexOutOfRange(proof.index, proof.size).asLeft
    else
      walkInclusion(proof.path.toList, proof.index, proof.size - 1, leafHash).flatMap {
        case (sn, computed) =>
          if (sn != 0) MerkleError.InvalidProof("path is too short for the tree size").asLeft
          else Either.cond(computed == root, (), MerkleError.RootMismatch(root.hex, computed.hex))
      }

  @tailrec
  private def walkInclusion[H](path: List[Digest[H]], fn: Long, sn: Long, r: Digest[H])(
    implicit M: MerkleHashing[H]
  ): Either[MerkleError, (Long, Digest[H])] =
    path match {
      case Nil               => (sn, r).asRight
      case _ :: _ if sn == 0 => MerkleError.InvalidProof("path is too long for the tree size").asLeft
      case p :: rest =>
        if ((fn & 1) == 1 || fn == sn) {
          val (f, s) = if ((fn & 1) == 0) shiftUntilOddOrZero(fn, sn) else (fn, sn)
          walkInclusion(rest, f >> 1, s >> 1, M.node(p, r))
        } else walkInclusion(rest, fn >> 1, sn >> 1, M.node(r, p))
    }

  /** RFC 9162 §2.1.4.2. */
  def consistency[H](oldRoot: Digest[H], newRoot: Digest[H], proof: ConsistencyProof[H])(
    implicit M: MerkleHashing[H]
  ): Either[MerkleError, Unit] = {
    val (m, n) = (proof.oldSize, proof.newSize)
    if (m < 0 || m > n) MerkleError.InvalidSizes(m, n).asLeft
    else if (m == 0)
      Either.cond(proof.path.isEmpty && oldRoot == M.empty, (), MerkleError.InvalidProof("size 0 needs an empty proof and the empty root"))
    else if (m == n)
      Either.cond(proof.path.isEmpty && oldRoot == newRoot, (), MerkleError.InvalidProof("equal sizes need an empty proof and equal roots"))
    else {
      val path = if (java.lang.Long.bitCount(m) == 1) oldRoot :: proof.path.toList else proof.path.toList
      path match {
        case Nil => MerkleError.InvalidProof("empty consistency path").asLeft
        case first :: rest =>
          val (fn, sn) = shiftWhileOdd(m - 1, n - 1)
          walkConsistency(rest, fn, sn, first, first).flatMap {
            case (s, fr, sr) =>
              if (s != 0) MerkleError.InvalidProof("path is too short for the sizes").asLeft
              else if (fr != oldRoot) MerkleError.RootMismatch(oldRoot.hex, fr.hex).asLeft
              else Either.cond(sr == newRoot, (), MerkleError.RootMismatch(newRoot.hex, sr.hex))
          }
      }
    }
  }

  @tailrec
  private def walkConsistency[H](path: List[Digest[H]], fn: Long, sn: Long, fr: Digest[H], sr: Digest[H])(
    implicit M: MerkleHashing[H]
  ): Either[MerkleError, (Long, Digest[H], Digest[H])] =
    path match {
      case Nil               => (sn, fr, sr).asRight
      case _ :: _ if sn == 0 => MerkleError.InvalidProof("path is too long for the sizes").asLeft
      case c :: rest =>
        if ((fn & 1) == 1 || fn == sn) {
          val (f, s) = if ((fn & 1) == 0) shiftUntilOddOrZero(fn, sn) else (fn, sn)
          walkConsistency(rest, f >> 1, s >> 1, M.node(c, fr), M.node(c, sr))
        } else walkConsistency(rest, fn >> 1, sn >> 1, fr, M.node(sr, c))
    }

  @tailrec
  private def shiftUntilOddOrZero(fn: Long, sn: Long): (Long, Long) =
    if ((fn & 1) == 1 || fn == 0) (fn, sn) else shiftUntilOddOrZero(fn >> 1, sn >> 1)

  @tailrec
  private def shiftWhileOdd(fn: Long, sn: Long): (Long, Long) =
    if ((fn & 1) == 1) shiftWhileOdd(fn >> 1, sn >> 1) else (fn, sn)
}
