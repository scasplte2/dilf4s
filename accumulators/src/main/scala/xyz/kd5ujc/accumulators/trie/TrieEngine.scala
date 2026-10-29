package xyz.kd5ujc.accumulators.trie

import cats.Monad
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.hash.Digest

import scodec.bits.BitVector

/** One write to the trie: `Some` inserts or replaces the value hash, `None` deletes. */
final case class TrieOp[H](path: BitVector, value: Option[Digest[H]])

/**
 * The result of applying operations: the new root, and every node created on the way that the new root still reaches.
 * Nodes are content-addressed, so a "created" node may already exist in the store (identical subtrees deduplicate).
 */
final case class TrieDelta[H](root: Option[Digest[H]], created: Map[Digest[H], Node[H]])

/**
 * Insert, delete and lookup over any node source (docs/specs/trie.md §2). Descents are `tailRecM` loops and rebuilds
 * fold an explicit frame stack, so path depth never consumes JVM stack.
 */
object TrieEngine {

  /** A branch passed on the way down, and the bit taken out of it. */
  final private case class Frame[H](segment: BitVector, value: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]], bit: Boolean)

  /** Where a descent stopped: the node there (if any), the path still unconsumed at it, and the frames above it. */
  final private case class Stop[H](frames: List[Frame[H]], node: Option[Node[H]], remaining: BitVector)

  def get[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], path: BitVector): F[Option[Digest[H]]] =
    root.fold(Option.empty[Digest[H]].pure[F]) { start =>
      (start, path).tailRecM { case (hash, remaining) =>
        lookup(hash).map {
          case l: Leaf[H] => Option.when(l.segment == remaining)(l.value).asRight[(Digest[H], BitVector)]
          case b: Branch[H] =>
            if (!remaining.startsWith(b.segment)) Option.empty[Digest[H]].asRight
            else {
              val rest = remaining.drop(b.segment.size)
              if (rest.isEmpty) b.value.asRight
              else b.child(rest.head).fold(Option.empty[Digest[H]].asRight[(Digest[H], BitVector)])(c => (c, rest.tail).asLeft)
            }
        }
      }
    }

  /** Applies operations in order; later operations see earlier ones. */
  def apply[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], ops: List[TrieOp[H]])(implicit
    T: TrieHashing[H]
  ): F[TrieDelta[H]] =
    ops
      .foldLeftM((root, Map.empty[Digest[H], Node[H]])) { case ((r, created), op) =>
        val overlay: Digest[H] => F[Node[H]] = h => created.get(h).fold(lookup(h))(_.pure[F])
        applyOne(overlay, r, op).map { case (newRoot, made) => (newRoot, created ++ made.map(n => n.hash -> n)) }
      }
      .map { case (newRoot, created) => TrieDelta(newRoot, reachable(newRoot, created)) }

  /** The created nodes the root still reaches; superseded intermediates are dropped. */
  private def reachable[H](root: Option[Digest[H]], created: Map[Digest[H], Node[H]]): Map[Digest[H], Node[H]] = {
    @scala.annotation.tailrec
    def go(todo: List[Digest[H]], acc: Map[Digest[H], Node[H]]): Map[Digest[H], Node[H]] =
      todo match {
        case Nil                              => acc
        case h :: rest if acc.contains(h)     => go(rest, acc)
        case h :: rest =>
          created.get(h) match {
            case None                => go(rest, acc)
            case Some(n: Leaf[H])    => go(rest, acc.updated(h, n))
            case Some(b: Branch[H])  => go(b.left.toList ++ b.right.toList ++ rest, acc.updated(h, b))
          }
      }
    go(root.toList, Map.empty)
  }

  private def applyOne[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], op: TrieOp[H])(implicit
    T: TrieHashing[H]
  ): F[(Option[Digest[H]], List[Node[H]])] =
    descend(lookup, root, op.path).flatMap { stop =>
      op.value match {
        case Some(v) =>
          val (top, made) = insertAt(stop, v)
          rebuild(lookup, stop.frames, Some(top), made)
        case None =>
          deleteAt(lookup, stop).flatMap {
            case None                    => (root, List.empty[Node[H]]).pure[F] // key absent: unchanged
            case Some((replacement, made)) => rebuild(lookup, stop.frames, replacement, made)
          }
      }
    }

  private def descend[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], root: Option[Digest[H]], path: BitVector): F[Stop[H]] =
    root.fold(Stop[H](Nil, None, path).pure[F]) { start =>
      (start, path, List.empty[Frame[H]]).tailRecM { case (hash, remaining, frames) =>
        lookup(hash).map {
          case l: Leaf[H] => Stop(frames, Some(l), remaining).asRight
          case b: Branch[H] =>
            val rest = remaining.drop(b.segment.size)
            if (remaining.startsWith(b.segment) && rest.nonEmpty)
              b.child(rest.head) match {
                case Some(c) => (c, rest.tail, Frame(b.segment, b.value, b.left, b.right, rest.head) :: frames).asLeft
                case None    => Stop(frames, Some(b), remaining).asRight
              }
            else Stop(frames, Some(b), remaining).asRight
        }
      }
    }

  private def withChild[H](bit: Boolean, child: Option[Digest[H]], left: Option[Digest[H]], right: Option[Digest[H]]) =
    if (bit) (left, child) else (child, right)

  /** The subtree that replaces the stop node after inserting `value` at the stop's remaining path. */
  private def insertAt[H](stop: Stop[H], value: Digest[H])(implicit T: TrieHashing[H]): (Node[H], List[Node[H]]) = {
    val rem = stop.remaining
    def leafAt(path: BitVector, v: Digest[H]) = Node.leaf(path, v)
    def fork(shared: BitVector, branchValue: Option[Digest[H]], a: (Boolean, Node[H]), b: Option[(Boolean, Node[H])]): (Node[H], List[Node[H]]) = {
      val children = a :: b.toList
      val left = children.collectFirst { case (false, n) => n.hash }
      val right = children.collectFirst { case (true, n) => n.hash }
      val top = Node.branch(shared, branchValue, left, right)
      (top, top :: children.map(_._2))
    }
    stop.node match {
      case None =>
        val l = leafAt(rem, value)
        (l, List(l))
      case Some(l: Leaf[H]) if l.segment == rem =>
        val n = leafAt(rem, value)
        (n, List(n))
      case Some(l: Leaf[H]) =>
        val c = Bits.commonPrefix(l.segment, rem)
        val (shared, oldRest, newRest) = (l.segment.take(c), l.segment.drop(c), rem.drop(c))
        if (newRest.isEmpty) fork(shared, Some(value), (oldRest.head, leafAt(oldRest.tail, l.value)), None)
        else if (oldRest.isEmpty) fork(shared, Some(l.value), (newRest.head, leafAt(newRest.tail, value)), None)
        else fork(shared, None, (oldRest.head, leafAt(oldRest.tail, l.value)), Some((newRest.head, leafAt(newRest.tail, value))))
      case Some(b: Branch[H]) =>
        val c = Bits.commonPrefix(b.segment, rem)
        if (c == b.segment.size) {
          val rest = rem.drop(c)
          if (rest.isEmpty) {
            val n = Node.branch(b.segment, Some(value), b.left, b.right)
            (n, List(n))
          } else {
            val leaf = leafAt(rest.tail, value)
            val (left, right) = withChild(rest.head, Some(leaf.hash), b.left, b.right)
            val n = Node.branch(b.segment, b.value, left, right)
            (n, List(n, leaf))
          }
        } else {
          val (shared, oldRest, newRest) = (b.segment.take(c), b.segment.drop(c), rem.drop(c))
          val lowered = Node.branch(oldRest.tail, b.value, b.left, b.right)
          if (newRest.isEmpty) fork(shared, Some(value), (oldRest.head, lowered), None)
          else fork(shared, None, (oldRest.head, lowered), Some((newRest.head, leafAt(newRest.tail, value))))
        }
    }
  }

  /** `None` if the key is absent (no change); otherwise the replacement subtree, possibly empty. */
  private def deleteAt[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], stop: Stop[H])(implicit
    T: TrieHashing[H]
  ): F[Option[(Option[Node[H]], List[Node[H]])]] =
    stop.node match {
      case Some(l: Leaf[H]) if l.segment == stop.remaining => Option((Option.empty[Node[H]], List.empty[Node[H]])).pure[F]
      case Some(b: Branch[H]) if b.segment == stop.remaining && b.value.isDefined =>
        normalize(lookup, b.segment, None, b.left, b.right, None).map(r => Option((r, r.toList)))
      case _ => Option.empty[(Option[Node[H]], List[Node[H]])].pure[F]
    }

  /**
   * Restores the branch invariant: no children and no value is nothing; a value alone is a leaf; one child and no value
   * merges into the child. `known` supplies a child node already in hand (just rebuilt, maybe not in the store yet).
   */
  private def normalize[F[_]: Monad, H](
    lookup:  Digest[H] => F[Node[H]],
    segment: BitVector,
    value:   Option[Digest[H]],
    left:    Option[Digest[H]],
    right:   Option[Digest[H]],
    known:   Option[Node[H]]
  )(implicit T: TrieHashing[H]): F[Option[Node[H]]] = {
    def fetch(h: Digest[H]): F[Node[H]] = known.filter(_.hash == h).fold(lookup(h))(_.pure[F])
    def merge(bit: Boolean, h: Digest[H]): F[Option[Node[H]]] =
      fetch(h).map {
        case l: Leaf[H]   => Some(Node.leaf(segment ++ BitVector.bit(bit) ++ l.segment, l.value))
        case b: Branch[H] => Some(Node.branch(segment ++ BitVector.bit(bit) ++ b.segment, b.value, b.left, b.right))
      }
    (value, left, right) match {
      case (None, None, None)       => Option.empty[Node[H]].pure[F]
      case (Some(v), None, None)    => Option[Node[H]](Node.leaf(segment, v)).pure[F]
      case (None, Some(l), None)    => merge(bit = false, l)
      case (None, None, Some(r))    => merge(bit = true, r)
      case _                        => Option[Node[H]](Node.branch(segment, value, left, right)).pure[F]
    }
  }

  /** Re-hashes the frames bottom-up around a replaced subtree, normalising each branch it passes. */
  private def rebuild[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], frames: List[Frame[H]], replacement: Option[Node[H]], made: List[Node[H]])(implicit
    T: TrieHashing[H]
  ): F[(Option[Digest[H]], List[Node[H]])] =
    frames
      .foldLeftM((replacement, made)) { case ((child, acc), f) =>
        val (left, right) = withChild(f.bit, child.map(_.hash), f.left, f.right)
        normalize(lookup, f.segment, f.value, left, right, child).map(parent => (parent, acc ++ parent.toList))
      }
      .map { case (top, acc) => (top.map(_.hash), acc) }

  /** Every node reachable from `root` (used to load whole subtrees for proofs and scans), gathered by a work list. */
  def collect[F[_]: Monad, H](lookup: Digest[H] => F[Node[H]], roots: List[Digest[H]]): F[Map[Digest[H], Node[H]]] =
    (roots, Map.empty[Digest[H], Node[H]]).tailRecM {
      case (Nil, acc)                        => acc.asRight[(List[Digest[H]], Map[Digest[H], Node[H]])].pure[F]
      case (h :: rest, acc) if acc.contains(h) => (rest, acc).asLeft[Map[Digest[H], Node[H]]].pure[F]
      case (h :: rest, acc) =>
        lookup(h).map {
          case l: Leaf[H]   => (rest, acc.updated(h, l)).asLeft
          case b: Branch[H] => (b.left.toList ++ b.right.toList ++ rest, acc.updated(h, b)).asLeft
        }
    }

}
