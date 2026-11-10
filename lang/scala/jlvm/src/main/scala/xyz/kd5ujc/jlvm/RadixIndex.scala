package xyz.kd5ujc.jlvm

import scala.annotation.tailrec

/**
 * An immutable crit-bit set over prefix-free typed keys. Branch bit positions strictly increase, and a short probe
 * stops before its end: a key of L bytes visits at most 8L branches per descent. Comparing the candidate reads at
 * most 2L bytes; insertion descends once more and rebuilds that path. Thus each operation takes O(L), including
 * absent keys and duplicates, without assumptions about key distribution or hashing.
 */
private[jlvm] final class RadixIndex private (root: Option[RadixIndex.Node]) {
  import RadixIndex._

  def contains(key: Key): (Boolean, Work) = root.fold((false, Work.Zero)) { node =>
    val (candidate, walked) = leaf(node, key, 0L, 0L)
    val (difference, compared) = differing(key, candidate)
    (difference.isEmpty, walked + compared)
  }

  def added(key: Key): Insertion = root match {
    case None => Insertion(new RadixIndex(Some(Leaf(key))), true, Work.Zero)
    case Some(node) =>
      val (candidate, walked) = leaf(node, key, 0L, 0L)
      val (difference, compared) = differing(key, candidate)
      difference.fold(Insertion(this, false, walked + compared)) { bit =>
        val (next, inserted) = insert(node, key, bit, Root, 0L)
        Insertion(new RadixIndex(Some(next)), true, walked + compared + inserted)
      }
  }
}

private[jlvm] object RadixIndex {

  /** Length-prefixed payloads are prefix-free, including payloads with zero bytes. Payload access must be O(1). */
  final class Key private (kind: Int, payloadLength: Long, payload: Long => Int) {
    val length: Long = 9L + payloadLength
    def byteAt(index: Long): Int =
      if (index < 8L) ((payloadLength >>> ((7L - index).toInt * 8)) & 255L).toInt
      else if (index == 8L) kind
      else payload(index - 9L)
    private[RadixIndex] def bitAt(index: Long): Boolean = (byteAt(index >>> 3) & (128 >>> (index.toInt & 7))) != 0
  }

  object Key {
    def apply(kind: Int, payloadLength: Long)(payload: Long => Int): Key = new Key(kind, payloadLength, payload)
  }

  /** Actual byte accesses and branch visits, carried by the production traversal without mutable counters. */
  final case class Work(bytes: Long, branches: Long) {
    def +(other: Work): Work = Work(bytes + other.bytes, branches + other.branches)
  }
  object Work { val Zero: Work = Work(0L, 0L) }

  final case class Insertion(index: RadixIndex, added: Boolean, work: Work)
  val empty: RadixIndex = new RadixIndex(None)

  private sealed trait Node { def representative: Key }
  private final case class Leaf(representative: Key) extends Node
  private final case class Branch(bit: Long, left: Node, right: Node, representative: Key) extends Node
  private def branch(bit: Long, left: Node, right: Node): Branch = Branch(bit, left, right, left.representative)

  private sealed trait Path
  private case object Root extends Path
  private final case class Parent(node: Branch, right: Boolean, rest: Path) extends Path

  @tailrec
  private def rebuild(child: Node, path: Path): Node = path match {
    case Root => child
    case Parent(parent, right, rest) =>
      val next = if (right) branch(parent.bit, parent.left, child) else branch(parent.bit, child, parent.right)
      rebuild(next, rest)
  }

  /** A short probe must not walk a suffix beyond its own end; a representative is available in constant time. */
  @tailrec
  private def leaf(node: Node, key: Key, bytes: Long, branches: Long): (Key, Work) = node match {
    case Leaf(candidate) => (candidate, Work(bytes, branches))
    case b: Branch =>
      if (b.bit >= key.length * 8L) (b.representative, Work(bytes, branches + 1L))
      else leaf(if (key.bitAt(b.bit)) b.right else b.left, key, bytes + 1L, branches + 1L)
  }

  private def differing(a: Key, b: Key): (Option[Long], Work) = {
    val end = math.min(a.length, b.length)
    @tailrec
    def go(i: Long): (Option[Long], Work) =
      if (i == end) (None, Work(2L * i, 0L))
      else {
        val different = a.byteAt(i) ^ b.byteAt(i)
        if (different == 0) go(i + 1L)
        else (Some(i * 8L + Integer.numberOfLeadingZeros(different) - 24L), Work(2L * (i + 1L), 0L))
      }
    go(0L)
  }

  @tailrec
  private def insert(
    node:    Node,
    key:     Key,
    bit:     Long,
    path:    Path,
    visited: Long
  ): (Node, Work) = node match {
    case b: Branch if b.bit < bit =>
      val right = key.bitAt(b.bit)
      insert(if (right) b.right else b.left, key, bit, Parent(b, right, path), visited + 1L)
    case _ =>
      val added = Leaf(key)
      val split = if (key.bitAt(bit)) branch(bit, node, added) else branch(bit, added, node)
      (rebuild(split, path), Work(visited + 1L, visited))
  }
}
