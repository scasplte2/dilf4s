package xyz.kd5ujc.accumulators.trie

import cats.effect.Concurrent
import cats.syntax.all._
import cats.{Eval, Parallel}

import xyz.kd5ujc.core.hash.Digest

import scodec.bits.{BitVector, ByteVector}

/**
 * An immutable authenticated dictionary (docs/specs/trie.md): a binary Patricia Merkle trie in `fixed` or `variable` key
 * mode. Nodes are content-addressed and reference counted, so the map holds exactly the nodes the root reaches. Values
 * live with the caller; the trie commits to their hashes.
 */
final class Trie[H] private (
  val mode:             KeyMode,
  private val rootHash: Option[Digest[H]],
  private val nodes:    Map[Digest[H], (Node[H], Int)]
)(implicit T: TrieHashing[H]) {

  private type Result[A] = Either[TrieError, A]

  def root: Digest[H] = rootHash.getOrElse(T.empty)
  def isEmpty: Boolean = rootHash.isEmpty
  def nodeCount: Int = nodes.size

  private[trie] def nodesForTest: Map[Digest[H], (Node[H], Int)] = nodes
  private[trie] def rootForTest: Option[Digest[H]] = rootHash

  private def node(h: Digest[H]): Result[Node[H]] = nodes.get(h).map(_._1).toRight(TrieError.MissingNode(h.hex))

  def get(key: ByteVector): Result[Option[Digest[H]]] =
    mode.path(key).flatMap(p => TrieEngine.get[Result, H](node, rootHash, p))

  def put(key:      ByteVector, valueHash: Digest[H]): Result[Trie[H]] = update(List(key -> Some(valueHash)))
  def putValue(key: ByteVector, value:     ByteVector): Result[Trie[H]] = put(key, T.value(value))
  def remove(key:   ByteVector): Result[Trie[H]] = update(List(key -> None))

  /** Applies writes in order (`None` deletes) and garbage-collects what the old root no longer shares. */
  def update(ops: List[(ByteVector, Option[Digest[H]])]): Result[Trie[H]] =
    for {
      trieOps <- ops.traverse { case (k, v) => mode.path(k).map(TrieOp(_, v)) }
      delta   <- TrieEngine.apply[Result, H](node, rootHash, trieOps)
      plan    <- RefCounting.plan[Result, H](h => nodes.get(h).asRight, delta, retainNewRoot = true, release = rootHash.toList)
    } yield
      new Trie[H](
        mode,
        delta.root,
        plan.foldLeft(nodes) {
          case (acc, (h, Some(entry))) => acc.updated(h, entry)
          case (acc, (h, None))        => acc.removed(h)
        }
      )

  def prove(
    keys:     List[ByteVector],
    prefixes: List[ByteVector] = Nil,
    ranges:   List[(ByteVector, Option[ByteVector])] = Nil
  ): Result[TrieProof[H]] =
    keys.traverse(mode.path).flatMap { keyPaths =>
      val queries = keyPaths.map(TrieProofs.Key.apply) ++ prefixes.map(p => TrieProofs.Prefix(p.bits)) ++
        ranges.map { case (f, u) => TrieProofs.Range(f.bits, u.map(_.bits)) }
      TrieProofs.prove[Result, H](node, rootHash, queries)
    }

  /** Every entry in key order. */
  def entries: Result[List[(ByteVector, Digest[H])]] =
    prove(Nil, List(ByteVector.empty)).flatMap(p =>
      TrieProofs
        .verify(mode, root, p)
        .flatMap(_.prefix(BitVector.empty).toRight(TrieError.InvalidProof("full listing must cover the trie")))
        .map(_.map { case (bits, v) => (bits.bytes, v) })
    )

  override def equals(other: Any): Boolean = other match {
    case that: Trie[_] => mode == that.mode && root == that.root
    case _             => false
  }
  override def hashCode: Int = root.hashCode
  override def toString: String = s"Trie($mode, root=${root.hex}, nodes=$nodeCount)"
}

object Trie {
  def empty[H: TrieHashing](mode: KeyMode): Trie[H] = new Trie[H](mode, None, Map.empty)

  /** Builds a trie from entries in one pass (later duplicates win); equal to inserting them in any order. */
  def fromEntries[H](mode: KeyMode, entries: List[(ByteVector, Digest[H])])(implicit T: TrieHashing[H]): Either[TrieError, Trie[H]] =
    prepare(mode, entries).map(sorted => assembleBuilt(mode, sorted.headOption.map(_ => Bulk.build[H](sorted, 0L).value)))

  /**
   * As [[fromEntries]], building the top `parallelDepth` levels of independent subtrees in parallel. The result is
   * identical; only the schedule differs.
   */
  def fromEntriesPar[F[_]: Concurrent: Parallel, H](mode: KeyMode, entries: List[(ByteVector, Digest[H])], parallelDepth: Int = 4)(
    implicit T: TrieHashing[H]
  ): F[Either[TrieError, Trie[H]]] =
    prepare(mode, entries).traverse { sorted =>
      sorted.headOption
        .traverse(_ => Bulk.buildPar[F, H](sorted, 0L, parallelDepth))
        .map(built => assembleBuilt(mode, built))
    }

  /** Validates keys, keeps the last value per key, and sorts by path (a prefix sorts before its extensions). */
  private def prepare[H](
    mode:    KeyMode,
    entries: List[(ByteVector, Digest[H])]
  ): Either[TrieError, Vector[(scodec.bits.BitVector, Digest[H])]] =
    entries.traverse { case (k, v) => mode.path(k).map(_ -> v) }
      .map(_.foldLeft(Map.empty[scodec.bits.BitVector, Digest[H]])(_ + _).toVector.sortWith((a, b) => Bulk.lessThan(a._1, b._1)))

  /** Every built node is new and has exactly the parents the build gave it, so counts follow from the edges. */
  private def assembleBuilt[H](mode: KeyMode, built: Option[(Node[H], List[Node[H]])])(implicit T: TrieHashing[H]): Trie[H] =
    built.fold(empty[H](mode)) {
      case (root, nodes) =>
        val unique = nodes.map(n => n.hash -> n).toMap
        val edges = unique.values.toList.flatMap {
          case b: Node.Branch[H] => b.left.toList ++ b.right.toList
          case _: Node.Leaf[H]   => Nil
        } :+ root.hash
        val counts = edges.groupBy(identity).view.mapValues(_.size).toMap
        new Trie[H](mode, Some(root.hash), unique.map { case (h, n) => h -> (n, counts.getOrElse(h, 0)) })
    }

  private object Bulk {

    /** Bitwise lexicographic order with a proper prefix first. */
    def lessThan(a: scodec.bits.BitVector, b: scodec.bits.BitVector): Boolean = {
      val c = Bits.commonPrefix(a, b)
      if (c == a.size) a.size < b.size else if (c == b.size) false else !a(c)
    }

    /** One subtree of sorted, distinct paths that all share their first `offset` bits. */
    private def split[H](entries: Vector[(scodec.bits.BitVector, Digest[H])], offset: Long) = {
      val (firstPath, lastPath) = (entries(0)._1.drop(offset), entries(entries.size - 1)._1.drop(offset))
      val c = Bits.commonPrefix(firstPath, lastPath)
      val segment = firstPath.take(c)
      val pos = offset + c
      val (here, rest) = entries.partition(_._1.size == pos)
      val (left, right) = rest.partition(e => !e._1(pos))
      (segment, here.headOption.map(_._2), left, right, pos + 1)
    }

    def build[H](entries: Vector[(scodec.bits.BitVector, Digest[H])], offset: Long)(
      implicit T: TrieHashing[H]
    ): Eval[(Node[H], List[Node[H]])] =
      if (entries.size == 1) Eval.now(leaf(entries(0), offset))
      else {
        val (segment, value, left, right, next) = split(entries, offset)
        (
          Option.when(left.nonEmpty)(left).traverse(l => Eval.defer(build(l, next))),
          Option.when(right.nonEmpty)(right).traverse(r => Eval.defer(build(r, next)))
        ).mapN(branch(segment, value, _, _))
      }

    def buildPar[F[_]: Concurrent: Parallel, H](entries: Vector[(scodec.bits.BitVector, Digest[H])], offset: Long, depth: Int)(
      implicit T: TrieHashing[H]
    ): F[(Node[H], List[Node[H]])] =
      if (depth <= 0 || entries.size == 1) Concurrent[F].unit.map(_ => build(entries, offset).value)
      else {
        val (segment, value, left, right, next) = split(entries, offset)
        (
          Option.when(left.nonEmpty)(left).traverse(l => buildPar[F, H](l, next, depth - 1)),
          Option.when(right.nonEmpty)(right).traverse(r => buildPar[F, H](r, next, depth - 1))
        ).parMapN(branch(segment, value, _, _))
      }

    private def leaf[H](e: (scodec.bits.BitVector, Digest[H]), offset: Long)(implicit T: TrieHashing[H]): (Node[H], List[Node[H]]) = {
      val l = Node.leaf(e._1.drop(offset), e._2)
      (l, List(l))
    }

    private def branch[H](
      segment: scodec.bits.BitVector,
      value:   Option[Digest[H]],
      l:       Option[(Node[H], List[Node[H]])],
      r:       Option[(Node[H], List[Node[H]])]
    )(implicit T: TrieHashing[H]): (Node[H], List[Node[H]]) = {
      val b = Node.branch(segment, value, l.map(_._1.hash), r.map(_._1.hash))
      (b, b :: (l.toList.flatMap(_._2) ++ r.toList.flatMap(_._2)))
    }
  }
}
