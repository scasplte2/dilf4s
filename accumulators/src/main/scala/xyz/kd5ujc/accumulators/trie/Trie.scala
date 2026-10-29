package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._

import xyz.kd5ujc.core.hash.Digest

import scodec.bits.{BitVector, ByteVector}

/**
 * An immutable authenticated dictionary (docs/specs/trie.md): a binary Patricia Merkle trie in `fixed` or `variable` key
 * mode. Nodes are content-addressed and reference counted, so the map holds exactly the nodes the root reaches. Values
 * live with the caller; the trie commits to their hashes.
 */
final class Trie[H] private (
  val mode:              KeyMode,
  private val rootHash:  Option[Digest[H]],
  private val nodes:     Map[Digest[H], (Node[H], Int)]
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

  def put(key: ByteVector, valueHash: Digest[H]): Result[Trie[H]] = update(List(key -> Some(valueHash)))
  def putValue(key: ByteVector, value: ByteVector): Result[Trie[H]] = put(key, T.value(value))
  def remove(key: ByteVector): Result[Trie[H]] = update(List(key -> None))

  /** Applies writes in order (`None` deletes) and garbage-collects what the old root no longer shares. */
  def update(ops: List[(ByteVector, Option[Digest[H]])]): Result[Trie[H]] =
    for {
      trieOps <- ops.traverse { case (k, v) => mode.path(k).map(TrieOp(_, v)) }
      delta   <- TrieEngine.apply[Result, H](node, rootHash, trieOps)
      plan    <- RefCounting.plan[Result, H](h => nodes.get(h).asRight, delta, retainNewRoot = true, release = rootHash.toList)
    } yield new Trie[H](mode, delta.root, plan.foldLeft(nodes) {
      case (acc, (h, Some(entry))) => acc.updated(h, entry)
      case (acc, (h, None))        => acc.removed(h)
    })

  def prove(keys: List[ByteVector], prefixes: List[ByteVector] = Nil): Result[TrieProof[H]] =
    for {
      keyPaths    <- keys.traverse(mode.path)
      prefixPaths  = prefixes.map(_.bits)
      proof       <- TrieProofs.prove[Result, H](node, rootHash, keyPaths.map(TrieProofs.Key) ++ prefixPaths.map(TrieProofs.Prefix))
    } yield proof

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
}
