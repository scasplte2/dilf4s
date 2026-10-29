package xyz.kd5ujc.accumulators.trie

import cats.Monad
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.hash.Digest

/**
 * Reference-counted node lifecycle (docs/specs/trie.md §6). A node's count is the number of stored parents referencing
 * it plus the number of retained roots that are it. The plan is computed against a read-only view and returned as the
 * final state of every touched entry, so a backend can apply it as one atomic batch.
 */
object RefCounting {

  /** The stored node and its count, or `None` if absent. */
  type View[F[_], H] = Digest[H] => F[Option[(Node[H], Int)]]

  /** Final state per touched digest: `Some((node, count))` to store, `None` to delete. */
  type Plan[H] = Map[Digest[H], Option[(Node[H], Int)]]

  private def children[H](n: Node[H]): List[Digest[H]] = n match {
    case _: Leaf[H]   => Nil
    case b: Branch[H] => b.left.toList ++ b.right.toList
  }

  /**
   * Stores the delta's new nodes, adds one reference for the new root (if `retainNewRoot`), and releases one reference
   * to each of `release`, cascading deletions of anything that drops to zero.
   */
  def plan[F[_]: Monad, H](view: View[F, H], delta: TrieDelta[H], retainNewRoot: Boolean, release: List[Digest[H]]): F[Plan[H]] = {
    def read(overlay: Plan[H], h: Digest[H]): F[Option[(Node[H], Int)]] = overlay.get(h).fold(view(h))(_.pure[F])

    def adjust(overlay: Plan[H], h: Digest[H], by: Int): F[(Plan[H], Option[Node[H]])] =
      read(overlay, h).map {
        case Some((n, c)) =>
          val next = c + by
          if (next <= 0) (overlay.updated(h, None), Some(n)) else (overlay.updated(h, Some((n, next))), None)
        case None => (overlay, None) // releasing an unknown node is a no-op
      }

    for {
      // 1. insert genuinely new nodes with count 0
      fresh <- delta.created.toList.filterA { case (h, _) => view(h).map(_.isEmpty) }
      start = fresh.foldLeft(Map.empty: Plan[H]) { case (acc, (h, n)) => acc.updated(h, Some((n, 0))) }
      // 2. each new node references its children
      linked <- fresh.flatMap { case (_, n) => children(n) }.foldLeftM(start)((acc, c) => adjust(acc, c, 1).map(_._1))
      // 3. the new root is retained
      rooted <- (if (retainNewRoot) delta.root.toList else Nil).foldLeftM(linked)((acc, r) => adjust(acc, r, 1).map(_._1))
      // 4. release, cascading through nodes that reach zero
      done <- (release, rooted).tailRecM[F, Plan[H]] {
        case (Nil, acc) => acc.asRight[(List[Digest[H]], Plan[H])].pure[F]
        case (h :: rest, acc) =>
          adjust(acc, h, -1).map {
            case (next, Some(deleted)) => (children(deleted) ++ rest, next).asLeft
            case (next, None)          => (rest, next).asLeft
          }
      }
    } yield done
  }
}
