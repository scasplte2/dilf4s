package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.trie.{Trie, TrieHashing}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest

import scodec.bits.ByteVector

/** A whole committed state as a snapshot carries it: the head, every current entry, and every committed version. */
final case class Snapshot[V, H](head: Head[V, H], entries: List[(StateKey, CanonicalValue)], history: List[(V, Digest[H])])

object Snapshot {

  /** Reassembles a snapshot stream: one head first, then entries, then versions, as `VersionedTrieStore.snapshot` emits. */
  def fromChunks[V, H](chunks: List[SnapshotChunk[V, H]]): Either[VersionedTrieError, Snapshot[V, H]] =
    chunks match {
      case SnapshotChunk.Top(head) :: rest =>
        val entries = rest.collect { case SnapshotChunk.Entry(k, v) => k -> v }
        val history = rest.collect { case SnapshotChunk.History(u, s) => u -> s }
        val ordered = rest.map {
          case _: SnapshotChunk.Entry[V, H]   => 0
          case _: SnapshotChunk.History[V, H] => 1
          case _: SnapshotChunk.Top[V, H]     => 2
        }
        Either.cond(
          ordered == ordered.sorted && !ordered.contains(2),
          Snapshot(head, entries, history),
          VersionedTrieError.Malformed("snapshot chunks out of order")
        )
      case _ => VersionedTrieError.Malformed("a snapshot starts with its head").asLeft
    }
}

/**
 * A replica of a committed state, kept in memory and checked locally (docs/specs/versioned-trie.md §7). It starts empty or
 * from a snapshot verified against a head the caller already trusts, and follows deltas, recomputing both tries, so a
 * forged root, a dropped write or a gap is caught by the replica itself rather than taken from the producer.
 */
final case class Replica[V, H] private (
  head:    Option[Head[V, H]],
  state:   Trie[H],
  history: Trie[H],
  values:  Map[StateKey, CanonicalValue]
) {
  def root(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Option[Digest[H]]] =
    head.traverse(VersionedTrie.root(_))
}

object Replica {

  private def mismatch(component: String, expected: Digest[?], computed: Digest[?]): VersionedTrieError =
    VersionedTrieError.RootMismatch(component, expected.hex, computed.hex)

  def empty[V: VersionKey, H: TrieHashing]: Either[VersionedTrieError, Replica[V, H]] =
    (VersionedTrie.stateMode, VersionedTrie.historyMode[V]).mapN((s, h) =>
      Replica[V, H](None, Trie.empty[H](s), Trie.empty[H](h), Map.empty)
    )

  @tailrec
  private def increasing(keys: List[ByteVector]): Boolean =
    keys match {
      case a :: (rest @ (b :: _)) => Cbor.compareBytes(a, b) < 0 && increasing(rest)
      case _                      => true
    }

  /**
   * A replica of `snapshot`, which must reproduce `trusted`: its state root and history root, with the head invariant
   * (the last history entry is the head version with the head's state root). The snapshot's own head is only a claim.
   */
  def fromSnapshot[V, H](
    trusted:  Head[V, H],
    snapshot: Snapshot[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] =
    for {
      modes <- (VersionedTrie.stateMode, VersionedTrie.historyMode[V]).tupled
      keys  <- snapshot.history.traverse { case (u, _) => V.encode(u) }
      hk    <- V.encode(trusted.version)
      _     <- Either.cond(increasing(keys), (), VersionedTrieError.Malformed("snapshot versions are not strictly increasing"))
      _ <- Either.cond(
        keys.lastOption.contains(hk) && snapshot.history.lastOption.exists(_._2 == trusted.stateRoot),
        (),
        VersionedTrieError.Malformed("the snapshot's history does not end at the trusted head")
      )
      values = snapshot.entries.toMap
      _ <- Either.cond(values.size == snapshot.entries.size, (), VersionedTrieError.Malformed("a snapshot entry is repeated"))
      state <- VersionedTrie.trie(
        Trie.fromEntries[H](modes._1, snapshot.entries.map { case (k, v) => k.bytes -> VersionedTrie.valueHash[H](v) })
      )
      history <- VersionedTrie.trie(
        Trie.fromEntries[H](modes._2, keys.zip(snapshot.history).map { case (k, (_, s)) => k -> VersionedTrie.bytesHash[H](s.bytes) })
      )
      _ <- Either.cond(state.root == trusted.stateRoot, (), mismatch("state", trusted.stateRoot, state.root))
      _ <- Either.cond(history.root == trusted.historyRoot, (), mismatch("history", trusted.historyRoot, history.root))
    } yield Replica(Some(trusted), state, history, values)

  /**
   * The replica after `delta`, which must chain from its head and whose claimed head must be what the replica
   * computes. A key may not be both written and removed.
   */
  def applyDelta[V, H](
    r:     Replica[V, H],
    delta: Delta[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] = {
    val written = delta.upserts.map(_._1).toSet
    for {
      parent  <- r.head.traverse(VersionedTrie.root(_))
      claimed <- delta.parent.traverse(VersionedTrie.root(_))
      _ <- Either.cond(
        parent == claimed,
        (),
        VersionedTrieError.RootMismatch("parent", parent.fold("none")(_.hex), claimed.fold("none")(_.hex))
      )
      nk <- V.encode(delta.head.version)
      hk <- r.head.traverse(h => V.encode(h.version))
      _  <- hk.filterNot(Cbor.compareBytes(nk, _) > 0).toLeft(()).leftMap(VersionedTrieError.VersionNotIncreasing(_, nk))
      _  <- Either.cond(delta.removes.forall(k => !written(k)), (), VersionedTrieError.Malformed("a key is both written and removed"))
      _  <- Either.cond(written.size == delta.upserts.size, (), VersionedTrieError.Malformed("a key is written twice"))
      ops = delta.removes.map(k => k.bytes -> Option.empty[Digest[H]]) ++ delta.upserts.map {
        case (k, v) => k.bytes -> Option(VersionedTrie.valueHash[H](v))
      }
      state   <- VersionedTrie.trie(r.state.update(ops))
      _       <- Either.cond(state.root == delta.head.stateRoot, (), mismatch("state", delta.head.stateRoot, state.root))
      history <- VersionedTrie.trie(r.history.put(nk, VersionedTrie.bytesHash[H](state.root.bytes)))
      _       <- Either.cond(history.root == delta.head.historyRoot, (), mismatch("history", delta.head.historyRoot, history.root))
    } yield Replica(Some(delta.head), state, history, delta.upserts.foldLeft(r.values -- delta.removes)(_ + _))
  }
}
