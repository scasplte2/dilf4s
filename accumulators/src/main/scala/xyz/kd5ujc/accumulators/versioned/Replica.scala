package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{Trie, TrieHashing}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue.CNull
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest

/**
 * A replica of a versioned trie, kept in memory and checked locally (docs/specs/versioned-trie.md §7). It starts empty or
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

  /**
   * A replica of `snapshot`, which must reproduce `trusted`: its state root and history root, with the head invariant
   * (the last history entry is the head version with the head's state root). The snapshot's own head is only a claim;
   * its contents pass every check of [[SnapshotReader]].
   */
  def fromSnapshot[V, H](
    trusted:  Head[V, H],
    snapshot: Snapshot[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] =
    for {
      modes <- (VersionedTrie.stateMode, VersionedTrie.historyMode[V]).tupled
      // every structural check of the incremental reader: order, sizes, no nulls, the history ending at the head
      checked <- Snapshot.read(trusted, Snapshot.chunks(snapshot.copy(head = trusted)))
      keys    <- checked.history.traverse { case (u, _) => V.encode(u) }
      values = checked.entries.toMap
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
      // the limits a commit and a snapshot enforce, so no replication path accepts a value another refuses
      _ <- delta.upserts.traverse_ {
        case (k, v) =>
          if (v == CNull) VersionedTrieError.Malformed(s"${k.value} is upserted as null").asLeft else VersionedTrie.checkValue(k, v)
      }
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
