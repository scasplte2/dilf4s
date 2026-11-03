package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import scala.collection.immutable.{TreeMap, TreeSet}

import xyz.kd5ujc.accumulators.trie.{Trie, TrieHashing}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue.CNull
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest

/**
 * A replica of a versioned trie, kept in memory and checked locally (docs/specs/versioned-trie.md §7). It starts empty or
 * from a snapshot verified against a head the caller already trusts, and follows deltas, recomputing both tries, so a
 * forged root, a dropped write or a gap is caught by the replica itself rather than taken from the producer.
 *
 * Both tries are updated in place of a rebuild: a delta re-hashes only the paths of the keys it changes, O(changed keys
 * x key length), whatever the state's size. The values are a `TreeMap` in [[Replica.keyOrder]]: a peer's key is only
 * ever compared, never hashed (`StateKey`'s hash code is its string's, which is trivially collidable).
 */
final case class Replica[V, H] private (
  head:    Option[Head[V, H]],
  state:   Trie[H],
  history: Trie[H],
  values:  TreeMap[StateKey, CanonicalValue]
) {
  def root(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Option[Digest[H]]] =
    head.traverse(VersionedTrie.root(_))
}

object Replica {

  private def mismatch(component: String, expected: Digest[?], computed: Digest[?]): VersionedTrieError =
    VersionedTrieError.RootMismatch(component, expected.hex, computed.hex)

  /**
   * The order of a replica's values: string order, which for the key grammar's ASCII alphabet is byte order, the trie's.
   * A comparison reads the keys only up to their first difference, and allocates nothing.
   */
  val keyOrder: Ordering[StateKey] = new Ordering[StateKey] {
    def compare(a: StateKey, b: StateKey): Int = a.value.compareTo(b.value)
  }

  def empty[V: VersionKey, H: TrieHashing]: Either[VersionedTrieError, Replica[V, H]] = emptyIn[V, H](keyOrder)

  /** [[empty]] with its values in `order`: the seam through which a test counts the comparisons a replica makes. */
  private[versioned] def emptyIn[V: VersionKey, H: TrieHashing](order: Ordering[StateKey]): Either[VersionedTrieError, Replica[V, H]] =
    (VersionedTrie.stateMode, VersionedTrie.historyMode[V]).mapN((s, h) =>
      Replica[V, H](None, Trie.empty[H](s), Trie.empty[H](h), TreeMap.empty[StateKey, CanonicalValue](using order))
    )

  /**
   * A replica of `snapshot`, which must reproduce `trusted`: its state root and history root, with the head invariant
   * (the last history entry is the head version with the head's state root). The snapshot's own head is only a claim;
   * its contents pass every check of [[SnapshotReader]].
   */
  def fromSnapshot[V, H](
    trusted:  Head[V, H],
    snapshot: Snapshot[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] = fromSnapshotIn(trusted, snapshot, keyOrder)

  /** [[fromSnapshot]] with its values in `order` (see [[emptyIn]]). */
  private[versioned] def fromSnapshotIn[V, H](
    trusted:  Head[V, H],
    snapshot: Snapshot[V, H],
    order:    Ordering[StateKey]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] =
    for {
      modes <- (VersionedTrie.stateMode, VersionedTrie.historyMode[V]).tupled
      // every structural check of the incremental reader: order, sizes, no nulls, the history ending at the head
      checked <- Snapshot.read(trusted, Snapshot.chunks(snapshot.copy(head = trusted)))
      keys    <- checked.history.traverse { case (u, _) => V.encode(u) }
      state <- VersionedTrie.trie(
        Trie.fromEntries[H](modes._1, checked.entries.map { case (k, v) => k.bytes -> VersionedTrie.valueHash[H](v) })
      )
      _ <- Either.cond(state.root == trusted.stateRoot, (), mismatch("state", trusted.stateRoot, state.root))
      history <- VersionedTrie.trie(
        Trie.fromEntries[H](modes._2, keys.zip(checked.history).map { case (k, (_, s)) => k -> VersionedTrie.bytesHash[H](s.bytes) })
      )
      _ <- Either.cond(history.root == trusted.historyRoot, (), mismatch("history", trusted.historyRoot, history.root))
    } yield Replica(Some(trusted), state, history, TreeMap.from(checked.entries)(using order))

  /**
   * The replica after `delta`, which must chain from its head and whose claimed head must be what the replica
   * computes. A key may not be both written and removed.
   */
  def applyDelta[V, H](
    r:     Replica[V, H],
    delta: Delta[V, H]
  )(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Replica[V, H]] = {
    val written = TreeSet.from(delta.upserts.map(_._1))(using r.values.ordering)
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
          if (v == CNull) VersionedTrieError.Malformed(s"${k.preview.short} is upserted as null").asLeft else VersionedTrie.checkValue(k, v)
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
