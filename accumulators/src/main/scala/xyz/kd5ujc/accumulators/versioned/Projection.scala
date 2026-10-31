package xyz.kd5ujc.accumulators.versioned

import scala.collection.immutable.SortedMap

import xyz.kd5ujc.accumulators.trie.{Trie, TrieHashing}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Digest
import xyz.kd5ujc.storage.Batch

/**
 * An application state projected onto committed entries (metakit's view, as an adapter; docs/specs/versioned-trie.md §8).
 * The committed core takes batches; a view turns two states into the batch between them. `delta` may be overridden
 * with a cheaper one, and [[Projection.verifyAgainst]] is the opt-in check that it agrees with `entries`.
 */
trait StateView[S] {
  def entries(s:  S): SortedMap[StateKey, CanonicalValue]
  def delta(prev: S, next: S): Batch[StateKey, CanonicalValue] = Projection.diff(entries(prev), entries(next))
}

object Projection {

  /** Removes for keys that left, puts for keys that are new or changed; unchanged keys are not written. */
  def diff(prev: Map[StateKey, CanonicalValue], next: Map[StateKey, CanonicalValue]): Batch[StateKey, CanonicalValue] =
    Batch.deletes[StateKey, CanonicalValue](prev.keys.filterNot(next.contains).toList.sorted) ++
    Batch.puts(next.toList.filterNot { case (k, v) => prev.get(k).contains(v) }.sortBy(_._1))

  /** The state root of a set of entries, built from scratch. */
  def stateRoot[H: TrieHashing](entries: Map[StateKey, CanonicalValue]): Either[VersionedTrieError, Digest[H]] =
    VersionedTrie.stateMode
      .flatMap(m =>
        VersionedTrie.trie(
          Trie.fromEntries[H](
            m,
            entries.toList.collect { case (k, v) if v != CanonicalValue.CNull => k.bytes -> VersionedTrie.valueHash[H](v) }
          )
        )
      )
      .map(_.root)

  /** Whether committed state with root `stateRoot` is exactly `entries`: a `RootMismatch` on "state" if not. */
  def verifyAgainst[H: TrieHashing](entries: Map[StateKey, CanonicalValue], stateRoot: Digest[H]): Either[VersionedTrieError, Unit] =
    Projection
      .stateRoot[H](entries)
      .flatMap(computed => Either.cond(computed == stateRoot, (), VersionedTrieError.RootMismatch("state", stateRoot.hex, computed.hex)))
}
