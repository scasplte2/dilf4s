package xyz.kd5ujc.accumulators.versioned

import xyz.kd5ujc.accumulators.trie.TrieError
import xyz.kd5ujc.storage.StoreError

import scodec.bits.ByteVector

/**
 * Typed failures of a versioned trie (docs/specs/versioned-trie.md), raised through `cats.mtl.Raise[F, VersionedTrieError]`
 * or returned in `Either` by the pure verifiers. Versions appear as their version keys (`VersionKey.encode`), so the
 * type does not depend on the version type.
 */
sealed trait VersionedTrieError extends Product with Serializable

object VersionedTrieError {

  /** A commit key or namespace that breaks the grammar. */
  final case class Key(error: StateKeyError) extends VersionedTrieError

  /** A version the version key cannot encode, or bytes it cannot decode. */
  final case class InvalidVersion(reason: String) extends VersionedTrieError

  /** Nothing has been committed yet, so there is no head to read or prove against. */
  case object Empty extends VersionedTrieError

  /** A commit's version is not above the head's (and is not an identical re-commit of the head). */
  final case class VersionNotIncreasing(head: ByteVector, next: ByteVector) extends VersionedTrieError

  /** Optimistic concurrency: the caller expected another head (`None`: nothing committed). */
  final case class VersionConflict(expected: Option[ByteVector], actual: Option[ByteVector]) extends VersionedTrieError

  /** The head version committed again with a batch that gives a different state. */
  final case class CommitRewrite(version: ByteVector) extends VersionedTrieError

  /** The version was committed, but what was asked of it is below the prune floor. */
  final case class NotRetained(version: ByteVector) extends VersionedTrieError

  /** The version was never committed. */
  final case class UnknownVersion(version: ByteVector) extends VersionedTrieError

  /** A recomputed root differs from the claimed or trusted one; `component` names which. */
  final case class RootMismatch(component: String, expected: String, computed: String) extends VersionedTrieError

  /** A proof that does not reach the queried key, prefix, range or slot: neither presence nor absence is proven. */
  final case class NotCovered(what: String) extends VersionedTrieError

  /** A proof, delta or snapshot whose shape or contents are inconsistent with what it claims. */
  final case class Malformed(reason: String) extends VersionedTrieError

  /**
   * A value or snapshot chunk over its limit (docs/specs/versioned-trie.md §7), in canonical CBOR bytes. `size` is the
   * exact length, or, when the weight alone shows the limit is passed, the weight: a lower bound on the length (saturated
   * at `Long.MaxValue` for a value that shares structure), reported without encoding anything.
   */
  final case class TooLarge(what: String, size: Long, max: Long) extends VersionedTrieError

  /** A value nested deeper than every wire format can carry (docs/specs/versioned-trie.md §4). */
  final case class TooDeep(what: String, depth: Int, max: Int) extends VersionedTrieError

  final case class Store(error: StoreError) extends VersionedTrieError
  final case class Trie(error: TrieError) extends VersionedTrieError
}
