package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieError, TrieHashing, TrieProof}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Digest

import scodec.bits.ByteVector

/**
 * A versioned trie (docs/specs/versioned-trie.md): a trie of current state, and a trie of the state root at every
 * version, under one root. That root is the root of a `fixed(1)` top trie whose slot `0x00` holds the version key, `0x01` the state
 * root and `0x02` the history root, each committed as a byte string (`ValueCommitment`, trie.md §5). Slots
 * `0x03..0xff` are reserved and absent, so a proof shows them absent. The top trie is recomputed from the head, never
 * stored.
 */
object VersionedTrie {

  /**
   * What one commitment root commits to (docs/specs/versioned-trie.md §3): the head version, the root of the state trie at it,
   * and the root of the history trie, which maps every committed version up to and including it to its state root.
   */
  final case class Head[V, H](version: V, stateRoot: Digest[H], historyRoot: Digest[H])

  val VersionSlot: ByteVector = ByteVector(0x00)
  val StateSlot: ByteVector = ByteVector(0x01)
  val HistorySlot: ByteVector = ByteVector(0x02)

  /** Modes are valid constants; the `Either`s only carry `KeyMode`'s checked construction. */
  val topMode: Either[VersionedTrieError, KeyMode] = KeyMode.fixed(1).leftMap(VersionedTrieError.Trie(_))
  val stateMode: Either[VersionedTrieError, KeyMode] = KeyMode.variable(StateKey.MaxLength).leftMap(VersionedTrieError.Trie(_))
  def historyMode[V](implicit V: VersionKey[V]): Either[VersionedTrieError, KeyMode] =
    KeyMode.fixed(V.width).leftMap(VersionedTrieError.Trie(_))

  /** The hash a trie commits for a value: `valueHash(commit(v))`, as a JLVM program claims it. */
  def valueHash[H](v: CanonicalValue)(implicit T: TrieHashing[H]): Digest[H] = T.value(ValueCommitment.bytes(v))

  /** The hash a trie commits for a byte string: the top trie's slots and the history trie's state roots. */
  def bytesHash[H](b: ByteVector)(implicit T: TrieHashing[H]): Digest[H] = valueHash(CanonicalValue.bytes(b))

  def trie[A](e: Either[TrieError, A]): Either[VersionedTrieError, A] = e.leftMap(VersionedTrieError.Trie(_))

  def top[V, H](head: Head[V, H])(implicit V: VersionKey[V], T: TrieHashing[H]): Either[VersionedTrieError, Trie[H]] =
    for {
      mode <- topMode
      vkey <- V.encode(head.version)
      t <- trie(
        Trie.fromEntries[H](
          mode,
          List(
            VersionSlot -> bytesHash(vkey),
            StateSlot   -> bytesHash(head.stateRoot.bytes),
            HistorySlot -> bytesHash(head.historyRoot.bytes)
          )
        )
      )
    } yield t

  def root[V: VersionKey, H: TrieHashing](head: Head[V, H]): Either[VersionedTrieError, Digest[H]] = top(head).map(_.root)

  /** The whole top trie as a proof: every slot is present or provably absent. */
  def topProof[V: VersionKey, H: TrieHashing](head: Head[V, H]): Either[VersionedTrieError, TrieProof[H]] =
    top(head).flatMap(t => trie(t.prove(Nil, List(ByteVector.empty))))
}
