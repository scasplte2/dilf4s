package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.ValueCommitment
import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieError, TrieHashing, TrieProof}
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
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

  /** The largest value a key may hold, as canonical CBOR: every entry then fits in one snapshot chunk (§7). */
  val MaxValueBytes: Long = 1L << 20

  /** At most this many entries or versions in one snapshot chunk. */
  val MaxChunkEntries: Int = 1024

  /** The largest snapshot chunk, as canonical CBOR: refused before it is decoded. */
  val MaxChunkBytes: Long = 1L << 22

  /**
   * The deepest any wire record wraps a value: a delta's `{"upserts": [{"k", "v"}]}` and a snapshot chunk's
   * `{"state": [{"k", "v"}]}` each add a map, an array and a map. Proofs and stored records carry no nested values.
   */
  val ValueWrapperDepth: Int = 3

  /** The deepest value a key may hold, so that every record carrying it stays within `CanonicalValue.MaxDepth`. */
  val MaxValueDepth: Int = CanonicalValue.MaxDepth - ValueWrapperDepth

  /** A value every wire format can carry: at most `MaxValueDepth` deep and `MaxValueBytes` long. */
  def checkValue(key: StateKey, v: CanonicalValue): Either[VersionedTrieError, Unit] =
    if (v.depth > MaxValueDepth) VersionedTrieError.TooDeep(s"the value of ${key.value}", v.depth, MaxValueDepth).asLeft
    else {
      val size = Cbor.encode(v).size
      Either.cond(size <= MaxValueBytes, (), VersionedTrieError.TooLarge(s"the value of ${key.value}", size, MaxValueBytes))
    }

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
