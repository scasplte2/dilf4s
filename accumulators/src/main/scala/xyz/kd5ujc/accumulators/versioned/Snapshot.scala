package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue.CNull
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest

import scodec.bits.ByteVector

/** A whole versioned trie as a snapshot carries it: the head, every current entry, and every committed version. */
final case class Snapshot[V, H](head: Head[V, H], entries: List[(StateKey, CanonicalValue)], history: List[(V, Digest[H])])

/**
 * One chunk of a snapshot stream (docs/specs/versioned-trie.md §7): the head first, then the current entries in
 * strictly increasing key order, then the committed versions in strictly increasing order, ending with the head
 * version. Each state or history chunk holds 1 to `VersionedTrie.MaxChunkEntries` items.
 */
sealed trait SnapshotChunk[V, H] extends Product with Serializable

object SnapshotChunk {
  final case class Top[V, H](head: Head[V, H]) extends SnapshotChunk[V, H]
  final case class State[V, H](entries: Vector[(StateKey, CanonicalValue)]) extends SnapshotChunk[V, H]
  final case class History[V, H](versions: Vector[(V, Digest[H])]) extends SnapshotChunk[V, H]
}

object Snapshot {

  /** An entry's share of a chunk's encoding: its key and value, and a margin for the record around them. */
  def entryBytes(k: StateKey, v: CanonicalValue): Long = k.bytes.size + Cbor.encode(v).size + 16L

  /** The bytes a chunk's entries may take, leaving a margin for the chunk's own framing. */
  val ChunkBudget: Long = VersionedTrie.MaxChunkBytes - 64L

  /**
   * Groups entries greedily: a group closes before it would pass `limit` entries or [[ChunkBudget]] bytes, so every
   * group encodes within `VersionedTrie.MaxChunkBytes`. One entry always fits (`VersionedTrie.MaxValueBytes`).
   */
  def stateGroups(entries: List[(StateKey, CanonicalValue)], limit: Int): List[Vector[(StateKey, CanonicalValue)]] = {
    type Group = Vector[(StateKey, CanonicalValue)]
    entries.foldLeft((Vector.empty[Group], Vector.empty[(StateKey, CanonicalValue)], 0L)) {
      case ((done, current, bytes), entry @ (k, v)) =>
        val size = entryBytes(k, v)
        if (current.nonEmpty && (current.size >= limit || bytes + size > ChunkBudget)) (done :+ current, Vector(entry), size)
        else (done, current :+ entry, bytes + size)
    } match {
      case (done, current, _) => (if (current.isEmpty) done else done :+ current).toList
    }
  }

  /** The chunks of a snapshot: at most `size` items each, and within the wire byte limit. */
  def chunks[V, H](snapshot: Snapshot[V, H], size: Int = VersionedTrie.MaxChunkEntries): List[SnapshotChunk[V, H]] = {
    val n = size.max(1).min(VersionedTrie.MaxChunkEntries)
    (SnapshotChunk.Top[V, H](snapshot.head): SnapshotChunk[V, H]) ::
    stateGroups(snapshot.entries, n).map(es => SnapshotChunk.State[V, H](es): SnapshotChunk[V, H]) ++
    snapshot.history.grouped(n).map(vs => SnapshotChunk.History[V, H](vs.toVector): SnapshotChunk[V, H]).toList
  }

  /** Reads a whole chunk sequence against a trusted head: every check of [[SnapshotReader]], then completeness. */
  def read[V: VersionKey, H](trusted: Head[V, H], chunks: List[SnapshotChunk[V, H]]): Either[VersionedTrieError, Snapshot[V, H]] =
    chunks.foldM(SnapshotReader.start(trusted))(_.feed(_)).flatMap(_.finish)
}

/**
 * Checks a snapshot chunk by chunk against a head the caller already trusts, so that a malformed, reordered,
 * oversized or foreign snapshot is refused at the first chunk that shows it, before it is all received. What needs
 * every entry, the state and history roots, is checked by `Replica.fromSnapshot` once the reader has finished.
 */
final case class SnapshotReader[V, H] private (
  trusted:     Head[V, H],
  stage:       SnapshotReader.Stage,
  lastKey:     Option[ByteVector],
  lastVersion: Option[ByteVector],
  entries:     Vector[(StateKey, CanonicalValue)],
  history:     Vector[(V, Digest[H])]
)(implicit V: VersionKey[V]) {

  import SnapshotReader.Stage

  private def malformed[A](reason: String): Either[VersionedTrieError, A] = VersionedTrieError.Malformed(s"snapshot: $reason").asLeft[A]

  private def sized(n: Int, what: String): Either[VersionedTrieError, Unit] =
    if (n < 1 || n > VersionedTrie.MaxChunkEntries) malformed(s"a $what chunk holds 1..${VersionedTrie.MaxChunkEntries} items, not $n")
    else ().asRight

  private def increasing(previous: Option[ByteVector], keys: Vector[ByteVector], what: String): Either[VersionedTrieError, Unit] =
    (previous.toVector ++ keys).sliding(2).toVector.traverse_ {
      case Vector(a, b) if Cbor.compareBytes(a, b) >= 0 => malformed[Unit](s"$what ${b.toHex} does not follow ${a.toHex}")
      case _                                            => ().asRight[VersionedTrieError]
    }

  def feed(chunk: SnapshotChunk[V, H]): Either[VersionedTrieError, SnapshotReader[V, H]] =
    (stage, chunk) match {
      case (Stage.Head, SnapshotChunk.Top(head)) =>
        Either.cond(head == trusted, copy(stage = Stage.State), VersionedTrieError.Malformed("snapshot: its head is not the trusted head"))
      case (Stage.Head, _)           => malformed("it starts with its head")
      case (_, SnapshotChunk.Top(_)) => malformed("it has one head")
      case (Stage.State, SnapshotChunk.State(es)) =>
        for {
          _ <- sized(es.size, "state")
          _ <- es.traverse_ { case (k, v) => if (v == CNull) malformed[Unit](s"${k.value} is null") else VersionedTrie.checkValue(k, v) }
          keys = es.map(_._1.bytes)
          _ <- increasing(lastKey, keys, "key")
        } yield copy(lastKey = keys.lastOption.orElse(lastKey), entries = entries ++ es)
      case (Stage.Done, _)                         => malformed("nothing follows the head version")
      case (Stage.History, SnapshotChunk.State(_)) => malformed("state entries precede the history")
      case (_, SnapshotChunk.History(vs)) =>
        for {
          _    <- sized(vs.size, "history")
          keys <- vs.traverse { case (u, _) => V.encode(u) }
          hk   <- V.encode(trusted.version)
          _    <- increasing(lastVersion, keys, "version")
          // the head version may only come last, and with the head's state root
          _ <- keys.zip(vs).traverse_ {
            case (k, (_, s)) =>
              if (Cbor.compareBytes(k, hk) > 0) malformed[Unit](s"version ${k.toHex} is after the head")
              else if (k == hk && s != trusted.stateRoot) malformed[Unit]("the head version has another state root")
              else ().asRight[VersionedTrieError]
          }
        } yield
          copy(
            stage = if (keys.lastOption.contains(hk)) Stage.Done else Stage.History,
            lastVersion = keys.lastOption.orElse(lastVersion),
            history = history ++ vs
          )
    }

  /** The snapshot, once its history has reached the head version. */
  def finish: Either[VersionedTrieError, Snapshot[V, H]] =
    if (stage == Stage.Done) Snapshot(trusted, entries.toList, history.toList).asRight else malformed("it ends before the head version")
}

object SnapshotReader {

  sealed trait Stage extends Product with Serializable

  object Stage {
    case object Head extends Stage
    case object State extends Stage
    case object History extends Stage
    case object Done extends Stage
  }

  def start[V: VersionKey, H](trusted: Head[V, H]): SnapshotReader[V, H] =
    SnapshotReader(trusted, Stage.Head, None, None, Vector.empty, Vector.empty)
}
