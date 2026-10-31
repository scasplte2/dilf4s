package xyz.kd5ujc.accumulators.versioned

import cats.Functor
import cats.effect.Concurrent
import cats.mtl.Raise
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{TrieError, TrieHashing, TrieProof, TrieStore}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue.CNull
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Digest
import xyz.kd5ujc.storage._

import fs2.Stream
import scodec.bits.ByteVector

/**
 * A commit computed but not written (docs/specs/versioned-trie.md §4): the head and root it would produce, and the staged
 * writes of every store. A draft can be extended by further drafts; committing one writes its whole chain, or nothing
 * if the store moved since it was staged.
 */
final case class Draft[V, H] private[versioned] (
  head:                           Head[V, H],
  root:                           Digest[H],
  private[versioned] val base:    Option[ByteVector],
  private[versioned] val state:   TrieStore.Staged[H],
  private[versioned] val history: TrieStore.Staged[H],
  private[versioned] val values:  VersionedStore.Staged,
  private[versioned] val index:   RawBatch
)

/**
 * A versioned trie (docs/specs/versioned-trie.md): an application commits a batch of writes at a strictly
 * increasing version `V`, and every commit yields one commitment root over the version, the state and the history of
 * all committed versions. Every mutation is one atomic compare-and-write over all of the layer's stores.
 */
trait VersionedTrieStore[F[_], V, H] {

  /** The latest commit, or `None` before the first. */
  def head: F[Option[Head[V, H]]]

  /** The head as it was at a committed version; `None` if `v` was never committed, `NotRetained` if pruned. */
  def headAt(v: V): F[Option[Head[V, H]]]

  /** The commit `commit(expected, next, batch)` would make, with no write. */
  def preview(next: V, batch: Batch[StateKey, CanonicalValue]): F[Draft[V, H]]

  /** A further commit on top of an unwritten draft. */
  def previewOn(draft: Draft[V, H], next: V, batch: Batch[StateKey, CanonicalValue]): F[Draft[V, H]]

  /**
   * Commits `batch` at `next`, which must exceed the head's version; `expected` must be the head's version (`None`
   * before the first commit). Committing the head version again, against its parent, is idempotent if the batch gives
   * the same state, and a `CommitRewrite` otherwise. A null value deletes its key.
   */
  def commit(expected: Option[V], next: V, batch: Batch[StateKey, CanonicalValue]): F[Head[V, H]]

  /** Writes a draft's whole chain in one compare-and-write; a `VersionConflict` if the store moved since. */
  def commitDraft(draft: Draft[V, H]): F[Head[V, H]]

  /** The current value, or the value at a committed, retained version. */
  def get(key: StateKey, at: Option[V] = None): F[Option[CanonicalValue]]

  /** The current entries, of one namespace or all, in key order. */
  def scan(ns: Option[StateNamespace] = None): Stream[F, (StateKey, CanonicalValue)]

  /** The head, with the history trie's tail from the head version up (the head invariant, §3). */
  def proveHead: F[VersionedTrieProof[V, H]]

  /** Keys, namespaces and ranges of the current state, or of a retained past version's state. */
  def proveState(q: StateQuery, at: Option[V] = None): F[VersionedTrieProof[V, H]]

  /** Whether `u` was committed, and its state root if so. */
  def proveVersion(u: V): F[VersionedTrieProof[V, H]]

  /** The latest version at or before `t`, or that there is none. */
  def proveAsOf(t: V): F[VersionedTrieProof[V, H]]

  /** Every committed version in `[from, until)` with its state root, complete: an epoch is such a range. */
  def proveVersions(from: V, until: Option[V]): F[VersionedTrieProof[V, H]]

  /** The change a retained version made, between its parent's head and its own. */
  def delta(v: V): F[Delta[V, H]]

  /**
   * The head, the current entries and the committed versions, in chunks of at most `chunkEntries` items and
   * `VersionedTrie.MaxChunkBytes` bytes, for bootstrapping a replica or a store (§7).
   */
  def snapshot(chunkEntries: Int = VersionedTrie.MaxChunkEntries): Stream[F, SnapshotChunk[V, H]]

  /**
   * Loads a snapshot, checked against a head the caller already trusts, into an empty store, in one write. The store
   * is then exactly as if it had committed every version of the snapshot's history and pruned through the head
   * version (§4): every version stays attestable, states before the head are not retained, and commits continue from
   * the head. A store that is not empty is a `VersionConflict` against no head.
   */
  def restore(trusted: Head[V, H], snapshot: Snapshot[V, H]): F[Head[V, H]]

  /** [[restore]] from a chunk stream, refused at the first chunk that the incremental reader rejects. */
  def restoreFrom(trusted: Head[V, H], chunks: Stream[F, SnapshotChunk[V, H]]): F[Head[V, H]]

  /** Discards every version after `to`. */
  def rollback(to: V): F[Unit]

  /**
   * Discards states and values below `through`: past states before it can no longer be read or proven, and its delta
   * is gone. The history trie stays whole at the head, so every version stays attestable.
   */
  def prune(through: V): F[Unit]
}

object VersionedTrieStore {

  /**
   * The store over `raw`, under `<name>/state`, `<name>/hist`, `<name>/vals` and `<name>/vidx`. Opening checks that the
   * stores are at the same internal sequence, which every write keeps (they only ever move together).
   */
  def make[F[_]: Concurrent, V, H](raw: RawStore[F], name: String)(
    implicit V: VersionKey[V],
    T:          TrieHashing[H],
    R:          Raise[F, VersionedTrieError]
  ): F[VersionedTrieStore[F, V, H]] = {
    implicit val rs: Raise[F, StoreError] = via[F, StoreError](VersionedTrieError.Store(_))
    implicit val rt: Raise[F, TrieError] = via[F, TrieError](VersionedTrieError.Trie(_))
    def lift[A](e: Either[VersionedTrieError, A]): F[A] = e.fold(R.raise[VersionedTrieError, A], _.pure[F])
    for {
      modes   <- lift((VersionedTrie.stateMode, VersionedTrie.historyMode[V]).tupled)
      indexes <- lift((Namespace(s"$name/vidx/v"), Namespace(s"$name/vidx/s")).tupled.leftMap(VersionedTrieError.Store(_)))
      state   <- TrieStore.make[F, H](raw, s"$name/state", modes._1)
      history <- TrieStore.make[F, H](raw, s"$name/hist", modes._2)
      values  <- VersionedStore.make[F, StateKey, CanonicalValue](raw, s"$name/vals")
      store = new Impl[F, V, H](raw, state, history, values, indexes._1.prefix, indexes._2.prefix)
      _ <- store.checkAligned
    } yield store
  }

  /** Raises an inner store's error as the versioned trie's, so callers handle one error type. */
  private def via[F[_], E](wrap: E => VersionedTrieError)(implicit R: Raise[F, VersionedTrieError]): Raise[F, E] =
    new Raise[F, E] {
      def functor: Functor[F] = R.functor
      def raise[E2 <: E, A](e: E2): F[A] = R.raise[VersionedTrieError, A](wrap(e))
    }

  /** The head at an internal sequence number: every store is at this sequence. */
  final private case class Current[V, H](seq: Long, head: Head[V, H])

  final private class Impl[F[_]: Concurrent, V, H](
    raw:       RawStore[F],
    state:     TrieStore[F, H],
    history:   TrieStore[F, H],
    values:    VersionedStore[F, StateKey, CanonicalValue],
    byVersion: ByteVector,
    bySeq:     ByteVector
  )(implicit V: VersionKey[V], T: TrieHashing[H], R: Raise[F, VersionedTrieError])
      extends VersionedTrieStore[F, V, H] {

    private def fail[A](e:         VersionedTrieError): F[A] = R.raise[VersionedTrieError, A](e)
    private def lift[A](e:         Either[VersionedTrieError, A]): F[A] = e.fold(fail[A], _.pure[F])
    private def corrupt[A](reason: String): F[A] = fail(VersionedTrieError.Store(StoreError.Corrupt(reason)))
    private def vkey(v:            V): F[ByteVector] = lift(V.encode(v))
    private def version(n:         Long): Version = Version.unsafe(n)
    private def after(a:           ByteVector, b: ByteVector): Boolean = Cbor.compareBytes(a, b) > 0

    // ---- the version index: vkey(V) -> seq, and seq -> vkey(V) ‖ stateRoot (kept whole: pruning never touches it)

    private def seqKey(n: Long): ByteVector = bySeq ++ ByteVector.fromLong(n)

    private def indexWrites(n: Long, vk: ByteVector, stateRoot: Digest[H]): RawBatch =
      RawBatch.put(byVersion ++ vk, ByteVector.fromLong(n)) ++ RawBatch.put(seqKey(n), vk ++ stateRoot.bytes)

    private def seqOf(vk: ByteVector): F[Option[Long]] =
      raw
        .get(byVersion ++ vk)
        .flatMap(_.traverse(b => if (b.size == 8L) b.toLong(signed = false).pure[F] else corrupt[Long]("version index record")))

    private def decodeEntry(b: ByteVector): F[(ByteVector, Digest[H])] =
      if (b.size <= V.width.toLong) corrupt("sequence index record")
      else
        Digest.fromBytes[H](b.drop(V.width.toLong))(using T.hasher).fold(e => corrupt(e.toString), d => (b.take(V.width.toLong), d).pure[F])

    private def entryAt(n: Long): F[Option[(ByteVector, Digest[H])]] = raw.get(seqKey(n)).flatMap(_.traverse(decodeEntry))

    private def requireEntry(n: Long): F[(ByteVector, Digest[H])] =
      entryAt(n).flatMap(_.fold(corrupt[(ByteVector, Digest[H])](s"no index entry for sequence $n"))(_.pure[F]))

    private def requireSeq(vk: ByteVector): F[Long] =
      seqOf(vk).flatMap(_.fold(fail[Long](VersionedTrieError.UnknownVersion(vk)))(_.pure[F]))

    /** The sequence of a committed version whose state is retained. */
    private def retainedSeq(vk: ByteVector): F[Long] =
      (requireSeq(vk), state.floor).tupled.flatMap {
        case (n, f) => if (n < f.value) fail(VersionedTrieError.NotRetained(vk)) else n.pure[F]
      }

    private def headOfSeq(n: Long): F[Option[Head[V, H]]] =
      if (n == 0L) Option.empty[Head[V, H]].pure[F]
      else
        (requireEntry(n), history.root(version(n))).tupled.flatMap {
          case ((vk, stateRoot), historyRoot) => lift(V.decode(vk)).map(v => Option(Head(v, stateRoot, historyRoot)))
        }

    private def current: F[Current[V, H]] =
      state.latest.flatMap(n =>
        headOfSeq(n.value).flatMap(_.fold(fail[Current[V, H]](VersionedTrieError.Empty))(h => Current(n.value, h).pure[F]))
      )

    private def headKey: F[Option[ByteVector]] = head.flatMap(_.traverse(h => vkey(h.version)))

    def checkAligned: F[Unit] =
      (state.latest, history.latest, values.latest).tupled.flatMap {
        case (s, h, v) =>
          if (s != h || s != v) corrupt(s"stores out of step: state ${s.value}, history ${h.value}, values ${v.value}")
          else
            (entryAt(s.value), entryAt(s.value + 1)).tupled.flatMap {
              case (here, next) =>
                if ((s.value > 0L && here.isEmpty) || next.isDefined) corrupt(s"version index out of step at sequence ${s.value}")
                else ().pure[F]
            }
      }

    // ---- reads

    def head: F[Option[Head[V, H]]] = state.latest.flatMap(n => headOfSeq(n.value))

    def headAt(v: V): F[Option[Head[V, H]]] =
      vkey(v).flatMap(vk =>
        seqOf(vk).flatMap(_.flatTraverse { n =>
          history.floor.flatMap(f => if (n < f.value) fail[Option[Head[V, H]]](VersionedTrieError.NotRetained(vk)) else headOfSeq(n))
        })
      )

    def get(key: StateKey, at: Option[V]): F[Option[CanonicalValue]] =
      at.fold(values.get(key))(u => vkey(u).flatMap(retainedSeq).flatMap(n => values.getAt(key, version(n))))

    def scan(ns: Option[StateNamespace]): Stream[F, (StateKey, CanonicalValue)] =
      values.scan(ns.fold[KeyRange[StateKey]](KeyRange.All)(n => KeyRange.EncodedPrefix(n.prefix)))

    // ---- commits

    /** A batch's final write per key; a null value is a delete, as a null map value is absent (canonical-encoding.md §1). */
    private def finals(batch: Batch[StateKey, CanonicalValue]): Vector[(StateKey, Option[CanonicalValue])] =
      batch.last.map { case (k, v) => k -> v.filter(_ != CNull) }

    private def valueBatch(fs: Vector[(StateKey, Option[CanonicalValue])]): Batch[StateKey, CanonicalValue] =
      Batch(fs.map { case (k, v) => v.fold[Op[StateKey, CanonicalValue]](Op.Delete(k))(Op.Put(k, _)) })

    private def trieOps(fs: Vector[(StateKey, Option[CanonicalValue])]): List[(ByteVector, Option[Digest[H]])] =
      fs.toList.map { case (k, v) => k.bytes -> v.map(VersionedTrie.valueHash[H]) }

    private def draft(
      base:    Option[ByteVector],
      next:    V,
      nk:      ByteVector,
      seq:     Long,
      s:       TrieStore.Staged[H],
      h:       F[TrieStore.Staged[H]],
      v:       F[VersionedStore.Staged],
      indexed: RawBatch
    ): F[Draft[V, H]] =
      (h, v).tupled.flatMap {
        case (hs, vs) =>
          val head = Head(next, s.root, hs.root)
          lift(VersionedTrie.root(head)).map(root => Draft(head, root, base, s, hs, vs, indexed ++ indexWrites(seq, nk, s.root)))
      }

    private def checkValues(fs: Vector[(StateKey, Option[CanonicalValue])]): F[Unit] =
      lift(fs.traverse_ { case (k, v) => v.traverse_(VersionedTrie.checkValue(k, _)) })

    private def stageOnDisk(
      n:     Long,
      base:  Option[ByteVector],
      next:  V,
      nk:    ByteVector,
      batch: Batch[StateKey, CanonicalValue]
    ): F[Draft[V, H]] = {
      val fs = finals(batch)
      checkValues(fs) >> state.stage(version(n), trieOps(fs)).flatMap { s =>
        draft(
          base,
          next,
          nk,
          n + 1,
          s,
          history.stage(version(n), List(nk -> Some(VersionedTrie.bytesHash[H](s.root.bytes)))),
          values.stage(version(n), valueBatch(fs)),
          RawBatch.empty
        )
      }
    }

    def preview(next: V, batch: Batch[StateKey, CanonicalValue]): F[Draft[V, H]] =
      for {
        nk  <- vkey(next)
        n   <- state.latest
        cur <- headKey
        _   <- cur.filterNot(after(nk, _)).traverse_(k => fail[Unit](VersionedTrieError.VersionNotIncreasing(k, nk)))
        d   <- stageOnDisk(n.value, cur, next, nk, batch)
      } yield d

    def previewOn(d: Draft[V, H], next: V, batch: Batch[StateKey, CanonicalValue]): F[Draft[V, H]] =
      for {
        nk <- vkey(next)
        dk <- vkey(d.head.version)
        _  <- if (after(nk, dk)) ().pure[F] else fail[Unit](VersionedTrieError.VersionNotIncreasing(dk, nk))
        fs = finals(batch)
        _ <- checkValues(fs)
        s <- state.stageOn(d.state, trieOps(fs))
        nd <- draft(
          d.base,
          next,
          nk,
          s.version.value,
          s,
          history.stageOn(d.history, List(nk -> Some(VersionedTrie.bytesHash[H](s.root.bytes)))),
          values.stageOn(d.values, valueBatch(fs)),
          d.index
        )
      } yield nd

    def commitDraft(d: Draft[V, H]): F[Head[V, H]] =
      raw
        .writeIf(d.state.guards ++ d.history.guards ++ d.values.guards, d.state.batch ++ d.history.batch ++ d.values.batch ++ d.index)
        .flatMap(wrote => if (wrote) d.head.pure[F] else headKey.flatMap(now => fail(VersionedTrieError.VersionConflict(d.base, now))))

    def commit(expected: Option[V], next: V, batch: Batch[StateKey, CanonicalValue]): F[Head[V, H]] =
      for {
        nk  <- vkey(next)
        ek  <- expected.traverse(vkey)
        n   <- state.latest
        cur <- head
        ck  <- cur.traverse(h => vkey(h.version))
        result <- (cur, ck) match {
          case (Some(h), Some(k)) if k == nk && !ek.contains(k) => recommit(n.value, h, nk, ek, batch)
          case _ if ek != ck                                    => fail[Head[V, H]](VersionedTrieError.VersionConflict(ek, ck))
          case (_, Some(k)) if !after(nk, k)                    => fail[Head[V, H]](VersionedTrieError.VersionNotIncreasing(k, nk))
          case _                                                => stageOnDisk(n.value, ck, next, nk, batch).flatMap(commitDraft)
        }
      } yield result

    /** The head committed again against its parent: the same state is the same commit; any other is a rewrite. */
    private def recommit(
      n:     Long,
      h:     Head[V, H],
      nk:    ByteVector,
      ek:    Option[ByteVector],
      batch: Batch[StateKey, CanonicalValue]
    ): F[Head[V, H]] =
      for {
        parent <- if (n <= 1L) Option.empty[ByteVector].pure[F] else requireEntry(n - 1).map(e => Option(e._1))
        _      <- if (ek == parent) ().pure[F] else fail[Unit](VersionedTrieError.VersionConflict(ek, Some(nk)))
        floor  <- state.floor
        _      <- if (n - 1 < floor.value) fail[Unit](VersionedTrieError.NotRetained(nk)) else ().pure[F]
        root   <- state.rootAfter(version(n - 1), trieOps(finals(batch)))
        _      <- if (root == h.stateRoot) ().pure[F] else fail[Unit](VersionedTrieError.CommitRewrite(nk))
      } yield h

    // ---- proofs

    private def topProof(h: Head[V, H]): F[TrieProof[H]] = lift(VersionedTrie.topProof(h))

    def proveHead: F[VersionedTrieProof[V, H]] =
      for {
        cur  <- current
        hk   <- vkey(cur.head.version)
        top  <- topProof(cur.head)
        hist <- history.prove(Nil, Nil, version(cur.seq), List(hk -> None))
      } yield VersionedTrieProof(cur.head, top, None, Some(hist), None)

    def proveState(q: StateQuery, at: Option[V]): F[VersionedTrieProof[V, H]] =
      for {
        cur    <- current
        top    <- topProof(cur.head)
        target <- at.traverse(u => vkey(u).flatMap(uk => retainedSeq(uk).flatMap(n => requireEntry(n).map(e => (u, uk, n, e._2)))))
        sp <- state.prove(
          q.keys.map(_.bytes),
          q.namespaces.map(_.prefix),
          version(target.fold(cur.seq)(_._3)),
          q.ranges.map { case (f, u) => f.bytes -> u.map(_.bytes) }
        )
        hp <- target.traverse(t => history.prove(List(t._2), Nil, version(cur.seq)))
      } yield VersionedTrieProof(cur.head, top, target.map(t => At[V, H](t._1, Some(t._4))), hp, Some(sp))

    def proveVersion(u: V): F[VersionedTrieProof[V, H]] =
      for {
        cur <- current
        top <- topProof(cur.head)
        uk  <- vkey(u)
        s   <- seqOf(uk).flatMap(_.traverse(requireEntry)).map(_.map(_._2))
        hp  <- history.prove(List(uk), Nil, version(cur.seq))
      } yield VersionedTrieProof(cur.head, top, Some(At(u, s)), Some(hp), None)

    /** The latest sequence whose version key is at or before `tk`: a binary search over the dense sequence index. */
    private def atOrBefore(tk: ByteVector, n: Long): F[Option[(ByteVector, Digest[H])]] =
      (1L, n, Option.empty[(ByteVector, Digest[H])]).tailRecM {
        case (lo, hi, best) =>
          if (lo > hi) best.asRight[(Long, Long, Option[(ByteVector, Digest[H])])].pure[F]
          else {
            val mid = lo + (hi - lo) / 2
            requireEntry(mid).map(e => if (after(e._1, tk)) (lo, mid - 1, best).asLeft else (mid + 1, hi, Option(e)).asLeft)
          }
      }

    def proveAsOf(t: V): F[VersionedTrieProof[V, H]] =
      for {
        cur   <- current
        top   <- topProof(cur.head)
        tk    <- vkey(t)
        found <- atOrBefore(tk, cur.seq)
        at    <- found.traverse { case (pk, s) => lift(V.decode(pk)).map(p => At[V, H](p, Some(s))) }
        hp    <- history.prove(Nil, Nil, version(cur.seq), List(found.fold(ByteVector.empty)(_._1) -> Some(tk :+ 0x00.toByte)))
      } yield VersionedTrieProof(cur.head, top, at, Some(hp), None)

    def proveVersions(from: V, until: Option[V]): F[VersionedTrieProof[V, H]] =
      for {
        cur <- current
        top <- topProof(cur.head)
        fk  <- vkey(from)
        uk  <- until.traverse(vkey)
        hp  <- history.prove(Nil, Nil, version(cur.seq), List(fk -> uk))
      } yield VersionedTrieProof(cur.head, top, None, Some(hp), None)

    // ---- replication

    def delta(v: V): F[Delta[V, H]] =
      for {
        vk      <- vkey(v)
        n       <- requireSeq(vk)
        floor   <- values.floor
        _       <- if (floor.value > 0L && n <= floor.value) fail[Unit](VersionedTrieError.NotRetained(vk)) else ().pure[F]
        parent  <- headOfSeq(n - 1)
        h       <- headOfSeq(n).flatMap(_.fold(corrupt[Head[V, H]](s"no head at sequence $n"))(_.pure[F]))
        changes <- values.changes(version(n))
      } yield
        Delta(
          parent,
          h,
          changes.flatMap(c => c.after.map(c.key -> _).toList).sortBy(_._1),
          changes.filter(_.after.isEmpty).map(_.key).sorted
        )

    def snapshot(chunkEntries: Int): Stream[F, SnapshotChunk[V, H]] = {
      val limit = chunkEntries.max(1).min(VersionedTrie.MaxChunkEntries)
      Stream.eval(current).flatMap { cur =>
        Stream.emit[F, SnapshotChunk[V, H]](SnapshotChunk.Top(cur.head)) ++
        values
          .scan()
          // close a chunk before it would pass the entry or byte limit; one entry always fits (MaxValueBytes)
          .mapAccumulate((0L, 0, 0L)) {
            case ((group, n, bytes), (k, v)) =>
              // the same rule as Snapshot.stateGroups, over a stream
              val size = Snapshot.entryBytes(k, v)
              val next = if (n >= limit || bytes + size > Snapshot.ChunkBudget) (group + 1, 1, size) else (group, n + 1, bytes + size)
              (next, (k, v))
          }
          .groupAdjacentBy(_._1._1)
          .map { case (_, c) => SnapshotChunk.State[V, H](c.toVector.map(_._2)): SnapshotChunk[V, H] } ++
        raw
          .scan(ByteRange(Some(seqKey(1L)), Some(seqKey(cur.seq + 1))))
          .evalMap {
            case (_, b) => decodeEntry(b).flatMap { case (vk, s) => lift(V.decode(vk)).map(u => u -> s) }
          }
          .chunkN(limit)
          .map(c => SnapshotChunk.History[V, H](c.toVector): SnapshotChunk[V, H]) ++
        // the pieces are separate reads: a commit in between would tear the snapshot, so it is refused instead
        Stream
          .eval(state.latest.flatMap { n =>
            if (n.value == cur.seq) ().pure[F]
            else
              (vkey(cur.head.version), headKey).tupled.flatMap {
                case (was, now) => fail[Unit](VersionedTrieError.VersionConflict(Some(was), now))
              }
          })
          .drain
      }
    }

    // ---- restore

    def restore(trusted: Head[V, H], snapshot: Snapshot[V, H]): F[Head[V, H]] =
      for {
        replica <- lift(Replica.fromSnapshot(trusted, snapshot))
        now     <- headKey
        _       <- now.traverse_(k => fail[Unit](VersionedTrieError.VersionConflict(None, Some(k))))
        // the base gets the sequence it would have had: one per committed version, so the index is dense and real
        at = version(snapshot.history.size.toLong)
        s     <- state.stageRestore(replica.state, at)
        h     <- history.stageRestore(replica.history, at)
        v     <- values.stageRestore(snapshot.entries, at)
        index <- snapshot.history.zipWithIndex.traverse { case ((u, root), i) => vkey(u).map(indexWrites(i.toLong + 1L, _, root)) }
        wrote <- raw.writeIf(s.guards ++ h.guards ++ v.guards :+ (seqKey(1L) -> None), s.batch ++ h.batch ++ v.batch ++ index.combineAll)
        _     <- if (wrote) ().pure[F] else headKey.flatMap(k => fail[Unit](VersionedTrieError.VersionConflict(None, k)))
      } yield trusted

    def restoreFrom(trusted: Head[V, H], chunks: Stream[F, SnapshotChunk[V, H]]): F[Head[V, H]] =
      chunks
        .evalScan(SnapshotReader.start(trusted))((r, c) => lift(r.feed(c)))
        .compile
        .last
        .flatMap(r => lift(r.getOrElse(SnapshotReader.start(trusted)).finish))
        .flatMap(restore(trusted, _))

    // ---- retention

    private def writeAll(was: Current[V, H], parts: List[(List[(ByteVector, Option[ByteVector])], RawBatch)]): F[Unit] =
      raw.writeIf(parts.flatMap(_._1), parts.foldMap(_._2)).flatMap { wrote =>
        if (wrote) ().pure[F]
        else
          (vkey(was.head.version), headKey).tupled.flatMap { case (k, now) => fail[Unit](VersionedTrieError.VersionConflict(Some(k), now)) }
      }

    def rollback(to: V): F[Unit] =
      for {
        cur   <- current
        tk    <- vkey(to)
        n     <- retainedSeq(tk)
        later <- raw.scan(ByteRange(Some(seqKey(n + 1)), Some(seqKey(cur.seq + 1)))).compile.toList
        s     <- state.stageRollback(version(n))
        h     <- history.stageRollback(version(n))
        v     <- values.stageRollback(version(n))
        forget = later.foldMap { case (k, record) => RawBatch.delete(k) ++ RawBatch.delete(byVersion ++ record.take(V.width.toLong)) }
        _ <- writeAll(cur, List(s.guards -> s.batch, h.guards -> h.batch, v.guards -> v.batch, Nil -> forget))
      } yield ()

    def prune(through: V): F[Unit] =
      for {
        cur <- current
        n   <- vkey(through).flatMap(retainedSeq)
        s   <- state.stagePrune(version(n))
        h   <- history.stagePrune(version(n))
        v   <- values.stagePrune(version(n))
        _   <- writeAll(cur, List(s.guards -> s.batch, h.guards -> h.batch, v.guards -> v.batch))
      } yield ()
  }
}
