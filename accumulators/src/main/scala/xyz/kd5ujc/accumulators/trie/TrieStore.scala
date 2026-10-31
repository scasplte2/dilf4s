package xyz.kd5ujc.accumulators.trie

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.Concurrent
import cats.mtl.Raise
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.core.hash.Digest
import xyz.kd5ujc.storage._

import scodec.bits.ByteVector

/**
 * A versioned, persistent authenticated dictionary (docs/specs/trie.md §6). Nodes are stored by digest with a reference
 * count; every retained version records its root, so any retained version can be read and proven. Commits, rollbacks
 * and prunes are each one compare-and-write on the version state, so concurrent instances stay consistent and
 * garbage collection is atomic with the change that causes it.
 *
 * Each mutation is also available staged, computed without writing (docs/specs/storage.md §3), so that several stores
 * in one backend can make one atomic compare-and-write, and a staged commit is a draft later stages can build on.
 */
trait TrieStore[F[_], H] {
  def mode: KeyMode
  def latest: F[Version]
  def floor: F[Version]
  def root(version:    Version): F[Digest[H]]
  def get(key:         ByteVector, version: Version): F[Option[Digest[H]]]
  def commit(expected: Version, ops:        List[(ByteVector, Option[Digest[H]])]): F[Version]
  def prove(
    keys:     List[ByteVector],
    prefixes: List[ByteVector],
    version:  Version,
    ranges:   List[(ByteVector, Option[ByteVector])] = Nil
  ): F[TrieProof[H]]

  /** The root `ops` would give applied to a retained version, without writing: what that version plus `ops` commits to. */
  def rootAfter(version: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[Digest[H]]

  /** Discards versions after `to`; their nodes are freed unless an older version shares them. */
  def rollback(to: Version): F[Unit]

  /** Discards versions older than `through`; `through` becomes the oldest readable version (the floor). */
  def prune(through: Version): F[Unit]

  /**
   * The state record as it is now. A write guarded on it applies only if the store has not been mutated since, even if
   * it has returned to the same version: every mutation advances a generation that never repeats.
   */
  def revision: F[Revision]

  /** The commit `commit(expected, ops)` would make, without writing. */
  def stage(expected: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[TrieStore.Staged[H]]

  /** A commit on top of a staged one that is not yet written: nodes are read through the draft, and writing it writes both. */
  def stageOn(base: TrieStore.Staged[H], ops: List[(ByteVector, Option[Digest[H]])]): F[TrieStore.Staged[H]]

  /**
   * Loads a whole trie into an empty store as version `at`, which becomes both the latest and the floor: the store is
   * then exactly as if it had committed up to `at` and pruned through it. Versions below `at` are not readable.
   */
  def stageRestore(trie: Trie[H], at: Version): F[TrieStore.Staged[H]]

  def stageRollback(to:   Version): F[TrieStore.Staged[H]]
  def stagePrune(through: Version): F[TrieStore.Staged[H]]

  /** Applies a stage as one compare-and-write; a `VersionConflict` if the store moved since it was staged. */
  def write(staged: TrieStore.Staged[H]): F[Version]
}

object TrieStore {

  /**
   * A mutation computed but not written: `writeIf(guards, batch)` applies it. `version` is the latest version after it
   * and `root` that version's root. A stage built on a draft carries the draft's guards and writes, and reads nodes
   * through its plan, so it applies both or neither.
   */
  final case class Staged[H] private[trie] (
    guards:                       List[(ByteVector, Option[ByteVector])],
    batch:                        RawBatch,
    version:                      Version,
    root:                         Digest[H],
    private[trie] val top:        Option[Digest[H]],
    private[trie] val expected:   Long,
    private[trie] val generation: Long,
    private[trie] val floor:      Long,
    private[trie] val nodes:      RefCounting.Plan[H]
  )

  def make[F[_]: Concurrent, H: TrieHashing](raw: RawStore[F], name: String, keyMode: KeyMode)(
    implicit RS: Raise[F, StoreError],
    RT:          Raise[F, TrieError]
  ): F[TrieStore[F, H]] =
    (Namespace(s"$name/node"), Namespace(s"$name/root"), Namespace(s"$name/meta")).tupled.fold(
      RS.raise[StoreError, TrieStore[F, H]],
      {
        case (nodeNs, rootNs, metaNs) =>
          (new Impl[F, H](
            raw,
            keyMode,
            nodeNs.prefix,
            KeyValueStore[F, Long, ByteVector](raw, rootNs),
            metaNs.prefix ++ ByteVector("state".getBytes(UTF_8))
          ): TrieStore[F, H]).pure[F]
      }
    )

  /** Latest and floor versions, with the raw record kept to guard the compare-and-write. */
  final private case class State(latest: Long, floor: Long, generation: Long, raw: Option[ByteVector])

  final private class Impl[F[_]: Concurrent, H](
    raw:      RawStore[F],
    val mode: KeyMode,
    nodeNs:   ByteVector,
    roots:    KeyValueStore[F, Long, ByteVector],
    stateKey: ByteVector
  )(implicit T: TrieHashing[H], RS: Raise[F, StoreError], RT: Raise[F, TrieError])
      extends TrieStore[F, H] {

    private def failS[A](e: StoreError): F[A] = RS.raise[StoreError, A](e)
    private def failT[A](e: TrieError): F[A] = RT.raise[TrieError, A](e)
    private def liftT[A](e: Either[TrieError, A]): F[A] = e.fold(failT[A], _.pure[F])

    // ---- state and roots

    private def state: F[State] =
      raw.get(stateKey).flatMap {
        case None => State(0L, 0L, 0L, None).pure[F]
        case Some(b) if b.size == 24L =>
          State(b.take(8).toLong(signed = false), b.slice(8L, 16L).toLong(signed = false), b.drop(16).toLong(signed = false), Some(b))
            .pure[F]
        case Some(b) => failS(StoreError.Corrupt(s"trie state has ${b.size} bytes"))
      }

    def latest: F[Version] = state.map(s => Version.unsafe(s.latest))
    def floor: F[Version] = state.map(s => Version.unsafe(s.floor))

    private def checkReadable(s: State, v: Version): F[Unit] =
      if (v.value > s.latest) failS(StoreError.UnknownVersion(v.value, s.latest))
      else if (v.value < s.floor) failS(StoreError.Pruned(v.value, s.floor))
      else ().pure[F]

    /** The stored root of a version; an absent record means the empty trie (version 0 is always empty). */
    private def rootHash(version: Long): F[Option[Digest[H]]] =
      roots.get(version).flatMap {
        case None                 => Option.empty[Digest[H]].pure[F]
        case Some(b) if b.isEmpty => Option.empty[Digest[H]].pure[F]
        case Some(b)              => digest(b).map(Some(_))
      }

    private def liftC[A](e: Either[String, A]): F[A] = e.fold(m => failS[A](StoreError.Corrupt(s"node record: $m")), _.pure[F])

    private def digest(b: ByteVector): F[Digest[H]] =
      Digest.fromBytes[H](b)(using T.hasher).fold(e => failS(StoreError.Corrupt(e.toString)), _.pure[F])

    def root(version: Version): F[Digest[H]] =
      state.flatMap(s => checkReadable(s, version) >> rootHash(version.value).map(_.getOrElse(T.empty)))

    // ---- nodes: count (4 bytes) ++ canonical node record, re-hashed and checked on load

    private def nodeKey(h: Digest[H]): ByteVector = nodeNs ++ h.bytes

    private def encodeEntry(n: Node[H], count: Int): ByteVector = {
      val record = n match {
        case l: Leaf[H]   => NodeCodec.leaf(l.segment, l.value)
        case b: Branch[H] => NodeCodec.branch(b.segment, b.value.map(_.bytes), b.left.map(_.bytes), b.right.map(_.bytes))
      }
      ByteVector.fromInt(count) ++ Cbor.encode(record)
    }

    /** The generated `trie-node` decoder checks the record's shape; this classifies it and checks the invariant. */
    private def decodeEntry(h: Digest[H], bytes: ByteVector): F[(Node[H], Int)] = {
      val parsed: F[Node[H]] =
        liftC(
          Cbor.decode(bytes.drop(4)).leftMap(_.toString).flatMap(xyz.kd5ujc.models.trie.TrieNode.codec.decoder.decode(_).leftMap(_.message))
        ).flatMap { w =>
          (liftT(NodeCodec.fromWire(w.s)), w.v.traverse(digest), w.l.traverse(digest), w.r.traverse(digest)).tupled.flatMap {
            case (segment, Some(v), None, None) => (Node.leaf(segment, v): Node[H]).pure[F]
            case (segment, v, l, r) if Node.validBranch(v, l.fold(0)(_ => 1) + r.fold(0)(_ => 1)) =>
              (Node.branch(segment, v, l, r): Node[H]).pure[F]
            case _ => failS[Node[H]](StoreError.Corrupt("node violates the branch invariant"))
          }
        }
      parsed.flatMap { n =>
        if (n.hash != h) failS(StoreError.Corrupt(s"node ${h.hex} hashes to ${n.hash.hex}"))
        else (n, bytes.take(4).toInt()).pure[F]
      }
    }

    private def entry(h: Digest[H]): F[Option[(Node[H], Int)]] = raw.get(nodeKey(h)).flatMap(_.traverse(decodeEntry(h, _)))

    private def node(h: Digest[H]): F[Node[H]] = entry(h).flatMap(_.fold(failT[Node[H]](TrieError.MissingNode(h.hex)))(e => e._1.pure[F]))

    private def planBatch(plan: RefCounting.Plan[H]): RawBatch =
      plan.foldLeft(RawBatch.empty) {
        case (acc, (h, Some((n, c)))) => acc ++ RawBatch.put(nodeKey(h), encodeEntry(n, c))
        case (acc, (h, None))         => acc ++ RawBatch.delete(nodeKey(h))
      }

    /**
     * latest ‖ floor ‖ generation. The generation advances with every mutation and never repeats, so a stage staged
     * before a rollback is refused even after the store re-reaches its version (latest and floor alone would repeat).
     */
    private def stateRecord(latest: Long, floor: Long, generation: Long): ByteVector =
      ByteVector.fromLong(latest) ++ ByteVector.fromLong(floor) ++ ByteVector.fromLong(generation)

    def revision: F[Revision] = state.map(s => Revision(List(stateKey -> s.raw)))

    // ---- reads

    def get(key: ByteVector, version: Version): F[Option[Digest[H]]] =
      for {
        s    <- state
        _    <- checkReadable(s, version)
        path <- liftT(mode.path(key))
        r    <- rootHash(version.value)
        v    <- TrieEngine.get[F, H](node, r, path)
      } yield v

    def rootAfter(version: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[Digest[H]] =
      for {
        s       <- state
        _       <- checkReadable(s, version)
        trieOps <- liftT(ops.traverse { case (k, v) => mode.path(k).map(TrieOp(_, v)) })
        r       <- rootHash(version.value)
        delta   <- TrieEngine.apply[F, H](node, r, trieOps)
      } yield delta.root.getOrElse(T.empty)

    def prove(
      keys:     List[ByteVector],
      prefixes: List[ByteVector],
      version:  Version,
      ranges:   List[(ByteVector, Option[ByteVector])]
    ): F[TrieProof[H]] =
      for {
        s     <- state
        _     <- checkReadable(s, version)
        paths <- liftT(keys.traverse(mode.path))
        r     <- rootHash(version.value)
        queries = paths.map(TrieProofs.Key.apply) ++ prefixes.map(p => TrieProofs.Prefix(p.bits)) ++
          ranges.map { case (f, u) => TrieProofs.Range(f.bits, u.map(_.bits)) }
        proof <- TrieProofs.prove[F, H](node, r, queries)
      } yield proof

    // ---- writes: each is one staged compare-and-write

    def commit(expected: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[Version] = stage(expected, ops).flatMap(write)
    def rollback(to:     Version): F[Unit] = stageRollback(to).flatMap(write).void
    def prune(through:   Version): F[Unit] = stagePrune(through).flatMap(write).void

    def write(staged: Staged[H]): F[Version] =
      raw.writeIf(staged.guards, staged.batch).flatMap { wrote =>
        if (wrote) staged.version.pure[F] else state.flatMap(now => failS(StoreError.VersionConflict(staged.expected, now.latest)))
      }

    def stage(expected: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[Staged[H]] =
      state.flatMap { s =>
        if (s.latest != expected.value) failS(StoreError.VersionConflict(expected.value, s.latest))
        else
          rootHash(s.latest).flatMap(current =>
            stageCommit(List(stateKey -> s.raw), RawBatch.empty, s.latest, s.generation + 1, s.latest, s.floor, current, Map.empty, ops)
          )
      }

    def stageOn(base: Staged[H], ops: List[(ByteVector, Option[Digest[H]])]): F[Staged[H]] =
      stageCommit(base.guards, base.batch, base.expected, base.generation, base.version.value, base.floor, base.top, base.nodes, ops)

    /** One commit after `current` (whose root is `top`), reading nodes through `overlay` (the drafts below it). */
    private def stageCommit(
      guards:     List[(ByteVector, Option[ByteVector])],
      below:      RawBatch,
      expected:   Long,
      generation: Long,
      current:    Long,
      floor:      Long,
      top:        Option[Digest[H]],
      overlay:    RefCounting.Plan[H],
      ops:        List[(ByteVector, Option[Digest[H]])]
    ): F[Staged[H]] = {
      val view: RefCounting.View[F, H] = h => overlay.get(h).fold(entry(h))(_.pure[F])
      val lookup: Digest[H] => F[Node[H]] = h => view(h).flatMap(_.fold(failT[Node[H]](TrieError.MissingNode(h.hex)))(e => e._1.pure[F]))
      val next = current + 1
      for {
        trieOps <- liftT(ops.traverse { case (k, v) => mode.path(k).map(TrieOp(_, v)) })
        delta   <- TrieEngine.apply[F, H](lookup, top, trieOps)
        plan    <- RefCounting.plan[F, H](view, delta, retainNewRoot = true, release = Nil)
        rootRec <- roots.encodeBatch(Batch.put(next, delta.root.fold(ByteVector.empty)(_.bytes)))
      } yield
        Staged(
          guards,
          below ++ planBatch(plan) ++ rootRec ++ RawBatch.put(stateKey, stateRecord(next, floor, generation)),
          Version.unsafe(next),
          delta.root.getOrElse(T.empty),
          delta.root,
          expected,
          generation,
          floor,
          overlay ++ plan
        )
    }

    def stageRestore(trie: Trie[H], at: Version): F[Staged[H]] =
      state.flatMap { s =>
        if (s.raw.isDefined) failS[Staged[H]](StoreError.VersionConflict(0L, s.latest))
        else if (trie.mode != mode) failT[Staged[H]](TrieError.InvalidKey(s"a $mode store cannot hold a ${trie.mode} trie"))
        else if (at.value < 1L) failS[Staged[H]](StoreError.UnknownVersion(at.value, 0L))
        else
          roots.encodeBatch(Batch.put(at.value, trie.rootOption.fold(ByteVector.empty)(_.bytes))).map { rootRec =>
            val plan: RefCounting.Plan[H] = trie.storedNodes.map { case (h, e) => h -> Option(e) }
            Staged(
              List(stateKey -> None),
              planBatch(plan) ++ rootRec ++ RawBatch.put(stateKey, stateRecord(at.value, at.value, 1L)),
              at,
              trie.root,
              trie.rootOption,
              0L,
              1L,
              at.value,
              plan
            )
          }
      }

    /** Releases the roots of `versions` and deletes their root records, as one stage. */
    private def dropVersions(s: State, versions: List[Long], newLatest: Long, newFloor: Long): F[Staged[H]] =
      for {
        released <- versions.traverse(rootHash).map(_.flatten)
        plan     <- RefCounting.plan[F, H](entry, TrieDelta[H](None, Map.empty), retainNewRoot = false, release = released)
        deletes  <- roots.encodeBatch(Batch.deletes(versions))
        top      <- rootHash(newLatest)
      } yield
        Staged(
          List(stateKey -> s.raw),
          planBatch(plan) ++ deletes ++ RawBatch.put(stateKey, stateRecord(newLatest, newFloor, s.generation + 1)),
          Version.unsafe(newLatest),
          top.getOrElse(T.empty),
          top,
          s.latest,
          s.generation + 1,
          newFloor,
          plan
        )

    def stageRollback(to: Version): F[Staged[H]] =
      state.flatMap(s => checkReadable(s, to) >> dropVersions(s, ((to.value + 1) to s.latest).toList, to.value, s.floor))

    def stagePrune(through: Version): F[Staged[H]] =
      state.flatMap(s =>
        checkReadable(s, through) >> dropVersions(s, (s.floor until through.value).toList.filter(_ > 0L), s.latest, through.value)
      )
  }
}
