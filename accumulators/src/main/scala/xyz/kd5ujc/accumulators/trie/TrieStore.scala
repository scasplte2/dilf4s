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

  /** Discards versions after `to`; their nodes are freed unless an older version shares them. */
  def rollback(to: Version): F[Unit]

  /** Discards versions older than `through`; `through` becomes the oldest readable version (the floor). */
  def prune(through: Version): F[Unit]
}

object TrieStore {

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
  final private case class State(latest: Long, floor: Long, raw: Option[ByteVector]) {
    def encode(l: Long, f: Long): ByteVector = ByteVector.fromLong(l) ++ ByteVector.fromLong(f)
  }

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
        case None                     => State(0L, 0L, None).pure[F]
        case Some(b) if b.size == 16L => State(b.take(8).toLong(signed = false), b.drop(8).toLong(signed = false), Some(b)).pure[F]
        case Some(b)                  => failS(StoreError.Corrupt(s"trie state has ${b.size} bytes"))
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

    private def guarded(s: State, newLatest: Long, newFloor: Long, batch: RawBatch): F[Unit] =
      raw.writeIf(List(stateKey -> s.raw), batch ++ RawBatch.put(stateKey, s.encode(newLatest, newFloor))).flatMap { wrote =>
        if (wrote) ().pure[F] else state.flatMap(now => failS(StoreError.VersionConflict(s.latest, now.latest)))
      }

    // ---- reads

    def get(key: ByteVector, version: Version): F[Option[Digest[H]]] =
      for {
        s    <- state
        _    <- checkReadable(s, version)
        path <- liftT(mode.path(key))
        r    <- rootHash(version.value)
        v    <- TrieEngine.get[F, H](node, r, path)
      } yield v

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

    // ---- writes

    def commit(expected: Version, ops: List[(ByteVector, Option[Digest[H]])]): F[Version] =
      state.flatMap { s =>
        if (s.latest != expected.value) failS(StoreError.VersionConflict(expected.value, s.latest))
        else
          for {
            trieOps <- liftT(ops.traverse { case (k, v) => mode.path(k).map(TrieOp(_, v)) })
            current <- rootHash(s.latest)
            delta   <- TrieEngine.apply[F, H](node, current, trieOps)
            plan    <- RefCounting.plan[F, H](entry, delta, retainNewRoot = true, release = Nil)
            next = s.latest + 1
            rootRec <- roots.encodeBatch(Batch.put(next, delta.root.fold(ByteVector.empty)(_.bytes)))
            _       <- guarded(s, next, s.floor, planBatch(plan) ++ rootRec)
          } yield Version.unsafe(next)
      }

    /** Releases the roots of `versions` and deletes their root records, in one guarded batch. */
    private def dropVersions(s: State, versions: List[Long], newLatest: Long, newFloor: Long): F[Unit] =
      for {
        released <- versions.traverse(rootHash).map(_.flatten)
        plan     <- RefCounting.plan[F, H](entry, TrieDelta[H](None, Map.empty), retainNewRoot = false, release = released)
        deletes  <- roots.encodeBatch(Batch.deletes(versions))
        _        <- guarded(s, newLatest, newFloor, planBatch(plan) ++ deletes)
      } yield ()

    def rollback(to: Version): F[Unit] =
      state.flatMap(s => checkReadable(s, to) >> dropVersions(s, ((to.value + 1) to s.latest).toList, to.value, s.floor))

    def prune(through: Version): F[Unit] =
      state.flatMap(s =>
        checkReadable(s, through) >> dropVersions(s, (s.floor until through.value).toList.filter(_ > 0L), s.latest, through.value)
      )
  }
}
