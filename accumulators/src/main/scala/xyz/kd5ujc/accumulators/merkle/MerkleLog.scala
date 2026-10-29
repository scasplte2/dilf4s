package xyz.kd5ujc.accumulators.merkle

import cats.effect.{Concurrent, Ref}
import cats.mtl.Raise
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.storage._

import fs2.Stream
import scodec.bits.ByteVector

/**
 * A stateful RFC 9162 Merkle log. The in-memory variant holds a [[MerkleTree]] in a `Ref`. The persistent variant
 * stores only **perfect** subtree hashes, keyed by `(level, index)`, plus one state record (size and generation): it
 * keeps nothing in memory, opens instantly, and answers roots and proofs with O(log n) reads.
 *
 * Appends never rewrite a stored node, so a reader that takes the size first sees a consistent tree without snapshots.
 * `update`, `prepend` and `remove` do rewrite nodes; a reader racing one of them may assemble a proof that fails
 * verification, but never one that verifies falsely. Every write is a compare-and-write on the state record.
 */
trait MerkleLog[F[_], H] {
  def size: F[Long]
  def root: F[Digest[H]]
  def leafHash(index: Long): F[Digest[H]]
  def leaves: Stream[F, Digest[H]]
  def append(leafHashes: List[Digest[H]]): F[Unit]
  def update(index:      Long, leafHash: Digest[H]): F[Unit]

  /** O(n): every index shifts, and the result is not an append-only extension. */
  def prepend(leafHashes: List[Digest[H]]): F[Unit]

  /** O(n): later indices shift down, and the result is not an append-only extension. */
  def remove(index: Long): F[Unit]

  def inclusionProof(index:     Long): F[InclusionProof[H]]
  def consistencyProof(oldSize: Long): F[ConsistencyProof[H]]
}

object MerkleLog {

  def inMemory[F[_]: Concurrent, H: MerkleHashing](implicit RM: Raise[F, MerkleError]): F[MerkleLog[F, H]] =
    Ref.of[F, MerkleTree[H]](MerkleTree.empty[H]).map { ref =>
      new MerkleLog[F, H] {
        private def liftE[A](e: Either[MerkleError, A]): F[A] = e.fold(RM.raise[MerkleError, A], _.pure[F])
        private def modify(f: MerkleTree[H] => Either[MerkleError, MerkleTree[H]]): F[Unit] =
          ref.modify(t => f(t).fold(e => (t, e.asLeft[Unit]), n => (n, ().asRight[MerkleError]))).flatMap(liftE)

        def size: F[Long] = ref.get.map(_.size)
        def root: F[Digest[H]] = ref.get.map(_.root)
        def leafHash(index: Long): F[Digest[H]] = ref.get.flatMap(t => liftE(t.leafHash(index)))
        def leaves: Stream[F, Digest[H]] = Stream.eval(ref.get).flatMap(t => Stream.emits(t.leafHashes))
        def append(leafHashes:        List[Digest[H]]): F[Unit] = modify(_.appendAll(leafHashes).asRight)
        def update(index:             Long, leafHash: Digest[H]): F[Unit] = modify(_.update(index, leafHash))
        def prepend(leafHashes:       List[Digest[H]]): F[Unit] = modify(_.prependAll(leafHashes).asRight)
        def remove(index:             Long): F[Unit] = modify(_.removeAt(index))
        def inclusionProof(index:     Long): F[InclusionProof[H]] = ref.get.flatMap(t => liftE(t.inclusionProof(index)))
        def consistencyProof(oldSize: Long): F[ConsistencyProof[H]] = ref.get.flatMap(t => liftE(t.consistencyProof(oldSize)))
      }
    }

  /** Perfect nodes at `name/node` keyed by `(level, index)`; the state record (size ++ generation) at `name/meta`. */
  def persistent[F[_]: Concurrent, H: MerkleHashing: Hasher](raw: RawStore[F], name: String)(
    implicit RS: Raise[F, StoreError],
    RM:          Raise[F, MerkleError]
  ): F[MerkleLog[F, H]] =
    (Namespace(s"$name/node"), Namespace(s"$name/meta")).tupled.fold(
      RS.raise[StoreError, MerkleLog[F, H]],
      {
        case (nodeNs, metaNs) =>
          (new Persistent[F, H](
            raw,
            KeyValueStore[F, (Int, Long), ByteVector](raw, nodeNs),
            metaNs.prefix ++ ByteVector("state".getBytes("UTF-8"))
          ): MerkleLog[F, H]).pure[F]
      }
    )

  /** Size and generation, with the raw record kept to guard the compare-and-write. */
  final private case class State(size: Long, generation: Long, raw: Option[ByteVector]) {
    def next(newSize: Long): ByteVector = ByteVector.fromLong(newSize) ++ ByteVector.fromLong(generation + 1)
  }

  final private class Persistent[F[_]: Concurrent, H](
    raw:      RawStore[F],
    nodes:    KeyValueStore[F, (Int, Long), ByteVector],
    stateKey: ByteVector
  )(implicit M: MerkleHashing[H], H: Hasher[H], RS: Raise[F, StoreError], RM: Raise[F, MerkleError])
      extends MerkleLog[F, H] {

    private def failM[A](e: MerkleError): F[A] = RM.raise[MerkleError, A](e)
    private def failS[A](e: StoreError): F[A] = RS.raise[StoreError, A](e)

    private def state: F[State] =
      raw.get(stateKey).flatMap {
        case None                     => State(0L, 0L, None).pure[F]
        case Some(b) if b.size == 16L => State(b.take(8).toLong(signed = false), b.drop(8).toLong(signed = false), Some(b)).pure[F]
        case Some(b)                  => failS(StoreError.Corrupt(s"Merkle log state has ${b.size} bytes"))
      }

    private def perfect(level: Int, index: Long): F[Digest[H]] =
      nodes.get((level, index)).flatMap {
        case None    => failS(StoreError.Corrupt(s"missing perfect node ($level, $index)"))
        case Some(b) => Digest.fromBytes[H](b).fold(e => failS(StoreError.Corrupt(e.toString)), _.pure[F])
      }

    private def commit(read: State, newSize: Long, batch: Batch[(Int, Long), ByteVector]): F[Unit] =
      nodes.encodeBatch(batch).flatMap { writes =>
        raw.writeIf(List(stateKey -> read.raw), writes ++ RawBatch.put(stateKey, read.next(newSize))).flatMap { wrote =>
          if (wrote) ().pure[F] else state.flatMap(now => failS(StoreError.VersionConflict(read.generation, now.generation)))
        }
      }

    def size: F[Long] = state.map(_.size)
    def root: F[Digest[H]] = state.flatMap(s => PerfectNodes.root(perfect, s.size))

    def leafHash(index: Long): F[Digest[H]] =
      state.flatMap(s => if (index < 0 || index >= s.size) failM(MerkleError.IndexOutOfRange(index, s.size)) else perfect(0, index))

    def leaves: Stream[F, Digest[H]] =
      Stream.eval(state).flatMap { s =>
        nodes
          .scan(KeyRange.between((0, 0L), (0, s.size)))
          .evalMap { case (_, b) => Digest.fromBytes[H](b).fold(e => failS[Digest[H]](StoreError.Corrupt(e.toString)), _.pure[F]) }
      }

    /** The perfect nodes completed by `hashes` placed after `size` leaves, reading older children as needed. */
    private def placed(size: Long, hashes: List[Digest[H]], seed: Map[(Int, Long), Digest[H]]): F[Map[(Int, Long), Digest[H]]] =
      hashes.zipWithIndex.foldLeftM(seed) {
        case (written, (leaf, offset)) =>
          val index = size + offset
          PerfectNodes.completedBy(index).foldLeftM(written.updated((0, index), leaf)) {
            case (acc, (level, block)) =>
              def child(i: Long) = acc.get((level - 1, i)).fold(perfect(level - 1, i))(_.pure[F])
              (child(2 * block), child(2 * block + 1)).mapN(M.node).map(acc.updated((level, block), _))
          }
      }

    private def puts(written: Map[(Int, Long), Digest[H]]): Batch[(Int, Long), ByteVector] =
      Batch.puts(written.toList.map { case (k, d) => k -> d.bytes })

    def append(leafHashes: List[Digest[H]]): F[Unit] =
      if (leafHashes.isEmpty) ().pure[F]
      else state.flatMap(s => placed(s.size, leafHashes, Map.empty).flatMap(w => commit(s, s.size + leafHashes.size, puts(w))))

    def update(index: Long, leafHash: Digest[H]): F[Unit] =
      state.flatMap { s =>
        if (index < 0 || index >= s.size) failM(MerkleError.IndexOutOfRange(index, s.size))
        else
          PerfectNodes
            .ancestorsWithin(index, s.size)
            .foldLeftM(Map((0, index) -> leafHash)) {
              case (acc, (level, block)) =>
                def child(i: Long) = acc.get((level - 1, i)).fold(perfect(level - 1, i))(_.pure[F])
                (child(2 * block), child(2 * block + 1)).mapN(M.node).map(acc.updated((level, block), _))
            }
            .flatMap(w => commit(s, s.size, puts(w)))
      }

    /** Rewrites the whole log from an edited leaf list, deleting perfect nodes beyond the new size. */
    private def rewrite(s: State, edit: Vector[Digest[H]] => Either[MerkleError, Vector[Digest[H]]]): F[Unit] =
      leaves.compile.toVector.flatMap { current =>
        edit(current).fold(
          failM[Unit],
          next =>
            for {
              written <- placed(0L, next.toList, Map.empty)
              stale   <- nodes.scan().map(_._1).filterNot(written.contains).compile.toList
              _       <- commit(s, next.size.toLong, puts(written) ++ Batch.deletes(stale))
            } yield ()
        )
      }

    def prepend(leafHashes: List[Digest[H]]): F[Unit] = state.flatMap(s => rewrite(s, cur => (leafHashes.toVector ++ cur).asRight))

    def remove(index: Long): F[Unit] =
      state.flatMap(s =>
        rewrite(
          s,
          cur =>
            Either.cond(
              index >= 0 && index < cur.size.toLong,
              cur.patch(index.toInt, Nil, 1),
              MerkleError.IndexOutOfRange(index, cur.size.toLong)
            )
        )
      )

    def inclusionProof(index: Long): F[InclusionProof[H]] =
      state.flatMap { s =>
        if (index < 0 || index >= s.size) failM(MerkleError.IndexOutOfRange(index, s.size))
        else PerfectNodes.auditPath(perfect, index, 0L, s.size).map(InclusionProof(index, s.size, _))
      }

    def consistencyProof(oldSize: Long): F[ConsistencyProof[H]] =
      state.flatMap { s =>
        if (oldSize < 0 || oldSize > s.size) failM(MerkleError.InvalidSizes(oldSize, s.size))
        else if (oldSize == 0 || oldSize == s.size) ConsistencyProof(oldSize, s.size, Vector.empty[Digest[H]]).pure[F]
        else PerfectNodes.consistencyPath(perfect, oldSize, 0L, s.size, complete = true).map(ConsistencyProof(oldSize, s.size, _))
      }
  }
}
