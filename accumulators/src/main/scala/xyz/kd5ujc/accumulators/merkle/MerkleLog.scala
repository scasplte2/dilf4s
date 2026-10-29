package xyz.kd5ujc.accumulators.merkle

import cats.effect.{Concurrent, Ref}
import cats.mtl.Raise
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Digest, Hasher}
import xyz.kd5ujc.storage._

import scodec.bits.ByteVector

/**
 * A stateful Merkle tree. The in-memory variant holds the tree in a `Ref`; the persistent variant stores leaf hashes by
 * index in a [[RawStore]] and caches the tree, tagged with a generation counter. Every mutation is a compare-and-write
 * on that counter, so instances sharing a store never diverge: a stale instance reloads before answering.
 */
trait MerkleLog[F[_], H] {
  def tree: F[MerkleTree[H]]
  def size: F[Long]
  def root: F[Digest[H]]
  def append(leafHashes:        List[Digest[H]]): F[Unit]
  def update(index:             Long, leafHash: Digest[H]): F[Unit]
  def prepend(leafHashes:       List[Digest[H]]): F[Unit]
  def remove(index:             Long): F[Unit]
  def inclusionProof(index:     Long): F[InclusionProof[H]]
  def consistencyProof(oldSize: Long): F[ConsistencyProof[H]]
}

object MerkleLog {

  def inMemory[F[_]: Concurrent, H: MerkleHashing](implicit RM: Raise[F, MerkleError]): F[MerkleLog[F, H]] =
    Ref.of[F, MerkleTree[H]](MerkleTree.empty[H]).map { ref =>
      new Base[F, H] {
        def tree: F[MerkleTree[H]] = ref.get
        protected def modify(f: MerkleTree[H] => Either[MerkleError, MerkleTree[H]]): F[Unit] =
          ref
            .modify(t => f(t).fold(e => (t, e.asLeft[Unit]), n => (n, ().asRight[MerkleError])))
            .flatMap(_.fold(RM.raise[MerkleError, Unit], _.pure[F]))
      }
    }

  /** Leaves at `name/leaf` keyed by index, plus a generation and size record at `name/meta`. */
  def persistent[F[_]: Concurrent, H: MerkleHashing: Hasher](raw: RawStore[F], name: String)(
    implicit RS: Raise[F, StoreError],
    RM:          Raise[F, MerkleError]
  ): F[MerkleLog[F, H]] =
    (Namespace(s"$name/leaf"), Namespace(s"$name/meta")).tupled.fold(
      RS.raise[StoreError, MerkleLog[F, H]],
      {
        case (leafNs, metaNs) =>
          val genKey = metaNs.prefix ++ ByteVector("gen".getBytes("UTF-8"))
          val leaves = KeyValueStore[F, Long, ByteVector](raw, leafNs)

          def load: F[(Option[ByteVector], MerkleTree[H])] =
            for {
              gen    <- raw.get(genKey)
              hashes <- leaves.scan().map(_._2).compile.toVector
              digests <- hashes.traverse(b =>
                Digest.fromBytes[H](b).fold(e => RS.raise[StoreError, Digest[H]](StoreError.Corrupt(e.toString)), _.pure[F])
              )
            } yield (gen, MerkleTree.fromLeafHashes[H](digests))

          def genValue(gen: Option[ByteVector]): Long = gen.fold(0L)(_.toLong(signed = false))
          def nextGen(gen:  Option[ByteVector]): ByteVector = ByteVector.fromLong(genValue(gen) + 1)

          load.flatMap(Ref.of[F, (Option[ByteVector], MerkleTree[H])](_)).map { cache =>
            new Base[F, H] {

              /** Fresh when the stored generation matches the cached one; otherwise reloads. */
              private def current: F[(Option[ByteVector], MerkleTree[H])] =
                (cache.get, raw.get(genKey)).tupled.flatMap {
                  case (cached @ (gen, _), stored) =>
                    if (gen == stored) cached.pure[F] else load.flatTap(cache.set)
                }

              def tree: F[MerkleTree[H]] = current.map(_._2)

              /** Writes the leaves that differ between `before` and `after`, guarded on the generation. */
              protected def modify(f: MerkleTree[H] => Either[MerkleError, MerkleTree[H]]): F[Unit] =
                current.flatMap {
                  case (gen, before) =>
                    f(before).fold(
                      RM.raise[MerkleError, Unit],
                      after => {
                        val (b, a) = (before.leafHashes, after.leafHashes)
                        val changed =
                          Batch.puts[Long, ByteVector](a.indices.collect { case i if b.lift(i) != a.lift(i) => i.toLong -> a(i).bytes })
                        val dropped = Batch.deletes[Long, ByteVector]((a.size until b.size).map(_.toLong))
                        leaves.encodeBatch(changed ++ dropped).flatMap { batch =>
                          val next = nextGen(gen)
                          raw.writeIf(List(genKey -> gen), batch ++ RawBatch.put(genKey, next)).flatMap { wrote =>
                            if (wrote) cache.set((Some(next), after))
                            else
                              load.flatMap {
                                case fresh @ (stored, _) =>
                                  cache.set(fresh) >> RS
                                    .raise[StoreError, Unit](StoreError.VersionConflict(genValue(gen), genValue(stored)))
                              }
                          }
                        }
                      }
                    )
                }
            }
          }
      }
    )

  abstract private class Base[F[_]: Concurrent, H](implicit RM: Raise[F, MerkleError]) extends MerkleLog[F, H] {
    protected def modify(f: MerkleTree[H] => Either[MerkleError, MerkleTree[H]]): F[Unit]
    def size: F[Long] = tree.map(_.size)
    def root: F[Digest[H]] = tree.map(_.root)

    private def liftE[A](e: Either[MerkleError, A]): F[A] = e.fold(RM.raise[MerkleError, A], _.pure[F])

    def append(leafHashes:        List[Digest[H]]): F[Unit] = modify(_.appendAll(leafHashes).asRight)
    def update(index:             Long, leafHash: Digest[H]): F[Unit] = modify(_.update(index, leafHash))
    def prepend(leafHashes:       List[Digest[H]]): F[Unit] = modify(_.prependAll(leafHashes).asRight)
    def remove(index:             Long): F[Unit] = modify(_.removeAt(index))
    def inclusionProof(index:     Long): F[InclusionProof[H]] = tree.flatMap(t => liftE(t.inclusionProof(index)))
    def consistencyProof(oldSize: Long): F[ConsistencyProof[H]] = tree.flatMap(t => liftE(t.consistencyProof(oldSize)))
  }
}
