package xyz.kd5ujc.storage.rocksdb

import java.nio.file.Path

import cats.effect.std.Mutex
import cats.effect.{Async, Resource, Sync}
import cats.mtl.Raise
import cats.syntax.all._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import xyz.kd5ujc.storage.ByteOrder.unsigned
import xyz.kd5ujc.storage._

import fs2.{Chunk, Stream}
import org.rocksdb._
import scodec.bits.ByteVector

/**
 * Durability and performance tuning.
 *   - `bloomBitsPerKey`: bloom filters make point reads of absent keys cheap (trie nodes are random digest keys); 0
 *     disables them.
 *   - `blockCacheBytes`: shared LRU cache for uncompressed blocks.
 *   - `compression`: LZ4 by default. RocksDB compacts in the background, so space freed by deletes (pruned history,
 *     garbage-collected nodes) returns as compaction runs; [[RocksDbHandle.compact]] forces it for a range.
 */
final case class RocksDbConfig(
  syncWrites:      Boolean = true,
  scanChunkSize:   Int = 256,
  bloomBitsPerKey: Int = 10,
  blockCacheBytes: Long = 64L * 1024 * 1024,
  compression:     CompressionType = CompressionType.LZ4_COMPRESSION
)

/** A [[RawStore]] with RocksDB maintenance operations. */
trait RocksDbHandle[F[_]] extends RawStore[F] {

  /** Compacts `range` now rather than on RocksDB's schedule, reclaiming space from deletes and pruned history. */
  def compact(range: ByteRange): F[Unit]
}

/**
 * [[RawStore]] on RocksDB. Every native call runs in `Sync[F].blocking`, native handles are scoped by `Resource`,
 * batches use one `WriteBatch` (atomic), and scans iterate a snapshot. Writes are serialised by one lock per open
 * database; RocksDB's own lock file guarantees a database is open at most once, so the lock covers every writer and
 * makes [[RawStore.writeIf]] a true compare-and-write. `RocksDBException`s become
 * [[StoreError.Backend]]; any other failure stays in `F`'s error channel.
 */
object RocksDbStore {

  def open[F[_]: Async](path: Path, config: RocksDbConfig = RocksDbConfig())(
    implicit R: Raise[F, StoreError]
  ): Resource[F, RocksDbHandle[F]] =
    for {
      _      <- Resource.eval(native("load library")(RocksDB.loadLibrary()))
      cache  <- Resource.fromAutoCloseable(Sync[F].delay(new LRUCache(config.blockCacheBytes)))
      filter <- Resource.fromAutoCloseable(Sync[F].delay(new BloomFilter(math.max(config.bloomBitsPerKey, 1).toDouble, false)))
      options <- Resource.fromAutoCloseable(Sync[F].delay {
        val table = new BlockBasedTableConfig().setBlockCache(cache)
        val tuned = if (config.bloomBitsPerKey > 0) table.setFilterPolicy(filter) else table
        new Options().setCreateIfMissing(true).setTableFormatConfig(tuned).setCompressionType(config.compression)
      })
      db     <- Resource.fromAutoCloseable(native(s"open $path")(RocksDB.open(options, path.toString)))
      writes <- Resource.fromAutoCloseable(Sync[F].delay(new WriteOptions().setSync(config.syncWrites)))
      lock   <- Resource.eval(Mutex[F])
    } yield new Impl[F](db, writes, lock, config)

  private def native[F[_]: Sync, A](operation: String)(thunk: => A)(implicit R: Raise[F, StoreError]): F[A] =
    Sync[F].blocking(thunk).recoverWith {
      case e: RocksDBException =>
        R.raise[StoreError, A](StoreError.Backend(operation, Option(e.getMessage).getOrElse(e.getClass.getName)))
    }

  final private class Impl[F[_]: Sync](db: RocksDB, writeOptions: WriteOptions, lock: Mutex[F], config: RocksDbConfig)(
    implicit R: Raise[F, StoreError]
  ) extends RocksDbHandle[F] {

    def compact(range: ByteRange): F[Unit] =
      range.until.fold(native("compact")(db.compactRange())) { until =>
        native("compact")(db.compactRange(range.from.fold(Array.emptyByteArray)(_.toArray), until.toArray))
      }

    def get(key: ByteVector): F[Option[ByteVector]] =
      native("get")(Option(db.get(key.toArray)).map(ByteVector.view))

    def getAll(keys: List[ByteVector]): F[List[Option[ByteVector]]] =
      if (keys.isEmpty) List.empty[Option[ByteVector]].pure[F]
      else native("multiGet")(db.multiGetAsList(keys.map(_.toArray).asJava).asScala.toList.map(v => Option(v).map(ByteVector.view)))

    def write(batch: RawBatch): F[Unit] = lock.lock.surround(unguardedWrite(batch))

    def writeIf(guards: List[(ByteVector, Option[ByteVector])], batch: RawBatch): F[Boolean] =
      lock.lock.surround {
        getAll(guards.map(_._1)).flatMap { current =>
          val matches = current.zip(guards).forall { case (actual, (_, expected)) => actual == expected }
          unguardedWrite(batch).whenA(matches).as(matches)
        }
      }

    private def unguardedWrite(batch: RawBatch): F[Unit] =
      if (batch.isEmpty) ().pure[F]
      else
        Resource.fromAutoCloseable(Sync[F].delay(new WriteBatch())).use { wb =>
          batch.ops.traverse_ {
            case RawOp.Put(k, v) => native("batch put")(wb.put(k.toArray, v.toArray))
            case RawOp.Delete(k) => native("batch delete")(wb.delete(k.toArray))
          } >> native("write")(db.write(writeOptions, wb))
        }

    def scan(range: ByteRange): Stream[F, (ByteVector, ByteVector)] =
      Stream
        .resource(
          for {
            snapshot <- Resource.make(Sync[F].delay(db.getSnapshot))(s => Sync[F].delay(db.releaseSnapshot(s)))
            read     <- Resource.fromAutoCloseable(Sync[F].delay(new ReadOptions().setSnapshot(snapshot)))
            iterator <- Resource.fromAutoCloseable(Sync[F].delay(db.newIterator(read)))
            _        <- Resource.eval(native("seek")(range.from.fold(iterator.seekToFirst())(f => iterator.seek(f.toArray))))
          } yield iterator
        )
        .flatMap(iterator => Stream.unfoldChunkEval(())(_ => nextChunk(iterator, range)))

    /** Reads up to `scanChunkSize` in-range entries; the iterator is only touched inside `blocking`. */
    private def nextChunk(it: RocksIterator, range: ByteRange): F[Option[(Chunk[(ByteVector, ByteVector)], Unit)]] =
      native("iterate")(read(it, range, config.scanChunkSize, Vector.empty))
        .flatMap(entries => native("status")(it.status()).as(Option.when(entries.nonEmpty)((Chunk.from(entries), ()))))

    /** Advances the iterator only past entries it returns, so no entry is skipped between chunks. */
    @tailrec
    private def read(
      it:        RocksIterator,
      range:     ByteRange,
      remaining: Int,
      acc:       Vector[(ByteVector, ByteVector)]
    ): Vector[(ByteVector, ByteVector)] =
      if (remaining <= 0 || !it.isValid) acc
      else {
        val key = ByteVector.view(it.key())
        if (range.until.exists(u => !unsigned.lt(key, u))) acc
        else {
          val entry = (key, ByteVector.view(it.value()))
          it.next()
          read(it, range, remaining - 1, acc :+ entry)
        }
      }
  }
}
