package xyz.kd5ujc.storage.rocksdb

import java.nio.file.Path

import cats.effect.{Resource, Sync}
import cats.mtl.Raise
import cats.syntax.all._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import xyz.kd5ujc.storage.ByteOrder.unsigned
import xyz.kd5ujc.storage._

import fs2.{Chunk, Stream}
import org.rocksdb.{Options, ReadOptions, RocksDB, RocksDBException, RocksIterator, WriteBatch, WriteOptions}
import scodec.bits.ByteVector

/** Tuning that affects durability and memory. */
final case class RocksDbConfig(syncWrites: Boolean = true, scanChunkSize: Int = 256)

/**
 * [[RawStore]] on RocksDB. Every native call runs in `Sync[F].blocking`, native handles are scoped by `Resource`,
 * batches use one `WriteBatch` (atomic), and scans iterate a snapshot. `RocksDBException`s become
 * [[StoreError.Backend]]; any other failure stays in `F`'s error channel.
 */
object RocksDbStore {

  def open[F[_]: Sync](path: Path, config: RocksDbConfig = RocksDbConfig())(implicit
    R: Raise[F, StoreError]
  ): Resource[F, RawStore[F]] =
    for {
      _       <- Resource.eval(native("load library")(RocksDB.loadLibrary()))
      options <- Resource.fromAutoCloseable(Sync[F].delay(new Options().setCreateIfMissing(true)))
      db      <- Resource.fromAutoCloseable(native(s"open $path")(RocksDB.open(options, path.toString)))
      writes  <- Resource.fromAutoCloseable(Sync[F].delay(new WriteOptions().setSync(config.syncWrites)))
    } yield new Impl[F](db, writes, config)

  private def native[F[_]: Sync, A](operation: String)(thunk: => A)(implicit R: Raise[F, StoreError]): F[A] =
    Sync[F].blocking(thunk).recoverWith { case e: RocksDBException =>
      R.raise[StoreError, A](StoreError.Backend(operation, Option(e.getMessage).getOrElse(e.getClass.getName)))
    }

  final private class Impl[F[_]: Sync](db: RocksDB, writeOptions: WriteOptions, config: RocksDbConfig)(implicit
    R: Raise[F, StoreError]
  ) extends RawStore[F] {

    def get(key: ByteVector): F[Option[ByteVector]] =
      native("get")(Option(db.get(key.toArray)).map(ByteVector.view))

    def getAll(keys: List[ByteVector]): F[List[Option[ByteVector]]] =
      if (keys.isEmpty) List.empty[Option[ByteVector]].pure[F]
      else native("multiGet")(db.multiGetAsList(keys.map(_.toArray).asJava).asScala.toList.map(v => Option(v).map(ByteVector.view)))

    def write(batch: RawBatch): F[Unit] =
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
            _ <- Resource.eval(native("seek")(range.from.fold(iterator.seekToFirst())(f => iterator.seek(f.toArray))))
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
