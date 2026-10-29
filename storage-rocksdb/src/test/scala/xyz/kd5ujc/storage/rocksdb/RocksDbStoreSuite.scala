package xyz.kd5ujc.storage.rocksdb

import java.nio.file.{Files, Path}
import java.util.Comparator

import cats.effect.{IO, Resource}
import cats.mtl.Raise

import xyz.kd5ujc.storage.{RawStore, StoreBehaviours, StoreError}

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object RocksDbStoreSuite extends SimpleIOSuite with Checkers with StoreBehaviours {

  def backend: String = "rocksdb"

  private val tempDir: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("dilf4s-rocksdb-")))(dir =>
      IO.blocking(Files.walk(dir).sorted(Comparator.reverseOrder[Path]()).forEach(p => Files.delete(p)))
    )

  def withRaw[A](use: RawStore[IO] => IO[A])(implicit R: Raise[IO, StoreError]): IO[A] =
    tempDir.flatMap(dir => RocksDbStore.open[IO](dir.resolve("db"), RocksDbConfig(syncWrites = false, scanChunkSize = 7))).use(use)

  test("data survives closing and reopening the database") {
    expectOk { implicit r =>
      tempDir.use { dir =>
        val path = dir.resolve("db")
        RocksDbStore.open[IO](path).use(_.write(xyz.kd5ujc.storage.RawBatch.put(scodec.bits.ByteVector(1), scodec.bits.ByteVector(2)))) >>
        RocksDbStore.open[IO](path).use(_.get(scodec.bits.ByteVector(1))).map(v => expect.same(Some(scodec.bits.ByteVector(2)), v))
      }
    }
  }

  test("a second open of a locked database is a typed backend error") {
    attempt { implicit r =>
      tempDir.use { dir =>
        val path = dir.resolve("db")
        RocksDbStore.open[IO](path).use(_ => RocksDbStore.open[IO](path).use(_ => IO.unit))
      }
    }.map {
      case Left(StoreError.Backend(_, _)) => success
      case other                          => failure(s"expected a backend error, got $other")
    }
  }
}
