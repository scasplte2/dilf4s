package xyz.kd5ujc.storage.versioned_store

import java.nio.file.Path
import java.util.UUID

import cats.effect.{Ref, Resource, Sync}
import cats.implicits._

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.VersionedStore
import xyz.kd5ujc.storage.store.LevelDbStore
import xyz.kd5ujc.storage.versioned_store.schema.{Catalog, Diff, Meta}

import io.circe.{Decoder, Encoder}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * A *VersionedStore* implementation that is adopted from a similar imperative approach in Ergo
 * https://github.com/ergoplatform/ergo/blob/master/avldb/src/main/scala/scorex/db/LDBVersionedStore.scala
 */
object VersionedLevelDbStore {

  def make[F[_]: Sync: JsonSerializer, Key: Encoder: Decoder, Value: Encoder: Decoder](
    base: Path
  ): Resource[F, VersionedStore[F, Key, Value]] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass(VersionedLevelDbStore.getClass)

    for {
      ldbFactory <- LevelDbStore.makeFactory[F](useJni = true)
      mainDb     <- LevelDbStore.makeDb[F](Path.of(base.toAbsolutePath.toString, "/main"), ldbFactory)
      undoDb     <- LevelDbStore.makeDb[F](Path.of(base.toAbsolutePath.toString, "/undo"), ldbFactory)
      metaDb     <- LevelDbStore.makeDb[F](Path.of(base.toAbsolutePath.toString, "/meta"), ldbFactory)
      main       <- LevelDbStore.make[F, Key, Value](mainDb)
      undo       <- LevelDbStore.make[F, Long, Diff[Key, Value]](undoDb)
      meta       <- LevelDbStore.make[F, UUID, Meta](metaDb)
      ref <- Resource.liftK(
        Ref.of[F, Catalog[F, Key, Value]](Catalog(main, undo, meta)).flatTap { ref =>
          ref.get.flatMap { state =>
            state.meta.get(Meta.reserved).flatMap {
              case Some(_) => ().pure[F]
              case None    => state.meta.put(Meta.reserved, Meta.genesis)
            }
          }
        }
      )
      store = impl[F, Key, Value](ref)
    } yield store
  }
}
