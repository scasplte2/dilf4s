package xyz.kd5ujc.storage.interpreters.versioned_store

import java.util.UUID

import cats.effect.{Ref, Sync}
import cats.implicits._

import xyz.kd5ujc.storage.algebras.VersionedStore
import xyz.kd5ujc.storage.interpreters.store.RefMapStore
import xyz.kd5ujc.storage.interpreters.versioned_store.schema.{Catalog, Diff, Meta}

import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object VersionedRefStore {

  def make[F[_]: Sync, Key, Value]: F[VersionedStore[F, Key, Value]] =
    for {
      implicit0(logger: SelfAwareStructuredLogger[F]) <- Slf4jLogger.fromClass[F](VersionedRefStore.getClass)
      main                                            <- RefMapStore.make[F, Key, Value]
      undo                                            <- RefMapStore.make[F, Long, Diff[Key, Value]]
      meta                                            <- RefMapStore.make[F, UUID, Meta]
      ref <- Ref.of[F, Catalog[F, Key, Value]](Catalog(main, undo, meta)).flatTap { ref =>
        ref.get.flatMap { state =>
          state.meta.get(Meta.reserved).flatMap {
            case Some(_) => ().pure[F]
            case None    => state.meta.put(Meta.reserved, Meta.genesis)
          }
        }
      }
    } yield impl[F, Key, Value](ref)
}
