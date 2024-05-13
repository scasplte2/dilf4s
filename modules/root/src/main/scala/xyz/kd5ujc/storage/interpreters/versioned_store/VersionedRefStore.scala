package xyz.kd5ujc.storage.interpreters.versioned_store

import java.util.UUID

import cats.effect.{Ref, Sync}
import cats.implicits._

import xyz.kd5ujc.storage.algebras.VersionedStore
import xyz.kd5ujc.storage.interpreters.store.RefMapStore
import xyz.kd5ujc.storage.interpreters.versioned_store.schema.{Catalog, Diff, Meta}

object VersionedRefStore {

  def make[F[_]: Sync, Key, Value]: F[VersionedStore[F, Key, Value]] =
    for {
      main <- RefMapStore.make[F, Key, Value]
      undo <- RefMapStore.make[F, Long, Diff[Key, Value]]
      meta <- RefMapStore.make[F, UUID, Meta]
      ref  <- Ref.of[F, Catalog[F, Key, Value]](Catalog(main, undo, meta))
    } yield impl[F, Key, Value](ref)
}
