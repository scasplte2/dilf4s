package xyz.kd5ujc.storage.interpreters

import java.util.UUID

import cats.data.OptionT
import cats.effect.{Ref, Sync}
import cats.implicits.{toFlatMapOps, toFoldableOps, toFunctorOps, toTraverseOps}
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import xyz.kd5ujc.storage.algebras.{Store, VersionedStore}
import xyz.kd5ujc.storage.interpreters.versioned_store.schema.{Catalog, Diff, Meta}

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.typelevel.log4cats.Logger

package object versioned_store {

  def impl[F[_]: Sync: Logger, Key, Value](
    ref: Ref[F, Catalog[F, Key, Value]]
  ): VersionedStore[F, Key, Value] = new VersionedStore[F, Key, Value] {
    override def get(key: Key): F[Option[Value]] = ref.get.flatMap(_.main.get(key))

    override def getWithFilter(cond: (Key, Value) => Boolean): F[List[(Key, Value)]] =
      ref.get.flatMap(_.main.getWithFilter(cond))

    override def contains(key: Key): F[Boolean] = ref.get.flatMap(_.main.contains(key))

    override def latest: F[UUID] = ref.get.flatMap(_.meta.getOrRaise(Meta.reserved).map(_.id))

    override def listVersions: F[List[UUID]] = ref.get.flatMap {
      _.meta.getOrRaise(Meta.reserved).map(_.history)
    }

    override def getFromVersionDiff(key: Key, id: UUID): F[Option[Value]] =
      (for {
        state   <- OptionT.liftF(ref.get)
        version <- OptionT(state.meta.get(id))
        diffs   <- OptionT.liftF(state.undo.get(version.diffs.toList))
        value <- OptionT.fromOption[F](diffs.collectFirst {
          case (_, Some(diff)) if diff.key == key => diff.previous
        }.flatten)
      } yield value).value

    override def dumpVersionDiff(id: UUID): F[List[(Key, Option[Value])]] =
      ref.get.flatMap { state =>
        for {
          version <- state.meta.getOrRaise(id)
          previousValues <- version.diffs.toList.traverse { lsn =>
            state.undo.getOrRaise(lsn).map(diff => (diff.key, diff.previous))
          }
        } yield previousValues
      }

    override def update(id: UUID, toRemove: List[Key], toUpdate: List[(Key, Value)]): F[Boolean] =
      ref.modify { state =>
        val effect =
          for {
            head <- state.meta.getOrRaise(Meta.reserved)
            headLast = head.diffs.last
            start = 1 + headLast
            end = start + toRemove.size + toUpdate.size
            lsnRange = (start to end).iterator

            // Remove effect with associated LSNs
            _ <- toRemove.traverse_ { key =>
              for {
                prevValueOpt <- state.main.get(key)
                _ <- prevValueOpt match {
                  case Some(prevValue) =>
                    val lsn = lsnRange.next()
                    state.undo.put(lsn, Diff(id, lsn, key, Some(prevValue)))
                  case None =>
                    ().pure[F]
                }
                _ <- state.main.remove(key)
              } yield ()
            }

            // Update effect with associated LSNs
            _ <- toUpdate.traverse_ {
              case (key, value) =>
                for {
                  prevValueOpt <- state.main.get(key)
                  lsn = lsnRange.next()
                  _ <- prevValueOpt match {
                    case Some(prevValue) =>
                      state.undo.put(lsn, Diff(id, lsn, key, Some(prevValue)))
                    case None =>
                      state.undo.put(lsn, Diff(id, lsn, key, None))
                  }
                  _ <- state.main.put(key, value)
                } yield ()
            }

            // Updated meta information based on actual number of LSNs used
            updatedHead = Meta(id, SortedSet.from(start until lsnRange.next()), id :: head.history)
            metaUpdates = List((id, updatedHead), (Meta.reserved, updatedHead))
            _ <- state.meta.putBatch(metaUpdates)
          } yield ()

        (state, effect)
      }.flatten
        .as(true)
        .handleErrorWith { err =>
          Logger[F].warn(s"Versioned store update failed with error: $err") *> false.pure[F]
        }

    override def rollback(id: UUID): F[Boolean] =
      ref.modify { state =>
        val effect = for {
          head <- state.meta.getOrRaise(Meta.reserved)
          dest <- state.meta.getOrRaise(id)
          ancestorIndex = head.history.indexOf(id)
          _ <-
            if (ancestorIndex == -1) {
              new Exception("UUID not found in version history").raiseError
            } else if (ancestorIndex > -1) {
              val toRestore = head.history.slice(0, ancestorIndex)

              // Fold over the UUIDs, collecting all the diffs and applying them to the primary store
              val restoreEffect = for {
                _ <- toRestore.foldM(()) { (_, restoreId) =>
                  for {
                    restorePoint <- state.meta.getOrRaise(restoreId)
                    allDiffs <- restorePoint.diffs.toList.traverse { lsn =>
                      state.undo.getOrRaise(lsn).map(diff => (diff.key, diff.previous))
                    }
                    (diffUpdates, diffRemovals) = allDiffs.foldLeft((List.empty[(Key, Value)], List.empty[Key])) {
                      case ((updates, removals), (key, Some(value))) =>
                        ((key, value) :: updates, removals) // If Value exists, add to updates

                      case ((updates, removals), (key, None)) =>
                        (updates, key :: removals) // If Value is None, record key for removal
                    }

                    _ <- state.main.putBatch(diffUpdates)
                    _ <- state.main.removeBatch(diffRemovals)
                  } yield ()
                }
                _ <- state.meta.put(Meta.reserved, dest)
              } yield ()

              val removeEffect = for {
                versionsToRemove <- state.meta.get(toRestore)
                (listId, listMeta) = versionsToRemove.collect { case (id, Some(t)) => (id, t) }.unzip
                listDiffs = listMeta.flatMap(_.diffs)
                _ <- state.meta.removeBatch(listId)
                _ <- state.undo.removeBatch(listDiffs)
              } yield ()

              // restore then remove since remove deletes diffs applied to this id
              restoreEffect *> removeEffect
            } else {
              new Exception("Unexpected index").raiseError
            }
        } yield ()

        (state, effect)
      }.flatten
        .as(true)
        .handleErrorWith { err =>
          Logger[F].warn(s"Versioned store rollback failed with error: $err") *> false.pure[F]
        }
  }

  object schema {
    final case class Catalog[F[_], Key, Value](
      main: Store[F, Key, Value],
      undo: Store[F, Long, Diff[Key, Value]],
      meta: Store[F, UUID, Meta]
    )

    final case class Diff[Key, Value](id: UUID, lsn: Long, key: Key, previous: Option[Value])

    object Diff {
      implicit def diffEncoder[K: Encoder, V: Encoder]: Encoder[Diff[K, V]] = deriveEncoder
      implicit def diffDecoder[K: Decoder, V: Decoder]: Decoder[Diff[K, V]] = deriveDecoder
    }

    final case class Meta(id: UUID, diffs: SortedSet[Long], history: List[UUID])

    object Meta {
      val reserved: UUID = new UUID(0L, 0L)

      val genesis: Meta = Meta(reserved, SortedSet(0L), List(reserved))

      implicit val metaEncoder: Encoder[Meta] = deriveEncoder
      implicit val metaDecoder: Decoder[Meta] = deriveDecoder
    }
  }
}
