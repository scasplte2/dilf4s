package xyz.kd5ujc.level_db

import cats.data.OptionT
import cats.effect.{Async, Ref, Resource}
import cats.implicits._
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.{Decoder, Encoder}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.{Store, VersionedStore}

import java.nio.file.Path
import java.util.UUID
import scala.collection.immutable.SortedSet

/**
 * A *VersionedStore* implementation that is adopted from a similar imperative approach in Ergo
 * https://github.com/ergoplatform/ergo/blob/master/avldb/src/main/scala/scorex/db/LDBVersionedStore.scala
 */
object VersionedLevelDbStore {

  def make[F[_]: Async: JsonSerializer, Key: Encoder: Decoder, Value: Encoder: Decoder](
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
        Ref.of[F, State[F, Key, Value]](State(main, undo, meta)).flatTap { ref =>
          ref.get.flatMap { state =>
            state.meta.get(Meta.reserved).flatMap {
              case Some(_) => Async[F].unit
              case None    => state.meta.put(Meta.reserved, Meta.genesis)
            }
          }
        }
      )
      store = VersionedLevelDbStore.make[F, Key, Value](ref)
    } yield store
  }

  def make[F[_]: Async, Key, Value](
    stateRef: Ref[F, State[F, Key, Value]]
  ): VersionedStore[F, Key, Value] = new VersionedStore[F, Key, Value] {

    override def get(key: Key): F[Option[Value]] = stateRef.get.flatMap(_.main.get(key))

    override def getWithFilter(cond: (Key, Value) => Boolean): F[List[(Key, Value)]] =
      stateRef.get.flatMap(_.main.getWithFilter(cond))

    override def contains(key: Key): F[Boolean] = stateRef.get.flatMap(_.main.contains(key))

    override def latest: F[UUID] = stateRef.get.flatMap(_.meta.getOrRaise(Meta.reserved).map(_.id))

    override def listVersions: F[List[UUID]] = stateRef.get.flatMap {
      _.meta.getOrRaise(Meta.reserved).map(_.history)
    }

    override def getFromVersionDiff(key: Key, id: UUID): F[Option[Value]] =
      (for {
        state   <- OptionT.liftF(stateRef.get)
        version <- OptionT(state.meta.get(id))
        diffs   <- OptionT.liftF(state.undo.get(version.diffs.toList))
        value <- OptionT.fromOption[F](diffs.collectFirst {
          case (_, Some(diff)) if diff.key == key => diff.previous
        }.flatten)
      } yield value).value

    override def dumpVersionDiff(id: UUID): F[List[(Key, Option[Value])]] =
      stateRef.get.flatMap { state =>
        for {
          version <- state.meta.getOrRaise(id)
          previousValues <- version.diffs.toList.traverse { lsn =>
            state.undo.getOrRaise(lsn).map(diff => (diff.key, diff.previous))
          }
        } yield previousValues
      }

    override def update(id: UUID, toRemove: List[Key], toUpdate: List[(Key, Value)]): F[Boolean] =
      stateRef.modify { state =>
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
                    Async[F].unit
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
        .handleErrorWith(_ => Async[F].pure(false))

    override def rollback(id: UUID): F[Boolean] =
      stateRef.modify { state =>
        val effect = for {
          head <- state.meta.getOrRaise(Meta.reserved)
          dest <- state.meta.getOrRaise(id)
          ancestorIndex = head.history.indexOf(id)
          _ <-
            if (ancestorIndex == -1) {
              Async[F].raiseError(new Exception("UUID not found in version history"))
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
              Async[F].raiseError(new Exception("Unexpected index"))
            }
        } yield ()

        (state, effect)
      }.flatten
        .as(true)
        .handleErrorWith(_ => Async[F].pure(false))
  }

  final case class State[F[_], Key, Value](
    main: Store[F, Key, Value],
    undo: Store[F, Long, Diff[Key, Value]],
    meta: Store[F, UUID, Meta]
  )

  @derive(decoder, encoder)
  final case class Diff[Key, Value](id: UUID, lsn: Long, key: Key, previous: Option[Value])

  @derive(decoder, encoder)
  final case class Meta(id: UUID, diffs: SortedSet[Long], history: List[UUID])

  object Meta {
    val reserved: UUID = new UUID(0L, 0L)

    val genesis: Meta = Meta(reserved, SortedSet(0L), List(reserved))
  }
}
