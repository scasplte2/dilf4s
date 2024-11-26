package xyz.kd5ujc.storage.store

import cats.effect.{Ref, Sync}
import cats.implicits.toFunctorOps
import xyz.kd5ujc.storage.Store

object RefMapStore {

  def make[F[_]: Sync, Key, Value]: F[Store[F, Key, Value]] =
    Ref.of[F, Map[Key, Value]](Map.empty).map { store =>
      new Store[F, Key, Value] {

        override def get(id: Key): F[Option[Value]] = store.get.map(_.get(id))

        override def getBatch(ids: List[Key]): F[List[(Key, Option[Value])]] =
          store.get.map(m => ids.map(id => (id, m.get(id))))

        override def contains(id: Key): F[Boolean] = store.get.map(_.contains(id))

        override def getWithFilter(cond: (Key, Value) => Boolean): F[List[(Key, Value)]] =
          store.get.map(_.toList.filter(cond.tupled))

        override def put(id: Key, t: Value): F[Unit] = store.update(_.updated(id, t))

        override def remove(id: Key): F[Unit] = store.update(_ - id)

        override def putBatch(updates: List[(Key, Value)]): F[Unit] =
          store.update(m => updates.foldLeft(m)((m, kv) => m.updated(kv._1, kv._2)))

        override def removeBatch(deletions: List[Key]): F[Unit] =
          store.update(m => deletions.foldLeft(m)(_ - _))
      }
    }
}
