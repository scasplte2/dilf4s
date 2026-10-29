package xyz.kd5ujc.storage

import cats.mtl.Raise
import cats.syntax.all._
import cats.{Monad, Monoid}

import fs2.Stream
import scodec.bits.ByteVector

/** A typed write; later operations on the same key win. */
sealed trait Op[+K, +V] extends Product with Serializable

object Op {
  final case class Put[K, V](key: K, value: V) extends Op[K, V]
  final case class Delete[K](key: K) extends Op[K, Nothing]
}

final case class Batch[K, V](ops: Vector[Op[K, V]]) {
  def ++(that: Batch[K, V]): Batch[K, V] = Batch(ops ++ that.ops)

  /** The final effect per key, in first-touched order. */
  def last: Vector[(K, Option[V])] = {
    val finals = ops.foldLeft(Map.empty[K, Option[V]]) {
      case (acc, Op.Put(k, v)) => acc.updated(k, Some(v))
      case (acc, Op.Delete(k)) => acc.updated(k, None)
    }
    ops.map {
      case Op.Put(k, _) => k
      case Op.Delete(k) => k
    }.distinct.map(k => k -> finals.getOrElse(k, None))
  }
}

object Batch {
  def empty[K, V]: Batch[K, V] = Batch(Vector.empty)
  def put[K, V](key:      K, value: V): Batch[K, V] = Batch(Vector(Op.Put(key, value)))
  def delete[K, V](key:   K): Batch[K, V] = Batch(Vector(Op.Delete(key)))
  def puts[K, V](entries: Iterable[(K, V)]): Batch[K, V] = Batch(entries.toVector.map { case (k, v) => Op.Put(k, v) })
  def deletes[K, V](keys: Iterable[K]): Batch[K, V] = Batch(keys.toVector.map(Op.Delete(_)))

  implicit def batchMonoid[K, V]: Monoid[Batch[K, V]] = Monoid.instance(empty, _ ++ _)
}

/** A key range in the key type's order; `prefix` takes raw encoded bytes for composite or string keys. */
sealed trait KeyRange[+K] extends Product with Serializable

object KeyRange {
  case object All extends KeyRange[Nothing]
  final case class Between[K](from: Option[K], until: Option[K]) extends KeyRange[K]
  final case class EncodedPrefix(prefix: ByteVector) extends KeyRange[Nothing]

  def from[K](k:       K): KeyRange[K] = Between(Some(k), None)
  def until[K](k:      K): KeyRange[K] = Between(None, Some(k))
  def between[K](from: K, until: K): KeyRange[K] = Between(Some(from), Some(until))
}

/** A typed, namespaced key-value store. Every method raises [[StoreError]] through `Raise` rather than throwing. */
trait KeyValueStore[F[_], K, V] {
  def get(key:        K): F[Option[V]]
  def getOrRaise(key: K): F[V]
  def contains(key:   K): F[Boolean]
  def getAll(keys:    List[K]): F[List[(K, Option[V])]]
  def put(key:        K, value: V): F[Unit]
  def remove(key:     K): F[Unit]

  /** Applies the whole batch atomically. */
  def commit(batch: Batch[K, V]): F[Unit]

  /** Ordered scan over one snapshot. */
  def scan(range: KeyRange[K] = KeyRange.All): Stream[F, (K, V)]

  /** The raw writes for a batch, so several stores in one backend can commit together atomically. */
  def encodeBatch(batch: Batch[K, V]): F[RawBatch]
}

object KeyValueStore {

  def apply[F[_]: Monad, K: KeyCodec, V: ValueCodec](raw: RawStore[F], namespace: Namespace)(
    implicit R: Raise[F, StoreError]
  ): KeyValueStore[F, K, V] =
    new KeyValueStore[F, K, V] {
      private val K = KeyCodec[K]
      private val V = ValueCodec[V]
      private val ns = namespace.prefix

      private def rawKey(k: K): ByteVector = ns ++ K.encode(k)

      private def lift[A](e: Either[StoreError, A]): F[A] = e.fold(R.raise[StoreError, A], _.pure[F])

      private def decodeValue(bytes: ByteVector): F[V] = lift(V.decode(bytes))

      def get(key: K): F[Option[V]] = raw.get(rawKey(key)).flatMap(_.traverse(decodeValue))

      def getOrRaise(key: K): F[V] =
        get(key).flatMap(_.fold(R.raise[StoreError, V](StoreError.NotFound(K.encode(key).toHex)))(_.pure[F]))

      def contains(key: K): F[Boolean] = raw.get(rawKey(key)).map(_.isDefined)

      def getAll(keys: List[K]): F[List[(K, Option[V])]] =
        raw.getAll(keys.map(rawKey)).flatMap(values => keys.zip(values).traverse { case (k, v) => v.traverse(decodeValue).map(k -> _) })

      def put(key: K, value: V): F[Unit] = commit(Batch.put(key, value))

      def remove(key: K): F[Unit] = commit(Batch.delete(key))

      def commit(batch: Batch[K, V]): F[Unit] = encodeBatch(batch).flatMap(raw.write)

      def encodeBatch(batch: Batch[K, V]): F[RawBatch] =
        batch.ops.traverse {
          case Op.Put(k, v) => lift(V.encode(v)).map(bytes => RawOp.Put(rawKey(k), bytes): RawOp)
          case Op.Delete(k) => (RawOp.Delete(rawKey(k)): RawOp).pure[F]
        }
          .map(RawBatch(_))

      def scan(range: KeyRange[K]): Stream[F, (K, V)] = {
        val bytes = range match {
          case KeyRange.All              => ByteRange.prefix(ns)
          case KeyRange.EncodedPrefix(p) => ByteRange.prefix(ns ++ p)
          case KeyRange.Between(f, u) =>
            ByteRange(Some(f.fold(ns)(rawKey)), u.fold(ByteRange.successor(ns))(k => Some(rawKey(k))))
        }
        raw.scan(bytes).evalMap { case (k, v) => (lift(K.decode(k.drop(ns.size))), decodeValue(v)).tupled }
      }
    }
}
