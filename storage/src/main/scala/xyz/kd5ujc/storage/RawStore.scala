package xyz.kd5ujc.storage

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.{Ref, Sync}
import cats.syntax.all._
import cats.{Applicative, Monoid}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.storage.ByteOrder.unsigned

import fs2.Stream
import scodec.bits.ByteVector

/** One write in a [[RawBatch]]. */
sealed trait RawOp extends Product with Serializable

object RawOp {
  final case class Put(key: ByteVector, value: ByteVector) extends RawOp
  final case class Delete(key: ByteVector) extends RawOp
}

/** Writes applied atomically and in order: all or none, and a later op on the same key wins. */
final case class RawBatch(ops: Vector[RawOp]) {
  def ++(that: RawBatch): RawBatch = RawBatch(ops ++ that.ops)
  def isEmpty: Boolean = ops.isEmpty
}

object RawBatch {
  val empty: RawBatch = RawBatch(Vector.empty)
  def put(key:    ByteVector, value: ByteVector): RawBatch = RawBatch(Vector(RawOp.Put(key, value)))
  def delete(key: ByteVector): RawBatch = RawBatch(Vector(RawOp.Delete(key)))

  implicit val rawBatchMonoid: Monoid[RawBatch] = Monoid.instance(empty, _ ++ _)
}

/** Half-open byte range `[from, until)` in unsigned order; `None` is unbounded. */
final case class ByteRange(from: Option[ByteVector], until: Option[ByteVector]) {
  def contains(key: ByteVector): Boolean =
    from.forall(f => unsigned.gteq(key, f)) && until.forall(u => unsigned.lt(key, u))
}

object ByteRange {

  val all: ByteRange = ByteRange(None, None)

  /** Every key starting with `prefix`. */
  def prefix(prefix: ByteVector): ByteRange = ByteRange(Some(prefix), successor(prefix))

  /** The smallest byte string greater than every string with this prefix; `None` if the prefix is all `0xff`. */
  def successor(prefix: ByteVector): Option[ByteVector] = {
    @tailrec
    def go(p: ByteVector): Option[ByteVector] =
      p.lastOption match {
        case None                        => None
        case Some(b) if b == 0xff.toByte => go(p.dropRight(1))
        case Some(b)                     => Some(p.dropRight(1) :+ (b + 1).toByte)
      }
    go(prefix)
  }
}

/** A length-prefixed key prefix: no namespace is a prefix of another, so namespaces never overlap. */
sealed abstract case class Namespace(prefix: ByteVector)

object Namespace {

  def apply(name: String): Either[StoreError, Namespace] = {
    val bytes = ByteVector(name.getBytes(UTF_8))
    Either.cond(
      bytes.nonEmpty && bytes.size <= 255,
      new Namespace(ByteVector(bytes.size.toInt) ++ bytes) {},
      StoreError.Codec("namespace", s"'$name' must be 1..255 UTF-8 bytes")
    )
  }
}

/** The byte-level store every backend implements. Batches are atomic; scans are ordered and see one snapshot. */
trait RawStore[F[_]] {
  def get(key:     ByteVector): F[Option[ByteVector]]
  def getAll(keys: List[ByteVector]): F[List[Option[ByteVector]]]
  def write(batch: RawBatch): F[Unit]

  /**
   * Compare-and-write: applies `batch` atomically **only if** every guarded key currently holds exactly the expected
   * value (`None` = absent). Returns whether it wrote. This is what makes optimistic concurrency safe across any number
   * of store instances sharing a backend.
   */
  def writeIf(guards: List[(ByteVector, Option[ByteVector])], batch: RawBatch): F[Boolean]

  def scan(range: ByteRange): Stream[F, (ByteVector, ByteVector)]
}

object RawStore {

  /** In-memory interpreter over an immutable sorted map; `write` is a single atomic `Ref` update. */
  def inMemory[F[_]: Sync]: F[RawStore[F]] =
    Ref.of[F, TreeMap[ByteVector, ByteVector]](TreeMap.empty).map(fromRef[F])

  private def fromRef[F[_]: Applicative](ref: Ref[F, TreeMap[ByteVector, ByteVector]]): RawStore[F] =
    new RawStore[F] {
      def get(key: ByteVector): F[Option[ByteVector]] = ref.get.map(_.get(key))

      def getAll(keys: List[ByteVector]): F[List[Option[ByteVector]]] = ref.get.map(m => keys.map(m.get))

      def write(batch: RawBatch): F[Unit] = ref.update(apply(_, batch))

      def writeIf(guards: List[(ByteVector, Option[ByteVector])], batch: RawBatch): F[Boolean] =
        ref.modify { m =>
          if (guards.forall { case (k, expected) => m.get(k) == expected }) (apply(m, batch), true) else (m, false)
        }

      def scan(range: ByteRange): Stream[F, (ByteVector, ByteVector)] =
        Stream.eval(ref.get).flatMap { m =>
          val lower = range.from.fold(m)(m.rangeFrom)
          val bounded = range.until.fold(lower)(lower.rangeUntil)
          Stream.emits(bounded.toSeq)
        }
    }

  private[storage] def apply(m: TreeMap[ByteVector, ByteVector], batch: RawBatch): TreeMap[ByteVector, ByteVector] =
    batch.ops.foldLeft(m) {
      case (acc, RawOp.Put(k, v)) => acc.updated(k, v)
      case (acc, RawOp.Delete(k)) => acc.removed(k)
    }
}
