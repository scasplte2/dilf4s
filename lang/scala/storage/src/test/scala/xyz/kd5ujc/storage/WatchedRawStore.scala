package xyz.kd5ujc.storage

import cats.effect.{IO, Ref}
import cats.syntax.all._

import fs2.Stream
import scodec.bits.ByteVector

/**
 * A raw store that counts the writes reaching its backend, and can be made to fail them as a crash would: the batch
 * never lands. Staged mutations must reach the backend as exactly one compare-and-write.
 */
final class WatchedRawStore private (
  underlying: RawStore[IO],
  counts:     Ref[IO, (Int, Int)],
  crashing:   Ref[IO, Boolean],
  hook:       Ref[IO, Option[(ByteVector, IO[Unit])]],
  getHook:    Ref[IO, Option[(ByteVector, IO[Unit])]]
) extends RawStore[IO] {

  /** `(write, writeIf)` calls so far, crashed ones included. */
  def writes: IO[(Int, Int)] = counts.get
  def crashNextWrites(on: Boolean): IO[Unit] = crashing.set(on)

  private def crashOr[A](fa: IO[A]): IO[A] =
    crashing.get.flatMap(c => if (c) IO.raiseError(new RuntimeException("crash before the batch landed")) else fa)

  /** Runs `action` once, just before the next point read of a key starting with `prefix`. */
  def beforeNextGetOf(prefix: ByteVector, action: IO[Unit]): IO[Unit] = getHook.set(Some(prefix -> action))

  private def beforeGet(keys: List[ByteVector]): IO[Unit] =
    getHook.modify {
      case Some((p, action)) if keys.exists(_.startsWith(p)) => (None, action)
      case other                                             => (other, IO.unit)
    }.flatten

  def get(key:     ByteVector): IO[Option[ByteVector]] = beforeGet(List(key)) >> underlying.get(key)
  def getAll(keys: List[ByteVector]): IO[List[Option[ByteVector]]] = beforeGet(keys) >> underlying.getAll(keys)

  /**
   * Runs `action` once, just before the next scan whose lower bound starts with `prefix`: a deterministic way to land
   * another writer's change between two reads of an operation under test.
   */
  def beforeNextScanOf(prefix: ByteVector, action: IO[Unit]): IO[Unit] = hook.set(Some(prefix -> action))

  def scan(range: ByteRange): Stream[IO, (ByteVector, ByteVector)] =
    Stream
      .eval(hook.modify {
        case Some((p, action)) if range.from.exists(_.startsWith(p)) => (None, action)
        case other                                                   => (other, IO.unit)
      })
      .flatMap(Stream.eval(_)) >> underlying.scan(range)

  def write(batch: RawBatch): IO[Unit] = counts.update { case (w, c) => (w + 1, c) } >> crashOr(underlying.write(batch))

  def writeIf(guards: List[(ByteVector, Option[ByteVector])], batch: RawBatch): IO[Boolean] =
    counts.update { case (w, c) => (w, c + 1) } >> crashOr(underlying.writeIf(guards, batch))

  /** Every key and value, to compare whole-store states. */
  def contents: IO[List[(ByteVector, ByteVector)]] = underlying.scan(ByteRange.all).compile.toList
}

object WatchedRawStore {
  def apply(underlying: RawStore[IO]): IO[WatchedRawStore] =
    (
      Ref.of[IO, (Int, Int)]((0, 0)),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Option[(ByteVector, IO[Unit])]](None),
      Ref.of[IO, Option[(ByteVector, IO[Unit])]](None)
    ).mapN(new WatchedRawStore(underlying, _, _, _, _))
}
