package xyz.kd5ujc.storage

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.Concurrent
import cats.mtl.Raise
import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.models.storage.VersionedRecord

import fs2.Stream
import scodec.bits.ByteVector

/** A store version: a sequence number, `0` before the first commit. */
sealed abstract case class Version(value: Long) {
  def next: Version = Version.unsafe(value + 1)
}

object Version {
  val Genesis: Version = unsafe(0L)

  def apply(value: Long): Either[StoreError, Version] =
    Either.cond(value >= 0, unsafe(value), StoreError.UnknownVersion(value, -1))

  private[kd5ujc] def unsafe(value: Long): Version = new Version(value) {}

  implicit val versionOrder: Order[Version] = Order.by(_.value)
  implicit val versionShow: Show[Version] = Show.show(v => s"v${v.value}")
}

/** The effect of one commit on one key. */
final case class Change[K, V](key: K, before: Option[V], after: Option[V])

/**
 * A key-value store with linear history. Commits are optimistic (`expected` must be the latest version) and atomic;
 * any retained version can be read with [[getAt]] or restored with [[rollback]]; [[prune]] discards history. Every
 * mutation is a compare-and-write guarded on the stored version records, so concurrent writers, even through separate
 * instances over one backend, can never both succeed against the same version.
 *
 * Each mutation is also available staged: computed without writing, as the guards and batch of its compare-and-write
 * (docs/specs/storage.md §3). Several stores in one backend combine their stages into one atomic `writeIf`, and a
 * staged commit is a draft that later stages can build on.
 */
trait VersionedStore[F[_], K, V] {
  def latest: F[Version]

  /** The oldest version still readable: history at or below it has been pruned. */
  def floor: F[Version]

  def get(key:         K): F[Option[V]]
  def getAt(key:       K, version:     Version): F[Option[V]]
  def scan(range:      KeyRange[K] = KeyRange.All): Stream[F, (K, V)]
  def commit(expected: Version, batch: Batch[K, V]): F[Version]
  def changes(version: Version): F[List[Change[K, V]]]
  def rollback(to:     Version): F[Unit]
  def prune(through:   Version): F[Unit]

  /** The commit `commit(expected, batch)` would make, without writing. */
  def stage(expected: Version, batch: Batch[K, V]): F[VersionedStore.Staged]

  /** A commit on top of a staged one that is not yet written; it reads through the draft, and writing it writes both. */
  def stageOn(base: VersionedStore.Staged, batch: Batch[K, V]): F[VersionedStore.Staged]

  def stageRollback(to:   Version): F[VersionedStore.Staged]
  def stagePrune(through: Version): F[VersionedStore.Staged]

  /** Applies a stage as one compare-and-write; a `VersionConflict` if the store moved since it was staged. */
  def write(staged: VersionedStore.Staged): F[Version]
}

object VersionedStore {

  /**
   * A mutation computed but not written: `writeIf(guards, batch)` applies it. `version` is the latest version after it.
   * A stage built on a draft carries the draft's guards and writes, so it applies both or neither.
   */
  final case class Staged private[storage] (
    guards:                        List[(ByteVector, Option[ByteVector])],
    batch:                         RawBatch,
    version:                       Version,
    private[storage] val expected: Long,
    private[storage] val floor:    Long,
    private[storage] val data:     Map[ByteVector, Option[ByteVector]]
  )

  /**
   * Layout inside one [[RawStore]], so every operation is a single atomic batch:
   *   - `data`: key -> latest value
   *   - `log`:  version ++ key -> {b: before, a: after} (one entry per key changed in that version)
   *   - `hist`: terminated(key) ++ version -> {b: before} (seekable per key for [[getAt]])
   *   - `meta`: "latest", "floor" -> version
   */
  def make[F[_]: Concurrent, K: KeyCodec, V: ValueCodec](raw: RawStore[F], name: String)(
    implicit R: Raise[F, StoreError]
  ): F[VersionedStore[F, K, V]] =
    (
      lift(Namespace(s"$name/data")),
      lift(Namespace(s"$name/log")),
      lift(Namespace(s"$name/hist")),
      lift(Namespace(s"$name/meta"))
    ).mapN { (dataNs, logNs, histNs, metaNs) =>
      new Impl[F, K, V](raw, dataNs.prefix, logNs.prefix, histNs.prefix, metaNs.prefix)
    }

  private def lift[F[_]: Concurrent, A](e: Either[StoreError, A])(implicit R: Raise[F, StoreError]): F[A] =
    e.fold(R.raise[StoreError, A], _.pure[F])

  final private class Impl[F[_]: Concurrent, K: KeyCodec, V: ValueCodec](
    raw:    RawStore[F],
    dataNs: ByteVector,
    logNs:  ByteVector,
    histNs: ByteVector,
    metaNs: ByteVector
  )(implicit R: Raise[F, StoreError])
      extends VersionedStore[F, K, V] {

    private val K = KeyCodec[K]
    private val V = ValueCodec[V]
    private val latestKey = metaNs ++ ByteVector("latest".getBytes(UTF_8))
    private val floorKey = metaNs ++ ByteVector("floor".getBytes(UTF_8))

    private def ver(v:         Long): ByteVector = ByteVector.fromLong(v)
    private def dataKey(ek:    ByteVector): ByteVector = dataNs ++ ek
    private def logKey(v:      Long, ek: ByteVector): ByteVector = logNs ++ ver(v) ++ ek
    private def histPrefix(ek: ByteVector): ByteVector = histNs ++ KeyCodec.Terminated.encode(ek)

    private def fail[A](e:  StoreError): F[A] = R.raise[StoreError, A](e)
    private def liftE[A](e: Either[StoreError, A]): F[A] = e.fold(fail[A], _.pure[F])

    private def parseVersion(raw: Option[ByteVector]): F[Long] =
      raw match {
        case None                    => 0L.pure[F]
        case Some(b) if b.size == 8L => b.toLong(signed = false).pure[F]
        case Some(b)                 => fail(StoreError.Corrupt(s"version record has ${b.size} bytes"))
      }

    /** The raw version records, kept to guard the compare-and-write. */
    private def versionRecords: F[(Option[ByteVector], Option[ByteVector])] =
      raw.getAll(List(latestKey, floorKey)).flatMap {
        case List(l, f) => (l, f).pure[F]
        case other      => fail(StoreError.Corrupt(s"expected 2 version records, got ${other.size}"))
      }

    def latest: F[Version] = raw.get(latestKey).flatMap(parseVersion).map(Version.unsafe)
    def floor: F[Version] = raw.get(floorKey).flatMap(parseVersion).map(Version.unsafe)

    private def guardsOf(records: (Option[ByteVector], Option[ByteVector])): List[(ByteVector, Option[ByteVector])] =
      List(latestKey -> records._1, floorKey -> records._2)

    // ---- records: shared/schemas/storage.cddl `versioned-record`; an absent field means "no value"

    private def record(before: Option[ByteVector], after: Option[ByteVector]): F[ByteVector] =
      liftE(VersionedRecord.codec.encode(VersionedRecord(before, after)).bimap(e => StoreError.Corrupt(s"record: $e"), Cbor.encode))

    private def readRecord(bytes: ByteVector): F[(Option[ByteVector], Option[ByteVector])] =
      liftE(
        Cbor
          .decode(bytes)
          .leftMap(_.toString)
          .flatMap(VersionedRecord.codec.decoder.decode(_).leftMap(_.message))
          .bimap(e => StoreError.Corrupt(s"record: $e"), r => (r.b, r.a))
      )

    private def decodeValue(b: Option[ByteVector]): F[Option[V]] = b.traverse(bytes => liftE(V.decode(bytes)))

    // ---- reads

    private def checkReadable(v: Version): F[Unit] =
      (latest, floor).tupled.flatMap {
        case (l, f) =>
          if (v.value > l.value) fail(StoreError.UnknownVersion(v.value, l.value))
          else if (v.value < f.value) fail(StoreError.Pruned(v.value, f.value))
          else ().pure[F]
      }

    def get(key: K): F[Option[V]] = raw.get(dataKey(K.encode(key))).flatMap(decodeValue)

    def getAt(key: K, version: Version): F[Option[V]] = {
      val ek = K.encode(key)
      val prefix = histPrefix(ek)
      checkReadable(version) >>
      raw
        .scan(ByteRange(Some(prefix ++ ver(version.value + 1)), ByteRange.successor(prefix)))
        .head
        .compile
        .last
        .flatMap {
          case None         => get(key)
          case Some((_, r)) => readRecord(r).flatMap { case (before, _) => decodeValue(before) }
        }
    }

    def scan(range: KeyRange[K]): Stream[F, (K, V)] = {
      val bytes = range match {
        case KeyRange.All              => ByteRange.prefix(dataNs)
        case KeyRange.EncodedPrefix(p) => ByteRange.prefix(dataNs ++ p)
        case KeyRange.Between(f, u) =>
          ByteRange(Some(f.fold(dataNs)(k => dataKey(K.encode(k)))), u.fold(ByteRange.successor(dataNs))(k => Some(dataKey(K.encode(k)))))
      }
      raw.scan(bytes).evalMap { case (k, v) => (liftE(K.decode(k.drop(dataNs.size))), liftE(V.decode(v))).tupled }
    }

    /** Log entries for versions in `[fromV, untilV)`, ascending by version then key. */
    private def logEntries(fromV: Long, untilV: Long): F[List[(Long, ByteVector, ByteVector)]] =
      raw
        .scan(ByteRange(Some(logNs ++ ver(fromV)), Some(logNs ++ ver(untilV))))
        .map {
          case (k, r) =>
            val body = k.drop(logNs.size)
            (body.take(8).toLong(signed = false), body.drop(8), r)
        }
        .compile
        .toList

    /** The log of the floor version went with the history below it: the change into it is pruned too. */
    def changes(version: Version): F[List[Change[K, V]]] =
      checkReadable(version) >> floor.flatMap(f =>
        if (f.value > 0L && version.value == f.value) fail[Unit](StoreError.Pruned(version.value, f.value)) else ().pure[F]
      ) >>
      (if (version.value == 0L) List.empty[Change[K, V]].pure[F]
       else
         logEntries(version.value, version.value + 1).flatMap(_.traverse {
           case (_, ek, r) =>
             (liftE(K.decode(ek)), readRecord(r).flatMap { case (b, a) => (decodeValue(b), decodeValue(a)).tupled }).mapN {
               case (k, (b, a)) => Change(k, b, a)
             }
         }))

    // ---- writes: each is one staged compare-and-write

    def commit(expected: Version, batch: Batch[K, V]): F[Version] = stage(expected, batch).flatMap(write)
    def rollback(to:     Version): F[Unit] = stageRollback(to).flatMap(write).void
    def prune(through:   Version): F[Unit] = stagePrune(through).flatMap(write).void

    def write(staged: Staged): F[Version] =
      raw.writeIf(staged.guards, staged.batch).flatMap { wrote =>
        if (wrote) staged.version.pure[F] else latest.flatMap(now => fail(StoreError.VersionConflict(staged.expected, now.value)))
      }

    def stage(expected: Version, batch: Batch[K, V]): F[Staged] =
      versionRecords.flatMap { records =>
        (parseVersion(records._1), parseVersion(records._2)).tupled.flatMap {
          case (current, f) =>
            if (current != expected.value) fail(StoreError.VersionConflict(expected.value, current))
            else stageCommit(guardsOf(records), RawBatch.empty, current, current, f, Map.empty, batch)
        }
      }

    def stageOn(base: Staged, batch: Batch[K, V]): F[Staged] =
      stageCommit(base.guards, base.batch, base.expected, base.version.value, base.floor, base.data, batch)

    /** One commit after `current`, reading current values through `overlay` (the writes of the drafts below it). */
    private def stageCommit(
      guards:   List[(ByteVector, Option[ByteVector])],
      below:    RawBatch,
      expected: Long,
      current:  Long,
      f:        Long,
      overlay:  Map[ByteVector, Option[ByteVector]],
      batch:    Batch[K, V]
    ): F[Staged] = {
      val next = current + 1
      val finals = batch.last.map { case (k, v) => (K.encode(k), v) }
      val unread = finals.toList.map(_._1).filterNot(overlay.contains)
      for {
        stored <- raw.getAll(unread.map(dataKey)).map(unread.zip(_).toMap)
        befores = finals.map { case (ek, _) => overlay.getOrElse(ek, stored.getOrElse(ek, None)) }
        afters <- finals.traverse { case (_, v) => v.traverse(x => liftE(V.encode(x))) }
        writes <- finals.zip(befores).zip(afters).foldLeftM(RawBatch.empty) {
          case (acc, (((ek, _), before), after)) =>
            (record(before, after), record(before, None)).mapN { (logRecord, histRecord) =>
              acc ++
              after.fold(RawBatch.delete(dataKey(ek)))(a => RawBatch.put(dataKey(ek), a)) ++
              RawBatch.put(logKey(next, ek), logRecord) ++
              RawBatch.put(histPrefix(ek) ++ ver(next), histRecord)
            }
        }
        written = finals.map(_._1).zip(afters).toMap
      } yield Staged(guards, below ++ writes ++ RawBatch.put(latestKey, ver(next)), Version.unsafe(next), expected, f, overlay ++ written)
    }

    def stageRollback(to: Version): F[Staged] =
      versionRecords.flatMap { records =>
        checkReadable(to) >> (parseVersion(records._1), parseVersion(records._2)).tupled.flatMap {
          case (current, f) =>
            logEntries(to.value + 1, current + 1).flatMap { entries =>
              entries.traverse { case (v, ek, r) => readRecord(r).map { case (before, _) => (v, ek, before) } }.map { decoded =>
                // the earliest change after `to` holds each key's value as of `to`
                val restore = decoded.foldLeft(Map.empty[ByteVector, Option[ByteVector]]) {
                  case (acc, (_, ek, before)) =>
                    if (acc.contains(ek)) acc else acc.updated(ek, before)
                }
                val writes =
                  restore.foldLeft(RawBatch.empty) {
                    case (acc, (ek, before)) =>
                      acc ++ before.fold(RawBatch.delete(dataKey(ek)))(b => RawBatch.put(dataKey(ek), b))
                  } ++
                  decoded.foldLeft(RawBatch.empty) {
                    case (acc, (v, ek, _)) =>
                      acc ++ RawBatch.delete(logKey(v, ek)) ++ RawBatch.delete(histPrefix(ek) ++ ver(v))
                  }
                Staged(guardsOf(records), writes ++ RawBatch.put(latestKey, ver(to.value)), to, current, f, restore)
              }
            }
        }
      }

    def stagePrune(through: Version): F[Staged] =
      versionRecords.flatMap { records =>
        checkReadable(through) >> (parseVersion(records._1), parseVersion(records._2)).tupled.flatMap {
          case (current, f) =>
            logEntries(f + 1, through.value + 1).map { entries =>
              val writes = entries.foldLeft(RawBatch.empty) {
                case (acc, (v, ek, _)) =>
                  acc ++ RawBatch.delete(logKey(v, ek)) ++ RawBatch.delete(histPrefix(ek) ++ ver(v))
              }
              Staged(
                guardsOf(records),
                writes ++ RawBatch.put(floorKey, ver(through.value)),
                Version.unsafe(current),
                current,
                through.value,
                Map.empty
              )
            }
        }
      }
  }
}
