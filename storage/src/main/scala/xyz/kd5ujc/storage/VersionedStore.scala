package xyz.kd5ujc.storage

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.Concurrent
import cats.mtl.Raise
import cats.syntax.all._
import cats.{Order, Show}

import xyz.kd5ujc.core.canonical.CanonicalValue.{CBytes, CMap}
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

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

  private[storage] def unsafe(value: Long): Version = new Version(value) {}

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
}

object VersionedStore {

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

    /** Writes only if both version records are unchanged; otherwise reports the conflict against the new latest. */
    private def guarded(records: (Option[ByteVector], Option[ByteVector]), expected: Long, batch: RawBatch): F[Unit] =
      raw.writeIf(List(latestKey -> records._1, floorKey -> records._2), batch).flatMap { wrote =>
        if (wrote) ().pure[F] else latest.flatMap(now => fail(StoreError.VersionConflict(expected, now.value)))
      }

    // ---- records: canonical CBOR maps of optional raw values; an absent field means "no value"

    private def record(before: Option[ByteVector], after: Option[ByteVector]): F[ByteVector] =
      liftE(
        CanonicalValue
          .map(before.map(b => "b" -> CBytes(b)).toList ++ after.map(a => "a" -> CBytes(a)).toList)
          .bimap(e => StoreError.Corrupt(s"record: $e"), Cbor.encode)
      )

    private def field(m: Map[String, CanonicalValue], name: String): F[Option[ByteVector]] =
      m.get(name) match {
        case None            => none[ByteVector].pure[F]
        case Some(CBytes(b)) => b.some.pure[F]
        case Some(other)     => fail(StoreError.Corrupt(s"record field '$name' is not bytes: $other"))
      }

    private def readRecord(bytes: ByteVector): F[(Option[ByteVector], Option[ByteVector])] =
      Cbor.decode(bytes) match {
        case Right(CMap(m)) => (field(m, "b"), field(m, "a")).tupled
        case Right(other)   => fail(StoreError.Corrupt(s"record is not a map: $other"))
        case Left(e)        => fail(StoreError.Corrupt(s"record does not decode: $e"))
      }

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

    def changes(version: Version): F[List[Change[K, V]]] =
      checkReadable(version) >>
      (if (version.value == 0L) List.empty[Change[K, V]].pure[F]
       else
         logEntries(version.value, version.value + 1).flatMap(_.traverse {
           case (_, ek, r) =>
             (liftE(K.decode(ek)), readRecord(r).flatMap { case (b, a) => (decodeValue(b), decodeValue(a)).tupled }).mapN {
               case (k, (b, a)) => Change(k, b, a)
             }
         }))

    // ---- writes: each is one atomic RawBatch, serialised by the mutex

    def commit(expected: Version, batch: Batch[K, V]): F[Version] =
      versionRecords.flatMap { records =>
        parseVersion(records._1).flatMap { current =>
          if (current != expected.value) fail(StoreError.VersionConflict(expected.value, current))
          else {
            val next = expected.next
            val finals = batch.last.map { case (k, v) => (K.encode(k), v) }
            for {
              befores <- raw.getAll(finals.toList.map { case (ek, _) => dataKey(ek) })
              afters  <- finals.traverse { case (_, v) => v.traverse(x => liftE(V.encode(x))) }
              writes <- finals.zip(befores).zip(afters).foldLeftM(RawBatch.empty) {
                case (acc, (((ek, _), before), after)) =>
                  (record(before, after), record(before, None)).mapN { (logRecord, histRecord) =>
                    acc ++
                    after.fold(RawBatch.delete(dataKey(ek)))(a => RawBatch.put(dataKey(ek), a)) ++
                    RawBatch.put(logKey(next.value, ek), logRecord) ++
                    RawBatch.put(histPrefix(ek) ++ ver(next.value), histRecord)
                  }
              }
              _ <- guarded(records, expected.value, writes ++ RawBatch.put(latestKey, ver(next.value)))
            } yield next
          }
        }
      }

    def rollback(to: Version): F[Unit] =
      versionRecords.flatMap { records =>
        checkReadable(to) >> parseVersion(records._1).flatMap { current =>
          logEntries(to.value + 1, current + 1).flatMap { entries =>
            entries.traverse { case (v, ek, r) => readRecord(r).map { case (before, _) => (v, ek, before) } }.flatMap { decoded =>
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
              guarded(records, current, writes ++ RawBatch.put(latestKey, ver(to.value)))
            }
          }
        }
      }

    def prune(through: Version): F[Unit] =
      versionRecords.flatMap { records =>
        checkReadable(through) >> (parseVersion(records._1), parseVersion(records._2)).tupled.flatMap {
          case (current, f) =>
            logEntries(f + 1, through.value + 1).flatMap { entries =>
              val writes = entries.foldLeft(RawBatch.empty) {
                case (acc, (v, ek, _)) =>
                  acc ++ RawBatch.delete(logKey(v, ek)) ++ RawBatch.delete(histPrefix(ek) ++ ver(v))
              }
              guarded(records, current, writes ++ RawBatch.put(floorKey, ver(through.value)))
            }
        }
      }
  }
}
