package xyz.kd5ujc.storage

/** Typed storage failures, raised through `cats.mtl.Raise[F, StoreError]`. */
sealed trait StoreError extends Product with Serializable

object StoreError {

  /** A key or value failed to encode or decode. */
  final case class Codec(context: String, reason: String) extends StoreError

  final case class NotFound(key: String) extends StoreError

  /** Optimistic concurrency: the caller committed against a version that is no longer the latest. */
  final case class VersionConflict(expected: Long, actual: Long) extends StoreError

  final case class UnknownVersion(requested: Long, latest: Long) extends StoreError

  /** The requested version is older than the pruning floor, so its history is gone. */
  final case class Pruned(requested: Long, floor: Long) extends StoreError

  /** The backend reported a failure (I/O, corruption, lock contention). */
  final case class Backend(operation: String, reason: String) extends StoreError

  /** Stored bytes violate the layout this library wrote. */
  final case class Corrupt(reason: String) extends StoreError
}
