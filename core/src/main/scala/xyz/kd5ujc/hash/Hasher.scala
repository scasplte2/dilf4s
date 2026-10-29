package xyz.kd5ujc.hash

/**
 * Base trait for hash algorithm implementations that work with raw bytes
 */
trait Hasher[F[_]] {

  /**
   * Hash bytes with an optional prefix
   *
   * @param bytes The raw bytes to hash
   * @param prefix Optional prefix bytes to prepend
   * @return The resulting digest
   */
  def hashBytes(bytes: Array[Byte], prefix: Array[Byte]): F[Digest]
}

object Hasher {
  def apply[F[_]](implicit hasher: Hasher[F]): Hasher[F] = hasher
}
