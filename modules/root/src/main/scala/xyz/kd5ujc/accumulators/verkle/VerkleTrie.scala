package xyz.kd5ujc.accumulators.verkle

trait VerkleTrie[F[_], K, V] {

  def get(key: K): F[V]

  def put(key: K, value: V): F[V]

  def remove(key: K): F[Unit]

  def root: F[String]
}
