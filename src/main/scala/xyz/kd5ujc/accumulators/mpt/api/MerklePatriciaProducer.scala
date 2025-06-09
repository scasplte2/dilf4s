package xyz.kd5ujc.accumulators.mpt.api

import cats.MonadThrow

import xyz.kd5ujc.accumulators.mpt.MerklePatriciaTrie
import xyz.kd5ujc.accumulators.mpt.impl.{OptimizedMerklePatriciaProducer, SimpleMerklePatriciaProducer}
import xyz.kd5ujc.hash.Digest
import xyz.kd5ujc.hash.api.DigestProducer

import io.circe.Encoder

trait MerklePatriciaProducer[F[_]] {
  def create[A: Encoder](data: Map[Digest, A]): F[MerklePatriciaTrie]

  def insert[A: Encoder](current: MerklePatriciaTrie, data: Map[Digest, A]): F[Either[MerklePatriciaError, MerklePatriciaTrie]]

  def remove(current: MerklePatriciaTrie, data: List[Digest]): F[Either[MerklePatriciaError, MerklePatriciaTrie]]
}

object MerklePatriciaProducer {
  def apply[F[_]](implicit producer: MerklePatriciaProducer[F]): MerklePatriciaProducer[F] = producer

  def make[F[_]: DigestProducer: MonadThrow]: MerklePatriciaProducer[F] =
    new OptimizedMerklePatriciaProducer[F]

  def simple[F[_]: DigestProducer: MonadThrow]: MerklePatriciaProducer[F] =
    new SimpleMerklePatriciaProducer[F]
}

sealed trait MerklePatriciaError extends Throwable
case class InvalidData(message: String) extends MerklePatriciaError
case class OperationError(message: String) extends MerklePatriciaError
