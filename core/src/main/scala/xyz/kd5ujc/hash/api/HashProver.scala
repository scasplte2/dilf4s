package xyz.kd5ujc.hash.api

import cats.MonadThrow
import cats.syntax.functor._

import xyz.kd5ujc.hash.HashProof.{HashComputation, PreimageKnowledge}
import xyz.kd5ujc.hash.{Digest, HashProof, InvalidProof, InvalidProofFormat}

import io.circe.Encoder

/**
 * Type class for generating proofs that can verify hash computations and preimage knowledge
 */
trait HashProver[F[_]] {

  /**
   * Generate a proof that data hashes to a specific digest
   *
   * @param data The data to hash
   * @param prefix Optional prefix bytes to prepend
   * @return A proof containing the digest and verification data
   */
  def attestHash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[HashProof]

  /**
   * Generate a proof that we know a preimage for a given digest
   * without revealing the preimage itself
   *
   * @param data The preimage data
   * @param digest The digest to prove knowledge of
   * @return A zero-knowledge proof of preimage knowledge, or an error if the data
   *         does not hash to the claimed digest
   */
  def attestPreimage[A: Encoder](data: A, digest: Digest): F[Either[InvalidProof, HashProof]]
}

object HashProver {
  def apply[F[_]](implicit prover: HashProver[F]): HashProver[F] = prover

  /**
   * Create a prover instance using a DigestProducer
   */
  private def fromDigestProducer[F[_]: MonadThrow](producer: DigestProducer[F]): HashProver[F] =
    new HashProver[F] {
      def attestHash[A: Encoder](data: A, prefix: Array[Byte] = Array()): F[HashProof] =
        producer.hash(data, prefix).map(digest => HashProof(digest, Array(), HashComputation))

      def attestPreimage[A: Encoder](data: A, digest: Digest): F[Either[InvalidProof, HashProof]] =
        producer.hash(data).map { computed =>
          if (computed == digest) Right(HashProof(digest, Array(), PreimageKnowledge))
          else Left(InvalidProofFormat("Data does not hash to claimed digest"))
        }
    }

  /**
   * Automatically derive a HashProver from a DigestProducer in scope
   */
  implicit def fromProducer[F[_]: MonadThrow](implicit producer: DigestProducer[F]): HashProver[F] =
    fromDigestProducer(producer)

  /**
   * Provides syntax extensions for more ergonomic proof generation
   */
  object syntax {
    implicit class HashProverOps[A](val data: A) extends AnyVal {
      def attestHash[F[_]](prefix: Array[Byte] = Array())(implicit hp: HashProver[F], enc: Encoder[A]): F[HashProof] =
        hp.attestHash(data, prefix)

      def attestPreimage[F[_]](digest: Digest)(implicit hp: HashProver[F], enc: Encoder[A]): F[Either[InvalidProof, HashProof]] =
        hp.attestPreimage(data, digest)
    }
  }
}
