package xyz.kd5ujc.hash.api

import cats.MonadThrow
import cats.syntax.applicative._
import cats.syntax.functor._

import xyz.kd5ujc.hash.HashProof.PreimageKnowledge
import xyz.kd5ujc.hash.{Digest, HashProof}

import io.circe.Encoder

/**
 * Type class for verifying hash computations and proofs of preimage knowledge
 */
trait HashVerifier[F[_]] {

  /**
   * Confirm that data hashes to an expected digest
   *
   * @param data The data to hash
   * @param expectedHash The expected digest
   * @return True if the data hashes to the expected digest
   */
  def confirmDigest[A: Encoder](data: A, expectedHash: Digest): F[Boolean]

  /**
   * Confirm that a hash proof is valid for the given data
   *
   * @param data The preimage data
   * @param proof The proof to verify
   * @return True if the proof is valid
   */
  def confirmHash[A: Encoder](data: A, proof: HashProof): F[Boolean]

  /**
   * Confirm that data matches any of the given proofs
   *
   * @param data The preimage data
   * @param proofs The proofs to verify against
   * @return True if the data satisfies any of the proofs
   */
  def confirmAnyProof[A: Encoder](data: A, proofs: Seq[HashProof]): F[Boolean]

  /**
   * Confirm a proof of preimage knowledge without seeing the preimage
   *
   * @param digest The claimed digest
   * @param proof The zero-knowledge proof
   * @return True if the proof demonstrates knowledge of a valid preimage
   */
  def confirmPreimage(digest: Digest, proof: HashProof): F[Boolean]
}

object HashVerifier {
  def apply[F[_]](implicit verifier: HashVerifier[F]): HashVerifier[F] = verifier

  /**
   * Create a verifier instance using a DigestProducer
   */
  private def fromDigestProducer[F[_]: MonadThrow](producer: DigestProducer[F]): HashVerifier[F] =
    new HashVerifier[F] {
      def confirmDigest[A: Encoder](data: A, expectedHash: Digest): F[Boolean] =
        producer.hash(data).map(_ == expectedHash)

      def confirmHash[A: Encoder](data: A, proof: HashProof): F[Boolean] =
        producer.hash(data).map(_ == proof.digest)

      def confirmAnyProof[A: Encoder](data: A, proofs: Seq[HashProof]): F[Boolean] =
        producer.hash(data).map(hash => proofs.exists(_.digest == hash))

      def confirmPreimage(digest: Digest, proof: HashProof): F[Boolean] =
        (proof.digest == digest && proof.proofType == PreimageKnowledge).pure[F]
    }

  /**
   * Automatically derive a HashVerifier from a DigestProducer in scope
   */
  implicit def fromProducer[F[_]: MonadThrow](implicit producer: DigestProducer[F]): HashVerifier[F] =
    fromDigestProducer(producer)

  /**
   * Provides syntax extensions for more ergonomic hash verification
   */
  object syntax {
    implicit class HashVerifierOps[A](val data: A) extends AnyVal {

      def confirm[F[_]](expectedHash: Digest)(implicit hv: HashVerifier[F], enc: Encoder[A]): F[Boolean] =
        hv.confirmDigest(data, expectedHash)

      def confirmHash[F[_]](proof: HashProof)(implicit hv: HashVerifier[F], enc: Encoder[A]): F[Boolean] =
        hv.confirmHash(data, proof)

      def confirmAnyProof[F[_]](proofs: Seq[HashProof])(implicit hv: HashVerifier[F], enc: Encoder[A]): F[Boolean] =
        hv.confirmAnyProof(data, proofs)
    }
  }
}
