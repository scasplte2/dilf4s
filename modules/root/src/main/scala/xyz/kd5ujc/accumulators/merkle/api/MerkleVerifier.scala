package xyz.kd5ujc.accumulators.merkle.api

import cats.Monad
import cats.implicits.toFunctorOps
import cats.syntax.either._

import xyz.kd5ujc.accumulators.merkle.MerkleInclusionProof
import xyz.kd5ujc.hash.{Digest, Hasher}

trait MerkleVerifier[F[_], L] {
  def isValid(proof: MerkleInclusionProof[L]): F[Boolean]
}

object MerkleVerifier {
  def make[F[_]: Monad, L](root: Digest[L])(implicit hasher: Hasher[F, L]): MerkleVerifier[F, L] =
    new MerkleVerifier[F, L] {
      override def isValid(proof: MerkleInclusionProof[L]): F[Boolean] = {

        def combine(a: Digest[L], b: Digest[L]): F[Digest[L]] = hasher.hashBytes(a.value ++ b.value)

        Monad[F]
          .tailRecM[MerkleInclusionProof[L], Digest[L]](proof) {
            case MerkleInclusionProof(acc, (digest, MerkleInclusionProof.leftSide) :: _) =>
              combine(digest, acc).map(_.asRight[MerkleInclusionProof[L]])

            case MerkleInclusionProof(acc, (digest, MerkleInclusionProof.rightSide) :: _) =>
              combine(acc, digest).map(_.asRight[MerkleInclusionProof[L]])

            case MerkleInclusionProof(acc, _) => Monad[F].pure(Right(acc))
          }
          .map(_.value.sameElements(root.value))
      }
    }
}
