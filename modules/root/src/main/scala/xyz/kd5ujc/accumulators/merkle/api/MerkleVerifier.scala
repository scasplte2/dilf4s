package xyz.kd5ujc.accumulators.merkle.api

import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, toFoldableOps, toFunctorOps}

import xyz.kd5ujc.accumulators.merkle.{MerkleInclusionProof, MerkleTree}
import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.Json
import io.circe.syntax.EncoderOps

trait MerkleVerifier[F[_], L] {
  def isValid(proof: MerkleInclusionProof[L]): F[Boolean]
}

object MerkleVerifier {
  def make[F[_]: Monad, L](root: Digest[L])(implicit hasher: JsonHasher[F, L]): MerkleVerifier[F, L] =
    new MerkleVerifier[F, L] {
      override def isValid(proof: MerkleInclusionProof[L]): F[Boolean] = {

        def combine(a: Digest[L], b: Digest[L]): F[Digest[L]] =
          hasher.hash(
            Json.obj("leftDigest" -> a.asJson, "rightDigest" -> b.asJson),
            MerkleTree.InternalPrefix
          )

        proof.path
          .foldLeftM(proof.leafDigest) {
            case (acc, (digest, MerkleInclusionProof.leftSide))  => combine(digest, acc)
            case (acc, (digest, MerkleInclusionProof.rightSide)) => combine(acc, digest)
            case (acc, _)                                        => acc.pure[F] // for single node trees
          }
          .map(_.value.sameElements(root.value))
      }
    }
}
