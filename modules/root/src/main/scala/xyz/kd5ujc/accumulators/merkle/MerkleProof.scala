package xyz.kd5ujc.accumulators.merkle

import cats.MonadError
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import cats.syntax.either._
import cats.syntax.option._

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.Node
import xyz.kd5ujc.accumulators.merkle.nodes.{InternalNode, LeafNode}
import xyz.kd5ujc.hash.Digest._
import xyz.kd5ujc.hash.{Digest, Hasher}

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerkleProof[L](
  leafDigest: Digest[L],
  witness:    Seq[(Digest[L], Side)]
)

object MerkleProof {
  val LeftSide: Side = Side(0: Byte)
  val RightSide: Side = Side(1: Byte)

  implicit def proofEncoder[L]: Encoder[MerkleProof[L]] = (mp: MerkleProof[L]) =>
    Json.obj(
      "leafDigest" -> mp.leafDigest.asJson,
      "witness" -> mp.witness.map {
        case (digest, side) =>
          Json.obj(
            "digest" -> digest.asJson,
            "side"   -> side.asJson
          )
      }.asJson
    )

  implicit def proofDecoder[L]: Decoder[MerkleProof[L]] = (c: HCursor) =>
    for {
      leafDigest <- c.downField("leafDigest").as[Digest[L]]
      witness    <- c.downField("witness").as[Seq[(Digest[L], Side)]]
    } yield MerkleProof[L](leafDigest, witness)

  def fromLeafNode[A, L](leaf: LeafNode[A, L]): MerkleTree[L] => Option[MerkleProof[L]] = fromLeafDigest(leaf.digest)

  def fromLeafDigest[L](digest: Digest[L]): MerkleTree[L] => Option[MerkleProof[L]] = tree =>
    tree.leafDigestIndex.get(digest).flatMap(i => proofByIndex(i)(tree))

  def proofByIndex[A, L](index: Int): MerkleTree[L] => Option[MerkleProof[L]] = tree => {

    // bitwise shift operation to calculate log2 by counting number of divisions by 2
    @tailrec
    def log2(n: Int, acc: Int = 0): Int =
      if (n <= 1) acc
      else log2(n >> 1, acc + 1)

    val maxDepth = log2(tree.leafDigestIndex.size)

    @tailrec
    def loop(
      node:  Option[Node[L]],
      acc:   Seq[Option[(Digest[L], Side)]],
      depth: Int
    ): Option[(LeafNode[_, L], Seq[Option[(Digest[L], Side)]])] =
      node match {
        case Some(n: InternalNode[_]) if ((index >> (maxDepth - depth)) & 1) == 0 =>
          loop(Some(n.left), n.right.map(_.digest).map((_, MerkleProof.LeftSide)) +: acc, depth + 1)

        case Some(n: InternalNode[_]) =>
          loop(n.right, Some((n.left.digest, MerkleProof.RightSide)) +: acc, depth + 1)

        case Some(n: LeafNode[_, _]) =>
          Some((n, acc))

        case _ =>
          None
      }

    if (index < 0 || index >= tree.leafDigestIndex.size) None
    else {
      loop(tree.rootNode.some, Seq(), 0).flatMap { lp =>
        lp._2.sequence.map(w => MerkleProof(lp._1.digest, w))
      }
    }
  }

  def verify[F[_], L](
    proof:       MerkleProof[L],
    root:        Digest[L]
  )(implicit me: MonadError[F, Throwable], hasher: Hasher[F, L]): F[Boolean] = {

    def combine(a: Digest[L], b: Digest[L]): F[Digest[L]] = hasher.hashBytes(a.value ++ b.value)

    MonadError[F, Throwable]
      .tailRecM[MerkleProof[L], Digest[L]](proof) {
        case MerkleProof(acc, (digest, LeftSide) :: _)  => combine(digest, acc).map(_.asRight[MerkleProof[L]])
        case MerkleProof(acc, (digest, RightSide) :: _) => combine(acc, digest).map(_.asRight[MerkleProof[L]])
        case MerkleProof(acc, _)                        => me.pure(Right(acc))
      }
      .flatMap(hasher.compare(_, root))
  }
}

final case class Side(value: Byte) extends AnyVal

object Side {
  implicit val sideEncoder: Encoder[Side] = deriveEncoder
  implicit val sideDecoder: Decoder[Side] = deriveDecoder
}
