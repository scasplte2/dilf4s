package xyz.kd5ujc.accumulators.mpt

import cats.MonadThrow

import xyz.kd5ujc.accumulators.mpt.api.{MerklePatriciaError, MerklePatriciaProducer}
import xyz.kd5ujc.hash.Digest
import xyz.kd5ujc.hash.api.DigestProducer

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerklePatriciaTrie(rootNode: MerklePatriciaNode)

object MerklePatriciaTrie {
  implicit val merkleTreeEncoder: Encoder[MerklePatriciaTrie] =
    (tree: MerklePatriciaTrie) => Json.obj("rootNode" -> tree.rootNode.asJson)

  implicit val merkleTreeDecoder: Decoder[MerklePatriciaTrie] = (c: HCursor) =>
    c.downField("rootNode").as[MerklePatriciaNode].map(MerklePatriciaTrie(_))

  /**
   * Create a new MerklePatriciaTrie from a map of data using the optimized producer
   */
  def make[F[_]: DigestProducer: MonadThrow, A: Encoder](data: Map[Digest, A]): F[MerklePatriciaTrie] =
    MerklePatriciaProducer
      .make[F]
      .create(data)

  /**
   * Create a new MerklePatriciaTrie from a map of data using the simple producer
   */
  def simple[F[_]: DigestProducer: MonadThrow, A: Encoder](data: Map[Digest, A]): F[MerklePatriciaTrie] =
    MerklePatriciaProducer
      .simple[F]
      .create(data)

  /**
   * Create a new MerklePatriciaTrie from a map of data (uses simple producer for backward compatibility)
   */
  def create[F[_]: DigestProducer: MonadThrow, A: Encoder](data: Map[Digest, A]): F[MerklePatriciaTrie] =
    simple(data)

  /**
   * Insert new data into an existing MerklePatriciaTrie (uses simple producer for backward compatibility)
   */
  def insert[F[_]: DigestProducer: MonadThrow, A: Encoder](
    current: MerklePatriciaTrie,
    data:    Map[Digest, A]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    MerklePatriciaProducer
      .simple[F]
      .insert(current, data)

  /**
   * Remove data from an existing MerklePatriciaTrie (uses simple producer for backward compatibility)
   */
  def remove[F[_]: DigestProducer: MonadThrow](
    current: MerklePatriciaTrie,
    data:    List[Digest]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    MerklePatriciaProducer
      .simple[F]
      .remove(current, data)

  def collectLeafNodes(trie: MerklePatriciaTrie): List[MerklePatriciaNode.Leaf] = {
    @scala.annotation.tailrec
    def traverse(nodes: List[MerklePatriciaNode], acc: List[MerklePatriciaNode.Leaf]): List[MerklePatriciaNode.Leaf] =
      nodes match {
        case Nil                                               => acc
        case (head: MerklePatriciaNode.Leaf) :: tail           => traverse(tail, head :: acc)
        case MerklePatriciaNode.Branch(paths, _) :: tail       => traverse(paths.values.toList ++ tail, acc)
        case MerklePatriciaNode.Extension(_, child, _) :: tail => traverse(child :: tail, acc)
      }

    traverse(List(trie.rootNode), List()).reverse
  }
}
