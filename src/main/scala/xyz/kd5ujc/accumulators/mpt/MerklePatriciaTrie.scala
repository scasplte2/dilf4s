package xyz.kd5ujc.accumulators.mpt

import cats.MonadError
import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.hash.{Digest, JsonHasher}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerklePatriciaTrie(rootNode: MerklePatriciaNode)

object MerklePatriciaTrie {
  implicit def merkleTreeEncoder: Encoder[MerklePatriciaTrie] =
    (tree: MerklePatriciaTrie) => Json.obj("rootNode" -> tree.rootNode.asJson)

  implicit def merkleTreeDecoder: Decoder[MerklePatriciaTrie] = (c: HCursor) =>
    c.downField("rootNode").as[MerklePatriciaNode].map(MerklePatriciaTrie(_))

  def create[F[_]: JsonHasher, A: Encoder](data: Map[Digest, A])(implicit me: MonadError[F, Throwable]): F[MerklePatriciaTrie] =
    data.toList match {
      case (path, data) :: Nil => MerklePatriciaNode.Leaf[F](Nibble(path), data.asJson).map(MerklePatriciaTrie(_))
      case (hPath, hData) :: tail =>
        for {
          initialNode <- MerklePatriciaNode.Leaf[F](Nibble(hPath), hData.asJson)
          updatedNode <- tail.foldM(initialNode: MerklePatriciaNode) {
            case (acc, (tPath, tData)) => insertEncoded(acc, Nibble(tPath), tData.asJson)
          }
        } yield MerklePatriciaTrie(updatedNode)
      case _ => me.raiseError(new Exception("Unexpected input"))
    }

  def insert[F[_]: JsonHasher, A: Encoder](
    current: MerklePatriciaTrie,
    data:    Map[Digest, A]
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerklePatriciaTrie] =
    data.toList
      .foldM(current.rootNode) {
        case (acc, (_path, _data)) => insertEncoded(acc, Nibble(_path), _data.asJson)
      }
      .map(MerklePatriciaTrie(_))

  private def insertEncoded[F[_]: JsonHasher](
    currentNode: MerklePatriciaNode,
    path:        Seq[Nibble],
    data:        Json
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerklePatriciaNode] = {

    def insertForLeafNode(
      leafNode:     MerklePatriciaNode.Leaf,
      key:          Seq[Nibble],
      updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] = {

      val commonPrefix = Nibble.commonPrefix(leafNode.remaining, key)
      val leafSuffixRemaining = leafNode.remaining.drop(commonPrefix.length)
      val keyRemaining = key.drop(commonPrefix.length)

      if (leafSuffixRemaining.isEmpty && keyRemaining.isEmpty) {
        // If both paths are identical, we replace the existing Leaf with the new one.
        for {
          newLeaf <- MerklePatriciaNode.Leaf[F](key, data)
          result  <- updateParent(newLeaf)
        } yield result.asRight[InsertState[F]]
      } else {
        // The paths diverge, we need to create a Branch node.
        for {

          // Create a new Leaf node for the existing data with its updated path.
          existingLeaf <- MerklePatriciaNode.Leaf[F](leafSuffixRemaining.tail, leafNode.data)

          // Create a new Leaf node for the new data.
          newLeaf <- MerklePatriciaNode.Leaf[F](keyRemaining.tail, data)

          // Place both leaves in a Branch node
          branchNode <- MerklePatriciaNode.Branch[F](
            Map[Nibble, MerklePatriciaNode](
              leafSuffixRemaining.head -> existingLeaf,
              keyRemaining.head        -> newLeaf
            )
          )

          // If there's a common prefix, we need to create an Extension node as well.
          resultNode <-
            if (commonPrefix.nonEmpty) MerklePatriciaNode.Extension[F](commonPrefix, branchNode)
            else branchNode.pure[F]

          updatedNode <- updateParent(resultNode)
        } yield Done(updatedNode).asLeft[MerklePatriciaNode]
      }
    }

    def insertForExtensionNode(
      extensionNode: MerklePatriciaNode.Extension,
      _key:          Seq[Nibble],
      updateParent:  MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] = {
      val commonPrefix = Nibble.commonPrefix(extensionNode.shared, _key)
      val prefixRemaining = extensionNode.shared.drop(commonPrefix.length)
      val keyRemaining = _key.drop(commonPrefix.length)

      if (prefixRemaining.isEmpty) {
        (Continue(
          extensionNode.child,
          keyRemaining,
          (updatedChild: MerklePatriciaNode) =>
            MerklePatriciaNode
              .Extension[F](extensionNode.shared, updatedChild.asInstanceOf[MerklePatriciaNode.Branch])
              .flatMap(updateParent)
        ): InsertState[F]).asLeft[MerklePatriciaNode].pure[F]
      } else {
        for {
          newExtension <- MerklePatriciaNode.Extension[F](prefixRemaining, extensionNode.child)
          newLeaf <-
            if (keyRemaining.nonEmpty) MerklePatriciaNode.Leaf[F](keyRemaining, data)
            else MerklePatriciaNode.Leaf[F](Seq.empty, data)
          children = Map[Nibble, MerklePatriciaNode](
            prefixRemaining.head                            -> newExtension,
            keyRemaining.headOption.getOrElse(Nibble.empty) -> newLeaf
          )
          branchNode <- MerklePatriciaNode.Branch[F](children)
          resultNode <-
            if (commonPrefix.nonEmpty) MerklePatriciaNode.Extension[F](commonPrefix, branchNode)
            else branchNode.pure[F]
          updatedNode <- updateParent(resultNode)
        } yield Done(updatedNode).asLeft[MerklePatriciaNode]
      }
    }

    def insertForBranchNode(
      branchNode:   MerklePatriciaNode.Branch,
      _key:         Seq[Nibble],
      updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] =
      if (_key.isEmpty) {
        me.raiseError(new Exception("Key exhausted at branch node"))
      } else {
        val nibble = _key.head
        val keyRemaining = _key.tail
        val childOpt = branchNode.paths.get(nibble)

        childOpt match {
          case Some(childNode) =>
            (Continue(
              childNode,
              keyRemaining,
              (updatedChild: MerklePatriciaNode) =>
                MerklePatriciaNode.Branch[F](branchNode.paths + (nibble -> updatedChild)).flatMap(updateParent)
            ): InsertState[F]).asLeft[MerklePatriciaNode].pure[F]

          case None =>
            for {
              newLeaf       <- MerklePatriciaNode.Leaf[F](keyRemaining, data)
              updatedBranch <- MerklePatriciaNode.Branch[F](branchNode.paths + (nibble -> newLeaf))
              result        <- updateParent(updatedBranch)
            } yield result.asRight[InsertState[F]]
        }
      }

    def step(state: InsertState[F]): F[Either[InsertState[F], MerklePatriciaNode]] = state match {
      case Continue(currentNode, key, updateParent) =>
        currentNode match {
          case node: MerklePatriciaNode.Leaf      => insertForLeafNode(node, key, updateParent)
          case node: MerklePatriciaNode.Extension => insertForExtensionNode(node, key, updateParent)
          case node: MerklePatriciaNode.Branch    => insertForBranchNode(node, key, updateParent)
        }

      case Done(node) => node.asRight[InsertState[F]].pure[F]
    }

    val initialState = Continue[F](currentNode, path, _.pure[F])
    me.tailRecM[InsertState[F], MerklePatriciaNode](initialState)(step)
  }

  def collectLeafNodes(trie: MerklePatriciaTrie): List[MerklePatriciaNode.Leaf] = {
    @tailrec
    def traverse(nodes: List[MerklePatriciaNode], acc: List[MerklePatriciaNode.Leaf]): List[MerklePatriciaNode.Leaf] =
      nodes match {
        case Nil                                               => acc
        case (head: MerklePatriciaNode.Leaf) :: tail           => traverse(tail, head :: acc)
        case MerklePatriciaNode.Branch(paths, _) :: tail       => traverse(paths.values.toList ++ tail, acc)
        case MerklePatriciaNode.Extension(_, child, _) :: tail => traverse(child :: tail, acc)
      }

    traverse(List(trie.rootNode), List()).reverse
  }

  private sealed trait InsertState[F[_]]

  private case class Continue[F[_]](
    currentNode:  MerklePatriciaNode,
    key:          Seq[Nibble],
    updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
  ) extends InsertState[F]

  private case class Done[F[_]](node: MerklePatriciaNode) extends InsertState[F]
}
