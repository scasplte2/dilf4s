package xyz.kd5ujc.accumulators.mpt

import cats.MonadError
import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import xyz.kd5ujc.hash.JsonHasher

final case class MerklePatriciaTrie(rootNode: MerklePatriciaNode)

object MerklePatriciaTrie {
  implicit def merkleTreeEncoder: Encoder[MerklePatriciaTrie] = (tree: MerklePatriciaTrie) => Json.obj("rootNode" -> tree.rootNode.asJson)

  implicit def merkleTreeDecoder: Decoder[MerklePatriciaTrie] = (c: HCursor) =>
    c.downField("rootNode").as[MerklePatriciaNode].map(MerklePatriciaTrie(_))

  def create[F[_]: JsonHasher, A: Encoder](data: NonEmptySet[A])(implicit me: MonadError[F, Throwable]): F[MerklePatriciaTrie] = {
    def singleElement(el: A): F[MerklePatriciaNode] = for {
      (path, json) <- JsonHasher[F].hash(el).map(digest => Nibble(digest) -> el.asJson)
      leaf         <- MerklePatriciaNode.Leaf[F](json, path)
    } yield leaf

    data.toList match {
      case head :: Nil => singleElement(head).map(MerklePatriciaTrie(_))
      case head :: _ =>
        for {
          initialNode <- singleElement(head)
          tailSet     <- NonEmptySet.fromSet(data.tail).liftTo[F](new Exception("Unhandled exception in tail"))
          trie        <- update(tailSet, initialNode)
        } yield trie
      case _ => me.raiseError(new Exception("Unexpected input"))
    }
  }

  def update[F[_]: JsonHasher, A: Encoder](
                                            data:            NonEmptySet[A],
                                            initialRootNode: MerklePatriciaNode
                                          )(
                                            implicit me: MonadError[F, Throwable]
                                          ): F[MerklePatriciaTrie] =
    for {
      jsonPathPairs <- data.toList.traverse { a =>
        JsonHasher[F].hash(a).map { digest =>
          Nibble(digest) -> a.asJson
        }
      }
      rootNode <- jsonPathPairs.foldM[F, MerklePatriciaNode](initialRootNode) {
        case (currentNode, (key, data)) => insertEncoded(currentNode, key, data)
      }
    } yield MerklePatriciaTrie(rootNode)

  private def insertEncoded[F[_]: JsonHasher](
                                               currentNode: MerklePatriciaNode,
                                               path:        Seq[Nibble],
                                               data:        Json
                                             )(
                                               implicit me: MonadError[F, Throwable]
                                             ): F[MerklePatriciaNode] = {

    def insertForLeafNode(
                           leafNode:     MerklePatriciaNode.Leaf,
                           _key:         Seq[Nibble],
                           updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
                         ): F[Either[InsertState[F], MerklePatriciaNode]] = {
      val commonPrefix = Nibble.commonPrefix(leafNode.remaining, _key)
      val leafSuffixRemaining = leafNode.remaining.drop(commonPrefix.length)
      val keyRemaining = _key.drop(commonPrefix.length)

      if (leafSuffixRemaining.isEmpty && keyRemaining.isEmpty) for {
        newLeaf <- MerklePatriciaNode.Leaf[F](data, Seq.empty)
        result  <- updateParent(newLeaf)
      } yield result.asRight[InsertState[F]]
      else
        for {
          leafChild <-
            if (leafSuffixRemaining.nonEmpty) MerklePatriciaNode.Leaf[F](leafNode.data, leafSuffixRemaining)
            else leafNode.pure[F]
          newChild <-
            if (keyRemaining.nonEmpty) MerklePatriciaNode.Leaf[F](data, keyRemaining)
            else MerklePatriciaNode.Leaf[F](data, Seq.empty)
          children = Map[Nibble, MerklePatriciaNode](
            leafSuffixRemaining.headOption.getOrElse(Nibble.empty) -> leafChild,
            keyRemaining.headOption.getOrElse(Nibble.empty)        -> newChild
          )
          branchNode <- MerklePatriciaNode.Branch[F](children)
          resultNode <-
            if (commonPrefix.nonEmpty) MerklePatriciaNode.Extension[F](commonPrefix, branchNode)
            else me.pure(branchNode)
          updatedNode <- updateParent(resultNode)
        } yield Done(updatedNode).asLeft[MerklePatriciaNode]
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
      } else
        for {
          newExtension <- MerklePatriciaNode.Extension[F](prefixRemaining, extensionNode.child)
          newLeaf <-
            if (keyRemaining.nonEmpty) MerklePatriciaNode.Leaf[F](data, keyRemaining)
            else MerklePatriciaNode.Leaf[F](data, Seq.empty)
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
              newLeaf       <- MerklePatriciaNode.Leaf[F](data, keyRemaining)
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

  private sealed trait InsertState[F[_]]

  private case class Continue[F[_]](
                                     currentNode:  MerklePatriciaNode,
                                     key:          Seq[Nibble],
                                     updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
                                   ) extends InsertState[F]

  private case class Done[F[_]](node: MerklePatriciaNode) extends InsertState[F]
}
