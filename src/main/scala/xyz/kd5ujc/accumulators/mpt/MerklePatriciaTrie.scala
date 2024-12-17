package xyz.kd5ujc.accumulators.mpt

import cats.syntax.all._
import cats.{Monad, MonadError}

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

  def create[F[_]: JsonHasher, A: Encoder](
    data: Map[Digest, A]
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerklePatriciaTrie] =
    data.toList match {
      case (path, data) :: Nil => MerklePatriciaNode.Leaf[F](Nibble(path), data.asJson).map(MerklePatriciaTrie(_))
      case (hPath, hData) :: tail =>
        for {
          initialNode <- MerklePatriciaNode.Leaf[F](Nibble(hPath), hData.asJson)
          updatedNode <- tail.foldM(initialNode: MerklePatriciaNode) {
            case (acc, (tPath, tData)) => insertEncoded(acc, Nibble(tPath), tData.asJson)
          }
        } yield MerklePatriciaTrie(updatedNode)
      case _ => MonadError[F, Throwable].raiseError(new Exception("Unexpected input"))
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

  def remove[F[_]: JsonHasher](
    current: MerklePatriciaTrie,
    data:    List[Digest]
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerklePatriciaTrie] =
    data
      .foldM(current.rootNode) {
        case (acc, _path) => removeEncoded(acc, Nibble(_path))
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
      _key:         Seq[Nibble],
      updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] =
      if (leafNode.remaining == _key) {
        // partial paths are identical, replace the existing Leaf with the new one.
        for {
          newLeaf <- MerklePatriciaNode.Leaf[F](_key, data)
          result  <- updateParent(newLeaf)
        } yield result.asRight[InsertState[F]]

      } else {
        // partial paths diverge, determine what type of node to replace with
        val commonPrefix = Nibble.commonPrefix(leafNode.remaining, _key)
        val leafRemaining = leafNode.remaining.drop(commonPrefix.length)
        val keyRemaining = _key.drop(commonPrefix.length)

        for {
          // Create a new Leaf node for the existing data with its updated path.
          existingLeaf <- MerklePatriciaNode.Leaf[F](leafRemaining.tail, leafNode.data)

          // Create a new Leaf node for the new data.
          newLeaf <- MerklePatriciaNode.Leaf[F](keyRemaining.tail, data)

          // Place both leaves in a Branch node
          branchNode <- MerklePatriciaNode.Branch[F](
            Map[Nibble, MerklePatriciaNode](
              leafRemaining.head -> existingLeaf,
              keyRemaining.head  -> newLeaf
            )
          )

          // If there's a common prefix, create an Extension node to place the Branch in
          resultNode <-
            if (commonPrefix.nonEmpty) MerklePatriciaNode.Extension[F](commonPrefix, branchNode)
            else branchNode.pure[F]

          updatedNode <- updateParent(resultNode)
        } yield InsertDone(updatedNode).asLeft[MerklePatriciaNode]
      }

    def insertForExtensionNode(
      extensionNode: MerklePatriciaNode.Extension,
      _key:          Seq[Nibble],
      updateParent:  MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] = {
      val commonPrefix = Nibble.commonPrefix(extensionNode.shared, _key)
      val sharedRemaining = extensionNode.shared.drop(commonPrefix.length)
      val keyRemaining = _key.drop(commonPrefix.length)

      if (_key.isEmpty) me.raiseError(new Exception("Key exhausted at extension node"))
      else if (sharedRemaining.isEmpty) {
        // shared path portion is not changing so just update child node with remaining path
        (InsertContinue(
          extensionNode.child,
          keyRemaining,
          {
            case branch: MerklePatriciaNode.Branch =>
              MerklePatriciaNode.Extension[F](extensionNode.shared, branch).flatMap(updateParent)

            case _ => me.raiseError(new Exception("Unexpected node type while creating extension node"))
          }
        ): InsertState[F]).asLeft[MerklePatriciaNode].pure[F]
      } else {
        // shared path diverges, update Extension node path and remaining path in a Leaf then place within a Branch
        for {
          newExtension <- MerklePatriciaNode.Extension[F](sharedRemaining.tail, extensionNode.child)
          newLeaf      <- MerklePatriciaNode.Leaf[F](keyRemaining.tail, data)
          branchNode <- MerklePatriciaNode.Branch[F](
            Map(
              sharedRemaining.head -> newExtension,
              keyRemaining.head    -> newLeaf
            )
          )
          resultNode <-
            if (commonPrefix.nonEmpty) MerklePatriciaNode.Extension[F](commonPrefix, branchNode)
            else branchNode.pure[F]
          updatedNode <- updateParent(resultNode)
        } yield InsertDone(updatedNode).asLeft[MerklePatriciaNode]
      }
    }

    def insertForBranchNode(
      branchNode:   MerklePatriciaNode.Branch,
      _key:         Seq[Nibble],
      updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
    ): F[Either[InsertState[F], MerklePatriciaNode]] =
      if (_key.isEmpty) me.raiseError(new Exception("Key exhausted at branch node"))
      else {
        val nibble = _key.head
        val keyRemaining = _key.tail

        branchNode.paths.get(nibble) match {
          case Some(childNode) =>
            (InsertContinue(
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
      case InsertContinue(currentNode, key, updateParent) =>
        currentNode match {
          case node: MerklePatriciaNode.Leaf      => insertForLeafNode(node, key, updateParent)
          case node: MerklePatriciaNode.Extension => insertForExtensionNode(node, key, updateParent)
          case node: MerklePatriciaNode.Branch    => insertForBranchNode(node, key, updateParent)
        }

      case InsertDone(node) => node.asRight[InsertState[F]].pure[F]
    }

    val initialState = InsertContinue[F](currentNode, path, _.pure[F])
    Monad[F].tailRecM[InsertState[F], MerklePatriciaNode](initialState)(step)
  }

  def removeEncoded[F[_]: JsonHasher](
    currentNode: MerklePatriciaNode,
    path:        Seq[Nibble]
  )(
    implicit me: MonadError[F, Throwable]
  ): F[MerklePatriciaNode] = {

    def removeForLeafNode(
      leafNode:     MerklePatriciaNode.Leaf,
      _key:         Seq[Nibble],
      updateParent: Option[MerklePatriciaNode] => F[Option[MerklePatriciaNode]]
    ): F[Either[RemoveState[F], Option[MerklePatriciaNode]]] =
      if (leafNode.remaining == _key) updateParent(None).map(Right(_))
      else me.pure(Right(Some(leafNode)))

    def removeForExtensionNode(
      extensionNode: MerklePatriciaNode.Extension,
      _key:          Seq[Nibble],
      updateParent:  Option[MerklePatriciaNode] => F[Option[MerklePatriciaNode]]
    ): F[Either[RemoveState[F], Option[MerklePatriciaNode]]] = {
      val commonPrefix = Nibble.commonPrefix(extensionNode.shared, _key)

      if (commonPrefix.length == extensionNode.shared.length) {
        // _key contains Extension node shared path, proceed to child
        (RemoveContinue(
          extensionNode.child,
          _key.drop(commonPrefix.length),
          {
            case Some(updatedChild) =>
              updatedChild match {
                case childBranch: MerklePatriciaNode.Branch =>
                  MerklePatriciaNode
                    .Extension[F](extensionNode.shared, childBranch)
                    .flatMap(node => updateParent(Some(node)))

                case childLeaf: MerklePatriciaNode.Leaf =>
                  MerklePatriciaNode
                    .Leaf[F](extensionNode.shared ++ childLeaf.remaining, childLeaf.data)
                    .flatMap(node => updateParent(Some(node)))

                case childExtension: MerklePatriciaNode.Extension =>
                  MerklePatriciaNode
                    .Extension[F](extensionNode.shared ++ childExtension.shared, childExtension.child)
                    .flatMap(node => updateParent(Some(node)))
              }

            case None => updateParent(None)
          }
        ): RemoveState[F]).asLeft[Option[MerklePatriciaNode]].pure[F]

      } else {
        // Key does not match, nothing to remove
        me.pure(Right(Some(extensionNode)))
      }
    }

    def removeForBranchNode(
      branchNode:   MerklePatriciaNode.Branch,
      _key:         Seq[Nibble],
      updateParent: Option[MerklePatriciaNode] => F[Option[MerklePatriciaNode]]
    ): F[Either[RemoveState[F], Option[MerklePatriciaNode]]] =
      if (_key.nonEmpty) {
        val nibble = _key.head
        val keyRemaining = _key.tail

        branchNode.paths.get(nibble) match {
          case Some(childNode) =>
            (RemoveContinue(
              childNode,
              keyRemaining,
              {
                case Some(updatedChild) =>
                  MerklePatriciaNode
                    .Branch[F](branchNode.paths + (nibble -> updatedChild))
                    .flatMap(node => updateParent(Some(node)))
                case None =>
                  val updatedPaths = branchNode.paths - nibble

                  updatedPaths.size match {
                    case 0 =>
                      // No paths left, remove this Branch node
                      updateParent(None)

                    case 1 =>
                      // Only one child left, can compress
                      val (remainingNibble, onlyChild) = updatedPaths.head
                      onlyChild match {
                        case leafNode: MerklePatriciaNode.Leaf =>
                          MerklePatriciaNode
                            .Leaf[F](Seq(remainingNibble) ++ leafNode.remaining, leafNode.data)
                            .flatMap(node => updateParent(Some(node)))

                        case extensionNode: MerklePatriciaNode.Extension =>
                          MerklePatriciaNode
                            .Extension[F](Seq(remainingNibble) ++ extensionNode.shared, extensionNode.child)
                            .flatMap(node => updateParent(Some(node)))

                        case branchNode: MerklePatriciaNode.Branch =>
                          MerklePatriciaNode
                            .Extension[F](Seq(remainingNibble), branchNode)
                            .flatMap(node => updateParent(Some(node)))
                      }

                    case _ =>
                      // More than one child, keep the Branch node
                      MerklePatriciaNode
                        .Branch[F](updatedPaths)
                        .flatMap(node => updateParent(Some(node)))
                  }
              }
            ): RemoveState[F]).asLeft[Option[MerklePatriciaNode]].pure[F]

          case None =>
            // Key not found in Branch, nothing to remove
            me.pure(Right(Some(branchNode)))
        }
      } else {
        // Key exhausted at Branch node, key not found
        me.pure(Right(Some(branchNode)))
      }

    def step(state: RemoveState[F]): F[Either[RemoveState[F], Option[MerklePatriciaNode]]] = state match {
      case RemoveContinue(currentNode, key, updateParent) =>
        currentNode match {
          case node: MerklePatriciaNode.Leaf      => removeForLeafNode(node, key, updateParent)
          case node: MerklePatriciaNode.Extension => removeForExtensionNode(node, key, updateParent)
          case node: MerklePatriciaNode.Branch    => removeForBranchNode(node, key, updateParent)
        }
      case RemoveDone(nodeOpt) => me.pure(Right(nodeOpt))
    }

    val initialState = RemoveContinue[F](currentNode, path, _.pure[F])

    Monad[F].tailRecM[RemoveState[F], Option[MerklePatriciaNode]](initialState)(step).flatMap {
      case Some(newRootNode) => newRootNode.pure[F]
      case None              => MerklePatriciaNode.Branch[F](Map.empty).widen
    }
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

  private sealed trait RemoveState[F[_]]

  private case class InsertContinue[F[_]](
    currentNode:  MerklePatriciaNode,
    key:          Seq[Nibble],
    updateParent: MerklePatriciaNode => F[MerklePatriciaNode]
  ) extends InsertState[F]

  private case class InsertDone[F[_]](node: MerklePatriciaNode) extends InsertState[F]

  private case class RemoveContinue[F[_]](
    currentNode:  MerklePatriciaNode,
    key:          Seq[Nibble],
    updateParent: Option[MerklePatriciaNode] => F[Option[MerklePatriciaNode]]
  ) extends RemoveState[F]

  private case class RemoveDone[F[_]](nodeOpt: Option[MerklePatriciaNode]) extends RemoveState[F]

}
