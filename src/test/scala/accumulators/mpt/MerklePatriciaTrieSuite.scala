package accumulators.mpt

import cats.effect.IO
import cats.implicits.toTraverseOps

import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaNode, MerklePatriciaTrie, Nibble}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.Blake2b256Hasher

import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaTrieSuite extends SimpleIOSuite with Checkers {

  test("ensure root of MerkleTree is non-empty for list of non-empty strings") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves = (1 to 2).toSet
      leafMap <- leaves.toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
      trie    <- MerklePatriciaTrie.create[IO, Int](leafMap)
      //        _ <- hasher.hash("test").flatMap(d => IO.println(Hex.toHexString(d.value)))
      //        _ <- hasher.hash("test1").flatMap(d => IO.println(Hex.toHexString(d.value)))
      _ <- IO.println(leafMap)
      //      res <- IO.fromEither(trie.asJson.as[MerklePatriciaTrie])
      _ <- IO.println(trie.asJson.noSpaces)

      //      _ <- trie.rootNode match {
      //        case n: Branch    => IO.println(s"${n.asJson.noSpaces}")
      //        case n: Leaf      => IO.println(s"${n.asJson.noSpaces}")
      //        case n: Extension => IO.println(s"${n.asJson.noSpaces}")
      //        case n            => IO.println(n.asJson)
      //      }
    } yield expect(trie.rootNode.digest.value.nonEmpty)
  }

  test("create method produces a trie with expected structure") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      content = (1 to 2).toSet
      leafMap    <- content.toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
      trieActual <- MerklePatriciaTrie.create[IO, Int](leafMap)
      trieExpected <- for {
        leafNodes <- leafMap.toList.traverse {
          case (digest, data) =>
            val path = Nibble(digest)
            val json = data.asJson
            MerklePatriciaNode.Leaf[IO](path.tail, json).map(leaf => Map(path.head -> leaf))
        }
        mergedLeafNodes = leafNodes.fold(Map.empty)(_ ++ _)
        branchNode <- MerklePatriciaNode.Branch[IO](mergedLeafNodes)
      } yield MerklePatriciaTrie(branchNode)
    } yield expect(trieActual == trieExpected)
  }

// able to create a tree (of one node, of many nodes)
// able to insert data and change root node
// root node is deterministic from fixed initial data
// root node is deterministic from fixed initial and additional data
// values in a leaf node are as expected
// values in a branch node are as expected
// values in an extension node are as expected

}
