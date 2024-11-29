package accumulators.mpt

import cats.effect.IO
import cats.implicits.{toBifunctorOps, toTraverseOps}

import scala.collection.immutable.SortedSet

import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaNode, MerklePatriciaTrie, Nibble}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Blake2b256Hasher, Digest, l256}

import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaTrieSuite extends SimpleIOSuite with Checkers {

  test("trie can be encoded and decoded from json") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leafMap <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trieExpected <- MerklePatriciaTrie.create(leafMap)
        trieActual <- IO.fromEither(trieExpected.asJson.as[MerklePatriciaTrie])
      } yield expect(trieExpected == trieActual)
    }
  }

  test("root of trie is non-empty") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leafMap <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie <- MerklePatriciaTrie.create(leafMap)
      } yield expect(trie.rootNode.digest.value.nonEmpty)
    }
  }

  test("trie from create contains all values in leaves") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leafMap <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie <- MerklePatriciaTrie.create(leafMap)
        listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie).traverse(_.data.as[Long]))
        sortedInputSet = SortedSet.from(listLong)
        sortedOutputSet = SortedSet.from(listLeaves)
      } yield expect(sortedInputSet == sortedOutputSet)
    }
  }

  test("trie from insert contains all values in leaves") {
    forall(Gen.listOfN(32, Gen.long).flatMap(v1 => Gen.listOfN(32, Gen.long).flatMap(v2 => (v1, v2)))) {
      case (list1, list2) =>
        for {
          implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
          implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
          initMap <- list1.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          updMap <- list2.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          trie <- MerklePatriciaTrie.create(initMap)
          trie2 <- MerklePatriciaTrie.insert(trie, updMap)
          listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie2).traverse(_.data.as[Long]))
          sortedInputSet = SortedSet.from(list1 ++ list2)
          sortedOutputSet = SortedSet.from(listLeaves)
        } yield expect(sortedInputSet == sortedOutputSet)
    }
  }

  test("updating a trie with an existing path updates the data held by the leaf and changes the root node digest") {
    forall(Gen.long.flatMap(v1 => Gen.long.flatMap(v2 => (v1, v2))).suchThat(g => g._1 != g._2)) {
      case (val1, val2) =>
        for {
          implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
          implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
          path <- IO.fromEither(l256.from(Array.fill(32)(1: Byte)).toEither.leftMap(err => new Exception(s"${err.toString}")))
          trie1 <- MerklePatriciaTrie.create[IO, Long](Map(path -> val1))
          trie2 <- MerklePatriciaTrie.insert[IO, Long](trie1, Map(path -> val2))
          (root1, data1, digest1) <- trie1.rootNode match {
            case MerklePatriciaNode.Leaf(_, _data, _digest) => IO.pure((trie1.rootNode.digest, _data, _digest))
            case _ => IO.raiseError(new Exception("unexpected root node found"))
          }
          (root2, data2, digest2) <- trie2.rootNode match {
            case MerklePatriciaNode.Leaf(_, _data, _digest) => IO.pure((trie2.rootNode.digest, _data, _digest))
            case _ => IO.raiseError(new Exception("unexpected root node found"))
          }
        } yield expect(root1 != root2 && data1 != data2 && digest1 != digest2)
    }
  }

  test("create method produces a trie with a known root digest") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leafMap <- (0 to 31).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
      trie <- MerklePatriciaTrie.create[IO, Int](leafMap)
    } yield expect(trie.rootNode.digest == l256.unsafe(Hex.decode("b6514df9316d956f5f99222cd93655a6976abcc3a35c23700f3040681e6b2683")))
  }

  test("create then insert produces a trie with a known root digest") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leafMap <- (0 to 31).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
      trie <- MerklePatriciaTrie.create[IO, Int](leafMap)
      newLeaves <- (-31 to -0).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
      trie2 <- MerklePatriciaTrie.insert(trie, newLeaves)
    } yield expect(trie2.rootNode.digest == l256.unsafe(Hex.decode("c2c3169def5009ec37ed562e30d51c3f47527e428d1b327b4bba8c111b790984")))
  }

  test("create method produces a trie with a single branch and leaves") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      content = (1 to 5) ++ (7 to 9)
      leafMap <- content.toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
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

  test("create method produces a trie with branches, extensions, and leaves") {
    val toDigest = (str: String) => l256.unsafe(Hex.decode(str))
    val toNibbleSeq = (str: String) => str.map(Nibble.unsafe)

    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "a value",
      toDigest("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "another value",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1") -> "yet another value",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2") -> "are we done yet?"
    )

    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      trieActual <- MerklePatriciaTrie.create(leafMap)
      trieExpected <- for {
        leaf1 <- MerklePatriciaNode
          .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), "a value".asJson)
        leaf2 <- MerklePatriciaNode
          .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), "another value".asJson)
        leaf3 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("1"), "yet another value".asJson)
        leaf4 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("2"), "are we done yet?".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x0f: Byte) -> leaf3,
            Nibble.unsafe(0x0d: Byte) -> leaf4
          )
        )
        ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), branch1)
        branch2 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf1,
            Nibble.unsafe(0x01: Byte) -> leaf2,
            Nibble.unsafe(0x0a: Byte) -> ext
          )
        )
      } yield MerklePatriciaTrie(branch2)
    } yield expect(trieActual == trieExpected)
  }
}
