package accumulators

import cats.effect.IO

import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Blake2b256Hasher, l256}

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleTreeSuite extends SimpleIOSuite with Checkers {

  test("ensure root of MerkleTree is non-empty for list of non-empty strings") {
    forall(Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        merkleTree                              <- MerkleTree.create[IO, String, l256](strings)
      } yield expect(merkleTree.rootNode.digest.value.nonEmpty)
    }
  }

  test("the same list of strings produces MerkleTrees with the same root") {
    forall(Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        merkleTree1                             <- MerkleTree.create[IO, String, l256](strings)
        merkleTree2                             <- MerkleTree.create[IO, String, l256](strings)
      } yield expect.same(merkleTree1.asJson, merkleTree2.asJson)
    }
  }

  test("different lists of strings produce MerkleTrees with different roots") {
    val distinctNonEmptyLists: Gen[(List[String], List[String])] = for {
      list1 <- Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))
      list2 <- Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty)).suchThat(_ != list1)
    } yield (list1, list2)

    forall(distinctNonEmptyLists) {
      case (strings1, strings2) =>
        for {
          implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
          implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
          merkleTree1                             <- MerkleTree.create[IO, String, l256](strings1)
          merkleTree2                             <- MerkleTree.create[IO, String, l256](strings2)
        } yield expect(merkleTree1.asJson != merkleTree2.asJson)
    }
  }

  // to replicate behavior in another environment, make sure to use a hash function that allows for incremental updating
  /**
   * NodeJS example
   * ------------------------------------------
   * const blake = require('blakejs');
   *
   * function computeHash(data, prefix) {
   * const context = blake.blake2bInit(32, null);
   * blake.blake2bUpdate(context, prefix);
   * blake.blake2bUpdate(context, data);
   * return Buffer.from(blake.blake2bFinal(context))
   * }
   *
   * // const data = Buffer.from(JSON.stringify({"a":1}));
   * // const data = Buffer.from(JSON.stringify({"b":2}));
   * // const prefix = Buffer.from([0x00])
   *
   * const data = Buffer.from("decc15cf89e2d4b56e2778609a452bcc74f0b73e406f49653a2679f3411aebd11c9ef5a4045965dccaec2c9b476555b7bc9e54035372bf3834d764783de5e830", "hex")
   * const prefix = Buffer.from([0x01])
   *
   * console.log(computeHash(data, prefix));
   */
  test("ensure root of MerkleTree matches expected value for fixed data") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      tree <- MerkleTree.create[IO, Json, l256](List(Json.obj("a" -> 1.asJson), Json.obj("b" -> 2.asJson)))
      expectedRootHash = l256.unsafe(Hex.decodeStrict("01e8eee29abca850a2a0c6a3e8d4465d156abafc937d0b91f6922de99f165b69"))
    } yield expect.same(tree.rootNode.digest.asJson, expectedRootHash.asJson)
  }
}
