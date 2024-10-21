package accumulators.merkle

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
  // ------------------------------------------
  // NodeJS example
  // ------------------------------------------
  //  const blake = require('blakejs');
  //
  //  function computeHash(data, prefix) {
  //    const context = blake.blake2bInit(32, null);
  //    blake.blake2bUpdate(context, prefix);
  //    blake.blake2bUpdate(context, data);
  //    return Buffer.from(blake.blake2bFinal(context))
  //  }
  //
  //  const leafPrefix = Buffer.from([0x00]);
  //  const internalPrefix = Buffer.from([0x01]);
  //
  //  const left = { "a": 1 };
  //  const leftBinary = Buffer.from(JSON.stringify(left));
  //  const leftDigest = computeHash(leftBinary, leafPrefix);
  //
  //  const right = { "b": 2 };
  //  const rightBinary = Buffer.from(JSON.stringify(right));
  //  const rightDigest = computeHash(rightBinary, leafPrefix);
  //
  //  const internalHashable = {
  //    "leftDigest": leftDigest.toString('hex'),
  //    "rightDigest": rightDigest.toString('hex')
  //  };
  //  const internalBinary = Buffer.from(JSON.stringify(internalHashable));
  //  const internalDigest = computeHash(internalBinary, internalPrefix)
  //
  //  console.log(internalDigest.toString('hex'))

  test("ensure root of MerkleTree matches expected value for fixed data") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      tree <- MerkleTree.create[IO, Json, l256](List(Json.obj("a" -> 1.asJson), Json.obj("b" -> 2.asJson)))
      expectedRootHash = l256.unsafe(Hex.decodeStrict("b1784feac2b405fc80b56d001f4e2364bc1dec7678622cfdba15b4be3902e873"))
    } yield expect.same(tree.rootNode.digest.asJson, expectedRootHash.asJson)
  }
}
