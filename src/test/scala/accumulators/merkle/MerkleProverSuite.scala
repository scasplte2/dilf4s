package accumulators.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import xyz.kd5ujc.accumulators.merkle.api.MerkleProver
import xyz.kd5ujc.accumulators.merkle.{MerkleNode, MerkleTree}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.Blake2b256Hasher

import generators.nonEmptyStringListGen
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleProverSuite extends SimpleIOSuite with Checkers {

  test("Creating a proof is successful using a Leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree                                    <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        proof <- prover.fromLeafNode(leaves.head)
      } yield expect(proof.nonEmpty)
    }
  }

  test("Creating a proof is successful using a digest of a leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree                                    <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        proof <- prover.fromLeafDigest(leaves.head.digest)
      } yield expect(proof.nonEmpty)
    }
  }

  test("Creating a proof fails when using a Leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree                                    <- MerkleTree.create[IO, String](strings.tail)
        prover = MerkleProver.make[IO](tree)
        proof <- prover.fromLeafNode(leaves.head)
      } yield expect(proof.isEmpty)
    }
  }

  test("Creating a proof fails when using a digest of a leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree                                    <- MerkleTree.create[IO, String](strings.tail)
        prover = MerkleProver.make[IO](tree)
        proof <- prover.fromLeafDigest(leaves.head.digest)
      } yield expect(proof.isEmpty)
    }
  }
}
