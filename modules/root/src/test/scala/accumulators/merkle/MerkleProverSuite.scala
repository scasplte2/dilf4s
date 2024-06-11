package accumulators.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import xyz.kd5ujc.accumulators.merkle.MerkleTree
import xyz.kd5ujc.accumulators.merkle.api.MerkleProver
import xyz.kd5ujc.accumulators.merkle.nodes.LeafNode
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.{Blake2b256Hasher, l256}

import generators.nonEmptyStringListGen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleProverSuite extends SimpleIOSuite with Checkers {

  test("Creating a proof is successful using a Leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(LeafNode(_))
        tree                                    <- MerkleTree.create[IO, String, l256](strings)
        prover = MerkleProver.make[IO, l256](tree)
        proof <- prover.fromLeafNode(leaves.head)
      } yield expect(proof.nonEmpty)
    }
  }

  test("Creating a proof is successful using a digest of a leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(LeafNode(_))
        tree                                    <- MerkleTree.create[IO, String, l256](strings)
        prover = MerkleProver.make[IO, l256](tree)
        proof <- prover.fromLeafDigest(leaves.head.digest)
      } yield expect(proof.nonEmpty)
    }
  }

  test("Creating a proof fails when using a Leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(LeafNode(_))
        tree                                    <- MerkleTree.create[IO, String, l256](strings.tail)
        prover = MerkleProver.make[IO, l256](tree)
        proof <- prover.fromLeafNode(leaves.head)
      } yield expect(proof.isEmpty)
    }
  }

  test("Creating a proof fails when using a digest of a leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(LeafNode(_))
        tree                                    <- MerkleTree.create[IO, String, l256](strings.tail)
        prover = MerkleProver.make[IO, l256](tree)
        proof <- prover.fromLeafDigest(leaves.head.digest)
      } yield expect(proof.isEmpty)
    }
  }
}
