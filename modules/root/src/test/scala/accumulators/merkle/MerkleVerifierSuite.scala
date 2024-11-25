package accumulators.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import xyz.kd5ujc.accumulators.merkle.api.{MerkleProver, MerkleVerifier}
import xyz.kd5ujc.accumulators.merkle.{MerkleNode, MerkleTree}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.Blake2b256Hasher

import generators.nonEmptyStringListGen
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleVerifierSuite extends SimpleIOSuite with Checkers {

  test("Check whether a valid proof is verified successfully") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(str => MerkleNode.Leaf(str.asJson))
        tree                                    <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        verifier = MerkleVerifier.make[IO](tree.rootNode.digest)
        proof   <- prover.fromLeafNode(leaves.head)
        outcome <- proof.traverse(verifier.isValid)
      } yield expect(outcome.getOrElse(false))
    }
  }

  test("Check that a valid proof does NOT verify against another tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.traverse(str => MerkleNode.Leaf(str.asJson))
        tree1                                   <- MerkleTree.create[IO, String](strings)
        tree2                                   <- MerkleTree.create[IO, String](List("a", "b", "c"))
        prover1 = MerkleProver.make[IO](tree1)
        verifier2 = MerkleVerifier.make[IO](tree2.rootNode.digest)
        proof   <- prover1.fromLeafNode(leaves.head)
        outcome <- proof.traverse(verifier2.isValid)
      } yield expect(outcome.nonEmpty) && expect(!outcome.get)
    }
  }
}
