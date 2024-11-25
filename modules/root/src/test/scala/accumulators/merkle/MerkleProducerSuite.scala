package accumulators.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import xyz.kd5ujc.accumulators.merkle.MerkleNode
import xyz.kd5ujc.accumulators.merkle.api.MerkleProducer
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.Blake2b256Hasher

import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleProducerSuite extends SimpleIOSuite with Checkers {

  test("Building with no leaves throws an error") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      producer                                <- MerkleProducer.make[IO, String](List())
      outcome                                 <- producer.build.attempt
    } yield
      outcome match {
        case Left(expectedError: RuntimeException) => expect(expectedError.getMessage == "Input list must be non-empty")
        case Left(_)                               => failure("Unexpected error type")
        case Right(_)                              => failure("Expecting exception but got successful result")
      }
  }

  test("Building a tree with non-empty list is successful") {
    forall(Gen.listOf(Gen.alphaNumStr).suchThat(_.nonEmpty)) { strings =>
      for {
        implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
        implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
        leaves                                  <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        producer                                <- MerkleProducer.make[IO, String](leaves)
        outcome                                 <- producer.build
      } yield expect(outcome.rootNode.digest.value.nonEmpty)
    }
  }

  test("Appending a value should result in a new tree") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves                                  <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf                                 <- List("five").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer                                <- MerkleProducer.make[IO, String](leaves)
      oldTree                                 <- producer.build
      _                                       <- producer.append(newLeaf)
      newTree                                 <- producer.build
      newLeaves                               <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("one", "two", "three", "four", "five").map(_.asJson))
  }

  test("Prepending a value should result in a new tree") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves                                  <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf                                 <- List("five").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer                                <- MerkleProducer.make[IO, String](leaves)
      oldTree                                 <- producer.build
      _                                       <- producer.prepend(newLeaf)
      newTree                                 <- producer.build
      newLeaves                               <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("five", "one", "two", "three", "four").map(_.asJson))
  }

  test("Updating a value should result in a new tree") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves                                  <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf                                 <- MerkleNode.Leaf("five".asJson)
      producer                                <- MerkleProducer.make[IO, String](leaves)
      oldTree                                 <- producer.build
      _                                       <- producer.update(0, newLeaf)
      newTree                                 <- producer.build
      newLeaves                               <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("five", "two", "three", "four").map(_.asJson))
  }

  test("Removing a value should result in a new tree") {
    for {
      implicit0(json2bin: JsonSerializer[IO]) <- JsonSerializer.forSync[IO]
      implicit0(hasher: Blake2b256Hasher[IO]) <- IO(new Blake2b256Hasher[IO])
      leaves                                  <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer                                <- MerkleProducer.make[IO, String](leaves)
      oldTree                                 <- producer.build
      _                                       <- producer.remove(0)
      newTree                                 <- producer.build
      newLeaves                               <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("two", "three", "four").map(_.asJson))
  }
}
