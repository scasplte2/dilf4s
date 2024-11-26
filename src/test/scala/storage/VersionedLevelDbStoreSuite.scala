package storage

import cats.effect.{IO, Resource}
import cats.implicits.toTraverseOps
import weaver.IOSuite
import weaver.scalacheck.Checkers
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.VersionedStore
import xyz.kd5ujc.storage.versioned_store.VersionedLevelDbStore

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.UUID

object VersionedLevelDbStoreSuite extends IOSuite with Checkers {

  // ensure tests are run serially
  override def maxParallelism = 1

  // tests share the same mutable resource
  override type Res = VersionedStore[IO, Int, String]

  override def sharedResource: Resource[IO, Res] =
    for {
      randSuffix                              <- Resource.eval(IO(scala.util.Random.alphanumeric.take(10).mkString))
      tmpDbFile                               <- Resource.make(IO(Files.createTempDirectory(s"leveldb_${randSuffix}")))(deleteRecursively)
      implicit0(json2bin: JsonSerializer[IO]) <- Resource.eval(JsonSerializer.forSync[IO])
      store                                   <- VersionedLevelDbStore.make[IO, Int, String](tmpDbFile)
    } yield store

  private def deleteRecursively(path: Path): IO[Unit] = IO {
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      }
    )
  } *> IO.unit

  test("Insert key-value pairs") { store =>
    val pairs = List((1, "a"), (2, "b"))
    for {
      id     <- IO(UUID.randomUUID())
      result <- store.insert(id, pairs)
    } yield expect(result)
  }

  test("Get value by key after insertion") { store =>
    val pairs = List((3, "c"), (4, "d"))
    for {
      id      <- IO(UUID.randomUUID())
      _       <- store.insert(id, pairs)
      results <- pairs.traverse { case (k, _) => store.get(k) }
    } yield expect.all(results.forall(_.isDefined))
  }

  test("Key not present in store after removal") { store =>
    val pairs = List((7, "g"), (8, "h"))
    val uniqueKey = pairs.map(_._1).max + 1
    for {
      id1    <- IO(UUID.randomUUID())
      _      <- store.insert(id1, (uniqueKey, "i") :: pairs)
      id2    <- IO(UUID.randomUUID())
      _      <- store.remove(id2, List(uniqueKey))
      result <- store.get(uniqueKey)
    } yield expect(result.isEmpty)
  }

  test("Retrieve diff value by version ID") { store =>
    val pairs = List((9, "j"), (10, "k"))
    for {
      id1 <- IO(UUID.randomUUID())
      _   <- store.insert(id1, pairs)
      id2 <- IO(UUID.randomUUID())
      pairs2 = pairs.map { case (k, v) => (k, v + "_updated") }
      _       <- store.insert(id2, pairs2)
      results <- pairs.traverse { case (k, v) => store.getFromVersionDiff(k, id2).map(_.contains(v)) }
    } yield expect.all(results.forall(identity))
  }

  test("Updating a value for a key in a new version") { store =>
    val pairs = List((11, "l"), (12, "m"))
    for {
      id1 <- IO(UUID.randomUUID())
      _   <- store.insert(id1, pairs)
      id2 <- IO(UUID.randomUUID())
      updatedPairs = pairs.map { case (k, _) => (k, (k + 96).toChar.toString + "_updated") }
      _       <- store.insert(id2, updatedPairs)
      results <- updatedPairs.traverse { case (k, v) => store.get(k).map(_.contains(v)) }
    } yield expect.all(results.forall(identity))
  }

  test("Rollback to previous version after multiple updates") { store =>
    val initialPairs = List((13, "n"), (14, "o"))
    val additionalPairs = List(
      (15, "p"),
      (16, "q"),
      (17, "r"),
      (18, "s")
    )
    for {
      idA <- IO(UUID.randomUUID())
      _   <- store.insert(idA, initialPairs)
      idB <- IO(UUID.randomUUID())
      (removals, _) = initialPairs.splitAt(1)
      _   <- store.update(idB, removals.map(_._1), additionalPairs.slice(0, 1))
      idC <- IO(UUID.randomUUID())
      _   <- store.update(idC, initialPairs.drop(1).map(_._1), additionalPairs.slice(1, 2))
      idD <- IO(UUID.randomUUID())
      _   <- store.update(idD, List(additionalPairs.head._1), additionalPairs.drop(2))
      _   <- store.rollback(idB)
      retrievalResults <- (initialPairs.drop(1).map(_._1) :+ additionalPairs.head._1)
        .traverse(k => store.get(k).map(_.isDefined))
      invalidResults <- (initialPairs.take(1).map(_._1) ++ additionalPairs.drop(1).map(_._1))
        .traverse(k => store.get(k).map(_.isEmpty))
    } yield expect.all((retrievalResults ++ invalidResults).forall(identity))
  }
}
