package storage

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.algebras.Store
import xyz.kd5ujc.storage.interpreters.store.LevelDbStore

import generators.kvListGenUniqueKeys
import weaver.IOSuite
import weaver.scalacheck.Checkers

object LevelDbStoreSuite extends IOSuite with Checkers {

  override type Res = Store[IO, Int, String]

  override def sharedResource: Resource[IO, Res] =
    for {
      randSuffix                              <- Resource.eval(IO(scala.util.Random.alphanumeric.take(10).mkString))
      tmpDbFile                               <- Resource.make(IO(Files.createTempDirectory(s"leveldb_${randSuffix}")))(deleteRecursively)
      implicit0(json2bin: JsonSerializer[IO]) <- Resource.eval(JsonSerializer.forSync[IO])
      store                                   <- LevelDbStore.make[IO, Int, String](tmpDbFile)
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

  test("Add and remove a single value") { store =>
    for {
      _      <- store.put(1, "A")
      value1 <- store.get(1)
      _      <- store.remove(1)
      value2 <- store.get(1)
    } yield expect(value1.contains("A") && value2.isEmpty)
  }

  test("check if contains a value") { store =>
    for {
      _        <- store.put(2, "B")
      contains <- store.contains(2)
    } yield expect(contains)
  }

  test("get values with filter") { store =>
    for {
      _      <- store.putBatch(List((3, "C"), (4, "D")))
      values <- store.getWithFilter((_, v) => v == "C")
      expected = List((3, "C"))
    } yield expect(values == expected)
  }

  test("getOrRaise value") { store =>
    for {
      _     <- store.put(5, "E")
      value <- store.getOrRaise(5)
    } yield expect(value == "E")
  }

  test("get values with filter") { store =>
    for {
      _        <- store.putBatch(List((6, "F"), (7, "G")))
      values   <- store.get(List(6, 7))
      filtered <- store.getWithFilter((_, v) => v == "F")
    } yield expect(values.toSet == Set((6, Some("F")), (7, Some("G")))) && expect(filtered == List((6, "F")))
  }

  test("getOrRaise value when value does not exist") { store =>
    for {
      result <- store.getOrRaise(42).attempt
    } yield
      result match {
        case Left(e: NoSuchElementException) => expect(e.getMessage == s"Element not found. id=42")
        case _                               => failure("Exception not caught")
      }
  }

  test("put and get a batch of key value pairs") { store =>
    for {
      kvPairs <- IO.fromOption(kvListGenUniqueKeys(3).sample.map(NonEmptyList.fromListUnsafe))(
        new RuntimeException("Failed to generate key-value list")
      )
      (keys, _) = kvPairs.toList.unzip
      _      <- store.putBatch(kvPairs.toList)
      actual <- store.get(keys).map(_.flatMap { case (i, maybeStr) => maybeStr.map(str => (i, str)) })
      expected = kvPairs.toList
    } yield expect(actual == expected)
  }

  test("delete a batch of keys") { store =>
    for {
      kvPairs <- IO.fromOption(kvListGenUniqueKeys(2).sample.map(NonEmptyList.fromListUnsafe))(
        new RuntimeException("Failed to generate key-value list")
      )
      (keys, _) = kvPairs.toList.unzip
      actual <- store.get(keys).map(_.flatMap { case (i, maybeStr) => maybeStr.map(str => (i, str)) })
    } yield expect(actual.isEmpty)
  }
}
