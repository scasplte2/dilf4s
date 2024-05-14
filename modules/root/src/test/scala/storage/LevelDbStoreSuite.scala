package storage

import LevelDbStoreSuite.{Res, makeRandomStore}
import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.implicits.catsSyntaxOptionId
import generators.{kvGen, kvListGenUniqueKeys}
import org.scalacheck.Gen
import storage.RefMapStoreSuite.{expect, storeIO}
import weaver.{Expectations, IOSuite, SimpleIOSuite}
import weaver.scalacheck.Checkers
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.algebras.Store
import xyz.kd5ujc.storage.interpreters.store.LevelDbStore

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import scala.concurrent.duration.DurationInt

object LevelDbStoreSuite extends SimpleIOSuite with Checkers {

  private def deleteRecursively(path: Path): IO[Unit] = IO {
    Files.walkFileTree(path, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  } *> IO.unit

  private def makeRandomStore: Resource[IO, Store[IO, Int, String]] = {
    for {
      randSuffix <- Resource.eval(IO(scala.util.Random.alphanumeric.take(10).mkString))
      timestamp <- Resource.eval(IO(java.time.Instant.now().toEpochMilli.toString))
      tmpDbFile <- Resource.make(IO(Files.createTempDirectory(s"leveldb_${timestamp}_$randSuffix")))(deleteRecursively)
      implicit0(json2bin: JsonSerializer[IO]) <- Resource.eval { JsonSerializer.forSync[IO] }
      store <- LevelDbStore.make[IO, Int, String](tmpDbFile)
    } yield store
  }

  test("remove a value") {
    makeRandomStore.use { store =>
      for {
        _ <- store.put(1, "A")
        _ <- store.remove(1)
        value <- store.get(1)
      } yield expect(value.isEmpty)
    }
  }

//  test("check if contains a value") {
//    makeRandomStore.use { store =>
//      for {
//        _ <- store.put(1, "A")
//        contains <- store.contains(1)
//      } yield expect(contains)
//    }
//  }
//
//  test("get values with filter") {
//    makeRandomStore.use { store =>
//      for {
//        _ <- store.put(1, "A")
//        _ <- store.put(2, "B")
//        values <- store.getWithFilter((_, v) => v == "A")
//        expected = List((1, "A"))
//      } yield expect(values == expected)
//    }
//  }
//
//  test("getOrRaise value") {
//    makeRandomStore.use { store =>
//      for {
//        _ <- store.put(1, "A")
//        value <- store.getOrRaise(1)
//      } yield expect(value == "A")
//    }
//  }
//
//  test("getOrRaise value when value does not exist") {
//    makeRandomStore.use { store =>
//      for {
//        result <- store.getOrRaise(42).attempt
//      } yield {
//        result match {
//          case Left(e: NoSuchElementException) => expect(e.getMessage == s"Element not found. id=42")
//          case _ => failure("Exception not caught")
//        }
//      }
//    }
//  }
//
//  test("dump values") {
//    makeRandomStore.use { store =>
//      for {
//        _ <- store.put(1, "A")
//        _ <- store.put(2, "B")
//        values <- store.dump
//        expected = List((1, "A"), (2, "B"))
//      } yield expect(values == expected)
//    }
//  }
//
//  test("put and get a key value pair") {
//    makeRandomStore.use { store =>
//      forall(kvListGenUniqueKeys(2).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
//
//        val (keys, _) = kvPairs.toList.unzip
//        for {
//          _ <- IO.println(s"Got pairs: $kvPairs")
//          _ <- store.putBatch(kvPairs.toList)
//          actual <- store.get(keys).map(_.flatMap { case (i, maybeStr) => maybeStr.map(str => (i, str)) })
//          expected = kvPairs.toList
//          _ <- store.removeBatch(keys)
//        } yield expect(actual == expected)
//      }
//    }
//  }

//  test("delete a key") {
//    makeRandomStore.use { store =>
//      forall(kvListGenUniqueKeys(2).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
//        for {
//          _ <- kvPairs.traverse(pair => store.put(pair._1, pair._2))
//          actual <- kvPairs.traverse(pair => store.get(pair._1))
//          expected = kvPairs.map(_._2).toList
//        } yield expect(actual.toList.flatten == expected)
//      }
//    }
//  }
}