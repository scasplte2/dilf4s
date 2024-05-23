package storage

import cats.data.NonEmptyList
import cats.effect.IO

import xyz.kd5ujc.storage.algebras.Store
import xyz.kd5ujc.storage.interpreters.store.RefMapStore

import generators.kvListGenUniqueKeys
import weaver._
import weaver.scalacheck.Checkers

object RefMapStoreSuite extends SimpleIOSuite with Checkers {

  private val storeIO: IO[Store[IO, Int, String]] = RefMapStore.make[IO, Int, String]

  test("put and get a value") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store  <- storeIO
        _      <- kvPairs.traverse(pair => store.put(pair._1, pair._2))
        actual <- kvPairs.traverse(pair => store.get(pair._1))
        expected = kvPairs.map(_._2).toList
      } yield expect(actual.toList.flatten == expected)
    }
  }

  test("get multiple values") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store  <- storeIO
        _      <- kvPairs.traverse(pair => store.put(pair._1, pair._2))
        actual <- store.get(kvPairs.map(_._1).toList)
        expected = kvPairs.map(pair => (pair._1, Some(pair._2))).toList
      } yield expect(actual == expected)
    }
  }

  test("remove a value") {
    for {
      store <- storeIO
      _     <- store.put(1, "A")
      _     <- store.remove(1)
      value <- store.get(1)
    } yield expect(value.isEmpty)
  }

  test("put and remove batch values") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store              <- storeIO
        _                  <- store.putBatch(kvPairs.toList)
        valuesBeforeDelete <- store.get(kvPairs.map(_._1).toList)
        _                  <- store.removeBatch(kvPairs.map(_._1).toList)
        valuesAfterDelete  <- store.get(kvPairs.map(_._1).toList)
        expectedBeforeDelete = kvPairs.map(pair => pair._1 -> Some(pair._2)).toList
        expectedAfterDelete = kvPairs.map(pair => pair._1 -> None).toList
      } yield expect(valuesBeforeDelete == expectedBeforeDelete).and(expect(valuesAfterDelete == expectedAfterDelete))
    }
  }

  test("check if contains a value") {
    for {
      store    <- storeIO
      _        <- store.put(1, "A")
      contains <- store.contains(1)
    } yield expect(contains)
  }

  test("get values with filter") {
    for {
      store  <- storeIO
      _      <- store.put(1, "A")
      _      <- store.put(2, "B")
      values <- store.getWithFilter((_, v) => v == "A")
      expected = List((1, "A"))
    } yield expect(values == expected)
  }

  test("getOrRaise value") {
    for {
      store <- storeIO
      _     <- store.put(1, "A")
      value <- store.getOrRaise(1)
    } yield expect(value == "A")
  }

  test("getOrRaise value when value does not exist") {
    for {
      store  <- storeIO
      result <- store.getOrRaise(42).attempt
    } yield
      result match {
        case Left(e: NoSuchElementException) => expect(e.getMessage == s"Element not found. id=42")
        case _                               => failure("Exception not caught")
      }
  }

  test("dump values") {
    for {
      store  <- storeIO
      _      <- store.put(1, "A")
      _      <- store.put(2, "B")
      values <- store.dump
      expected = List((1, "A"), (2, "B"))
    } yield expect(values == expected)
  }
}
