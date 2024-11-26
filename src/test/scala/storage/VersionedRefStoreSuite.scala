package storage

import cats.effect.IO
import cats.implicits.toTraverseOps
import generators.kvListGenUniqueKeys
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import xyz.kd5ujc.storage.VersionedStore
import xyz.kd5ujc.storage.versioned_store.VersionedRefStore

import java.util.UUID

object VersionedRefStoreSuite extends SimpleIOSuite with Checkers {

  private val storeIO: IO[VersionedStore[IO, Int, String]] = VersionedRefStore.make[IO, Int, String]

  test("Insert key-value pairs") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      for {
        store  <- storeIO
        id     <- IO(UUID.randomUUID())
        result <- store.insert(id, pairs)
      } yield expect(result)
    }
  }

  test("Get value by key after insertion") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      for {
        store   <- storeIO
        id      <- IO(UUID.randomUUID())
        _       <- store.insert(id, pairs)
        results <- pairs.traverse { case (k, _) => store.get(k) }
      } yield expect.all(results.forall(_.isDefined))
    }
  }

  test("Verify latest version ID after insertion") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      for {
        store  <- storeIO
        id     <- IO(UUID.randomUUID())
        _      <- store.insert(id, pairs)
        latest <- store.latest
      } yield expect(latest == id)
    }
  }

  test("Key not present in store after removal") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      val uniqueKey = pairs.map(_._1).max + 1

      for {
        store <- storeIO
        id1   <- IO(UUID.randomUUID())
        value = Gen.alphaNumStr.sample.getOrElse("")
        _      <- store.insert(id1, (uniqueKey, value) :: pairs)
        id2    <- IO(UUID.randomUUID())
        _      <- store.remove(id2, List(uniqueKey))
        result <- store.get(uniqueKey)
      } yield expect(result.isEmpty)
    }
  }

  test("Retrieve diff value by version ID") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      for {
        store <- storeIO
        id1   <- IO(UUID.randomUUID())
        id2   <- IO(UUID.randomUUID())
        _     <- store.insert(id1, pairs)
        pairs2 = pairs.map { case (k, v) => (k, v + 1) }
        _       <- store.insert(id2, pairs2)
        results <- pairs.traverse { case (k, v) => store.getFromVersionDiff(k, id2).map(_.contains(v)) }
      } yield expect.all(results.forall(identity))
    }
  }

  test("Latest version ID reflects the newest insertion") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      for {
        store           <- storeIO
        id1             <- IO(UUID.randomUUID())
        _               <- store.insert(id1, pairs)
        id2             <- IO(UUID.randomUUID())
        _               <- store.insert(id2, pairs)
        latestVersionId <- store.latest
      } yield expect(latestVersionId == id2)
    }
  }

  test("Updating a value for a key in a new version") {
    forall(kvListGenUniqueKeys(10)) { pairs =>
      val updatedPairs = pairs.map { case (k, v) => (k, v + "_updated") }
      for {
        store   <- storeIO
        id1     <- IO(UUID.randomUUID())
        _       <- store.insert(id1, pairs)
        id2     <- IO(UUID.randomUUID())
        _       <- store.insert(id2, updatedPairs)
        results <- updatedPairs.traverse { case (k, v) => store.get(k).map(_.contains(v)) }
      } yield expect.all(results.forall(identity))
    }
  }

  test("Rollback to previous version after multiple updates") {
    // Generate unique keys for testing
    forall(kvListGenUniqueKeys(10)) { initialPairs =>
      // Introduce some new pairs with Int keys and String values that will be added in different states
      val additionalPairs = List(
        (101, "new-value-1"),
        (102, "new-value-2"),
        (103, "new-value-3"),
        (104, "new-value-4")
      )

      // Initiate store and IDs for each state
      for {
        store <- storeIO
        idA   <- IO(UUID.randomUUID())
        idB   <- IO(UUID.randomUUID())
        idC   <- IO(UUID.randomUUID())
        idD   <- IO(UUID.randomUUID())

        // Insert initial pairs at idA
        _ <- store.insert(idA, initialPairs)

        // Perform updates at idB - remove half of the initial pairs and add a new pair
        (removals, _) = initialPairs.splitAt(5)
        _ <- store.update(idB, removals.map(_._1), additionalPairs.slice(0, 1))

        // Perform updates at idC - remove another half of the initial pairs and add a new pair
        _ <- store.update(idC, initialPairs.drop(5).map(_._1), additionalPairs.slice(1, 2))

        // Perform updates at idD - remove the new pair added at idB and add two new pairs
        _ <- store.update(idD, List(additionalPairs.head._1), additionalPairs.drop(2))

        // Perform rollback to idB
        _ <- store.rollback(idB)

        // Verify: All pairs from idB are retrievable while the others are not present
        retrievalResults <- (initialPairs.drop(5).map(_._1) :+ additionalPairs.head._1)
          .traverse(k => store.get(k).map(_.isDefined))
        invalidResults <- (initialPairs.take(5).map(_._1) ++ additionalPairs.drop(1).map(_._1))
          .traverse(k => store.get(k).map(_.isEmpty))
      } yield
        // All original pairs from idB should be retrievable while the removed and new pairs (introduced after idB) should not be present
        expect((retrievalResults ++ invalidResults).forall(identity))
    }
  }
}
