package xyz.kd5ujc.core

import scala.compiletime.testing.{Error, typeCheckErrors}

import weaver.{Expectations, SimpleIOSuite}

/**
 * The compiler, not the runtime, keeps a canonical map's keys in [[xyz.kd5ujc.core.canonical.KeyOrder.Default]]: outside
 * the canonical package, entries come only from constructors in that order, and no tree in another order passes for
 * them. Each refusal is paired with a control written the same way that compiles, so a typo cannot pass for a refusal.
 * This suite is outside the canonical package on purpose: there its package-private seams are out of reach.
 */
object CanonicalEntriesTypeSuite extends SimpleIOSuite {

  private def refused(what: String, errors: List[Error], fragment: String): Expectations =
    expect(errors.nonEmpty, s"$what compiles").and(
      expect(errors.exists(_.message.contains(fragment)), s"$what: ${errors.map(_.message).mkString("; ")}")
    )

  private def accepted(what: String, errors: List[Error]): Expectations =
    expect(errors.isEmpty, s"$what: ${errors.map(_.message).mkString("; ")}")

  pureTest("a TreeMap in another order cannot pass for entries, and entries cannot be unwrapped to one") {
    refused(
      "a reversed TreeMap as entries",
      typeCheckErrors(
        """val e: xyz.kd5ujc.core.canonical.CanonicalEntries[Int] =
             scala.collection.immutable.TreeMap("a" -> 1)(using Ordering.String.reverse)"""
      ),
      "CanonicalEntries"
    )
      .and(
        accepted(
          "entries built by their constructors",
          typeCheckErrors(
            """val e: xyz.kd5ujc.core.canonical.CanonicalEntries[Int] =
                 xyz.kd5ujc.core.canonical.CanonicalEntries.empty[Int].updated("a", 1)"""
          )
        )
      )
      .and(
        refused(
          "a reversed TreeMap encoded as a text map",
          typeCheckErrors(
            """xyz.kd5ujc.core.canonical.Codec.textMap(xyz.kd5ujc.core.canonical.Codec.uint)
                 .encode(scala.collection.immutable.TreeMap("a" -> BigInt(1))(using Ordering.String.reverse))"""
          ),
          "CanonicalEntries"
        )
      )
      .and(
        accepted(
          "entries encoded as a text map",
          typeCheckErrors(
            """xyz.kd5ujc.core.canonical.Codec.textMap(xyz.kd5ujc.core.canonical.Codec.uint)
                 .encode(xyz.kd5ujc.core.canonical.CanonicalEntries.empty[BigInt].updated("a", BigInt(1)))"""
          )
        )
      )
      .and(
        refused(
          "entries unwrapped to their TreeMap",
          typeCheckErrors(
            """val t: scala.collection.immutable.TreeMap[String, Int] =
                 xyz.kd5ujc.core.canonical.CanonicalEntries.empty[Int]"""
          ),
          "TreeMap"
        )
      )
      .and(
        refused(
          "a key set unwrapped to its SortedSet",
          typeCheckErrors(
            """val s: scala.collection.immutable.SortedSet[String] = xyz.kd5ujc.core.canonical.CanonicalKeys.empty"""
          ),
          "SortedSet"
        )
      )
  }

  pureTest("no other key order can be made or passed outside the canonical package") {
    refused(
      "a new KeyOrder",
      typeCheckErrors(
        """new xyz.kd5ujc.core.canonical.KeyOrder {
             def compare(a: String, b: String): Int = b.compareTo(a)
             def measure(key: String): Long = 0L
           }"""
      ),
      "KeyOrder"
    )
      .and(
        accepted(
          "the default order, read",
          typeCheckErrors("""val o: Ordering[String] = xyz.kd5ujc.core.canonical.KeyOrder.Default""")
        )
      )
      .and(
        refused(
          "building entries in a given order",
          typeCheckErrors(
            """xyz.kd5ujc.core.canonical.CanonicalEntries.inserted(List("a" -> 1), xyz.kd5ujc.core.canonical.KeyOrder.Default)"""
          ),
          "inserted"
        )
      )
      .and(
        refused(
          "a map built in a given order",
          typeCheckErrors(
            """xyz.kd5ujc.core.canonical.CanonicalValue.map(List("a" -> xyz.kd5ujc.core.canonical.CanonicalValue.True),
                 xyz.kd5ujc.core.canonical.KeyOrder.Default)"""
          ),
          // the overload taking an order is invisible here: only the one taking entries is offered
          "Required: Iterable"
        )
      )
      .and(
        accepted(
          "a map built in the default order",
          typeCheckErrors(
            """xyz.kd5ujc.core.canonical.CanonicalValue.map(List("a" -> xyz.kd5ujc.core.canonical.CanonicalValue.True))"""
          )
        )
      )
  }
}
