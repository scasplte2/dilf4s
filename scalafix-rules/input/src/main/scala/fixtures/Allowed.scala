/*
rules = [NoHashing]
 */
package fixtures

import scala.collection.immutable.{SortedMap, TreeMap, TreeSet}

import xyz.kd5ujc.core.canonical.{CanonicalEntries, CanonicalValue, KeyOrder}

/** Ordered code that never hashes: nothing here may be refused, whatever words it uses. */
object Allowed {

  // a comment may say Map, Set, HashMap, toMap, distinct and hashCode
  val words: String = "Map Set HashMap toMap distinct hashCode"

  def tree(xs: List[(String, Int)]): TreeMap[String, Int] = TreeMap.from(xs)(using KeyOrder.Default)

  def sorted(xs: List[(String, Int)]): SortedMap[String, Int] = tree(xs)

  def treeSetToText(xs: TreeSet[String]): TreeSet[String] = xs.map(_ + "!")

  def treeMapToText(xs: TreeMap[String, Int]): SortedMap[String, Int] = xs.map { case (k, v) => (k + "!", v) }

  def vector(xs: Vector[String]): Vector[Int] = xs.map(_.length)

  def entries(e: CanonicalEntries[CanonicalValue]): Option[CanonicalValue] = e.get("a")

  def keys(e: CanonicalEntries[Int]): Vector[String] = e.keys.map(identity)

  final class Filter {
    def intersect(that: Filter): Filter = that
  }

  def ownIntersect(a: Filter, b: Filter): Filter = a.intersect(b)

  // core's own libraries are read through their Scala 2.13 builds: nothing here is unknown
  def libraries(s: String): Either[String, io.circe.Json] =
    cats.syntax.either.catsSyntaxEitherId(io.circe.Json.fromString(s)).asRight[String]

  def matched(v: Any): Int = v match {
    case s: String => s.length
    case _         => 0
  }
}
