/*
rules = [NoHashing]
 */
package fixtures

import scala.collection.immutable.{SortedSet, TreeMap, TreeSet}

import xyz.kd5ujc.core.canonical.{CanonicalEntries, KeyOrder}

/** Every way of hashing a key found so far: each marked line must be refused. */
object Bypasses {

  def vectorMap(xs: List[(String, Int)]): Int =
    scala.collection.immutable.VectorMap // assert: NoHashing
      .from(xs) // assert: NoHashing
      .size

  def treeSeqMap(xs: List[(String, Int)]): Int =
    TreeMap
      .from(
        scala.collection.immutable.TreeSeqMap // assert: NoHashing
          .from(xs) // assert: NoHashing
      )(using Ordering.String)
      .size

  def combined(xs: Vector[String]): Int =
    xs.combinations(1).size // assert: NoHashing

  def typedMap: Int =
    Map[String, Int]("a" -> 1).size // assert: NoHashing

  def typedSet: Int =
    Set[String]("a").size // assert: NoHashing

  def infixDistinctBy(xs: List[(String, Int)]): Int =
    xs.distinctBy(_._1).size // assert: NoHashing

  def infixDiff(xs: List[String], ys: List[String]): Int =
    xs.diff(ys).size // assert: NoHashing

  def toMapCall(xs: List[(String, Int)]): Int =
    xs.toMap.size // assert: NoHashing

  def hashCodeCall(s: String): Int =
    s.hashCode // assert: NoHashing

  def hashHash(s: String): Int =
    s.## // assert: NoHashing

  def hashtable: Int =
    new java.util.Hashtable[String, Int]().size // assert: NoHashing

  def properties: Int =
    new java.util.Properties().size // assert: NoHashing

  def concurrentHashMap: Int =
    new java.util.concurrent.ConcurrentHashMap[String, Int]().size // assert: NoHashing

  def unsorted(xs: TreeSet[String]): Int =
    xs.unsorted.size // assert: NoHashing

  def interned(s: String): String =
    s.intern // assert: NoHashing

  final case class Wrapped(key: String)

  // a sorted set cannot be mapped to a type with no ordering (the compiler refuses) unless made unsorted first
  def treeSetMap(xs: TreeSet[String]): Int =
    xs.unsorted // assert: NoHashing
      .map(Wrapped(_))
      .size

  def keysMapped(e: CanonicalEntries[Int]): Int =
    e.keys.distinct.size // assert: NoHashing

  def typePattern(v: Any): Int = v match {
    case t: TreeMap[?, ?] => t.size // assert: NoHashing
    case _                => 0
  }

  def sortedPattern(v: Any): Int = v match {
    case s: SortedSet[?] => s.size // assert: NoHashing
    case _               => 0
  }

  def cast(v: Any): Int =
    v.asInstanceOf[TreeMap[String, Int]].size // assert: NoHashing

  def hashedValue: Int = {
    val m: scala.collection.mutable.HashMap[String, Int] = // assert: NoHashing
      scala.collection.mutable.HashMap // assert: NoHashing
        .empty // assert: NoHashing
    m.size // assert: NoHashing
  }

  def grouped(xs: List[String]): Int =
    xs.groupBy(_.length).size // assert: NoHashing

  def toSetCall(xs: List[String]): Int =
    xs.toSet.size // assert: NoHashing

  def murmur(s: String): Int =
    scala.util.hashing.MurmurHash3.stringHash(s) // assert: NoHashing

  def objectsHash(s: String): Int =
    java.util.Objects.hash(s) // assert: NoHashing

  def instanceTest(v: Any): Boolean =
    v.isInstanceOf[TreeSet[?]] // assert: NoHashing

  def ordered(keys: List[String]): Int = TreeSet.from(keys)(using KeyOrder.Default).size
}
