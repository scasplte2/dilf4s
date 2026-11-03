package xyz.kd5ujc.core.canonical

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, TreeMap, TreeSet}

/**
 * A canonical map's entries: text keys in [[KeyOrder.Default]], only ever compared, never hashed. Outside this file the
 * type is opaque. Its constructors all build in that order (a test's counting order comes only through the
 * package-private builders below), and the tree beneath, with its ordering, is never handed out, so no entries in
 * another order can reach a canonical map.
 */
opaque type CanonicalEntries[+A] = TreeMap[String, A]

/** A set of text keys, in the order of the entries or the parser that made it; opaque like [[CanonicalEntries]]. */
opaque type CanonicalKeys = SortedSet[String]

object CanonicalEntries {

  def empty[A]: CanonicalEntries[A] = TreeMap.empty(using KeyOrder.Default)

  /** The entries of `pairs`, inserted in order; a repeated key is refused, the first repeat in input order. */
  def from[A](pairs: Iterable[(String, A)]): Either[CanonicalError, CanonicalEntries[A]] = unique(pairs.toList, empty)

  /** No entries, in `keys`'s order: for the package's own seams. */
  private[canonical] def emptyIn[A](keys: KeyOrder): CanonicalEntries[A] = TreeMap.empty(using keys)

  /** `pairs`, each key once, inserted one at a time in input order, in `keys`'s order. */
  private[canonical] def inserted[A](pairs: IterableOnce[(String, A)], keys: KeyOrder): CanonicalEntries[A] =
    TreeMap.from(pairs)(using keys)

  /** `pairs`, each key once, inserted one at a time in input order, in the order of `like`. */
  private[canonical] def insertedLike[A](like: CanonicalEntries[Any], pairs: IterableOnce[(String, A)]): CanonicalEntries[A] =
    TreeMap.from(pairs)(using like.ordering)

  /** The order `e` is in. */
  private[canonical] def ordering(e: CanonicalEntries[Any]): Ordering[String] = e.ordering

  @tailrec
  private def unique[A](pairs: List[(String, A)], acc: TreeMap[String, A]): Either[CanonicalError, CanonicalEntries[A]] =
    pairs match {
      case Nil => Right(acc)
      case (k, v) :: rest =>
        val next = acc.updated(k, v)
        if (next.size == acc.size) Left(CanonicalError.DuplicateKey(k)) else unique(rest, next)
    }

  extension [A](e: CanonicalEntries[A]) {
    def get(key:               String): Option[A] = e.get(key)
    def contains(key:          String): Boolean = e.contains(key)
    def getOrElse[B >: A](key: String, default: => B): B = e.getOrElse(key, default)
    def size: Int = e.size
    def isEmpty: Boolean = e.isEmpty
    def nonEmpty: Boolean = e.nonEmpty
    def iterator: Iterator[(String, A)] = e.iterator
    def keysIterator: Iterator[String] = e.keysIterator
    def valuesIterator: Iterator[A] = e.valuesIterator
    def keys: Iterable[String] = e.keys
    def values: Iterable[A] = e.values
    def toList: List[(String, A)] = e.toList
    def toVector: Vector[(String, A)] = e.toVector
    def foldLeft[B](z: B)(op: (B, (String, A)) => B): B = e.foldLeft(z)(op)
    def forall(p:      ((String, A)) => Boolean): Boolean = e.forall(p)
    def exists(p:      ((String, A)) => Boolean): Boolean = e.exists(p)
    def keySet: CanonicalKeys = e.keySet

    /** These entries with `key` set to `value`, in their own order. */
    def updated[B >: A](key: String, value: B): CanonicalEntries[B] = e.updated(key, value)

    /** These entries without `key`, in their own order. */
    def removed(key: String): CanonicalEntries[A] = e - key

    /** These entries, then `that`'s, later keys winning, in these entries' order. */
    def concat[B >: A](that: CanonicalEntries[B]): CanonicalEntries[B] = e ++ that

    /** The entries `p` keeps, in their own order. */
    def filter(p: ((String, A)) => Boolean): CanonicalEntries[A] = e.filter(p)
  }
}

object CanonicalKeys {

  def empty: CanonicalKeys = TreeSet.empty(using KeyOrder.Default)

  /** No keys, in `keys`'s order: for the package's own seams. */
  private[canonical] def emptyIn(keys: KeyOrder): CanonicalKeys = TreeSet.empty(using keys)

  /** `keys`, in the order of `like`. */
  private[canonical] def like(like: CanonicalEntries[Any], keys: Iterable[String]): CanonicalKeys =
    TreeSet.from(keys)(using CanonicalEntries.ordering(like))

  extension (k: CanonicalKeys) {
    def contains(key: String): Boolean = k.contains(key)
    def size: Int = k.size
    def isEmpty: Boolean = k.isEmpty
    def iterator: Iterator[String] = k.iterator
    def toList: List[String] = k.toList

    /** These keys and `key`, in their own order. */
    def added(key: String): CanonicalKeys = k + key
  }
}
