package xyz.kd5ujc.jlvm

import scala.collection.Searching

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * How the evaluator orders what a program chooses: binding names (the `let` scope while compiling, the `$` bindings
 * while evaluating) and values (`unique`, `intersect`). Nothing a program chooses is hashed, where values made to
 * collide could slow a lookup; it is compared, through these orders and nothing else. A value is compared by its sort
 * key ([[Values.sortKey]], which encodes an array or a map once), made by `key`. Production uses [[Kernels.Default]];
 * a test passes orders and a key that count their calls, which bounds the evaluator's work on adversarial input
 * without timing it.
 */
private[jlvm] final case class Kernels(
  names:  Ordering[String],
  values: Ordering[Values.SortKey],
  key:    CanonicalValue => Values.SortKey,
  dedup:  Dedup
)

private[jlvm] object Kernels {

  /** UTF-16 order on names, the canonical value order, and deduplication by sorting. */
  val Default: Kernels = Kernels(Ordering.String, Values.sortKeyOrder, Values.sortKey, Dedup.Sorting)
}

/** Deduplication and subset tests that use only an order on their elements: never `hashCode` or `equals`. */
private[jlvm] trait Dedup {

  /** `vs` without repeats, keeping each value's first occurrence, in order. */
  def distinct[A](vs: Vector[A])(using order: Ordering[A]): Vector[A]

  /** Whether every element of `xs` is in `ys`: `xs` is consumed only up to its first element missing from `ys`. */
  def subset[A](xs: Iterator[A], ys: Vector[A])(using order: Ordering[A]): Boolean
}

private[jlvm] object Dedup {

  /** A stable sort and a scan, O(n log n) comparisons; a sort and a binary search per element, O((m + n) log n). */
  object Sorting extends Dedup {

    def distinct[A](vs: Vector[A])(using order: Ordering[A]): Vector[A] = {
      val sorted = vs.zipWithIndex.sortBy(_._1)
      val firsts = sorted.zip(None +: sorted.map(Option(_))).collect {
        case ((e, i), previous) if !previous.exists(p => order.equiv(p._1, e)) => i
      }
      firsts.sorted.map(vs)
    }

    def subset[A](xs: Iterator[A], ys: Vector[A])(using order: Ordering[A]): Boolean = {
      val sorted = ys.sorted
      xs.forall(x =>
        sorted.search(x) match {
          case Searching.Found(_)          => true
          case Searching.InsertionPoint(_) => false
        }
      )
    }
  }
}
