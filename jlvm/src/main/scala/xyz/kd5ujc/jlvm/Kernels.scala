package xyz.kd5ujc.jlvm

import scala.collection.Searching

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * How the parser, the compiler and the evaluator order what a program chooses, and how their work is counted: binding
 * names (the `let` scope while compiling, the `$` bindings while evaluating), object keys (a map expression's fields),
 * and values (`unique`, `intersect`). Nothing a program chooses is hashed, where values made to collide could slow a
 * lookup; it is compared, through these orders and nothing else. A value is compared by its sort key
 * ([[Values.sortKey]], which encodes an array or a map once), made by `key`. `steps` is told of each expression the
 * parser, [[Program.from]] and the compiler visit. Production uses [[Kernels.Default]]; a test passes orders, a key
 * and steps that count their calls, which bounds the work on adversarial input without timing it.
 */
private[jlvm] final case class Kernels(
  names:  Ordering[String],
  keys:   Ordering[String],
  values: Ordering[Values.SortKey],
  key:    CanonicalValue => Values.SortKey,
  dedup:  Dedup,
  steps:  Steps
)

private[jlvm] object Kernels {

  /** Code-point order on object keys (the canonical order: UTF-8 byte order). */
  val CodePoints: Ordering[String] = (a, b) => Values.compareCodePoints(a, b)

  /** UTF-16 order on names, code-point order on keys, the canonical value order, sorting, and uncounted steps. */
  val Default: Kernels =
    Kernels(Ordering.String, CodePoints, Values.sortKeyOrder, Values.sortKey, Dedup.Sorting, Steps.Uncounted)
}

/** Told of each unit of parse and compile work: an expression visited by one pass. */
private[jlvm] trait Steps {
  def step(): Unit
}

private[jlvm] object Steps {

  /** Production: nothing is counted, and nothing happens. */
  object Uncounted extends Steps {
    def step(): Unit = ()
  }
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
