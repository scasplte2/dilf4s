package xyz.kd5ujc.jlvm

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

/**
 * How the parser, the compiler and the evaluator order what a program chooses, and how their work is counted: binding
 * names (the `let` scope while compiling, the `$` bindings while evaluating), object keys (a map expression's fields),
 * and values (`unique`, `intersect`). Nothing a program chooses is hashed. Names use the supplied orders; values use
 * a radix index over typed byte keys ([[Values.sortKey]], which encodes each compound value once). `steps` is told
 * of each expression the parser, [[Program.from]] and the compiler visit. Production uses [[Kernels.Default]]; a test passes orders, a key
 * and steps that count their calls, which bounds the work on adversarial input without timing it.
 */
private[jlvm] final case class Kernels(
  names: Ordering[String],
  keys:  Ordering[String],
  key:   CanonicalValue => Either[Cbor.TooLarge, Values.SortKey],
  dedup: Dedup,
  steps: Steps
)

private[jlvm] object Kernels {

  /** Code-point order on object keys (the canonical order: UTF-8 byte order). */
  val CodePoints: Ordering[String] = (a, b) => Values.compareCodePoints(a, b)

  /** UTF-16 order on names, code-point order on keys, radix membership on values, and uncounted steps. */
  val Default: Kernels =
    Kernels(Ordering.String, CodePoints, Values.sortKey, Dedup.Radix, Steps.Uncounted)
}

/** Told of each unit of parse and compile work: an expression visited by one pass. */
private[jlvm] trait Steps {
  def step(): Unit

  /** Told of each span of program text the parser reads back through the strict JSON parser, with its length. */
  def spanned(length: Int): Unit = ()
}

private[jlvm] object Steps {

  /** Production: nothing is counted, and nothing happens. */
  object Uncounted extends Steps {
    def step(): Unit = ()
  }
}

/** First occurrences and subset membership, with the traversal's work available to tests. */
private[jlvm] trait Dedup {
  def distinct(vs:     Vector[Values.SortKey]): (Vector[Values.SortKey], RadixIndex.Work)
  def subset[E, A](xs: Vector[A], key: A => Either[E, Values.SortKey], ys: Vector[Values.SortKey]): Either[E, (Boolean, RadixIndex.Work)]
}

private[jlvm] object Dedup {
  object Radix extends Dedup {
    def distinct(vs: Vector[Values.SortKey]): (Vector[Values.SortKey], RadixIndex.Work) = {
      val (_, reversed, work) = vs.foldLeft((RadixIndex.empty, List.empty[Values.SortKey], RadixIndex.Work.Zero)) {
        case ((index, kept, work), value) =>
          val next = index.added(value.radix)
          (next.index, if (next.added) value :: kept else kept, work + next.work)
      }
      (reversed.reverse.toVector, work)
    }

    def subset[E, A](
      xs:  Vector[A],
      key: A => Either[E, Values.SortKey],
      ys:  Vector[Values.SortKey]
    ): Either[E, (Boolean, RadixIndex.Work)] = {
      val (index, built) = ys.foldLeft((RadixIndex.empty, RadixIndex.Work.Zero)) {
        case ((index, work), value) =>
          val next = index.added(value.radix)
          (next.index, work + next.work)
      }
      @tailrec
      def from(at: Int, work: RadixIndex.Work): Either[E, (Boolean, RadixIndex.Work)] =
        if (at == xs.size) Right((true, work))
        else
          key(xs(at)) match {
            case Left(error) => Left(error)
            case Right(value) =>
              val (found, looked) = index.contains(value.radix)
              if (found) from(at + 1, work + looked) else Right((false, work + looked))
          }
      from(0, built)
    }
  }
}
