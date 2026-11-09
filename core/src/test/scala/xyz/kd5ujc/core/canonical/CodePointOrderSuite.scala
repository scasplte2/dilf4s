package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.IO

import scala.annotation.tailrec

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/**
 * A map's entries in code-point order, from the UTF-16 order the map keeps them in: the order JSON renders and the
 * JLVM lists them in, and the CBOR encoder's within each UTF-8 length. The work is counted, never timed: the pairs of
 * UTF-16 units read, against the proof's bound in [[CodePointOrder.counted]], and against the comparison sort it
 * replaced, run through the same count.
 */
object CodePointOrderSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(500)

  private def get[A](e: Either[Any, A]): A = e.fold(err => sys.error(err.toString), identity)

  private def mapOf(keys: Iterable[String]): CanonicalEntries[CanonicalValue] =
    get(CanonicalValue.map(keys.zipWithIndex.map { case (k, i) => k -> CanonicalValue.CInt(i.toLong) })).entries

  /** Code-point order, independently: the keys' UTF-8 bytes, unsigned. */
  private val Utf8Order: Ordering[String] = (a, b) => java.util.Arrays.compareUnsigned(a.getBytes(UTF_8), b.getBytes(UTF_8))

  /** A comparison merge sort that rereads each common prefix, counting unit pairs without mutable counters. */
  private def oldSort(entries: Vector[(String, CanonicalValue)]): (Vector[String], Long) = {
    @tailrec
    def merge(left: List[String], right: List[String], reversed: List[String], reads: Long): (List[String], Long) =
      (left, right) match {
        case (Nil, _) => (reversed.reverse ::: right, reads)
        case (_, Nil) => (reversed.reverse ::: left, reads)
        case (a :: as, b :: bs) =>
          val n = math.min(a.length, b.length)
          val l = (0 until n).find(i => a.charAt(i) != b.charAt(i)).getOrElse(n)
          val total = reads + l.toLong + (if (l < n) 1L else 0L)
          if (CodePointOrder.compare(a, b) < 0) merge(as, right, a :: reversed, total)
          else merge(left, bs, b :: reversed, total)
      }
    def sort(keys: List[String]): (List[String], Long) =
      if (keys.sizeIs <= 1) (keys, 0L)
      else {
        val (left, right) = keys.splitAt(keys.size / 2)
        val (a, ar) = sort(left)
        val (b, br) = sort(right)
        merge(a, b, Nil, ar + br)
      }
    val (sorted, reads) = sort(entries.map(_._1).toList)
    (sorted.toVector, reads)
  }

  /**
   * The proof's bound before rounding to the weight: `2 S + n (ceil(log2 k) + 1)` for `n` keys of `S` UTF-16 units, in
   * `k` runs already in code-point order.
   */
  private def bound(entries: Vector[(String, CanonicalValue)]): Long = {
    val keys = entries.map(_._1)
    val runs = 1 + keys.zip(keys.drop(1)).count { case (a, b) => CodePointOrder.compare(a, b) > 0 }
    val rounds = 64 - java.lang.Long.numberOfLeadingZeros(runs.toLong - 1)
    2L * keys.map(_.length.toLong).sum + keys.size.toLong * (rounds + 1)
  }

  /** The map's weight: one, and each entry's `1 + key UTF-8 length + value weight`. */
  private def weight(entries: Vector[(String, CanonicalValue)]): Long =
    entries.foldLeft(1L) { case (w, (k, v)) => w + 1 + k.getBytes(UTF_8).length + v.weight }

  /**
   * A 256-unit prefix, then every string of 1 to 7 letters of `a`, U+E000 and U+10000: UTF-16 order puts U+10000 (a
   * surrogate pair) before U+E000 and code-point order after it, so under every shared node of the keys' trie the order
   * turns, and the UTF-16 order is a run about every three keys, of mixed lengths.
   */
  private val adversarial: CanonicalEntries[CanonicalValue] = {
    val letters = Vector("a", "\ue000", "\ud800\udc00")
    val suffixes = (1 to 7)
      .foldLeft((Vector(""), Vector.empty[String])) {
        case ((level, all), _) =>
          val next = level.flatMap(s => letters.map(s + _))
          (next, all ++ next)
      }
      ._2
    mapOf(suffixes.map("p" * 256 + _))
  }

  test("an adversarial map is listed in code-point order reading linearly in its weight; the comparison sort reads far more") {
    val entries = adversarial.toVector
    val (listed, reads) = CodePointOrder.counted(entries)
    val (old, oldReads) = oldSort(entries)
    val (b, w) = (bound(entries), weight(entries))
    IO.pure(
      expect
        .same(old, listed.map(_._1))
        .and(expect.same(entries.sortBy(_._1)(using Utf8Order), listed))
        .and(expect(reads <= b, s"$reads unit pairs read, over the proof's bound $b"))
        .and(expect(reads < 10 * w, s"$reads unit pairs read, over 10 W = ${10 * w}"))
        .and(expect(oldReads > 2 * b, s"the comparison sort reads $oldReads unit pairs, not over twice the bound $b"))
    )
  }

  test("keys whose UTF-16 order is their code-point order are one run: one pass, and the entries as they were") {
    val entries = mapOf((0 until 1000).map(i => "p" * 300 + i.toString + "\ud800\udc00" * (i % 3))).toVector
    val (listed, reads) = CodePointOrder.counted(entries)
    val units = entries.map(_._1.length.toLong).sum
    IO.pure(
      expect(listed eq entries, "the entries themselves")
        .and(expect(reads <= units + entries.size, s"$reads unit pairs read in one pass over $units units"))
    )
  }

  /** Code points either side of every UTF-8 width boundary and of the surrogate range, and the empty key. */
  private val keyChar: Gen[String] =
    Gen.oneOf("a", "b", "\u007f", "\u0080", "\u07ff", "\u0800", "\ud7ff", "\ue000", "\uffff", "\ud800\udc00", "\udbff\udfff")

  private val keys: Gen[List[String]] =
    Gen.choose(0, 40).flatMap(n => Gen.listOfN(n, Gen.choose(0, 5).flatMap(l => Gen.listOfN(l, keyChar).map(_.mkString))))

  test("any keys are listed in code-point order, each with its value, within the bound") {
    forall(keys) { ks =>
      val entries = mapOf(ks.distinct).toVector
      val (listed, reads) = CodePointOrder.counted(entries)
      expect
        .same(entries.sortBy(_._1)(using Utf8Order), listed)
        .and(expect(reads <= bound(entries), s"$reads unit pairs read, over ${bound(entries)}"))
    }
  }

  test("any keys, in any order, are put in code-point order: the merge needs no particular input order") {
    forall(keys) { ks =>
      val entries = ks.distinct.toVector.map(k => k -> (CanonicalValue.CNull: CanonicalValue))
      expect.same(entries.sortBy(_._1)(using Utf8Order), CodePointOrder.sorted(entries))
    }
  }

  test("code-point comparison is the order of the UTF-8 bytes") {
    forall(Gen.zip(keys, keys)) {
      case (a, b) =>
        val (x, y) = (a.mkString, b.mkString)
        expect.same(Utf8Order.compare(x, y).sign, CodePointOrder.compare(x, y).sign)
    }
  }
}
