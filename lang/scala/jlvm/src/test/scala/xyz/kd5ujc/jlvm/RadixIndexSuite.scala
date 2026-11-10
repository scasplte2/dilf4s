package xyz.kd5ujc.jlvm

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object RadixIndexSuite extends SimpleIOSuite with Checkers {
  private def key(kind: Int, bytes: Vector[Int]): RadixIndex.Key = RadixIndex.Key(kind, bytes.size.toLong)(i => bytes(i.toInt))

  pureTest("empty payloads, kind tags and zero bytes remain distinct, with exact traversal counts") {
    val a = key(0, Vector.empty)
    val b = key(1, Vector.empty)
    val first = RadixIndex.empty.added(a)
    val duplicate = first.index.added(a)
    val second = first.index.added(b)
    val zero = key(0, Vector(0))
    expect
      .same(true, first.added)
      .and(expect.same(RadixIndex.Work(0, 0), first.work))
      .and(expect.same(false, duplicate.added))
      .and(expect.same(RadixIndex.Work(18, 0), duplicate.work))
      .and(expect.same(true, second.added))
      .and(expect.same(RadixIndex.Work(19, 0), second.work))
      .and(expect.same((true, RadixIndex.Work(19, 1)), second.index.contains(b)))
      .and(expect.same(false, second.index.contains(zero)._1))
      .and(expect.same(true, second.index.added(zero).index.contains(zero)._1))
      .and(expect.same(false, first.index.contains(b)._1))
  }

  pureTest("a short absent probe does not traverse a long-key suffix") {
    val prefix = Vector.fill(4096)(0)
    val index = (0 until 256).foldLeft(RadixIndex.empty)((set, n) => set.added(key(4, prefix :+ n)).index)
    val short = key(4, Vector.empty)
    val (found, work) = index.contains(short)
    val inserted = index.added(short)
    expect
      .same(false, found)
      .and(expect.same(1L, work.branches))
      .and(expect(work.bytes <= 2 * short.length, s"short probe read ${work.bytes} bytes"))
      .and(expect.same(true, inserted.added))
      .and(expect.same(true, inserted.index.contains(short)._1))
      .and(expect.same(false, index.contains(short)._1))
      .and(expect(inserted.work.bytes <= 18 * short.length + 1, s"short insertion read ${inserted.work.bytes} bytes"))
  }

  private val payload: Gen[(Int, Vector[Int])] = for {
    kind   <- Gen.choose(0, 7)
    length <- Gen.choose(0, 32)
    bytes  <- Gen.listOfN(length, Gen.choose(0, 255))
  } yield (kind, bytes.toVector)

  test("insertion and membership agree with exact keys, within a linear per-key work bound") {
    forall(Gen.choose(0, 80).flatMap(Gen.listOfN(_, payload))) { inputs =>
      val (index, distinct, checks) = inputs.foldLeft((RadixIndex.empty, Vector.empty[(Int, Vector[Int])], success)) {
        case ((set, seen, previous), item @ (kind, bytes)) =>
          val k = key(kind, bytes)
          val inserted = set.added(k)
          val (found, lookup) = inserted.index.contains(k)
          val added = !seen.contains(item)
          val next = if (added) seen :+ item else seen
          val check = expect
            .same(added, inserted.added)
            .and(expect.same(true, found))
            .and(expect(inserted.work.bytes <= 18 * k.length + 1, s"insertion byte reads ${inserted.work.bytes}, key length ${k.length}"))
            .and(expect(inserted.work.branches <= 16 * k.length + 1, s"insertion branch visits ${inserted.work.branches}"))
            .and(expect(lookup.bytes <= 10 * k.length, s"lookup byte reads ${lookup.bytes}, key length ${k.length}"))
            .and(expect(lookup.branches <= 8 * k.length + 1, s"lookup branch visits ${lookup.branches}"))
          (inserted.index, next, previous.and(check))
      }
      checks.and(
        expect(distinct.forall { case (kind, bytes) => index.contains(key(kind, bytes))._1 }, "every inserted key remains present")
      )
    }
  }
}
