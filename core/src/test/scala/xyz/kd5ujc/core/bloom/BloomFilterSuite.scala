package xyz.kd5ujc.core.bloom

import scala.util.Random

import xyz.kd5ujc.core.hash.Sha256

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object BloomFilterSuite extends SimpleIOSuite with Checkers {

  private val p = BloomParams.Ledger2048
  private val item: Gen[ByteVector] = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val items: Gen[List[ByteVector]] = Gen.listOf(item).map(_.take(200))
  implicit private val showItems: cats.Show[List[ByteVector]] = cats.Show.show(_.map(_.toHex).mkString(","))
  implicit private val showItem: cats.Show[ByteVector] = cats.Show.show(_.toHex)
  implicit private val showTwo: cats.Show[(List[ByteVector], List[ByteVector])] = cats.Show.fromToString

  test("no false negatives") {
    forall(items) { xs =>
      val f = BloomFilter.of[Sha256](p, xs)
      expect(xs.forall(f.contains), "every inserted item must be reported")
    }
  }

  test("each item sets exactly k distinct bits") {
    forall(item)(x => expect.same(p.hashes.toLong, BloomFilter.empty[Sha256](p).add(x).setBits))
  }

  test("union is the filter of the union; insertion order is irrelevant") {
    forall(Gen.zip(items, items)) {
      case (a, b) =>
        val (fa, fb) = (BloomFilter.of[Sha256](p, a), BloomFilter.of[Sha256](p, b))
        expect
          .same(Right(BloomFilter.of[Sha256](p, a ++ b)), fa.union(fb))
          .and(expect.same(BloomFilter.of[Sha256](p, a ++ b), BloomFilter.of[Sha256](p, (a ++ b).reverse)))
    }
  }

  pureTest("the false-positive rate matches theory (n=500, m=2048, k=4)") {
    val rnd = new Random(20201121L)
    val inserted = List.fill(500)(ByteVector(rnd.nextBytes(32)))
    val probes = List.fill(20000)(ByteVector(rnd.nextBytes(33)))
    val f = BloomFilter.of[Sha256](p, inserted)
    val rate = probes.count(f.contains).toDouble / probes.size
    val theory = math.pow(1 - math.exp(-4.0 * 500 / 2048), 4) // ~0.151
    expect(math.abs(rate - theory) < 0.02, s"observed $rate vs theoretical $theory")
      .and(expect(math.abs(f.estimatedFalsePositiveRate - theory) < 0.02, s"fill estimate ${f.estimatedFalsePositiveRate}"))
  }

  pureTest("parameters are validated and mismatched filters do not combine") {
    val small = BloomParams(1024, 3).toOption.get
    expect(BloomParams(1000, 4).isLeft, "size must be a power of two")
      .and(expect(BloomParams(2048, 0).isLeft, "at least one hash"))
      .and(expect(BloomFilter.empty[Sha256](p).union(BloomFilter.empty[Sha256](small)).isLeft, "union needs equal params"))
  }

  test("the canonical encoding round-trips") {
    forall(items) { xs =>
      val f = BloomFilter.of[Sha256](p, xs)
      expect.same(Right(f), BloomFilter.decode[Sha256](BloomFilter.encode(f)))
    }
  }

  pureTest("an empty filter contains nothing") {
    expect(!BloomFilter.empty[Sha256](p).contains(ByteVector(1, 2, 3))).and(expect(BloomFilter.empty[Sha256](p).isEmpty))
  }
}
