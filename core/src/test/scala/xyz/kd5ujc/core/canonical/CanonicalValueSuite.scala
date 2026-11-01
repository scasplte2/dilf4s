package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8

import xyz.kd5ujc.core.canonical.CanonicalValue._

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CanonicalValueSuite extends SimpleIOSuite with Checkers {

  private def t(s: String): CText = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  private def recount(v: CanonicalValue): Long = v match {
    case CNull | CBool(_) => 1L
    case CInt(i)          => 1L + i.bitLength / 64
    case CRational(r)     => 2L + r.numerator.bitLength / 64 + r.denominator.bitLength / 64
    case CText(s)         => 1L + s.getBytes(UTF_8).length
    case CBytes(b)        => 1L + b.size
    case CArray(vs)       => 1L + vs.map(recount).sum
    case CMap(m)          => 1L + m.toList.map { case (k, x) => 1L + k.getBytes(UTF_8).length + recount(x) }.sum
  }

  test("weight is nodes plus UTF-8 bytes, byte-string lengths and 64-bit limbs") {
    forall(CanonicalGens.value(3))(v => expect.same(recount(v), v.weight))
  }

  pureTest("weight counts shared sub-values every time and saturates instead of overflowing") {
    val doubled = (1 to 80).foldLeft[CanonicalValue](CNull)((v, _) => CanonicalValue.array(Vector(v, v)).toOption.get)
    expect
      .same(Long.MaxValue, doubled.weight)
      .and(expect.same(4L, t("é😀").weight - 3L))
  }

  pureTest("weights match the Rust SDK's (sdk/rust/core/tests/shared_value.rs), including bitLength on negative powers of two") {
    val two = BigInt(2)
    def rational(n: BigInt, d: BigInt) =
      CanonicalValue.number(xyz.kd5ujc.numerics.Ratio.of(n, d).fold(e => sys.error(e.toString), identity)).weight
    def leaf = CanonicalValue.map(List("pk" -> CBytes(scodec.bits.ByteVector.fill(33)(0)), "type" -> t("dlog"))).toOption.get
    expect
      .same(List(1L, 1L, 1L, 6L, 4L), List(CNull.weight, True.weight, t("").weight, t("é€").weight, bytesOf(3).weight))
      .and(expect.same(List(1L, 1L, 2L), List(CInt(0).weight, CInt(two.pow(63) - 1).weight, CInt(two.pow(63)).weight)))
      .and(expect.same(List(1L, 2L), List(CInt(-two.pow(63)).weight, CInt(-two.pow(63) - 1).weight)))
      .and(expect.same(List(2L, 3L), List(CInt(-two.pow(127)).weight, CInt(-two.pow(127) - 1).weight)))
      .and(expect.same(List(2L, 3L, 3L), List(rational(1, 2), rational(two.pow(64), 3), rational(-1, -two.pow(64)))))
      .and(expect.same(48L, leaf.weight))
      .and(expect.same(5L, CanonicalValue.array(Vector(CNull, t("ab"))).toOption.get.weight))
      .and(expect.same(1L, CanonicalValue.map(List("é" -> CNull)).toOption.get.weight))
  }

  private def bytesOf(n: Int): CanonicalValue = CBytes(scodec.bits.ByteVector.fill(n.toLong)(1))

  test("updated, removed and merged agree with building the map from scratch") {
    val maps: Gen[CMap] = CanonicalGens.value(2).map {
      case m: CMap => m
      case other   => CanonicalValue.map(List("x" -> other)).toOption.get
    }
    forall(Gen.zip(maps.map(m => m: CanonicalValue), maps.map(m => m: CanonicalValue), CanonicalGens.text, CanonicalGens.value(1))) {
      case (m: CMap, n: CMap, k, v) =>
        val key = t(k)
        expect
          .same(CanonicalValue.map(m.entries.updated(k, v)), CanonicalValue.updated(m, key, v))
          .and(expect.same(CanonicalValue.map(m.entries - k), Right(CanonicalValue.removed(m, key))))
          .and(expect.same(CanonicalValue.map(m.entries ++ n.entries), Right(CanonicalValue.merged(Vector(m, n)))))
          // equality is on entries alone: check the cached depth and weight too
          .and(expect.same(shape(CanonicalValue.map(m.entries.updated(k, v))), shape(CanonicalValue.updated(m, key, v))))
          .and(expect.same(shape(CanonicalValue.map(m.entries - k)), shape(Right(CanonicalValue.removed(m, key)))))
      case _ => failure("the generator only yields maps")
    }
  }

  private def shape(v: Either[?, CanonicalValue]): Either[String, (Int, Long)] = v.left.map(_.toString).map(x => (x.depth, x.weight))

  test("a chain of updates and removals keeps depth and weight exact, including replacing the deepest value") {
    val deep = (1 to 6).foldLeft[CanonicalValue](CNull)((v, _) => CanonicalValue.array(Vector(v)).toOption.get)
    val values = Gen.oneOf(Gen.const(deep), Gen.const(CInt(BigInt(1))), Gen.const(CNull), CanonicalGens.value(2))
    val step = Gen.zip(Gen.oneOf("a", "b", "c", "d"), values)
    forall(Gen.listOfN(30, step)) { steps =>
      val (_, checks) = steps.foldLeft((CanonicalValue.EmptyMap, success)) {
        case ((m, acc), (k, v)) =>
          val next = CanonicalValue.updated(m, t(k), v).toOption.getOrElse(m)
          val fresh = CanonicalValue.map(next.entries)
          (next, acc.and(expect.same(shape(fresh), shape(Right(next)))))
      }
      checks
    }
  }
}
