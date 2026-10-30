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
      case _ => failure("the generator only yields maps")
    }
  }
}
