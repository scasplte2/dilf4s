package xyz.kd5ujc.jlvm

import weaver.SimpleIOSuite

object GasSuite extends SimpleIOSuite {

  pureTest("gas arithmetic saturates instead of overflowing, and never goes negative") {
    expect
      .same(Long.MaxValue, (Gas.Max + Gas(1)).value)
      .and(expect.same(Long.MaxValue, (Gas(Long.MaxValue / 2 + 1) * 2).value))
      .and(expect.same(0L, Gas(-5).value))
      .and(expect.same(0L, (Gas(3) - Gas(10)).value))
      .and(expect.same(12L, (Gas(4) * 3).value))
  }

  pureTest("schedule v1 prices every operation of language version 1") {
    val unpriced = Op.values.toList.filterNot(GasSchedule.V1.ops.contains)
    expect(unpriced.isEmpty, s"unpriced: ${unpriced.map(_.wire).mkString(", ")}")
  }

  pureTest("a position's own rate: a sigma message pays the hashing rate, the proposition and proof the curve rate") {
    import xyz.kd5ujc.core.canonical.CanonicalValue.CBytes
    import scodec.bits.ByteVector
    def sized(prop: Int, proof: Int, message: Int) =
      GasSchedule.V1
        .sized(
          Op.SigmaVerify,
          Vector(
            CBytes(ByteVector.fill(prop.toLong)(1)),
            CBytes(ByteVector.fill(proof.toLong)(1)),
            CBytes(ByteVector.fill(message.toLong)(1))
          )
        )
        .value
    expect
      .same(1000L, sized(10, 56, 1032) - sized(10, 56, 32))
      .and(expect.same(GasSchedule.V1.cost(Op.SigmaVerify).perUnit.value * 1000, sized(10, 1056, 32) - sized(10, 56, 32)))
      .and(
        expect.same(
          // per unit of weight (2 each), and per operand: bytes have no limbs
          GasSchedule.V1.cost(Op.Add).perUnit.value * 4 + 2 * GasSchedule.V1
            .cost(Op.Add)
            .extra
            .map(r => GasSchedule.units(r.unit, CBytes(ByteVector(1))) * r.perUnit.value)
            .sum,
          GasSchedule.V1.sized(Op.Add, Vector(CBytes(ByteVector(1)), CBytes(ByteVector(1)))).value
        )
      )
  }

  pureTest("limb-squared units: numbers only; integers count only under Limbs2; a rational's gcd costs GcdLimbs more") {
    import xyz.kd5ujc.core.canonical.CanonicalValue
    import xyz.kd5ujc.numerics.Ratio
    val big = xyz.kd5ujc.core.canonical.CanonicalGens.int(BigInt(1) << 4000)
    val q =
      CanonicalValue.number(Ratio.of(BigInt(1) << 1000, (BigInt(1) << 600) + 1).toOption.get).fold(e => sys.error(e.toString), identity)
    val limbs = GasSchedule.GcdLimbs + 24
    expect
      .same(62L * 62, GasSchedule.units(SizeUnit.Limbs2, big))
      .and(expect.same(0L, GasSchedule.units(SizeUnit.RationalLimbs2, big)))
      .and(expect.same(limbs * limbs, GasSchedule.units(SizeUnit.RationalLimbs2, q)))
      .and(expect.same(limbs * limbs, GasSchedule.units(SizeUnit.Limbs2, q)))
      .and(expect.same(0L, GasSchedule.units(SizeUnit.Limbs2, CanonicalValue.CInt(7))))
      .and(expect.same(0L, GasSchedule.units(SizeUnit.Limbs2, CanonicalValue.True)))
      .and(expect.same(q.weight, GasSchedule.units(SizeUnit.RationalWeight, q)))
      .and(expect.same(0L, GasSchedule.units(SizeUnit.RationalWeight, big)))
  }

  pureTest("a map's depth bound is ceil(log2(n + 1)); a search pays one level more, for the key") {
    expect
      .same(List(0L, 1L, 2L, 2L, 3L, 3L, 4L, 24L, 25L), List(0L, 1L, 2L, 3L, 4L, 6L, 8L, (1L << 24) - 1, 1L << 24).map(GasSchedule.depth))
      .and(expect.same(1L, GasSchedule.levels(0L)))
      .and(expect.same(GasSchedule.depth(1000L) + 1, GasSchedule.levels(1000L)))
  }

  pureTest("get, has, set and unset charge each level of the search, and each SearchBlock of the key at each level") {
    import xyz.kd5ujc.core.canonical.CanonicalValue
    def text(s: String) = CanonicalValue.text(s).toOption.get
    val map = CanonicalValue.map((0 until 1000).map(i => s"k$i" -> CanonicalValue.CInt(i))).toOption.get
    val key = text("k" * (3 * Values.SearchBlock + 5))
    List(Op.Get, Op.Has, Op.Set, Op.Unset).foldLeft(success) { (acc, op) =>
      val s = GasSchedule.V1.cost(op).search.get
      val levels = GasSchedule.levels(1000L)
      acc
        .and(expect.same(((s.perLevel + s.perBlock * 3L) * levels).value, GasSchedule.V1.sized(op, Vector(map, key)).value))
        .and(expect.same((s.perLevel * levels).value, GasSchedule.V1.sized(op, Vector(map, text("k"))).value))
        .and(expect.same(0L, GasSchedule.V1.sized(op, Vector(text("m"), key)).value))
    }
  }

  pureTest("merging maps charges each entry after the first map at each level of all their entries; arrays do not") {
    import xyz.kd5ujc.core.canonical.CanonicalValue
    def m(n: Int) = CanonicalValue.map((0 until n).map(i => s"k$i" -> CanonicalValue.CInt(i))).toOption.get
    val (a, b, c) = (m(100), m(30), m(5))
    val mg = GasSchedule.V1.cost(Op.Merge).merging.get
    val perUnit = GasSchedule.V1.cost(Op.Merge).perUnit
    val plain = perUnit * (a.weight + b.weight + c.weight)
    val merged = (mg.perLevel * 35L + mg.perBlock * ((b.weight + c.weight) / Values.SearchBlock)) * GasSchedule.levels(135L)
    val arr = CanonicalValue.array(Vector(CanonicalValue.CInt(1))).toOption.get
    expect
      .same((plain + merged).value, GasSchedule.V1.sized(Op.Merge, Vector(a, b, c)).value)
      .and(expect.same((perUnit * a.weight).value, GasSchedule.V1.sized(Op.Merge, Vector(a)).value))
      .and(expect.same((perUnit * (a.weight + arr.weight)).value, GasSchedule.V1.sized(Op.Merge, Vector(a, arr)).value))
  }
}
