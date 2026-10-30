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
      .and(expect.same(430L * 1000, sized(10, 1056, 32) - sized(10, 56, 32)))
      .and(
        expect.same(
          GasSchedule.V1.cost(Op.Add).perUnit.value * 4,
          GasSchedule.V1.sized(Op.Add, Vector(CBytes(ByteVector(1)), CBytes(ByteVector(1)))).value
        )
      )
  }
}
