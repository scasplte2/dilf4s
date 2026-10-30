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
}
