package xyz.kd5ujc.core.canonical

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** A value's `toString` is bounded however often it repeats a shared part, as the SDKs' `inspect` and `Debug` are. */
object InspectSuite extends SimpleIOSuite {

  private def get[A](e: Either[CanonicalError, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def text(s:   String): CanonicalValue = get(CanonicalValue.text(s))

  pureTest("a small value reads as the TypeScript SDK shows it") {
    val third = get(CanonicalValue.number(Ratio.of(1, 3).toOption.get))
    val small = get(
      CanonicalValue.map(
        List("a" -> get(CanonicalValue.array(Vector(CInt(1), text("x")))), "b" -> CBytes(ByteVector(0x00, 0xff)), "c" -> third)
      )
    )
    expect
      .same("""{"a": [1, "x"], "b": h'00ff', "c": 1/3}""", small.toString)
      .and(expect.same(small.toString, (small: CanonicalValue).show))
      .and(expect.same(List("null", "true", "-5", "\"a\\n\\\"\""), List(CNull, True, CInt(-5), text("a\n\"")).map(_.toString)))
      .and(expect.same("<int of 301 bits>", CanonicalGens.int(BigInt(1) << 300).toString))
  }

  pureTest("a shared value of saturated weight is shown within the budget, the rest elided") {
    val dag = get(BuiltValues.build(BuiltValues.of(CInt(1)).wrap(BuiltValues.Pair, 70)))
    val shown = dag.toString
    expect(shown.length < 5 * DebugBudget, s"${shown.length} characters for a value of weight ${dag.weight}")
      .and(expect(shown.contains("…"), "what the budget does not reach is elided"))
      .and(expect(shown.startsWith("[" * DebugDepth + "…"), "nothing deeper than the depth limit is shown"))
  }

  pureTest("a long text or wide map is elided once the budget is spent") {
    val long = text("x" * (2 * DebugBudget.toInt))
    val wide = get(CanonicalValue.map((0 until 100000).map(i => f"k$i%06d" -> CInt(i.toLong)).toList))
    val shown = wide.toString
    expect
      .same("…", long.toString)
      .and(expect(shown.length < 5 * DebugBudget, s"${shown.length} characters for 100,000 entries"))
      .and(expect(shown.startsWith("{\"k000000\": 0, \"k000001\": 1, ") && shown.endsWith(", …}"), shown.take(60)))
  }

  pureTest("a key longer than the budget is elided from its length, and nothing after the budget is spent is shown") {
    val longKey = get(CanonicalValue.map(List("k" * (4 * DebugBudget.toInt) -> CInt(1), "a" -> CInt(2))))
    val spent = get(CanonicalValue.array(Vector(text("x" * (DebugBudget.toInt - 2)), longKey, CInt(3))))
    expect
      .same("{\"a\": 2, …}", longKey.toString)
      .and(expect.same("[\"" + "x" * (DebugBudget.toInt - 2) + "\", …]", spent.toString))
  }
}
