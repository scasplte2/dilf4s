package xyz.kd5ujc.benchmarks

import cats.syntax.all._

import xyz.kd5ujc.benchmarks.DedupInputs.Error
import xyz.kd5ujc.core.canonical.Preview
import xyz.kd5ujc.jlvm.JsonSurface

import weaver.FunSuite

object DedupInputsSuite extends FunSuite {
  test("recipe validation retains exact invalid fields and permutation refusal") {
    expect
      .same(Left(Error.Count(Preview.of("0"))), DedupInputs.read("dedup:int:0:ascending"))
      .and(expect.same(Left(Error.InputKind(Preview.of("other"))), DedupInputs.read("dedup:other:5:ascending")))
      .and(expect.same(Left(Error.InputOrder(Preview.of("other"))), DedupInputs.read("dedup:int:5:other")))
      .and(expect.same(Left(Error.NonBijective(104729)), DedupInputs.read("dedup:int:104729:permuted")))
  }

  test("each typed order produces the exact integer inputs") {
    val cases = List(
      "ascending"  -> "{\"a0\":[0,1,2,3,4],\"a1\":[0,1,2,3,4]}",
      "descending" -> "{\"a0\":[4,3,2,1,0],\"a1\":[4,3,2,1,0]}",
      "permuted"   -> "{\"a0\":[0,4,3,2,1],\"a1\":[0,4,3,2,1]}",
      "repeated"   -> "{\"a0\":[0,1,2,3,4],\"a1\":[0,1,2,3,4],\"a2\":[0,1,2,3,4]}"
    )
    cases.foldLeft(expect.same(4, cases.size)) {
      case (acc, (order, json)) =>
        acc.and(expect.same(JsonSurface.parse(json).leftMap(Error.Surface(_)), DedupInputs.read(s"dedup:int:5:$order")))
    }
  }
}
