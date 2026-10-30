package xyz.kd5ujc.jlvm

import cats.Show

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object EvaluatorSuite extends SimpleIOSuite with Checkers {

  private given Show[Program] = Show.fromToString

  private def program(json: String): Program = Parser.parse(json).fold(e => sys.error(e.toString), identity)
  private def value(json:   String): CanonicalValue = JsonSurface.parse(json).fold(e => sys.error(e.toString), identity)

  private def eval(
    json:   String,
    data:   String = "{}",
    env:    Map[String, CanonicalValue] = Map.empty,
    driver: Config.Driver = Config.Driver.StackMachine,
    limit:  Long = GasLimit.Default.amount.value
  ): Outcome = Jlvm.evaluate(program(json), value(data), env, Config(limit = GasLimit(Gas(limit)), driver = driver))

  private def result(json: String, data: String = "{}", env: Map[String, CanonicalValue] = Map.empty) =
    eval(json, data, env).result

  private def ok(json: String): Either[JlvmError, CanonicalValue] = Right(value(json))

  // ---- values

  pureTest("operations, literals, arrays and objects evaluate element-wise") {
    expect
      .same(ok("3"), result("""{"+": [1, 2]}"""))
      .and(expect.same(ok("[2, 3]"), result("""[{"+": [1, 1]}, 3]""")))
      .and(expect.same(ok("""{"a": 2, "b": "x"}"""), result("""{"a": {"*": [1, 2]}, "b": "x"}""")))
      .and(expect.same(ok("""{"a": 1}"""), result("""{"a": 1, "b": null}""")))
  }

  pureTest("var reads data paths and array indices; absent paths are null or the default") {
    val data = """{"a": {"b": [10, 20]}, "z": 0}"""
    expect
      .same(ok("20"), result("""{"var": "a.b.1"}""", data))
      .and(expect.same(ok("null"), result("""{"var": "a.b.2"}""", data)))
      .and(expect.same(ok("null"), result("""{"var": "a.b.01"}""", data)))
      .and(expect.same(ok("5"), result("""{"var": ["missing", 5]}""", data)))
      .and(expect.same(ok("0"), result("""{"var": ["z", 5]}""", data)))
      .and(expect.same(Right(value(data)), result("""{"var": ""}""", data)))
  }

  pureTest("$ reads host bindings, which let and callbacks shadow; an unbound name is an error unless defaulted") {
    val env = Map("ordinal" -> CInt(BigInt(42)))
    expect
      .same(ok("43"), result("""{"+": [{"var": "$ordinal"}, 1]}""", env = env))
      .and(expect.same(ok("1"), result("""{"let": [[["ordinal", 1]], {"var": "$ordinal"}]}""", env = env)))
      .and(expect(result("""{"var": "$timestamp"}""").isLeft, "unbound"))
      .and(expect.same(ok("0"), result("""{"var": ["$timestamp", 0]}""")))
  }

  pureTest("let binds sequentially, in either surface form") {
    expect
      .same(ok("3"), result("""{"let": [[["x", 1], ["y", {"+": [{"var": "$x"}, 1]}]], {"+": [{"var": "$x"}, {"var": "$y"}]}]}"""))
      .and(expect.same(ok("3"), result("""{"let": [{"a": 1, "b": {"+": [{"var": "$a"}, 1]}}, {"+": [{"var": "$a"}, {"var": "$b"}]}]}""")))
      .and(expect(Parser.parse("""{"let": [[["x.y", 1]], 0]}""").flatMap(Compiled(_)).isLeft, "dotted names"))
      .and(expect(Parser.parse("""{"let": [[["x", 1], ["x", 2]], 0]}""").flatMap(Compiled(_)).isLeft, "duplicates"))
  }

  // ---- laziness

  pureTest("if, and, or and default evaluate only what decides the result") {
    val boom = """{"/": [1, 0]}"""
    expect
      .same(ok("1"), result(s"""{"if": [true, 1, $boom]}"""))
      .and(expect.same(ok("2"), result(s"""{"if": [false, $boom, true, 2, $boom]}""")))
      .and(expect.same(ok("0"), result(s"""{"and": [0, $boom]}""")))
      .and(expect.same(ok("\"x\""), result(s"""{"or": ["x", $boom]}""")))
      .and(expect.same(ok("5"), result(s"""{"default": [null, 0, 5, $boom]}""")))
      .and(expect.same(ok("null"), result("""{"default": [null, 0]}""")))
      .and(expect.same(ok("true"), result("""{"and": []}""")))
      .and(expect.same(ok("false"), result("""{"or": []}""")))
      .and(expect(Parser.parse("""{"if": [true, 1]}""").flatMap(Compiled(_)).isLeft, "if needs an else"))
  }

  // ---- callbacks

  pureTest("callbacks bind $current, $index and $accumulator; plain paths still read the data") {
    val data = """{"xs": [1, 2, 3, 4], "k": 2, "people": [{"age": 30}, {"age": 12}]}"""
    expect
      .same(ok("[2, 4, 6, 8]"), result("""{"map": [{"var": "xs"}, {"*": [{"var": "$current"}, 2]}]}""", data))
      .and(expect.same(ok("[3, 4]"), result("""{"filter": [{"var": "xs"}, {">": [{"var": "$current"}, {"var": "k"}]}]}""", data)))
      .and(expect.same(ok("10"), result("""{"reduce": [{"var": "xs"}, {"+": [{"var": "$accumulator"}, {"var": "$current"}]}, 0]}""", data)))
      .and(expect.same(ok("[0, 1, 2, 3]"), result("""{"map": [{"var": "xs"}, {"var": "$index"}]}""", data)))
      .and(
        expect.same(
          ok("[30]"),
          result("""{"map": [{"filter": [{"var": "people"}, {">=": [{"var": "$current.age"}, 18]}]}, {"var": "$current.age"}]}""", data)
        )
      )
      .and(
        expect.same(
          ok("[[1, 4], [1, 4]]"),
          result("""{"map": [[1, 2], {"map": [[1, 2], {"*": [{"var": "$current"}, {"var": "$current"}]}]}]}""")
        )
      )
  }

  pureTest("all, some (with a quorum), none, find and count") {
    val data = """{"xs": [1, 2, 3]}"""
    expect
      .same(ok("true"), result("""{"all": [{"var": "xs"}, {">": [{"var": "$current"}, 0]}]}""", data))
      .and(expect.same(ok("false"), result("""{"all": [[], true]}""")))
      .and(expect.same(ok("true"), result("""{"some": [{"var": "xs"}, {">": [{"var": "$current"}, 2]}]}""", data)))
      .and(expect.same(ok("false"), result("""{"some": [{"var": "xs"}, {">": [{"var": "$current"}, 1]}, 3]}""", data)))
      .and(expect.same(ok("true"), result("""{"some": [{"var": "xs"}, {">": [{"var": "$current"}, 1]}, 2]}""", data)))
      .and(expect.same(ok("true"), result("""{"none": [{"var": "xs"}, {">": [{"var": "$current"}, 5]}]}""", data)))
      .and(expect.same(ok("2"), result("""{"find": [{"var": "xs"}, {">": [{"var": "$current"}, 1]}]}""", data)))
      .and(expect.same(ok("null"), result("""{"find": [{"var": "xs"}, false]}""", data)))
      .and(expect.same(ok("2"), result("""{"count": [{"var": "xs"}, {">": [{"var": "$current"}, 1]}]}""", data)))
      .and(expect.same(ok("3"), result("""{"count": [{"var": "xs"}]}""", data)))
      .and(expect.same(ok("[]"), result("""{"map": [{"var": "absent"}, 1]}""", data)))
      .and(expect(result("""{"map": [5, 1]}""").isLeft, "only arrays (or null) iterate"))
  }

  pureTest("all, some and find stop at the first decisive element") {
    val boom = """{"/": [1, {"-": [{"var": "$current"}, 2]}]}"""
    expect
      .same(ok("false"), result(s"""{"all": [[1, 0, 2], {"and": [{"var": "$$current"}, $boom]}]}"""))
      .and(expect.same(ok("true"), result(s"""{"some": [[3, 2], {"!!": $boom}]}""")))
      .and(expect.same(ok("3"), result(s"""{"find": [[3, 2], {"!!": $boom}]}""")))
  }

  pureTest("count with a callback is a callback, decided by the syntax, not by evaluated arguments") {
    expect.same(ok("1"), result("""{"count": [[0, 7], {"var": "$current"}]}"""))
  }

  pureTest("missing and missing_some read the data") {
    val data = """{"a": 1, "b": {"c": 2}}"""
    expect
      .same(ok("""["x"]"""), result("""{"missing": ["a", "b.c", "x"]}""", data))
      .and(expect.same(ok("""["x"]"""), result("""{"missing": [["a", "x"]]}""", data)))
      .and(expect.same(ok("[]"), result("""{"missing_some": [1, ["a", "x"]]}""", data)))
      .and(expect.same(ok("""["x", "y"]"""), result("""{"missing_some": [2, ["a", "x", "y"]]}""", data)))
  }

  // ---- gas

  pureTest("gas follows the schedule: base, then arguments, then the size charge") {
    // + base 5; two literals at 1; size: perUnit 1 x (weight 1 + weight 1)
    expect
      .same(Gas(9), eval("""{"+": [1, 2]}""").gasUsed)
      .and(expect.same(Gas(2 + 1 * 2), eval("""{"var": "a.b"}""", """{"a": {"b": 1}}""").gasUsed))
  }

  pureTest("failures report the gas they used; running out uses the whole limit") {
    // base 5, literals 1 + 1, size 1 x (weight 1 + weight 2): "x" is one node plus one byte
    val failed = eval("""{"+": [1, "x"]}""")
    val starved = eval("""{"+": [1, 2]}""", limit = 8)
    expect(failed.result.isLeft, "type error")
      .and(expect.same(Gas(10), failed.gasUsed))
      .and(expect.same(Gas(8), starved.gasUsed))
      .and(expect.same(Left(JlvmError.OutOfGas(9, 8)), starved.result))
  }

  pureTest("an untaken branch costs nothing") {
    val cheap = eval("""{"if": [true, 1, {"+": [1, 2, 3, 4, 5]}]}""").gasUsed
    val dear = eval("""{"if": [false, 1, {"+": [1, 2, 3, 4, 5]}]}""").gasUsed
    expect(cheap.value < dear.value, s"$cheap < $dear")
  }

  pureTest("callbacks pay per element up front, and per run") {
    val one = eval("""{"map": [[1], {"var": "$current"}]}""").gasUsed.value
    val two = eval("""{"map": [[1, 1], {"var": "$current"}]}""").gasUsed.value
    // one more literal element (1 literal + 1 element), one more per-element charge (1), one more run (1 + var 2 + 1)
    expect.same(2L + 1L + 4L, two - one)
  }

  // ---- stack safety

  pureTest("long iterations run in constant stack on both drivers") {
    val xs = CanonicalValue.array(Vector.tabulate(20000)(i => CInt(BigInt(i)))).toOption.get
    val p = program("""{"reduce": [{"var": "xs"}, {"+": [{"var": "$accumulator"}, {"var": "$current"}]}, 0]}""")
    val data = CanonicalValue.map(List("xs" -> xs)).toOption.get
    val outcomes =
      Config.Driver.values.toList.map(d => Jlvm.evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(10000000L)), driver = d)))
    expect
      .same(List.fill(2)(Right(CInt(BigInt(20000L * 19999L / 2)))), outcomes.map(_.result))
      .and(expect.same(1, outcomes.distinct.size))
  }

  pureTest("programs nest at most Program.MaxDepth deep, checked without recursion") {
    val deep = (1 to 100000).foldLeft[Expr](Expr.Lit(CNull))((e, _) => Expr.Apply(Op.Not, Vector(e)))
    val legal = (1 to Program.MaxDepth).foldLeft[Expr](Expr.Lit(CNull))((e, _) => Expr.Apply(Op.Not, Vector(e)))
    expect(Program.from(deep).isLeft, "too deep")
      .and(expect.same(Right(False), Program.from(legal).flatMap(p => Jlvm.evaluate(p, CNull, Map.empty, Config()).result)))
  }

  // ---- the two drivers agree

  private val dataJson = """{"n": 7, "xs": [3, 1, 4, 1, 5], "name": "jlvm", "o": {"k": 2}}"""

  private def lit(i:  Int): Expr = Expr.Lit(CInt(BigInt(i)))
  private def v(path: String): Expr = Expr.Var(VarRef.parse(path).toOption.get, scala.None)
  private def ap(op:  Op, args: Expr*): Expr = Expr.Apply(op, args.toVector)

  /** Mostly well-typed numeric programs, with enough ill-typed and failing ones to compare errors too. */
  private def expr(depth: Int, bound: List[String]): Gen[Expr] = {
    val leaf: Gen[Expr] = Gen.frequency(
      4 -> Gen.choose(-3, 9).map(lit),
      2 -> Gen.const(v("n")),
      1 -> Gen.oneOf(v("o.k"), v("missing"), v("name")),
      3 -> (if (bound.isEmpty) Gen.const(v("n")) else Gen.oneOf(bound).map(b => v("$" + b)))
    )
    if (depth <= 0) leaf
    else {
      val sub = expr(depth - 1, bound)
      val inner = expr(depth - 1, "current" :: "index" :: bound)
      Gen.frequency(
        3 -> leaf,
        4 -> Gen.zip(Gen.oneOf(Op.Add, Op.Sub, Op.Mul, Op.Min, Op.Max, Op.Div, Op.Mod), sub, sub).map { case (o, a, b) => ap(o, a, b) },
        2 -> Gen.zip(Gen.oneOf(Op.Lt, Op.Ge, Op.Eq), sub, sub, sub, sub).map { case (o, a, b, t, e) => ap(Op.If, ap(o, a, b), t, e) },
        1 -> Gen.zip(Gen.oneOf(Op.And, Op.Or, Op.Default), sub, sub).map { case (o, a, b) => ap(o, a, b) },
        2 -> Gen.zip(sub, expr(depth - 1, "x" :: bound)).map {
          case (a, body) => ap(Op.Let, Expr.Arr(Vector(Expr.Arr(Vector(Expr.Lit(CanonicalValue.text("x").toOption.get), a)))), body)
        },
        2 -> inner.map(body => ap(Op.Reduce, v("xs"), ap(Op.Add, v("$accumulator"), body), lit(0))),
        1 -> inner.map(body => ap(Op.Count, v("xs"), ap(Op.Gt, body, lit(2)))),
        1 -> inner.map(body => ap(Op.Length, ap(Op.Map, v("xs"), body))),
        1 -> inner.map(body => ap(Op.Some, v("xs"), ap(Op.Lt, body, lit(3)), lit(2)))
      )
    }
  }

  private val programs: Gen[Program] = Gen.choose(1, 5).flatMap(expr(_, Nil)).map(e => Program.from(e).toOption.get)

  private val limits: Gen[Long] = Gen.frequency(1 -> Gen.choose(0L, 300L), 1 -> Gen.const(GasLimit.Default.amount.value))

  test("the stack machine and the recursive driver agree on value, error and gas, at every gas limit") {
    val data = value(dataJson)
    forall(Gen.zip(programs, limits)) {
      case (p, limit) =>
        val run = (d: Config.Driver) => Jlvm.evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit)), driver = d))
        expect.same(run(Config.Driver.Recursive), run(Config.Driver.StackMachine))
    }
  }

  pureTest("the agreement property is not vacuous: most generated programs succeed, some fail, some run out of gas") {
    val data = value(dataJson)
    val sample = Gen.listOfN(600, Gen.zip(programs, limits)).pureApply(Gen.Parameters.default, Seed(20261030L))
    val outcomes = sample.map { case (p, limit) => Jlvm.evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit)))).result }
    val succeeded = outcomes.count(_.isRight)
    val starved = outcomes.count {
      case Left(JlvmError.OutOfGas(_, _)) => true
      case _                              => false
    }
    val failed = outcomes.size - succeeded - starved
    expect(succeeded * 100 >= outcomes.size * 35, s"succeeded $succeeded of ${outcomes.size}")
      .and(expect(starved >= 30, s"out of gas $starved"))
      .and(expect(failed >= 30, s"other errors $failed"))
  }
}
