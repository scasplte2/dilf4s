package xyz.kd5ujc.jlvm

import cats.Show
import cats.effect.{IO, Resource}

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import weaver.scalacheck.Checkers
import weaver.{Expectations, IOSuite}

object EvaluatorSuite extends IOSuite with Checkers {

  type Res = Jlvm
  def sharedResource: Resource[IO, Jlvm] = Resource.eval(Jlvm.load[IO])

  private def check(name: String)(f: Jlvm ?=> Expectations) = test(name)(jlvm => IO(f(using jlvm)))

  private given Show[Program] = Show.fromToString

  private def program(json: String): Program = Parser.parse(json).fold(e => sys.error(e.toString), identity)
  private def value(json:   String): CanonicalValue = JsonSurface.parse(json).fold(e => sys.error(e.toString), identity)

  private def eval(
    json:   String,
    data:   String = "{}",
    env:    Map[String, CanonicalValue] = Map.empty,
    driver: Config.Driver = Config.Driver.StackMachine,
    limit:  Long = GasLimit.Default.amount.value
  )(using jlvm: Jlvm): Outcome = jlvm.evaluate(program(json), value(data), env, Config(limit = GasLimit(Gas(limit)), driver = driver))

  private def result(json: String, data: String = "{}", env: Map[String, CanonicalValue] = Map.empty)(using Jlvm) =
    eval(json, data, env).result

  private def ok(json: String): Either[JlvmError, CanonicalValue] = Right(value(json))

  // ---- values

  check("operations, literals, arrays and objects evaluate element-wise") {
    expect
      .same(ok("3"), result("""{"+": [1, 2]}"""))
      .and(expect.same(ok("[2, 3]"), result("""[{"+": [1, 1]}, 3]""")))
      .and(expect.same(ok("""{"a": 2, "b": "x"}"""), result("""{"a": {"*": [1, 2]}, "b": "x"}""")))
      .and(expect.same(ok("""{"a": 1}"""), result("""{"a": 1, "b": null}""")))
  }

  check("var reads data paths and array indices; absent paths are null or the default") {
    val data = """{"a": {"b": [10, 20]}, "z": 0}"""
    expect
      .same(ok("20"), result("""{"var": "a.b.1"}""", data))
      .and(expect.same(ok("null"), result("""{"var": "a.b.2"}""", data)))
      .and(expect.same(ok("null"), result("""{"var": "a.b.01"}""", data)))
      .and(expect.same(ok("5"), result("""{"var": ["missing", 5]}""", data)))
      .and(expect.same(ok("0"), result("""{"var": ["z", 5]}""", data)))
      .and(expect.same(Right(value(data)), result("""{"var": ""}""", data)))
  }

  check("$ reads host bindings, which let and callbacks shadow; an unbound name is an error unless defaulted") {
    val env = Map("ordinal" -> CInt(BigInt(42)))
    expect
      .same(ok("43"), result("""{"+": [{"var": "$ordinal"}, 1]}""", env = env))
      .and(expect.same(ok("1"), result("""{"let": [[["ordinal", 1]], {"var": "$ordinal"}]}""", env = env)))
      .and(expect(result("""{"var": "$timestamp"}""").isLeft, "unbound"))
      .and(expect.same(ok("0"), result("""{"var": ["$timestamp", 0]}""")))
  }

  check("let binds sequentially, in either surface form") {
    expect
      .same(ok("3"), result("""{"let": [[["x", 1], ["y", {"+": [{"var": "$x"}, 1]}]], {"+": [{"var": "$x"}, {"var": "$y"}]}]}"""))
      .and(expect.same(ok("3"), result("""{"let": [{"a": 1, "b": {"+": [{"var": "$a"}, 1]}}, {"+": [{"var": "$a"}, {"var": "$b"}]}]}""")))
      .and(expect(Parser.parse("""{"let": [[["x.y", 1]], 0]}""").flatMap(Compiled(_)).isLeft, "dotted names"))
      .and(expect(Parser.parse("""{"let": [[["x", 1], ["x", 2]], 0]}""").flatMap(Compiled(_)).isLeft, "duplicates"))
  }

  // ---- laziness

  check("if, and, or and default evaluate only what decides the result") {
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
      .and(expect.same(ok("1"), result("""{"if": [true, 1]}""")))
      .and(expect.same(ok("null"), result("""{"if": [false, 1, false, 2]}""")))
      .and(expect(Parser.parse("""{"if": [true]}""").flatMap(Compiled(_)).isLeft, "if needs a condition and a branch"))
  }

  check("reduce without an initial value starts from the first element; an empty array gives null") {
    val sum = """{"+": [{"var": "$accumulator"}, {"var": "$current"}]}"""
    val indices = """{"cat": [{"var": "$accumulator"}, {"var": "$index"}]}"""
    expect
      .same(ok("10"), result(s"""{"reduce": [[1, 2, 3, 4], $sum]}"""))
      .and(expect.same(ok("42"), result(s"""{"reduce": [[42], $sum]}""")))
      .and(expect.same(ok("null"), result(s"""{"reduce": [[], $sum]}""")))
      .and(expect.same(ok("null"), result(s"""{"reduce": [{"var": "absent"}, $sum]}""")))
      .and(expect.same(ok("\"a12\""), result(s"""{"reduce": [["a", "b", "c"], $indices]}""")))
  }

  check("a plain path naming a let binding is rejected; the binding, the data and the element are each explicit") {
    def compiles(json: String) = Parser.parse(json).flatMap(Compiled(_)).isRight
    expect(!compiles("""{"let": [[["x", 5]], {"var": "x"}]}"""), "the body")
      .and(expect(!compiles("""{"let": [[["x", 5], ["y", {"var": "x.a"}]], 0]}"""), "a later binding"))
      .and(expect(!compiles("""{"let": [[["x", 5]], {"map": [[1], {"var": "x"}]}]}"""), "inside a callback"))
      .and(expect(compiles("""{"let": [[["y", {"var": "x"}], ["x", 5]], 0]}"""), "an earlier binding sees the data"))
      .and(expect.same(ok("5"), result("""{"let": [[["x", 5]], {"var": "$x"}]}""")))
      .and(expect.same(ok("7"), result("""{"let": [[["x", 5]], {"var": [["x"]]}]}""", """{"x": 7}""")))
  }

  check("migrate rewrites metakit's plain reads of let names to $name") {
    def run(json: String, data: String = "{}") = Parser.migrate(json).map(result(_, data))
    expect
      .same(
        Right("""{"let":[[["x",5],["y",{"+":[{"var":"$x"},3]}]],{"var":"$y"}]}"""),
        Parser.migrate("""{"let": [[["x", 5], ["y", {"+": [{"var": "x"}, 3]}]], {"var": "y"}]}""")
      )
      .and(expect.same(Right(ok("8")), run("""{"let": [[["x", 5], ["y", {"+": [{"var": "x"}, 3]}]], {"var": "y"}]}""")))
      .and(
        expect.same(
          Right(ok("11")),
          run("""{"let": [{"doubled": {"*": [{"var": "x"}, 2]}}, {"+": [{"var": "doubled"}, 1]}]}""", """{"x": 5}""")
        )
      )
      .and(expect.same(Right(ok("2")), run("""{"let": [{"b": {"+": [{"var": "a"}, 1]}, "a": 1}, {"var": "b"}]}""")))
  }

  // ---- callbacks

  check("callbacks bind $current, $index and $accumulator; plain paths fall through to the data") {
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

  check("all, some (with a quorum), none, find and count") {
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

  check("all, some and find stop at the first decisive element") {
    val boom = """{"/": [1, {"-": [{"var": "$current"}, 2]}]}"""
    expect
      .same(ok("false"), result(s"""{"all": [[1, 0, 2], {"and": [{"var": "$$current"}, $boom]}]}"""))
      .and(expect.same(ok("true"), result(s"""{"some": [[3, 2], {"!!": $boom}]}""")))
      .and(expect.same(ok("3"), result(s"""{"find": [[3, 2], {"!!": $boom}]}""")))
  }

  check("count with a callback is a callback, decided by the syntax, not by evaluated arguments") {
    expect.same(ok("1"), result("""{"count": [[0, 7], {"var": "$current"}]}"""))
  }

  check("missing and missing_some read the data") {
    val data = """{"a": 1, "b": {"c": 2}}"""
    expect
      .same(ok("""["x"]"""), result("""{"missing": ["a", "b.c", "x"]}""", data))
      .and(expect.same(ok("""["x"]"""), result("""{"missing": [["a", "x"]]}""", data)))
      .and(expect.same(ok("[]"), result("""{"missing_some": [1, ["a", "x"]]}""", data)))
      .and(expect.same(ok("""["x", "y"]"""), result("""{"missing_some": [2, ["a", "x", "y"]]}""", data)))
  }

  // ---- callback scope (decision C1): the element first, then the data

  check("inside a callback a plain path reads the element first, then the data; \"\" is the element") {
    val data = """{"x": 1, "k": 2, "proofs": [{"address": "a"}, {"address": "b"}], "items": [{"x": 2}, {"y": 3}]}"""
    expect
      .same(ok("""["a", "b"]"""), result("""{"map": [{"var": "proofs"}, {"var": "address"}]}""", data))
      .and(expect.same(ok("[2, 1]"), result("""{"map": [{"var": "items"}, {"var": "x"}]}""", data)))
      .and(expect.same(ok("[1, 2]"), result("""{"map": [[1, 2], {"var": ""}]}""", data)))
      .and(expect.same(ok("[3]"), result("""{"filter": [[2, 3], {"!==": [{"var": ""}, {"var": "k"}]}]}""", data)))
      .and(expect.same(ok("1"), result("""{"var": "x"}""", data)))
  }

  check("nested callbacks: the innermost element, then the enclosing ones, then the data") {
    val data = """{"z": 0, "rows": [{"a": 1, "cells": [{"b": 10}, {"a": 5, "b": 20}]}]}"""
    expect.same(
      ok("[[11, 25]]"),
      result("""{"map": [{"var": "rows"}, {"map": [{"var": "cells"}, {"+": [{"var": "a"}, {"var": "b"}, {"var": "z"}]}]}]}""", data)
    )
  }

  check("reduce exposes current and accumulator as plain paths, as metakit merged them") {
    val data = """{"votes": [{"vote": 2}, {"vote": 3}]}"""
    expect
      .same(ok("5"), result("""{"reduce": [{"var": "votes"}, {"+": [{"var": "accumulator"}, {"var": "current.vote"}]}, 0]}""", data))
      .and(
        expect
          .same(ok("5"), result("""{"reduce": [{"var": "votes"}, {"+": [{"var": "$accumulator"}, {"var": "$current.vote"}]}, 0]}""", data))
      )
  }

  check("missing resolves like a plain path: inside a callback, the element first") {
    val data = """{"top": 1, "xs": [{"a": 1}, {}]}"""
    expect.same(ok("""[[], ["a"]]"""), result("""{"map": [{"var": "xs"}, {"missing": ["a", "top"]}]}""", data))
  }

  check("a path the element and the data both hold is flagged, once; unambiguous reads are not, and cost the same") {
    val data = """{"x": 1, "items": [{"x": 2}, {"x": 3}], "ys": [{"y": 1}]}"""
    val plain = """{"map": [{"var": "items"}, {"var": "x"}]}"""
    val explicit = """{"map": [{"var": "items"}, {"var": "$current.x"}]}"""
    expect
      .same(Vector(Warning.AmbiguousPath("x")), eval(plain, data).warnings)
      .and(expect.same(Vector.empty[Warning], eval("""{"map": [{"var": "ys"}, {"var": "y"}]}""", data).warnings))
      .and(expect.same(Vector.empty[Warning], eval(explicit, data).warnings))
      .and(expect.same(eval(plain, data, driver = Config.Driver.Recursive), eval(plain, data)))
      .and(expect.same(ok("[2, 3]"), eval(plain, data).result))
  }

  // ---- computed paths (H1)

  check("a computed text path is read as a var path; an array path is exact segments, so a key may hold a dot") {
    val data = """{"balances": {"alice": 5}, "who": "alice", "m": {"x.y": 1, "x": {"y": 2}}, "xs": [7, 8]}"""
    val env = Map("ordinal" -> CInt(BigInt(9)))
    expect
      .same(ok("5"), result("""{"var": {"cat": ["balances.", {"var": "who"}]}}""", data))
      .and(expect.same(ok("5"), result("""{"var": [["balances", {"var": "who"}]]}""", data)))
      .and(expect.same(ok("1"), result("""{"var": [["m", "x.y"]]}""", data)))
      .and(expect.same(ok("2"), result("""{"var": {"literal": "m.x.y"}}""", data)))
      .and(expect.same(ok("8"), result("""{"var": [["xs", 1]]}""", data)))
      .and(expect.same(ok("0"), result("""{"var": [{"cat": ["balances.", "bob"]}, 0]}""", data)))
      .and(expect.same(ok("9"), result("""{"var": {"cat": ["$", "ordinal"]}}""", data, env)))
      .and(expect.same(ok("null"), result("""{"var": [["$ordinal"]]}""", data, env)))
  }

  check("a computed path of the wrong kind or form is an error") {
    def kind(r: Either[JlvmError, CanonicalValue]) = r.left.toOption.map(_.getClass.getSimpleName)
    expect
      .same(Some("Type"), kind(result("""{"var": {"+": [1, 2]}}""")))
      .and(expect.same(Some("Type"), kind(result("""{"var": [[true]]}"""))))
      .and(expect.same(Some("Domain"), kind(result("""{"var": {"cat": ["a", "..b"]}}"""))))
  }

  check("a computed path costs its expression, then the access and its segments") {
    val data = """{"a": {"b": 1}}"""
    // computed: the literal path, then the same access and segments as the static path
    expect.same(
      eval("""{"var": "a.b"}""", data).gasUsed.value + S.literal.value,
      eval("""{"var": {"literal": "a.b"}}""", data).gasUsed.value
    )
  }

  // ---- spreading, decided by the syntax

  check("a variadic operation with a non-array argument takes the elements of its value") {
    val data = """{"balances": {"a": 3, "b": 4}, "xs": [3, 1, 4], "parts": ["x", "y"], "maps": [{"a": 1}, {"b": 2}], "n": 5}"""
    expect
      .same(ok("7"), result("""{"+": {"values": {"var": "balances"}}}""", data))
      .and(expect.same(ok("0"), result("""{"+": {"values": {}}}""", data)))
      .and(expect.same(ok("12"), result("""{"*": {"var": "xs"}}""", data)))
      .and(expect.same(ok("4"), result("""{"max": {"var": "xs"}}""", data)))
      .and(expect.same(ok("\"xy\""), result("""{"cat": {"var": "parts"}}""", data)))
      .and(expect.same(ok("""{"a": 1, "b": 2}"""), result("""{"merge": {"var": "maps"}}""", data)))
      .and(expect(result("""{"+": [{"var": "xs"}]}""", data).isLeft, "the bracketed form never spreads"))
      .and(expect(result("""{"+": {"var": "n"}}""", data).isLeft, "a spread needs an array"))
      .and(expect(Parser.parse("""{"+": 3}""").flatMap(Compiled(_)).isLeft, "a literal that is no array does not compile"))
      .and(expect(Parser.parse("""{"+": {"a": 1, "b": 2}}""").flatMap(Compiled(_)).isLeft, "nor does an object"))
  }

  // ---- surface forms

  check("object constructs maps with any keys; _ keys are data; the SDK aliases evaluate") {
    val data = """{"m": {"a": 1}}"""
    expect
      .same(ok("""{"count": 3}"""), result("""{"object": {"count": {"+": [1, 2]}}}"""))
      .and(
        expect
          .same(ok("""{"_result": {"error": "no", "valid": true}}"""), result("""{"_result": {"valid": {"==": [1, 1]}, "error": "no"}}"""))
      )
      .and(expect.same(ok("1"), result("""{"getKey": [{"var": "m"}, "a"]}""", data)))
      .and(expect.same(ok("""{"a": 1, "b": 2}"""), result("""{"setKey": [{"var": "m"}, "b", 2]}""", data)))
      .and(expect.same(ok("{}"), result("""{"deleteKey": [{"var": "m"}, "a"]}""", data)))
      .and(expect.same(ok("1"), result("""{"size": {"var": "m"}}""", data)))
  }

  // ---- gas

  private val S = GasSchedule.V1
  private def add = S.cost(Op.Add)

  check("gas follows the schedule: base, then arguments, then the size charge") {
    // + base; two literals; size: perUnit x (weight 1 + weight 1), and no rational term for integers
    val sum = add.base.value + 2 * S.literal.value + add.perUnit.value * 2
    expect
      .same(Gas(sum), eval("""{"+": [1, 2]}""").gasUsed)
      .and(expect.same(Gas(S.varAccess.value + S.perPathSegment.value * 2), eval("""{"var": "a.b"}""", """{"a": {"b": 1}}""").gasUsed))
  }

  check("failures report the gas they used; running out uses the whole limit") {
    // "x" weighs 2: one node plus one byte
    val sum = add.base.value + 2 * S.literal.value + add.perUnit.value * 2
    val failed = eval("""{"+": [1, "x"]}""")
    val starved = eval("""{"+": [1, 2]}""", limit = sum - 1)
    expect(failed.result.isLeft, "type error")
      .and(expect.same(Gas(sum + add.perUnit.value), failed.gasUsed))
      .and(expect.same(Gas(sum - 1), starved.gasUsed))
      .and(expect.same(Left(JlvmError.OutOfGas(sum, sum - 1)), starved.result))
  }

  check("an untaken branch costs nothing") {
    val cheap = eval("""{"if": [true, 1, {"+": [1, 2, 3, 4, 5]}]}""").gasUsed
    val dear = eval("""{"if": [false, 1, {"+": [1, 2, 3, 4, 5]}]}""").gasUsed
    expect(cheap.value < dear.value, s"$cheap < $dear")
  }

  check("callbacks pay per element up front, and per run") {
    val one = eval("""{"map": [[1], {"var": "$current"}]}""").gasUsed.value
    val two = eval("""{"map": [[1, 1], {"var": "$current"}]}""").gasUsed.value
    // one more literal element (literal + element), one more per-element charge, one more run (run + var + its name)
    val element = S.literal.value + S.element.value
    val run = S.callbackRun.value + S.varAccess.value + S.perPathSegment.value
    expect.same(element + S.cost(Op.Map).perUnit.value + run, two - one)
  }

  // ---- stack safety

  check("long iterations run in constant stack on every driver") {
    val xs = CanonicalValue.array(Vector.tabulate(20000)(i => CInt(BigInt(i)))).toOption.get
    val p = program("""{"reduce": [{"var": "xs"}, {"+": [{"var": "$accumulator"}, {"var": "$current"}]}, 0]}""")
    val data = CanonicalValue.map(List("xs" -> xs)).toOption.get
    val outcomes =
      Config.Driver.values.toList.map(d => summon[Jlvm].evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(10000000L)), driver = d)))
    expect
      .same(List.fill(Config.Driver.values.size)(Right(CInt(BigInt(20000L * 19999L / 2)))), outcomes.map(_.result))
      .and(expect.same(1, outcomes.distinct.size))
  }

  check("programs nest at most Program.MaxDepth deep, checked without recursion") {
    val deep = (1 to 100000).foldLeft[Expr](Expr.Lit(CNull))((e, _) => Expr.Apply(Op.Not, Vector(e)))
    val legal = (1 to Program.MaxDepth).foldLeft[Expr](Expr.Lit(CNull))((e, _) => Expr.Apply(Op.Not, Vector(e)))
    expect(Program.from(deep).isLeft, "too deep")
      .and(expect.same(Right(False), Program.from(legal).flatMap(p => summon[Jlvm].evaluate(p, CNull, Map.empty, Config()).result)))
  }

  // ---- the two drivers agree

  private val dataJson = """{"n": 7, "xs": [3, 1, 4, 1, 5], "name": "jlvm", "o": {"k": 2}, "rs": [{"n": 1}, {"m": 2}, 4]}"""

  private def lit(i:  Int): Expr = Expr.Lit(CInt(BigInt(i)))
  private def v(path: String): Expr = Expr.Var(VarRef.parse(path).toOption.get, scala.None)
  private def ap(op:  Op, args: Expr*): Expr = Expr.Apply(op, args.toVector)

  /** Mostly well-typed numeric programs, with enough ill-typed and failing ones to compare errors too. */
  private def expr(depth: Int, bound: List[String]): Gen[Expr] = {
    val leaf: Gen[Expr] = Gen.frequency(
      4 -> Gen.choose(-3, 9).map(lit),
      2 -> Gen.const(v("n")),
      1 -> Gen.oneOf(v("o.k"), v("missing"), v("name"), v("m")),
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
        1 -> inner.map(body => ap(Op.Some, v("xs"), ap(Op.Lt, body, lit(3)), lit(2))),
        1 -> inner.map(body => ap(Op.Length, ap(Op.Filter, v("rs"), body))),
        1 -> Gen
          .zip(Gen.oneOf(Op.Add, Op.Mul, Op.Min, Op.Max), Gen.oneOf(Gen.const(v("xs")), inner.map(b => ap(Op.Map, v("rs"), b))))
          .map { case (o, a) => Expr.Spread(o, a) },
        1 -> sub.map(i => Expr.VarAt(Expr.Arr(Vector(Expr.Lit(CanonicalValue.text("xs").toOption.get), i)), scala.None))
      )
    }
  }

  private val programs: Gen[Program] = Gen.choose(1, 5).flatMap(expr(_, Nil)).map(e => Program.from(e).toOption.get)

  private val limits: Gen[Long] = Gen.frequency(1 -> Gen.choose(0L, 300L), 1 -> Gen.const(GasLimit.Default.amount.value))

  test("all three drivers agree on value, error, gas and warnings, at every gas limit") { jlvm =>
    given Jlvm = jlvm
    val data = value(dataJson)
    forall(Gen.zip(programs, limits)) {
      case (p, limit) =>
        val run = (d: Config.Driver) => summon[Jlvm].evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit)), driver = d))
        val reference = run(Config.Driver.Recursive)
        expect.same(reference, run(Config.Driver.StackMachine)).and(expect.same(reference, run(Config.Driver.Direct)))
    }
  }

  check("the agreement property is not vacuous: most generated programs succeed, some fail, some run out of gas") {
    val data = value(dataJson)
    val sample = Gen.listOfN(600, Gen.zip(programs, limits)).pureApply(Gen.Parameters.default, Seed(20261030L))
    val outcomes = sample.map { case (p, limit) => summon[Jlvm].evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit)))).result }
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
