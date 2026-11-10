package xyz.kd5ujc.modelsgen

import cats.parse.Parser

import xyz.kd5ujc.modelsgen.Ast._

import weaver.FunSuite

object CddlParserSuite extends FunSuite {

  private def parse(text: String): Either[CddlParser.ParseFailure, File] = CddlParser.parse(text)
  private def body(text:  String): Option[Ty] = parse(text).toOption.flatMap(_.rules.headOption).map(_.body)
  private def firstGroup(t: Option[Ty]): Option[Vector[Entry]] = t.collect {
    case Ty.Array(Group(Vector(es))) => es
    case Ty.MapT(Group(Vector(es)))  => es
  }

  test("rules, size control and bytes") {
    expect.same(
      Right(
        Vector(Rule("hash32", Vector.empty, Assign.Eq, Ty.Ctrl(Ty.Name("bytes", Vector.empty), Control.Size, Ty.Literal(Lit.IntL(32)))))
      ),
      parse("hash32 = bytes .size 32\n").map(_.rules)
    )
  }

  test("ranges, with and without spaces, and a size range") {
    expect
      .same(Some(Ty.Range(Ty.Literal(Lit.IntL(0)), Ty.Literal(Lit.IntL(12)), true)), body("a = 0 .. 12"))
      .and(expect.same(Some(Ty.Range(Ty.Literal(Lit.IntL(0)), Ty.Literal(Lit.IntL(1)), true)), body("a = 0..1")))
      .and(expect(body("a = bytes .size (0 .. 32)").exists { case _: Ty.Ctrl => true; case _ => false }))
  }

  test("arrays with named entries, occurrence indicators and nested choices") {
    val es = firstGroup(body("x = [ a : uint, ? b : bytes, * c, 2* d, +e, 1*3 f ]"))
    expect
      .same(Some(Vector(Occ.Once, Occ.Optional, Occ.Star, Occ(2, None), Occ.Plus, Occ(1, Some(3)))), es.map(_.map(_.occ)))
      .and(expect.same(Some(Some(Key.Bare("a"))), es.flatMap(_.headOption).map(_.key)))
  }

  test("type choice versus group choice") {
    expect
      .same(Some(3), body("a = b / c / d").collect { case Ty.Choice(alts) => alts.size })
      .and(expect.same(Some(2), body("a = [ b // c ]").collect { case Ty.Array(g) => g.choices.size }))
      .and(expect.same(Some(1), body("a = [ b / c ]").collect { case Ty.Array(g) => g.choices.size }))
  }

  test("maps with integer keys, optional entries and typed keys") {
    expect.same(
      Some(
        Vector(
          Some(Key.Value(Lit.IntL(0))),
          Some(Key.Value(Lit.IntL(3))),
          Some(Key.OfType(Ty.Name("k", Vector.empty), false)),
          Some(Key.Value(Lit.TextL("s")))
        )
      ),
      firstGroup(body("m = { 0 : a, ? 3 : b, * k => v, ? \"s\" : c }")).map(_.map(_.key))
    )
  }

  test("tags, generics, sockets, extensions and comments") {
    val f = parse("$x /= 1\n$x /= 2\ng<a, b> = [a, b]\nh = g<uint, tstr>\n")
    expect
      .same(
        Some(Ty.Tag(6, Some(BigInt(258)), Some(Ty.Array(Group(Vector(Vector(Entry(Occ.Star, None, Ty.Name("a", Vector.empty))))))))),
        body("s<a> = #6.258([* a]) ; set")
      )
      .and(expect.same(Right(Vector(Assign.TypeChoice, Assign.TypeChoice, Assign.Eq, Assign.Eq)), f.map(_.rules.map(_.assign))))
      .and(expect.same(Right(Some(Vector("a", "b"))), f.map(_.rules.lift(2).map(_.params))))
      .and(expect(body("a = #6.24(bytes .cbor foo)").exists { case _: Ty.Tag => true; case _ => false }))
  }

  test("a parenthesised group rule and a parenthesised type") {
    expect(body("a = (1, b)").exists { case _: Ty.GroupT => true; case _ => false })
      .and(expect.same(Some(Ty.Name("b", Vector.empty)), body("a = (b)")))
  }

  test("pragmas are collected") {
    expect.same(
      Right(Vector(Pragma(PragmaKind.Import, "network.base", "base"), Pragma(PragmaKind.Include, "byron", "byron"))),
      parse(";# import network.base as base\n;# include byron as byron\na = 1\n").map(_.pragmas)
    )
  }

  test("typed pragmas retain source order across kinds") {
    expect.same(
      Right(Vector(Pragma(PragmaKind.Include, "a", "a"), Pragma(PragmaKind.Import, "b", "b"), Pragma(PragmaKind.Include, "c", "c"))),
      parse(";# include a as a\n;# import b as b\n;# include c as c\nx = 1\n").map(_.pragmas)
    )
  }

  test("errors retain the exact parser expectation, source and position") {
    val input = "a = 1\n!"
    val parsed = CddlParser.parse(input)
    expect
      .same(Left((2, 1, 6, "!")), parsed.left.map(e => (e.line, e.column, e.cause.failedAtOffset, e.near)))
      .and(expect.same(Left(List(Parser.Expectation.EndOfString(6, 7))), parsed.left.map(_.cause.expected.toList)))
      .and(expect.same(Left(Some(input)), parsed.left.map(_.cause.input)))
  }

  test("control operators are classified while unknown extensions retain their input name") {
    val controls = Vector(
      "size"      -> Control.Size,
      "regexp"    -> Control.Regexp,
      "cbor"      -> Control.Cbor,
      "default"   -> Control.Default,
      "gt"        -> Control.Compare(Comparison.Greater),
      "ge"        -> Control.Compare(Comparison.GreaterOrEqual),
      "lt"        -> Control.Compare(Comparison.Less),
      "le"        -> Control.Compare(Comparison.LessOrEqual),
      "ne"        -> Control.Compare(Comparison.NotEqual),
      "extension" -> Control.Other("extension")
    )
    controls.foldLeft(success) {
      case (acc, (wire, expected)) =>
        acc.and(expect.same(Some(Ty.Ctrl(Ty.Name("uint", Vector.empty), expected, Ty.Literal(Lit.IntL(3)))), body(s"a = uint .$wire 3")))
    }
  }
}
