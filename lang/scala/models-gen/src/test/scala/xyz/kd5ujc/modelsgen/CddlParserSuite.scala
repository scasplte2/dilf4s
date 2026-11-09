package xyz.kd5ujc.modelsgen

import cats.syntax.all._

import xyz.kd5ujc.modelsgen.Ast._

import weaver.FunSuite

object CddlParserSuite extends FunSuite {

  private def parse(text: String): Either[String, File] = CddlParser.parse(text).leftMap(_.message)
  private def body(text:  String): Option[Ty] = parse(text).toOption.flatMap(_.rules.headOption).map(_.body)
  private def firstGroup(t: Option[Ty]): Option[Vector[Entry]] = t.collect {
    case Ty.Array(Group(Vector(es))) => es
    case Ty.MapT(Group(Vector(es)))  => es
  }

  test("rules, size control and bytes") {
    expect.same(
      Right(Vector(Rule("hash32", Vector.empty, Assign.Eq, Ty.Ctrl(Ty.Name("bytes", Vector.empty), "size", Ty.Literal(Lit.IntL(32)))))),
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
      Right(Vector(Pragma("import", "network.base", "base"), Pragma("include", "byron", "byron"))),
      parse(";# import network.base as base\n;# include byron as byron\na = 1\n").map(_.pragmas)
    )
  }

  test("errors carry a position") {
    expect(CddlParser.parse("a = [\nb = c").left.exists(e => e.line >= 1 && e.column >= 1))
  }
}
