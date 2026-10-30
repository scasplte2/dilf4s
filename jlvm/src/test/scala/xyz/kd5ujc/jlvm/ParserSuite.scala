package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.Sha256

import scodec.bits.hex
import weaver.SimpleIOSuite

object ParserSuite extends SimpleIOSuite {

  private def p(json: String) = Parser.parse(json).map(_.expr)
  private def int(i:  Int): Expr = Expr.Lit(CInt(BigInt(i)))
  private def text(s: String): Expr = Expr.Lit(CanonicalValue.text(s).toOption.get)

  pureTest("operations, arguments, arrays as data, and the null literal") {
    expect
      .same(Right(Expr.Apply(Op.Add, Vector(int(1), int(2)))), p("""{"+": [1, 2]}"""))
      .and(expect.same(Right(Expr.Apply(Op.Not, Vector(Expr.Lit(CBool(true))))), p("""{"!": true}""")))
      .and(expect.same(Right(Expr.Arr(Vector(text("+"), int(1), int(2)))), p("""["+", 1, 2]""")))
      .and(expect.same(Right(Expr.Lit(CNull)), p("null")))
      .and(expect.same(Right(Expr.Apply(Op.StartsWith, Vector(text("abc"), text("a")))), p("""{"startsWith": ["abc", "a"]}""")))
  }

  pureTest("var paths: data paths, $-bindings, defaults, integers") {
    expect
      .same(Right(Expr.Var(VarRef.Data(Vector("a", "b")), None)), p("""{"var": "a.b"}"""))
      .and(expect.same(Right(Expr.Var(VarRef.Bound("current", Vector("price")), None)), p("""{"var": "$current.price"}""")))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector.empty), None)), p("""{"var": ""}""")))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector("x")), Some(CInt(5)))), p("""{"var": ["x", 5]}""")))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector("0")), None)), p("""{"var": 0}""")))
      .and(expect(p("""{"var": "a..b"}""").isLeft, "empty segment"))
      .and(expect(p("""{"var": "$"}""").isLeft, "empty binding name"))
  }

  pureTest("0x-hex strings are bytes; anything else stays text") {
    expect
      .same(Right(Expr.Lit(CBytes(hex"00ff"))), p("\"0x00ff\""))
      .and(expect.same(Right(Expr.Lit(CBytes(hex""))), p("\"0x\"")))
      .and(expect.same(Right(text("0x0")), p("\"0x0\"")))
      .and(expect.same(Right(text("0xAB")), p("\"0xAB\"")))
      .and(expect.same(Right(CBytes(hex"abcd")), JsonSurface.parse("\"0xabcd\"")))
      .and(expect.same(Right("\"0xabcd\""), JsonSurface.render(CBytes(hex"abcd"))))
  }

  pureTest("single-key objects: always operations by default; quoting; lenient reading for older programs") {
    val mapLit = Expr.Obj(Map("price" -> int(5)))
    expect(p("""{"price": 5}""").isLeft, "strict (default): an unknown single key is an error")
      .and(expect(p("""{"iff": [true, 1, 2]}""").isLeft, "a typo is rejected"))
      .and(expect.same(Right(mapLit), Parser.parse("""{"price": 5}""", Parser.Mode.Lenient).map(_.expr)))
      .and(expect.same(Right(Expr.Obj(Map("price" -> int(5), "qty" -> int(1)))), p("""{"price": 5, "qty": 1}""")))
      .and(expect.same(Right(Expr.Lit(CanonicalValue.map(List("if" -> CInt(1))).toOption.get)), p("""{"literal": {"if": 1}}""")))
      .and(expect.same(Right(Expr.Obj(Map("a" -> Expr.Lit(CNull), "b" -> int(1)))), p("""{"a": null, "b": 1}""")))
  }

  pureTest("strictly valid JSON only: duplicate keys and excessive depth are rejected") {
    expect(p("""{"a": 1, "a": 2}""").isLeft, "duplicate key")
      .and(expect(p(("[" * 300) + ("]" * 300)).isLeft, "depth 300"))
      .and(expect(p("1e-1000000000").isLeft, "unbounded literal scale"))
  }

  pureTest("the canonical program form round-trips, and identity is stable") {
    val src =
      """{"if": [{"==": [{"var": "$current.a"}, "0x01"]}, {"map": [{"var": "items"}, {"+": [{"var": "$current"}, 1]}]}, {"k": null, "v": [1, 2.5]}]}"""
    val prog = Parser.parse(src).toOption.get
    val bytes = Parser.encode(prog)
    expect
      .same(Right(prog), bytes.flatMap(Parser.decode))
      .and(expect.same(Parser.id[Sha256](prog), bytes.flatMap(Parser.decode).flatMap(Parser.id[Sha256](_))))
      .and(expect(Parser.decode(hex"a0").isLeft, "a bare map is not a program"))
  }
}
