package xyz.kd5ujc.jlvm

import cats.Show

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.Sha256

import io.circe.Json
import org.scalacheck.Gen
import scodec.bits.hex
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ParserSuite extends SimpleIOSuite with Checkers {

  private def p(json: String) = Parser.parse(json).map(_.expr)
  private def int(i:  Int): Expr = Expr.Lit(CInt(BigInt(i)))
  private def text(s: String): Expr = Expr.Lit(CanonicalValue.text(s).toOption.get)

  pureTest("operations, arguments, arrays as data, and the null literal") {
    expect
      .same(Right(Expr.Apply(Op.Add, Vector(int(1), int(2)))), p("""{"+": [1, 2]}"""))
      .and(expect.same(Right(Expr.Apply(Op.Not, Vector(Expr.Lit(CBool(true))))), p("""{"!": true}""")))
      .and(expect.same(Right(Expr.Arr(Vector(text("a"), int(1), int(2)))), p("""["a", 1, 2]""")))
      .and(expect.same(Right(Expr.Lit(CNull)), p("null")))
      .and(expect.same(Right(Expr.Apply(Op.StartsWith, Vector(text("abc"), text("a")))), p("""{"startsWith": ["abc", "a"]}""")))
  }

  pureTest("metakit's array syntax: strict rejects it, lenient reads it as the call, and data quotes it") {
    def lenient(json: String) = Parser.parse(json, Parser.Mode.Lenient).map(_.expr)
    val sum = Expr.Apply(Op.Add, Vector(int(1), int(2)))
    expect(p("""["+", 1, 2]""").isLeft, "strict: a call in array syntax")
      .and(expect(p("""{"in": ["a", ["var", "x"]]}""").isLeft, "strict: var heads count too"))
      .and(expect.same(Right(sum), lenient("""["+", 1, 2]""")))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector("a")), Some(CInt(BigInt(5))))), lenient("""["var", "a", 5]""")))
      .and(
        expect.same(
          Right(Expr.Lit(CanonicalValue.array(Vector(CanonicalValue.text("+").toOption.get)).toOption.get)),
          p("""{"literal": ["+"]}""")
        )
      )
      .and(
        expect
          .same(Right(Expr.Arr(Vector(text("max"), Expr.Var(VarRef.Data(Vector("x")), None)))), p("""[{"literal": "max"}, {"var": "x"}]"""))
      )
      .and(expect(p("""{"var": [["values", "x"]]}""").isRight, "a computed path's segments are data"))
      .and(
        expect.same(
          Right("""{"+":[1,2]}"""),
          Parser.migrate("""["+", 1, 2]""")
        )
      )
  }

  pureTest("an array headed by a call's name renders quoted, so it reads back as the same array") {
    val arr = Expr.Arr(Vector(text("max"), Expr.Var(VarRef.Data(Vector("x")), None)))
    val back = Program.from(arr).flatMap(Parser.render).map(_.noSpaces).flatMap(Parser.parse(_)).map(_.expr)
    expect.same(Right(arr), back)
  }

  pureTest("metakit operations removed in v1 are rejected in both modes, naming their replacement") {
    def message(e: Either[JlvmError, ?]) = e.left.toOption.map(_.toString).getOrElse("")
    Parser.Removed.keys.toList.foldLeft(success) { (acc, name) =>
      val json = s"""{"$name": [1]}"""
      acc
        .and(expect(message(p(json)).contains("removed in JLVM v1"), s"strict $name"))
        .and(expect(message(Parser.parse(json, Parser.Mode.Lenient)).contains("removed in JLVM v1"), s"lenient $name"))
        .and(expect(message(Parser.parse(s"""["$name", 1]""", Parser.Mode.Lenient)).contains("removed in JLVM v1"), s"array $name"))
    }
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

  private def v(path: String): Expr = Expr.Var(VarRef.parse(path).toOption.get, None)

  pureTest("{\"object\": ...} is a map expression with any keys, including operation names") {
    expect
      .same(Right(Expr.Obj(Map("count" -> Expr.Apply(Op.Add, Vector(int(1), int(2)))))), p("""{"object": {"count": {"+": [1, 2]}}}"""))
      .and(expect.same(Right(Expr.Obj(Map.empty)), p("""{"object": {}}""")))
      .and(expect.same(Right(Expr.Obj(Map("a" -> int(1), "b" -> int(2)))), p("""{"object": {"a": 1, "b": 2}}""")))
      .and(expect(p("""{"object": [1]}""").isLeft, "object takes an object"))
  }

  pureTest("a single key starting with _ is data: host triggers read as map expressions") {
    expect
      .same(
        Right(Expr.Obj(Map("_result" -> Expr.Obj(Map("valid" -> Expr.Lit(CBool(false)), "error" -> text("no")))))),
        p("""{"_result": {"valid": false, "error": "no"}}""")
      )
      .and(expect.same(Right(Expr.Obj(Map("_emit" -> Expr.Arr(Vector(v("x")))))), p("""{"_emit": [{"var": "x"}]}""")))
      .and(expect(Op.values.forall(o => !o.wire.startsWith(Parser.DataPrefix)), "no operation name starts with _"))
      .and(
        expect(
          (Op.values.map(_.wire).toSet ++ Op.Aliases.keySet).intersect(Set(Parser.Literal, Parser.Variable, Parser.ObjectKey)).isEmpty,
          "the surface keywords are not operations"
        )
      )
      .and(expect(Op.Aliases.keySet.intersect(Op.values.map(_.wire).toSet).isEmpty, "aliases do not shadow operations"))
  }

  pureTest("spreading is decided by the syntax: a variadic operation with a non-array argument") {
    val values = Expr.Apply(Op.Values, Vector(v("balances")))
    expect
      .same(Right(Expr.Spread(Op.Add, values)), p("""{"+": {"values": {"var": "balances"}}}"""))
      .and(expect.same(Right(Expr.Apply(Op.Add, Vector(values))), p("""{"+": [{"values": {"var": "balances"}}]}""")))
      .and(expect.same(Right(Expr.Spread(Op.Max, v("xs"))), p("""{"max": {"var": "xs"}}""")))
      .and(expect.same(Right(Expr.Apply(Op.Length, Vector(v("xs")))), p("""{"length": {"var": "xs"}}""")))
      .and(expect(Program.from(Expr.Spread(Op.Length, v("xs"))).isLeft, "only variadic operations spread"))
      .and(expect(Program.from(Expr.Spread(Op.Add, Expr.Arr(Vector(int(1))))).isLeft, "an array expression is an argument list"))
  }

  pureTest("var paths may be computed: text is a var path, an array the exact segments") {
    val cat = Expr.Apply(Op.Cat, Vector(text("content.balances."), v("from")))
    expect
      .same(Right(Expr.VarAt(cat, None)), p("""{"var": {"cat": ["content.balances.", {"var": "from"}]}}"""))
      .and(expect.same(Right(Expr.VarAt(Expr.Arr(Vector(text("m"), v("k"))), Some(CInt(0)))), p("""{"var": [["m", {"var": "k"}], 0]}""")))
      .and(expect.same(Right(Expr.VarAt(text("a"), None)), p("""{"var": [{"literal": "a"}]}""")))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector("a")), Some(CInt(1)))), p("""{"var": ["a", 1]}""")))
      .and(expect(p("""{"var": true}""").isLeft, "a boolean is no path"))
  }

  pureTest("the SDK's names are aliases: accepted, and normalised in the canonical form") {
    val prog = Parser.parse("""{"getKey": [{"var": "m"}, "k"]}""").toOption.get
    expect
      .same(Expr.Apply(Op.Get, Vector(v("m"), text("k"))), prog.expr)
      .and(expect.same(Right(Expr.Apply(Op.Length, Vector(v("m")))), p("""{"size": {"var": "m"}}""")))
      .and(expect.same(Right(Expr.Apply(Op.Set, Vector(v("m"), text("k"), int(1)))), p("""{"setKey": [{"var": "m"}, "k", 1]}""")))
      .and(expect.same(Right(Expr.Apply(Op.Unset, Vector(v("m"), text("k")))), p("""{"deleteKey": [{"var": "m"}, "k"]}""")))
      .and(expect.same(Parser.parse("""{"get": [{"var": "m"}, "k"]}""").flatMap(Parser.encode), Parser.encode(prog)))
  }

  pureTest("computed paths and spreads round-trip through the canonical form") {
    val src = """{"+": {"map": [{"var": "xs"}, {"var": [["o", {"var": "$current"}], 0]}]}}"""
    val prog = Parser.parse(src).toOption.get
    expect.same(Right(prog), Parser.encode(prog).flatMap(Parser.decode))
  }

  // ---- the strict surface, written back

  private val keys = Gen.oneOf("a", "b", "count", "if", "var", "literal", "object", "_result", "x.y", "")
  private val texts = Gen.oneOf("", "a", "+", "0x0", "0xAB", "{}", "if", "é")

  private def lit: Gen[CanonicalValue] =
    Gen.oneOf[CanonicalValue](
      Gen.oneOf(CNull, CBool(true), CBool(false)),
      Gen.choose(-5, 5).map(i => CInt(BigInt(i))),
      texts.map(t => CanonicalValue.text(t).toOption.get),
      Gen.const(CBytes(hex"00ff")),
      Gen.const(CanonicalValue.array(Vector(CInt(1), CBytes(hex"01"))).toOption.get),
      keys.map(k => CanonicalValue.map(List(k -> CInt(1))).toOption.get)
    )

  /** A default is never null (a null default is no default): mapped rather than filtered, so nothing is discarded. */
  private def default: Gen[CanonicalValue] = lit.map(d => if (d == CNull) CInt(1) else d)

  private def expr(depth: Int): Gen[Expr] = {
    val leaf = Gen.oneOf[Expr](
      lit.map(Expr.Lit(_)),
      Gen.oneOf("a", "a.b", "", "$current", "$x.0", "0").map(v),
      Gen.zip(Gen.oneOf("a", "$x"), default).map { case (q, d) => Expr.Var(VarRef.parse(q).toOption.get, Some(d)) }
    )
    if (depth <= 0) leaf
    else {
      val sub = expr(depth - 1)
      Gen.frequency(
        3 -> leaf,
        2 -> Gen.zip(Gen.oneOf(Op.Add, Op.If, Op.Not, Op.Map, Op.Values), Gen.listOfN(2, sub)).map {
          case (o, as) => Expr.Apply(o, as.toVector)
        },
        1 -> Gen
          .zip(
            Gen.oneOf(Op.Variadic.toList),
            // an array expression would be an argument list: spread its first item instead
            sub.map {
              case Expr.Arr(items) => items.headOption.getOrElse(v("a"))
              case e               => e
            }
          )
          .map { case (o, a) => Expr.Spread(o, a) },
        1 -> Gen.zip(sub, Gen.option(default)).map { case (q, d) => Expr.VarAt(q, d) },
        1 -> Gen.listOfN(2, sub).map(is => Expr.Arr(is.toVector)),
        2 -> Gen.choose(0, 3).flatMap(n => Gen.listOfN(n, Gen.zip(keys, sub))).map(fs => Expr.Obj(fs.toMap))
      )
    }
  }

  private given Show[Program] = Show.fromToString

  /** Literals holding 0x-hex text have no surface form; the generator's "0x0" and "0xAB" are not hex bytes. */
  private val programs: Gen[Program] = Gen.choose(0, 4).flatMap(expr).map(e => Program.from(e).toOption.get)

  test("render writes the strict surface: parsing it back, in either mode, gives the same program") {
    forall(programs) { prog =>
      val text = Parser.render(prog).map(_.noSpaces)
      expect
        .same(Right(prog), text.flatMap(Parser.parse(_)))
        .and(expect.same(Right(prog), text.flatMap(Parser.parse(_, Parser.Mode.Lenient))))
    }
  }

  /** The same program as metakit wrote it: a single-key data map bare wherever its key is not an operation. */
  private def metakitStyle(j: Json): Json =
    j.asObject.map(_.toList) match {
      case Some(List((Parser.ObjectKey, inner))) =>
        inner.asObject.map(_.toList) match {
          case Some(List((k, v))) if Op.fromWire(k).isEmpty && !Set(Parser.Literal, Parser.Variable, Parser.ObjectKey).contains(k) =>
            Json.obj(k -> metakitStyle(v))
          case _ => Json.obj(Parser.ObjectKey -> metakitStyle(inner))
        }
      case Some(List((Parser.Literal, _))) => j
      case Some(List((Parser.Variable, arg))) =>
        arg.asArray match {
          case Some(path +: default) => Json.obj(Parser.Variable -> Json.fromValues(metakitStyle(path) +: default))
          case _                     => Json.obj(Parser.Variable -> metakitStyle(arg))
        }
      case Some(fields) => Json.fromFields(fields.map { case (k, v) => k -> metakitStyle(v) })
      case scala.None   => j.asArray.fold(j)(a => Json.fromValues(a.map(metakitStyle)))
    }

  test("migrate reads a metakit-style program as metakit did, and writes the same program in the strict surface") {
    forall(programs) { prog =>
      val old = Parser.render(prog).map(j => metakitStyle(j).noSpaces)
      expect
        .same(Right(prog), old.flatMap(Parser.parse(_, Parser.Mode.Lenient)))
        .and(expect.same(Right(prog), old.flatMap(Parser.migrate).flatMap(Parser.parse(_))))
    }
  }

  pureTest("a var default is a literal; the strict surface quotes object and array defaults") {
    val m = CanonicalValue.map(List("var" -> CanonicalValue.text("y").toOption.get)).toOption.get
    expect
      .same(Right(Expr.Var(VarRef.Data(Vector("x")), Some(m))), p("""{"var": ["x", {"literal": {"var": "y"}}]}"""))
      .and(expect(p("""{"var": ["x", {"var": "y"}]}""").isLeft, "an unquoted object default could be read as code"))
      .and(expect(p("""{"var": ["x", []]}""").isLeft, "so could an array"))
      .and(
        expect.same(
          Right(Expr.Var(VarRef.Data(Vector("x")), Some(m))),
          Parser.parse("""{"var": ["x", {"var": "y"}]}""", Parser.Mode.Lenient).map(_.expr)
        )
      )
  }

  pureTest("values with no surface form are reported, not rendered wrongly") {
    expect(Parser.render(Program.from(Expr.Lit(CanonicalValue.text("0xab").toOption.get)).toOption.get).isLeft, "0x-hex text")
      .and(
        expect(
          Parser.render(Program.from(Expr.Lit(CanonicalValue.number(xyz.kd5ujc.numerics.Ratio.of(1, 3).toOption.get))).toOption.get).isLeft,
          "1/3"
        )
      )
  }

  pureTest("migrate rewrites a metakit program into the strict surface, with the same meaning") {
    val old = """{"if": [{"==": [{"var": "method"}, "inc"]}, {"value": {"+": [{"var": "state.value"}, 1]}}, {"count": [1, 2]}]}"""
    val migrated = Parser.migrate(old)
    expect
      .same(
        Right("""{"if":[{"==":[{"var":"method"},"inc"]},{"object":{"value":{"+":[{"var":"state.value"},1]}}},{"count":[1,2]}]}"""),
        migrated
      )
      .and(expect.same(Parser.parse(old, Parser.Mode.Lenient), migrated.flatMap(Parser.parse(_))))
      .and(expect(Parser.parse(old).isLeft, "the old text is not strict"))
  }
}
