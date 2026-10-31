package xyz.kd5ujc.jlvm

import cats.Show

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.Sha256

import io.circe.Json
import org.scalacheck.Gen
import scodec.bits.hex
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

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

  pureTest("an array headed by a call's name is rejected, naming the call form; data quotes it") {
    def message(e: Either[JlvmError, ?]) = e.left.toOption.map(_.toString).getOrElse("")
    expect(p("""["+", 1, 2]""").isLeft, "an operation's name")
      .and(expect(p("""{"in": ["a", ["var", "x"]]}""").isLeft, "var heads count too"))
      .and(expect(p("""["getKey", 1]""").isLeft, "an alias heads count too"))
      .and(expect(message(p("""["+", 1, 2]""")).contains("""{"+": [...]}"""), "the message names the call form"))
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
  }

  pureTest("let binding names are never code, even when they spell an operation's name") {
    val json = """{"let":[[["values",[1,2]],["max",3]],{"var":"$values"}]}"""
    expect(p(json).isRight, "binds values and max")
      .and(expect.same(Right(json), Parser.parse(json).flatMap(Parser.render).map(_.noSpaces)))
  }

  pureTest("an array headed by a call's name renders quoted, so it reads back as the same array") {
    val arr = Expr.Arr(Vector(text("max"), Expr.Var(VarRef.Data(Vector("x")), None)))
    val back = Program.from(arr).flatMap(Parser.render).map(_.noSpaces).flatMap(Parser.parse(_)).map(_.expr)
    expect.same(Right(arr), back)
  }

  pureTest("names that read like operations but are not are rejected, naming the operation to use") {
    def message(e: Either[JlvmError, ?]) = e.left.toOption.map(_.toString).getOrElse("")
    Parser.Removed.toList.foldLeft(success) {
      case (acc, (name, use)) =>
        acc
          .and(expect(message(p(s"""{"$name": [1]}""")).contains(s""""$name" is not an operation: $use"""), name))
          .and(expect(message(p(s"""["$name", 1]""")).contains(s""""$name" is not an operation: $use"""), s"an array headed by $name"))
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

  pureTest("single-key objects are always operations; data is quoted") {
    expect(p("""{"price": 5}""").isLeft, "an unknown single key is an error")
      .and(expect(p("""{"iff": [true, 1, 2]}""").isLeft, "a typo is rejected"))
      .and(expect(p("""{"": "a"}""").isLeft, "the empty key is no operation"))
      .and(expect.same(Right(Expr.Obj(Map("price" -> int(5)))), p("""{"object": {"price": 5}}""")))
      .and(expect.same(Right(Expr.Apply(Op.Round, Vector(int(1)))), p("""{"round": [1]}""")))
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
            // an array expression would be an argument list (Program.from rejects it): spread its values instead
            sub.map {
              case a @ Expr.Arr(_) => Expr.Apply(Op.Values, Vector(a))
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

  private given Show[Expr] = Show.fromToString
  private given Show[VarRef] = Show.fromToString

  /**
   * Syntax trees Program.from accepts by construction: shallow (depth 4, far inside both depth bounds), spellable var
   * paths, non-null defaults, and no spread of an array expression. Literals holding 0x-hex text have no surface form;
   * the generator's "0x0" and "0xAB" are not hex bytes.
   */
  private val exprs: Gen[Expr] = Gen.choose(0, 4).flatMap(expr)

  /** A generated tree as a program; a tree Program.from rejects is a failure of the generator, reported, not thrown. */
  private def accepted(e: Expr)(f: Program => Expectations): Expectations =
    Program.from(e).fold(err => failure(s"the generator built a tree Program.from rejects: $err\n$e"), f)

  test("the program generator stays within Program.from's bounds: depth, surface depth and canonical depth") {
    forall(exprs) { e =>
      expect(Program.from(e).isRight, s"accepted: ${Program.from(e)}")
        .and(expect(Parser.surfaceDepth(e) <= Program.MaxSurfaceDepth, s"surface depth ${Parser.surfaceDepth(e)}"))
        .and(
          expect(
            Program.from(e).flatMap(Parser.canonical).forall(_.depth <= CanonicalValue.MaxDepth),
            "canonical depth within the value bound"
          )
        )
    }
  }

  test("render writes the surface: parsing it back gives the same program") {
    forall(exprs)(accepted(_) { prog =>
      expect.same(Right(prog), Parser.render(prog).map(_.noSpaces).flatMap(Parser.parse(_)))
    })
  }

  pureTest("a var default is a literal; object and array defaults are quoted") {
    val m = CanonicalValue.map(List("var" -> CanonicalValue.text("y").toOption.get)).toOption.get
    expect
      .same(Right(Expr.Var(VarRef.Data(Vector("x")), Some(m))), p("""{"var": ["x", {"literal": {"var": "y"}}]}"""))
      .and(expect(p("""{"var": ["x", {"var": "y"}]}""").isLeft, "an unquoted object default could be read as code"))
      .and(expect(p("""{"var": ["x", []]}""").isLeft, "so could an array"))
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

  // ---- identity: distinct behaviour never shares an encoding

  pureTest("a var ref must be what its text reads back as: segments the text cannot spell are rejected") {
    def from(ref: VarRef) = Program.from(Expr.Var(ref, None))
    expect(from(VarRef.Data(Vector("a.b"))).isLeft, "a segment holding '.' would encode as the path a, b")
      .and(expect(from(VarRef.Data(Vector("a", "b"))).isRight, "the path a, b"))
      .and(expect(from(VarRef.Data(Vector(""))).isLeft, "an empty segment would encode as the whole data"))
      .and(expect(from(VarRef.Data(Vector("a", ""))).isLeft, "an empty segment does not read back"))
      .and(expect(from(VarRef.Data(Vector("$x"))).isLeft, "a data path spelling a binding"))
      .and(expect(from(VarRef.Bound("x.y", Vector.empty)).isLeft, "a binding name holding '.'"))
      .and(expect(from(VarRef.Bound("", Vector("a"))).isLeft, "an empty binding name"))
      .and(expect(from(VarRef.Bound("x", Vector("a.b"))).isLeft, "a bound path segment holding '.'"))
      .and(expect(from(VarRef.Data(Vector.empty)).isRight, "the whole data"))
  }

  pureTest("a null default is no default: the syntax tree never holds one, since the canonical form drops it") {
    expect(Program.from(Expr.Var(VarRef.Data(Vector("a")), Some(CNull))).isLeft, "var")
      .and(expect(Program.from(Expr.Var(VarRef.Bound("x", Vector.empty), Some(CNull))).isLeft, "a bound var"))
      .and(expect(Program.from(Expr.VarAt(text("a"), Some(CNull))).isLeft, "a computed var"))
      .and(expect.same(Right(Expr.Var(VarRef.Data(Vector("a")), None)), p("""{"var": ["a", null]}""")))
      .and(expect.same(Right(Expr.VarAt(Expr.Arr(Vector(text("a"))), None)), p("""{"var": [["a"], null]}""")))
  }

  private val wildRefs: Gen[VarRef] = Gen.oneOf(
    Gen.choose(0, 2).flatMap(n => Gen.listOfN(n, Gen.oneOf("a", "b", "a.b", "", "$x", "0"))).map(s => VarRef.Data(s.toVector)),
    Gen
      .zip(Gen.oneOf("x", "", "x.y", "$x"), Gen.choose(0, 2).flatMap(n => Gen.listOfN(n, Gen.oneOf("a", "a.b", ""))))
      .map { case (n, s) => VarRef.Bound(n, s.toVector) }
  )

  test("every accepted program decodes from its encoding to itself, so no two share an encoding or an identity") {
    forall(Gen.zip(wildRefs, Gen.option(lit), Gen.option(lit))) {
      case (ref, d1, d2) =>
        val e = Expr.Arr(Vector(Expr.Var(ref, d1), Expr.VarAt(text("a"), d2)))
        Program.from(e).fold(_ => success, prog => expect.same(Right(prog), Parser.encode(prog).flatMap(Parser.decode)))
    }
  }

  pureTest("a program is built only by Program.from: no product mirror can bypass its checks") {
    expect(!scala.compiletime.testing.typeChecks("summon[scala.deriving.Mirror.ProductOf[Program]]"), "Program")
      .and(expect(!scala.compiletime.testing.typeChecks("summon[scala.deriving.Mirror.ProductOf[Compiled]]"), "Compiled"))
  }

  // ---- rendering: exact, within the depth bound

  pureTest("150 nested computed var paths render within the depth bound and read back as the same program") {
    val src = ("""{"var":""" * 150) + "\"a\"" + ("}" * 150)
    val prog = Parser.parse(src)
    expect(prog.isRight, "parses")
      .and(expect.same(prog, prog.flatMap(Parser.render).map(_.noSpaces).flatMap(Parser.parse(_))))
      .and(expect.same(Right(src), prog.flatMap(Parser.render).map(_.noSpaces)))
  }

  /** Non-empty arrays and objects nested, as the canonical JSON parser counts them. */
  private def nesting(j: Json): Int =
    j.arrayOrObject(
      0,
      a => if (a.isEmpty) 0 else 1 + a.foldLeft(0)((d, x) => math.max(d, nesting(x))),
      o => if (o.isEmpty) 0 else 1 + o.values.foldLeft(0)((d, x) => math.max(d, nesting(x)))
    )

  private val structured: Gen[CanonicalValue] =
    Gen.oneOf[CanonicalValue](
      CanonicalValue.array(Vector(CInt(1))).toOption.get,
      CanonicalValue.map(List("k" -> CanonicalValue.array(Vector(CInt(1))).toOption.get)).toOption.get,
      CanonicalValue.array(Vector.empty).toOption.get
    )

  private val chainLeaf: Gen[Expr] =
    Gen.oneOf[Expr](
      Gen.const(v("a")),
      Gen.zip(Gen.oneOf("a", "$x"), Gen.oneOf(structured, Gen.const(CInt(1)))).map {
        case (q, d) => Expr.Var(VarRef.parse(q).toOption.get, Some(d))
      },
      structured.map(Expr.Lit(_)),
      Gen.const(int(1))
    )

  /** Deep chains of every wrapper, so the bound is reached through each kind of nesting. */
  private def chain(n: Int): Gen[Expr] =
    (1 to n).foldLeft(chainLeaf) { (inner, _) =>
      inner.flatMap { e =>
        Gen.oneOf[Expr](
          Gen.const(Expr.VarAt(e, None)),
          Gen.oneOf(structured, Gen.const(CInt(0))).map(d => Expr.VarAt(e, Some(d))),
          Gen.const(Expr.Apply(Op.Not, Vector(e))),
          Gen.const(Expr.Arr(Vector(e))),
          Gen.const(Expr.Obj(Map("k" -> e))),
          Gen.const(Expr.Obj(Map("_k" -> e))),
          Gen.const(e match {
            case Expr.Arr(_) => Expr.Spread(Op.Add, Expr.Obj(Map("k" -> e)))
            case other       => Expr.Spread(Op.Add, other)
          })
        )
      }
    }

  test("every program Program.from accepts, the deepest included, renders within the bound and reads back exactly") {
    forall(Gen.choose(60, 260).flatMap(chain)) { e =>
      Program.from(e) match {
        case Left(err) => expect(err.isInstanceOf[JlvmError.Limit], s"only the depth bound rejects a chain: $err")
        case Right(prog) =>
          val rendered = Parser.render(prog)
          expect
            .same(Right(prog), rendered.map(_.noSpaces).flatMap(Parser.parse(_)))
            .and(expect(rendered.forall(nesting(_) <= CanonicalValue.MaxDepth), "within the JSON depth bound"))
      }
    }
  }

  test("the surface depth Program.from bounds is the depth of the rendered text") {
    // chains of 40 wrappers nest at most 3 levels each (surface or canonical), plus a few for the leaf: within the bound
    forall(Gen.oneOf(exprs, Gen.choose(1, 40).flatMap(chain)))(accepted(_) { prog =>
      expect.same(Parser.render(prog).map(nesting), Right(Parser.surfaceDepth(prog.expr)))
    })
  }

  // ---- results: 0x-hex text has no surface form

  private def t(s: String): CanonicalValue = CanonicalValue.text(s).toOption.get

  pureTest("rendering a value checks it reads back: 0x-hex text, at any depth, is an error rather than bytes") {
    val hexText = t("0xab")
    expect(JsonSurface.render(hexText).isLeft, "text 0xab")
      .and(expect(JsonSurface.render(CanonicalValue.array(Vector(CInt(1), hexText)).toOption.get).isLeft, "in an array"))
      .and(expect(JsonSurface.render(CanonicalValue.map(List("k" -> hexText)).toOption.get).isLeft, "in a map"))
      .and(expect.same(Right("\"0xab\""), JsonSurface.render(CBytes(hex"ab"))))
      .and(expect.same(Right("""{"0xab":"0xab"}"""), JsonSurface.render(CanonicalValue.map(List("0xab" -> CBytes(hex"ab"))).toOption.get)))
      .and(expect.same(Right("\"0xAB\""), JsonSurface.render(t("0xAB"))))
  }
}
