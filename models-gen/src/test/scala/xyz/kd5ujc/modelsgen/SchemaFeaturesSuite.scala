package xyz.kd5ujc.modelsgen

import cats.syntax.all._

import weaver.{Expectations, FunSuite}

/**
 * The emitter features the codec and JLVM error families need, in every language: comments carried into the generated
 * documentation, closed choices of named texts (the JLVM's operations), error families nested in error families, and
 * error integers wider than the SDK's sizes.
 */
object SchemaFeaturesSuite extends FunSuite {

  private final case class Out(scala: String, rust: String, ts: String)

  private def gen(cddl: String): Either[GenError, Out] =
    (
      Main.generate(Vector("t" -> cddl), "p").map(_.foldMap(_._2)),
      SdkSources.generate(Vector("t" -> cddl)).map(s => (s.rust.toMap.getOrElse("t.rs", ""), s.typescript.toMap.getOrElse("t.ts", "")))
    ).mapN { case (scala, (rust, ts)) => Out(scala, rust, ts) }

  private def outside(e: Either[GenError, ?]): Boolean =
    e.swap.exists { case GenError.Unsupported(_, UnsupportedFeature.OutsideProfile(_)) => true; case _ => false }

  /** Refused by the shared profile, so by every language. */
  private def refused(cddl: String): Boolean =
    outside(Main.generate(Vector("t" -> cddl), "p")) && outside(SdkSources.generate(Vector("t" -> cddl)))

  private def has(out: Either[GenError, Out], language: Out => String, text: String): Expectations =
    expect(out.exists(o => language(o).contains(text)), out.fold(_.message, o => s"missing:\n$text\nin:\n${language(o)}"))

  private def lacks(out: Either[GenError, Out], text: String): Expectations =
    expect(out.exists(o => !o.scala.contains(text) && !o.rust.contains(text) && !o.ts.contains(text)), s"$text is documented somewhere")

  private val documented =
    "; A file header, which a blank line separates from the first rule.\n" +
    "\n" +
    "; A family */ of refusals.\n" +
    "e = {\n" +
    "  ; The first kind.\n" +
    "  \"kind\": \"A\",\n" +
    "  ; Its [size].\n" +
    "  \"n\": uint .le 10 //\n" +
    "  \"kind\": \"B\"\n" +
    "}\n" +
    "\n" +
    "e-kind = \"A\" / \"B\"\n" +
    "\n" +
    "; A record.\n" +
    "r = {\n" +
    "  ; The x entry, documented.\n" +
    "  \"x\": uint,\n" +
    "  \"y\": text, ; after the entry, so not its documentation\n" +
    "}\n"

  test("a comment directly above a rule, a kind or a field is its documentation in every language") {
    val out = gen(documented)
    has(
      out,
      _.scala,
      "/**\n * `e` (`shared/schemas/t.cddl`): an error family, by kind ([[EKind]]).\n *\n * A family *\\/ of refusals.\n */\nsealed trait E"
    )
      .and(has(out, _.scala, "  /**\n   * The first kind.\n   *\n   * @param n Its [size].\n   */\n  final case class A(n: Int)"))
      .and(has(out, _.scala, "/**\n * A record.\n *\n * @param x The x entry, documented.\n */\nfinal case class R(x: BigInt, y: String)"))
      .and(
        has(out, _.rust, "/// `e` (`shared/schemas/t.cddl`). An error family: its kinds are [`EKind`].\n///\n/// A family */ of refusals.")
      )
      .and(has(out, _.rust, "    /// The first kind.\n    A {\n        /// Its \\[size\\].\n        n: u32,"))
      .and(has(out, _.rust, "    /// The kind `B`.\n    B,"))
      .and(has(out, _.rust, "    /// The x entry, documented.\n    pub x: u64,\n    /// The `y` entry.\n    pub y: String,"))
      .and(
        has(
          out,
          _.ts,
          " * A family *\\/ of refusals.\n */\nexport type E =\n  /** The first kind. */\n  | {\n      readonly kind: \"A\";\n      /** Its [size]. */\n      readonly n: number;\n    }\n  | { readonly kind: \"B\" };"
        )
      )
      .and(has(out, _.ts, "export interface R {\n  /** The x entry, documented. */\n  readonly x: bigint;\n  readonly y: string;\n}"))
      .and(lacks(out, "file header"))
      .and(lacks(out, "after the entry"))
  }

  test("a comment's own delimiters never open or close a generated comment: Scala's comments nest") {
    val cddl =
      "; Paths under data/* and a /* block */ example, closed */ early.\n" +
      "r = {\n  ; Each of data/** too.\n  \"x\": uint,\n}\n" +
      "e-kind = \"A\" / \"B\"\n" +
      "e = {\n  ; Kind data/*.\n  \"kind\": \"A\" //\n  \"kind\": \"B\"\n}\n" +
      "o = o-a / o-b\n; Member data/*.\no-a = \"a\"\no-b = \"b\"\n"
    val out = gen(cddl)
    // every comment the generated Scala opens is closed once: no opener or closer inside the prose
    def balanced(s: String) = "/\\*".r.findAllMatchIn(s).size == "\\*/".r.findAllMatchIn(s).size
    expect(out.exists(o => balanced(o.scala)), out.fold(_.message, _.scala))
      .and(has(out, _.scala, "Paths under data/&#42; and a /&#42; block *\\/ example, closed *\\/ early."))
      // TypeScript's comments do not nest: only the closer is escaped
      .and(has(out, _.ts, "Paths under data/* and a /* block *\\/ example, closed *\\/ early."))
  }

  test("CddlParser.docs takes the comment block directly above a line's first token, and nothing else") {
    val text = ";# import a as b\n; one\n;two\n  x = 1 ; trailing\ny = 2\n\n; apart\n\nz = 3\n"
    expect
      .same(Vector("one", "two"), CddlParser.docs(text, text.indexOf("x =")))
      .and(expect.same(Vector.empty[String], CddlParser.docs(text, text.indexOf("1 ;"))))
      .and(expect.same(Vector.empty[String], CddlParser.docs(text, text.indexOf("y ="))))
      .and(expect.same(Vector.empty[String], CddlParser.docs(text, text.indexOf("z ="))))
  }

  private val ops =
    "; An operation.\n" +
    "op = op-not / op-strict-eq / op-sha3_256 / op-some\n" +
    "\n" +
    "; Negation.\n" +
    "op-not = \"!\"\n" +
    "op-strict-eq = \"===\"\n" +
    "op-sha3_256 = \"sha3_256\"\n" +
    "op-some = \"some\"\n" +
    "site = op / \"var\"\n" +
    "call = { \"op\": op, \"at\": site }\n"

  test("a choice of rules of one text literal is a closed type named by its rules, written as its texts") {
    val out = gen(ops)
    has(
      out,
      _.scala,
      "/** An operation. */\nenum Op(val wire: String) {\n  /** Negation. */\n  case Not extends Op(\"!\")\n  case StrictEq extends Op(\"===\")"
    )
      .and(has(out, _.scala, "  case Sha3_256 extends Op(\"sha3_256\")\n  case Some extends Op(\"some\")"))
      .and(has(out, _.scala, "def fromWire(wire: String): Option[Op] = byWire.get(wire)"))
      .and(has(out, _.scala, "List((\"!\", Op.Not), (\"===\", Op.StrictEq), (\"sha3_256\", Op.Sha3_256), (\"some\", Op.Some))"))
      .and(has(out, _.scala, "type Site = \"var\" | Op"))
      .and(has(out, _.scala, "val values: Vector[Site] = Vector(\"var\", Op.Not, Op.StrictEq, Op.Sha3_256, Op.Some)"))
      .and(has(out, _.scala, "final case class Call(op: Op, at: Site)"))
      .and(
        has(
          out,
          _.rust,
          "#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]\npub enum Op {\n    /// Negation.\n    Not,\n    /// The text `===`.\n    StrictEq,"
        )
      )
      .and(has(out, _.rust, "pub const ALL: [Self; 4] = [Self::Not, Self::StrictEq, Self::Sha3_256, Self::Some];"))
      .and(has(out, _.rust, "            Self::Sha3_256 => \"sha3_256\","))
      .and(has(out, _.rust, "            \"!\" => Some(Self::Not),"))
      .and(
        has(
          out,
          _.rust,
          "#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]\npub enum Site {\n    /// The text `var`.\n    Var,\n    /// A [`Op`].\n    Op(Op),\n}"
        )
      )
      .and(has(out, _.rust, "c::Alternative::Text(1) => Ok(Self::Op(Op::Not)),"))
      .and(has(out, _.rust, "            Self::Var => f.write_str(\"var\"),\n            Self::Op(a) => core::fmt::Display::fmt(a, f),"))
      .and(has(out, _.ts, "export type Op =\n  /** Negation. */\n  | \"!\"\n  | \"===\"\n  | \"sha3_256\"\n  | \"some\";"))
      .and(has(out, _.ts, "export type Site = \"var\" | Op;"))
      .and(expect(gen(ops).exists(o => !o.rust.contains("OP_NOT") && !o.scala.contains("OpNot")), "a member's own rule is not emitted"))
  }

  test("a choice of named texts is checked in every language") {
    expect(refused("x = a / \"b\"\na = \"a\"\n"), "a member mixed with a text")
      .and(expect(refused("x = a / r\na = \"a\"\nr = { \"k\": uint }\n"), "a member mixed with a record"))
      .and(expect(refused("x = p-a / p-b\np-a = \"!\"\np-b = \"!\"\n"), "a text twice"))
      .and(expect(refused("x = p-1 / p-2\np-1 = \"a\"\np-2 = \"b\"\n"), "a member with no identifier form"))
      .and(
        expect(
          Main.generate(Vector("t" -> "x = p-a-b / p-a--b\np-a-b = \"a\"\np-a--b = \"b\"\n"), "p").swap.exists {
            case GenError.NameClash(_, _, _) => true
            case _                           => false
          },
          "two members named alike"
        )
      )
  }

  private val nested =
    "inner-kind = \"X\" / \"Y\"\n" +
    "inner = { \"kind\": \"X\", \"n\": uint .le 5 // \"kind\": \"Y\" }\n" +
    "outer-kind = \"P\" / \"Q\"\n" +
    "outer = { \"kind\": \"P\", \"cause\": inner // \"kind\": \"Q\", ? \"maybe\": inner, \"tag\": uint }\n"

  test("an error family's field may hold another error family, never itself; a wide integer stays exact") {
    val out = gen(nested)
    has(out, _.scala, "final case class P(cause: Inner) extends Outer")
      .and(has(out, _.scala, "final case class Q(maybe: Option[Inner], tag: BigInt) extends Outer"))
      .and(has(out, _.rust, "        cause: Inner,"))
      .and(has(out, _.rust, "Self::P { cause: a0 } => write!(f, \"P (cause: {a0:?})\"),"))
      .and(has(out, _.ts, "| { readonly kind: \"P\"; readonly cause: Inner }"))
      .and(has(out, _.ts, "| { readonly kind: \"Q\"; readonly maybe?: Inner; readonly tag: bigint }"))
      .and(has(out, _.ts, "| { readonly kind: \"X\"; readonly n: number }"))
      .and(expect(refused("a-kind = \"A\" / \"Z\"\na = { \"kind\": \"A\", \"a\": a // \"kind\": \"Z\" }\n"), "a family holding itself"))
      .and(
        expect(
          refused(
            "a-kind = \"A\" / \"Z\"\na = { \"kind\": \"A\", \"b\": b // \"kind\": \"Z\" }\n" +
            "b-kind = \"B\" / \"W\"\nb = { \"kind\": \"B\", \"a\": a // \"kind\": \"W\" }\n"
          ),
          "two families holding each other"
        )
      )
      .and(expect(refused(nested + "r = { \"e\": inner }\n"), "a family in a wire type"))
  }

  test("Rust lifts a family into the one kind that is only that family; never two kinds' family, or an optional one") {
    val out = gen(nested)
    val twice = gen(
      "inner-kind = \"X\" / \"Y\"\ninner = { \"kind\": \"X\" // \"kind\": \"Y\" }\n" +
      "outer-kind = \"P\" / \"Q\"\nouter = { \"kind\": \"P\", \"a\": inner // \"kind\": \"Q\", \"b\": inner }\n"
    )
    has(out, _.rust, "impl From<Inner> for Outer {\n    fn from(cause: Inner) -> Self {\n        Self::P { cause }\n    }\n}")
      .and(expect(out.exists(_.rust.split("impl From<").length == 2), "one conversion: Q's inner is optional"))
      .and(expect(twice.exists(o => !o.rust.contains("impl From<")), "two kinds wrap inner: no conversion picks one"))
  }

  test("an error family has exactly one \"kind\", a literal") {
    expect(refused("e-kind = \"A\" / \"B\"\ne = { \"kind\": text // \"kind\": \"B\" }\n"), "a kind that is not a literal")
      .and(expect(refused("e-kind = \"A\" / \"B\"\ne = { \"kind\": \"A\", ? \"kind\": text // \"kind\": \"B\" }\n"), "a second kind"))
  }
}
