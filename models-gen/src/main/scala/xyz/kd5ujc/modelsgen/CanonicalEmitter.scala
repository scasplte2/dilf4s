package xyz.kd5ujc.modelsgen

import cats.syntax.all._

/**
 * Emits Scala 3 types and `xyz.kd5ujc.core.canonical.Codec`s for the **dilf4s canonical profile** of CDDL: the subset
 * that fits the canonical data model (docs/specs/canonical-encoding.md). The profile itself lives in [[Profile]], which
 * the TypeScript and Rust emitters share.
 *
 *   - a map of text-literal keys (each required or `?` optional) is a case class
 *   - a labelled integer enumeration `&(a: 1, b: 2)` is an enum carrying its wire values
 *   - a choice of map rules is a union type, decoded by key set; the choice must be unambiguous (some required key of
 *     one alternative is outside the other, for every pair), which is checked here, not at run time
 *   - anything else in the profile (a primitive, a range, a sized string, a homogeneous array, a reference) is an alias
 *
 * Everything outside the profile (integer map keys, tags, floats, groups, inline choices) is a typed [[GenError]].
 * Output is deterministic: rules in name order, one file per module.
 */
final class CanonicalEmitter(schema: Schema, packagePrefix: String) {

  import CanonicalEmitter._

  private val profile = new Profile(schema)

  def typeName(q: QName): String = pascal(q.name)
  private def qualified(from: String, q: QName): String =
    if (q.module == from) typeName(q) else s"$packagePrefix.${CanonicalEmitter.packageSegment(q.module)}.${typeName(q)}"

  /** Scala type and codec expression of a type in field or element position. */
  private def tc(rule: QName, t: FieldType): (String, String) = t match {
    case FieldType.Uint                                                           => ("BigInt", "Codec.uint")
    case FieldType.Int                                                            => ("BigInt", "Codec.int")
    case FieldType.Nint                                                           => ("BigInt", "Codec.intRange(None, Some(BigInt(-1)))")
    case FieldType.Bytes                                                          => ("ByteVector", "Codec.bytes")
    case FieldType.Text                                                           => ("String", "Codec.text")
    case FieldType.Bool                                                           => ("Boolean", "Codec.bool")
    case FieldType.IntRange(lo, hi) if lo >= Int.MinValue && hi <= Int.MaxValue   => ("Int", s"Codec.intIn($lo, $hi)")
    case FieldType.IntRange(lo, hi) if lo >= Long.MinValue && hi <= Long.MaxValue => ("Long", s"Codec.longIn(${lo}L, ${hi}L)")
    case FieldType.IntRange(lo, hi)   => ("BigInt", s"""Codec.intRange(Some(BigInt("$lo")), Some(BigInt("$hi")))""")
    case FieldType.SizedBytes(lo, hi) => ("ByteVector", s"Codec.sizedBytes(${lo}L, ${bound(hi, "L")})")
    case FieldType.SizedText(lo, hi)  => ("String", s"Codec.sizedText($lo, ${bound(hi, "")})")
    case FieldType.Ref(q)             => (qualified(rule.module, q), s"Codec.defer(${qualified(rule.module, q)}.codec)")
    case FieldType.AnyValue           => ("CanonicalValue", "Codec.any")
    case FieldType.LitInt(v)          => (s"$v", s"Codec.literalInt[$v]($v)")
    case FieldType.LitText(v)         => (literal(v), s"Codec.literalText[${literal(v)}](${literal(v)})")
    case FieldType.Matching(pattern)  => ("String", s"Codec.matching(${literal(pattern)}.r)")
    case FieldType.TextMap(value) =>
      val (t, c) = tc(rule, value)
      (s"Map[String, $t]", s"Codec.textMap($c)")
    case FieldType.Vec(element, min, max) =>
      val (et, ec) = tc(rule, element)
      (s"Vector[$et]", s"Codec.vector($ec, $min, ${max.fold("None")(m => s"Some($m)")})")
  }

  private def bound(hi: Int, suffix: String): String = if (hi == Int.MaxValue) "None" else s"Some($hi$suffix)"

  private final case class Field(key: String, name: String, tpe: String, codec: String, optional: Boolean)

  private def fields(rule: QName, profiled: Vector[ProfileField]): Either[GenError, Vector[Field]] = {
    val fs = profiled.map { f =>
      val (t, c) = tc(rule, f.tpe)
      Field(f.key, fieldName(f.key), t, c, f.optional)
    }
    val clash = fs.groupBy(_.name).collect { case (n, xs) if xs.size > 1 => n }
    Either.cond(clash.isEmpty, fs, GenError.NameClash(rule.toString, clash.mkString(", "), fs.map(_.key)))
  }

  private def record(rule: QName, profiled: Vector[ProfileField]): Either[GenError, String] =
    fields(rule, profiled).map { fs =>
      val n = typeName(rule)
      val params = fs.map(f => s"${f.name}: ${if (f.optional) s"Option[${f.tpe}]" else f.tpe}").mkString(", ")
      val enc = fs
        .map(f => s"""        Codec.${if (f.optional) "optionalField" else "field"}("${f.key}", ${f.codec}, a.${f.name})""")
        .mkString(",\n")
      val keys = fs.map(f => s""""${f.key}"""").mkString(", ")
      val dec = fs
        .map(f => s"""        ${f.name} <- fields$$.${if (f.optional) "optional" else "required"}("${f.key}")(using ${f.codec}.decoder)""")
        .mkString("\n")
      val body =
        if (fs.isEmpty) s"scala.util.Right($n())"
        else s"for {\n$dec\n      } yield $n(${fs.map(_.name).mkString(", ")})"
      s"""final case class $n($params)
         |
         |object $n {
         |  lazy val codec: Codec[$n] = Codec(
         |    ${if (fs.isEmpty) "_" else "a"} =>
         |      Codec.record(List(
         |$enc
         |      )),
         |    CanonicalDecoder.record(Set($keys)) { ${if (fs.isEmpty) "_" else "fields$"} =>
         |      $body
         |    }
         |  )
         |  given Canonical[$n] = codec.canonical
         |  given CanonicalDecoder[$n] = codec.decoder
         |}
         |""".stripMargin
    }

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): String = {
    val n = typeName(rule)
    s"""enum $n(val wire: BigInt) {
       |${cases.map { case (l, v) => s"""  case ${pascal(l)} extends $n(BigInt("$v"))""" }.mkString("\n")}
       |}
       |
       |object $n {
       |  lazy val codec: Codec[$n] = Codec(
       |    e => Codec.int.encode(e.wire),
       |    CanonicalDecoder.int.emap(i => $n.values.find(_.wire == i).toRight(s"$$i is not a $n"))
       |  )
       |  given Canonical[$n] = codec.canonical
       |  given CanonicalDecoder[$n] = codec.decoder
       |}
       |""".stripMargin
  }

  private def union(rule: QName, ks: Vector[UnionAlternative], texts: Vector[String]): String = {
    val n = typeName(rule)
    val names = ks.map(a => qualified(rule.module, a.rule))
    val enc = (names.map(t => s"      case a: $t => $t.codec.encode(a)") ++
      texts.map(t => s"      case ${literal(t)} => Codec.literalText[${literal(t)}](${literal(t)}).encode(${literal(t)})"))
      .mkString("\n")
    val textAlts = texts.map(t => s"(${literal(t)}, ${literal(t)}: $n)").mkString(", ")
    def set(s: Set[String]) = s"Set(${s.toList.sorted.map(k => s""""$k"""").mkString(", ")})"
    val sel = ks.map { a =>
      s"        (${set(a.required)}, ${set(a.allowed)}, ${qualified(rule.module, a.rule)}.codec.decoder.map(x => x: $n))"
    }.mkString(",\n")
    s"""type $n = ${(names ++ texts.map(literal)).mkString(" | ")}
       |
       |object $n {
       |  lazy val codec: Codec[$n] = Codec(
       |    {
       |$enc
       |    },
       |    (v: CanonicalValue) =>
       |      Codec.selectAlternative[$n](v, List(
       |$sel
       |      ), List($textAlts))
       |  )
       |}
       |""".stripMargin
  }

  private def alias(rule: QName, t: FieldType): String = {
    val (tpe, codec) = tc(rule, t)
    val n = typeName(rule)
    s"""type $n = $tpe
       |
       |object $n {
       |  lazy val codec: Codec[$n] = $codec
       |}
       |""".stripMargin
  }

  private def emitRule(q: QName, model: ModelRule): Either[GenError, String] = model match {
    case ModelRule.Record(fs)         => record(q, fs)
    case ModelRule.Enumeration(cs)    => enumeration(q, cs).asRight
    case ModelRule.Union(alts, texts) => union(q, alts, texts).asRight
    case ModelRule.Alias(t)           => alias(q, t).asRight
  }

  /** The source of one module, or the first profile violation. */
  def module(name: String): Either[GenError, String] =
    schema.modules.get(name).toRight(GenError.UnknownModule(name, "configuration")).flatMap { m =>
      val rules = m.rules.keys.toVector.sorted.map(QName(name, _))
      val clash = rules.groupBy(typeName).collect { case (t, qs) if qs.size > 1 => t -> qs }
      clash.headOption match {
        case Some((t, qs)) => GenError.NameClash(name, t, qs.map(_.name)).asLeft
        case None =>
          rules.traverse(q => profile.rule(q).flatMap(emitRule(q, _))).map { bodies =>
            val code = bodies.mkString("\n")
            val imports = List(
              Option.when(code.contains("ByteVector"))("import scodec.bits.ByteVector"),
              Some(
                s"import xyz.kd5ujc.core.canonical.{${List("Canonical", "CanonicalDecoder", "CanonicalValue", "Codec").filter(i => s"\\b$i\\b".r.findFirstIn(code).isDefined).mkString(", ")}}"
              )
            ).flatten
            s"""// GENERATED by models-gen from shared/schemas/$name.cddl. Do not edit.
               |package $packagePrefix.${CanonicalEmitter.packageSegment(name)}
               |
               |${imports.mkString("\n")}
               |
               |$code""".stripMargin
          }
      }
    }
}

object CanonicalEmitter {

  /** Scala 3 reserved words, escaped with backticks when a key maps to one (the syntax lint reads them as code). */
  // scalafix:off DisableSyntax.noTry, DisableSyntax.noCatch, DisableSyntax.noFinally
  private val keywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )
  // scalafix:on DisableSyntax.noTry, DisableSyntax.noCatch, DisableSyntax.noFinally

  private def words(s: String): List[String] = s.split("[-_ .]+").toList.filter(_.nonEmpty)

  /** A module name as a package segment: `merkle-vectors` is `merkle_vectors`. */
  def packageSegment(module: String): String = module.map(c => if (c.isLetterOrDigit) c.toLower else '_')

  /** A Scala string literal (also usable as a singleton type) for `s`. */
  def literal(s: String): String =
    "\"" + s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c    => c.toString
    } + "\""

  def pascal(s: String): String = words(s).map(w => w.take(1).toUpperCase + w.drop(1)).mkString

  def fieldName(key: String): String = {
    val camel = words(key) match {
      case first :: rest => first + rest.map(w => w.take(1).toUpperCase + w.drop(1)).mkString
      case Nil           => "_"
    }
    if (keywords(camel)) s"`$camel`" else camel
  }
}
