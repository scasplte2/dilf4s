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
 *     one alternative is outside the other, for every pair), which is checked here, not at run time. A choice of texts
 *     only also lists them (`values`)
 *   - an error family is a sealed trait with a case class (or object) per kind, each giving its `kind`; no codec
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
    case FieldType.Nint                                                           => ("BigInt", "Codec.nint")
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
      (s"CanonicalEntries[$t]", s"Codec.textMap($c)")
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
    fs.groupBy(_.name)
      .toVector
      .sortBy(_._1)
      .collectFirst { case (n, xs) if xs.size > 1 => GenError.NameClash(rule.toString, n, xs.map(_.key)) }
      .orElse(fs.collectFirst { case f if reservedFields(f.name) => GenError.ReservedName(rule.toString, Language.Scala, f.name) })
      .toLeft(fs)
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

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): Either[GenError, String] = {
    val n = typeName(rule)
    cases.collectFirst {
      case (l, _) if reservedCases(pascal(l)) => GenError.ReservedName(rule.toString, Language.Scala, pascal(l))
    }.toLeft {
      s"""enum $n(val wire: BigInt) {
       |${cases.map { case (l, v) => s"""  case ${pascal(l)} extends $n(BigInt("$v"))""" }.mkString("\n")}
       |}
       |
       |object $n {
       |  lazy val codec: Codec[$n] = Codec(
       |    e => Codec.int.encode(e.wire),
       |    CanonicalDecoder.int.emap(i => $n.values.find(_.wire == i).toRight(s"an integer of $${i.bitLength} bits is not a $n"))
       |  )
       |  given Canonical[$n] = codec.canonical
       |  given CanonicalDecoder[$n] = codec.decoder
       |}
       |""".stripMargin
    }
  }

  private def union(rule: QName, ks: Vector[UnionAlternative], texts: Vector[String], choices: Vector[TextChoice]): String = {
    val n = typeName(rule)
    val names = ks.map(a => qualified(rule.module, a.rule))
    val allTexts = texts ++ choices.flatMap(_.texts)
    val enc = (names.map(t => s"      case a: $t => $t.codec.encode(a)") ++
      allTexts.map(t => s"      case ${literal(t)} => Codec.literalText[${literal(t)}](${literal(t)}).encode(${literal(t)})"))
      .mkString("\n")
    val textAlts = allTexts.map(t => s"(${literal(t)}, ${literal(t)}: $n)").mkString(", ")
    val values =
      if (ks.nonEmpty) ""
      else s"\n  /** Every alternative, in schema order. */\n  val values: Vector[$n] = Vector(${allTexts.map(literal).mkString(", ")})\n"
    def set(s: Set[String]) = s"Set(${s.toList.sorted.map(k => s""""$k"""").mkString(", ")})"
    val sel = ks.map { a =>
      s"        (${set(a.required)}, ${set(a.allowed)}, ${qualified(rule.module, a.rule)}.codec.decoder.map(x => x: $n))"
    }.mkString(",\n")
    s"""type $n = ${(names ++ texts.map(literal) ++ choices.map(c => qualified(rule.module, c.rule))).mkString(" | ")}
       |
       |object $n {$values
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

  /** A sealed trait with one case per kind; a kind without fields is a case object. */
  private def errorFamily(rule: QName, kinds: QName, cases: Vector[ErrorCase]): Either[GenError, String] = {
    val n = typeName(rule)
    val k = qualified(rule.module, kinds)
    val clash = cases.collectFirst {
      case c if reservedTypes(c.kind) || c.kind == n || c.kind == typeName(kinds) =>
        GenError.ReservedName(rule.toString, Language.Scala, c.kind)
    }
    clash.toLeft(()).flatMap(_ => cases.traverse(c => fields(rule, c.fields).map(c -> _))).map { withFields =>
      val members = withFields.map {
        case (c, fs) =>
          val body = s" extends $n {\n    def kind: $k = ${literal(c.kind)}\n  }"
          if (fs.isEmpty) s"  case object ${c.kind}$body"
          else
            s"  final case class ${c.kind}(${fs.map(f => s"${f.name}: ${if (f.optional) s"Option[${f.tpe}]" else f.tpe}").mkString(", ")})$body"
      }
      s"""/** `${rule.name}` (`shared/schemas/${rule.module}.cddl`): an error family, by kind ([[$k]]). */
         |sealed trait $n extends Product with Serializable {
         |
         |  /** This error's kind. */
         |  def kind: $k
         |}
         |
         |object $n {
         |${members.mkString("\n\n")}
         |}
         |""".stripMargin
    }
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

  /** A constant: an `Int` literal when it fits, then a `Long` one, else a `BigInt`. */
  private def constant(rule: QName, v: BigInt): String = {
    val n = typeName(rule)
    val value =
      if (v <= Int.MaxValue) s"final val $n = $v"
      else if (v <= Long.MaxValue) s"final val $n = ${v}L"
      else s"""val $n: BigInt = BigInt("$v")"""
    s"""/** `${rule.name}` (`shared/schemas/${rule.module}.cddl`). */
       |$value
       |""".stripMargin
  }

  private def emitRule(q: QName, model: ModelRule): Either[GenError, String] = model match {
    case ModelRule.Record(fs)                  => record(q, fs)
    case ModelRule.Enumeration(cs)             => enumeration(q, cs)
    case ModelRule.Union(alts, texts, choices) => union(q, alts, texts, choices).asRight
    case ModelRule.ErrorFamily(kinds, cases)   => errorFamily(q, kinds, cases)
    case ModelRule.Alias(t)                    => alias(q, t).asRight
    case ModelRule.Constant(v)                 => constant(q, v).asRight
  }

  /** The source of one module, or the first profile violation. */
  def module(name: String): Either[GenError, String] =
    schema.modules.get(name).toRight(GenError.UnknownModule(name, "configuration")).flatMap { m =>
      val rules = m.rules.keys.toVector.sorted.map(QName(name, _))
      val problem = rules
        .groupBy(typeName)
        .toVector
        .sortBy(_._1)
        .collectFirst { case (t, qs) if qs.size > 1 => GenError.NameClash(name, t, qs.map(_.name)) }
        .orElse(rules.collectFirst { case q if reservedTypes(typeName(q)) => GenError.ReservedName(name, Language.Scala, typeName(q)) })
      problem match {
        case Some(e) => e.asLeft
        case None =>
          rules.traverse(q => profile.rule(q).flatMap(emitRule(q, _))).map { bodies =>
            val code = bodies.mkString("\n")
            val canonical =
              List("Canonical", "CanonicalDecoder", "CanonicalEntries", "CanonicalValue", "Codec")
                .filter(i => s"\\b$i\\b".r.findFirstIn(code).isDefined)
            val imports = List(
              Option.when(code.contains("ByteVector"))("import scodec.bits.ByteVector"),
              Option.when(canonical.nonEmpty)(s"import xyz.kd5ujc.core.canonical.{${canonical.mkString(", ")}}")
            ).flatten
            s"""// GENERATED by models-gen from shared/schemas/$name.cddl. Do not edit.
               |package $packagePrefix.${CanonicalEmitter.packageSegment(name)}
               |
               |${imports.map(_ + "\n").mkString}${if (imports.isEmpty) "" else "\n"}$code""".stripMargin
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

  /**
   * Type names the generated code refers to unqualified: a rule with one of these names would shadow it in its file.
   * Each was confirmed by compiling generated output (the rest of Predef's and scala's types do not break it).
   */
  private val reservedTypes = Set(
    "BigInt",
    "Boolean",
    "ByteVector",
    "Canonical",
    "CanonicalDecoder",
    "CanonicalEntries",
    "CanonicalValue",
    "Codec",
    "Int",
    "List",
    "Long",
    "Map",
    "None",
    "Option",
    "Set",
    "Some",
    "String",
    "Vector"
  )

  /** Terms an enumeration's own body refers to, which one of its cases would shadow. */
  private val reservedCases = Set("BigInt", "CanonicalDecoder", "Codec")

  /** Members of every case class that a field cannot override (Scala requires `override`, or they are final). */
  private val reservedFields = Set(
    "clone",
    "finalize",
    "hashCode",
    "notify",
    "notifyAll",
    "productArity",
    "productElementNames",
    "productIterator",
    "productPrefix",
    "wait"
  )

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
