package xyz.kd5ujc.modelsgen

import cats.syntax.all._

import xyz.kd5ujc.modelsgen.Ast.Occ

/**
 * Emits Scala 3 types and `xyz.kd5ujc.core.canonical.Codec`s for the **dilf4s canonical profile** of CDDL: the subset
 * that fits the canonical data model (docs/specs/canonical-encoding.md).
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

  private def outside(rule: QName, what: String): GenError = GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile(what))

  def typeName(q: QName): String = pascal(q.name)
  private def qualified(from: String, q: QName): String =
    if (q.module == from) typeName(q) else s"$packagePrefix.${q.module}.${typeName(q)}"

  /** Scala type and codec expression of a type in field or element position. */
  private def tc(rule: QName, t: RT): Either[GenError, (String, String)] = t match {
    case RT.Prim(PrimKind.Uint)                                            => ("BigInt", "Codec.uint").asRight
    case RT.Prim(PrimKind.Int)                                             => ("BigInt", "Codec.int").asRight
    case RT.Prim(PrimKind.Nint)                                            => ("BigInt", "Codec.intRange(None, Some(BigInt(-1)))").asRight
    case RT.Prim(PrimKind.Bytes)                                           => ("ByteVector", "Codec.bytes").asRight
    case RT.Prim(PrimKind.Text)                                            => ("String", "Codec.text").asRight
    case RT.Prim(PrimKind.Bool)                                            => ("Boolean", "Codec.bool").asRight
    case RT.IntRange(lo, hi) if lo >= Int.MinValue && hi <= Int.MaxValue   => ("Int", s"Codec.intIn($lo, $hi)").asRight
    case RT.IntRange(lo, hi) if lo >= Long.MinValue && hi <= Long.MaxValue => ("Long", s"Codec.longIn(${lo}L, ${hi}L)").asRight
    case RT.IntRange(lo, hi)   => ("BigInt", s"""Codec.intRange(Some(BigInt("$lo")), Some(BigInt("$hi")))""").asRight
    case RT.SizedBytes(lo, hi) => ("ByteVector", s"Codec.sizedBytes(${lo}L, ${bound(hi, "L")})").asRight
    case RT.SizedText(lo, hi)  => ("String", s"Codec.sizedText($lo, ${bound(hi, "")})").asRight
    case RT.Ref(q)             => (qualified(rule.module, q), s"${qualified(rule.module, q)}.codec").asRight
    case RT.Arr(Vector(REntry(occ, _, None, element))) if occ != Occ.Once && occ != Occ.Optional =>
      tc(rule, element).map {
        case (et, ec) => (s"Vector[$et]", s"Codec.vector($ec, ${occ.min}, ${occ.max.fold("None")(m => s"Some($m)")})")
      }
    case other => outside(rule, s"${SyntaxSummary(other)} in field position (name it as its own rule)").asLeft
  }

  private def bound(hi: Int, suffix: String): String = if (hi == Int.MaxValue) "None" else s"Some($hi$suffix)"

  private final case class Field(key: String, name: String, tpe: String, codec: String, optional: Boolean)

  private def fields(rule: QName, entries: Vector[REntry]): Either[GenError, Vector[Field]] =
    entries.traverse {
      case REntry(occ, _, Some(RKey.TextKey(key)), value) if occ == Occ.Once || occ == Occ.Optional =>
        tc(rule, value).map { case (t, c) => Field(key, fieldName(key), t, c, occ == Occ.Optional) }
      case REntry(_, _, key, _) =>
        outside(rule, s"map entry with key ${key.fold("<none>")(_.toString)} (text-literal keys, once or optional)").asLeft
    }.flatMap { fs =>
      val clash = fs.groupBy(_.name).collect { case (n, xs) if xs.size > 1 => n }
      Either.cond(clash.isEmpty, fs, GenError.NameClash(rule.toString, clash.mkString(", "), fs.map(_.key)))
    }

  private def record(rule: QName, entries: Vector[REntry]): Either[GenError, String] =
    fields(rule, entries).map { fs =>
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
         |    a =>
         |      Codec.record(List(
         |$enc
         |      )),
         |    CanonicalDecoder.record(Set($keys)) { fields$$ =>
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
    val names = cases.map { case (l, _) => pascal(l) }
    val dupNames = names.diff(names.distinct)
    val dupValues = cases.map(_._2).diff(cases.map(_._2).distinct)
    if (dupNames.nonEmpty || dupValues.nonEmpty) outside(rule, "enumeration with repeated labels or values").asLeft
    else
      s"""enum $n(val wire: BigInt) {
         |${cases.zip(names).map { case ((_, v), cn) => s"""  case $cn extends $n(BigInt("$v"))""" }.mkString("\n")}
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
         |""".stripMargin.asRight
  }

  /** Required and permitted keys of a map rule (a choice alternative). */
  private def keySets(alt: QName): Either[GenError, (Set[String], Set[String])] =
    schema.rule(alt).flatMap {
      case RuleDef(_, RuleBody.Type(RT.MapT(es))) =>
        fields(alt, es).map(fs => (fs.filterNot(_.optional).map(_.key).toSet, fs.map(_.key).toSet))
      case _ => outside(alt, "choice alternative that is not a map rule").asLeft
    }

  private def union(rule: QName, alts: Vector[QName]): Either[GenError, String] =
    alts.traverse(a => keySets(a).map(a -> _)).flatMap { ks =>
      val ambiguous = for {
        (a, (ra, aa)) <- ks
        (b, (rb, ab)) <- ks
        if typeName(a) < typeName(b) && (ra -- ab).isEmpty && (rb -- aa).isEmpty
      } yield s"${a.name}/${b.name}"
      if (ambiguous.nonEmpty) GenError.Unsupported(rule.toString, UnsupportedFeature.AmbiguousAlternatives(ambiguous.toVector)).asLeft
      else {
        val n = typeName(rule)
        val names = alts.map(qualified(rule.module, _))
        val enc = names.map(t => s"      case a: $t => $t.codec.encode(a)").mkString("\n")
        def set(s: Set[String]) = s"Set(${s.toList.sorted.map(k => s""""$k"""").mkString(", ")})"
        val sel = ks.map {
          case (q, (r, a)) => s"        (${set(r)}, ${set(a)}, ${qualified(rule.module, q)}.codec.decoder.map(x => x: $n))"
        }.mkString(",\n")
        s"""type $n = ${names.mkString(" | ")}
           |
           |object $n {
           |  lazy val codec: Codec[$n] = Codec(
           |    {
           |$enc
           |    },
           |    (v: CanonicalValue) =>
           |      Codec.selectAlternative[$n](v, List(
           |$sel
           |      ))
           |  )
           |}
           |""".stripMargin.asRight
      }
    }

  private def alias(rule: QName, t: RT): Either[GenError, String] =
    tc(rule, t).map {
      case (tpe, codec) =>
        val n = typeName(rule)
        s"""type $n = $tpe
         |
         |object $n {
         |  lazy val codec: Codec[$n] = $codec
         |}
         |""".stripMargin
    }

  private def emitRule(q: QName): Either[GenError, String] =
    schema.rule(q).flatMap {
      case RuleDef(_, RuleBody.Type(RT.MapT(es)))    => record(q, es)
      case RuleDef(_, RuleBody.Type(RT.IntEnum(cs))) => enumeration(q, cs)
      case RuleDef(_, RuleBody.Type(RT.Choice(alts))) =>
        alts.traverse {
          case RT.Ref(a) => a.some
          case _         => none[QName]
        }
          .toRight(outside(q, "inline choice (use a labelled enumeration or a choice of named map rules)"))
          .flatMap(union(q, _))
      case RuleDef(_, RuleBody.Type(t))      => alias(q, t)
      case RuleDef(_, RuleBody.GroupBody(_)) => outside(q, "group rule").asLeft
    }

  /** The source of one module, or the first profile violation. */
  def module(name: String): Either[GenError, String] =
    schema.modules.get(name).toRight(GenError.UnknownModule(name, "configuration")).flatMap { m =>
      val rules = m.rules.keys.toVector.sorted.map(QName(name, _))
      val clash = rules.groupBy(typeName).collect { case (t, qs) if qs.size > 1 => t -> qs }
      clash.headOption match {
        case Some((t, qs)) => GenError.NameClash(name, t, qs.map(_.name)).asLeft
        case None =>
          rules.traverse(emitRule).map { bodies =>
            val code = bodies.mkString("\n")
            val imports = List(
              Option.when(code.contains("ByteVector"))("import scodec.bits.ByteVector"),
              Some(
                s"import xyz.kd5ujc.core.canonical.{${List("Canonical", "CanonicalDecoder", "CanonicalValue", "Codec").filter(i => s"\\b$i\\b".r.findFirstIn(code).isDefined).mkString(", ")}}"
              )
            ).flatten
            s"""// GENERATED by models-gen from shared/schemas/$name.cddl. Do not edit.
               |package $packagePrefix.$name
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

  def pascal(s: String): String = words(s).map(w => w.take(1).toUpperCase + w.drop(1)).mkString

  def fieldName(key: String): String = {
    val camel = words(key) match {
      case first :: rest => first + rest.map(w => w.take(1).toUpperCase + w.drop(1)).mkString
      case Nil           => "_"
    }
    if (keywords(camel)) s"`$camel`" else camel
  }
}
