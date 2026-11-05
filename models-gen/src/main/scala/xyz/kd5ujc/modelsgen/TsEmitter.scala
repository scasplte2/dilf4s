package xyz.kd5ujc.modelsgen

import cats.syntax.all._

/**
 * Emits TypeScript for the dilf4s canonical profile ([[Profile]]): one module per schema file, against the runtime in
 * `sdk/ts/packages/core/src/codec.ts`. The output mirrors [[CanonicalEmitter]]: the same rules, the same shape checks,
 * the same field order. TypeScript-specific choices:
 *
 *   - a record is a readonly interface (camelCase field names, as in Scala) and a codec constant of the same name
 *   - every integer is a `bigint`; byte strings are `Uint8Array`; text-keyed maps are `ReadonlyMap`
 *   - an enumeration is a union of its labels; a union of records is a plain TypeScript union, told apart when
 *     encoding by the same key sets that select it when decoding
 *   - an error family is a union of readonly object types told apart by `kind`; no codec
 *   - every reference is deferred, so modules may refer to rules in any order, recursively
 *
 * Codecs never throw: they return the core's `Result`.
 */
final class TsEmitter(models: Map[QName, ModelRule]) {

  import TsEmitter._

  private def typeName(q: QName): String = CanonicalEmitter.pascal(q.name)

  private def qualified(from: String, q: QName): String =
    if (q.module == from) typeName(q) else s"${namespace(q.module)}.${typeName(q)}"

  /** TypeScript type and codec expression of a field or element type. */
  private def ts(rule: QName, t: FieldType): (String, String) = t match {
    case FieldType.Uint               => ("bigint", "c.uint")
    case FieldType.Int                => ("bigint", "c.int")
    case FieldType.Nint               => ("bigint", "c.nint")
    case FieldType.Bytes              => ("Uint8Array", "c.bytes")
    case FieldType.Text               => ("string", "c.text")
    case FieldType.Bool               => ("boolean", "c.bool")
    case FieldType.AnyValue           => ("c.Value", "c.any")
    case FieldType.IntRange(lo, hi)   => ("bigint", s"c.intRange(${lo}n, ${hi}n)")
    case FieldType.SizedBytes(lo, hi) => ("Uint8Array", s"c.sizedBytes($lo, ${bound(hi)})")
    case FieldType.SizedText(lo, hi)  => ("string", s"c.sizedText($lo, ${bound(hi)})")
    case FieldType.Matching(p)        => ("string", s"c.matching(${string(p)})")
    case FieldType.LitInt(v)          => (s"${v}n", s"c.literalInt(${v}n)")
    case FieldType.LitText(v)         => (string(v), s"c.literalText(${string(v)})")
    case FieldType.Ref(q) =>
      val n = qualified(rule.module, q)
      (n, s"c.defer(() => $n)")
    case FieldType.TextMap(value) =>
      val (vt, vc) = ts(rule, value)
      (s"ReadonlyMap<string, $vt>", s"c.textMap($vc)")
    case FieldType.Vec(e, min, max) =>
      val (et, ec) = ts(rule, e)
      (s"readonly ${if (et.contains(" ")) s"($et)" else et}[]", s"c.vector($ec, $min, ${max.fold("null")(_.toString)})")
  }

  private def doc(rule: QName): String = s"/** `${rule.name}` (`shared/schemas/${rule.module}.cddl`). */"

  private final case class Field(key: String, name: String, tpe: String, codec: String, optional: Boolean)

  private def fields(rule: QName, profiled: Vector[ProfileField]): Either[GenError, Vector[Field]] = {
    val fs = profiled.map { f =>
      val (t, c) = ts(rule, f.tpe)
      Field(f.key, fieldName(f.key), t, c, f.optional)
    }
    fs.groupBy(_.name)
      .toVector
      .sortBy(_._1)
      .collectFirst { case (n, xs) if xs.size > 1 => GenError.NameClash(rule.toString, n, xs.map(_.key)) }
      .toLeft(fs)
  }

  private def record(rule: QName, profiled: Vector[ProfileField]): Either[GenError, String] =
    fields(rule, profiled).map { fs =>
      val n = typeName(rule)
      val tpe =
        if (fs.isEmpty) s"export type $n = Readonly<Record<string, never>>;"
        else s"export interface $n {\n${fs.map(f => s"  readonly ${f.name}${if (f.optional) "?" else ""}: ${f.tpe};").mkString("\n")}\n}"
      val enc = fs.map { f =>
        s"        c.${if (f.optional) "optionalField" else "field"}(${string(f.key)}, ${f.codec}, a.${f.name}, e),"
      }.mkString("\n")
      val keys = fs.map(f => string(f.key)).mkString(", ")
      val dec = fs.zipWithIndex.map {
        case (f, i) =>
          s"    const r$i = f.value.${if (f.optional) "optional" else "required"}(${string(f.key)}, ${f.codec});\n    if (!r$i.ok) return r$i;"
      }.mkString("\n")
      val build = fs.zipWithIndex.map { case (f, i) => s"${f.name}: r$i.value" }.mkString(", ")
      s"""${doc(rule)}
         |$tpe
         |
         |export const $n: c.Codec<$n> = {
         |  encode: (${if (fs.isEmpty) "_a" else "a"}, d = 0) =>
         |    c.nested(d, (${if (fs.isEmpty) "" else "e"}) =>
         |      c.record([
         |$enc
         |      ]),
         |    ),
         |  decode: (v) => {
         |    const f = c.fields(v, [$keys]);
         |    if (!f.ok) return f;
         |$dec
         |    return c.ok({ $build });
         |  },
         |};
         |""".stripMargin
        .replace("c.record([\n\n      ])", "c.record([])")
        .replace("    if (!f.ok) return f;\n\n    return c.ok({  });", "    if (!f.ok) return f;\n    return c.ok({});")
    }

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): String = {
    val n = typeName(rule)
    s"""${doc(rule)}
       |export type $n = ${cases.map { case (l, _) => string(l) }.mkString(" | ")};
       |
       |export const $n: c.Codec<$n> = c.intEnum<$n>(${string(n)}, [${cases.map { case (l, v) => s"[${string(l)}, ${v}n]" }
        .mkString(", ")}]);
       |""".stripMargin
  }

  private def union(
    rule:     QName,
    alts:     Vector[UnionAlternative],
    ownTexts: Vector[String],
    choices:  Vector[TextChoice]
  ): Either[GenError, String] = {
    val n = typeName(rule)
    val texts = ownTexts ++ choices.flatMap(_.texts)
    // encoding tells record alternatives apart by their field names, decoding by their wire keys
    val fieldsOf: Either[GenError, Vector[(UnionAlternative, Vector[ProfileField])]] = alts.traverse { a =>
      models.get(a.rule) match {
        case Some(ModelRule.Record(fs)) => (a -> fs).asRight
        case _ => GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile("choice alternative that is not a map rule")).asLeft
      }
    }
    fieldsOf.map { withFields =>
      val names = alts.map(a => qualified(rule.module, a.rule))
      def list(xs: Iterable[String]) = s"[${xs.toList.sorted.map(string).mkString(", ")}]"
      val known = list(withFields.flatMap(_._2.map(f => fieldName(f.key))).distinct)
      val guards = withFields.zip(names).map {
        case ((_, fs), t) =>
          val required = fs.filterNot(_.optional).map(f => fieldName(f.key))
          val allowed = fs.map(f => fieldName(f.key))
          s"    if (c.hasFields(a, $t, ${list(required)}, ${list(allowed)}, known)) return $t.encode(a, d);"
      }
      val objectBranch =
        if (alts.isEmpty) ""
        else s"\n    const known = $known;\n${guards.mkString("\n")}"
      val textBranch =
        if (texts.isEmpty) "" else s"\n    if (typeof a === \"string\") return c.oneOfText(a, ${list(texts)});"
      val records = alts.map(a => s"[${list(a.required)}, ${list(a.allowed)}]").mkString(", ")
      val dec = names.zipWithIndex.map { case (t, i) => s"      case $i:\n        return $t.decode(v);" } ++
        texts.zipWithIndex.map { case (t, i) => s"      case ${alts.size + i}:\n        return c.ok<$n>(${string(t)});" }
      s"""${doc(rule)}
         |export type $n = ${(names ++ ownTexts.map(string) ++ choices.map(ch => qualified(rule.module, ch.rule))).mkString(" | ")};
         |
         |export const $n: c.Codec<$n> = {
         |  encode: (a${if (alts.isEmpty) "" else ", d"}) => {$textBranch$objectBranch
         |    return c.typeMismatch(${string(s"no alternative of $n fits")});
         |  },
         |  decode: (v) => {
         |    const s = c.selectAlternative(v, [$records], [${texts.map(string).mkString(", ")}]);
         |    if (!s.ok) return s;
         |    switch (s.value) {
         |${dec.mkString("\n")}
         |      default:
         |        return c.decodeFailure("unknown alternative");
         |    }
         |  },
         |};
         |""".stripMargin
    }
  }

  /** Whether an error field is an integer, directly or through aliases. */
  @scala.annotation.tailrec
  private def integer(t: FieldType): Boolean = t match {
    case FieldType.Uint | FieldType.Int | FieldType.Nint | FieldType.IntRange(_, _) => true
    case FieldType.Ref(q) =>
      models.get(q) match {
        case Some(ModelRule.Alias(target)) => integer(target)
        case _                             => false
      }
    case _ => false
  }

  /**
   * A union of readonly object types, one per kind, each with its `kind`. An integer field is a `number`, the type of
   * the SDK's own sizes and periods: an error is built from the SDK's arguments, never decoded.
   */
  private def errorFamily(rule: QName, kinds: QName, cases: Vector[ErrorCase]): Either[GenError, String] =
    cases
      .traverse(c =>
        fields(rule, c.fields).map(fs => c -> fs.zip(c.fields).map { case (f, p) => if (integer(p.tpe)) f.copy(tpe = "number") else f })
      )
      .map { withFields =>
        val members = withFields.map {
          case (c, fs) =>
            val props = s"readonly kind: ${string(c.kind)}" +: fs.map(f => s"readonly ${f.name}${if (f.optional) "?" else ""}: ${f.tpe}")
            s"  | { ${props.mkString("; ")} }"
        }
        // members start with `|`, which stripMargin would take for the margin: they are appended after it
        s"""/** `${rule.name}` (`shared/schemas/${rule.module}.cddl`): an error family, by `kind` (`${qualified(rule.module, kinds)}`). */
         |export type ${typeName(rule)} =
         |""".stripMargin + members.mkString("\n") + ";\n"
      }

  private def alias(rule: QName, t: FieldType): String = {
    val (tpe, codec) = ts(rule, t)
    val n = typeName(rule)
    s"""${doc(rule)}
       |export type $n = $tpe;
       |
       |export const $n: c.Codec<$n> = $codec;
       |""".stripMargin
  }

  /** A constant: a `number` when it is a safe integer (at most 2^53 - 1), else a `bigint`. */
  private def constant(rule: QName, v: BigInt): String =
    s"""${doc(rule)}
       |export const ${RustEmitter.constName(rule)} = ${if (v <= MaxSafeInteger) v.toString else s"${v}n"};
       |""".stripMargin

  /** The source of one module. */
  def module(name: String): Either[GenError, String] = {
    val rules = models.toVector.collect { case (q, m) if q.module == name => q -> m }.sortBy(_._1.name)
    val problem = rules.groupBy { case (q, _) => typeName(q) }.toVector
      .sortBy(_._1)
      .collectFirst { case (t, qs) if qs.size > 1 => GenError.NameClash(name, t, qs.map(_._1.name)) }
      .orElse(rules.collectFirst {
        case (q, _) if reservedTypes(typeName(q)) => GenError.ReservedName(name, Language.TypeScript, typeName(q))
      })
      .orElse(
        Option
          .when(reservedWords(namespace(name)) || namespace(name) == "c")(GenError.ReservedName(name, Language.TypeScript, namespace(name)))
      )
    problem.toLeft(()).flatMap { _ =>
      rules.traverse {
        case (q, ModelRule.Record(fs))             => record(q, fs)
        case (q, ModelRule.Enumeration(cs))        => enumeration(q, cs).asRight
        case (q, ModelRule.Union(as, texts, cs))   => union(q, as, texts, cs)
        case (q, ModelRule.ErrorFamily(ks, cases)) => errorFamily(q, ks, cases)
        case (q, ModelRule.Alias(t))               => alias(q, t).asRight
        case (q, ModelRule.Constant(v))            => constant(q, v).asRight
      }.map { bodies =>
        val code = bodies.mkString("\n")
        val others = models.keys.collect {
          case q if q.module != name && code.contains(s"${namespace(q.module)}.${typeName(q)}") => q.module
        }.toList.distinct.sorted
        val imports =
          Option.when("(?<![A-Za-z0-9_$.])c\\.".r.findFirstIn(code).isDefined)(s"""import * as c from "../codec.js";""").toList ++
          others.map { m =>
            // a module named only in types (a choice of its texts) is imported as types: its codecs are not used
            val ns = namespace(m)
            val asValue = code.contains(s"() => $ns.") || s"\\b$ns\\.[A-Za-z0-9_]+\\.(encode|decode)\\(".r.findFirstIn(code).isDefined
            s"""import ${if (asValue) "" else "type "}* as $ns from "./${CanonicalEmitter.packageSegment(m)}.js";"""
          }
        // the code is not margin-stripped again: an error family's members start with `|`
        s"""// GENERATED by models-gen from shared/schemas/$name.cddl. Do not edit.
           |${imports.map(_ + "\n").mkString}
           |""".stripMargin + code
      }
    }
  }

  /** `generated/index.ts`, one namespace per module. */
  def index(modules: Vector[String]): String =
    s"""// The wire models of shared/schemas, one namespace per schema file. GENERATED by models-gen. Do not edit.
       |${modules.sorted.map(m => s"""export * as ${namespace(m)} from "./${CanonicalEmitter.packageSegment(m)}.js";""").mkString("\n")}
       |""".stripMargin
}

object TsEmitter {

  /** Global types the generated code refers to, which a generated type must not shadow. */
  private val reservedTypes = Set("Readonly", "Record", "ReadonlyMap", "Uint8Array")

  /** `Number.MAX_SAFE_INTEGER`. */
  private val MaxSafeInteger: BigInt = (BigInt(1) << 53) - 1

  // scalafix:off DisableSyntax.noTry, DisableSyntax.noCatch, DisableSyntax.noFinally
  /** JavaScript reserved words, which a module's namespace (its import binding) cannot be. */
  private val reservedWords = Set(
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "let",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield"
  )
  // scalafix:on DisableSyntax.noTry, DisableSyntax.noCatch, DisableSyntax.noFinally

  /** A module's namespace: `merkle-vectors` is `merkleVectors`. */
  def namespace(module: String): String = CanonicalEmitter.fieldName(module).stripPrefix("`").stripSuffix("`")

  /** A wire key as a property name: camelCase, as the Scala emitter names fields. */
  def fieldName(key: String): String = CanonicalEmitter.fieldName(key).stripPrefix("`").stripSuffix("`")

  /** A TypeScript string literal. */
  def string(s: String): String =
    "\"" + s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    } + "\""

  def bound(hi: Int): String = if (hi == Int.MaxValue) "null" else hi.toString
}
