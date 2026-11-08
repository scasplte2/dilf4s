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
 *   - a codec is a description (`c.record`, `c.union`, `c.vector`, ...) that the runtime's one encoder and one decoder
 *     walk iteratively, so no schema's nesting reaches the native stack
 *   - every exported constant is such a codec, which the combinators freeze with its shape when they build it, or a
 *     number: a module exports nothing a caller can change (no array or object literal of its own)
 *
 * Codecs never throw: they return the core's `Result`.
 */
final class TsEmitter(models: Map[QName, ModelRule], docs: QName => Vector[String] = _ => Vector.empty) {

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
    // an error's field only (Profile refuses it in a wire type): the core's Preview, imported as a type
    case FieldType.Preview => (PreviewType, "")
  }

  /** A rule's TSDoc: where it is defined (and `more`), then its CDDL comment. */
  private def doc(rule: QName, more: String = ""): String = {
    val first = s"`${rule.name}` (`shared/schemas/${rule.module}.cddl`)$more."
    val text = docs(rule)
    if (text.isEmpty) s"/** $first */" else CanonicalEmitter.comment(first +: "" +: text, "").stripSuffix("\n")
  }

  /** A member's or field's TSDoc at `indent`, ending in a newline, or nothing when it has none. */
  private def memberDoc(text: Vector[String], indent: String): String =
    text match {
      case Vector()     => ""
      case Vector(line) => s"$indent/** ${line.replace("*/", "*\\/")} */\n"
      case _            => CanonicalEmitter.comment(text, indent)
    }

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
        else
          s"export interface $n {\n${fs
              .zip(profiled)
              .map((f, p) => s"${memberDoc(p.doc, "  ")}  readonly ${f.name}${if (f.optional) "?" else ""}: ${f.tpe};")
              .mkString("\n")}\n}"
      // in schema order, which an object literal keeps: the profile gives every field an identifier, never an index
      val body = fs.map(f => s"  ${f.name}: c.${if (f.optional) "optional" else "required"}(${string(f.key)}, ${f.codec}),\n").mkString
      s"""${doc(rule)}
         |$tpe
         |
         |export const $n: c.Codec<$n> = c.record<$n>({${if (fs.isEmpty) "" else s"\n$body"}});
         |""".stripMargin
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
      val alternatives = withFields.zip(names).map {
        case ((a, fs), t) =>
          val required = fs.filterNot(_.optional).map(f => fieldName(f.key))
          val allowed = fs.map(f => fieldName(f.key))
          s"  c.alternative(c.defer(() => $t), [${list(required)}, ${list(allowed)}], [${list(a.required)}, ${list(a.allowed)}]),\n"
      }
      // the texts in schema order: decoding numbers them after the records, in this order
      val textList = texts.map(string).mkString(", ")
      val records = if (alts.isEmpty) "" else s"\n${alternatives.mkString}"
      s"""${doc(rule)}
         |export type $n = ${(names ++ ownTexts.map(string) ++ choices.map(ch => qualified(rule.module, ch.rule))).mkString(" | ")};
         |
         |export const $n: c.Codec<$n> = c.union<$n>(${string(n)}, [$records], [$textList]);
         |""".stripMargin
    }
  }

  /**
   * Whether an error field is an integer held as a `number`, directly or through aliases: one within the signed 64-bit
   * range (the SDK's sizes, offsets and limits). A wider one (a plain `uint`, such as a CBOR tag) stays a `bigint`, so it
   * is exact.
   */
  @scala.annotation.tailrec
  private def integer(t: FieldType): Boolean = t match {
    case FieldType.IntRange(lo, hi) => lo >= Long.MinValue && hi <= Long.MaxValue
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
            val documented = c.fields.exists(_.doc.nonEmpty)
            val props = s"readonly kind: ${string(c.kind)}" +: fs.zip(c.fields).map { (f, p) =>
              val field = s"readonly ${f.name}${if (f.optional) "?" else ""}: ${f.tpe}"
              if (documented) s"${memberDoc(p.doc, "      ")}      $field" else field
            }
            val shape =
              if (documented) s"{\n${props.map(p => if (p.startsWith(" ")) p else s"      $p").mkString(";\n")};\n    }"
              else s"{ ${props.mkString("; ")} }"
            s"${memberDoc(c.doc, "  ")}  | $shape"
        }
        // members start with `|`, which stripMargin would take for the margin: they are appended after it
        s"""${doc(rule, s": an error family, by `kind` (`${qualified(rule.module, kinds)}`)")}
         |export type ${typeName(rule)} =
         |""".stripMargin + members.mkString("\n") + ";\n"
      }

  /** A union of the members' texts (TypeScript names no members), each with its documentation. */
  private def namedTexts(rule: QName, members: Vector[NamedText]): String = {
    val n = typeName(rule)
    // members start with `|`, which stripMargin would take for the margin: they are appended after it
    s"""${doc(rule)}
       |export type $n =
       |""".stripMargin + members.map(m => s"${memberDoc(m.doc, "  ")}  | ${string(m.text)}").mkString("\n") +
    s""";\n\nexport const $n: c.Codec<$n> = c.union<$n>(${string(n)}, [], [${members.map(m => string(m.text)).mkString(", ")}]);\n"""
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

  /** Whether an error family of these rules holds a preview, so the module imports the core's `Preview`. */
  private def usesPreview(rules: Vector[(QName, ModelRule)]): Boolean = rules.exists {
    case (_, ModelRule.ErrorFamily(_, cases)) => cases.exists(_.fields.exists(_.tpe == FieldType.Preview))
    case _                                    => false
  }

  /** The source of one module. */
  def module(name: String): Either[GenError, String] = {
    val rules = models.toVector.collect { case (q, m) if q.module == name => q -> m }.sortBy(_._1.name)
    val problem = rules.groupBy { case (q, _) => typeName(q) }.toVector
      .sortBy(_._1)
      .collectFirst { case (t, qs) if qs.size > 1 => GenError.NameClash(name, t, qs.map(_._1.name)) }
      .orElse(rules.collectFirst {
        case (q, _) if reservedTypes(typeName(q)) => GenError.ReservedName(name, Language.TypeScript, typeName(q))
        // a module whose errors hold previews imports the core's Preview, which a generated type would shadow
        case (q, m) if typeName(q) == PreviewType && m != ModelRule.PreviewType && usesPreview(rules) =>
          GenError.ReservedName(name, Language.TypeScript, PreviewType)
      })
      .orElse(
        Option
          .when(reservedWords(namespace(name)) || namespace(name) == "c")(GenError.ReservedName(name, Language.TypeScript, namespace(name)))
      )
    problem.toLeft(()).flatMap { _ =>
      rules.traverse {
        case (q, ModelRule.Record(fs))             => record(q, fs).map(Some(_))
        case (q, ModelRule.Enumeration(cs))        => Some(enumeration(q, cs)).asRight
        case (q, ModelRule.Union(as, texts, cs))   => union(q, as, texts, cs).map(Some(_))
        case (q, ModelRule.ErrorFamily(ks, cases)) => errorFamily(q, ks, cases).map(Some(_))
        case (q, ModelRule.Alias(t))               => Some(alias(q, t)).asRight
        case (q, ModelRule.Constant(v))            => Some(constant(q, v)).asRight
        case (_, ModelRule.PreviewType)            => None.asRight
        case (q, ModelRule.NamedTexts(ms))         => Some(namedTexts(q, ms)).asRight
        case (_, ModelRule.TextMember(_))          => None.asRight
      }.map { bodies =>
        val code = bodies.flatten.mkString("\n")
        val others = models.keys.collect {
          case q if q.module != name && code.contains(s"${namespace(q.module)}.${typeName(q)}") => q.module
        }.toList.distinct.sorted
        val imports =
          Option.when("(?<![A-Za-z0-9_$.])c\\.".r.findFirstIn(code).isDefined)(s"""import * as c from "../codec.js";""").toList ++
          Option.when(usesPreview(rules))(s"""import type { $PreviewType } from "../preview.js";""").toList ++
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

  /** The core's preview type, which an error's preview field holds (imported from `../preview.js`). */
  private val PreviewType = "Preview"

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
