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
    val clash = fs.groupBy(_.name).collect { case (n, xs) if xs.size > 1 => n }
    Either.cond(clash.isEmpty, fs, GenError.NameClash(rule.toString, clash.mkString(", "), fs.map(_.key)))
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

  private def union(rule: QName, alts: Vector[UnionAlternative], texts: Vector[String]): Either[GenError, String] = {
    val n = typeName(rule)
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
         |export type $n = ${(names ++ texts.map(string)).mkString(" | ")};
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

  private def alias(rule: QName, t: FieldType): String = {
    val (tpe, codec) = ts(rule, t)
    val n = typeName(rule)
    s"""${doc(rule)}
       |export type $n = $tpe;
       |
       |export const $n: c.Codec<$n> = $codec;
       |""".stripMargin
  }

  /** The source of one module. */
  def module(name: String): Either[GenError, String] = {
    val rules = models.toVector.collect { case (q, m) if q.module == name => q -> m }.sortBy(_._1.name)
    val clash = rules.groupBy { case (q, _) => typeName(q) }.collect { case (t, qs) if qs.size > 1 => t -> qs.map(_._1.name) }
    clash.headOption.fold {
      rules.traverse {
        case (q, ModelRule.Record(fs))       => record(q, fs)
        case (q, ModelRule.Enumeration(cs))  => enumeration(q, cs).asRight
        case (q, ModelRule.Union(as, texts)) => union(q, as, texts)
        case (q, ModelRule.Alias(t))         => alias(q, t).asRight
      }.map { bodies =>
        val code = bodies.mkString("\n")
        val others = models.keys.collect {
          case q if q.module != name && code.contains(s"${namespace(q.module)}.${typeName(q)}") => q.module
        }.toList.distinct.sorted
        val imports = s"""import * as c from "../codec.js";""" ::
          others.map(m => s"""import * as ${namespace(m)} from "./${CanonicalEmitter.packageSegment(m)}.js";""")
        s"""// GENERATED by models-gen from shared/schemas/$name.cddl. Do not edit.
           |${imports.mkString("\n")}
           |
           |$code""".stripMargin
      }
    } { case (t, qs) => GenError.NameClash(name, t, qs).asLeft }
  }

  /** `generated/index.ts`, one namespace per module. */
  def index(modules: Vector[String]): String =
    s"""// The wire models of shared/schemas, one namespace per schema file. GENERATED by models-gen. Do not edit.
       |${modules.sorted.map(m => s"""export * as ${namespace(m)} from "./${CanonicalEmitter.packageSegment(m)}.js";""").mkString("\n")}
       |""".stripMargin
}

object TsEmitter {

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
