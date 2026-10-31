package xyz.kd5ujc.modelsgen

import cats.syntax.all._

/**
 * Emits Rust for the dilf4s canonical profile ([[Profile]]): one module per schema file, against the runtime in
 * `sdk/rust/core/src/codec.rs`. The output mirrors [[CanonicalEmitter]]: the same rules, the same shape checks, the
 * same field order. Rust-specific choices:
 *
 *   - a record is a struct; a field holding a literal (`"version": 1`) carries no data, so it is left out of the struct
 *     and written and checked by the codec
 *   - an enumeration is a fieldless enum; a union is an enum with one variant per alternative
 *   - an alias is a type alias plus a codec constant (`TREE_INDEX`)
 *   - integer ranges use the smallest of `u32`, `u64`, `i32`, `i64` that holds them, otherwise `BigInt`; a byte
 *     string of one fixed size is an array `[u8; N]`
 *   - a field whose type contains its own record by value is boxed
 *
 * Output is deterministic and clippy-clean; it is not rustfmt-formatted (the parent module is `#[rustfmt::skip]`).
 */
final class RustEmitter(models: Map[QName, ModelRule]) {

  import RustEmitter._

  private def outside(rule: QName, what: String): GenError =
    GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile(s"$what (Rust)"))

  private def typeName(q: QName): String = CanonicalEmitter.pascal(q.name)

  private def path(from: String, q: QName, name: String): String =
    if (q.module == from) name else s"super::${CanonicalEmitter.packageSegment(q.module)}::$name"

  /** Rules reachable from each rule through fields held by value (not through `Vec` or maps). */
  private val byValue: Map[QName, Set[QName]] = {
    def refs(t: FieldType): Set[QName] = t match {
      case FieldType.Ref(q) => Set(q)
      case _                => Set.empty
    }
    models.map {
      case (q, ModelRule.Record(fs))   => q -> fs.flatMap(f => refs(f.tpe)).toSet
      case (q, ModelRule.Union(as, _)) => q -> as.map(_.rule).toSet
      case (q, ModelRule.Alias(t))     => q -> refs(t)
      case (q, _)                      => q -> Set.empty[QName]
    }
  }

  private def reaches(from: QName, to: QName): Boolean = {
    @scala.annotation.tailrec
    def go(frontier: List[QName], seen: Set[QName]): Boolean = frontier match {
      case Nil                  => false
      case q :: _ if q == to    => true
      case q :: rest if seen(q) => go(rest, seen)
      case q :: rest            => go(byValue.getOrElse(q, Set.empty).toList ++ rest, seen + q)
    }
    go(List(from), Set.empty)
  }

  /** Rust type, codec type and codec expression of a field or element type. */
  private final case class Rs(tpe: String, codecType: String, codec: String)

  /**
   * `from` is the module the code is emitted into: an alias's target is expanded in the alias's own context (`rule`),
   * but every path in the result is written relative to `from`.
   */
  private def rs(rule: QName, t: FieldType, resolving: Set[QName], from: String): Either[GenError, Rs] = t match {
    case FieldType.Uint     => Rs("u64", "c::U64In", "c::UINT").asRight
    case FieldType.Int      => Rs("i128", "c::I128In", "c::INT").asRight
    case FieldType.Nint     => Rs("i128", "c::I128In", "c::NINT").asRight
    case FieldType.Bytes    => Rs("Vec<u8>", "c::Bytes", "c::Bytes").asRight
    case FieldType.Text     => Rs("String", "c::Text", "c::Text").asRight
    case FieldType.Bool     => Rs("bool", "c::Bool", "c::Bool").asRight
    case FieldType.AnyValue => Rs("c::Value", "c::Any", "c::Any").asRight
    case FieldType.IntRange(lo, hi) =>
      native(lo, hi)
        .fold(Rs("c::BigInt", "c::BigIntIn", s"c::BigIntIn { lo: Some(${bigBound(lo)}), hi: Some(${bigBound(hi)}) }")) { n =>
          Rs(n, s"c::${n.capitalize}In", s"c::${n.capitalize}In { lo: ${number(lo)}, hi: ${number(hi)} }")
        }
        .asRight
    case FieldType.SizedBytes(lo, hi) if lo == hi => Rs(s"[u8; $lo]", s"c::FixedBytes<$lo>", s"c::FixedBytes::<$lo>").asRight
    case FieldType.SizedBytes(lo, hi)             => Rs("Vec<u8>", "c::SizedBytes", s"c::SizedBytes { lo: $lo, hi: ${bound(hi)} }").asRight
    case FieldType.SizedText(lo, hi)              => Rs("String", "c::SizedText", s"c::SizedText { lo: $lo, hi: ${bound(hi)} }").asRight
    case FieldType.Matching(p)                    => Rs("String", "c::Matching", s"c::Matching(c::TextPattern::${patternName(p)})").asRight
    case FieldType.LitInt(v)                      => Rs("()", "c::LiteralInt", s"c::LiteralInt($v)").asRight
    case FieldType.LitText(v)                     => Rs("()", "c::LiteralText", s"c::LiteralText(${string(v)})").asRight
    case FieldType.Ref(q) =>
      models.get(q).toRight(GenError.UnresolvedName(rule.module, q.name)).flatMap {
        case ModelRule.Alias(_) if resolving(q) => outside(rule, s"alias cycle through ${q.name}").asLeft
        case ModelRule.Alias(target) =>
          rs(q, target, resolving + q, from).map(r => Rs(path(from, q, typeName(q)), r.codecType, path(from, q, constName(q))))
        case _ =>
          val n = path(from, q, typeName(q))
          Rs(n, s"c::ModelOf<$n>", s"c::ModelOf::<$n>::NEW").asRight
      }
    case FieldType.TextMap(value) =>
      rs(rule, value, resolving, from).flatMap(v =>
        element(rule, v).as(Rs(s"BTreeMap<String, ${v.tpe}>", s"c::TextMapOf<${v.codecType}>", s"c::TextMapOf(${v.codec})"))
      )
    case FieldType.Vec(e, min, max) =>
      rs(rule, e, resolving, from).flatMap(v =>
        element(rule, v).as(
          Rs(
            s"Vec<${v.tpe}>",
            s"c::VecOf<${v.codecType}>",
            s"c::VecOf { element: ${v.codec}, min: $min, max: ${max.fold("None")(m => s"Some($m)")} }"
          )
        )
      )
  }

  /** Literals carry no data, so they only make sense as required record fields. */
  private def element(rule: QName, r: Rs): Either[GenError, Unit] =
    Either.cond(r.tpe != "()", (), outside(rule, "literal outside a required record field"))

  private def doc(rule: QName): String = s"/// `${rule.name}` (`shared/schemas/${rule.module}.cddl`)."

  private final case class Field(key: String, name: String, rs: Rs, optional: Boolean, boxed: Boolean)

  private def fields(rule: QName, profiled: Vector[ProfileField]): Either[GenError, Vector[Field]] =
    profiled.traverse { f =>
      rs(rule, f.tpe, Set.empty, rule.module).flatMap { r =>
        val boxed = f.tpe match {
          case FieldType.Ref(q) => reaches(q, rule)
          case _                => false
        }
        if (r.tpe == "()" && f.optional) outside(rule, s"optional literal field ${f.key}").asLeft
        else
          Field(
            f.key,
            fieldName(f.key),
            if (boxed) Rs(s"Box<${r.tpe}>", s"c::Boxed<${r.codecType}>", s"c::Boxed(${r.codec})") else r,
            f.optional,
            boxed
          ).asRight
      }
    }.flatMap { fs =>
      val clash = fs.groupBy(_.name).collect { case (n, xs) if xs.size > 1 => n }
      Either.cond(clash.isEmpty, fs, GenError.NameClash(rule.toString, clash.mkString(", "), fs.map(_.key)))
    }

  private def record(rule: QName, profiled: Vector[ProfileField]): Either[GenError, String] =
    fields(rule, profiled).map { fs =>
      val n = typeName(rule)
      val data = fs.filter(_.rs.tpe != "()")
      val struct =
        if (data.isEmpty) s"pub struct $n;"
        else
          s"pub struct $n {\n${data.map { f =>
              val t = if (f.optional) s"Option<${f.rs.tpe}>" else f.rs.tpe
              s"    /// The `${string(f.key).drop(1).dropRight(1)}` entry.\n    pub ${f.name}: $t,"
            }.mkString("\n")}\n}"
      val enc = fs.map { f =>
        if (f.rs.tpe == "()") s"            c::field(${string(f.key)}, &${f.rs.codec}, &(), inner)?,"
        else if (f.optional) s"            c::optional_field(${string(f.key)}, &${f.rs.codec}, self.${f.name}.as_ref(), inner)?,"
        else s"            c::field(${string(f.key)}, &${f.rs.codec}, &self.${f.name}, inner)?,"
      }.mkString("\n")
      val keys = fs.map(f => string(f.key)).mkString(", ")
      val dec = fs.zipWithIndex.map {
        case (f, i) =>
          val call = s"fields.${if (f.optional) "optional" else "required"}(${string(f.key)}, &${f.rs.codec})?"
          if (f.rs.tpe == "()") s"        $call;" else s"        let f$i = $call;"
      }.mkString("\n")
      val build =
        if (data.isEmpty) "Self"
        else s"Self { ${fs.zipWithIndex.collect { case (f, i) if f.rs.tpe != "()" => s"${f.name}: f$i" }.mkString(", ")} }"
      val decodeBody =
        if (fs.isEmpty) s"        c::Fields::of(value, &[]).map(|_| Self)"
        else s"        let fields = c::Fields::of(value, &[$keys])?;\n$dec\n        Ok($build)"
      s"""${doc(rule)}
         |#[derive(Clone, Debug, PartialEq, Eq)]
         |$struct
         |
         |impl c::Model for $n {
         |    fn to_value_at(&self, depth: usize) -> Result<c::Value, c::CanonicalError> {
         |        let inner = c::nested(depth)?;
         |        c::record([
         |$enc
         |        ])
         |    }
         |
         |    fn from_value(value: &c::Value) -> Result<Self, c::DecodeFailure> {
         |$decodeBody
         |    }
         |}
         |""".stripMargin.replace(
        "        let inner = c::nested(depth)?;\n        c::record([\n\n        ])",
        "        c::nested(depth)?;\n        c::record([])"
      )
    }

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): Either[GenError, String] =
    cases.find { case (_, v) => v < Profile.IntMin || v > Profile.IntMax }.fold {
      val n = typeName(rule)
      val names = cases.map { case (l, v) => (CanonicalEmitter.pascal(l), l, v) }
      s"""${doc(rule)}
         |#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
         |pub enum $n {
         |${names.map { case (cn, l, v) => s"    /// `$l`, written as $v.\n    $cn," }.mkString("\n")}
         |}
         |
         |impl $n {
         |    /// The integer this case is written as.
         |    pub const fn wire(self) -> i128 {
         |        match self {
         |${names.map { case (cn, _, v) => s"            Self::$cn => ${number(v)}," }.mkString("\n")}
         |        }
         |    }
         |}
         |
         |impl c::Model for $n {
         |    fn to_value_at(&self, _depth: usize) -> Result<c::Value, c::CanonicalError> {
         |        Ok(c::Value::int(self.wire()))
         |    }
         |
         |    fn from_value(value: &c::Value) -> Result<Self, c::DecodeFailure> {
         |        let n = c::Codec::decode(&c::INT, value)?;
         |        match n {
         |${names.map { case (cn, _, v) => s"            ${number(v)} => Ok(Self::$cn)," }.mkString("\n")}
         |            _ => Err(c::DecodeFailure::new(alloc::format!("{n} is not a $n"))),
         |        }
         |    }
         |}
         |""".stripMargin.asRight[GenError]
    } { case (l, _) => outside(rule, s"enumeration case $l beyond CDDL int").asLeft }

  private def union(rule: QName, alts: Vector[UnionAlternative], texts: Vector[String]): Either[GenError, String] = {
    val n = typeName(rule)
    val recordVariants = alts.map(a => typeName(a.rule) -> path(rule.module, a.rule, typeName(a.rule)))
    val textVariants = texts.map(t => CanonicalEmitter.pascal(t) -> t)
    val variantNames = recordVariants.map(_._1) ++ textVariants.map(_._1)
    val dup = variantNames.diff(variantNames.distinct)
    if (dup.nonEmpty || textVariants.exists(_._1.isEmpty)) GenError.NameClash(rule.toString, dup.mkString(", "), variantNames).asLeft
    else {
      val variants = recordVariants.map { case (v, t) => s"    /// A [`$t`].\n    $v($t)," } ++
        textVariants.map { case (v, t) => s"    /// The text `${string(t).drop(1).dropRight(1)}`.\n    $v," }
      val enc = recordVariants.map { case (v, _) => s"            Self::$v(a) => c::Model::to_value_at(a, depth)," } ++
        textVariants.map { case (v, t) => s"            Self::$v => Ok(c::Value::text(${string(t)}))," }
      def keys(s: Set[String]) = s"&[${s.toList.sorted.map(string).mkString(", ")}]"
      val records = alts.map(a => s"(${keys(a.required)}, ${keys(a.allowed)})").mkString(", ")
      val dec = recordVariants.zipWithIndex.map {
        case ((v, t), i) => s"            c::Alternative::Record($i) => <$t as c::Model>::from_value(value).map(Self::$v),"
      } ++ textVariants.zipWithIndex.map { case ((v, _), i) => s"            c::Alternative::Text($i) => Ok(Self::$v)," }
      s"""${doc(rule)}
         |#[derive(Clone, Debug, PartialEq, Eq)]
         |pub enum $n {
         |${variants.mkString("\n")}
         |}
         |
         |impl c::Model for $n {
         |    fn to_value_at(&self, ${if (recordVariants.isEmpty) "_depth" else "depth"}: usize) -> Result<c::Value, c::CanonicalError> {
         |        match self {
         |${enc.mkString("\n")}
         |        }
         |    }
         |
         |    fn from_value(value: &c::Value) -> Result<Self, c::DecodeFailure> {
         |        match c::select_alternative(value, &[$records], &[${texts.map(string).mkString(", ")}])? {
         |${dec.mkString("\n")}
         |            other => Err(c::unknown_alternative(other)),
         |        }
         |    }
         |}
         |""".stripMargin.asRight
    }
  }

  private def alias(rule: QName, t: FieldType): Either[GenError, String] =
    rs(rule, t, Set(rule), rule.module).flatMap(r => element(rule, r).as(r)).map { r =>
      s"""${doc(rule)}
         |pub type ${typeName(rule)} = ${r.tpe};
         |
         |/// The codec of [`${typeName(rule)}`].
         |pub const ${constName(rule)}: ${r.codecType} = ${r.codec};
         |""".stripMargin
    }

  /** The source of one module. */
  def module(name: String): Either[GenError, String] = {
    val rules = models.toVector.collect { case (q, m) if q.module == name => q -> m }.sortBy(_._1.name)
    val clash = rules.groupBy { case (q, _) => typeName(q) }.collect { case (t, qs) if qs.size > 1 => t -> qs.map(_._1.name) }
    clash.headOption.fold {
      rules.traverse {
        case (q, ModelRule.Record(fs))       => record(q, fs)
        case (q, ModelRule.Enumeration(cs))  => enumeration(q, cs)
        case (q, ModelRule.Union(as, texts)) => union(q, as, texts)
        case (q, ModelRule.Alias(t))         => alias(q, t)
      }.map { bodies =>
        val code = bodies.mkString("\n")
        val imports = List(
          "alloc::boxed::Box"            -> "Box<",
          "alloc::collections::BTreeMap" -> "BTreeMap<",
          "alloc::string::String"        -> "String",
          "alloc::vec::Vec"              -> "Vec<"
        ).collect { case (i, used) if code.contains(used) => s"use $i;" }
        s"""//! GENERATED by models-gen from `shared/schemas/$name.cddl`. Do not edit.
           |
           |${(imports :+ "use crate::codec as c;").mkString("\n")}
           |
           |$code""".stripMargin
      }
    } { case (t, qs) => GenError.NameClash(name, t, qs).asLeft }
  }

  /** `generated/mod.rs`, declaring every module. */
  def index(modules: Vector[String]): String =
    s"""//! The wire models of `shared/schemas`, one module per schema file. GENERATED by models-gen. Do not edit.
       |
       |${modules.sorted.map(m => s"pub mod ${CanonicalEmitter.packageSegment(m)};").mkString("\n")}
       |""".stripMargin
}

object RustEmitter {

  // scalafix:off DisableSyntax.noTry
  /** Rust 2024 keywords, strict and reserved. */
  private val keywords = Set(
    "abstract",
    "as",
    "async",
    "await",
    "become",
    "box",
    "break",
    "const",
    "continue",
    "crate",
    "do",
    "dyn",
    "else",
    "enum",
    "extern",
    "false",
    "final",
    "fn",
    "for",
    "gen",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "macro",
    "match",
    "mod",
    "move",
    "mut",
    "override",
    "priv",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "try",
    "type",
    "typeof",
    "unsafe",
    "unsized",
    "use",
    "virtual",
    "where",
    "while",
    "yield"
  )
  // scalafix:on DisableSyntax.noTry

  /** Keywords that cannot be raw identifiers. */
  private val unrawable = Set("crate", "self", "Self", "super")

  private def snakeWords(s: String): List[String] =
    s.replaceAll("([a-z0-9])([A-Z])", "$1_$2").split("[-_ .]+").toList.filter(_.nonEmpty).map(_.toLowerCase)

  /** A wire key as a Rust field name: `contentHash` is `content_hash`, `type` is `r#type`. */
  def fieldName(key: String): String = {
    val snake = snakeWords(key) match {
      case Nil   => "_"
      case words => words.mkString("_")
    }
    if (unrawable(snake)) s"${snake}_" else if (keywords(snake)) s"r#$snake" else snake
  }

  def constName(q: QName): String = snakeWords(q.name).mkString("_").toUpperCase

  def patternName(pattern: String): String = pattern match {
    case "([0-9a-f]{2})*"   => "Hex"
    case "0x([0-9a-f]{2})*" => "Hex0x"
    case _                  => "Decimal"
  }

  /** A Rust string literal. */
  def string(s: String): String =
    "\"" + s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case c if c < ' ' => f"\\u{${c.toInt}%x}"
      case c            => c.toString
    } + "\""

  /** The smallest native integer type holding `lo..hi`, if any. */
  def native(lo: BigInt, hi: BigInt): Option[String] =
    if (lo >= 0 && hi <= BigInt("4294967295")) Some("u32")
    else if (lo >= 0 && hi <= BigInt("18446744073709551615")) Some("u64")
    else if (lo >= Int.MinValue && hi <= Int.MaxValue) Some("i32")
    else if (lo >= Long.MinValue && hi <= Long.MaxValue) Some("i64")
    else None

  /** A decimal literal with `_` every three digits (clippy's `unreadable_literal`). */
  def number(n: BigInt): String = {
    val digits = n.abs.toString
    val grouped = if (digits.length <= 4) digits else digits.reverse.grouped(3).mkString("_").reverse
    if (n.signum < 0) s"-$grouped" else grouped
  }

  def bigBound(n: BigInt): String = {
    val magnitude = n.abs.toByteArray.dropWhile(_ == 0).map(b => f"0x${b & 0xff}%02x").mkString(", ")
    s"c::BigBound { negative: ${n.signum < 0}, magnitude: &[$magnitude] }"
  }

  def bound(hi: Int): String = if (hi == Int.MaxValue) "None" else s"Some($hi)"
}
