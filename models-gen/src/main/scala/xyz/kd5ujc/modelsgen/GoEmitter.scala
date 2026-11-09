package xyz.kd5ujc.modelsgen

import cats.syntax.all._

/**
 * Emits Go for the dilf4s canonical profile ([[Profile]]): one package per schema file, against the runtime in
 * `sdk/go/core/codec.go`. The output mirrors [[CanonicalEmitter]] and [[RustEmitter]]: the same rules, the same shape
 * checks, the same field order. Go-specific choices:
 *
 *   - the modules core's own API is built on ([[GoEmitter.CoreModules]]: its error families and limits) are emitted
 *     into package `core` itself, as Go forbids the import cycle a package of their own would make; every other module
 *     is a package of its own, `core/generated/<module>`, importing core and the modules it names. A cycle between
 *     modules, or a core module naming another, is a generator error
 *   - a record is a struct with exported fields, documented as a value the caller owns: decoding copies everything
 *     out of the canonical value, and encoding copies everything in, so no model shares memory with a value. A field
 *     holding a literal (`"version": 1`) carries no data, so it is left out of the struct and written and checked by
 *     the codec; an optional field is a `core.Optional`; a field reaching its own record by value is a pointer
 *   - a choice with a record alternative, or naming a choice of texts, is a sealed interface: an unexported marker
 *     method, one struct per alternative (a record of the same package is its own alternative; a text is an empty
 *     struct; a record or choice of another package is wrapped), told apart by Go type when encoding
 *   - a choice of texts alone, and named texts, are a string type with a constant per member; an enumeration an
 *     `int64` type with a constant per case (its wire integer). Go has no closed enums, so `Valid` checks membership,
 *     every encoder refuses a non-member as `TypeMismatch`, and decoders give members only
 *   - an error family is a sealed interface embedding `error`, with `Kind()`, and one struct per kind; no codec
 *   - an alias is a Go type alias, with the codec functions of its target
 *   - integer ranges use the smallest of `uint32`, `uint64`, `int32`, `int64` that holds them (as Rust's), otherwise
 *     `core.Int`; a byte string of one fixed size is an array `[N]byte`
 *   - a text-keyed map is a `core.TextMap`, in canonical key order, whose keys (a caller's or a peer's) are compared,
 *     never hashed, as a Go map would
 *   - a constant is an untyped Go constant, which every integer type it fits takes without a conversion
 *   - the emitter fixtures (`sdk/go/internal/fixtures`) are emitted under their own `root`, with no core modules
 *   - every rule `X` with a codec has `EncodeX(x, at)` and `DecodeX(v)`; `at` is a `core.Encoding`, its nesting level
 *     and weight budget (the zero one is the root): a Go model can share its parts, so encoding one is bounded by the
 *     weight of the value it builds, never by the model's memory
 *
 * Output is deterministic and gofmt-clean: every struct field, constant and method stands alone between blank lines,
 * so nothing needs gofmt's alignment.
 */
final class GoEmitter(
  models:  Map[QName, ModelRule],
  docs:    QName => Vector[String] = _ => Vector.empty,
  root:    String = GoEmitter.DefaultRoot,
  schemas: String = "shared/schemas"
) {

  import GoEmitter._

  /**
   * The modules emitted into package core: [[GoEmitter.CoreModules]] for the SDK's own schemas, none for another root
   * (the emitter fixtures), whose packages all import core.
   */
  private def isCore(module: String): Boolean = root == DefaultRoot && CoreModules(module)

  private def samePackage(a: String, b: String): Boolean = a == b || (isCore(a) && isCore(b))

  private def packageOf(module: String): String = if (isCore(module)) "core" else GoEmitter.packageOf(module)

  /** Where a module's source goes, under `sdk/go`. */
  def pathOf(module: String): String =
    if (isCore(module)) s"core/${CanonicalEmitter.packageSegment(module)}_gen.go"
    else s"$root/${packageOf(module)}/${packageOf(module)}_gen.go"

  /** The test listing every model, under `sdk/go`. */
  val registryPath: String = s"$root/models_gen_test.go"

  /** A module's package as Go imports it. */
  private def importPath(module: String): String = s"$ModulePath/$root/${packageOf(module)}"

  private def outside(rule: QName, what: String): GenError =
    GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile(s"$what (Go)"))

  private def typeName(q: QName): String = CanonicalEmitter.pascal(q.name)

  /** A runtime name as `from` refers to it: unqualified inside package core, `core.X` elsewhere. */
  private def rt(from: String, name: String): String = if (isCore(from)) name else s"core.$name"

  /** A generated name of `q` as `from` refers to it: unqualified in its own package, `pkg.X` elsewhere. */
  private def qualify(from: String, q: QName, name: String): String =
    if (samePackage(from, q.module)) name else s"${packageOf(q.module)}.$name"

  /**
   * Rules reachable from each record through fields held by value: records, aliases and optionals, not slices, maps,
   * interfaces or the named types of texts and enumerations.
   */
  private val byValue: Map[QName, Set[QName]] = {
    def refs(t: FieldType, seen: Set[QName]): Set[QName] = t match {
      case FieldType.Ref(q) if seen(q) => Set.empty
      case FieldType.Ref(q) =>
        models.get(q) match {
          case Some(ModelRule.Record(_))     => Set(q)
          case Some(ModelRule.Alias(target)) => refs(target, seen + q)
          case _                             => Set.empty
        }
      case _ => Set.empty
    }
    models.collect { case (q, ModelRule.Record(fs)) => q -> fs.flatMap(f => refs(f.tpe, Set.empty)).toSet }
  }

  private def reaches(from: QName, to: QName): Boolean = {
    @scala.annotation.tailrec
    def go(frontier: List[QName], seen: Set[QName]): Boolean = frontier match {
      case Nil                  => false
      case q :: _ if q == to    => true
      case q :: rest if seen(q) => go(rest, seen)
      case q :: rest            => go(byValue.getOrElse(q, Set.empty).toList.sortBy(_.toString) ++ rest, seen + q)
    }
    go(List(from), Set.empty)
  }

  /** The record a field type holds by value, through aliases, if any. */
  @scala.annotation.tailrec
  private def recordOf(t: FieldType, seen: Set[QName] = Set.empty): Option[QName] = t match {
    case FieldType.Ref(q) if !seen(q) =>
      models.get(q) match {
        case Some(ModelRule.Record(_))     => Some(q)
        case Some(ModelRule.Alias(target)) => recordOf(target, seen + q)
        case _                             => None
      }
    case _ => None
  }

  /** Whether a field holds an error family directly (not through an alias). */
  private def family(t: FieldType): Boolean = t match {
    case FieldType.Ref(q) =>
      models.get(q) match {
        case Some(ModelRule.ErrorFamily(_, _)) => true
        case _                                 => false
      }
    case _ => false
  }

  /** A Go type, its encoder (a `func(T, int) (Value, error)` expression) and its decoder (`func(Value) (T, error)`). */
  private final case class Go(tpe: String, enc: String, dec: String)

  /**
   * `from` is the module the code is emitted into: an alias's target is checked in the alias's own context (`rule`),
   * but every name in the result is written relative to `from`.
   */
  private def go(rule: QName, t: FieldType, resolving: Set[QName], from: String): Either[GenError, Go] = {
    def c(n:       String) = rt(from, n)
    def codec(tpe: String, value: String) = Go(tpe, s"$value.Encode", s"$value.Decode")
    t match {
      case FieldType.Uint     => Go("uint64", c("EncodeUint"), c("DecodeUint")).asRight
      case FieldType.Int      => Go(c("Int"), c("EncodeInt"), c("DecodeInt")).asRight
      case FieldType.Nint     => Go(c("Int"), c("EncodeNint"), c("DecodeNint")).asRight
      case FieldType.Bytes    => Go("[]byte", c("EncodeBytes"), c("DecodeBytes")).asRight
      case FieldType.Text     => Go("string", c("EncodeText"), c("DecodeText")).asRight
      case FieldType.Bool     => Go("bool", c("EncodeBool"), c("DecodeBool")).asRight
      case FieldType.AnyValue => Go(c("Value"), c("EncodeAny"), c("DecodeAny")).asRight
      case FieldType.IntRange(lo, hi) =>
        RustEmitter
          .native(lo, hi)
          .fold(codec(c("Int"), s"${c("IntIn")}{Lo: ${bigBound(lo, c("BigBound"))}, Hi: ${bigBound(hi, c("BigBound"))}}")) { n =>
            val (tpe, name) = NativeCodecs(n)
            codec(tpe, s"${c(name)}{Lo: $lo, Hi: $hi}")
          }
          .asRight
      case FieldType.SizedBytes(lo, hi) if lo == hi   => Go(s"[$lo]byte", s"encodeBytes$lo", s"decodeBytes$lo").asRight
      case FieldType.SizedBytes(lo, hi)               => codec("[]byte", s"${c("SizedBytes")}{Lo: $lo, Hi: ${bound(hi)}}").asRight
      case FieldType.SizedText(lo, hi)                => codec("string", s"${c("SizedText")}{Lo: $lo, Hi: ${bound(hi)}}").asRight
      case FieldType.Matching(p)                      => codec("string", s"${c("Matching")}{Pattern: ${c(patternName(p))}}").asRight
      case FieldType.LitInt(_) | FieldType.LitText(_) => Go("", "", "").asRight
      // an error's field only (Profile refuses it in a wire type): the core's Preview
      case FieldType.Preview => Go(c("Preview"), "", "").asRight
      case FieldType.Ref(q) =>
        models.get(q).toRight(GenError.UnresolvedName(rule.module, q.name)).flatMap {
          case ModelRule.Alias(_) if resolving(q) => outside(rule, s"alias cycle through ${q.name}").asLeft
          case ModelRule.Alias(target)            => go(q, target, resolving + q, from).flatMap(element(rule, _)).as(named(from, q))
          case ModelRule.ErrorFamily(_, _)        => Go(qualify(from, q, typeName(q)), "", "").asRight
          case _                                  => named(from, q).asRight
        }
      case FieldType.TextMap(value) =>
        go(rule, value, resolving, from).flatMap(element(rule, _)).map { v =>
          codec(s"${c("TextMap")}[${v.tpe}]", s"${c("TextMapCodec")}[${v.tpe}]{Enc: ${v.enc}, Dec: ${v.dec}}")
        }
      case FieldType.Vec(e, min, max) =>
        go(rule, e, resolving, from).flatMap(element(rule, _)).map { v =>
          codec(s"[]${v.tpe}", s"${c("VecOf")}[${v.tpe}]{Lo: $min, Hi: ${max.getOrElse(-1)}, Enc: ${v.enc}, Dec: ${v.dec}}")
        }
    }
  }

  /** A rule with a codec of its own, by its type and functions. */
  private def named(from: String, q: QName): Go =
    Go(qualify(from, q, typeName(q)), qualify(from, q, s"Encode${typeName(q)}"), qualify(from, q, s"Decode${typeName(q)}"))

  /** Literals carry no data, so they only make sense as required record fields. */
  private def element(rule: QName, g: Go): Either[GenError, Go] =
    Either.cond(g.tpe.nonEmpty, g, outside(rule, "literal outside a required record field"))

  /** The byte-string sizes held as arrays in `t`, each of which a module gives a pair of helper functions. */
  private def fixedSizes(t: FieldType): Set[Int] = t match {
    case FieldType.SizedBytes(lo, hi) if lo == hi => Set(lo)
    case FieldType.TextMap(value)                 => fixedSizes(value)
    case FieldType.Vec(e, _, _)                   => fixedSizes(e)
    case _                                        => Set.empty
  }

  /** A rule's doc comment: its name, where it is defined (and `more`), then its CDDL comment. */
  private def doc(rule: QName, name: String, more: String = ""): String =
    (s"// $name is `${rule.name}` ($schemas/${rule.module}.cddl)$more." +: lines(docs(rule), "", paragraph = true)).mkString("\n")

  /** Go comment lines at `indent`; a blank line first when `paragraph` and there are any. */
  private def lines(text: Vector[String], indent: String, paragraph: Boolean = false): Vector[String] = {
    val body = text.map(l => if (l.isEmpty) s"$indent//" else s"$indent// $l")
    if (paragraph && body.nonEmpty) s"$indent//" +: body else body
  }

  /** Documentation from the schema when it has some, else `fallback`. */
  private def documented(text: Vector[String], fallback: String, indent: String): String =
    (if (text.isEmpty) Vector(s"$indent// $fallback") else lines(text, indent)).mkString("\n")

  private final case class Field(key: String, name: String, go: Go, optional: Boolean, literal: Option[FieldType], doc: Vector[String])

  private def fields(rule: QName, profiled: Vector[ProfileField], inError: Boolean): Either[GenError, Vector[Field]] =
    profiled.traverse { f =>
      go(rule, f.tpe, Set.empty, rule.module).flatMap { g =>
        val literal = f.tpe match {
          case FieldType.LitInt(_) | FieldType.LitText(_) => Some(f.tpe)
          case _                                          => None
        }
        val boxed = !inError && recordOf(f.tpe).exists(reaches(_, rule))
        // an error's own `error` (the error it wraps) is Go's Err, as Error is the method every error has
        val name = if (inError && fieldName(f.key) == "Error") WrappedError else fieldName(f.key)
        if (literal.isDefined && f.optional) outside(rule, s"optional literal field ${f.key}").asLeft
        else if (!name.headOption.exists(_.isUpper)) outside(rule, s"key ${CanonicalEmitter.literal(f.key)} has no exported Go name").asLeft
        else {
          val held =
            if (boxed)
              Go(
                s"*${g.tpe}",
                s"${rt(rule.module, "Pointer")}[${g.tpe}]{Enc: ${g.enc}, Dec: ${g.dec}}.Encode",
                s"${rt(rule.module, "Pointer")}[${g.tpe}]{Enc: ${g.enc}, Dec: ${g.dec}}.Decode"
              )
            else g
          Field(f.key, name, held, f.optional, literal, f.doc).asRight
        }
      }
    }.flatMap { fs =>
      fs.groupBy(_.name)
        .toVector
        .sortBy(_._1)
        .collectFirst { case (n, xs) if xs.size > 1 => GenError.NameClash(rule.toString, n, xs.map(_.key)) }
        .toLeft(fs)
    }

  private def fieldType(f: Field, from: String): String = if (f.optional) s"${rt(from, "Optional")}[${f.go.tpe}]" else f.go.tpe

  /** A struct type: each field documented and standing alone, so gofmt aligns nothing. */
  private def struct(name: String, fs: Vector[Field], from: String, fallback: Field => String): String =
    if (fs.isEmpty) s"type $name struct{}"
    else
      s"type $name struct {\n${fs.map(f => s"${documented(f.doc, fallback(f), "\t")}\n\t${f.name} ${fieldType(f, from)}").mkString("\n\n")}\n}"

  private def literalValue(t: FieldType, from: String): (String, String) = t match {
    case FieldType.LitInt(v)  => (s"${rt(from, "EncodeLiteralInt")}($v, inner)", s"${rt(from, "LiteralInt")}($v)")
    case FieldType.LitText(v) => (s"${rt(from, "EncodeLiteralText")}(${string(v)}, inner)", s"${rt(from, "LiteralText")}(${string(v)})")
    case _                    => ("", "")
  }

  private def record(rule: QName, profiled: Vector[ProfileField]): Either[GenError, String] =
    fields(rule, profiled, inError = false).map { fs =>
      val n = typeName(rule)
      val from = rule.module
      val value = rt(from, "Value")
      val data = fs.filter(_.literal.isEmpty)
      val fail = s"return $value{}, err"
      val encodes = fs.zipWithIndex.map {
        case (f, i) =>
          f.literal match {
            case Some(l) =>
              s"\tf$i, err := ${literalValue(l, from)._1}\n\tif err != nil {\n\t\t$fail\n\t}"
            case None if f.optional =>
              s"\tf$i, err := ${rt(from, "EncodeOptional")}(x.${f.name}, inner, ${f.go.enc})\n\tif err != nil {\n\t\t$fail\n\t}"
            case None =>
              s"\tf$i, err := ${f.go.enc}(x.${f.name}, inner)\n\tif err != nil {\n\t\t$fail\n\t}"
          }
      }
      val entries = fs.zipWithIndex.map {
        case (f, i) => if (f.optional) s"${rt(from, "Opt")}(${string(f.key)}, f$i)" else s"${rt(from, "Req")}(${string(f.key)}, f$i)"
      }
      val nested = s"\tinner, err := at.Nested()\n\tif err != nil {\n\t\t$fail\n\t}"
      val encodeBody = (nested +: encodes :+ s"\treturn inner.Record(${entries.mkString(", ")})").mkString("\n")
      val zero = s"$n{}"
      val keys = fs.map(f => string(f.key)).mkString(", ")
      val decodes = fs.zipWithIndex.map {
        case (f, i) =>
          f.literal match {
            case Some(l) =>
              s"\tif _, err := ${rt(from, "Required")}(f, ${string(f.key)}, ${literalValue(l, from)._2}); err != nil {\n\t\treturn $zero, err\n\t}"
            case None =>
              val call = if (f.optional) "OptionalField" else "Required"
              s"\tf$i, err := ${rt(from, call)}(f, ${string(f.key)}, ${f.go.dec})\n\tif err != nil {\n\t\treturn $zero, err\n\t}"
          }
      }
      val build = fs.zipWithIndex.collect { case (f, i) if f.literal.isEmpty => s"${f.name}: f$i" }.mkString(", ")
      val decodeBody =
        if (fs.isEmpty) s"\tif _, err := ${rt(from, "RecordFields")}(v); err != nil {\n\t\treturn $zero, err\n\t}\n\treturn $zero, nil"
        else
          s"\tf, err := ${rt(from, "RecordFields")}(v${if (keys.isEmpty) "" else s", $keys"})\n\tif err != nil {\n\t\treturn $zero, err\n\t}\n${decodes.mkString("\n")}\n\treturn $n{$build}, nil"
      val fallback = (f: Field) => s"${f.name} is the `${string(f.key).drop(1).dropRight(1)}` entry."
      s"""${doc(rule, n)}
         |${struct(n, data, from, fallback)}
         |
         |// Encode$n is the canonical value of x, encoded at at.
         |func Encode$n(x $n, at ${rt(from, "Encoding")}) ($value, error) {
         |$encodeBody
         |}
         |
         |// Decode$n is the $n of v.
         |func Decode$n(v $value) ($n, error) {
         |$decodeBody
         |}
         |""".stripMargin
    }

  /** The constants, `Valid`, `AllX`, `extra` and codec functions of a closed type whose members are constants. */
  private def closed(
    rule:       QName,
    n:          String,
    underlying: String,
    members:    Vector[(String, String, Vector[String])],
    enc:        String,
    dec:        String,
    extra:      String = ""
  ): String = {
    val from = rule.module
    val value = rt(from, "Value")
    val consts = members.map { case (m, v, d) => s"${documented(d, s"$m is $v.", "")}\nconst $m $n = $v" }.mkString("\n\n")
    s"""${doc(rule, n)}
       |type $n $underlying
       |
       |$consts
       |
       |// Valid reports whether x is a member of $n.
       |func (x $n) Valid() bool {
       |\tswitch x {
       |\tcase ${members.map(_._1).mkString(", ")}:
       |\t\treturn true
       |\t}
       |\treturn false
       |}
       |
       |// All$n is every member of $n, in schema order: a fresh slice.
       |func All$n() []$n {
       |\treturn []$n{${members.map(_._1).mkString(", ")}}
       |}$extra
       |
       |// Encode$n is the canonical value of x; a value that is not a member is a TypeMismatch.
       |func Encode$n(x $n, at ${rt(from, "Encoding")}) ($value, error) {
       |\treturn $enc
       |}
       |
       |// Decode$n is the member of $n that v is.
       |func Decode$n(v $value) ($n, error) {
       |\treturn $dec
       |}
       |""".stripMargin
  }

  /**
   * A labelled integer enumeration: a `uint32` type whose constants are its cases, numbered from 1 in schema order (so
   * the zero value is no case), each written as its integer (`Wire`), whatever the integer's width.
   */
  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): String = {
    val n = typeName(rule)
    val from = rule.module
    val members = cases.zipWithIndex.map {
      case ((l, v), i) =>
        (n + CanonicalEmitter.pascal(l), (i + 1).toString, Vector(s"${n + CanonicalEmitter.pascal(l)} is `$l`, written as $v."))
    }
    def wire(v: BigInt) = if (v.isValidLong) s"${rt(from, "IntOf")}($v)" else s"${bigBound(v, rt(from, "BigBound"))}.Int()"
    val arms = members.zip(cases).map { case ((m, _, _), (_, v)) => s"\tcase $m:\n\t\treturn ${wire(v)}" }.mkString("\n")
    val extra =
      s"""
         |
         |// Wire is the integer x is written as; 0 for a value that is not a case.
         |func (x $n) Wire() ${rt(from, "Int")} {
         |\tswitch x {
         |$arms
         |\t}
         |\treturn ${rt(from, "Int")}{}
         |}""".stripMargin
    val enc = s"""${rt(from, "EncodeEnum")}(x.Wire(), x.Valid(), ${string(n)}, at)"""
    val dec = s"""${rt(from, "DecodeEnum")}(v, All$n(), $n.Wire, ${string(n)})"""
    closed(rule, n, "uint32", members, enc, dec, extra)
  }

  /** A choice of texts alone, or named texts: a string type with a constant per member. */
  private def texts(rule: QName, members: Vector[(String, String, Vector[String])]): String = {
    val n = typeName(rule)
    val enc = s"""${rt(rule.module, "EncodeTextChoice")}(x, x.Valid(), ${string(n)}, at)"""
    val dec = s"""${rt(rule.module, "DecodeTextChoice")}(v, $n.Valid)"""
    closed(rule, n, "string", members.map { case (m, t, d) => (m, string(t), d) }, enc, dec)
  }

  /** The constant naming `text` in the closed text type `q`. */
  private def textConstant(from: String, q: QName, text: String): String = {
    val member = models.get(q) match {
      case Some(ModelRule.NamedTexts(ms)) => ms.find(_.text == text).fold(CanonicalEmitter.pascal(text))(_.name)
      case _                              => CanonicalEmitter.pascal(text)
    }
    qualify(from, q, typeName(q) + member)
  }

  private def union(
    rule:    QName,
    alts:    Vector[UnionAlternative],
    own:     Vector[String],
    choices: Vector[TextChoice]
  ): Either[GenError, String] = {
    val n = typeName(rule)
    val from = rule.module
    val value = rt(from, "Value")
    val marker = s"is$n"
    // each alternative: its Go type, whether it is declared here (else it is a wrapper struct, holding Value), the
    // alternative it comes from, and its encoder
    final case class Alt(tpe: String, own: Boolean, wraps: Option[String], source: String, encode: String)
    val records = alts.map { a =>
      if (samePackage(from, a.rule.module)) Alt(typeName(a.rule), own = true, None, a.rule.name, s"Encode${typeName(a.rule)}(alt, at)")
      else {
        val g = named(from, a.rule)
        Alt(n + typeName(a.rule), own = false, Some(g.tpe), a.rule.name, s"${g.enc}(alt.Value, at)")
      }
    }
    val textAlts = own.map(t =>
      Alt(
        n + CanonicalEmitter.pascal(t),
        own = false,
        None,
        CanonicalEmitter.literal(t),
        s"${rt(from, "EncodeLiteralText")}(${string(t)}, at)"
      )
    )
    val choiceAlts = choices.map { ch =>
      if (samePackage(from, ch.rule.module))
        Alt(typeName(ch.rule), own = true, None, ch.rule.name, s"Encode${typeName(ch.rule)}(alt, at)")
      else {
        val g = named(from, ch.rule)
        Alt(n + typeName(ch.rule), own = false, Some(g.tpe), ch.rule.name, s"${g.enc}(alt.Value, at)")
      }
    }
    val all = records ++ textAlts ++ choiceAlts
    val declared = all.filterNot(_.own)
    declared
      .groupBy(_.tpe)
      .toVector
      .sortBy(_._1)
      .collectFirst { case (t, xs) if xs.size > 1 => GenError.NameClash(rule.toString, t, xs.map(_.source)) }
      .orElse(all.collectFirst {
        case a if !Profile.isIdentifier(a.tpe) || !a.tpe.head.isUpper =>
          outside(rule, s"choice alternative ${a.source} has no identifier form")
      })
      .toLeft {
        val wrappers = declared.map { a =>
          val body = a.wraps.fold("struct{}")(w => s"struct {\n\t// Value is the alternative.\n\tValue $w\n}")
          val what = a.wraps.fold(s"the text ${a.source}")(w => s"a $w")
          s"// ${a.tpe} is $what, an alternative of $n.\ntype ${a.tpe} $body"
        }
        val markers = all.map(a => s"func (${a.tpe}) $marker() {}")
        val cases = all.map { a =>
          val bind = if (a.encode.contains("(alt")) "alt" else "_"
          (bind, s"\tcase ${a.tpe}:\n\t\treturn ${a.encode}")
        }
        val switchHead = if (cases.exists(_._1 == "alt")) "\tswitch alt := x.(type) {" else "\tswitch x.(type) {"
        def keys(s: Set[String]) = s"[]string{${s.toList.sorted.map(string).mkString(", ")}}"
        val recordKeys = alts.map(a => s"{Required: ${keys(a.required)}, Allowed: ${keys(a.allowed)}}").mkString(", ")
        val choiceTexts = choices.flatMap(ch => ch.texts.map(t => (ch, t)))
        val textList = (own ++ choiceTexts.map(_._2)).map(string).mkString(", ")
        val recordDecodes = records.zip(alts).zipWithIndex.map {
          case ((a, alt), i) =>
            val dec = named(from, alt.rule).dec
            val wrap = if (a.own) "rec" else s"${a.tpe}{Value: rec}"
            s"\tcase $i:\n\t\trec, err := $dec(v)\n\t\tif err != nil {\n\t\t\treturn nil, err\n\t\t}\n\t\treturn $wrap, nil"
        }
        val textDecodes = textAlts.zipWithIndex.map { case (a, i) => s"\tcase ${records.size + i}:\n\t\treturn ${a.tpe}{}, nil" }
        val choiceDecodes = choiceTexts.zipWithIndex.map {
          case ((ch, t), i) =>
            val constant = textConstant(from, ch.rule, t)
            val a = choiceAlts(choices.indexOf(ch))
            val made = if (a.own) constant else s"${a.tpe}{Value: $constant}"
            s"\tcase ${records.size + own.size + i}:\n\t\treturn $made, nil"
        }
        val names = all.map(_.tpe)
        val listed = names.splitAt(names.size - 1) match {
          case (Vector(), last) => last.mkString; case (init, last) => s"${init.mkString(", ")} and ${last.mkString}"
        }
        s"""${doc(rule, n, s": a choice of $listed, each held by value (a pointer to one is no alternative)")}
           |type $n interface {
           |\t$marker()
           |}
           |
           |${(wrappers ++ markers).mkString("\n\n")}
           |
           |// Encode$n is the canonical value of x, encoded at at: a nil x, or one of a type that is not an alternative, is
           |// a TypeMismatch.
           |func Encode$n(x $n, at ${rt(from, "Encoding")}) ($value, error) {
           |$switchHead
           |${cases.map(_._2).mkString("\n")}
           |\t}
           |\treturn $value{}, ${rt(from, "NoAlternative")}(${string(n)})
           |}
           |
           |// Decode$n is the alternative of $n that v is: a record by its keys, a text by itself.
           |func Decode$n(v $value) ($n, error) {
           |\ti, err := ${rt(from, "SelectAlternative")}(v, []${rt(from, "Keys")}{$recordKeys}, []string{$textList})
           |\tif err != nil {
           |\t\treturn nil, err
           |\t}
           |\tswitch i {
           |${(recordDecodes ++ textDecodes ++ choiceDecodes).mkString("\n")}
           |\t}
           |\treturn nil, ${rt(from, "UnknownAlternative")}(i)
           |}
           |""".stripMargin
      }
  }

  /** A sealed interface embedding `error`, with `Kind()`, and one struct per kind; no codec. */
  private def errorFamily(rule: QName, kinds: QName, cases: Vector[ErrorCase]): Either[GenError, String] = {
    val n = typeName(rule)
    val from = rule.module
    val k = qualify(from, kinds, typeName(kinds))
    cases
      .traverse(c => fields(rule, c.fields, inError = true).map(c -> _))
      .flatMap { withFields =>
        withFields.collectFirst {
          case (c, fs) if fs.exists(f => ReservedErrorFields(f.name)) => (c, fs.find(f => ReservedErrorFields(f.name)))
        }.collect { case (_, Some(f)) => GenError.ReservedName(rule.toString, Language.Go, f.name) }
          .toLeft(withFields)
      }
      .map { withFields =>
        val kindsDoc = withFields.map {
          case (c, fs) =>
            val s = n + c.kind
            val fallback = (f: Field) => s"${f.name} is its `${string(f.key).drop(1).dropRight(1)}`."
            val rendered =
              if (fs.isEmpty) s"\treturn ${string(c.kind)}"
              else {
                val format = s"${c.kind} (${fs.map(f => s"${f.key}: %v").mkString(", ")})"
                s"\treturn fmt.Sprintf(${string(format)}, ${fs.map(f => s"e.${f.name}").mkString(", ")})"
              }
            val receiver = if (fs.isEmpty) s"($s)" else s"(e $s)"
            // a kind wrapping another family's error (its `error`) unwraps to it, so errors.As finds the cause
            val unwrap = fs
              .zip(c.fields)
              .collectFirst { case (f, p) if f.name == WrappedError && !f.optional && family(p.tpe) => f }
              .fold("")(f => s"\n\n// Unwrap is the error this one wraps.\nfunc (e $s) Unwrap() error {\n\treturn e.${f.name}\n}")
            s"""${documented(c.doc, s"$s is the kind ${c.kind}.", "")}
               |${struct(s, fs, from, fallback)}
               |
               |// Kind is ${textConstant(from, kinds, c.kind)}.
               |func ($s) Kind() $k {
               |\treturn ${textConstant(from, kinds, c.kind)}
               |}
               |
               |func ($s) is$n() {}
               |
               |// Error is the kind and its fields.
               |func $receiver Error() string {
               |$rendered
               |}$unwrap""".stripMargin
        }
        s"""${doc(rule, n, s": an error family, by kind ($k)")}
           |type $n interface {
           |\terror
           |\t// Kind is the error's kind.
           |\tKind() $k
           |\tis$n()
           |}
           |
           |${kindsDoc.mkString("\n\n")}
           |""".stripMargin
      }
  }

  private def alias(rule: QName, t: FieldType): Either[GenError, String] =
    go(rule, t, Set(rule), rule.module).flatMap(element(rule, _)).map { g =>
      val n = typeName(rule)
      val value = rt(rule.module, "Value")
      s"""${doc(rule, n)}
         |type $n = ${g.tpe}
         |
         |// Encode$n is the canonical value of x, encoded at at.
         |func Encode$n(x $n, at ${rt(rule.module, "Encoding")}) ($value, error) {
         |\treturn ${g.enc}(x, at)
         |}
         |
         |// Decode$n is the $n of v.
         |func Decode$n(v $value) ($n, error) {
         |\treturn ${g.dec}(v)
         |}
         |""".stripMargin
    }

  private def constant(rule: QName, v: BigInt): String =
    s"""${doc(rule, typeName(rule))}
       |const ${typeName(rule)} = $v
       |""".stripMargin

  /** The pair of helpers holding a byte string of `size` bytes as an array. */
  private def fixedHelpers(size: Int, from: String): String = {
    val value = rt(from, "Value")
    s"""// encodeBytes$size is the byte string holding a copy of x.
       |func encodeBytes$size(x [$size]byte, at ${rt(from, "Encoding")}) ($value, error) {
       |\treturn ${rt(from, "EncodeBytes")}(x[:], at)
       |}
       |
       |// decodeBytes$size is the byte string v, if it is exactly $size bytes long.
       |func decodeBytes$size(v $value) ([$size]byte, error) {
       |\tvar x [$size]byte
       |\terr := ${rt(from, "DecodeFixedBytes")}(v, x[:])
       |\treturn x, err
       |}
       |""".stripMargin
  }

  /** Every package-level name a module declares, with the rule each comes from. */
  private def declarations(name: String, rules: Vector[(QName, ModelRule)]): Vector[(String, String)] =
    rules.flatMap {
      case (q, rule) =>
        val t = typeName(q)
        val codecs = Vector(s"Encode$t", s"Decode$t").map(_ -> q.name)
        rule match {
          case ModelRule.Record(_) | ModelRule.Alias(_) => (t -> q.name) +: codecs
          case ModelRule.Union(as, own, cs) =>
            val wrapped =
              (as.map(_.rule) ++ cs.map(_.rule)).filterNot(r => samePackage(name, r.module)).map(r => (t + typeName(r)) -> q.name)
            // a choice of texts alone is a closed text type, which lists its members too
            val all = Option.when(as.isEmpty && cs.isEmpty)(s"All$t" -> q.name).toVector
            ((t -> q.name) +: codecs) ++ all ++ wrapped ++ own.map(x => (t + CanonicalEmitter.pascal(x)) -> q.name)
          case ModelRule.Enumeration(cs) =>
            ((t -> q.name) +: codecs) ++ Vector(s"All$t" -> q.name) ++ cs.map { case (l, _) => (t + CanonicalEmitter.pascal(l)) -> q.name }
          case ModelRule.NamedTexts(ms) => ((t -> q.name) +: codecs) ++ Vector(s"All$t" -> q.name) ++ ms.map(m => (t + m.name) -> q.name)
          case ModelRule.ErrorFamily(_, cs) => (t -> q.name) +: cs.map(c => (t + c.kind) -> q.name)
          case ModelRule.Constant(_)        => Vector(t -> q.name)
          case _                            => Vector.empty
        }
    }

  /** The modules each module's code names, so its package imports them. */
  def references(name: String): Set[String] = {
    def refs(t: FieldType): Set[QName] = t match {
      case FieldType.Ref(q)       => Set(q)
      case FieldType.TextMap(v)   => refs(v)
      case FieldType.Vec(e, _, _) => refs(e)
      case _                      => Set.empty
    }
    models.toVector.collect { case (q, m) if q.module == name => m }.flatMap {
      case ModelRule.Record(fs)          => fs.flatMap(f => refs(f.tpe))
      case ModelRule.Union(as, _, cs)    => as.map(_.rule) ++ cs.map(_.rule)
      case ModelRule.ErrorFamily(ks, cs) => ks +: cs.flatMap(_.fields.flatMap(f => refs(f.tpe)))
      case ModelRule.Alias(t)            => refs(t).toVector
      case _                             => Vector.empty
    }.map(_.module).filterNot(samePackage(name, _)).toSet
  }

  /** The first cycle among the packages the modules make, as the modules on it, if any. */
  private def importCycle(modules: Vector[String]): Option[Vector[String]] = {
    val edges = modules.map(m => packageOf(m) -> references(m).map(packageOf)).groupMapReduce(_._1)(_._2)(_ ++ _)
    @scala.annotation.tailrec
    def walk(frontier: List[(String, Vector[String])], seen: Set[String]): Option[Vector[String]] = frontier match {
      case Nil => None
      case (p, path) :: rest =>
        val next = edges.getOrElse(p, Set.empty).toVector.sorted
        next.find(path.contains) match {
          case Some(back) => Some(path.dropWhile(_ != back) :+ back)
          case None =>
            val fresh = next.filterNot(seen)
            walk(fresh.map(q => q -> (path :+ q)).toList ++ rest, seen ++ fresh)
        }
    }
    edges.keys.toVector.sorted.view.flatMap(p => walk(List(p -> Vector(p)), Set(p))).headOption
  }

  /** Problems with the set of modules as Go packages: names, clashes, and cycles. */
  def check(modules: Vector[String]): Option[GenError] = {
    val core = modules.filter(isCore)
    // an import named like a local of the generated code (`f0`) would be shadowed by it
    val badName = modules.find { m =>
      val p = packageOf(m)
      !isCore(m) && (p.isEmpty || ReservedPackages(p) || !p.head.isLetter || p.matches("f[0-9]+"))
    }
    badName
      .map(m => GenError.ReservedName(m, Language.Go, packageOf(m)))
      .orElse(
        modules
          .filterNot(isCore)
          .groupBy(packageOf)
          .toVector
          .sortBy(_._1)
          .collectFirst { case (p, ms) if ms.size > 1 => GenError.NameClash("Go packages", p, ms.sorted) }
      )
      .orElse(core.collectFirst {
        case m if references(m).nonEmpty =>
          outside(
            QName(m, "*"),
            s"a core module naming ${references(m).toVector.sorted.mkString(", ")} (core cannot import a generated package)"
          )
      })
      .orElse(importCycle(modules).map(c => outside(QName(c.headOption.getOrElse(""), "*"), s"an import cycle ${c.mkString(" -> ")}")))
  }

  /** The source of one module, and its path under `sdk/go`. */
  def module(name: String): Either[GenError, (String, String)] = {
    val rules = models.toVector.collect { case (q, m) if q.module == name => q -> m }.sortBy(_._1.name)
    val declared = declarations(name, rules)
    val problem = declared
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .collectFirst { case (d, xs) if xs.size > 1 => GenError.NameClash(name, d, xs.map(_._2).distinct) }
      .orElse(declared.collectFirst {
        case (d, _) if !Profile.isIdentifier(d) || !d.head.isUpper => outside(QName(name, d), s"name $d has no exported Go form")
      })
      .orElse(
        Option
          .when(isCore(name))(declared.collectFirst { case (d, _) if CoreNames(d) => GenError.ReservedName(name, Language.Go, d) })
          .flatten
      )
    problem.toLeft(()).flatMap { _ =>
      rules.traverse {
        case (q, ModelRule.Record(fs))      => record(q, fs).map(Some(_))
        case (q, ModelRule.Enumeration(cs)) => Some(enumeration(q, cs)).asRight
        case (q, ModelRule.Union(Vector(), ts, Vector())) =>
          Some(
            texts(
              q,
              ts.map(t =>
                (
                  typeName(q) + CanonicalEmitter.pascal(t),
                  t,
                  Vector(s"${typeName(q) + CanonicalEmitter.pascal(t)} is the text ${CanonicalEmitter.literal(t)}.")
                )
              )
            )
          ).asRight
        case (q, ModelRule.Union(as, ts, cs))      => union(q, as, ts, cs).map(Some(_))
        case (q, ModelRule.ErrorFamily(ks, cases)) => errorFamily(q, ks, cases).map(Some(_))
        case (q, ModelRule.Alias(t))               => alias(q, t).map(Some(_))
        case (q, ModelRule.Constant(v))            => Some(constant(q, v)).asRight
        case (_, ModelRule.PreviewType)            => None.asRight
        case (q, ModelRule.NamedTexts(ms)) =>
          Some(
            texts(
              q,
              ms.map(m =>
                (
                  typeName(q) + m.name,
                  m.text,
                  if (m.doc.isEmpty) Vector(s"${typeName(q) + m.name} is the text ${CanonicalEmitter.literal(m.text)}.") else m.doc
                )
              )
            )
          ).asRight
        case (_, ModelRule.TextMember(_)) => None.asRight
      }.map { bodies =>
        val sizes = rules.flatMap {
          case (_, ModelRule.Record(fs)) => fs.flatMap(f => fixedSizes(f.tpe))
          case (_, ModelRule.Alias(t))   => fixedSizes(t).toVector
          case _                         => Vector.empty
        }.distinct.sorted
        val code = (bodies.flatten ++ sizes.map(fixedHelpers(_, name))).mkString("\n")
        // imports follow the code itself: comments and string literals removed, then qualified names only
        val body = code.linesIterator.filterNot(_.trim.startsWith("//")).mkString("\n").replaceAll("\"(\\\\.|[^\"\\\\])*\"", "\"\"")
        def uses(pkg: String) = s"(?<![A-Za-z0-9_.])$pkg\\.".r.findFirstIn(body).isDefined
        val imports = (Vector("fmt" -> "fmt").filter(p => uses(p._2)) ++
          Option.when(!isCore(name) && uses("core"))(ModulePath + "/core" -> "core").toVector ++
          references(name).toVector.sorted
            .filterNot(isCore)
            .map(m => (importPath(m), packageOf(m)))
            .filter(p => uses(p._2)))
          .map(_._1)
          .distinct
          .sorted
        val importBlock = imports match {
          case Vector()    => ""
          case Vector(one) => s"import ${string(one)}\n\n"
          case many        => s"import (\n${many.map(i => s"\t${string(i)}").mkString("\n")}\n)\n\n"
        }
        val pkg = if (isCore(name)) "core" else packageOf(name)
        val pkgDoc = if (isCore(name)) "" else s"// Package $pkg holds the models of $schemas/$name.cddl.\n"
        val source =
          s"""// Code generated by models-gen from $schemas/$name.cddl. DO NOT EDIT.
             |
             |${pkgDoc}package $pkg
             |
             |""".stripMargin + importBlock + code
        pathOf(name) -> source
      }
    }
  }

  /** The test that lists every generated model's decoder and encoder (`core/generated/models_gen_test.go`). */
  def registry(modules: Vector[String]): (String, String) = {
    val listed = modules.sorted.map { m =>
      m -> models.toVector.collect { case (q, rule) if q.module == m => q -> rule }.sortBy(_._1.name).collect {
        case (
              q,
              ModelRule.Record(_) | ModelRule.Union(_, _, _) | ModelRule.Alias(_) | ModelRule.Enumeration(_) | ModelRule.NamedTexts(_)
            ) =>
          val p = if (isCore(m)) "core" else packageOf(m)
          s"\t\tmodel(${string(s"$m:${q.name}")}, $p.Decode${typeName(q)}, $p.Encode${typeName(q)}),"
      }
    }
    val entries = listed.flatMap(_._2)
    // a package with nothing to list (error families alone) is not imported, as Go refuses an unused import
    // core only when a core module lists a model: a package of fixtures alone does not use it
    val usesCore = listed.exists { case (m, es) => es.nonEmpty && isCore(m) }
    val imports = (Option.when(usesCore)(s"$ModulePath/core").toVector ++ listed.collect {
      case (m, es) if es.nonEmpty && !isCore(m) => importPath(m)
    }).distinct.sorted
    registryPath ->
    s"""// Code generated by models-gen from $schemas. DO NOT EDIT.
         |
         |package ${root.split('/').lastOption.getOrElse("generated")}_test
         |
         |import (
         |${imports.map(i => s"\t${string(i)}").mkString("\n")}
         |)
         |
         |// models is every generated model's rule, decoder and encoder, in module and rule order: a fresh slice.
         |func models() []modelCodec {
         |\treturn []modelCodec{
         |${entries.mkString("\n")}
         |\t}
         |}
         |""".stripMargin
  }
}

object GoEmitter {

  /** The Go module of `sdk/go`. */
  val ModulePath = "github.com/scasplte2/dilf4s/sdk/go"

  /**
   * The modules core's own API is built on, emitted into package core: the codec and hash error families (which its
   * CBOR, JSON and hash functions return) and the limits (which they enforce).
   */
  val CoreModules: Set[String] = Set("codec-errors", "hash-errors", "limits")

  /** Where the SDK's own models go, under `sdk/go`: a package per schema. */
  val DefaultRoot = "core/generated"

  /** Where a module of the SDK's own schemas goes, under `sdk/go`. */
  def pathOf(module: String): String = new GoEmitter(Map.empty).pathOf(module)

  /** The SDK's test listing every model, under `sdk/go`. */
  val RegistryPath: String = s"$DefaultRoot/models_gen_test.go"

  /** The directory under `sdk/go` whose generated files a root's run owns: core and its packages, or the root. */
  def scope(root: String): String = if (root == DefaultRoot) "core" else root

  /** A module's Go package: its name, lowercase, without separators (`merkle-vectors` is `merklevectors`). */
  def packageOf(module: String): String = module.filter(_.isLetterOrDigit).toLowerCase

  /** Whether a file under `sdk/go` is the generator's (so it may be rewritten or removed). */
  def generated(path: String): Boolean = path.endsWith("_gen.go") || path.endsWith("_gen_test.go")

  /** A wire key as an exported Go field name: `contentHash` is `ContentHash`. */
  def fieldName(key: String): String = CanonicalEmitter.pascal(key)

  /** The Go type and runtime codec of each native integer range (as [[RustEmitter.native]] names them). */
  private val NativeCodecs: Map[String, (String, String)] =
    Map("u32" -> ("uint32", "Uint32In"), "u64" -> ("uint64", "Uint64In"), "i32" -> ("int32", "Int32In"), "i64" -> ("int64", "Int64In"))

  /** The methods every error kind has, which a field cannot be named like. */
  private val ReservedErrorFields = Set("Error", "Kind", "Unwrap")

  /** The Go name of an error's `error` field, the error it wraps. */
  private val WrappedError = "Err"

  /** Names package core declares by hand, which a core module must not declare too. */
  private val CoreNames: Set[String] = Set(
    "Array",
    "BigBound",
    "Bool",
    "Build",
    "BuildStep",
    "BytesTag",
    "Bytes",
    "CBORTag",
    "ContentHash",
    "Decimal",
    "Decode",
    "DecodeFailure",
    "DecodeWithLimit",
    "DecodeWithin",
    "Digest",
    "DigestFromBytes",
    "DigestFromHex",
    "DivisionByZero",
    "DomainTag",
    "Encode",
    "EncodeWithin",
    "EncodedLengthWithin",
    "Entry",
    "ExpandMessageXmd",
    "Failure",
    "Field",
    "Fields",
    "FromCBOR",
    "FromHex",
    "FromJSON",
    "HashAlgorithm",
    "HashAlgorithms",
    "HashCommitment",
    "HashParts",
    "Int",
    "Int32In",
    "Int64",
    "Int64In",
    "IntFromBig",
    "IntIn",
    "IntOf",
    "IntValue",
    "Integer",
    "Keys",
    "Kind",
    "Map",
    "Matching",
    "MaxCopies",
    "MaxDepth",
    "MaxDigits",
    "MaxEncodedBytes",
    "MaxIntegerBits",
    "MaxPreviewCodePoints",
    "MaxRationalBits",
    "MaxRenderBytes",
    "MaxWeight",
    "Encoding",
    "EncodingWithin",
    "DefaultEncodingWeight",
    "ToCBORWithin",
    "NewDomainTag",
    "NewTaggedHash",
    "None",
    "Null",
    "Number",
    "Opt",
    "Optional",
    "Pointer",
    "Preview",
    "PreviewOf",
    "Rational",
    "RationalOf",
    "Req",
    "Required",
    "Some",
    "StagedBytes",
    "StartHash",
    "TaggedHash",
    "TaggedHashOf",
    "Text",
    "TextMap",
    "TextMapCodec",
    "TextEntry",
    "NewTextMap",
    "TextMapFrom",
    "TextPattern",
    "ToCBOR",
    "ToHex",
    "TooLarge",
    "Uint32In",
    "Uint64",
    "Uint64In",
    "UintOf",
    "Value",
    "ValueCommitment",
    "ValueCommitmentWithin",
    "VecOf",
    "WriteCommitment",
    "WriteValue"
  )

  /** Package names an import would shadow something with: Go's keywords and predeclared names, and the code's own. */
  private val ReservedPackages: Set[String] = Set(
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "map",
    "package",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var",
    "any",
    "bool",
    "byte",
    "comparable",
    "complex64",
    "complex128",
    "error",
    "float32",
    "float64",
    "int",
    "int8",
    "int16",
    "int32",
    "int64",
    "rune",
    "string",
    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "uintptr",
    "true",
    "false",
    "iota",
    "nil",
    "append",
    "cap",
    "clear",
    "close",
    "complex",
    "copy",
    "delete",
    "imag",
    "len",
    "make",
    "max",
    "min",
    "new",
    "panic",
    "print",
    "println",
    "real",
    "recover",
    "alt",
    "e",
    "f",
    "i",
    "rec",
    "v",
    "x",
    "err",
    "inner",
    "depth",
    "at",
    "core",
    "fmt",
    "generated"
  )

  def patternName(pattern: String): String = pattern match {
    case "([0-9a-f]{2})*"   => "PatternHex"
    case "0x([0-9a-f]{2})*" => "PatternHex0x"
    case "0|[1-9][0-9]*"    => "PatternDecimal"
    case _                  => "PatternStateKey"
  }

  /** A Go interpreted string literal. */
  def string(s: String): String =
    "\"" + s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case c if c < ' ' => f"\\x${c.toInt}%02x"
      case c            => c.toString
    } + "\""

  /** A big-integer bound: its sign and big-endian magnitude, as a string of byte escapes. */
  def bigBound(n: BigInt, tpe: String): String = {
    val magnitude = n.abs.toByteArray.dropWhile(_ == 0).map(b => f"\\x${b & 0xff}%02x").mkString
    s"""$tpe{Negative: ${n.signum < 0}, Magnitude: "$magnitude"}"""
  }

  def bound(hi: Int): String = if (hi == Int.MaxValue) "-1" else hi.toString
}
