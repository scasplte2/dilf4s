package xyz.kd5ujc.modelsgen

import cats.syntax.all._

import xyz.kd5ujc.modelsgen.Ast.Occ

/**
 * The **dilf4s canonical profile** of CDDL as a language-neutral model: every emitter (Scala, TypeScript, Rust) reads
 * its rules through here, so all of them accept exactly the same schemas and check exactly the same shapes.
 *
 *   - a map of text-literal keys (each required or `?` optional) is a [[ModelRule.Record]]
 *   - a labelled integer enumeration `&(a: 1, b: 2)` is a [[ModelRule.Enumeration]]
 *   - a choice of map rules and text literals is a [[ModelRule.Union]], decoded by key set; it must be unambiguous (some
 *     required key of one alternative is outside the other, for every pair), which is checked here. A choice may also
 *     name a choice of text literals (an error family's kinds), whose texts it then takes as its own
 *   - a choice of inline maps, each led by a distinct `"kind"` text literal, is a [[ModelRule.ErrorFamily]]: an error
 *     type, not a wire format, with its kinds in the rule named like it with `-kind`
 *   - a rule whose whole body is an unsigned integer literal (`max-depth = 256`) is a [[ModelRule.Constant]]: a named
 *     value, not a type. Range bounds and sizes may name it (`0..max-depth`); a field or element may not
 *   - anything else in the profile (a primitive, a range, a sized string, a homogeneous array, a text-keyed map, a
 *     reference) is a [[ModelRule.Alias]]
 *
 * Everything outside the profile (integer map keys, tags, floats, groups, inline choices, `.regexp` patterns other
 * than [[Profile.Patterns]]) is a typed [[GenError]].
 */
enum FieldType {

  /**
   * CDDL `uint`, `int` and `nint` follow RFC 8610: major type 0 (0..2^64-1), either major type (-2^64..2^64-1), and
   * major type 1 (-2^64..-1). A wider integer must be an explicit range.
   */
  case Uint, Int, Nint
  case Bytes, Text, Bool, AnyValue

  /** An integer in `lo..hi`, inclusive. */
  case IntRange(lo: BigInt, hi: BigInt)

  /** A byte string of `lo` to `hi` bytes; `hi == Int.MaxValue` is unbounded. */
  case SizedBytes(lo: scala.Int, hi: scala.Int)

  /** Text of `lo` to `hi` UTF-8 bytes; `hi == Int.MaxValue` is unbounded. */
  case SizedText(lo: scala.Int, hi: scala.Int)

  /** Text matching the whole of one of [[Profile.Patterns]]. */
  case Matching(pattern: String)
  case LitInt(value: scala.Int)
  case LitText(value: String)
  case Ref(target: QName)
  case TextMap(value: FieldType)
  case Vec(element: FieldType, min: scala.Int, max: Option[scala.Int])
}

/** A record field: its wire key, whether it may be absent, and its type. */
final case class ProfileField(key: String, optional: Boolean, tpe: FieldType)

/** A record alternative of a union, with the keys it requires and the keys it allows. */
final case class UnionAlternative(rule: QName, required: Set[String], allowed: Set[String])

/** A choice of text literals that a union names, whose texts are among the union's. */
final case class TextChoice(rule: QName, texts: Vector[String])

/** One kind of an error family, with its fields in schema order. */
final case class ErrorCase(kind: String, fields: Vector[ProfileField])

enum ModelRule {
  case Record(fields: Vector[ProfileField])
  case Enumeration(cases: Vector[(String, BigInt)])
  case Union(alternatives: Vector[UnionAlternative], texts: Vector[String], choices: Vector[TextChoice])

  /**
   * An error family: one case per kind, its kinds the text choice `kinds` lists in the same order. A type the SDKs
   * construct and match, never encoded: no codec is generated, and no wire type may hold one.
   */
  case ErrorFamily(kinds: QName, cases: Vector[ErrorCase])
  case Alias(tpe: FieldType)

  /** A named unsigned integer (a spec limit), emitted as a constant in every language. */
  case Constant(value: BigInt)
}

final class Profile(val schema: Schema) {

  private def outside(rule: QName, what: String): GenError = GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile(what))

  /** A type in field, element or alias position. */
  def fieldType(rule: QName, t: RT): Either[GenError, FieldType] = t match {
    case RT.Prim(PrimKind.Uint)  => FieldType.Uint.asRight
    case RT.Prim(PrimKind.Int)   => FieldType.Int.asRight
    case RT.Prim(PrimKind.Nint)  => FieldType.Nint.asRight
    case RT.Prim(PrimKind.Bytes) => FieldType.Bytes.asRight
    case RT.Prim(PrimKind.Text)  => FieldType.Text.asRight
    case RT.Prim(PrimKind.Bool)  => FieldType.Bool.asRight
    case RT.Prim(PrimKind.Any)   => FieldType.AnyValue.asRight
    case RT.IntRange(lo, hi)     => FieldType.IntRange(lo, hi).asRight
    case RT.SizedBytes(lo, hi)   => FieldType.SizedBytes(lo, hi).asRight
    case RT.SizedText(lo, hi)    => FieldType.SizedText(lo, hi).asRight
    case RT.Ref(q) =>
      schema.rule(q).flatMap {
        case RuleDef(_, RuleBody.Type(RT.LitInt(_))) =>
          outside(rule, s"constant ${q.name} in field position (a constant is a value: name it in a range or size bound)").asLeft
        case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(_))) =>
          outside(rule, s"error family ${q.name} in field position (an error is not a wire type: name its kinds)").asLeft
        case _ => FieldType.Ref(q).asRight
      }
    case RT.LitInt(v) if v >= Int.MinValue && v <= Int.MaxValue => FieldType.LitInt(v.toInt).asRight
    case RT.LitText(v)                                          => FieldType.LitText(v).asRight
    case RT.MatchingText(pattern) =>
      Either.cond(
        Profile.Patterns.contains(pattern),
        FieldType.Matching(pattern),
        outside(
          rule,
          s".regexp ${CanonicalEmitter.literal(pattern)} (supported: ${Profile.Patterns.map(CanonicalEmitter.literal).mkString(", ")})"
        )
      )
    case Profile.TextMapOf(value) => fieldType(rule, value).map(FieldType.TextMap(_))
    case RT.Arr(Vector(REntry(occ, _, None, element))) if occ != Occ.Once && occ != Occ.Optional =>
      fieldType(rule, element).map(FieldType.Vec(_, occ.min, occ.max))
    case other => outside(rule, s"${SyntaxSummary(other)} in field position (name it as its own rule)").asLeft
  }

  /**
   * The fields of a record. Two keys that any emitter would give the same name (camelCase in Scala and TypeScript,
   * snake_case in Rust) are a [[GenError.NameClash]] for every language, so all of them accept the same schemas.
   */
  def fields(rule: QName, entries: Vector[REntry]): Either[GenError, Vector[ProfileField]] =
    entries.traverse {
      case REntry(occ, _, Some(RKey.TextKey(key)), value) if occ == Occ.Once || occ == Occ.Optional =>
        fieldType(rule, value).map(ProfileField(key, occ == Occ.Optional, _))
      case REntry(_, _, key, _) =>
        outside(rule, s"map entry with key ${key.fold("<none>")(_.toString)} (text-literal keys, once or optional)").asLeft
    }.flatMap { fs =>
      val clash = Profile.Namings.collectFirst(Function.unlift { (_, name) =>
        fs.groupBy(f => name(f.key)).collectFirst { case (n, xs) if xs.size > 1 => n -> xs.map(_.key) }
      })
      val unnamed =
        fs.find(f => Profile.Namings.exists((n, name) => n != "wire" && !Profile.isIdentifier(name(f.key).stripPrefix("r#"))))
      // a TypeScript record reads its fields as properties, and every object inherits these
      val inherited =
        fs.find(f => Profile.ObjectMembers(Profile.Namings.collectFirst { case ("camelCase", name) => name(f.key) }.getOrElse(f.key)))
      unnamed
        .map(f => outside(rule, s"key ${CanonicalEmitter.literal(f.key)} has no identifier form"))
        .orElse(inherited.map(f => outside(rule, s"key ${CanonicalEmitter.literal(f.key)} names a member every object inherits")))
        .orElse(clash.map { case (n, keys) => GenError.NameClash(rule.toString, n, keys) })
        .toLeft(fs)
    }

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): Either[GenError, ModelRule] = {
    val names = cases.map { case (l, _) => CanonicalEmitter.pascal(l) }
    val unnamed = cases.collect { case (l, _) if !Profile.isIdentifier(CanonicalEmitter.pascal(l)) => l }
    val dupNames = names.diff(names.distinct)
    val dupValues = cases.map(_._2).diff(cases.map(_._2).distinct)
    val outOfRange = cases.collect { case (l, v) if v < Profile.IntMin || v > Profile.IntMax => l }
    if (dupNames.nonEmpty || dupValues.nonEmpty) outside(rule, "enumeration with repeated labels or values").asLeft
    else if (unnamed.nonEmpty) outside(rule, s"enumeration labels with no identifier form: ${unnamed.mkString(", ")}").asLeft
    else if (outOfRange.nonEmpty) outside(rule, s"enumeration values beyond CDDL int: ${outOfRange.mkString(", ")}").asLeft
    else ModelRule.Enumeration(cases).asRight
  }

  /** Required and permitted keys of a map rule (a choice alternative). */
  private def keySets(alt: QName): Either[GenError, UnionAlternative] =
    schema.rule(alt).flatMap {
      case RuleDef(_, RuleBody.Type(RT.MapT(es))) =>
        fields(alt, es).map(fs => UnionAlternative(alt, fs.filterNot(_.optional).map(_.key).toSet, fs.map(_.key).toSet))
      case _ => outside(alt, "choice alternative that is not a map rule").asLeft
    }

  /** The texts of a choice of text literals only, which a union may name as one of its alternatives. */
  private def textChoice(alt: QName): Either[GenError, Option[TextChoice]] =
    schema.rule(alt).map {
      case RuleDef(_, RuleBody.Type(RT.Choice(as))) =>
        as.traverse {
          case RT.LitText(t) => Some(t)
          case _             => None
        }.map(TextChoice(alt, _))
      case _ => None
    }

  private def union(rule: QName, refs: Vector[QName], texts: Vector[String]): Either[GenError, ModelRule] =
    refs.traverse(a => textChoice(a).map(_.toRight(a))).flatMap { classified =>
      val choices = classified.collect { case Right(c) => c }
      val all = texts ++ choices.flatMap(_.texts)
      val repeated = all.diff(all.distinct).distinct
      if (repeated.nonEmpty) outside(rule, s"choice with repeated texts: ${repeated.map(CanonicalEmitter.literal).mkString(", ")}").asLeft
      else records(rule, classified.collect { case Left(a) => a }, texts, choices)
    }

  private def records(rule: QName, alts: Vector[QName], texts: Vector[String], choices: Vector[TextChoice]): Either[GenError, ModelRule] =
    alts.traverse(keySets).flatMap { ks =>
      // ambiguous by wire keys (decoding), or by field names (TypeScript encodes a union by its present fields)
      val ambiguous = (for {
        (_, name) <- Profile.Namings
        a         <- ks
        b         <- ks
        if CanonicalEmitter.pascal(a.rule.name) < CanonicalEmitter.pascal(b.rule.name) &&
        (a.required.map(name) -- b.allowed.map(name)).isEmpty && (b.required.map(name) -- a.allowed.map(name)).isEmpty
      } yield s"${a.rule.name}/${b.rule.name}").distinct
      if (ambiguous.nonEmpty) GenError.Unsupported(rule.toString, UnsupportedFeature.AmbiguousAlternatives(ambiguous)).asLeft
      else ModelRule.Union(ks, texts, choices).asRight
    }

  /**
   * An error family's field holds text, an integer, a boolean or a kind (an enumeration or a choice of texts), through
   * aliases: never bytes, a collection or a record, so what an error holds stays bounded by what its kind admits.
   */
  private def errorField(rule: QName, t: FieldType, seen: Set[QName]): Either[GenError, Unit] = t match {
    case FieldType.Uint | FieldType.Int | FieldType.Nint | FieldType.Text | FieldType.Bool => ().asRight
    case FieldType.IntRange(_, _) | FieldType.SizedText(_, _) | FieldType.Matching(_)      => ().asRight
    case FieldType.Ref(q) if seen(q) => outside(rule, s"alias cycle through ${q.name}").asLeft
    case FieldType.Ref(q) =>
      this.rule(q).flatMap {
        case ModelRule.Alias(target)                => errorField(rule, target, seen + q)
        case ModelRule.Enumeration(_)               => ().asRight
        case ModelRule.Union(Vector(), _, Vector()) => ().asRight
        case _ => outside(rule, s"error field of type ${q.name} (text, an integer, a boolean or a kind)").asLeft
      }
    case other => outside(rule, s"error field of type $other (text, an integer, a boolean or a kind)").asLeft
  }

  private def errorFamily(q: QName, alternatives: Vector[Vector[REntry]]): Either[GenError, ModelRule] = {
    val kindRule = QName(q.module, s"${q.name}-kind")
    alternatives.traverse { es =>
      es.collect { case REntry(occ, _, Some(RKey.TextKey("kind")), RT.LitText(k)) if occ == Occ.Once => k } match {
        case Vector(k) if k.matches("[A-Z][A-Za-z0-9]*") =>
          fields(q, es.filterNot(_.key.contains(RKey.TextKey("kind"))))
            .flatMap(fs => fs.traverse_(f => errorField(q, f.tpe, Set.empty)).as(ErrorCase(k, fs)))
        case Vector(k) => outside(q, s"error kind ${CanonicalEmitter.literal(k)} (a kind is a PascalCase identifier)").asLeft
        case _         => outside(q, "error family alternative without one required \"kind\" text literal").asLeft
      }
    }.flatMap { cases =>
      val kinds = cases.map(_.kind)
      val listed = textChoice(kindRule).toOption.flatten.map(_.texts)
      if (kinds.distinct.size != kinds.size)
        outside(q, s"error family with repeated kinds: ${kinds.diff(kinds.distinct).mkString(", ")}").asLeft
      else if (!listed.contains(kinds))
        outside(
          q,
          s"error family whose kinds are not listed, in order, by ${kindRule.name} = ${kinds.map(CanonicalEmitter.literal).mkString(" / ")}"
        ).asLeft
      else ModelRule.ErrorFamily(kindRule, cases).asRight
    }
  }

  /** The model of one rule, or the first profile violation. */
  def rule(q: QName): Either[GenError, ModelRule] =
    schema.rule(q).flatMap {
      case RuleDef(_, RuleBody.Type(t @ Profile.TextMapOf(_))) => fieldType(q, t).map(ModelRule.Alias(_))
      case RuleDef(_, RuleBody.Type(RT.MapT(es)))              => fields(q, es).map(ModelRule.Record(_))
      case RuleDef(_, RuleBody.Type(RT.IntEnum(cs)))           => enumeration(q, cs)
      case RuleDef(_, RuleBody.Type(RT.LitInt(v))) =>
        Either.cond(v >= 0 && v <= Profile.UintMax, ModelRule.Constant(v), outside(q, s"constant $v (a constant is a uint, 0..2^64-1)"))
      case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(alternatives))) => errorFamily(q, alternatives)
      case RuleDef(_, RuleBody.Type(RT.Choice(alts))) =>
        alts.traverse {
          case RT.Ref(a)     => Left(a).some
          case RT.LitText(t) => Right(t).some
          case _             => none[Either[QName, String]]
        }
          .toRight(outside(q, "inline choice (a choice is of named map rules and text literals; use an enumeration for integers)"))
          .flatMap(as => union(q, as.collect { case Left(a) => a }, as.collect { case Right(t) => t }))
      case RuleDef(_, RuleBody.Type(t))      => fieldType(q, t).map(ModelRule.Alias(_))
      case RuleDef(_, RuleBody.GroupBody(_)) => outside(q, "group rule").asLeft
    }

  /** Every rule of a module in name order, with its model. */
  def module(name: String): Either[GenError, Vector[(QName, ModelRule)]] =
    schema.modules
      .get(name)
      .toRight(GenError.UnknownModule(name, "configuration"))
      .flatMap(m => m.rules.keys.toVector.sorted.map(QName(name, _)).traverse(q => rule(q).map(q -> _)))
}

object Profile {

  /**
   * The `.regexp` patterns of the profile. Each SDK implements these as named validators instead of carrying a
   * regular-expression engine; adding one means adding it to every SDK core.
   */
  /** Whether `s` is an identifier in every target language (ASCII letters, digits and `_`, not starting with a digit). */
  def isIdentifier(s: String): Boolean = s != "_" && s.matches("[A-Za-z_][A-Za-z0-9_]*")

  /** Members of `Object.prototype`: a field with one of these names would be "present" on every object. */
  val ObjectMembers: Set[String] = Set(
    "constructor",
    "hasOwnProperty",
    "isPrototypeOf",
    "propertyIsEnumerable",
    "toLocaleString",
    "toString",
    "valueOf",
    "__proto__",
    "__defineGetter__",
    "__defineSetter__",
    "__lookupGetter__",
    "__lookupSetter__"
  )

  /** CDDL `int` (RFC 8610: major types 0 and 1). */
  val IntMin: BigInt = -(BigInt(1) << 64)
  val IntMax: BigInt = (BigInt(1) << 64) - 1

  /** CDDL `uint` (major type 0): the range of a constant. */
  val UintMax: BigInt = IntMax

  val Patterns: Vector[String] =
    Vector("([0-9a-f]{2})*", "0x([0-9a-f]{2})*", "0|[1-9][0-9]*", "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*")

  /** How each emitter names a wire key: the key itself, camelCase (Scala, TypeScript) and snake_case (Rust). */
  val Namings: Vector[(String, String => String)] = Vector(
    "wire"       -> identity,
    "camelCase"  -> (k => CanonicalEmitter.fieldName(k).stripPrefix("`").stripSuffix("`")),
    "snake_case" -> RustEmitter.fieldName
  )

  /** A choice of inline maps, its alternatives `//` in one map rule: an error family's shape (`{ "kind": "A", ... // "kind": "B" }`). */
  object ErrorFamilyOf {
    def unapply(t: RT): Option[Vector[Vector[REntry]]] = t match {
      case RT.Choice(alts) if alts.size > 1 =>
        alts.traverse {
          case RT.MapT(es) if !TextMapOf.unapply(RT.MapT(es)).isDefined => Some(es)
          case _                                                        => None
        }
      case _ => None
    }
  }

  /** `{ * text => T }`: a map with arbitrary text keys. */
  object TextMapOf {
    def unapply(t: RT): Option[RT] = t match {
      case RT.MapT(Vector(REntry(occ, _, Some(RKey.OfType(RT.Prim(PrimKind.Text), _)), value))) if occ.min == 0 && occ.max.isEmpty =>
        Some(value)
      case _ => None
    }
  }
}
