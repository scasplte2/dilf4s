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
 *     type, not a wire format, with its kinds in the rule named like it with `-kind`. An error's field may hold
 *     another error family, never one that holds it back
 *   - a choice of rules that are each one text literal is a [[ModelRule.NamedTexts]]: a closed type whose members are
 *     named by their rules ([[ModelRule.TextMember]]), so a text need not be an identifier
 *   - the `;` comment directly above a rule, an error kind or a field is its documentation ([[CddlParser.docs]])
 *   - a rule whose whole body is an unsigned integer literal (`max-depth = 256`) is a [[ModelRule.Constant]]: a named
 *     value, not a type. Range bounds and sizes may name it (`0..max-depth`); a field or element may not
 *   - a rule named `preview` is [[ModelRule.PreviewType]]: each SDK's `Preview` (caller text as an error holds it,
 *     conformance.md section 8), so its body must be the preview's normative fields, `{ "head": text, "length": uint }`.
 *     Nothing is emitted for it; an error's field may name it, a wire type may not
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

  /** The SDK's `Preview` of caller text: an error's field only, never part of a wire type. */
  case Preview
}

/** A record field: its wire key, whether it may be absent, its type, and its documentation (the CDDL comment above it). */
final case class ProfileField(key: String, optional: Boolean, tpe: FieldType, doc: Vector[String] = Vector.empty)

/** A record alternative of a union, with the keys it requires and the keys it allows. */
final case class UnionAlternative(rule: QName, required: Set[String], allowed: Set[String])

/**
 * A choice of text literals that a union names, whose texts are among the union's: each text with its member's name
 * in the choice's own type (the text in PascalCase, or for a [[ModelRule.NamedTexts]] its rule's name), and whether
 * the choice is a [[ModelRule.NamedTexts]], whose members are not the texts themselves in Scala.
 */
final case class TextChoice(rule: QName, texts: Vector[String], names: Vector[String], named: Boolean = false)

/** One kind of an error family, with its fields in schema order and its documentation (the CDDL comment above it). */
final case class ErrorCase(kind: String, fields: Vector[ProfileField], doc: Vector[String] = Vector.empty)

/** One member of a [[ModelRule.NamedTexts]]: its identifier (from its rule's name), its text, and its documentation. */
final case class NamedText(name: String, text: String, doc: Vector[String])

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

  /** The rule `preview`: the SDK's own `Preview` type, which an error's field may hold. Nothing is emitted for it. */
  case PreviewType

  /**
   * A choice of rules that are each one text literal (`op = op-not / op-eq`, `op-not = "!"`): a closed type whose
   * members are named by their rules, not their texts, so a text need not be an identifier. Scala an enum carrying
   * `wire`, Rust an enum with `wire()`, `from_wire` and `ALL`, TypeScript a union of the texts.
   */
  case NamedTexts(members: Vector[NamedText])

  /** A rule that is one text literal: a member of a [[NamedTexts]], or a literal a field names. Nothing is emitted. */
  case TextMember(text: String)
}

final class Profile(val schema: Schema) {

  private def outside(rule: QName, what: ProfileRefusal): GenError =
    GenError.Unsupported(rule.toString, UnsupportedFeature.OutsideProfile(what))

  /**
   * A type in field, element or alias position. Only an error family's field (`inError`) may name another error
   * family, which nests one error in another (a reason with fields of its own).
   */
  def fieldType(rule: QName, t: RT, inError: Boolean = false): Either[GenError, FieldType] = t match {
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
          outside(rule, ProfileRefusal.ConstantInField(q)).asLeft
        case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(_))) if !inError =>
          outside(rule, ProfileRefusal.ErrorFamilyInField(q)).asLeft
        // a rule that is one text literal stands for its text
        case RuleDef(_, RuleBody.Type(RT.LitText(v))) => FieldType.LitText(v).asRight
        case _ if q.name == Profile.PreviewRule       => FieldType.Preview.asRight
        case _                                        => FieldType.Ref(q).asRight
      }
    case RT.LitInt(v) if v >= Int.MinValue && v <= Int.MaxValue => FieldType.LitInt(v.toInt).asRight
    case RT.LitText(v)                                          => FieldType.LitText(v).asRight
    case RT.MatchingText(pattern) =>
      Either.cond(
        Profile.Patterns.contains(pattern),
        FieldType.Matching(pattern),
        outside(
          rule,
          ProfileRefusal.UnsupportedPattern(pattern)
        )
      )
    case Profile.TextMapOf(value) => fieldType(rule, value).map(FieldType.TextMap(_))
    case RT.Arr(Vector(REntry(occ, _, None, element, _))) if occ != Occ.Once && occ != Occ.Optional =>
      fieldType(rule, element).map(FieldType.Vec(_, occ.min, occ.max))
    case other => outside(rule, ProfileRefusal.FieldTypeInPosition(other)).asLeft
  }

  /**
   * The fields of a record. Two keys that any emitter would give the same name (camelCase in Scala and TypeScript,
   * snake_case in Rust) are a [[GenError.NameClash]] for every language, so all of them accept the same schemas.
   */
  def fields(rule: QName, entries: Vector[REntry], inError: Boolean = false): Either[GenError, Vector[ProfileField]] =
    entries.traverse {
      case e @ REntry(occ, _, Some(RKey.TextKey(key)), value, _) if occ == Occ.Once || occ == Occ.Optional =>
        fieldType(rule, value, inError).map(ProfileField(key, occ == Occ.Optional, _, e.doc))
      case REntry(occ, _, key, _, _) =>
        outside(rule, ProfileRefusal.MapEntry(occ, key)).asLeft
    }.flatMap { fs =>
      val clash = Profile.Namings.collectFirst(Function.unlift { name =>
        fs.groupBy(f => name(f.key)).collectFirst { case (n, xs) if xs.size > 1 => n -> xs.map(_.key) }
      })
      val unnamed =
        fs.find(f => Profile.Namings.exists(n => n != Profile.Naming.Wire && !Profile.isIdentifier(n(f.key).stripPrefix("r#"))))
      // a TypeScript record reads its fields as properties, and every object inherits these
      val inherited =
        fs.find(f => Profile.ObjectMembers(Profile.Naming.Camel(f.key)))
      unnamed
        .map(f => outside(rule, ProfileRefusal.InvalidKeyIdentifier(f.key)))
        .orElse(inherited.map(f => outside(rule, ProfileRefusal.InheritedMember(f.key))))
        .orElse(clash.map { case (n, keys) => GenError.NameClash(rule.toString, n, keys) })
        .toLeft(fs)
    }

  /** A wire type never holds a preview, at any depth: a preview is built from caller text, never decoded. */
  private def wire(rule: QName, t: FieldType): Either[GenError, Unit] = t match {
    case FieldType.Preview            => outside(rule, ProfileRefusal.PreviewInWire).asLeft
    case FieldType.TextMap(value)     => wire(rule, value)
    case FieldType.Vec(element, _, _) => wire(rule, element)
    case _                            => ().asRight
  }

  private def enumeration(rule: QName, cases: Vector[(String, BigInt)]): Either[GenError, ModelRule] = {
    val names = cases.map { case (l, _) => CanonicalEmitter.pascal(l) }
    val unnamed = cases.collect { case (l, _) if !Profile.isIdentifier(CanonicalEmitter.pascal(l)) => l }
    val dupNames = names.diff(names.distinct)
    val dupValues = cases.map(_._2).diff(cases.map(_._2).distinct)
    val outOfRange = cases.collect { case (l, v) if v < Profile.IntMin || v > Profile.IntMax => l }
    if (dupNames.nonEmpty || dupValues.nonEmpty) outside(rule, ProfileRefusal.RepeatedEnumeration(dupNames, dupValues)).asLeft
    else if (unnamed.nonEmpty) outside(rule, ProfileRefusal.InvalidEnumerationLabels(unnamed)).asLeft
    else if (outOfRange.nonEmpty) outside(rule, ProfileRefusal.EnumerationOutOfRange(outOfRange)).asLeft
    else ModelRule.Enumeration(cases).asRight
  }

  /** Required and permitted keys of a map rule (a choice alternative). */
  private def keySets(alt: QName): Either[GenError, UnionAlternative] =
    schema.rule(alt).flatMap {
      case _ if alt.name == Profile.PreviewRule =>
        outside(alt, ProfileRefusal.PreviewAlternative).asLeft
      case RuleDef(_, RuleBody.Type(RT.MapT(es))) =>
        fields(alt, es).map(fs => UnionAlternative(alt, fs.filterNot(_.optional).map(_.key).toSet, fs.map(_.key).toSet))
      case _ => outside(alt, ProfileRefusal.NonMapAlternative).asLeft
    }

  /**
   * The texts of a choice of text literals only, or of rules that are each one ([[ModelRule.NamedTexts]]), which a union
   * may name as one of its alternatives.
   */
  private def textChoice(alt: QName): Either[GenError, Option[TextChoice]] =
    schema.rule(alt).flatMap {
      case RuleDef(_, RuleBody.Type(RT.Choice(as))) =>
        as.traverse {
          case RT.LitText(t) => Some(t)
          case _             => None
        }.map(ts => TextChoice(alt, ts, ts.map(CanonicalEmitter.pascal)))
          .fold(namedTexts(alt, as).map(_.map(ms => TextChoice(alt, ms.map(_.text), ms.map(_.name), named = true))))(_.some.asRight)
      case _ => none[TextChoice].asRight
    }

  /** The text of a rule that is one text literal, with its documentation. */
  private def textMember(q: QName): Either[GenError, Option[String]] =
    schema.rule(q).map {
      case RuleDef(_, RuleBody.Type(RT.LitText(t))) => Some(t)
      case _                                        => None
    }

  /**
   * A choice whose alternatives are all rules of one text literal: their names, the prefix they share up to a `-`
   * (`op-` of `op-not`) removed, become the members' identifiers (`Not`; words are split at `-` only, so `op-sha3_256`
   * is `Sha3_256`). `None` when no alternative is such a rule; a choice mixing them with anything else is refused.
   */
  private def namedTexts(q: QName, alts: Vector[RT]): Either[GenError, Option[Vector[NamedText]]] =
    alts.traverse {
      case RT.Ref(a) => textMember(a).map(_.map(a -> _))
      case _         => none[(QName, String)].asRight
    }.flatMap { found =>
      if (found.forall(_.isEmpty)) none[Vector[NamedText]].asRight
      else
        found.sequence.toRight(outside(q, ProfileRefusal.MixedTextAlternatives)).flatMap { members =>
          val names = members.map(_._1.name)
          val shared = names.reduceLeftOption((a, b) => a.zip(b).takeWhile((x, y) => x == y).map(_._1).mkString).getOrElse("")
          val prefix = shared.take(shared.lastIndexOf('-') + 1)
          val named = members.map { case (r, t) => NamedText(Profile.memberName(r.name.drop(prefix.length)), t, schema.doc(r)) }
          val texts = named.map(_.text)
          val ids = named.map(_.name)
          if (texts.distinct.size != texts.size)
            outside(q, ProfileRefusal.RepeatedTexts(texts.diff(texts.distinct))).asLeft
          else
            ids
              .diff(ids.distinct)
              .headOption
              .map(id => GenError.NameClash(q.toString, id, names))
              .orElse(named.find(m => !Profile.isIdentifier(m.name)).map(m => outside(q, ProfileRefusal.InvalidMemberIdentifier(m.name))))
              .toLeft(named.some)
        }
    }

  private def union(rule: QName, refs: Vector[QName], texts: Vector[String]): Either[GenError, ModelRule] =
    refs.traverse(a => textChoice(a).map(_.toRight(a))).flatMap { classified =>
      val choices = classified.collect { case Right(c) => c }
      val all = texts ++ choices.flatMap(_.texts)
      val repeated = all.diff(all.distinct).distinct
      if (repeated.nonEmpty) outside(rule, ProfileRefusal.RepeatedTexts(repeated)).asLeft
      else records(rule, classified.collect { case Left(a) => a }, texts, choices)
    }

  private def records(rule: QName, alts: Vector[QName], texts: Vector[String], choices: Vector[TextChoice]): Either[GenError, ModelRule] =
    alts.traverse(keySets).flatMap { ks =>
      // ambiguous by wire keys (decoding), or by field names (TypeScript encodes a union by its present fields)
      val ambiguous = (for {
        name <- Profile.Namings
        a    <- ks
        b    <- ks
        if CanonicalEmitter.pascal(a.rule.name) < CanonicalEmitter.pascal(b.rule.name) &&
        (a.required.map(name.apply) -- b.allowed.map(name.apply)).isEmpty && (b.required.map(name.apply) -- a.allowed
          .map(name.apply)).isEmpty
      } yield (a.rule, b.rule)).distinct
      if (ambiguous.nonEmpty) GenError.Unsupported(rule.toString, UnsupportedFeature.AmbiguousAlternatives(ambiguous)).asLeft
      else ModelRule.Union(ks, texts, choices).asRight
    }

  /**
   * An error family's field holds text, an integer, a boolean, a kind (an enumeration or a choice of texts) or a
   * preview, through aliases: never bytes, a collection or a record, so what an error holds stays bounded by what its
   * kind admits.
   */
  private def errorField(rule: QName, t: FieldType, seen: Set[QName]): Either[GenError, Unit] = t match {
    case FieldType.Uint | FieldType.Int | FieldType.Nint | FieldType.Text | FieldType.Bool => ().asRight
    case FieldType.IntRange(_, _) | FieldType.SizedText(_, _) | FieldType.Matching(_)      => ().asRight
    case FieldType.Preview                                                                 => ().asRight
    case FieldType.Ref(q) if seen(q) => outside(rule, ProfileRefusal.AliasCycle(q)).asLeft
    case FieldType.Ref(q) =>
      schema.rule(q).flatMap {
        // another error family, nested: checked on its own, and never back to this one
        case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(_))) =>
          Either.cond(!nests(q, rule), (), outside(rule, ProfileRefusal.NestedError(q, rule)))
        case _ =>
          this.rule(q).flatMap {
            case ModelRule.Alias(target)         => errorField(rule, target, seen + q)
            case ModelRule.Enumeration(_)        => ().asRight
            case ModelRule.NamedTexts(_)         => ().asRight
            case ModelRule.Union(Vector(), _, _) => ().asRight
            case _                               => outside(rule, ProfileRefusal.ErrorField(FieldType.Ref(q))).asLeft
          }
      }
    case other => outside(rule, ProfileRefusal.ErrorField(other)).asLeft
  }

  /** The error families an error family's fields name directly. */
  private def nested(q: QName): Set[QName] =
    schema.rule(q).toOption.fold(Set.empty[QName]) {
      case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(alternatives))) =>
        alternatives.flatten.collect {
          case REntry(_, _, _, RT.Ref(r), _) if schema.rule(r).toOption.exists {
                case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(_))) => true
                case _                                                   => false
              } =>
            r
        }.toSet
      case _ => Set.empty
    }

  /** Whether family `from` reaches family `to` through nested fields (itself included). */
  private def nests(from: QName, to: QName): Boolean = {
    @scala.annotation.tailrec
    def go(frontier: List[QName], seen: Set[QName]): Boolean = frontier match {
      case Nil                  => false
      case q :: _ if q == to    => true
      case q :: rest if seen(q) => go(rest, seen)
      case q :: rest            => go(nested(q).toList.sortBy(_.toString) ++ rest, seen + q)
    }
    go(List(from), Set.empty)
  }

  private def errorFamily(q: QName, alternatives: Vector[Vector[REntry]]): Either[GenError, ModelRule] = {
    val kindRule = QName(q.module, s"${q.name}-kind")
    alternatives.traverse { es =>
      // every "kind" entry is the discriminator: a second one, or one that is not a literal, would be lost
      es.filter(_.key.contains(RKey.TextKey("kind"))) match {
        case Vector(e @ REntry(occ, _, _, RT.LitText(k), _)) if occ == Occ.Once && k.matches("[A-Z][A-Za-z0-9]*") =>
          fields(q, es.filterNot(_.key.contains(RKey.TextKey("kind"))), inError = true)
            .flatMap(fs => fs.traverse_(f => errorField(q, f.tpe, Set.empty)).as(ErrorCase(k, fs, e.doc)))
        case Vector(REntry(occ, _, _, RT.LitText(k), _)) if occ == Occ.Once =>
          outside(q, ProfileRefusal.InvalidErrorKind(k)).asLeft
        case _ => outside(q, ProfileRefusal.ErrorKindField(es)).asLeft
      }
    }.flatMap { cases =>
      val kinds = cases.map(_.kind)
      val listed = textChoice(kindRule).toOption.flatten.map(_.texts)
      if (kinds.distinct.size != kinds.size)
        outside(q, ProfileRefusal.RepeatedErrorKinds(kinds.diff(kinds.distinct))).asLeft
      else if (!listed.contains(kinds))
        outside(
          q,
          ProfileRefusal.ErrorKindOrder(kindRule, kinds, listed)
        ).asLeft
      else ModelRule.ErrorFamily(kindRule, cases).asRight
    }
  }

  /** The model of one rule, or the first profile violation. */
  def rule(q: QName): Either[GenError, ModelRule] =
    schema.rule(q).flatMap {
      case RuleDef(_, body) if q.name == Profile.PreviewRule =>
        val shape = outside(q, ProfileRefusal.PreviewShape(body))
        body match {
          case RuleBody.Type(RT.MapT(es)) =>
            fields(q, es)
              .flatMap(fs => Either.cond(fs.map(_.copy(doc = Vector.empty)) == Profile.PreviewFields, ModelRule.PreviewType, shape))
          case _ => shape.asLeft
        }
      case RuleDef(_, RuleBody.Type(t @ Profile.TextMapOf(_))) => fieldType(q, t).flatTap(wire(q, _)).map(ModelRule.Alias(_))
      case RuleDef(_, RuleBody.Type(RT.MapT(es)))    => fields(q, es).flatTap(_.traverse_(f => wire(q, f.tpe))).map(ModelRule.Record(_))
      case RuleDef(_, RuleBody.Type(RT.IntEnum(cs))) => enumeration(q, cs)
      case RuleDef(_, RuleBody.Type(RT.LitInt(v))) =>
        Either.cond(v >= 0 && v <= Profile.UintMax, ModelRule.Constant(v), outside(q, ProfileRefusal.ConstantOutOfRange(v)))
      case RuleDef(_, RuleBody.Type(Profile.ErrorFamilyOf(alternatives))) => errorFamily(q, alternatives)
      case RuleDef(_, RuleBody.Type(RT.LitText(t)))                       => ModelRule.TextMember(t).asRight
      case RuleDef(_, RuleBody.Type(RT.Choice(alts))) if namedTexts(q, alts).exists(_.isDefined) =>
        namedTexts(q, alts).map(ms => ModelRule.NamedTexts(ms.getOrElse(Vector.empty)))
      case RuleDef(_, RuleBody.Type(RT.Choice(alts))) =>
        namedTexts(q, alts) >> alts.traverse {
          case RT.Ref(a)     => Left(a).some
          case RT.LitText(t) => Right(t).some
          case _             => none[Either[QName, String]]
        }
          .toRight(outside(q, ProfileRefusal.InlineChoice(alts)))
          .flatMap(as => union(q, as.collect { case Left(a) => a }, as.collect { case Right(t) => t }))
      case RuleDef(_, RuleBody.Type(t))      => fieldType(q, t).flatTap(wire(q, _)).map(ModelRule.Alias(_))
      case RuleDef(_, RuleBody.GroupBody(_)) => outside(q, ProfileRefusal.GroupRule).asLeft
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
  /** A [[ModelRule.NamedTexts]] member's identifier from its rule's name (prefix removed): words split at `-` only. */
  def memberName(s: String): String = s.split("-").toList.filter(_.nonEmpty).map(w => w.take(1).toUpperCase + w.drop(1)).mkString

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

  /** The rule that is each SDK's `Preview`, and the fields its body must have: the preview's normative fields. */
  val PreviewRule: String = "preview"
  val PreviewFields: Vector[ProfileField] =
    Vector(ProfileField("head", false, FieldType.Text), ProfileField("length", false, FieldType.Uint))

  /** CDDL `int` (RFC 8610: major types 0 and 1). */
  val IntMin: BigInt = -(BigInt(1) << 64)
  val IntMax: BigInt = (BigInt(1) << 64) - 1

  /** CDDL `uint` (major type 0): the range of a constant. */
  val UintMax: BigInt = IntMax

  val Patterns: Vector[String] =
    Vector("([0-9a-f]{2})*", "0x([0-9a-f]{2})*", "0|[1-9][0-9]*", "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*")

  /**
   * How each emitter names a wire key: the key itself, camelCase (Scala, TypeScript), snake_case (Rust) and PascalCase
   * (Go, whose exported names `xY` and `XY` would both make `XY`).
   */
  enum Naming {
    case Wire, Camel, Snake, Pascal

    def apply(key: String): String = this match {
      case Wire   => key
      case Camel  => CanonicalEmitter.fieldName(key).stripPrefix("`").stripSuffix("`")
      case Snake  => RustEmitter.fieldName(key)
      case Pascal => GoEmitter.fieldName(key)
    }
  }

  val Namings: Vector[Naming] = Naming.values.toVector

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
      case RT.MapT(Vector(REntry(occ, _, Some(RKey.OfType(RT.Prim(PrimKind.Text), _)), value, _))) if occ.min == 0 && occ.max.isEmpty =>
        Some(value)
      case _ => None
    }
  }
}
