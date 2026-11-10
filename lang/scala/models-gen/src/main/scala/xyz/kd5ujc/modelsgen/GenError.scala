// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

import cats.Eval
import cats.data.EitherT

/** Closed generator failures. Causes and unsupported syntax survive until CLI presentation. */
enum GenError:
  case Parse(file: String, failure: CddlParser.ParseFailure)
  case Io(path: String, cause: java.io.IOException)
  case Defect(path: String, cause: Throwable)
  case UnknownModule(name: String, referencedFrom: String)
  case UnresolvedName(scope: String, name: String)
  case ArityMismatch(rule: String, expected: Int, actual: Int)
  case Unsupported(rule: String, feature: UnsupportedFeature)

  /** Two or more rules (or keys, or modules) that map to the same generated name; `rules` lists only those. */
  case NameClash(scope: String, name: String, rules: Vector[String])

  /** A generated name that `language` keeps for itself or for names the generated code refers to. */
  case ReservedName(scope: String, language: Language, name: String)
  case BadConfig(line: Int, problem: ConfigProblem)
  case CyclicExpansion(rule: QName)
  case SamplingCycle(rule: QName)
  case EmptyIntegerDomain(lower: BigInt, upper: BigInt)

  def message: String = this match
    case Parse(f, e)            => s"$f: ${e.message}"
    case Io(p, c)               => s"$p: $c"
    case Defect(p, c)           => s"unexpected generator failure at $p: $c"
    case UnknownModule(m, from) => s"unknown module `$m` (referenced from $from)"
    case UnresolvedName(s, n)   => s"unresolved name `$n` in $s"
    case ArityMismatch(r, e, a) => s"rule `$r` takes $e type arguments, got $a"
    case Unsupported(r, f)      => s"unsupported CDDL feature in `$r`: ${f.message}"
    case NameClash(s, n, rs) =>
      s"in $s the rules ${rs.mkString(", ")} all map to the generated name `$n`"
    case ReservedName(s, l, n) =>
      s"in $s the generated name `$n` is reserved in $l"
    case BadConfig(l, p)            => s"config line $l: ${p.message}"
    case CyclicExpansion(q)         => s"cyclic expansion of `$q`"
    case EmptyIntegerDomain(lo, hi) => s"cannot sample an empty integer domain ($lo .. $hi)"
    case SamplingCycle(q)           => s"recursive sampling of `$q` cannot terminate beyond the depth cutoff"

/** A language the generator emits. */
enum Language:
  case Scala, Rust, TypeScript, Go

enum ConfigProblem:
  case QualifiedName(value: String)
  case Directive(words: Vector[String])
  case MissingPackage
  case UnreachableRule(rule: QName)

  def message: String = this match
    case QualifiedName(s)   => s"expected module:rule, got `$s`"
    case Directive(words)   => s"unrecognised directive: ${words.mkString(" ")}"
    case MissingPackage     => "missing `package`"
    case UnreachableRule(q) => s"$q is configured but not reachable from any root"

enum UnsupportedFeature:
  case UnappliedGeneric, GroupAlternatives, GroupInTypePosition, Unwrap
  case NonLiteralBound(value: Ast.Ty)
  case FloatLiteral(value: String)
  case TagForm(major: Int, number: Option[BigInt])
  case Enumeration(value: Ast.Ty)
  case UnsignedSize(value: Ast.Ty)
  case SizeBound(value: Ast.Ty)
  case Control(operator: Ast.Control)
  case LiteralKey(value: Ast.Lit)
  case PairsWithoutTypeKey, RepeatedGroupReference
  case FixedArrayOccurrence(occurrence: Ast.Occ)
  case RepeatedLiteralMapKey, MapMinimumAboveOne, MapWithoutKey
  case TypeInPosition(value: RT)
  case PreserveNonStruct, InlineNonStruct
  case StandaloneGroup(rule: QName)
  case GroupAsType(rule: QName)
  case NotGroup(rule: QName)
  case FloatSampling
  case OutsideProfile(reason: ProfileRefusal)
  case AmbiguousAlternatives(pairs: Vector[(QName, QName)])

  def message: String = this match
    case UnappliedGeneric          => "generic rule used without type arguments"
    case GroupAlternatives         => "group rule with `//` alternatives"
    case GroupInTypePosition       => "group in type position"
    case Unwrap                    => "unwrap (~)"
    case NonLiteralBound(t)        => s"non-literal bound ${SyntaxSummary(t)}"
    case FloatLiteral(s)           => s"float literal $s"
    case TagForm(m, _)             => s"tag form #$m"
    case Enumeration(t)            => s"enumeration of ${SyntaxSummary(t)}"
    case UnsignedSize(_)           => "uint .size with a range or invalid bound"
    case SizeBound(_)              => "bad .size"
    case Control(op)               => s".${op.wire}"
    case LiteralKey(_)             => "non-integer, non-text literal key"
    case PairsWithoutTypeKey       => "pairs without a type key"
    case RepeatedGroupReference    => "repeated or optional group reference"
    case FixedArrayOccurrence(occ) => s"entry with occurrence $occ in a fixed array"
    case RepeatedLiteralMapKey     => "repeated literal map key"
    case MapMinimumAboveOne        => "map entry with a minimum above one"
    case MapWithoutKey             => "map entry without a key"
    case TypeInPosition(t)         => s"type ${SyntaxSummary(t)} in this position"
    case PreserveNonStruct         => "`preserve` is supported on array and map rules only"
    case InlineNonStruct           => "inlined rule is not a struct"
    case StandaloneGroup(q)        => s"group rule $q has no standalone encoding"
    case GroupAsType(q)            => s"group $q as a type"
    case NotGroup(q)               => s"$q is not a group"
    case FloatSampling             => "float"
    case OutsideProfile(w)         => s"outside the dilf4s canonical profile: ${w.message}"
    case AmbiguousAlternatives(ps) =>
      s"choice alternatives cannot be told apart by keys: ${ps.map { case (a, b) => s"$a/$b" }.mkString(", ")}"

/** Eval trampolines schema depth; EitherT propagates domain failures as values. */
private[modelsgen] type GenResult[A] = EitherT[Eval, GenError, A]

private[modelsgen] object GenResult:
  def success[A](value:    A): GenResult[A] = EitherT.rightT(value)
  def failure[A](error:    GenError): GenResult[A] = EitherT.leftT(error)
  def fromEither[A](value: Either[GenError, A]): GenResult[A] = EitherT.fromEither(value)
  def deferred[A](body:    => GenResult[A]): GenResult[A] = EitherT(Eval.defer(body.value))

enum ProfileRefusal {
  case ConstantInField(rule: QName)
  case ErrorFamilyInField(rule: QName)
  case UnsupportedPattern(pattern: String)
  case FieldTypeInPosition(value: RT)
  case MapEntry(occurrence: Ast.Occ, key: Option[RKey])
  case InvalidKeyIdentifier(name: String)
  case InvalidMemberIdentifier(name: String)
  case InheritedMember(name: String)
  case PreviewInWire, PreviewAlternative, NonMapAlternative, MixedTextAlternatives
  case RepeatedEnumeration(names: Vector[String], values: Vector[BigInt])
  case InvalidEnumerationLabels(labels: Vector[String])
  case EnumerationOutOfRange(labels: Vector[String])
  case RepeatedTexts(values: Vector[String])
  case AliasCycle(rule: QName)
  case NestedError(from: QName, to: QName)
  case ErrorField(value: FieldType)
  case InvalidErrorKind(kind: String)
  case ErrorKindField(entries: Vector[REntry])
  case RepeatedErrorKinds(kinds: Vector[String])
  case ErrorKindOrder(rule: QName, expected: Vector[String], actual: Option[Vector[String]])
  case PreviewShape(body: RuleBody)
  case ConstantOutOfRange(value: BigInt)
  case InlineChoice(alternatives: Vector[RT])
  case GroupRule, LiteralOutsideRecord
  case OptionalLiteral(field: String)
  case NoExportedFieldName(name: String)
  case NoExportedTypeName(name: String)
  case EnumerationCaseOutOfRange(name: String)
  case InvalidAlternativeName(source: String)
  case CoreReferences(modules: Vector[String])
  case ImportCycle(modules: Vector[String])
  case ForLanguage(language: Language, reason: ProfileRefusal)

  def message: String = this match {
    case ConstantInField(q)    => s"constant ${q.name} in field position (a constant is a value: name it in a range or size bound)"
    case ErrorFamilyInField(q) => s"error family ${q.name} in field position (an error is not a wire type: name its kinds)"
    case UnsupportedPattern(pattern) =>
      s".regexp ${CanonicalEmitter.literal(pattern)} (supported: ${Profile.Patterns.map(CanonicalEmitter.literal).mkString(", ")})"
    case FieldTypeInPosition(t)           => s"${SyntaxSummary(t)} in field position (name it as its own rule)"
    case MapEntry(_, key)                 => s"map entry with key ${key.fold("<none>")(_.toString)} (text-literal keys, once or optional)"
    case InvalidKeyIdentifier(name)       => s"key ${CanonicalEmitter.literal(name)} has no identifier form"
    case InvalidMemberIdentifier(name)    => s"member $name has no identifier form"
    case InheritedMember(name)            => s"key ${CanonicalEmitter.literal(name)} names a member every object inherits"
    case PreviewInWire                    => "preview in a wire type (a preview is an error's field only)"
    case PreviewAlternative               => "preview as a choice alternative (a preview is an error's field only)"
    case NonMapAlternative                => "choice alternative that is not a map rule"
    case MixedTextAlternatives            => "choice mixing rules of one text literal with other alternatives"
    case RepeatedEnumeration(_, _)        => "enumeration with repeated labels or values"
    case InvalidEnumerationLabels(labels) => s"enumeration labels with no identifier form: ${labels.mkString(", ")}"
    case EnumerationOutOfRange(labels)    => s"enumeration values beyond CDDL int: ${labels.mkString(", ")}"
    case RepeatedTexts(values)            => s"choice with repeated texts: ${values.map(CanonicalEmitter.literal).mkString(", ")}"
    case AliasCycle(q)                    => s"alias cycle through ${q.name}"
    case NestedError(from, to)            => s"error family ${from.name} nests ${to.name} (an error cannot hold itself)"
    case ErrorField(FieldType.Ref(q))     => s"error field of type ${q.name} (text, an integer, a boolean, a kind or an error)"
    case ErrorField(t)                    => s"error field of type $t (text, an integer, a boolean, a kind or an error)"
    case InvalidErrorKind(kind)           => s"error kind ${CanonicalEmitter.literal(kind)} (a kind is a PascalCase identifier)"
    case ErrorKindField(_)                => "error family alternative without exactly one \"kind\", a required text literal"
    case RepeatedErrorKinds(kinds)        => s"error family with repeated kinds: ${kinds.mkString(", ")}"
    case ErrorKindOrder(rule, kinds, _) =>
      s"error family whose kinds are not listed, in order, by ${rule.name} = ${kinds.map(CanonicalEmitter.literal).mkString(" / ")}"
    case PreviewShape(_)           => "preview that is not { \"head\": text, \"length\": uint }"
    case ConstantOutOfRange(value) => s"constant $value (a constant is a uint, 0..2^64-1)"
    case InlineChoice(_)           => "inline choice (a choice is of named map rules and text literals; use an enumeration for integers)"
    case GroupRule                 => "group rule"
    case LiteralOutsideRecord      => "literal outside a required record field"
    case OptionalLiteral(field)    => s"optional literal field $field"
    case NoExportedFieldName(name) => s"key ${CanonicalEmitter.literal(name)} has no exported Go name"
    case NoExportedTypeName(name)  => s"name $name has no exported Go form"
    case EnumerationCaseOutOfRange(name) => s"enumeration case $name beyond CDDL int"
    case InvalidAlternativeName(source)  => s"choice alternative $source has no identifier form"
    case CoreReferences(modules)         => s"a core module naming ${modules.mkString(", ")} (core cannot import a generated package)"
    case ImportCycle(modules)            => s"an import cycle ${modules.mkString(" -> ")}"
    case ForLanguage(language, reason)   => s"${reason.message} ($language)"
  }

}
