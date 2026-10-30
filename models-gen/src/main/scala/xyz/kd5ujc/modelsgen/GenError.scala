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
  case NameClash(scope: String, scalaName: String, rules: Vector[String])
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
      s"in $s the rules ${rs.mkString(", ")} all map to the Scala name `$n`"
    case BadConfig(l, p)            => s"config line $l: ${p.message}"
    case CyclicExpansion(q)         => s"cyclic expansion of `$q`"
    case EmptyIntegerDomain(lo, hi) => s"cannot sample an empty integer domain ($lo .. $hi)"
    case SamplingCycle(q)           => s"recursive sampling of `$q` cannot terminate beyond the depth cutoff"

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
  case Control(operator: String)
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
    case Control(op)               => s".$op"
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

/** Eval trampolines schema depth; EitherT propagates domain failures as values. */
private[modelsgen] type GenResult[A] = EitherT[Eval, GenError, A]

private[modelsgen] object GenResult:
  def success[A](value:    A): GenResult[A] = EitherT.rightT(value)
  def failure[A](error:    GenError): GenResult[A] = EitherT.leftT(error)
  def fromEither[A](value: Either[GenError, A]): GenResult[A] = EitherT.fromEither(value)
  def deferred[A](body:    => GenResult[A]): GenResult[A] = EitherT(Eval.defer(body.value))
