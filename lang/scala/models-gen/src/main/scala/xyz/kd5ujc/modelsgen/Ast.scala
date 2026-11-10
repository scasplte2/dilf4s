// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

/**
 * Surface syntax of CDDL (RFC 8610), as parsed. Names are unresolved; generics are not expanded.
 */
object Ast:
  enum Lit:
    case IntL(value: BigInt)
    case TextL(value: String)
    case BytesL(value: Vector[Byte])
    case FloatL(text: String)

  /** Occurrence indicator: `min` to `max` repetitions (`None` is unbounded). */
  final case class Occ(min: Int, max: Option[Int]):
    def isOnce: Boolean = min == 1 && max.contains(1)
    def isOptional: Boolean = min == 0 && max.contains(1)
  object Occ:
    val Once: Occ = Occ(1, Some(1))
    val Optional: Occ = Occ(0, Some(1))
    val Star: Occ = Occ(0, None)
    val Plus: Occ = Occ(1, None)

  enum Comparison(val wire: String):
    case Greater extends Comparison("gt")
    case GreaterOrEqual extends Comparison("ge")
    case Less extends Comparison("lt")
    case LessOrEqual extends Comparison("le")
    case NotEqual extends Comparison("ne")

  enum Control:
    case Size, Regexp, Cbor, Default
    case Compare(relation: Comparison)
    case Other(name: String)

    def wire: String = this match
      case Size              => "size"
      case Regexp            => "regexp"
      case Cbor              => "cbor"
      case Default           => "default"
      case Compare(relation) => relation.wire
      case Other(name)       => name

  object Control:
    def fromWire(name: String): Control = name match
      case "size"    => Control.Size
      case "regexp"  => Control.Regexp
      case "cbor"    => Control.Cbor
      case "default" => Control.Default
      case other     => Comparison.values.find(_.wire == other).fold[Control](Control.Other(other))(Control.Compare(_))

  enum PragmaKind(val wire: String):
    case Import extends PragmaKind("import")
    case Include extends PragmaKind("include")

  enum Ty:
    case Name(id: String, args: Vector[Ty])
    case Literal(value: Lit)
    case Range(lo: Ty, hi: Ty, inclusive: Boolean)
    case Ctrl(base: Ty, op: Control, arg: Ty)
    case Choice(alts: Vector[Ty])
    case Array(group: Group)
    case MapT(group: Group)

    /** `#6.n(t)`; major is -1 for a bare `#`. */
    case Tag(major: Int, number: Option[BigInt], inner: Option[Ty])

    /** A parenthesised group in rule-body position (a group rule). */
    case GroupT(group: Group)

    /** `&group` / `&(group)`: the choice of the group's entry values. */
    case Enumeration(of: Ty)

    /** `~name`. */
    case Unwrap(of: Ty)

  enum Key:
    case Bare(name: String)
    case Value(lit: Lit)
    case OfType(ty: Ty, cut: Boolean)

  /**
   * `doc` is the `;` comment block directly above the entry, when the entry starts its line (see [[CddlParser.docs]]):
   * it becomes the generated field's or kind's documentation.
   */
  final case class Entry(occ: Occ, key: Option[Key], value: Ty, doc: Vector[String] = Vector.empty)

  /** A group: alternatives (`//`), each a sequence of entries. */
  final case class Group(choices: Vector[Vector[Entry]])

  enum Assign:
    case Eq, TypeChoice, GroupChoice

  /** `doc` is the `;` comment block directly above the rule: the generated type's documentation. */
  final case class Rule(name: String, params: Vector[String], assign: Assign, body: Ty, doc: Vector[String] = Vector.empty)

  /** `;# import|include <module> as <alias>` directives (ouroboros specs and the ledger cddl). */
  final case class Pragma(kind: PragmaKind, module: String, alias: String)

  final case class File(rules: Vector[Rule], pragmas: Vector[Pragma])
