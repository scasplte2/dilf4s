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

  enum Ty:
    case Name(id: String, args: Vector[Ty])
    case Literal(value: Lit)
    case Range(lo: Ty, hi: Ty, inclusive: Boolean)
    case Ctrl(base: Ty, op: String, arg: Ty)
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

  final case class Entry(occ: Occ, key: Option[Key], value: Ty)

  /** A group: alternatives (`//`), each a sequence of entries. */
  final case class Group(choices: Vector[Vector[Entry]])

  enum Assign:
    case Eq, TypeChoice, GroupChoice

  final case class Rule(name: String, params: Vector[String], assign: Assign, body: Ty)

  /** `;# import|include <module> as <alias>` directives (ouroboros specs and the ledger cddl). */
  final case class Pragma(kind: String, module: String, alias: String)

  final case class File(rules: Vector[Rule], pragmas: Vector[Pragma])
