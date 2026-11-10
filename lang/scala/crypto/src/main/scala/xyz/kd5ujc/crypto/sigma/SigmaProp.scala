package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, ValueKind}
import xyz.kd5ujc.crypto.sigma.SigmaError.{PointRole, PropositionKind, PropositionReason}

import scodec.bits.ByteVector

/**
 * A sigma proposition: a tree of connectives over two kinds of statement, each proven by a sigma protocol and composed
 * so that a single Fiat-Shamir challenge binds the whole tree (Cramer, Damgård, Schoenmakers). The construction and
 * every byte layout are Ergo's (sigmastate 5.0.13); docs/specs/sigma.md is normative.
 *
 * Built through the smart constructors, which reject trivial trees (they verify for anyone) and bound the size. A
 * product mirror can build one around them, so [[Sigma.check]] and [[SigmaProver]] check every tree again
 * ([[SigmaProp.validate]]).
 */
sealed trait SigmaProp {
  def depth: Int
  def leaves: Int
}

object SigmaProp {

  /** Knowledge of `w` with `pk = w·G`: a Schnorr signature. Built by [[dlog]]. */
  final case class Dlog private[SigmaProp] (pk: GroupElement) extends SigmaProp {
    def depth: Int = 1
    def leaves: Int = 1
  }

  /** Knowledge of `w` with `u = w·g` and `v = w·h`: (g, h, u, v) is a Diffie-Hellman tuple. Built by [[dhTuple]]. */
  final case class DhTuple private[SigmaProp] (g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement) extends SigmaProp {
    def depth: Int = 1
    def leaves: Int = 1
  }

  /** Every child holds. */
  final case class And private[SigmaProp] (children: Vector[SigmaProp]) extends SigmaProp with Conjecture

  /** At least one child holds; which one is hidden. */
  final case class Or private[SigmaProp] (children: Vector[SigmaProp]) extends SigmaProp with Conjecture

  /** At least `k` children hold; which ones are hidden. */
  final case class Threshold private[SigmaProp] (k: Int, children: Vector[SigmaProp]) extends SigmaProp with Conjecture

  sealed trait Conjecture { self: SigmaProp =>
    def children: Vector[SigmaProp]
    final lazy val depth: Int = 1 + children.foldLeft(0)((d, c) => math.max(d, c.depth))
    final lazy val leaves: Int = children.foldLeft(0)((n, c) => n + c.leaves)
  }

  /** Nesting bound: the verifier and prover recurse on the tree. */
  val MaxDepth: Int = xyz.kd5ujc.models.limits.SigmaMaxDepth

  /** Leaf bound: each leaf costs curve work, and a threshold evaluates at most 255 points. */
  val MaxLeaves: Int = xyz.kd5ujc.models.limits.SigmaMaxLeaves

  /** A threshold's points are the bytes 1..n. */
  val MaxChildren: Int = ChildPoint.Max

  /**
   * A statement point is never the identity. With `pk` the identity, `dlog(pk)` holds for `w = 0`, and anyone can prove
   * it; the same holds for a DH tuple with `u` or `v` the identity, and a base `g` or `h` that is the identity proves
   * nothing. (Commitments may be the identity: they are computed, never taken from input.)
   */
  private def statementPoint(field: PointRole, p: GroupElement): Either[SigmaError, GroupElement] =
    Either
      .cond(Secp256k1Group.isPoint(p), p, SigmaError.InvalidPoint(field, Secp256k1Group.DecodeError.InvalidPoint): SigmaError)
      .filterOrElse(!_.isIdentity, invalid(PropositionReason.Identity(field)))

  private def invalid(reason: PropositionReason): SigmaError = SigmaError.InvalidProposition(reason)

  /**
   * Checks every constraint the smart constructors enforce, over the whole tree: statement points on the curve and not
   * the identity, 1 to [[MaxChildren]] children per connective, `1 <= k <= n`, depth and leaf bounds. Scala 3 product
   * mirrors build these case classes around their private constructors, so [[Sigma.check]] and [[SigmaProver.prove]]
   * check here rather than trust the type. A worklist that stops at the first violation, so a tree of any depth or
   * width is rejected without overflowing the stack.
   */
  def validate(prop: SigmaProp): Either[SigmaError, SigmaProp] = {
    @tailrec
    def go(todo: List[(SigmaProp, Int)], leaves: Int): Either[SigmaError, Unit] =
      todo match {
        case Nil                                                  => ().asRight
        case (_, d) :: _ if d > MaxDepth                          => invalid(PropositionReason.Depth(MaxDepth)).asLeft
        case (_: (Dlog | DhTuple), _) :: _ if leaves >= MaxLeaves => invalid(PropositionReason.Leaves(MaxLeaves)).asLeft
        case (Dlog(pk), _) :: tl =>
          statementPoint(PointRole.PublicKey, pk) match {
            case Left(e)  => e.asLeft
            case Right(_) => go(tl, leaves + 1)
          }
        case (DhTuple(g, h, u, v), _) :: tl =>
          (
            statementPoint(PointRole.G, g),
            statementPoint(PointRole.H, h),
            statementPoint(PointRole.U, u),
            statementPoint(PointRole.V, v)
          ).tupled match {
            case Left(e)  => e.asLeft
            case Right(_) => go(tl, leaves + 1)
          }
        case (Threshold(k, cs), _) :: _ if k < 1 || k > cs.size =>
          invalid(PropositionReason.Threshold(k, cs.size)).asLeft
        case (c: Conjecture, _) :: _ if c.children.isEmpty =>
          invalid(PropositionReason.EmptyChildren).asLeft
        case (c: Conjecture, _) :: _ if c.children.size > MaxChildren => invalid(PropositionReason.Children(MaxChildren)).asLeft
        case (c: Conjecture, d) :: tl                                 => go(c.children.toList.map(_ -> (d + 1)) ::: tl, leaves)
      }
    go(List(prop -> 1), 0).as(prop)
  }

  // The smart constructors report the refusal's kind, as decoding a caller's proposition ([[decode]]) does.

  def dlog(pk: GroupElement): Either[SigmaError, Dlog] = statementPoint(PointRole.PublicKey, pk).map(Dlog(_))

  def dhTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): Either[SigmaError, DhTuple] =
    (statementPoint(PointRole.G, g), statementPoint(PointRole.H, h), statementPoint(PointRole.U, u), statementPoint(PointRole.V, v))
      .mapN(DhTuple(_, _, _, _))

  def and(children: Vector[SigmaProp]): Either[SigmaError, SigmaProp] = conjecture(children)(And(children))
  def or(children:  Vector[SigmaProp]): Either[SigmaError, SigmaProp] = conjecture(children)(Or(children))

  def threshold(k: Int, children: Vector[SigmaProp]): Either[SigmaError, SigmaProp] =
    Either
      .cond(k >= 1 && k <= children.size, (), invalid(PropositionReason.Threshold(k, children.size)))
      .flatMap(_ => conjecture(children)(Threshold(k, children)))

  private def conjecture(children: Vector[SigmaProp])(node: => SigmaProp): Either[SigmaError, SigmaProp] =
    if (children.isEmpty) invalid(PropositionReason.EmptyChildren).asLeft
    else if (children.size > MaxChildren) invalid(PropositionReason.Children(MaxChildren)).asLeft
    else {
      val n = node
      if (n.depth > MaxDepth) invalid(PropositionReason.Depth(MaxDepth)).asLeft
      else if (n.leaves > MaxLeaves) invalid(PropositionReason.Leaves(MaxLeaves)).asLeft
      else n.asRight
    }

  // ---- the canonical form (docs/specs/sigma.md §3): a map per node, discriminated by "type", with no other fields

  /**
   * The weight ([[CanonicalValue.weight]]) of the heaviest valid proposition in its canonical form (docs/specs/sigma.md
   * §3): 4096 `dhtuple` leaves (158 each) at depth 64, under `1 + 255 + 61·4096` one-`k` thresholds (29 each, plus
   * their children). A heavier value is refused before any of it is decoded.
   */
  val MaxWeight: Long = xyz.kd5ujc.models.limits.SigmaMaxWeight

  /** A connective as read, before its children are decoded; a threshold's `k` as given. */
  private enum Kind {
    case AllOf, AnyOf
    case AtLeast(k: Int)
  }

  /** One node as read: a checked leaf, or a connective, within [[MaxChildren]], whose children are still to decode. */
  private enum Read {
    case Leaf(prop: SigmaProp)
    case Node(kind: Kind, children: Vector[CanonicalValue])
  }

  private enum Step {
    case Decode(value: CanonicalValue)
    case Done(prop: SigmaProp)
  }

  /** A connective whose children are being decoded: those still to decode, and those decoded. */
  final private case class Open(kind: Kind, rest: List[CanonicalValue], done: Vector[SigmaProp])

  /**
   * `{"type": "dlog", "pk": bytes}`, `{"type": "dhtuple", "g", "h", "u", "v": bytes}`, `{"type": "and" | "or",
   * "children": [...]}`, `{"type": "threshold", "k": int, "children": [...]}`. A field outside its type's schema is an
   * error, so the encoding is canonical.
   *
   * The order (docs/specs/sigma.md §3): a value heavier than [[MaxWeight]] is refused first, in O(1). Then each node,
   * in pre-order: its own fields; for a leaf, the leaf bound before its points are decoded; for a connective, its width
   * before its children are decoded and, once they are, its own checks (`k`, emptiness, depth) through its smart
   * constructor. A worklist, so a value of any depth decodes without growing the stack.
   */
  def decode(v: CanonicalValue): Either[SigmaError, SigmaProp] =
    if (v.weight > MaxWeight) invalid(PropositionReason.Weight(MaxWeight)).asLeft
    else decode(Step.Decode(v), Nil, 0)

  /** `leaves` counts every leaf decoded so far, in the whole proposition. */
  @tailrec
  private def decode(step: Step, open: List[Open], leaves: Int): Either[SigmaError, SigmaProp] =
    step match {
      case Step.Decode(v) =>
        read(v, leaves) match {
          case Left(e)             => e.asLeft
          case Right(Read.Leaf(p)) => decode(Step.Done(p), open, leaves + 1)
          case Right(Read.Node(k, cs)) =>
            advance(Open(k, cs.toList, Vector.empty), open) match {
              case Left(e)          => e.asLeft
              case Right((s, next)) => decode(s, next, leaves)
            }
        }
      case Step.Done(p) =>
        open match {
          case Nil => p.asRight
          case o :: up =>
            advance(o.copy(done = o.done :+ p), up) match {
              case Left(e)          => e.asLeft
              case Right((s, next)) => decode(s, next, leaves)
            }
        }
    }

  /** The next child of `o` to decode, or, when there is none, `o` checked and built. */
  private def advance(o: Open, up: List[Open]): Either[SigmaError, (Step, List[Open])] =
    o.rest match {
      case c :: rest => (Step.Decode(c), o.copy(rest = rest) :: up).asRight
      case Nil =>
        (o.kind match {
          case Kind.AllOf      => conjecture(o.done)(And(o.done))
          case Kind.AnyOf      => conjecture(o.done)(Or(o.done))
          case Kind.AtLeast(k) => threshold(k, o.done)
        }).map(p => (Step.Done(p), up))
    }

  /** One node, `leaves` leaves into the proposition. */
  private def read(v: CanonicalValue, leaves: Int): Either[SigmaError, Read] =
    v match {
      case CMap(m) =>
        def point(k: PointRole): Either[SigmaError, GroupElement] =
          m.get(k.wire) match {
            case scala.Some(CBytes(b)) => Secp256k1Group.decode(b).leftMap(e => SigmaError.InvalidPoint(k, e))
            case other                 => invalid(PropositionReason.PointBytes(k, other.map(ValueKind.of))).asLeft
          }
        def node(kind: Kind): Either[SigmaError, Read] =
          m.get("children") match {
            case scala.Some(CArray(cs)) if cs.size > MaxChildren => invalid(PropositionReason.Children(MaxChildren)).asLeft
            case scala.Some(CArray(cs))                          => Read.Node(kind, cs).asRight
            case _                                               => invalid(PropositionReason.ChildrenArray).asLeft
          }
        def leaf(p: => Either[SigmaError, SigmaProp]): Either[SigmaError, Read] =
          Either.cond(leaves < MaxLeaves, (), invalid(PropositionReason.Leaves(MaxLeaves))) >> p.map(Read.Leaf(_))
        def fields(kind: PropositionKind): Either[SigmaError, Unit] =
          Either.cond(
            m.size == kind.fields.size + 1 && ("type" +: kind.fields).forall(k => m.contains(k)),
            (),
            invalid(PropositionReason.Fields(kind, m.size))
          )
        m.get("type") match {
          case scala.Some(CText("dlog")) => fields(PropositionKind.Dlog) >> leaf(point(PointRole.PublicKey).flatMap(dlog))
          case scala.Some(CText("dhtuple")) =>
            fields(PropositionKind.DhTuple) >> leaf(
              (point(PointRole.G), point(PointRole.H), point(PointRole.U), point(PointRole.V)).tupled.flatMap(dhTuple.tupled)
            )
          case scala.Some(CText("and")) => fields(PropositionKind.And) >> node(Kind.AllOf)
          case scala.Some(CText("or"))  => fields(PropositionKind.Or) >> node(Kind.AnyOf)
          case scala.Some(CText("threshold")) =>
            fields(PropositionKind.Threshold) >> (m.get("k") match {
              case scala.Some(CInt(k)) if k.isValidInt => node(Kind.AtLeast(k.toInt))
              case _                                   => invalid(PropositionReason.ThresholdInteger).asLeft
            })
          case _ => invalid(PropositionReason.UnknownType).asLeft
        }
      case _ => invalid(PropositionReason.ExpectedObject).asLeft
    }

  // ---- the Fiat-Shamir statement bytes of a leaf: Ergo's ErgoTree for SigmaPropConstant(leaf), constants segregated

  private val TreeHeader = ByteVector(0x10, 0x01, 0x08) // header (v0, segregated), 1 constant, of type SigmaProp
  private val TreeBody = ByteVector(0x73, 0x00) // the body: ConstantPlaceholder(0)

  def statement(leaf: Dlog | DhTuple): ByteVector =
    leaf match {
      case Dlog(pk)            => TreeHeader ++ ByteVector(0xcd) ++ pk.bytes ++ TreeBody
      case DhTuple(g, h, u, v) => TreeHeader ++ ByteVector(0xce) ++ g.bytes ++ h.bytes ++ u.bytes ++ v.bytes ++ TreeBody
    }
}
