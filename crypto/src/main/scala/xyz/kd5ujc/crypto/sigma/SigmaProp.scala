package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector

/**
 * A sigma proposition: a tree of connectives over two kinds of statement, each proven by a sigma protocol and composed
 * so that a single Fiat-Shamir challenge binds the whole tree (Cramer, Damgård, Schoenmakers). The construction and
 * every byte layout are Ergo's (sigmastate 5.0.13); docs/specs/sigma.md is normative.
 *
 * Built through the smart constructors, which reject trivial trees (they verify for anyone) and bound the size. A
 * product mirror can build one around them, so [[Sigma.verify]] and [[SigmaProver]] check every tree again
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
  val MaxDepth: Int = 64

  /** Leaf bound: each leaf costs curve work, and a threshold evaluates at most 255 points. */
  val MaxLeaves: Int = 4096

  /** A threshold's points are the bytes 1..n. */
  val MaxChildren: Int = 255

  /**
   * A statement point is never the identity. With `pk` the identity, `dlog(pk)` holds for `w = 0`, and anyone can prove
   * it; the same holds for a DH tuple with `u` or `v` the identity, and a base `g` or `h` that is the identity proves
   * nothing. (Commitments may be the identity: they are computed, never taken from input.)
   */
  private def statementPoint(field: String, p: GroupElement): Either[String, GroupElement] =
    Either
      .cond(Secp256k1Group.isPoint(p), p, s"$field is not a point of secp256k1")
      .filterOrElse(!_.isIdentity, s"$field is the identity, for which anyone can prove knowledge (w = 0)")

  /**
   * Checks every constraint the smart constructors enforce, over the whole tree: statement points on the curve and not
   * the identity, 1 to [[MaxChildren]] children per connective, `1 <= k <= n`, depth and leaf bounds. Scala 3 product
   * mirrors build these case classes around their private constructors, so [[Sigma.verify]] and [[SigmaProver.prove]]
   * check here rather than trust the type. A worklist that stops at the first violation, so a tree of any depth or
   * width is rejected without overflowing the stack.
   */
  def validate(prop: SigmaProp): Either[String, SigmaProp] = {
    @tailrec
    def go(todo: List[(SigmaProp, Int)], leaves: Int): Either[String, Unit] =
      todo match {
        case Nil                                                  => ().asRight
        case (_, d) :: _ if d > MaxDepth                          => s"a proposition nests at most $MaxDepth deep".asLeft
        case (_: (Dlog | DhTuple), _) :: _ if leaves >= MaxLeaves => s"a proposition has at most $MaxLeaves leaves".asLeft
        case (Dlog(pk), _) :: tl =>
          statementPoint("pk", pk) match {
            case Left(e)  => e.asLeft
            case Right(_) => go(tl, leaves + 1)
          }
        case (DhTuple(g, h, u, v), _) :: tl =>
          (statementPoint("g", g), statementPoint("h", h), statementPoint("u", u), statementPoint("v", v)).tupled match {
            case Left(e)  => e.asLeft
            case Right(_) => go(tl, leaves + 1)
          }
        case (Threshold(k, cs), _) :: _ if k < 1 || k > cs.size =>
          s"a threshold needs 1 <= k <= ${cs.size} children, got k = $k".asLeft
        case (c: Conjecture, _) :: _ if c.children.isEmpty =>
          "a connective needs at least one child: an empty one holds for anyone".asLeft
        case (c: Conjecture, _) :: _ if c.children.size > MaxChildren => s"a connective has at most $MaxChildren children".asLeft
        case (c: Conjecture, d) :: tl                                 => go(c.children.toList.map(_ -> (d + 1)) ::: tl, leaves)
      }
    go(List(prop -> 1), 0).as(prop)
  }

  def dlog(pk: GroupElement): Either[String, Dlog] = statementPoint("pk", pk).map(Dlog(_))

  def dhTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): Either[String, DhTuple] =
    (statementPoint("g", g), statementPoint("h", h), statementPoint("u", u), statementPoint("v", v)).mapN(DhTuple(_, _, _, _))

  def and(children: Vector[SigmaProp]): Either[String, SigmaProp] = conjecture(children)(And(children))
  def or(children:  Vector[SigmaProp]): Either[String, SigmaProp] = conjecture(children)(Or(children))

  def threshold(k: Int, children: Vector[SigmaProp]): Either[String, SigmaProp] =
    Either
      .cond(k >= 1 && k <= children.size, (), s"a threshold needs 1 <= k <= ${children.size} children, got k = $k")
      .flatMap(_ => conjecture(children)(Threshold(k, children)))

  private def conjecture(children: Vector[SigmaProp])(node: => SigmaProp): Either[String, SigmaProp] =
    if (children.isEmpty) "a connective needs at least one child: an empty one holds for anyone".asLeft
    else if (children.size > MaxChildren) s"a connective has at most $MaxChildren children".asLeft
    else {
      val n = node
      if (n.depth > MaxDepth) s"a proposition nests at most $MaxDepth deep".asLeft
      else if (n.leaves > MaxLeaves) s"a proposition has at most $MaxLeaves leaves".asLeft
      else n.asRight
    }

  // ---- the canonical form (docs/specs/sigma.md §3): a map per node, discriminated by "type", with no other fields

  /**
   * `{"type": "dlog", "pk": bytes}`, `{"type": "dhtuple", "g", "h", "u", "v": bytes}`, `{"type": "and" | "or",
   * "children": [...]}`, `{"type": "threshold", "k": int, "children": [...]}`. A field outside its type's schema is an
   * error, so the encoding is canonical. Value depth is bounded by the canonical model, so this recursion is too.
   */
  def fromCanonical(v: CanonicalValue): Either[String, SigmaProp] =
    v match {
      case CMap(m) =>
        def point(k: String): Either[String, GroupElement] =
          m.get(k) match {
            case scala.Some(CBytes(b)) => Secp256k1Group.decode(b).leftMap(e => s"$k: $e")
            case _                     => s"$k must be bytes".asLeft
          }
        def children: Either[String, Vector[SigmaProp]] =
          m.get("children") match {
            case scala.Some(CArray(cs)) => cs.traverse(fromCanonical)
            case _                      => "children must be an array".asLeft
          }
        def fields(ks: String*): Either[String, Unit] =
          Either.cond(m.keySet == (ks.toSet + "type"), (), s"${typeName(m)} takes exactly the fields ${ks.mkString(", ")}")
        m.get("type") match {
          case scala.Some(CText("dlog")) => fields("pk") >> point("pk").flatMap(dlog)
          case scala.Some(CText("dhtuple")) =>
            fields("g", "h", "u", "v") >> (point("g"), point("h"), point("u"), point("v")).tupled.flatMap(dhTuple.tupled)
          case scala.Some(CText("and")) => fields("children") >> children.flatMap(and)
          case scala.Some(CText("or"))  => fields("children") >> children.flatMap(or)
          case scala.Some(CText("threshold")) =>
            fields("k", "children") >> (m.get("k") match {
              case scala.Some(CInt(k)) if k.isValidInt => children.flatMap(threshold(k.toInt, _))
              case _                                   => "k must be an integer".asLeft
            })
          case _ => "a proposition's type is dlog, dhtuple, and, or or threshold".asLeft
        }
      case _ => "a proposition is an object".asLeft
    }

  private def typeName(m: Map[String, CanonicalValue]): String =
    m.get("type").collect { case CText(t) => t }.getOrElse("a proposition")

  // ---- the Fiat-Shamir statement bytes of a leaf: Ergo's ErgoTree for SigmaPropConstant(leaf), constants segregated

  private val TreeHeader = ByteVector(0x10, 0x01, 0x08) // header (v0, segregated), 1 constant, of type SigmaProp
  private val TreeBody = ByteVector(0x73, 0x00) // the body: ConstantPlaceholder(0)

  def statement(leaf: Dlog | DhTuple): ByteVector =
    leaf match {
      case Dlog(pk)            => TreeHeader ++ ByteVector(0xcd) ++ pk.bytes ++ TreeBody
      case DhTuple(g, h, u, v) => TreeHeader ++ ByteVector(0xce) ++ g.bytes ++ h.bytes ++ u.bytes ++ v.bytes ++ TreeBody
    }
}
