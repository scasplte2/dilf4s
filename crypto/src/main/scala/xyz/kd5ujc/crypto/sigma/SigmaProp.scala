package xyz.kd5ujc.crypto.sigma

import cats.syntax.all._

import scodec.bits.ByteVector

/**
 * A sigma proposition: a tree of connectives over two kinds of statement, each proven by a sigma protocol and composed
 * so that a single Fiat-Shamir challenge binds the whole tree (Cramer, Damgård, Schoenmakers). The construction and
 * every byte layout are Ergo's (sigmastate 5.0.13); docs/specs/sigma.md is normative.
 *
 * Built only through the smart constructors, which reject trivial trees (they verify for anyone) and bound the size.
 */
sealed trait SigmaProp {
  def depth: Int
  def leaves: Int
}

object SigmaProp {

  /** Knowledge of `w` with `pk = w·G`: a Schnorr signature. */
  final case class Dlog(pk: GroupElement) extends SigmaProp {
    def depth: Int = 1
    def leaves: Int = 1
  }

  /** Knowledge of `w` with `u = w·g` and `v = w·h`: (g, h, u, v) is a Diffie-Hellman tuple. */
  final case class DhTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement) extends SigmaProp {
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

  // ---- the Fiat-Shamir statement bytes of a leaf: Ergo's ErgoTree for SigmaPropConstant(leaf), constants segregated

  private val TreeHeader = ByteVector(0x10, 0x01, 0x08) // header (v0, segregated), 1 constant, of type SigmaProp
  private val TreeBody = ByteVector(0x73, 0x00) // the body: ConstantPlaceholder(0)

  def statement(leaf: Dlog | DhTuple): ByteVector =
    leaf match {
      case Dlog(pk)            => TreeHeader ++ ByteVector(0xcd) ++ pk.bytes ++ TreeBody
      case DhTuple(g, h, u, v) => TreeHeader ++ ByteVector(0xce) ++ g.bytes ++ h.bytes ++ u.bytes ++ v.bytes ++ TreeBody
    }
}
