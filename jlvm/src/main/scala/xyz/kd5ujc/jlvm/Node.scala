package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.{CArray, CText}

/**
 * The checked form of a program. Every special form's shape (`if`, `let`, the callbacks) is validated once, by
 * [[Node.compile]], so evaluation never meets a malformed form; what remains are value errors.
 */
enum Node {
  case Const(value: CanonicalValue)
  case Read(ref: VarRef, default: Option[CanonicalValue])

  /** `var` with a computed path: text is parsed as a `var` path; an array gives the exact segments of a data path. */
  case ReadAt(path: Node, default: Option[CanonicalValue])
  case ArrayOf(items: Vector[Node])
  case ObjectOf(fields: Vector[(String, Node)])

  /** An operation on evaluated arguments ([[PureOps]]). */
  case Strict(op: Op, args: Vector[Node])

  /** A [[Op.Variadic]] strict operation on the elements of one array value. */
  case Spread(op: Op, arg: Node)

  /** `if`: the first branch whose condition is truthy, else `otherwise`. */
  case Cond(branches: Vector[(Node, Node)], otherwise: Node)

  /** `and`, `or`, `default`: arguments evaluated left to right, only as far as needed. */
  case Logic(kind: Node.Logical, args: Vector[Node])

  /** `let`: each binding sees the ones before it; the body sees all of them, as `$name`. */
  case Bind(bindings: Vector[(String, Node)], body: Node)

  /** `map`, `filter`, `all`, `none`, `find`, `count`: the body runs per element with `$current` and `$index`. */
  case Each(kind: Node.Iteration, over: Node, body: Node)

  /** `some`, with an optional quorum (metakit): at least `min` elements satisfy the body; `min` defaults to 1. */
  case Quorum(over: Node, body: Node, min: Option[Node])

  /** `reduce`: the body runs per element with `$current`, `$accumulator` and `$index`. */
  case Fold(over: Node, body: Node, init: Node)

  /** `missing`, `missing_some`: which data paths are absent. */
  case Lookup(op: Op, args: Vector[Node])

  /** Cryptographic operations. */
  case Crypto(op: Op, args: Vector[Node])
}

object Node {

  enum Logical(val op: Op) {
    case And extends Logical(Op.And)
    case Or extends Logical(Op.Or)
    case Default extends Logical(Op.Default)
  }

  enum Iteration(val op: Op) {
    case Map extends Iteration(Op.Map)
    case Filter extends Iteration(Op.Filter)
    case All extends Iteration(Op.All)
    case None extends Iteration(Op.None)
    case Find extends Iteration(Op.Find)
    case Count extends Iteration(Op.Count)
  }

  /** Names a `let` may bind: non-empty, without `.` (the path separator) or a leading `$` (the reference sigil). */
  def bindable(name: String): Boolean = name.nonEmpty && !name.contains('.') && !name.startsWith("$")

  /** Program depth is bounded ([[Program.MaxDepth]]), so this recursion is bounded too. */
  def compile(program: Program): Either[JlvmError, Node] = node(program.expr)

  private def malformed[A](msg: String): Either[JlvmError, A] = JlvmError.Malformed(msg).asLeft

  private def node(e: Expr): Either[JlvmError, Node] =
    e match {
      case Expr.Lit(v)      => Const(v).asRight
      case Expr.Var(ref, d) => Read(ref, d).asRight
      case Expr.VarAt(p, d) => node(p).map(ReadAt(_, d))
      case Expr.Spread(op, a) =>
        def notArray(kind: String) = malformed[Node](s"a spread ${op.wire} takes an array, and its argument is always of kind $kind")
        node(a).flatMap {
          case n @ Const(CArray(_)) => Spread(op, n).asRight
          case Const(v)             => notArray(Values.kind(v))
          case ObjectOf(_)          => notArray("object")
          case n                    => Spread(op, n).asRight
        }
      case Expr.Arr(items)      => items.traverse(node).map(ArrayOf(_))
      case Expr.Obj(fields)     => fields.toVector.traverse { case (k, x) => node(x).map(k -> _) }.map(fs => ObjectOf(sorted(fs)))
      case Expr.Apply(op, args) => apply(op, args)
    }

  private def sorted[A](fields: Vector[(String, A)]): Vector[(String, A)] =
    fields.sortWith((a, b) => Values.compareCodePoints(a._1, b._1) < 0)

  private def apply(op: Op, args: Vector[Expr]): Either[JlvmError, Node] = {
    def each(kind: Iteration) = args match {
      case Vector(over, body) => (node(over), node(body)).mapN(Each(kind, _, _))
      case _                  => malformed(s"${op.wire} takes [array, callback]")
    }
    def nodes = args.traverse(node)
    op match {
      case Op.If =>
        if (args.size >= 3 && args.size % 2 == 1)
          nodes.flatMap {
            case pairs :+ otherwise => Cond(pairs.grouped(2).collect { case Vector(c, t) => (c, t) }.toVector, otherwise).asRight
            case _                  => malformed("if takes [condition, then, (condition, then)..., else]")
          }
        else malformed("if takes [condition, then, (condition, then)..., else]")
      case Op.And                     => nodes.map(Logic(Logical.And, _))
      case Op.Or                      => nodes.map(Logic(Logical.Or, _))
      case Op.Default                 => nodes.map(Logic(Logical.Default, _))
      case Op.Let                     => let(args)
      case Op.Map                     => each(Iteration.Map)
      case Op.Filter                  => each(Iteration.Filter)
      case Op.All                     => each(Iteration.All)
      case Op.None                    => each(Iteration.None)
      case Op.Find                    => each(Iteration.Find)
      case Op.Count if args.size == 2 => each(Iteration.Count)
      case Op.Some =>
        args match {
          case Vector(over, body)      => (node(over), node(body)).mapN(Quorum(_, _, scala.None))
          case Vector(over, body, min) => (node(over), node(body), node(min)).mapN((o, b, m) => Quorum(o, b, scala.Some(m)))
          case _                       => malformed("some takes [array, callback] or [array, callback, minimum]")
        }
      case Op.Reduce =>
        args match {
          case Vector(over, body, init) => (node(over), node(body), node(init)).mapN(Fold(_, _, _))
          case _                        => malformed("reduce takes [array, callback, initial]")
        }
      case Op.Missing | Op.MissingSome              => nodes.map(Lookup(op, _))
      case _ if PureOps(op, Vector.empty).isDefined => nodes.map(Strict(op, _))
      case _                                        => nodes.map(Crypto(op, _))
    }
  }

  /** `[[[name, expr], ...], body]`, or `[{name: expr, ...}, body]` with bindings in code-point order of the names. */
  private def let(args: Vector[Expr]): Either[JlvmError, Node] = {
    def name(n: String): Either[JlvmError, String] =
      Either.cond(bindable(n), n, JlvmError.Malformed(s"let cannot bind \"$n\": names are non-empty, without '.' or a leading '$$'"))
    val bindings: Either[JlvmError, Vector[(String, Expr)]] = args.headOption match {
      case scala.Some(Expr.Arr(pairs)) =>
        pairs.traverse {
          case Expr.Arr(Vector(Expr.Lit(CText(n)), x)) => name(n).map(_ -> x)
          case _                                       => malformed("a let binding is [name, expression]")
        }
      case scala.Some(Expr.Obj(fields)) => sorted(fields.toVector).traverse { case (n, x) => name(n).map(_ -> x) }
      case _                            => malformed("let takes [bindings, body]")
    }
    args match {
      case Vector(_, body) =>
        bindings.flatMap { bs =>
          val names = bs.map(_._1)
          Either.cond(names.distinct.size == names.size, bs, JlvmError.Malformed("let binds a name twice"))
        }.flatMap(bs => (bs.traverse { case (n, x) => node(x).map(n -> _) }, node(body)).mapN(Bind(_, _)))
      case _ => malformed("let takes [bindings, body]")
    }
  }
}
