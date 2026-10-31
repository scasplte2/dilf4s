package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.collection.immutable.TreeSet

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

  /** Keys are checked text, once, at compilation: evaluation never rescans them. */
  case ObjectOf(fields: Vector[(CText, Node)])

  /** An operation on evaluated arguments ([[PureOps]]). */
  case Strict(op: Op, args: Vector[Node])

  /** A [[Op.Variadic]] strict operation on the elements of one array value. */
  case Spread(op: Op, arg: Node)

  /** `if`: the first branch whose condition is truthy, else `otherwise` (null when the program gives no else). */
  case Cond(branches: Vector[(Node, Node)], otherwise: Node)

  /** `and`, `or`, `default`: arguments evaluated left to right, only as far as needed. */
  case Logic(kind: Node.Logical, args: Vector[Node])

  /** `let`: each binding sees the ones before it; the body sees all of them, as `$name`. */
  case Bind(bindings: Vector[(String, Node)], body: Node)

  /** `map`, `filter`, `all`, `none`, `find`, `count`: the body runs per element with `$current` and `$index`. */
  case Each(kind: Node.Iteration, over: Node, body: Node)

  /** `some`, with an optional quorum (metakit): at least `min` elements satisfy the body; `min` defaults to 1. */
  case Quorum(over: Node, body: Node, min: Option[Node])

  /**
   * `reduce`: the body runs per element with `$current`, `$accumulator` and `$index`. Without `init`, the first element
   * is the initial accumulator and the body runs from the second; an empty array gives null.
   */
  case Fold(over: Node, body: Node, init: Option[Node])

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

  /**
   * Program depth and size are bounded ([[Program.MaxDepth]], [[Program.MaxSize]]), so this recursion is bounded too, and
   * its work is linear in the program's size.
   */
  def compile(program: Program): Either[JlvmError, Node] = node(program.expr, TreeSet.empty)

  private def malformed[A](msg: String): Either[JlvmError, A] = JlvmError.Malformed(msg).asLeft

  /**
   * `lets` holds the `let` names in scope. A plain path whose first segment is one of them is rejected: metakit read
   * such a path as the binding, so accepting it would silently read the data instead. Names are chosen by the program,
   * so they are kept in a sorted set, never hashed.
   */
  private def node(e: Expr, lets: TreeSet[String]): Either[JlvmError, Node] =
    e match {
      case Expr.Lit(v) => Const(v).asRight
      case Expr.Var(ref @ VarRef.Data(path), _) if path.headOption.exists(lets.contains) =>
        val n = path.headOption.getOrElse("")
        malformed(
          s"\"${ref.render}\" starts with \"$n\", which let binds: read the binding as \"$$$n\", the data as " +
          s"{\"var\": [[\"$n\", ...]]}, or a callback element's field as \"$$current.$n\""
        )
      case Expr.Var(ref, d) => Read(ref, d).asRight
      case Expr.VarAt(p, d) => node(p, lets).map(ReadAt(_, d))
      case Expr.Spread(op, a) =>
        def notArray(kind: String) = malformed[Node](s"a spread ${op.wire} takes an array, and its argument is always of kind $kind")
        node(a, lets).flatMap {
          case n @ Const(CArray(_)) => Spread(op, n).asRight
          case Const(v)             => notArray(Values.kind(v))
          case ObjectOf(_)          => notArray("object")
          case n                    => Spread(op, n).asRight
        }
      case Expr.Arr(items) => items.traverse(node(_, lets)).map(ArrayOf(_))
      case Expr.Obj(fields) =>
        fields.toVector.traverse {
          case (k, x) =>
            (CanonicalValue.text(k).leftMap(_ => JlvmError.Malformed("an object key is not well-formed text")), node(x, lets)).tupled
        }
          .map(fs => ObjectOf(fs.sortWith((a, b) => Values.compareCodePoints(a._1.value, b._1.value) < 0)))
      case Expr.Apply(op, args) => apply(op, args, lets)
    }

  private def sorted[A](fields: Vector[(String, A)]): Vector[(String, A)] =
    fields.sortWith((a, b) => Values.compareCodePoints(a._1, b._1) < 0)

  private def apply(op: Op, args: Vector[Expr], lets: TreeSet[String]): Either[JlvmError, Node] = {
    def node(e: Expr): Either[JlvmError, Node] = Node.node(e, lets)
    def each(kind: Iteration) = args match {
      case Vector(over, body) => (node(over), node(body)).mapN(Each(kind, _, _))
      case _                  => malformed(s"${op.wire} takes [array, callback]")
    }
    def nodes = args.traverse(node)
    op match {
      case Op.If =>
        def branches(ns: Vector[Node]) = ns.grouped(2).collect { case Vector(c, t) => (c, t) }.toVector
        nodes.flatMap {
          case ns if ns.size >= 2 && ns.size % 2 == 0 => Cond(branches(ns), Const(CanonicalValue.CNull)).asRight
          case pairs :+ otherwise if pairs.size >= 2 => Cond(branches(pairs), otherwise).asRight
          case _                                     => malformed("if takes [condition, then, (condition, then)..., (else)]")
        }
      case Op.And                     => nodes.map(Logic(Logical.And, _))
      case Op.Or                      => nodes.map(Logic(Logical.Or, _))
      case Op.Default                 => nodes.map(Logic(Logical.Default, _))
      case Op.Let                     => let(args, lets)
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
          case Vector(over, body)       => (node(over), node(body)).mapN(Fold(_, _, scala.None))
          case Vector(over, body, init) => (node(over), node(body), node(init)).mapN((o, b, i) => Fold(o, b, scala.Some(i)))
          case _                        => malformed("reduce takes [array, callback] or [array, callback, initial]")
        }
      case Op.Missing | Op.MissingSome              => nodes.map(Lookup(op, _))
      case _ if PureOps(op, Vector.empty).isDefined => nodes.map(Strict(op, _))
      case _                                        => nodes.map(Crypto(op, _))
    }
  }

  /** `[[[name, expr], ...], body]`, or `[{name: expr, ...}, body]` with bindings in code-point order of the names. */
  private def let(args: Vector[Expr], lets: TreeSet[String]): Either[JlvmError, Node] = {
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
          val names = bs.map(_._1).sorted
          Either.cond(names.zip(names.drop(1)).forall(_ != _), bs, JlvmError.Malformed("let binds a name twice"))
        }.flatMap { bs =>
          // each binding sees the ones before it, and the body sees all of them: one pass, carrying the scope forward
          bs.foldLeftM((Vector.empty[(String, Node)], lets)) {
            case ((done, scope), (n, x)) => node(x, scope).map(c => (done :+ (n -> c), scope + n))
          }.flatMap { case (compiled, scope) => node(body, scope).map(Bind(compiled, _)) }
        }
      case _ => malformed("let takes [bindings, body]")
    }
  }
}
