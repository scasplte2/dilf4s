package xyz.kd5ujc.jlvm

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{CanonicalValue, Preview}

/**
 * The operations of language version 1, by wire name: generated from `shared/schemas/jlvm.cddl` (`jlvm-op`), each
 * member named by its rule and carrying its `wire` name, with `values` and `fromWire` (own names only). A program naming
 * anything else does not parse, so adding an operation in a later version never changes what an existing program means.
 */
export xyz.kd5ujc.models.jlvm.{JlvmOp => Op}

/**
 * Where an operation-level error arose (docs/specs/jlvm.md §3): an operation, or `"var"`, the read of a computed path.
 * It is the error's normative `op` field, generated from `shared/schemas/jlvm-errors.cddl` (`jlvm-site`), so it is
 * always one of these names, never caller text.
 */
type Site = xyz.kd5ujc.models.jlvm_errors.JlvmSite

object Site {

  /** The `var` read of a computed path. */
  val Var: Site = xyz.kd5ujc.models.jlvm_errors.JlvmSite.Var

  /** Every site: each operation, then `var`. */
  val values: Vector[Site] = xyz.kd5ujc.models.jlvm_errors.JlvmSite.values
}

/** A site's name as the vectors record it: the operation's wire name, or `var`. */
extension (site: Site) {
  def wire: String = site match {
    case op: Op                                     => op.wire
    case xyz.kd5ujc.models.jlvm_errors.JlvmSite.Var => "var"
  }
}

/** What the evaluator knows of the operations beyond their names. */
object Ops {

  /**
   * Other names for operations, accepted by the parser and normalised to the operation's own name, so they never
   * change a program's identity. They are the names the SDK and documentation apps use.
   */
  val Aliases: scala.collection.immutable.Map[String, Op] =
    scala.collection.immutable.Map("getKey" -> Op.Get, "setKey" -> Op.Set, "deleteKey" -> Op.Unset, "size" -> Op.Length)

  /** The operation a surface name denotes: its own name, or an alias. */
  def named(name: String): Option[Op] = Op.fromWire(name).orElse(Aliases.get(name))

  /**
   * The operations written with a single non-array argument to take that value's elements as their arguments
   * (`{"+": {"values": x}}`). Whether an operation spreads is fixed by the syntax, never by an evaluated value.
   */
  val Variadic: scala.collection.immutable.Set[Op] = scala.collection.immutable.Set(Op.Add, Op.Mul, Op.Min, Op.Max, Op.Cat, Op.Merge)
}

/** A `var` path: `$name.path` reads a binding (host, `let` or callback); any other path reads the data (§4.1). */
enum VarRef {
  case Data(path: Vector[String])
  case Bound(name: String, path: Vector[String])

  def render: String = this match {
    case Data(p)     => p.mkString(".")
    case Bound(n, p) => (("$" + n) +: p).mkString(".")
  }
}

object VarRef {

  /** The `.`-separated segments of `s`, empty ones included: one pass, without a regular expression. */
  def segments(s: String): Vector[String] = {
    @tailrec
    def go(from: Int, acc: Vector[String]): Vector[String] =
      s.indexOf('.', from) match {
        case -1 => acc :+ s.substring(from)
        case at => go(at + 1, acc :+ s.substring(from, at))
      }
    go(0, Vector.empty)
  }

  def parse(s: String): Either[JlvmError, VarRef] =
    if (s.isEmpty) Right(Data(Vector.empty))
    else {
      val segments = VarRef.segments(s)
      if (segments.exists(_.isEmpty)) Left(JlvmError.Malformed(s"empty segment in var path ${Preview.of(s)}"))
      else
        segments.headOption match {
          case scala.Some(h) if h.startsWith("$") =>
            val name = h.drop(1)
            Either.cond(name.nonEmpty, Bound(name, segments.drop(1)), JlvmError.Malformed(s"empty binding name in ${Preview.of(s)}"))
          case _ => Right(Data(segments))
        }
    }
}

/** The syntax tree of a program (language version 1). */
enum Expr {
  case Lit(value: CanonicalValue)
  case Var(ref: VarRef, default: Option[CanonicalValue])

  /** A path computed at run time: text is read as a `var` path; an array is the exact segments of a data path. */
  case VarAt(path: Expr, default: Option[CanonicalValue])
  case Apply(op: Op, args: Vector[Expr])

  /** A [[Ops.Variadic]] operation whose arguments are the elements of one array value. */
  case Spread(op: Op, arg: Expr)
  case Arr(items: Vector[Expr])

  /** Its keys are the program's own, so they are ordered, never hashed. */
  case Obj(fields: scala.collection.immutable.SortedMap[String, Expr])
}

/**
 * A program: language version 1 and its expression, nested at most [[Program.MaxDepth]] deep. The bound is checked
 * once, here, so everything that walks a program (compilation, hashing, rendering) may recurse on its structure.
 *
 * Not a case class: a case class has a product mirror whose `fromProduct` builds an instance around its private
 * constructor, so [[Program.from]] would not be the only way in.
 */
final class Program private (val expr: Expr) {
  def version: Int = Program.Version

  override def equals(that: Any): Boolean = that match {
    case p: Program => p.expr == expr
    case _          => false
  }
  override def hashCode: Int = expr.hashCode
  override def toString: String = s"Program($expr)"
}

object Program {
  val Version: Int = 1

  /** Expression nesting: an operation's arguments, an array's items and a map's fields are one level deeper. */
  val MaxDepth: Int = xyz.kd5ujc.models.limits.JlvmMaxProgramDepth

  /** JSON nesting of the rendered program ([[Parser.render]]), as the canonical JSON parser bounds it. */
  val MaxSurfaceDepth: Int = CanonicalValue.MaxDepth

  /**
   * Checks the depth bound and the shapes no surface text can produce, so every program has exactly one surface form:
   * a spread names a [[Ops.Variadic]] operation and its argument is not an array expression (that is an argument list).
   * Checks that the canonical form decodes to this tree, so programs that behave differently never share an identity:
   * a `var` path is what its text reads back as, and no default is null (the canonical form drops null). Then requires
   * the rendered surface and the canonical form to fit the depth bound, so every program has an identity and an exact
   * rendering: an operation takes two canonical levels, so about 127 nest, fewer around deep literals and defaults.
   *
   * Literal byte strings are expected to be arrays or slices of one, as the parser makes them (see `Jlvm.evaluate`).
   */
  def from(expr: Expr): Either[JlvmError, Program] = from(expr, Kernels.Default)

  /** [[from]], telling `kernels` of each expression each pass visits: production passes the default. */
  private[jlvm] def from(expr: Expr, kernels: Kernels): Either[JlvmError, Program] =
    for {
      _ <- measure(List(Pending(Vector(expr), 0, 0)), 0L, kernels).toLeft(())
      _ <- check(List(expr), kernels).toLeft(())
      surface = Parser.surfaceDepth(expr, kernels)
      _ <- Either.cond(
        surface <= MaxSurfaceDepth,
        (),
        JlvmError.Limit(s"the program's JSON surface nests $surface deep, deeper than $MaxSurfaceDepth")
      )
      program = new Program(expr)
      _ <- Parser.canonical(program)
    } yield program

  private def unspellable(ref: VarRef): JlvmError =
    JlvmError.Malformed(
      s"the var path ${Preview.of(ref.render)} has no text form: it reads back as another path; read a key holding '.', " +
      "an empty key or a key starting with '$' with the exact-segment form {\"var\": [[...]]}"
    )

  /**
   * The largest program, in size: one per expression, plus the weight of each literal and default, plus the length of
   * each object key, binding name and path segment, and one per segment. Compilation is linear in the size and uses no gas, so the bound is
   * checked first, by a walk that stops as soon as it is exceeded. The largest conformance programs are under 2^14.
   */
  val MaxSize: Long = xyz.kd5ujc.models.limits.JlvmMaxProgramSize

  /** A path's segments, each with its length. */
  private def segments(p: Vector[String]): Long = p.foldLeft(0L)((n, s) => n + 1L + s.length)

  /** Saturating: a literal's weight may itself have saturated. */
  private def plus(a: Long, b: Long): Long = if (a > Long.MaxValue - b) Long.MaxValue else a + b

  private def size(e: Expr): Long =
    e match {
      case Expr.Lit(v)                                        => plus(1L, v.weight)
      case Expr.Var(VarRef.Data(p), d)                        => plus(1L + segments(p), d.fold(0L)(_.weight))
      case Expr.Var(VarRef.Bound(n, p), d)                    => plus(2L + n.length + segments(p), d.fold(0L)(_.weight))
      case Expr.VarAt(_, d)                                   => plus(1L, d.fold(0L)(_.weight))
      case Expr.Obj(fs)                                       => fs.foldLeft(1L) { case (n, (k, _)) => n + 1L + k.length }
      case Expr.Arr(_) | Expr.Apply(_, _) | Expr.Spread(_, _) => 1L
    }

  /** The children of `items` still to visit, from `next`, each at `depth`. */
  private final case class Pending(items: Vector[Expr], next: Int, depth: Int)

  private def children(e: Expr): Vector[Expr] =
    e match {
      case Expr.Apply(_, as)            => as
      case Expr.Arr(is)                 => is
      case Expr.Obj(fs)                 => fs.values.toVector
      case Expr.Spread(_, a)            => Vector(a)
      case Expr.VarAt(p, _)             => Vector(p)
      case Expr.Lit(_) | Expr.Var(_, _) => Vector.empty
    }

  /**
   * The size and depth bounds, before anything else reads the tree: each node counts as it is reached, and arguments
   * are visited where they are, never copied into the worklist, so the walk stops within the bound whatever the tree.
   */
  @tailrec
  private def measure(todo: List[Pending], seen: Long, kernels: Kernels): Option[JlvmError] =
    todo match {
      case _ if seen > MaxSize => scala.Some(JlvmError.Limit(s"the program is larger than $MaxSize"))
      case Nil                 => scala.None
      case Pending(items, i, d) :: tl =>
        items.lift(i) match {
          case scala.None                    => measure(tl, seen, kernels)
          case scala.Some(_) if d > MaxDepth => scala.Some(JlvmError.Limit(s"the program nests deeper than $MaxDepth"))
          case scala.Some(e) =>
            kernels.steps.step()
            measure(Pending(children(e), 0, d + 1) :: Pending(items, i + 1, d) :: tl, plus(seen, size(e)), kernels)
        }
    }

  /**
   * The shapes no surface text can produce. Runs once [[measure]] has bounded the tree, so rendering every path costs
   * at most the size. A worklist walk, so that checking a deep tree cannot itself overflow the stack.
   */
  @tailrec
  private def check(todo: List[Expr], kernels: Kernels): Option[JlvmError] =
    todo match {
      case Nil => scala.None
      case Expr.Spread(op, _) :: _ if !Ops.Variadic.contains(op) =>
        scala.Some(JlvmError.Malformed(s"${op.wire} does not spread its argument"))
      case Expr.Spread(op, Expr.Arr(_)) :: _ =>
        scala.Some(JlvmError.Malformed(s"a spread ${op.wire} takes one expression, not an argument list"))
      case (Expr.Var(_, scala.Some(CanonicalValue.CNull)) | Expr.VarAt(_, scala.Some(CanonicalValue.CNull))) :: _ =>
        scala.Some(JlvmError.Malformed("a var default is never null: the canonical form cannot tell it from no default"))
      case Expr.Var(ref, _) :: _ if !VarRef.parse(ref.render).exists(_ == ref) => scala.Some(unspellable(ref))
      case e :: tl =>
        kernels.steps.step()
        check(children(e).toList ::: tl, kernels)
    }
}
