package xyz.kd5ujc.jlvm

import cats.Monad
import cats.mtl.{Raise, Stateful}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

/** Gas used so far, against the limit; and the lint warnings so far, distinct, at most [[Warning.Max]]. */
final case class Meter(used: Gas, limit: Gas, warnings: Vector[Warning] = Vector.empty) {
  def warn(w: Warning): Meter =
    if (warnings.size >= Warning.Max || warnings.contains(w)) this else copy(warnings = warnings :+ w)
}

/** Where a plain data path resolved, and whether an outer element or the data also holds its first key. */
final case class Resolved(value: CanonicalValue, ambiguous: Boolean)

/**
 * An enclosing callback's contribution to plain paths: the element, or in `reduce` the pair `{current, accumulator}`
 * that metakit merged into the data. Frames are not values, so building one can never fail a bound.
 */
enum Frame {
  case Element(value: CanonicalValue)
  case Accumulating(current: CanonicalValue, accumulator: CanonicalValue)

  /** What `""` reads: the element. */
  def element: CanonicalValue = this match {
    case Element(v)         => v
    case Accumulating(c, _) => c
  }

  /** The value this frame binds to `key`, if it binds it. */
  def lookup(key: String): Option[CanonicalValue] = this match {
    case Element(CMap(m))                           => m.get(key)
    case Element(_)                                 => scala.None
    case Accumulating(c, _) if key == "current"     => Option(c)
    case Accumulating(_, a) if key == "accumulator" => Option(a)
    case Accumulating(_, _)                         => scala.None
  }
}

/**
 * What a node sees: the input data, the `$` bindings (host bindings, then `let` and callback bindings), and inside
 * callbacks the enclosing frames, innermost first (docs/specs/jlvm.md §4.1).
 */
final case class Scope(data: CanonicalValue, bindings: Map[String, CanonicalValue], frames: List[Frame] = Nil) {
  def bind(entries: (String, CanonicalValue)*): Scope = copy(bindings = bindings ++ entries)

  def enter(frame: Frame): Scope = copy(frames = frame :: frames)

  /**
   * A plain data path. `""` is the innermost element, or the data outside callbacks. Otherwise the path is read from
   * the innermost frame binding its first key, else from the data: metakit's merged scope, with the element's keys
   * shadowing the data's.
   */
  def resolve(path: Vector[String]): Resolved =
    path match {
      case key +: rest =>
        frames.flatMap(_.lookup(key).toList) match {
          case inner :: outer => Resolved(Values.at(inner, rest), outer.nonEmpty || Values.at(data, Vector(key)) != CNull)
          case Nil            => Resolved(Values.at(data, path), ambiguous = false)
        }
      case _ => Resolved(frames.headOption.fold(data)(_.element), ambiguous = false)
    }
}

/**
 * One step of evaluation, as data. The semantics ([[Semantics]]) says what each node does in terms of these steps; the
 * two drivers ([[Machine.stackMachine]], [[Machine.recursive]]) only decide how to run them, so they cannot disagree
 * on values, errors or gas.
 */
enum Step {
  case Done(value: CanonicalValue)
  case Fail(error: JlvmError)

  /** Pay `amount`, then continue. `next` is not run unless the charge succeeds: no work happens before it is paid for. */
  case Charge(amount: Gas, next: () => Step)

  /** Evaluate `node` in `scope`, then continue with its value. */
  case Eval(node: Node, scope: Scope, k: CanonicalValue => Step)

  /** Record a lint warning, then continue. Costs nothing. */
  case Note(warning: Warning, next: () => Step)
}

/** The evaluation rules (docs/specs/jlvm.md §Evaluation), parameterised by a gas schedule. */
final class Semantics(schedule: GasSchedule, crypto: CryptoOps) {

  import Node._
  import Step._

  private def result(r: Either[JlvmError, CanonicalValue]): Step = r.fold(Fail(_), Done(_))

  /** The size charge of an operation on its evaluated arguments (taken after the arguments, before the work). */
  private def sized(op: Op, args: Vector[CanonicalValue]): Gas = {
    val cost = schedule.cost(op)
    val units = cost.unit match {
      case SizeUnit.None     => 0L
      case SizeUnit.Elements => args.foldLeft(0L)((n, v) => sat(n, Values.elements(v)))
      case SizeUnit.Weight   => args.foldLeft(0L)((n, v) => sat(n, v.weight))
    }
    cost.perUnit * units
  }

  private def sat(a: Long, b: Long): Long = if (a > Long.MaxValue - b) Long.MaxValue else a + b

  private def base(op: Op): Gas = schedule.cost(op).base

  /** Evaluates `nodes` left to right, then continues with all their values. */
  private def all(nodes: Vector[Node], scope: Scope)(k: Vector[CanonicalValue] => Step): Step = {
    def go(i: Int, acc: Vector[CanonicalValue]): Step =
      if (i >= nodes.size) k(acc) else Eval(nodes(i), scope, v => go(i + 1, acc :+ v))
    go(0, Vector.empty)
  }

  def start(node: Node, scope: Scope): Step =
    node match {
      case Const(v) => Charge(schedule.literal, () => Done(v))

      case Read(ref, default) => read(ref, default, scope)

      case ReadAt(path, default) =>
        Eval(
          path,
          scope,
          {
            case CText(t)   => VarRef.parse(t).fold(e => Fail(JlvmError.Domain("var", e.message)), read(_, default, scope))
            case CArray(ss) => ss.traverse(segment).fold(Fail(_), p => read(VarRef.Data(p), default, scope))
            case other      => Fail(JlvmError.Type("var", s"a computed path is text or an array of segments, got ${Values.kind(other)}"))
          }
        )

      case ArrayOf(items) =>
        Charge(schedule.literal + schedule.element * items.size.toLong, () => all(items, scope)(vs => result(Values.array("array", vs))))

      case ObjectOf(fields) =>
        Charge(
          schedule.literal + schedule.element * fields.size.toLong,
          () => all(fields.map(_._2), scope)(vs => result(Values.map("object", fields.map(_._1).zip(vs))))
        )

      case Strict(op, args) =>
        Charge(
          base(op),
          () =>
            all(args, scope) { vs =>
              Charge(sized(op, vs), () => PureOps(op, vs).fold(Fail(JlvmError.Malformed(s"${op.wire} has no strict form")))(result))
            }
        )

      case Spread(op, arg) =>
        Charge(
          base(op),
          () =>
            Eval(
              arg,
              scope,
              {
                case CArray(vs) =>
                  Charge(sized(op, vs), () => PureOps(op, vs).fold(Fail(JlvmError.Malformed(s"${op.wire} has no strict form")))(result))
                case other => Fail(JlvmError.Type(op.wire, s"a spread argument is an array, got ${Values.kind(other)}"))
              }
            )
        )

      case Cond(branches, otherwise) => Charge(base(Op.If), () => cond(branches, otherwise, scope))

      case Logic(kind, args) => Charge(base(kind.op), () => logic(kind, args, scope, 0))

      case Bind(bindings, body) => Charge(base(Op.Let), () => bind(bindings, body, scope))

      case Each(kind, over, body) =>
        Charge(base(kind.op), () => collection(kind.op, over, scope)(vs => iterate(kind, vs, body, scope)))

      case Quorum(over, body, min) =>
        Charge(
          base(Op.Some),
          () =>
            collection(Op.Some, over, scope) { vs =>
              min.fold(quorum(vs, body, scope, 1L, 0, 0L)) { m =>
                Eval(
                  m,
                  scope,
                  mv =>
                    Values
                      .integer(Op.Some, mv)
                      .fold(Fail(_), n => quorum(vs, body, scope, n.max(BigInt(0)).min(BigInt(Long.MaxValue)).toLong, 0, 0L))
                )
              }
            }
        )

      case Fold(over, body, init) =>
        Charge(base(Op.Reduce), () => collection(Op.Reduce, over, scope)(vs => Eval(init, scope, acc => fold(vs, body, scope, 0, acc))))

      case Lookup(op, args) =>
        Charge(base(op), () => all(args, scope)(vs => Charge(sized(op, vs), () => result(Lookups(op, vs, p => scope.resolve(p).value)))))

      case Crypto(op, args) =>
        Charge(
          base(op),
          () =>
            all(args, scope) { vs =>
              Charge(sized(op, vs), () => crypto(op, vs).fold(Fail(JlvmError.Crypto(op.wire, "not available yet")))(result))
            }
        )
    }

  // ---- variables

  /**
   * Charges for the access and each segment (a binding name counts as one), then reads. A plain path inside callbacks
   * may consult every enclosing frame, so each frame is charged as a segment too.
   */
  private def read(ref: VarRef, default: Option[CanonicalValue], scope: Scope): Step = {
    def orDefault(v: CanonicalValue): CanonicalValue = if (v == CNull) default.getOrElse(CNull) else v
    val segments = ref match {
      case VarRef.Data(p) if p.nonEmpty => p.size.toLong + scope.frames.size
      case VarRef.Data(_)               => 0L
      case VarRef.Bound(_, p)           => 1L + p.size
    }
    Charge(
      schedule.varAccess + schedule.perPathSegment * segments,
      () =>
        ref match {
          case VarRef.Data(path) =>
            val r = scope.resolve(path)
            if (r.ambiguous) Note(Warning.AmbiguousPath(ref.render), () => Done(orDefault(r.value))) else Done(orDefault(r.value))
          case VarRef.Bound(name, path) =>
            scope.bindings.get(name) match {
              case scala.Some(v) => Done(orDefault(Values.at(v, path)))
              case scala.None    => default.fold(Fail(JlvmError.Malformed(s"$$$name is not bound")))(Done(_))
            }
        }
    )
  }

  /** A segment of a computed data path: text is an exact key (it may contain `.`); an integer is an index. */
  private def segment(v: CanonicalValue): Either[JlvmError, String] =
    v match {
      case CText(t) => t.asRight
      case CInt(i)  => i.toString.asRight
      case other    => JlvmError.Type("var", s"a path segment is text or an integer, got ${Values.kind(other)}").asLeft
    }

  // ---- lazy forms

  private def cond(branches: Vector[(Node, Node)], otherwise: Node, scope: Scope): Step =
    branches match {
      case (c, t) +: rest =>
        Eval(c, scope, v => if (Values.truthy(v)) Eval(t, scope, Done(_)) else cond(rest, otherwise, scope))
      case _ => Eval(otherwise, scope, Done(_))
    }

  /** `and` stops at the first falsy value, `or` and `default` at the first truthy one. */
  private def logic(kind: Logical, args: Vector[Node], scope: Scope, i: Int): Step =
    if (i >= args.size)
      Done(kind match {
        case Logical.And     => True
        case Logical.Or      => False
        case Logical.Default => CNull
      })
    else
      Eval(
        args(i),
        scope,
        v => {
          val decisive = kind match {
            case Logical.And => !Values.truthy(v)
            case _           => Values.truthy(v)
          }
          val last = i == args.size - 1
          if (decisive || (last && kind != Logical.Default)) Done(v) else logic(kind, args, scope, i + 1)
        }
      )

  private def bind(bindings: Vector[(String, Node)], body: Node, scope: Scope): Step =
    bindings match {
      case (name, x) +: rest => Eval(x, scope, v => bind(rest, body, scope.bind(name -> v)))
      case _                 => Eval(body, scope, Done(_))
    }

  // ---- callbacks

  /** Evaluates the collection; null (absent) is empty, as in metakit. Charges per element before any body runs. */
  private def collection(op: Op, over: Node, scope: Scope)(k: Vector[CanonicalValue] => Step): Step =
    Eval(
      over,
      scope,
      {
        case CArray(vs) => Charge(schedule.cost(op).perUnit * vs.size.toLong, () => k(vs))
        case CNull      => k(Vector.empty)
        case other      => Fail(JlvmError.Type(op.wire, s"expected an array, got ${Values.kind(other)}"))
      }
    )

  /** The element is bound as `$current` and is the innermost frame for plain paths. */
  private def element(scope: Scope, vs: Vector[CanonicalValue], i: Int): Scope =
    scope.bind("current" -> vs(i), "index" -> Values.count(i.toLong)).enter(Frame.Element(vs(i)))

  private def iterate(kind: Iteration, vs: Vector[CanonicalValue], body: Node, scope: Scope): Step = {
    def go(i: Int, acc: Vector[CanonicalValue], n: Long): Step =
      if (i >= vs.size)
        kind match {
          case Iteration.Map    => result(Values.array(kind.op, acc))
          case Iteration.Filter => result(Values.array(kind.op, acc))
          case Iteration.All    => Done(Values.bool(vs.nonEmpty))
          case Iteration.None   => Done(True)
          case Iteration.Find   => Done(CNull)
          case Iteration.Count  => Done(Values.count(n))
        }
      else
        Charge(
          schedule.callbackRun,
          () =>
            Eval(
              body,
              element(scope, vs, i),
              r => {
                val t = Values.truthy(r)
                kind match {
                  case Iteration.Map                                   => go(i + 1, acc :+ r, n)
                  case Iteration.Filter                                => go(i + 1, if (t) acc :+ vs(i) else acc, n)
                  case Iteration.All if !t                             => Done(False)
                  case Iteration.None if t                             => Done(False)
                  case Iteration.Find if t                             => Done(vs(i))
                  case Iteration.Count                                 => go(i + 1, acc, if (t) n + 1 else n)
                  case Iteration.All | Iteration.None | Iteration.Find => go(i + 1, acc, n)
                }
              }
            )
        )
    go(0, Vector.empty, 0L)
  }

  /** Stops as soon as `min` elements satisfy the body; `min` of 0 holds even for an empty array. */
  private def quorum(vs: Vector[CanonicalValue], body: Node, scope: Scope, min: Long, i: Int, n: Long): Step =
    if (n >= min) Done(True)
    else if (i >= vs.size) Done(False)
    else
      Charge(
        schedule.callbackRun,
        () => Eval(body, element(scope, vs, i), r => quorum(vs, body, scope, min, i + 1, if (Values.truthy(r)) n + 1 else n))
      )

  private def accumulating(scope: Scope, vs: Vector[CanonicalValue], i: Int, acc: CanonicalValue): Scope =
    scope.bind("current" -> vs(i), "index" -> Values.count(i.toLong), "accumulator" -> acc).enter(Frame.Accumulating(vs(i), acc))

  private def fold(vs: Vector[CanonicalValue], body: Node, scope: Scope, i: Int, acc: CanonicalValue): Step =
    if (i >= vs.size) Done(acc)
    else
      Charge(
        schedule.callbackRun,
        () => Eval(body, accumulating(scope, vs, i, acc), next => fold(vs, body, scope, i + 1, next))
      )
}

object Machine {

  import Step._

  private def charge[F[_]: Monad](amount: Gas)(using S: Stateful[F, Meter], R: Raise[F, JlvmError]): F[Unit] =
    S.get.flatMap { m =>
      val total = m.used + amount
      if (total <= m.limit) S.set(m.copy(used = total))
      else S.set(m.copy(used = m.limit)) >> R.raise(JlvmError.OutOfGas(total.value, m.limit.value))
    }

  /** An explicit continuation stack, run by `tailRecM`: constant host stack whatever the program. */
  def stackMachine[F[_]: Monad](semantics: Semantics, first: Step)(using S: Stateful[F, Meter], R: Raise[F, JlvmError]): F[CanonicalValue] =
    (first, List.empty[CanonicalValue => Step]).tailRecM[F, CanonicalValue] {
      case (Done(v), Nil)        => v.asRight[(Step, List[CanonicalValue => Step])].pure[F]
      case (Done(v), k :: ks)    => (k(v), ks).asLeft[CanonicalValue].pure[F]
      case (Fail(e), _)          => R.raise(e)
      case (Charge(g, next), ks) => charge[F](g).map(_ => (next(), ks).asLeft[CanonicalValue])
      case (Eval(n, s, k), ks)   => (semantics.start(n, s), k :: ks).asLeft[CanonicalValue].pure[F]
      case (Note(w, next), ks)   => S.modify(_.warn(w)).map(_ => (next(), ks).asLeft[CanonicalValue])
    }

  /** The reference driver: each node is a nested computation in `F`, mirroring the program's structure. */
  def recursive[F[_]: Monad](semantics: Semantics, step: Step)(using S: Stateful[F, Meter], R: Raise[F, JlvmError]): F[CanonicalValue] =
    step match {
      case Done(v)         => v.pure[F]
      case Fail(e)         => R.raise(e)
      case Charge(g, next) => charge[F](g) >> recursive(semantics, next())
      case Eval(n, s, k)   => recursive(semantics, semantics.start(n, s)).flatMap(v => recursive(semantics, k(v)))
      case Note(w, next)   => S.modify(_.warn(w)) >> recursive(semantics, next())
    }
}
