package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue.{CArray, CMap, EmptyMap}
import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalValue, DecodeFailure, Preview}
import xyz.kd5ujc.models.{jlvm => W}

/**
 * The canonical program form (shared/schemas/jlvm.cddl) read and written one node at a time, on an explicit stack: the
 * generated codec recurses once per expression, past a normal JVM stack at the depths a form reaches. Each node is
 * read and written by the generated codec itself, its sub-expressions stood in for by the null expression `{}`; they
 * are then read or written in the codec's order and put in place. So what is accepted, what is written and every
 * refusal, with its path, are the generated codec's.
 */
private[jlvm] object CanonicalForm {

  /**
   * A tree folded on an explicit stack, depth first: `open` reads a node, refusing it or giving what `close` needs and
   * its children in order; `close` makes a node's result from its children's. The first refusal in that order ends the
   * fold. One stack entry per level open, so no tree is too deep for it.
   */
  def fold[N, S, R, E](root: N)(open: N => Either[E, (S, Vector[N])])(close: (S, Vector[R]) => Either[E, R]): Either[E, R] = {
    final case class Opened(state: S, rest: List[N], built: Vector[R])
    @tailrec
    def go(top: Opened, up: List[Opened]): Either[E, R] =
      top.rest match {
        case n :: ns =>
          open(n) match {
            case Left(e)              => Left(e)
            case Right((s, children)) => go(Opened(s, children.toList, Vector.empty), top.copy(rest = ns) :: up)
          }
        case Nil =>
          close(top.state, top.built) match {
            case Left(e) => Left(e)
            case Right(r) =>
              up match {
                case Nil     => Right(r)
                case p :: ps => go(p.copy(built = p.built :+ r), ps)
              }
          }
      }
    open(root).flatMap { case (s, children) => go(Opened(s, children.toList, Vector.empty), Nil) }
  }

  private val Null: W.JlvmExpr = W.JlvmNull()

  /** A node's sub-expressions, in the order the generated codec reads and writes them (a map's in its own order). */
  def parts(w: W.JlvmExpr): Vector[W.JlvmExpr] =
    w match {
      case W.JlvmVarAt(p, _)                             => Vector(p)
      case W.JlvmApply(_, as)                            => as
      case W.JlvmSpread(_, a)                            => Vector(a)
      case W.JlvmArray(items)                            => items
      case W.JlvmMap(fields)                             => fields.values
      case W.JlvmNull() | W.JlvmLit(_) | W.JlvmVar(_, _) => Vector.empty
    }

  /** `w` with its sub-expressions, in [[parts]] order, replaced by `xs`. */
  def withParts(w: W.JlvmExpr, xs: Vector[W.JlvmExpr]): W.JlvmExpr =
    w match {
      case W.JlvmVarAt(p, d)   => W.JlvmVarAt(xs.headOption.getOrElse(p), d)
      case W.JlvmApply(op, _)  => W.JlvmApply(op, xs)
      case W.JlvmSpread(op, a) => W.JlvmSpread(op, xs.headOption.getOrElse(a))
      case W.JlvmArray(_)      => W.JlvmArray(xs)
      case W.JlvmMap(fields)   => W.JlvmMap(fields.keys.zip(xs).foldLeft(fields) { case (acc, (k, x)) => acc.updated(k, x) })
      case leaf                => leaf
    }

  // ------------------------------------------------------------------ writing

  /** The canonical form of a program of expression `expr`: `W.JlvmProgram.codec.encode` of it. */
  def write(expr: W.JlvmExpr): Either[CanonicalError, CanonicalValue] =
    fold[W.JlvmExpr, W.JlvmExpr, CanonicalValue, CanonicalError](expr)(w => (w, parts(w)).asRight)(written)
      .flatMap(e => W.JlvmProgram.codec.encode(W.JlvmProgram(1, Null)).flatMap(placed(_, "expr", e)))

  /**
   * One node as the generated codec writes it, after its sub-expressions: its own fields (a map's keys are checked
   * then, as the codec checks them after its values), then their forms put in place, each container depth-checked.
   */
  private def written(w: W.JlvmExpr, forms: Vector[CanonicalValue]): Either[CanonicalError, CanonicalValue] =
    W.JlvmExpr.codec.encode(withParts(w, parts(w).map(_ => Null))).flatMap { shell =>
      w match {
        case W.JlvmVarAt(_, _)  => forms.headOption.fold(shell.asRight[CanonicalError])(placed(shell, "path", _))
        case W.JlvmSpread(_, _) => forms.headOption.fold(shell.asRight[CanonicalError])(placed(shell, "arg", _))
        case W.JlvmApply(_, _)  => CanonicalValue.array(forms).flatMap(placed(shell, "args", _))
        case W.JlvmArray(_)     => CanonicalValue.array(forms).flatMap(placed(shell, "arr", _))
        case W.JlvmMap(fields)  => CanonicalValue.map(fields.keys.zip(forms)).flatMap(placed(shell, "map", _))
        case _                  => shell.asRight
      }
    }

  /** `shell` (a record) with field `key` set to `v`: a form is never null, so this always sets it. */
  private def placed(shell: CanonicalValue, key: String, v: CanonicalValue): Either[CanonicalError, CanonicalValue] =
    shell match {
      case m: CMap => CanonicalValue.text(key).flatMap(CanonicalValue.updated(m, _, v))
      case other   => other.asRight
    }

  // ------------------------------------------------------------------ reading

  /** The expression tree of a program's canonical form, as `W.JlvmProgram.codec.decoder` reads it, with its refusals. */
  def read(form: CanonicalValue): Either[DecodeFailure, W.JlvmExpr] =
    W.JlvmProgram.codec.decoder.decode(hollowed(form, List("expr"))).flatMap { _ =>
      field(form, "expr")
        .toRight(DecodeFailure(Nil, "missing required key \"expr\""))
        .flatMap(e =>
          fold[(CanonicalValue, List[Preview]), W.JlvmExpr, W.JlvmExpr, DecodeFailure](e -> List(Preview.of("expr")))(opened)((w, xs) =>
            withParts(w, xs).asRight
          )
        )
    }

  private val Places = List("path", "arg", "args", "arr", "map")

  /**
   * One node, read by the generated decoder with its sub-expressions hollowed; a refusal gets the node's path from the
   * root (`at`, innermost first). Its sub-expressions follow, each with its own path, in the decoder's order.
   */
  private def opened(at: (CanonicalValue, List[Preview])): Either[DecodeFailure, (W.JlvmExpr, Vector[(CanonicalValue, List[Preview])])] = {
    val (v, path) = at
    def under(key: String): List[Preview] = Preview.of(key) :: path
    def items(key: String): Vector[(CanonicalValue, List[Preview])] =
      field(v, key) match {
        case Some(CArray(xs)) => xs.zipWithIndex.map { case (x, i) => x -> (Preview.of(s"[$i]") :: under(key)) }
        case _                => Vector.empty
      }
    def one(key: String): Vector[(CanonicalValue, List[Preview])] = field(v, key).toVector.map(_ -> under(key))
    W.JlvmExpr.codec.decoder
      .decode(hollowed(v, Places))
      .bimap(
        f => DecodeFailure(path.reverse ::: f.path, f.reason),
        w =>
          w -> (w match {
            case W.JlvmVarAt(_, _)  => one("path")
            case W.JlvmSpread(_, _) => one("arg")
            case W.JlvmApply(_, _)  => items("args")
            case W.JlvmArray(_)     => items("arr")
            case W.JlvmMap(_) =>
              field(v, "map") match {
                case Some(CMap(fields)) => fields.toVector.map { case (k, x) => x -> (Preview.of(k) :: under("map")) }
                case _                  => Vector.empty
              }
            case _ => Vector.empty
          })
      )
  }

  private def field(v: CanonicalValue, key: String): Option[CanonicalValue] =
    v match {
      case CMap(fields) => fields.get(key)
      case _            => scala.None
    }

  /**
   * `v` with each of the fields `keys` that holds sub-expressions in the shape the decoder reads them in (one, an
   * array of them, a map of them) holding `{}` in their place instead. Any other value, and any field in another
   * shape, is left as it is, so the decoder refuses it as it would the original.
   */
  private def hollowed(v: CanonicalValue, keys: List[String]): CanonicalValue =
    v match {
      case m: CMap =>
        keys.foldLeft(m) { (acc, key) =>
          val hollow = acc.entries.get(key).flatMap {
            case _ if key == "path" || key == "arg" || key == "expr" => scala.Some(EmptyMap)
            case CArray(xs) if key == "args" || key == "arr"         => CanonicalValue.array(xs.map(_ => EmptyMap)).toOption
            case CMap(fields) if key == "map"                        => CanonicalValue.map(fields.keys.map(_ -> EmptyMap)).toOption
            case _                                                   => scala.None
          }
          hollow.flatMap(h => CanonicalValue.text(key).flatMap(CanonicalValue.updated(acc, _, h)).toOption).getOrElse(acc)
        }
      case other => other
    }
}
