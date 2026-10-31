package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{Digest, DomainTag, Hasher, TaggedHash}
import xyz.kd5ujc.models.{jlvm => W}

import io.circe.{Json, JsonObject}
import scodec.bits.ByteVector

/**
 * The JSON authoring surface and the canonical program form (docs/specs/jlvm.md §1).
 *
 * Surface rules:
 *   - a single-key object whose key is an operation is that operation. An array value is its argument list. Any other
 *     value is one argument, except that a [[Op.Variadic]] operation takes the elements of that value's array
 *     (`{"+": {"values": x}}`);
 *   - `{"literal": x}` quotes `x`; `{"object": {...}}` is a map expression with any keys; `{"var": p}` and
 *     `{"var": [p, default]}` read a variable, with `p` a path or an expression computing one;
 *   - a single key starting with `_` is a map expression: no operation's name starts with `_`, so host triggers such
 *     as `{"_result": ...}` read as data;
 *   - any other single key is an error, so whether `{"count": 3}` is code never depends on the operation list, and
 *     typos such as `iff` are rejected;
 *   - an array headed by an operation's name (or `var`) is an error, so text shaped like a call is never data by
 *     accident; objects with any other number of keys are map expressions; arrays are evaluated element-wise; JSON
 *     null is the null literal; `0x`-hex strings are bytes ([[JsonSurface]]).
 *
 * Input is validated by the strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
 * structurally with nulls still visible, so a null field cannot turn a map expression into an operation. [[render]]
 * writes the surface back.
 */
object Parser {

  /** Surface keywords that are not operations. */
  val Literal: String = "literal"
  val Variable: String = "var"
  val ObjectKey: String = "object"

  /** Single keys with this prefix are data: the namespace of host triggers (`_result`, `_emit`, `_spawn`). */
  val DataPrefix: String = "_"

  /**
   * Names that are not operations although they read like one, each with what to write instead: the parse error
   * names it.
   */
  val Removed: scala.collection.immutable.Map[String, String] = scala.collection.immutable.Map(
    "prove_dlog_verify"    -> "use a one-leaf sigma_verify over {\"type\": \"dlog\"} (docs/specs/sigma.md)",
    "prove_dhtuple_verify" -> "use a one-leaf sigma_verify over {\"type\": \"dhtuple\"} (docs/specs/sigma.md)",
    "mpt_verify"           -> "use trie_verify (docs/specs/trie.md)",
    "mpt_prefix_verify"    -> "use trie_prefix_verify (docs/specs/trie.md)",
    "smt_verify"           -> "use trie_verify (docs/specs/trie.md)",
    "noop"                 -> "omit it: it does nothing"
  )

  /** Names that may not head an array, which would read as a call: `["+", 1, 2]`, `["var", "a"]`. */
  def callName(s: String): Boolean = s == Variable || Op.fromWire(s).isDefined || Removed.contains(s)

  private def malformed[A](m: String): Either[JlvmError, A] = JlvmError.Malformed(m).asLeft

  private def value(j: Json): Either[JlvmError, CanonicalValue] = JsonSurface.value(j)

  def parse(json: String): Either[JlvmError, Program] =
    for {
      _    <- CanonicalJson.parse(json).leftMap(canonicalError)
      j    <- io.circe.parser.parse(json).leftMap(e => JlvmError.Malformed(e.message))
      expr <- fromJson(j)
      prog <- Program.from(expr)
    } yield prog

  /** Depth is bounded by the canonical parser that validated the text (256), so this recursion is bounded too. */
  def fromJson(j: Json): Either[JlvmError, Expr] =
    j.fold(
      Expr.Lit(CanonicalValue.CNull).asRight,
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      array,
      fromObject
    )

  /** An array headed by a call's name is rejected, so text shaped like a call is never read as data by accident. */
  private def array(items: Vector[Json]): Either[JlvmError, Expr] =
    items.headOption.flatMap(_.asString).filter(callName) match {
      case Some(name) =>
        val call = Removed.get(name).fold(s"write the call as {\"$name\": [...]}")(hint => s"\"$name\" is not an operation: $hint")
        malformed(
          s"an array may not start with \"$name\", which reads as a call: $call; quote data as {\"literal\": [...]}, or " +
          s"start it with {\"literal\": \"$name\"}"
        )
      case scala.None => elements(items)
    }

  private def elements(items: Vector[Json]): Either[JlvmError, Expr] = items.traverse(fromJson).map(Expr.Arr(_))

  /** A `let`'s `[name, expression]` pairs: a name is never code, so a pair is exempt from the array check. */
  private def letBindings(j: Json): Either[JlvmError, Expr] =
    j.asArray.fold(fromJson(j))(_.traverse(pair => pair.asArray.fold(fromJson(pair))(elements)).map(Expr.Arr(_)))

  private def fromObject(obj: JsonObject): Either[JlvmError, Expr] =
    obj.toList match {
      case (key, arg) :: Nil => single(key, arg)
      case fields            => mapExpr(fields)
    }

  private def mapExpr(fields: List[(String, Json)]): Either[JlvmError, Expr] =
    fields.traverse { case (k, v) => fromJson(v).map(k -> _) }.map(fs => Expr.Obj(fs.toMap))

  private def single(key: String, arg: Json): Either[JlvmError, Expr] =
    key match {
      case Literal  => value(arg).map(Expr.Lit(_))
      case Variable => variable(arg)
      case ObjectKey =>
        arg.asObject.fold(malformed[Expr]("object takes an object: {\"object\": {key: expression, ...}}"))(o => mapExpr(o.toList))
      case data if data.startsWith(DataPrefix) => mapExpr(List(data -> arg))
      case removed if Removed.contains(removed) =>
        malformed(s"\"$removed\" is not an operation: ${Removed.getOrElse(removed, "")}")
      case name =>
        Op.fromWire(name)
          .fold(
            malformed[Expr](s"unknown operation \"$name\": write single-key data as {\"object\": {...}} or {\"literal\": {...}}")
          )(operation(_, arg))
    }

  private def operation(op: Op, arg: Json): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(Vector(bindings, body)) if op == Op.Let =>
        (letBindings(bindings), fromJson(body)).mapN((b, x) => Expr.Apply(op, Vector(b, x)))
      case Some(items)                      => items.traverse(fromJson).map(Expr.Apply(op, _))
      case None if Op.Variadic.contains(op) => fromJson(arg).map(Expr.Spread(op, _))
      case None                             => fromJson(arg).map(a => Expr.Apply(op, Vector(a)))
    }

  /** A path is text or an integer; an object or an array is an expression computing the path. */
  private def path(j: Json, default: Option[CanonicalValue]): Either[JlvmError, Expr] =
    j.asString
      .orElse(j.asNumber.flatMap(_.toBigInt).map(_.toString))
      .map(VarRef.parse(_).map(Expr.Var(_, default)))
      .getOrElse(
        // a literal array here is the path's segments, not code, so it is exempt from the array check
        if (j.isObject) fromJson(j).map(Expr.VarAt(_, default))
        else if (j.isArray) j.asArray.fold(fromJson(j))(elements).map(Expr.VarAt(_, default))
        else malformed("a var path is text, an integer, or an expression computing one")
      )

  private def variable(arg: Json): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(Vector(p))          => path(p, None)
      case Some(Vector(p, default)) => defaultValue(default).flatMap(d => path(p, Some(d).filter(_ != CanonicalValue.CNull)))
      case Some(_)                  => malformed("var takes a path, or [path, default]")
      case None                     => path(arg, None)
    }

  /**
   * A default is a literal value, never evaluated. An object or array default could be mistaken for code, so it is
   * quoted, `{"literal": ...}`.
   */
  private def defaultValue(j: Json): Either[JlvmError, CanonicalValue] =
    j.asObject.map(_.toList) match {
      case Some(List((Literal, x))) => value(x)
      case _ if j.isObject || j.isArray =>
        malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
      case _ => value(j)
    }

  // ------------------------------------------------------------------ the surface, written back

  /**
   * The surface text of a program: [[parse]] reads it back as the same program. A literal holding `0x`-hex text or a
   * non-terminating rational has no surface form (only the canonical form can carry it), and is an error.
   */
  def render(p: Program): Either[JlvmError, Json] = toJson(p.expr)

  private def quoted(v: CanonicalValue): Either[JlvmError, Json] = JsonSurface.json(v).map(j => Json.obj(Literal -> j))

  private def defaultJson(v: CanonicalValue): Either[JlvmError, Json] =
    v match {
      case CanonicalValue.CArray(_) | CanonicalValue.CMap(_) => quoted(v)
      case other                                             => JsonSurface.json(other)
    }

  /** Depth is bounded by [[Program.MaxDepth]], so this recursion is bounded too. */
  private def toJson(e: Expr): Either[JlvmError, Json] =
    e match {
      case Expr.Lit(v @ (CanonicalValue.CArray(_) | CanonicalValue.CMap(_))) => quoted(v)
      case Expr.Lit(v)                                                       => JsonSurface.json(v)
      case Expr.Var(ref, default) =>
        default
          .fold(Json.fromString(ref.render).asRight[JlvmError])(d => defaultJson(d).map(dj => Json.arr(Json.fromString(ref.render), dj)))
          .map(a => Json.obj(Variable -> a))
      case Expr.VarAt(path, default) =>
        // a literal path is quoted, or it would read as a static one; an object path with no default is written bare,
        // so a computed path adds one JSON level, as in the canonical form; a bare array would read as [path, default]
        val pathJson = path match {
          case Expr.Lit(v) => quoted(v)
          case other       => toJson(other)
        }
        (pathJson, default.traverse(defaultJson)).mapN {
          case (pj, scala.None) if pj.isObject => Json.obj(Variable -> pj)
          case (pj, dj)                        => Json.obj(Variable -> Json.fromValues(pj +: dj.toVector))
        }
      case Expr.Apply(Op.Let, Vector(Expr.Arr(pairs), body)) =>
        // binding names are written plainly: letBindings reads them back as names, whatever they spell
        (
          pairs.traverse {
            case Expr.Arr(Vector(n @ Expr.Lit(CanonicalValue.CText(_)), x)) => (toJson(n), toJson(x)).mapN(Json.arr(_, _))
            case other                                                      => toJson(other)
          },
          toJson(body)
        ).mapN((ps, b) => Json.obj(Op.Let.wire -> Json.arr(Json.fromValues(ps), b)))
      case Expr.Apply(op, args) => args.traverse(toJson).map(as => Json.obj(op.wire -> Json.fromValues(as)))
      case Expr.Spread(op, arg) => toJson(arg).map(a => Json.obj(op.wire -> a))
      case Expr.Arr(items)      =>
        // a leading text naming a call is quoted: unquoted, the array would not parse
        items.zipWithIndex.traverse {
          case (Expr.Lit(t @ CanonicalValue.CText(s)), 0) if callName(s) => quoted(t)
          case (x, _)                                                    => toJson(x)
        }.map(Json.fromValues)
      case Expr.Obj(fields) =>
        fields.toList.traverse { case (k, x) => toJson(x).map(k -> _) }.map {
          case List((k, x)) if !k.startsWith(DataPrefix) => Json.obj(ObjectKey -> Json.obj(k -> x))
          case fs                                        => Json.fromFields(fs)
        }
    }

  /**
   * How deep [[render]] nests a program's JSON, counted as the canonical JSON parser counts it (non-empty arrays and
   * objects), so [[Program.from]] can bound it without rendering. It follows [[toJson]] case by case. A worklist of
   * (expression, nesting of the JSON container holding it), so an unchecked tree cannot overflow the stack.
   */
  private[jlvm] def surfaceDepth(e: Expr): Int = {
    @tailrec
    def go(todo: List[(Expr, Int)], deepest: Int): Int =
      todo match {
        case Nil => deepest
        case (x, l) :: tl =>
          val (own, children) = surfaceStep(x, l)
          go(children ::: tl, math.max(deepest, own))
      }
    go(List(e -> 0), 0)
  }

  /** A literal value's nesting: bounded by the value depth (256), so this recursion is bounded too. */
  private def valueDepth(v: CanonicalValue): Int =
    v match {
      case CanonicalValue.CArray(vs) if vs.nonEmpty => 1 + vs.foldLeft(0)((d, x) => math.max(d, valueDepth(x)))
      case CanonicalValue.CMap(m) if m.nonEmpty     => 1 + m.values.foldLeft(0)((d, x) => math.max(d, valueDepth(x)))
      case _                                        => 0
    }

  /** A literal or default as [[toJson]] writes it: quoted, `{"literal": v}`, when it is an array or a map. */
  private def literalDepth(v: CanonicalValue): Int =
    v match {
      case CanonicalValue.CArray(_) | CanonicalValue.CMap(_) => 1 + valueDepth(v)
      case _                                                 => 0
    }

  /** The deepest level one node opens itself at, and its sub-expressions with the level of the container holding each. */
  private def surfaceStep(e: Expr, l: Int): (Int, List[(Expr, Int)]) = {
    def defaults(d: Option[CanonicalValue]) = d.fold(0)(literalDepth)
    e match {
      case Expr.Lit(v)                                       => (l + literalDepth(v), Nil)
      case Expr.Var(_, scala.None)                           => (l + 1, Nil)
      case Expr.Var(_, d)                                    => (l + 2 + defaults(d), Nil)
      case Expr.VarAt(Expr.Lit(v), scala.None)               => (l + 2 + valueDepth(v), Nil)
      case Expr.VarAt(Expr.Lit(v), d)                        => (math.max(l + 3 + valueDepth(v), l + 2 + defaults(d)), Nil)
      case Expr.VarAt(p @ Expr.Arr(_), scala.None)           => (l + 2, List(p -> (l + 2)))
      case Expr.VarAt(p, scala.None)                         => (l + 1, List(p -> (l + 1)))
      case Expr.VarAt(p, d)                                  => (l + 2 + defaults(d), List(p -> (l + 2)))
      case Expr.Apply(Op.Let, Vector(Expr.Arr(pairs), body)) =>
        // the pairs array, and each [name, expression] with its name written plainly
        val steps = pairs.toList.map {
          case Expr.Arr(Vector(Expr.Lit(CanonicalValue.CText(_)), x)) => (l + 4, List(x -> (l + 4)))
          case other                                                  => (l + 3, List(other -> (l + 3)))
        }
        (steps.foldLeft(l + 2)((d, s) => math.max(d, s._1)), (body -> (l + 2)) :: steps.flatMap(_._2))
      case Expr.Apply(_, as) => (if (as.isEmpty) l + 1 else l + 2, as.toList.map(_ -> (l + 2)))
      case Expr.Spread(_, a) => (l + 1, List(a -> (l + 1)))
      case Expr.Arr(items) =>
        items.toList match {
          // a leading text naming a call is quoted
          case Expr.Lit(CanonicalValue.CText(s)) :: rest if callName(s) => (l + 2, rest.map(_ -> (l + 1)))
          case Nil                                                      => (l, Nil)
          case is                                                       => (l + 1, is.map(_ -> (l + 1)))
        }
      case Expr.Obj(fields) =>
        fields.toList match {
          case List((k, x)) if !k.startsWith(DataPrefix) => (l + 2, List(x -> (l + 2)))
          case Nil                                       => (l, Nil)
          case fs                                        => (l + 1, fs.map(_._2 -> (l + 1)))
        }
    }
  }

  // ------------------------------------------------------------------ canonical program form (shared/schemas/jlvm.cddl)

  def toWire(e: Expr): W.JlvmExpr =
    e match {
      case Expr.Lit(CanonicalValue.CNull) => W.JlvmNull()
      case Expr.Lit(v)                    => W.JlvmLit(v)
      case Expr.Var(ref, default)         => W.JlvmVar(ref.render, default)
      case Expr.VarAt(path, default)      => W.JlvmVarAt(toWire(path), default)
      case Expr.Apply(op, args)           => W.JlvmApply(op.wire, args.map(toWire))
      case Expr.Spread(op, arg)           => W.JlvmSpread(op.wire, toWire(arg))
      case Expr.Arr(items)                => W.JlvmArray(items.map(toWire))
      case Expr.Obj(fields)               => W.JlvmMap(fields.map { case (k, v) => k -> toWire(v) })
    }

  def fromWire(w: W.JlvmExpr): Either[JlvmError, Expr] =
    w match {
      case W.JlvmNull()        => Expr.Lit(CanonicalValue.CNull).asRight
      case W.JlvmLit(v)        => Expr.Lit(v).asRight
      case W.JlvmVar(p, d)     => VarRef.parse(p).map(Expr.Var(_, d))
      case W.JlvmVarAt(p, d)   => fromWire(p).map(Expr.VarAt(_, d))
      case W.JlvmApply(op, as) => wireOp(op).flatMap(o => as.traverse(fromWire).map(Expr.Apply(o, _)))
      case W.JlvmSpread(op, a) => (wireOp(op), fromWire(a)).mapN(Expr.Spread(_, _))
      case W.JlvmArray(items)  => items.traverse(fromWire).map(Expr.Arr(_))
      case W.JlvmMap(fields)   => fields.toList.traverse { case (k, v) => fromWire(v).map(k -> _) }.map(fs => Expr.Obj(fs.toMap))
    }

  /** The canonical form names operations by their own names only; aliases are a surface convenience. */
  private def wireOp(name: String): Either[JlvmError, Op] =
    Op.values.find(_.wire == name).toRight(JlvmError.Malformed(s"unknown operation \"$name\""))

  def canonical(p: Program): Either[JlvmError, CanonicalValue] =
    W.JlvmProgram.codec.encode(W.JlvmProgram(1, toWire(p.expr))).leftMap(canonicalError)

  /** A depth bound is a [[JlvmError.Limit]], however it is reached; anything else malformed is [[JlvmError.Malformed]]. */
  private def canonicalError(e: CanonicalError): JlvmError = e match {
    case CanonicalError.DepthExceeded(n) => JlvmError.Limit(s"the program nests deeper than its canonical form allows ($n levels)")
    case other                           => JlvmError.Malformed(other.toString)
  }

  def fromCanonical(v: CanonicalValue): Either[JlvmError, Program] =
    W.JlvmProgram.codec.decoder.decode(v).leftMap(e => JlvmError.Malformed(e.message)).flatMap(w => fromWire(w.expr).flatMap(Program.from))

  def encode(p: Program): Either[JlvmError, ByteVector] = canonical(p).map(Cbor.encode)

  def decode(bytes: ByteVector): Either[JlvmError, Program] =
    Cbor.decode(bytes).leftMap(canonicalError).flatMap(fromCanonical)

  /** A program's identity: the tagged hash of its canonical form. */
  def id[H: Hasher](p: Program): Either[JlvmError, Digest[H]] =
    encode(p).map(TaggedHash[H](DomainTag.literal(ProgramTag)).apply)

  private[kd5ujc] val ProgramTag = "dilf4s/jlvm/program/v1"
}
