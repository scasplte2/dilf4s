package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue, Cbor}
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
 *   - any other single key is an error ([[Mode.Strict]], the default), so whether `{"count": 3}` is code never depends
 *     on the operation list, and typos such as `iff` are rejected. [[Mode.Lenient]] (opt-in, for migrating older
 *     programs) reads it as a map expression instead;
 *   - objects with any other number of keys are map expressions; arrays are evaluated element-wise; JSON null is the
 *     null literal; `0x`-hex strings are bytes ([[JsonSurface]]).
 *
 * Input is validated by the strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
 * structurally with nulls still visible, so a null field cannot turn a map expression into an operation.
 * [[render]] writes the strict surface back; [[migrate]] rewrites an older (lenient) program into it.
 */
object Parser {

  enum Mode {

    /** Every single-key object is an operation; unknown names are errors. The default. */
    case Strict

    /** Unknown single-key objects are map expressions (metakit's reading), for migrating older programs. */
    case Lenient
  }

  /** Surface keywords that are not operations. */
  val Literal: String = "literal"
  val Variable: String = "var"
  val ObjectKey: String = "object"

  /** Single keys with this prefix are data: the namespace of host triggers (`_result`, `_emit`, `_spawn`). */
  val DataPrefix: String = "_"

  private def malformed[A](m: String): Either[JlvmError, A] = JlvmError.Malformed(m).asLeft

  private def value(j: Json): Either[JlvmError, CanonicalValue] = JsonSurface.value(j)

  def parse(json: String, mode: Mode = Mode.Strict): Either[JlvmError, Program] =
    for {
      _    <- CanonicalJson.parse(json).leftMap(e => JlvmError.Malformed(e.toString))
      j    <- io.circe.parser.parse(json).leftMap(e => JlvmError.Malformed(e.message))
      expr <- fromJson(j, mode)
      prog <- Program.from(expr)
    } yield prog

  /** Depth is bounded by the canonical parser that validated the text (256), so this recursion is bounded too. */
  def fromJson(j: Json, mode: Mode = Mode.Strict): Either[JlvmError, Expr] =
    j.fold(
      Expr.Lit(CanonicalValue.CNull).asRight,
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      items => items.traverse(fromJson(_, mode)).map(Expr.Arr(_)),
      obj => fromObject(obj, mode)
    )

  private def fromObject(obj: JsonObject, mode: Mode): Either[JlvmError, Expr] =
    obj.toList match {
      case (key, arg) :: Nil => single(key, arg, mode)
      case fields            => mapExpr(fields, mode)
    }

  private def mapExpr(fields: List[(String, Json)], mode: Mode): Either[JlvmError, Expr] =
    fields.traverse { case (k, v) => fromJson(v, mode).map(k -> _) }.map(fs => Expr.Obj(fs.toMap))

  private def single(key: String, arg: Json, mode: Mode): Either[JlvmError, Expr] =
    key match {
      case Literal  => value(arg).map(Expr.Lit(_))
      case Variable => variable(arg, mode)
      case ObjectKey =>
        arg.asObject.fold(malformed[Expr]("object takes an object: {\"object\": {key: expression, ...}}"))(o => mapExpr(o.toList, mode))
      case data if data.startsWith(DataPrefix) => mapExpr(List(data -> arg), mode)
      case name =>
        (Op.fromWire(name), mode) match {
          case (Some(op), _)        => operation(op, arg, mode)
          case (None, Mode.Lenient) => mapExpr(List(name -> arg), mode)
          case (None, Mode.Strict) =>
            malformed(s"unknown operation \"$name\" (strict mode; write single-key data as {\"object\": ...} or {\"literal\": ...})")
        }
    }

  private def operation(op: Op, arg: Json, mode: Mode): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(items)                      => items.traverse(fromJson(_, mode)).map(Expr.Apply(op, _))
      case None if Op.Variadic.contains(op) => fromJson(arg, mode).map(Expr.Spread(op, _))
      case None                             => fromJson(arg, mode).map(a => Expr.Apply(op, Vector(a)))
    }

  /** A path is text or an integer; an object or an array is an expression computing the path. */
  private def path(j: Json, default: Option[CanonicalValue], mode: Mode): Either[JlvmError, Expr] =
    j.asString
      .orElse(j.asNumber.flatMap(_.toBigInt).map(_.toString))
      .map(VarRef.parse(_).map(Expr.Var(_, default)))
      .getOrElse(
        if (j.isObject || j.isArray) fromJson(j, mode).map(Expr.VarAt(_, default))
        else malformed("a var path is text, an integer, or an expression computing one")
      )

  private def variable(arg: Json, mode: Mode): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(Vector(p))          => path(p, None, mode)
      case Some(Vector(p, default)) => value(default).flatMap(d => path(p, Some(d).filter(_ != CanonicalValue.CNull), mode))
      case Some(_)                  => malformed("var takes a path, or [path, default]")
      case None                     => path(arg, None, mode)
    }

  // ------------------------------------------------------------------ the strict surface, written back

  /**
   * The strict surface text of a program: [[parse]] reads it back as the same program. A literal holding `0x`-hex text
   * or a non-terminating rational has no surface form (only the canonical form can carry it), and is an error.
   */
  def render(p: Program): Either[JlvmError, Json] = toJson(p.expr)

  /** Reads an older program as metakit did ([[Mode.Lenient]]) and writes it in the strict surface. */
  def migrate(json: String): Either[JlvmError, String] =
    parse(json, Mode.Lenient).flatMap(render).map(_.noSpaces)

  private def quoted(v: CanonicalValue): Either[JlvmError, Json] = JsonSurface.json(v).map(j => Json.obj(Literal -> j))

  /** Depth is bounded by [[Program.MaxDepth]], so this recursion is bounded too. */
  private def toJson(e: Expr): Either[JlvmError, Json] =
    e match {
      case Expr.Lit(v @ (CanonicalValue.CArray(_) | CanonicalValue.CMap(_))) => quoted(v)
      case Expr.Lit(v)                                                       => JsonSurface.json(v)
      case Expr.Var(ref, default) =>
        default
          .fold(Json.fromString(ref.render).asRight[JlvmError])(d =>
            JsonSurface.json(d).map(dj => Json.arr(Json.fromString(ref.render), dj))
          )
          .map(a => Json.obj(Variable -> a))
      case Expr.VarAt(path, default) =>
        // always the array form: a bare array would read as [path, default], and a bare literal path as a static one
        val pathJson = path match {
          case Expr.Lit(v) => quoted(v)
          case other       => toJson(other)
        }
        (pathJson, default.traverse(JsonSurface.json)).mapN((pj, dj) => Json.obj(Variable -> Json.fromValues(pj +: dj.toVector)))
      case Expr.Apply(op, args) => args.traverse(toJson).map(as => Json.obj(op.wire -> Json.fromValues(as)))
      case Expr.Spread(op, arg) => toJson(arg).map(a => Json.obj(op.wire -> a))
      case Expr.Arr(items)      => items.traverse(toJson).map(Json.fromValues)
      case Expr.Obj(fields) =>
        fields.toList.traverse { case (k, x) => toJson(x).map(k -> _) }.map {
          case List((k, x)) if !k.startsWith(DataPrefix) => Json.obj(ObjectKey -> Json.obj(k -> x))
          case fs                                        => Json.fromFields(fs)
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
    W.JlvmProgram.codec.encode(W.JlvmProgram(1, toWire(p.expr))).leftMap(e => JlvmError.Malformed(e.toString))

  def fromCanonical(v: CanonicalValue): Either[JlvmError, Program] =
    W.JlvmProgram.codec.decoder.decode(v).leftMap(e => JlvmError.Malformed(e.message)).flatMap(w => fromWire(w.expr).flatMap(Program.from))

  def encode(p: Program): Either[JlvmError, ByteVector] = canonical(p).map(Cbor.encode)

  def decode(bytes: ByteVector): Either[JlvmError, Program] =
    Cbor.decode(bytes).leftMap(e => JlvmError.Malformed(e.toString)).flatMap(fromCanonical)

  /** A program's identity: the tagged hash of its canonical form. */
  def id[H: Hasher](p: Program): Either[JlvmError, Digest[H]] =
    encode(p).map(TaggedHash[H](DomainTag.literal(ProgramTag)).apply)

  private[kd5ujc] val ProgramTag = "dilf4s/jlvm/program/v1"
}
