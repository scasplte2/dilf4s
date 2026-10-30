package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.{Digest, DomainTag, Hasher, TaggedHash}
import xyz.kd5ujc.models.{jlvm => W}

import io.circe.{Json, JsonObject}
import scodec.bits.ByteVector

/**
 * The JSON authoring surface and the canonical program form.
 *
 * Surface rules (docs/specs/jlvm.md): a single-key object whose key is an operation of this language version is
 * that operation; any other object is a map expression (in [[Mode.Strict]], an unknown single key is an error, to
 * reject typos such as `iff`); arrays are evaluated element-wise; JSON null is the null literal; `0x`-hex strings are
 * bytes ([[JsonSurface]]); `{"literal": x}` quotes `x`; `{"var": path}` or `{"var": [path, default]}` reads a variable.
 *
 * Input is validated by the strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
 * structurally with nulls still visible, so a null field cannot turn a map expression into an operation.
 */
object Parser {

  enum Mode {
    case Lenient, Strict
  }

  private def malformed[A](m: String): Either[JlvmError, A] = JlvmError.Malformed(m).asLeft

  private def value(j: Json): Either[JlvmError, CanonicalValue] = JsonSurface.value(j)

  def parse(json: String, mode: Mode = Mode.Lenient): Either[JlvmError, Program] =
    for {
      _    <- CanonicalJson.parse(json).leftMap(e => JlvmError.Malformed(e.toString))
      j    <- io.circe.parser.parse(json).leftMap(e => JlvmError.Malformed(e.message))
      expr <- fromJson(j, mode)
    } yield Program(expr)

  /** Depth is bounded by the canonical parser that validated the text (256), so this recursion is bounded too. */
  def fromJson(j: Json, mode: Mode = Mode.Lenient): Either[JlvmError, Expr] =
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
      case "literal" => value(arg).map(Expr.Lit(_))
      case "var"     => variable(arg)
      case name =>
        (Op.fromWire(name), mode) match {
          case (Some(op), _) => arg.asArray.fold(fromJson(arg, mode).map(Vector(_)))(_.traverse(fromJson(_, mode))).map(Expr.Apply(op, _))
          case (None, Mode.Lenient) => mapExpr(List(name -> arg), mode)
          case (None, Mode.Strict) =>
            malformed(s"unknown operation \"$name\" (strict mode; quote single-key data with {\"literal\": ...})")
        }
    }

  private def path(j: Json): Either[JlvmError, VarRef] =
    j.asString
      .orElse(j.asNumber.flatMap(_.toBigInt).map(_.toString))
      .fold(malformed[VarRef]("a var path is text or an integer"))(VarRef.parse)

  private def variable(arg: Json): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(Vector(p))          => path(p).map(Expr.Var(_, None))
      case Some(Vector(p, default)) => (path(p), value(default)).mapN((r, d) => Expr.Var(r, Some(d).filter(_ != CanonicalValue.CNull)))
      case Some(_)                  => malformed("var takes a path, or [path, default]")
      case None                     => path(arg).map(Expr.Var(_, None))
    }

  // ------------------------------------------------------------------ canonical program form (shared/schemas/jlvm.cddl)

  def toWire(e: Expr): W.JlvmExpr =
    e match {
      case Expr.Lit(CanonicalValue.CNull) => W.JlvmNull()
      case Expr.Lit(v)                    => W.JlvmLit(v)
      case Expr.Var(ref, default)         => W.JlvmVar(ref.render, default)
      case Expr.Apply(op, args)           => W.JlvmApply(op.wire, args.map(toWire))
      case Expr.Arr(items)                => W.JlvmArray(items.map(toWire))
      case Expr.Obj(fields)               => W.JlvmMap(fields.map { case (k, v) => k -> toWire(v) })
    }

  def fromWire(w: W.JlvmExpr): Either[JlvmError, Expr] =
    w match {
      case W.JlvmNull()    => Expr.Lit(CanonicalValue.CNull).asRight
      case W.JlvmLit(v)    => Expr.Lit(v).asRight
      case W.JlvmVar(p, d) => VarRef.parse(p).map(Expr.Var(_, d))
      case W.JlvmApply(op, as) =>
        Op.fromWire(op).toRight(JlvmError.Malformed(s"unknown operation \"$op\"")).flatMap(o => as.traverse(fromWire).map(Expr.Apply(o, _)))
      case W.JlvmArray(items) => items.traverse(fromWire).map(Expr.Arr(_))
      case W.JlvmMap(fields)  => fields.toList.traverse { case (k, v) => fromWire(v).map(k -> _) }.map(fs => Expr.Obj(fs.toMap))
    }

  def canonical(p: Program): Either[JlvmError, CanonicalValue] =
    W.JlvmProgram.codec.encode(W.JlvmProgram(1, toWire(p.expr))).leftMap(e => JlvmError.Malformed(e.toString))

  def fromCanonical(v: CanonicalValue): Either[JlvmError, Program] =
    W.JlvmProgram.codec.decoder.decode(v).leftMap(e => JlvmError.Malformed(e.message)).flatMap(w => fromWire(w.expr).map(Program(_)))

  def encode(p: Program): Either[JlvmError, ByteVector] = canonical(p).map(Cbor.encode)

  def decode(bytes: ByteVector): Either[JlvmError, Program] =
    Cbor.decode(bytes).leftMap(e => JlvmError.Malformed(e.toString)).flatMap(fromCanonical)

  /** A program's identity: the tagged hash of its canonical form. */
  def id[H: Hasher](p: Program): Either[JlvmError, Digest[H]] =
    encode(p).map(TaggedHash[H](DomainTag.literal(ProgramTag)).apply)

  private[kd5ujc] val ProgramTag = "dilf4s/jlvm/program/v1"
}
