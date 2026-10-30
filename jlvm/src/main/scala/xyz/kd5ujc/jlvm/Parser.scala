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

  /** metakit operations with no v1 form, and what replaces each: named in the error, so a migration knows where to go. */
  val Removed: scala.collection.immutable.Map[String, String] = scala.collection.immutable.Map(
    "prove_dlog_verify"    -> "a one-leaf sigma_verify over {\"type\": \"dlog\"} (docs/specs/sigma.md)",
    "prove_dhtuple_verify" -> "a one-leaf sigma_verify over {\"type\": \"dhtuple\"} (docs/specs/sigma.md)",
    "mpt_verify"           -> "trie_verify (docs/specs/trie.md)",
    "mpt_prefix_verify"    -> "trie_prefix_verify (docs/specs/trie.md)",
    "smt_verify"           -> "trie_verify (docs/specs/trie.md)",
    "noop"                 -> "nothing: it was internal to metakit's evaluator"
  )

  /** Names metakit's array syntax reads as a call when they head an array: `["+", 1, 2]`, `["var", "a"]`. */
  def callName(s: String): Boolean = s == Variable || Op.fromWire(s).isDefined || Removed.contains(s)

  private def malformed[A](m: String): Either[JlvmError, A] = JlvmError.Malformed(m).asLeft

  private def value(j: Json): Either[JlvmError, CanonicalValue] = JsonSurface.value(j)

  def parse(json: String, mode: Mode = Mode.Strict): Either[JlvmError, Program] =
    for {
      _    <- CanonicalJson.parse(json).leftMap(e => JlvmError.Malformed(e.toString))
      j    <- io.circe.parser.parse(json).leftMap(e => JlvmError.Malformed(e.message))
      expr <- fromJson(j, mode).map(e => if (mode == Mode.Lenient) bindLets(e, Set.empty) else e)
      prog <- Program.from(expr)
    } yield prog

  /** Depth is bounded by the canonical parser that validated the text (256), so this recursion is bounded too. */
  def fromJson(j: Json, mode: Mode = Mode.Strict): Either[JlvmError, Expr] =
    j.fold(
      Expr.Lit(CanonicalValue.CNull).asRight,
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      _ => value(j).map(Expr.Lit(_)),
      items => array(items, mode),
      obj => fromObject(obj, mode)
    )

  /**
   * metakit read an array headed by an operation's name (or `var`) as that call. Strict rejects that form, so no
   * metakit program changes meaning silently; lenient reads it as metakit did.
   */
  private def array(items: Vector[Json], mode: Mode): Either[JlvmError, Expr] =
    (items.headOption.flatMap(_.asString).filter(callName), mode) match {
      case (Some(name), Mode.Strict) =>
        malformed(
          s"[\"$name\", ...] is a call in metakit's array syntax: write {\"$name\": [...]}, or quote data with {\"literal\": [...]}"
        )
      case (Some(name), Mode.Lenient) => single(name, Json.fromValues(items.drop(1)), mode)
      case (scala.None, _)            => elements(items, mode)
    }

  private def elements(items: Vector[Json], mode: Mode): Either[JlvmError, Expr] = items.traverse(fromJson(_, mode)).map(Expr.Arr(_))

  /**
   * metakit read a `let` name with a plain path, `{"var": "x"}`; in v1 it is `$x` (decision B). Lenient parsing
   * rewrites those reads, so a migrated program keeps its meaning. Bounded by the program depth, like [[fromJson]].
   */
  private def bindLets(e: Expr, lets: Set[String]): Expr =
    e match {
      case Expr.Var(VarRef.Data(path), d) if path.headOption.exists(lets.contains) =>
        Expr.Var(VarRef.Bound(path.headOption.getOrElse(""), path.drop(1)), d)
      case Expr.Var(_, _) | Expr.Lit(_) => e
      case Expr.VarAt(p, d)             => Expr.VarAt(bindLets(p, lets), d)
      case Expr.Spread(op, a)           => Expr.Spread(op, bindLets(a, lets))
      case Expr.Arr(is)                 => Expr.Arr(is.map(bindLets(_, lets)))
      case Expr.Obj(fs)                 => Expr.Obj(fs.map { case (k, v) => k -> bindLets(v, lets) })
      case Expr.Apply(Op.Let, Vector(Expr.Arr(pairs), body)) =>
        val names = pairs.collect { case Expr.Arr(Vector(Expr.Lit(CanonicalValue.CText(n)), _)) => n }
        val bound = pairs.zipWithIndex.map {
          case (Expr.Arr(Vector(n, x)), i) => Expr.Arr(Vector(n, bindLets(x, lets ++ names.take(i))))
          case (other, _)                  => other
        }
        Expr.Apply(Op.Let, Vector(Expr.Arr(bound), bindLets(body, lets ++ names)))
      case Expr.Apply(Op.Let, Vector(Expr.Obj(fields), body)) =>
        // bound in code-point order of the names (docs/specs/jlvm.md section 4.2)
        def before(n: String) = fields.keySet.filter(m => Values.compareCodePoints(m, n) < 0)
        Expr.Apply(
          Op.Let,
          Vector(Expr.Obj(fields.map { case (n, x) => n -> bindLets(x, lets ++ before(n)) }), bindLets(body, lets ++ fields.keySet))
        )
      case Expr.Apply(op, as) => Expr.Apply(op, as.map(bindLets(_, lets)))
    }

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
      case removed if Removed.contains(removed) =>
        malformed(s"\"$removed\" was removed in JLVM v1: use ${Removed.getOrElse(removed, "")}")
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
        // a literal array here is the path's segments, not code, so it is exempt from the array-syntax check
        if (j.isObject) fromJson(j, mode).map(Expr.VarAt(_, default))
        else if (j.isArray) j.asArray.fold(fromJson(j, mode))(elements(_, mode)).map(Expr.VarAt(_, default))
        else malformed("a var path is text, an integer, or an expression computing one")
      )

  private def variable(arg: Json, mode: Mode): Either[JlvmError, Expr] =
    arg.asArray match {
      case Some(Vector(p))          => path(p, None, mode)
      case Some(Vector(p, default)) => defaultValue(default, mode).flatMap(d => path(p, Some(d).filter(_ != CanonicalValue.CNull), mode))
      case Some(_)                  => malformed("var takes a path, or [path, default]")
      case None                     => path(arg, None, mode)
    }

  /**
   * A default is a literal value, never evaluated (as in metakit). An object or array default could be mistaken for
   * code, so the strict surface requires it quoted, `{"literal": ...}`; both modes accept the quoted form.
   */
  private def defaultValue(j: Json, mode: Mode): Either[JlvmError, CanonicalValue] =
    (j.asObject.map(_.toList), mode) match {
      case (Some(List((Literal, x))), _) => value(x)
      case (_, Mode.Strict) if j.isObject || j.isArray =>
        malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
      case _ => value(j)
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
        // always the array form: a bare array would read as [path, default], and a bare literal path as a static one
        val pathJson = path match {
          case Expr.Lit(v) => quoted(v)
          case other       => toJson(other)
        }
        (pathJson, default.traverse(defaultJson)).mapN((pj, dj) => Json.obj(Variable -> Json.fromValues(pj +: dj.toVector)))
      case Expr.Apply(op, args) => args.traverse(toJson).map(as => Json.obj(op.wire -> Json.fromValues(as)))
      case Expr.Spread(op, arg) => toJson(arg).map(a => Json.obj(op.wire -> a))
      case Expr.Arr(items)      =>
        // a leading text naming a call is quoted, so the array cannot read back as metakit's array syntax
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
