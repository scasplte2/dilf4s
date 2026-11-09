package xyz.kd5ujc.jlvm

import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import xyz.kd5ujc.core.canonical.{CanonicalEntries, CanonicalError, CanonicalJson, CanonicalValue, Cbor, Preview}
import xyz.kd5ujc.core.hash.{Digest, DomainTag, Hasher, TaggedHash}
import xyz.kd5ujc.jlvm.SurfaceTree.{Kind, Source}
import xyz.kd5ujc.models.{jlvm => W}

import scodec.bits.ByteVector

/**
 * The JSON authoring surface and the canonical program form (docs/specs/jlvm.md §1).
 *
 * Surface rules:
 *   - a single-key object whose key is an operation is that operation. An array value is its argument list. Any other
 *     value is one argument, except that a [[Ops.Variadic]] operation takes the elements of that value's array
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
 * structurally with nulls still visible, so a null field cannot turn a map expression into an operation
 * ([[SurfaceTree]]); every scalar and literal is read by that parser from its own span. No other JSON parser reads a
 * program. [[render]] writes the surface back.
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
  def callName(s: String): Boolean = s == Variable || Ops.named(s).isDefined || Removed.contains(s)

  private def malformed[A](m: String): Either[JlvmError, A] = JlvmError.Malformed(m).asLeft

  def parse(json: String): Either[JlvmError, Program] = parse(json, Kernels.Default)

  /**
   * [[parse]] with `kernels` ordering object keys, and told of each expression read and each span of text read back:
   * production passes the default.
   */
  private[jlvm] def parse(json: String, kernels: Kernels): Either[JlvmError, Program] =
    for {
      read <- SurfaceTree.read(json, kernels.steps.spanned).leftMap(canonicalError)
      src  <- read.toRight(JlvmError.Malformed("not JSON"))
      expr <- expression(src.root)(using src, kernels)
      prog <- Program.from(expr, kernels)
    } yield prog

  /** A scalar, a literal or a default: its span through the strict parser, then `0x`-hex strings as bytes. */
  private def value(j: SurfaceTree)(using src: Source): Either[JlvmError, CanonicalValue] =
    src.value(j).bimap(e => JlvmError.Malformed(e.toString), JsonSurface.fromCanonicalJson)

  /**
   * One step per JSON value read. Depth is bounded by the canonical parser that validated the text (256), so this
   * recursion is bounded too.
   */
  private def expression(j: SurfaceTree)(using src: Source, kernels: Kernels): Either[JlvmError, Expr] = {
    kernels.steps.step()
    j.kind match {
      case Kind.Null                    => Expr.Lit(CanonicalValue.CNull).asRight
      case Kind.Scalar | Kind.Str       => value(j).map(Expr.Lit(_))
      case Kind.Arr(items)              => array(items)
      case Kind.Obj(Vector((key, arg))) => single(key, arg)
      case Kind.Obj(fields)             => mapExpr(fields)
    }
  }

  /** An array headed by a call's name is rejected, so text shaped like a call is never read as data by accident. */
  private def array(items: Vector[SurfaceTree])(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    items.headOption.flatMap(src.string).filter(callName) match {
      case Some(name) =>
        val call = Removed.get(name).fold(s"write the call as {\"$name\": [...]}")(hint => s"\"$name\" is not an operation: $hint")
        malformed(
          s"an array may not start with \"$name\", which reads as a call: $call; quote data as {\"literal\": [...]}, or " +
          s"start it with {\"literal\": \"$name\"}"
        )
      case scala.None => elements(items)
    }

  private def elements(items: Vector[SurfaceTree])(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    items.traverse(expression).map(Expr.Arr(_))

  /** A `let`'s `[name, expression]` pairs: a name is never code, so a pair is exempt from the array check. */
  private def letBindings(j: SurfaceTree)(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    j.kind match {
      case Kind.Arr(pairs) =>
        pairs.traverse { pair =>
          pair.kind match {
            case Kind.Arr(items) => elements(items)
            case _               => expression(pair)
          }
        }.map(Expr.Arr(_))
      case _ => expression(j)
    }

  /** The fields in [[Kernels.keys]] order: keys a program chooses are compared, never hashed. */
  private def mapExpr(fields: Vector[(String, SurfaceTree)])(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    fields.traverse { case (k, v) => expression(v).map(k -> _) }.map(fs => Expr.Obj(TreeMap.from(fs)(using kernels.keys)))

  private def single(key: String, arg: SurfaceTree)(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    key match {
      case Literal  => value(arg).map(Expr.Lit(_))
      case Variable => variable(arg)
      case ObjectKey =>
        arg.kind match {
          case Kind.Obj(fields) => mapExpr(fields)
          case _                => malformed("object takes an object: {\"object\": {key: expression, ...}}")
        }
      case data if data.startsWith(DataPrefix) => mapExpr(Vector(data -> arg))
      case removed if Removed.contains(removed) =>
        malformed(s"\"$removed\" is not an operation: ${Removed.getOrElse(removed, "")}")
      case name =>
        Ops
          .named(name)
          .fold(
            malformed[Expr](s"unknown operation ${Preview.of(name)}: write single-key data as {\"object\": {...}} or {\"literal\": {...}}")
          )(operation(_, arg))
    }

  private def operation(op: Op, arg: SurfaceTree)(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    arg.kind match {
      case Kind.Arr(Vector(bindings, body)) if op == Op.Let =>
        (letBindings(bindings), expression(body)).mapN((b, x) => Expr.Apply(op, Vector(b, x)))
      case Kind.Arr(items)                => items.traverse(expression).map(Expr.Apply(op, _))
      case _ if Ops.Variadic.contains(op) => expression(arg).map(Expr.Spread(op, _))
      case _                              => expression(arg).map(a => Expr.Apply(op, Vector(a)))
    }

  /**
   * A path is text or an integer; an object or an array is an expression computing the path. Only a string's or a
   * number's span is read back, so a nested computed path is read once, not once per level.
   */
  private def path(j: SurfaceTree, default: Option[CanonicalValue])(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    src.string(j).orElse(src.integer(j).map(_.toString)) match {
      case Some(p) => VarRef.parse(p).map(Expr.Var(_, default))
      case scala.None =>
        j.kind match {
          case Kind.Obj(_) => expression(j).map(Expr.VarAt(_, default))
          // a literal array here is the path's segments, not code, so it is exempt from the array check
          case Kind.Arr(items) => elements(items).map(Expr.VarAt(_, default))
          case _               => malformed("a var path is text, an integer, or an expression computing one")
        }
    }

  private def variable(arg: SurfaceTree)(using src: Source, kernels: Kernels): Either[JlvmError, Expr] =
    arg.kind match {
      case Kind.Arr(Vector(p))          => path(p, None)
      case Kind.Arr(Vector(p, default)) => defaultValue(default).flatMap(d => path(p, Some(d).filter(_ != CanonicalValue.CNull)))
      case Kind.Arr(_)                  => malformed("var takes a path, or [path, default]")
      case _                            => path(arg, None)
    }

  /**
   * A default is a literal value, never evaluated. An object or array default could be mistaken for code, so it is
   * quoted, `{"literal": ...}`.
   */
  private def defaultValue(j: SurfaceTree)(using src: Source): Either[JlvmError, CanonicalValue] =
    j.kind match {
      case Kind.Obj(Vector((Literal, x))) => value(x)
      case Kind.Obj(_) | Kind.Arr(_) =>
        malformed("a var default is a literal value: quote an object or array default with {\"literal\": ...}")
      case _ => value(j)
    }

  // ------------------------------------------------------------------ the surface, written back

  /**
   * The surface text of a program: [[parse]] reads it back as the same program. A literal holding `0x`-hex text or a
   * non-terminating rational has no surface form (only the canonical form can carry it), and is an error. Scalars and
   * literals are written by [[JsonSurface.render]].
   */
  def render(p: Program): Either[JlvmError, String] = toJson(p.expr).map(Out.print)

  /** JSON being written: text already rendered, or a container of it. A map expression may hold null fields, which no value can. */
  private enum Out {
    case Raw(text: String)
    case Arr(items: Vector[Out])

    /** Each member's name already written as JSON text. */
    case Obj(fields: Vector[(String, Out)])
  }

  private object Out {

    /** Compact text from a worklist: no recursion, and no text copied once per level. */
    def print(o: Out): String = {
      @tailrec
      def go(todo: List[Either[String, Out]], acc: Vector[String]): String =
        todo match {
          case Nil                           => acc.mkString
          case Left(text) :: rest            => go(rest, acc :+ text)
          case Right(Out.Raw(text)) :: rest  => go(rest, acc :+ text)
          case Right(Out.Arr(items)) :: rest => go(within("[", items.map(i => List(Right(i))), "]") ::: rest, acc)
          case Right(Out.Obj(fields)) :: rest =>
            go(within("{", fields.map { case (k, v) => List(Left(k), Left(":"), Right(v)) }, "}") ::: rest, acc)
        }
      go(List(Right(o)), Vector.empty)
    }

    private def within(open: String, parts: Vector[List[Either[String, Out]]], close: String): List[Either[String, Out]] =
      Left(open) :: parts.toList.zipWithIndex.flatMap { case (part, i) => if (i == 0) part else Left(",") :: part } ::: List(Left(close))
  }

  private def text(s: String): Either[JlvmError, String] =
    CanonicalValue.text(s).flatMap(CanonicalJson.render).leftMap(e => JlvmError.Malformed(e.toString))

  private def member(name: String, arg: Out): Either[JlvmError, Out] = text(name).map(k => Out.Obj(Vector(k -> arg)))

  private def literal(v: CanonicalValue): Either[JlvmError, Out] = JsonSurface.render(v).map(Out.Raw(_))

  private def quoted(v: CanonicalValue): Either[JlvmError, Out] = literal(v).flatMap(member(Literal, _))

  private def defaultJson(v: CanonicalValue): Either[JlvmError, Out] =
    v match {
      case CanonicalValue.CArray(_) | CanonicalValue.CMap(_) => quoted(v)
      case other                                             => literal(other)
    }

  /** Depth is bounded by [[Program.MaxDepth]], so this recursion is bounded too. */
  private def toJson(e: Expr): Either[JlvmError, Out] =
    e match {
      case Expr.Lit(v @ (CanonicalValue.CArray(_) | CanonicalValue.CMap(_))) => quoted(v)
      case Expr.Lit(v)                                                       => literal(v)
      case Expr.Var(ref, default) =>
        text(ref.render)
          .map(Out.Raw(_))
          .flatMap(r => default.fold(r.asRight[JlvmError])(d => defaultJson(d).map(dj => Out.Arr(Vector(r, dj)))))
          .flatMap(member(Variable, _))
      case Expr.VarAt(path, default) =>
        // a literal path is quoted, or it would read as a static one; an object path with no default is written bare,
        // so a computed path adds one JSON level, as in the canonical form; a bare array would read as [path, default]
        val pathJson = path match {
          case Expr.Lit(v) => quoted(v)
          case other       => toJson(other)
        }
        (pathJson, default.traverse(defaultJson)).tupled.flatMap {
          case (pj @ Out.Obj(_), scala.None) => member(Variable, pj)
          case (pj, dj)                      => member(Variable, Out.Arr(pj +: dj.toVector))
        }
      case Expr.Apply(Op.Let, Vector(Expr.Arr(pairs), body)) =>
        // binding names are written plainly: letBindings reads them back as names, whatever they spell
        (
          pairs.traverse {
            case Expr.Arr(Vector(n @ Expr.Lit(CanonicalValue.CText(_)), x)) => (toJson(n), toJson(x)).mapN((a, b) => Out.Arr(Vector(a, b)))
            case other                                                      => toJson(other)
          },
          toJson(body)
        ).tupled.flatMap((ps, b) => member(Op.Let.wire, Out.Arr(Vector(Out.Arr(ps), b))))
      case Expr.Apply(op, args) => args.traverse(toJson).flatMap(as => member(op.wire, Out.Arr(as)))
      case Expr.Spread(op, arg) => toJson(arg).flatMap(member(op.wire, _))
      case Expr.Arr(items)      =>
        // a leading text naming a call is quoted: unquoted, the array would not parse
        items.zipWithIndex.traverse {
          case (Expr.Lit(t @ CanonicalValue.CText(s)), 0) if callName(s) => quoted(t)
          case (x, _)                                                    => toJson(x)
        }.map(Out.Arr(_))
      case Expr.Obj(fields) =>
        fields.toVector.traverse { case (k, x) => (text(k), toJson(x)).tupled.map(k -> _) }.flatMap {
          case Vector((k, (_, x))) if !k.startsWith(DataPrefix) => member(k, x).flatMap(member(ObjectKey, _))
          case fs                                               => Out.Obj(fs.map(_._2)).asRight
        }
    }

  /**
   * How deep [[render]] nests a program's JSON, counted as the canonical JSON parser counts it (non-empty arrays and
   * objects), so [[Program.from]] can bound it without rendering. It follows [[toJson]] case by case. A worklist of
   * (expression, nesting of the JSON container holding it), so an unchecked tree cannot overflow the stack.
   */
  private[jlvm] def surfaceDepth(e: Expr, kernels: Kernels): Int = {
    @tailrec
    def go(todo: List[(Expr, Int)], deepest: Int): Int =
      todo match {
        case Nil => deepest
        case (x, l) :: tl =>
          kernels.steps.step()
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
      case Expr.Apply(op, args)           => W.JlvmApply(op, args.map(toWire))
      case Expr.Spread(op, arg)           => W.JlvmSpread(op, toWire(arg))
      case Expr.Arr(items)                => W.JlvmArray(items.map(toWire))
      case Expr.Obj(fields) =>
        W.JlvmMap(fields.foldLeft(CanonicalEntries.empty[W.JlvmExpr]) { case (acc, (k, v)) => acc.updated(k, toWire(v)) })
    }

  def fromWire(w: W.JlvmExpr): Either[JlvmError, Expr] =
    w match {
      case W.JlvmNull()        => Expr.Lit(CanonicalValue.CNull).asRight
      case W.JlvmLit(v)        => Expr.Lit(v).asRight
      case W.JlvmVar(p, d)     => VarRef.parse(p).map(Expr.Var(_, d))
      case W.JlvmVarAt(p, d)   => fromWire(p).map(Expr.VarAt(_, d))
      case W.JlvmApply(op, as) => as.traverse(fromWire).map(Expr.Apply(op, _))
      case W.JlvmSpread(op, a) => fromWire(a).map(Expr.Spread(op, _))
      case W.JlvmArray(items)  => items.traverse(fromWire).map(Expr.Arr(_))
      case W.JlvmMap(fields) =>
        fields.toList.traverse { case (k, v) => fromWire(v).map(k -> _) }.map(fs => Expr.Obj(TreeMap.from(fs)(using Kernels.CodePoints)))
    }

  def canonical(p: Program): Either[JlvmError, CanonicalValue] =
    W.JlvmProgram.codec.encode(W.JlvmProgram(1, toWire(p.expr))).leftMap(canonicalError)

  /** A depth bound is a [[JlvmError.Limit]], however it is reached; anything else malformed is [[JlvmError.Malformed]]. */
  private def canonicalError(e: CanonicalError): JlvmError = e match {
    case CanonicalError.DepthExceeded(n) => JlvmError.Limit(s"the program nests deeper than its canonical form allows ($n levels)")
    case other                           => JlvmError.Malformed(other.toString)
  }

  /**
   * The heaviest canonical form any program has (jlvm.md section 1, "Reading a canonical form"): 64 per unit of the
   * largest program's size, where a form needs at most 40 (an operation's node writes 38 besides its arguments).
   */
  val MaxCanonicalWeight: Long = xyz.kd5ujc.models.limits.JlvmMaxCanonicalWeight.toLong

  /**
   * The program a canonical form denotes, its refusals in the normative order (jlvm.md section 1): a form heavier than
   * [[MaxCanonicalWeight]] is a [[JlvmError.Limit]] from its cached weight, before anything reads it (decoding walks
   * the value as the schema reads it, so a value built with shared parts would be expanded); then the decoder's shape
   * errors, then `var` paths that do not parse ([[JlvmError.Malformed]]); then [[Program.from]]'s bounds and checks.
   */
  def fromCanonical(v: CanonicalValue): Either[JlvmError, Program] =
    if (v.weight > MaxCanonicalWeight) Left(JlvmError.Limit(s"the form weighs more than any program's, $MaxCanonicalWeight"))
    else
      W.JlvmProgram.codec.decoder
        .decode(v)
        .leftMap(e => JlvmError.Malformed(e.message))
        .flatMap(w => fromWire(w.expr).flatMap(Program.from))

  /** A program's canonical CBOR; one past `Cbor.MaxEncodedBytes` is a [[JlvmError.Limit]], never a throw. */
  def encode(p: Program): Either[JlvmError, ByteVector] =
    canonical(p).flatMap(Cbor.encode(_).leftMap(e => JlvmError.Limit(s"the program's encoding is over ${e.limit} bytes")))

  def decode(bytes: ByteVector): Either[JlvmError, Program] =
    Cbor.decode(bytes).leftMap(canonicalError).flatMap(fromCanonical)

  /** A program's identity: the tagged hash of its canonical form. */
  def id[H: Hasher](p: Program): Either[JlvmError, Digest[H]] =
    encode(p).map(TaggedHash[H](DomainTag.literal(ProgramTag)).apply)

  private[kd5ujc] val ProgramTag = "dilf4s/jlvm/program/v1"
}
