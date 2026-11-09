package xyz.kd5ujc.jlvm.difffuzz

import cats.Eval
import cats.data.NonEmptyVector
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.jlvm.{Op, Ops, Parser, Program}

import scodec.bits.ByteVector

import Gen._
import Sj._

/** A [[Sj]] tree's nodes by path (child indices, an object's by member position), and edits at a path. */
object Tree {

  type Path = List[Int]

  def children(j: Sj): Vector[Sj] = j match {
    case Arr(xs)  => xs
    case Obj(kvs) => kvs.map(_._2)
    case _        => Vector.empty
  }

  /** Every node, preorder, with its path: an explicit stack. */
  def nodes(j: Sj): Vector[(Path, Sj)] = {
    @scala.annotation.tailrec
    def go(todo: List[(Path, Sj)], acc: Vector[(Path, Sj)]): Vector[(Path, Sj)] = todo match {
      case Nil => acc
      case (p, x) :: rest =>
        go(children(x).zipWithIndex.map { case (c, i) => (p :+ i) -> c }.toList ::: rest, acc :+ (p -> x))
    }
    go(List(Nil -> j), Vector.empty)
  }

  def update(j: Sj, path: Path, f: Sj => Sj): Sj = {
    def go(x: Sj, p: Path): Eval[Sj] = p match {
      case Nil => Eval.now(f(x))
      case i :: rest =>
        x match {
          case Arr(xs) if xs.indices.contains(i) => Eval.defer(go(xs(i), rest)).map(c => Arr(xs.updated(i, c)))
          case Obj(kvs) if kvs.indices.contains(i) =>
            Eval.defer(go(kvs(i)._2, rest)).map(c => Obj(kvs.updated(i, kvs(i)._1 -> c)))
          case other => Eval.now(other)
        }
    }
    go(j, path).value
  }

  /** Every `0x` text in the tree (a byte string in the surface). */
  def hexes(j: Sj): Vector[Path] = nodes(j).collect { case (p, Str(s)) if s.startsWith("0x") && s.length % 2 == 0 => p }
}

/** Mutations of surface JSON, of byte strings in it, and of a program's canonical form. */
object Mutate {

  private val hexDigits = "0123456789abcdef"

  /** A byte string's mutations: a flipped digit, a byte dropped or added, upper case (which makes it text). */
  def hex(s: String): G[Sj] = {
    val body = s.drop(2)
    weighted(
      5 -> (below(body.length.max(1)), below(15)).mapN { (i, d) =>
        if (body.isEmpty) Str("0x00")
        else {
          val c = hexDigits.indexOf(body.charAt(i).toInt)
          Str("0x" + body.updated(i, hexDigits.charAt((c + 1 + d) % 16)))
        }
      },
      2 -> pure(Str("0x" + body.dropRight(2))),
      2 -> oneOf("00", "ff", "01").map(b => Str("0x" + body + b)),
      1 -> oneOf("00", "ff").map(b => Str("0x" + b + body)),
      1 -> pure(Str("0x" + body.toUpperCase(java.util.Locale.ROOT))),
      1 -> pure(Str("0x" + body.take(body.length / 2)))
    )
  }

  /** One byte string somewhere in `j`, mutated; `j` itself when it holds none. */
  def someHex(j: Sj): G[Sj] =
    NonEmptyVector.fromVector(Tree.hexes(j)).fold(pure(j)) { ps =>
      pick(ps).flatMap(p =>
        Tree.nodes(j).collectFirst { case (`p`, Str(s)) => s }.fold(pure(j))(s => hex(s).map(h => Tree.update(j, p, _ => h)))
      )
    }

  private val names: NonEmptyVector[String] =
    NonEmptyVector(
      "getKey",
      // the maps' keys sorted, so a seed's cases never depend on a map's iteration order
      Op.values.toVector.map(_.wire) ++ Ops.Aliases.keys.toVector.sorted ++ Parser.Removed.keys.toVector.sorted ++
      Vector("_x", "IF", "", "var", "literal", "object", "+ ")
    )

  /** The arity groups: operations written with the same argument counts. */
  private val sameArity: Map[Op, NonEmptyVector[Op]] = {
    val arity = (o: Op) =>
      Grammar.sig(o) match {
        case Sig.Fixed(alts)    => alts.map(_.size).toVector.toSet
        case Sig.Variadic(_, _) => Set(-1)
        case Sig.Form           => Set(-2)
        case Sig.Crypto         => Set(-3)
      }
    val all = Op.values.toVector
    all.map(o => o -> NonEmptyVector.fromVector(all.filter(x => arity(x) == arity(o))).getOrElse(NonEmptyVector.one(o))).toMap
  }

  private def swapOp(k: String): G[String] =
    Ops.named(k).fold(pick(names)) { op =>
      weighted(
        4 -> sameArity.get(op).fold(pick(names))(g => pick(g).map(_.wire)),
        2 -> pick(names),
        1 -> oneOf(k.toUpperCase(java.util.Locale.ROOT), k + " ", "_" + k)
      )
    }

  /** Spellings of the same number, some of them refused: `2.0`, `2e0`, `20e-1`, `+2`, `02`. */
  private def spelling(t: String): G[Sj] =
    BigDecimal(t.toLowerCase(java.util.Locale.ROOT).replace("+", "")) match {
      case _ if t.length > 400 => Edge.num
      case n =>
        oneOf(
          n.bigDecimal.toPlainString,
          n.bigDecimal.toEngineeringString,
          n.bigDecimal.stripTrailingZeros.toString,
          t + "0",
          if (t.contains('.') || t.contains('e') || t.contains('E')) t else t + ".0",
          t + "e0",
          t + "E+0",
          "+" + t,
          "0" + t,
          "-" + t,
          (n + 1).toString,
          (n - 1).toString,
          (n * 2).toString
        ).map(Num(_))
    }

  /** A replacement for one node: an edit of its own kind, or something generated. */
  def node(x: Sj, grammar: Grammar): G[Sj] = {
    val fresh = weighted(8 -> grammar.arg(K.AnyV, 2, Ctx(0, false, Vector.empty)), 1 -> Edge.refused)
    val wrapped = oneOf("!", "!!", "typeof", "literal", "var", "length", "+").map(o => obj(o, Arr(Vector(x))))
    val own: G[Sj] = x match {
      case Obj(Vector((k, v))) if Parser.callName(k) || k.startsWith("_") => swapOp(k).map(k2 => Obj(Vector(k2 -> v)))
      case Obj(kvs) if kvs.nonEmpty =>
        below(kvs.size).flatMap { i =>
          weighted(
            2 -> pure(Obj(kvs.patch(i, Nil, 1))),
            1 -> pure(Obj(kvs :+ kvs(i))),
            1 -> Edge.key.map(k => Obj(kvs.updated(i, k -> kvs(i)._2)))
          )
        }
      case Arr(xs) if xs.nonEmpty =>
        (below(xs.size), below(xs.size)).tupled.flatMap {
          case (i, j) =>
            weighted(
              3 -> pure(Arr(xs.patch(i, Nil, 1))),
              2 -> pure(Arr(xs.patch(i, Vector(xs(i), xs(i)), 1))),
              1 -> pure(Arr(xs.updated(i, xs(j)).updated(j, xs(i)))),
              1 -> Edge.any(1).map(v => Arr(xs.patch(i, Vector(v), 0)))
            )
        }
      case Num(t) if t.headOption.exists(c => c.isDigit || c == '-') && scala.util.Try(BigDecimal(t)).isSuccess =>
        weighted(3 -> Edge.num, 2 -> spelling(t))
      case Num(_)                       => Edge.num
      case Str(s) if s.startsWith("0x") => weighted(4 -> hex(s), 1 -> Edge.bytes)
      case Str(_)                       => weighted(3 -> Edge.text, 1 -> Edge.num)
      case Bool(b)                      => oneOf(Bool(!b), Null, Num("0"))
      case Null                         => oneOf(Num("0"), Bool(false), Str(""), Arr(Vector.empty))
      case _                            => fresh
    }
    weighted(6 -> own, 2 -> fresh, 1 -> wrapped)
  }

  /** `n` edits at random nodes. */
  def tree(j: Sj, n: Int, grammar: Grammar): G[Sj] =
    (1 to n).toList.foldLeftM(j) { (x, _) =>
      pick(NonEmptyVector(Nil -> x, Tree.nodes(x))).flatMap { case (p, at) => node(at, grammar).map(r => Tree.update(x, p, _ => r)) }
    }

  // ─── canonical forms ───

  private def text(s: String): CanonicalValue = CanonicalValue.text(s).getOrElse(CNull)

  /** Every map in a canonical value, by its path of member names and indices: an explicit stack. */
  private def maps(v: CanonicalValue): Vector[List[Either[Int, String]]] = {
    @scala.annotation.tailrec
    def go(
      todo: List[(List[Either[Int, String]], CanonicalValue)],
      acc:  Vector[List[Either[Int, String]]]
    ): Vector[List[Either[Int, String]]] =
      todo match {
        case Nil                     => acc
        case (p, CArray(xs)) :: rest => go(xs.zipWithIndex.map { case (x, i) => (p :+ Left(i)) -> x }.toList ::: rest, acc)
        case (p, m: CMap) :: rest =>
          go(m.entries.iterator.toList.map { case (k, x) => (p :+ Right(k)) -> x } ::: rest, acc :+ p)
        case _ :: rest => go(rest, acc)
      }
    go(List(Nil -> v), Vector.empty)
  }

  private def edit(v: CanonicalValue, path: List[Either[Int, String]], f: CanonicalValue => CanonicalValue): CanonicalValue = {
    def go(x: CanonicalValue, p: List[Either[Int, String]]): Eval[CanonicalValue] = (p, x) match {
      case (Nil, _) => Eval.now(f(x))
      case (Left(i) :: rest, CArray(xs)) if xs.indices.contains(i) =>
        Eval.defer(go(xs(i), rest)).map(c => CanonicalValue.array(xs.updated(i, c)).getOrElse(x))
      case (Right(k) :: rest, m: CMap) =>
        m.entries
          .get(k)
          .fold(Eval.now(x))(c =>
            Eval
              .defer(go(c, rest))
              .map(n => CanonicalValue.map(m.entries.iterator.toList.map(e => if (e._1 == k) k -> n else e)).getOrElse(x))
          )
      case _ => Eval.now(x)
    }
    go(v, path).value
  }

  private def withEntry(m: CMap, k: String, v: CanonicalValue): CanonicalValue =
    CanonicalValue.map(m.entries.iterator.toList.filterNot(_._1 == k) :+ (k -> v)).getOrElse(m)

  private def without(m: CMap, k: String): CanonicalValue = CanonicalValue.map(m.entries.iterator.toList.filterNot(_._1 == k)).getOrElse(m)

  /** A node of the form edited: the shapes `jlvm.cddl` and the domain decoder refuse, and some it admits. */
  private def formNode(x: CanonicalValue): G[CanonicalValue] = x match {
    case m: CMap =>
      val keys = m.entries.keysIterator.toVector
      val byKey: G[CanonicalValue] = keys match {
        case ks if ks.contains("op") =>
          weighted(
            4 -> pick(names).map(n => withEntry(m, "op", text(n))),
            1 -> pure(withEntry(m, "args", CInt(1))),
            1 -> pure(
              CanonicalValue
                .map(
                  List(
                    "spread" -> m.entries.getOrElse("op", CNull),
                    "arg"    -> CanonicalValue.map(List("arr" -> CanonicalValue.EmptyArray)).getOrElse(CNull)
                  )
                )
                .getOrElse(m)
            )
          )
        case ks if ks.contains("spread") =>
          weighted(
            2 -> pick(names).map(n => withEntry(m, "spread", text(n))),
            1 -> pure(withEntry(m, "arg", CanonicalValue.map(List("arr" -> CanonicalValue.EmptyArray)).getOrElse(CNull)))
          )
        case ks if ks.contains("var") =>
          weighted(
            3 -> oneOf("a..b", "", "$", ".a", "$x.", "a.", "$a.b", "x", "$current").map(p => withEntry(m, "var", text(p))),
            1 -> pure(withEntry(m, "default", CInt(5))),
            1 -> pure(withEntry(m, "var", CInt(1)))
          )
        case ks if ks.contains("jlvm") =>
          weighted(3 -> oneOf(CInt(2), CInt(0), text("1"), CNull).map(v => withEntry(m, "jlvm", v)), 1 -> pure(without(m, "expr")))
        case ks if ks.contains("lit") =>
          weighted(
            2 -> oneOf(CNull, text("0xab"), CBytes(ByteVector(1, 2)), CanonicalValue.EmptyMap, text("$")).map(v => withEntry(m, "lit", v)),
            1 -> pure(without(m, "lit"))
          )
        case _ => pure(m)
      }
      weighted(
        5 -> byKey,
        1 -> pure(withEntry(m, "zz", CInt(1))),
        1 -> NonEmptyVector.fromVector(keys).fold(pure[CanonicalValue](m))(ks => pick(ks).map(k => without(m, k))),
        1 -> oneOf[CanonicalValue](CanonicalValue.EmptyMap, CInt(3), text("x"), CanonicalValue.EmptyArray)
      )
    case other => pure(other)
  }

  /** The form's CBOR with one byte-level change: a flipped bit, bytes cut or added, a non-minimal or indefinite head, a tag. */
  private def bytes(b: ByteVector): G[ByteVector] =
    weighted(
      4 -> (below(b.size.toInt.max(1)), below(8)).mapN((i, bit) =>
        if (b.isEmpty) ByteVector(0) else b.update(i.toLong, (b(i.toLong) ^ (1 << bit)).toByte)
      ),
      2 -> below(b.size.toInt.max(1)).map(n => b.dropRight((n % 4 + 1).toLong)),
      2 -> oneOf(0x00, 0xff, 0xf6, 0x80).map(x => b :+ x.toByte),
      1 -> pure(if (b.headOption.exists(h => (h & 0xe0) == 0xa0 && (h & 0x1f) < 24)) ByteVector(0xb8, b.head & 0x1f) ++ b.drop(1) else b),
      1 -> pure(if (b.headOption.exists(h => (h & 0xe0) == 0xa0 && (h & 0x1f) < 24)) ByteVector(0xbf) ++ b.drop(1) :+ 0xff.toByte else b),
      1 -> pure(ByteVector(0xd8, 0x18) ++ b),
      1 -> pure(ByteVector(0xc6) ++ b),
      1 -> pure(b ++ b)
    )

  private def head(major: Int, n: Long): ByteVector =
    if (n < 24) ByteVector((major << 5) | n.toInt)
    else if (n < 256) ByteVector((major << 5) | 24, n.toInt)
    else if (n < 65536) ByteVector((major << 5) | 25) ++ ByteVector.fromInt(n.toInt, 2)
    else ByteVector((major << 5) | 26) ++ ByteVector.fromInt(n.toInt, 4)

  private def textHead(s: String): ByteVector = {
    val b = ByteVector(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    head(3, b.size) ++ b
  }

  /** A program's canonical CBOR around an expression's: `{"expr": e, "jlvm": 1}`, keys in canonical order. */
  private def program(expr: ByteVector): ByteVector = ByteVector(0xa2) ++ textHead("expr") ++ expr ++ textHead("jlvm") ++ ByteVector(0x01)

  /** `{"op": op, "args": [e]}`, written as bytes, so it nests past what a canonical value may hold. */
  private def apply1(op: String, e: ByteVector): ByteVector =
    ByteVector(0xa2) ++ textHead("op") ++ textHead(op) ++ textHead("args") ++ ByteVector(0x81) ++ e

  /** A leaf for the bound forms: a null literal, or one that is refused for its shape or its path, deep inside. */
  private val leaf: G[ByteVector] = weighted(
    4 -> pure(ByteVector(0xa0)),
    1 -> pure(ByteVector(0xa2) ++ textHead("op") ++ textHead("nope") ++ textHead("args") ++ ByteVector(0x80)),
    1 -> pure(ByteVector(0xa1) ++ textHead("var") ++ textHead("a..b")),
    1 -> pure(
      ByteVector(0xa2) ++ textHead("arg") ++ ByteVector(0xa1) ++ textHead("arr") ++ ByteVector(0x80) ++ textHead("spread") ++ textHead("+")
    )
  )

  /**
   * Forms around the bounds, written as bytes: operations nested around the 127 a program may hold and the CBOR depth
   * of 256, a literal nested past the value depth, and an operation with more arguments than `Program.MaxSize`; some
   * with a node refused for its shape or path inside, so the order of the refusals (jlvm.md section 1) is exercised.
   */
  private val bounds: G[ByteVector] = weighted(
    3 -> (oneOf("!", "-", "abs"), oneOf(120, 126, 127, 128, 129, 130), leaf).mapN((op, n, l) =>
      program((1 to n).foldLeft(l)((e, _) => apply1(op, e)))
    ),
    2 -> oneOf(250, 252, 253, 254, 255, 256).map(n =>
      program(ByteVector(0xa1) ++ textHead("lit") ++ ByteVector.fill(n.toLong)(0x81) ++ ByteVector(0x01))
    ),
    1 -> oneOf((1 << 18) + 1, (1 << 18) + 1, (1 << 18) + 1, (1 << 18) - 4).map(n =>
      program(
        ByteVector(0xa2) ++ textHead("op") ++ textHead("+") ++ textHead("args") ++ head(4, n.toLong) ++ ByteVector.fill(n.toLong)(0xa0)
      )
    )
  )

  private val vocabulary: NonEmptyVector[String] =
    NonEmptyVector("op", Vector("args", "spread", "arg", "var", "default", "path", "lit", "arr", "map", "jlvm", "expr", "x"))

  private def value(j: Sj): CanonicalValue = xyz.kd5ujc.jlvm.JsonSurface.parse(Sj.print(j)).getOrElse(CNull)

  /**
   * A random tree in the vocabulary of `jlvm.cddl`: every node shape, and maps of its keys in any combination, with
   * edge values where values go. Most are refused; the refusal's kind is compared.
   */
  def randomNode(d: Int): G[CanonicalValue] = {
    def m(kvs: (String, CanonicalValue)*): CanonicalValue = CanonicalValue.map(kvs.toList).getOrElse(CNull)
    val name = weighted(6 -> pick(NonEmptyVector.fromVectorUnsafe(Op.values.toVector)).map(_.wire), 1 -> pick(names))
    val child = if (d <= 0) pure[CanonicalValue](CanonicalValue.EmptyMap) else defer(randomNode(d - 1))
    weighted(
      2 -> pure(CanonicalValue.EmptyMap),
      2 -> Edge.any(1).map(v => m("lit" -> value(v))),
      2 -> Grammar.worldPath.map(p => m("var" -> text(p))),
      1 -> (Grammar.worldPath, Edge.scalar).mapN((p, v) => m("var" -> text(p), "default" -> value(v))),
      1 -> (child, Edge.scalar).mapN((c, v) => m("path" -> c, "default" -> value(v))),
      4 -> (name, upTo(3, child)).mapN((n, xs) => m("op" -> text(n), "args" -> CanonicalValue.array(xs).getOrElse(CNull))),
      1 -> (name, child).mapN((n, c) => m("spread" -> text(n), "arg" -> c)),
      1 -> upTo(3, child).map(xs => m("arr" -> CanonicalValue.array(xs).getOrElse(CNull))),
      1 -> upTo(3, (Edge.key, child).tupled).map(kvs => m("map" -> CanonicalValue.map(kvs.toList.distinctBy(_._1)).getOrElse(CNull))),
      1 -> upTo(3, (pick(vocabulary), choose(child, Edge.scalar.map(value))).tupled).map(kvs =>
        CanonicalValue.map(kvs.toList.distinctBy(_._1)).getOrElse(CNull)
      )
    )
  }

  /** A random form, version 1 mostly. */
  val randomForm: G[ByteVector] =
    (
      weighted(9 -> pure[CanonicalValue](CInt(1)), 1 -> oneOf[CanonicalValue](CInt(2), text("1"), CNull)),
      between(0, 4).flatMap(randomNode)
    ).mapN { (v, e) =>
      CanonicalValue.map(List("jlvm" -> v, "expr" -> e)).flatMap(Cbor.encode(_)).fold(_ => ByteVector.empty, identity)
    }

  /** A program's canonical form, edited at the value level (then encoded canonically) or at the byte level. */
  def form(p: Program): G[ByteVector] =
    Parser
      .canonical(p)
      .fold(
        _ => pure(ByteVector.empty),
        v =>
          weighted(
            1 -> bounds,
            2 -> randomForm,
            2 -> pure(Cbor.encode(v).fold(_ => ByteVector.empty, identity)),
            4 -> pick(NonEmptyVector(Nil, maps(v))).flatMap(path =>
              formNode(Mutate.at(v, path)).map(n => Cbor.encode(edit(v, path, _ => n)).fold(_ => ByteVector.empty, identity))
            ),
            3 -> bytes(Cbor.encode(v).fold(_ => ByteVector.empty, identity))
          )
      )

  private def at(v: CanonicalValue, path: List[Either[Int, String]]): CanonicalValue =
    path.foldLeft(v) {
      case (CArray(xs), Left(i)) => xs.lift(i).getOrElse(CNull)
      case (m: CMap, Right(k))   => m.entries.getOrElse(k, CNull)
      case (_, _)                => CNull
    }
}
