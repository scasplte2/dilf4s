package xyz.kd5ujc.jlvm

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * The operations of language version 1, by wire name. A program naming anything else does not parse, so adding an
 * operation in a later version never changes what an existing program means.
 */
enum Op(val wire: String) {
  // control and logic
  case If extends Op("if")
  case And extends Op("and")
  case Or extends Op("or")
  case Not extends Op("!")
  case Truthy extends Op("!!")
  case Default extends Op("default")
  case Let extends Op("let")
  // comparison
  case Eq extends Op("==")
  case Ne extends Op("!=")
  case StrictEq extends Op("===")
  case StrictNe extends Op("!==")
  case Lt extends Op("<")
  case Le extends Op("<=")
  case Gt extends Op(">")
  case Ge extends Op(">=")
  // arithmetic
  case Add extends Op("+")
  case Sub extends Op("-")
  case Mul extends Op("*")
  case Div extends Op("/")
  case Mod extends Op("%")
  case Min extends Op("min")
  case Max extends Op("max")
  case Abs extends Op("abs")
  case Round extends Op("round")
  case RoundHalfUp extends Op("round_half_up")
  case Floor extends Op("floor")
  case Ceil extends Op("ceil")
  case Pow extends Op("pow")
  // arrays (callbacks bind $current, $accumulator, $index)
  case Map extends Op("map")
  case Filter extends Op("filter")
  case Reduce extends Op("reduce")
  case All extends Op("all")
  case Some extends Op("some")
  case None extends Op("none")
  case Find extends Op("find")
  case Count extends Op("count")
  case Merge extends Op("merge")
  case In extends Op("in")
  case Intersect extends Op("intersect")
  case Unique extends Op("unique")
  case Slice extends Op("slice")
  case Reverse extends Op("reverse")
  case Flatten extends Op("flatten")
  case Length extends Op("length")
  // text
  case Cat extends Op("cat")
  case Substr extends Op("substr")
  case Lower extends Op("lower")
  case Upper extends Op("upper")
  case Join extends Op("join")
  case Split extends Op("split")
  case Trim extends Op("trim")
  case StartsWith extends Op("startsWith")
  case EndsWith extends Op("endsWith")
  // maps
  case Keys extends Op("keys")
  case Values extends Op("values")
  case Entries extends Op("entries")
  case Get extends Op("get")
  case Has extends Op("has")
  case Set extends Op("set")
  case Unset extends Op("unset")
  // utility
  case Exists extends Op("exists")
  case Missing extends Op("missing")
  case MissingSome extends Op("missing_some")
  case TypeOf extends Op("typeof")
  // bytes and hashing
  case Hex extends Op("hex")
  case Unhex extends Op("unhex")
  case HexToInt extends Op("hex_to_int")
  case BytesToInt extends Op("bytes_to_int")
  case IntToBytes extends Op("int_to_bytes")
  case Sha256 extends Op("sha256")
  case Sha3_256 extends Op("sha3_256")
  case Keccak256 extends Op("keccak256")
  case Blake2b256 extends Op("blake2b256")
  // signatures
  case Ed25519Verify extends Op("ed25519_verify")
  case Secp256k1Verify extends Op("secp256k1_verify")
  case Bip340Verify extends Op("bip340_verify")
  case Ecrecover extends Op("ecrecover")
  case BlsVerify extends Op("bls_verify")
  case BlsAggregateVerify extends Op("bls_aggregate_verify")
  // zk
  case Poseidon extends Op("poseidon")
  case PmtVerify extends Op("pmt_verify")
  case Groth16Verify extends Op("groth16_verify")
  case EcvrfVerify extends Op("ecvrf_verify")
  case Bn254Add extends Op("bn254_add")
  case Bn254Mul extends Op("bn254_mul")
  case Bn254Pairing extends Op("bn254_pairing")
  // sigma protocols over secp256k1, Ergo-compatible (docs/specs/sigma.md)
  case SchnorrVerify extends Op("schnorr_verify")
  case SigmaVerify extends Op("sigma_verify")
  // authenticated data structures (dilf4s formats; the successors of metakit's smt/mpt verify ops)
  case MerkleVerify extends Op("merkle_verify")
  case MerkleConsistencyVerify extends Op("merkle_consistency_verify")
  case TrieVerify extends Op("trie_verify")
  case TriePrefixVerify extends Op("trie_prefix_verify")
  case TrieRangeVerify extends Op("trie_range_verify")
}

object Op {

  /**
   * Other names for operations, accepted by the parser and normalised to the operation's own name, so they never
   * change a program's identity. They are the names the SDK and documentation apps use.
   */
  val Aliases: scala.collection.immutable.Map[String, Op] =
    scala.collection.immutable.Map("getKey" -> Get, "setKey" -> Set, "deleteKey" -> Unset, "size" -> Length)

  private val byWire: scala.collection.immutable.Map[String, Op] = Op.values.map(o => o.wire -> o).toMap ++ Aliases
  def fromWire(name: String): Option[Op] = byWire.get(name)

  /**
   * The operations written with a single non-array argument to take that value's elements as their arguments
   * (`{"+": {"values": x}}`). Whether an operation spreads is fixed by the syntax, never by an evaluated value.
   */
  val Variadic: scala.collection.immutable.Set[Op] = scala.collection.immutable.Set(Add, Mul, Min, Max, Cat, Merge)
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
      if (segments.exists(_.isEmpty)) Left(JlvmError.Malformed(s"empty segment in var path \"$s\""))
      else
        segments.headOption match {
          case scala.Some(h) if h.startsWith("$") =>
            val name = h.drop(1)
            Either.cond(name.nonEmpty, Bound(name, segments.drop(1)), JlvmError.Malformed(s"empty binding name in \"$s\""))
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

  /** A [[Op.Variadic]] operation whose arguments are the elements of one array value. */
  case Spread(op: Op, arg: Expr)
  case Arr(items: Vector[Expr])
  case Obj(fields: scala.collection.immutable.Map[String, Expr])
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
  val MaxDepth: Int = 256

  /** JSON nesting of the rendered program ([[Parser.render]]), as the canonical JSON parser bounds it. */
  val MaxSurfaceDepth: Int = CanonicalValue.MaxDepth

  /**
   * Checks the depth bound and the shapes no surface text can produce, so every program has exactly one surface form:
   * a spread names a [[Op.Variadic]] operation and its argument is not an array expression (that is an argument list).
   * Checks that the canonical form decodes to this tree, so programs that behave differently never share an identity:
   * a `var` path is what its text reads back as, and no default is null (the canonical form drops null). Then requires
   * the rendered surface and the canonical form to fit the depth bound, so every program has an identity and an exact
   * rendering: an operation takes two canonical levels, so about 127 nest, fewer around deep literals and defaults.
   */
  def from(expr: Expr): Either[JlvmError, Program] =
    for {
      _ <- measure(List(Pending(Vector(expr), 0, 0)), 0L).toLeft(())
      _ <- check(List(expr)).toLeft(())
      surface = Parser.surfaceDepth(expr)
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
      s"the var path $ref has no text form: \"${ref.render}\" reads back as another path; read a key holding '.', " +
      "an empty key or a key starting with '$' with the exact-segment form {\"var\": [[...]]}"
    )

  /**
   * The largest program, in size: one per expression, plus the weight of each literal and default, plus the length of
   * each object key, binding name and path segment, and one per segment. Compilation is linear in the size and uses no gas, so the bound is
   * checked first, by a walk that stops as soon as it is exceeded. The largest conformance programs are under 2^14.
   */
  val MaxSize: Long = 1L << 18

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
  private def measure(todo: List[Pending], seen: Long): Option[JlvmError] =
    todo match {
      case _ if seen > MaxSize => scala.Some(JlvmError.Limit(s"the program is larger than $MaxSize"))
      case Nil                 => scala.None
      case Pending(items, i, d) :: tl =>
        items.lift(i) match {
          case scala.None                    => measure(tl, seen)
          case scala.Some(_) if d > MaxDepth => scala.Some(JlvmError.Limit(s"the program nests deeper than $MaxDepth"))
          case scala.Some(e) =>
            measure(Pending(children(e), 0, d + 1) :: Pending(items, i + 1, d) :: tl, plus(seen, size(e)))
        }
    }

  /**
   * The shapes no surface text can produce. Runs once [[measure]] has bounded the tree, so rendering every path costs
   * at most the size. A worklist walk, so that checking a deep tree cannot itself overflow the stack.
   */
  @tailrec
  private def check(todo: List[Expr]): Option[JlvmError] =
    todo match {
      case Nil => scala.None
      case Expr.Spread(op, _) :: _ if !Op.Variadic.contains(op) =>
        scala.Some(JlvmError.Malformed(s"${op.wire} does not spread its argument"))
      case Expr.Spread(op, Expr.Arr(_)) :: _ =>
        scala.Some(JlvmError.Malformed(s"a spread ${op.wire} takes one expression, not an argument list"))
      case (Expr.Var(_, scala.Some(CanonicalValue.CNull)) | Expr.VarAt(_, scala.Some(CanonicalValue.CNull))) :: _ =>
        scala.Some(JlvmError.Malformed("a var default is never null: the canonical form cannot tell it from no default"))
      case Expr.Var(ref, _) :: _ if !VarRef.parse(ref.render).exists(_ == ref) => scala.Some(unspellable(ref))
      case e :: tl                                                             => check(children(e).toList ::: tl)
    }
}
