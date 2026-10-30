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
  def parse(s: String): Either[JlvmError, VarRef] =
    if (s.isEmpty) Right(Data(Vector.empty))
    else {
      val segments = s.split("\\.", -1).toVector
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
 */
final case class Program private (expr: Expr) {
  def version: Int = Program.Version
}

object Program {
  val Version: Int = 1

  /** Expression nesting: an operation's arguments, an array's items and a map's fields are one level deeper. */
  val MaxDepth: Int = 256

  /**
   * Checks the depth bound and the shapes no surface text can produce, so every program has exactly one surface form:
   * a spread names a [[Op.Variadic]] operation and its argument is not an array expression (that is an argument list).
   */
  def from(expr: Expr): Either[JlvmError, Program] = check(List(expr -> 0)).toLeft(new Program(expr))

  /** A worklist walk, so that checking an unbounded tree cannot itself overflow the stack. */
  @tailrec
  private def check(todo: List[(Expr, Int)]): Option[JlvmError] =
    todo match {
      case Nil                         => scala.None
      case (_, d) :: _ if d > MaxDepth => scala.Some(JlvmError.Limit(s"the program nests deeper than $MaxDepth"))
      case (Expr.Spread(op, _), _) :: _ if !Op.Variadic.contains(op) =>
        scala.Some(JlvmError.Malformed(s"${op.wire} does not spread its argument"))
      case (Expr.Spread(op, Expr.Arr(_)), _) :: _ =>
        scala.Some(JlvmError.Malformed(s"a spread ${op.wire} takes one expression, not an argument list"))
      case (Expr.Apply(_, as), d) :: tl            => check(as.toList.map(_ -> (d + 1)) ::: tl)
      case (Expr.Spread(_, a), d) :: tl            => check((a -> (d + 1)) :: tl)
      case (Expr.VarAt(p, _), d) :: tl             => check((p -> (d + 1)) :: tl)
      case (Expr.Arr(is), d) :: tl                 => check(is.toList.map(_ -> (d + 1)) ::: tl)
      case (Expr.Obj(fs), d) :: tl                 => check(fs.values.toList.map(_ -> (d + 1)) ::: tl)
      case (Expr.Lit(_) | Expr.Var(_, _), _) :: tl => check(tl)
    }
}
