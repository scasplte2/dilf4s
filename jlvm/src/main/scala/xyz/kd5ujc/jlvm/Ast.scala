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
  case Blake2b256 extends Op("blake2b256")
  // signatures
  case Ed25519Verify extends Op("ed25519_verify")
  case Secp256k1Verify extends Op("secp256k1_verify")
  case Bip340Verify extends Op("bip340_verify")
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
  // sigma protocols (unaudited; docs/specs/jlvm.md)
  case SchnorrVerify extends Op("schnorr_verify")
  case ProveDlogVerify extends Op("prove_dlog_verify")
  case ProveDhTupleVerify extends Op("prove_dhtuple_verify")
  case SigmaVerify extends Op("sigma_verify")
  // authenticated data structures (dilf4s formats; the successors of metakit's smt/mpt verify ops)
  case MerkleVerify extends Op("merkle_verify")
  case MerkleConsistencyVerify extends Op("merkle_consistency_verify")
  case TrieVerify extends Op("trie_verify")
  case TriePrefixVerify extends Op("trie_prefix_verify")
  case TrieRangeVerify extends Op("trie_range_verify")
}

object Op {
  private val byWire: scala.collection.immutable.Map[String, Op] = Op.values.map(o => o.wire -> o).toMap
  def fromWire(name: String): Option[Op] = byWire.get(name)
}

/** A `var` path: `$name.path` reads a let or callback binding; any other path reads the input data. */
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
  case Apply(op: Op, args: Vector[Expr])
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

  def from(expr: Expr): Either[JlvmError, Program] =
    Either.cond(
      within(List(expr -> 0)),
      new Program(expr),
      JlvmError.Limit(s"the program nests deeper than $MaxDepth")
    )

  /** A worklist walk, so that checking an unbounded tree cannot itself overflow the stack. */
  @tailrec
  private def within(todo: List[(Expr, Int)]): Boolean =
    todo match {
      case Nil                          => true
      case (_, d) :: _ if d > MaxDepth  => false
      case (Expr.Apply(_, as), d) :: tl => within(as.toList.map(_ -> (d + 1)) ::: tl)
      case (Expr.Arr(is), d) :: tl      => within(is.toList.map(_ -> (d + 1)) ::: tl)
      case (Expr.Obj(fs), d) :: tl      => within(fs.values.toList.map(_ -> (d + 1)) ::: tl)
      case _ :: tl                      => within(tl)
    }
}
