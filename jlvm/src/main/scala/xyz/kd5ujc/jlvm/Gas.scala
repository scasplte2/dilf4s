package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * An amount of gas: non-negative, and saturating at `Long.MaxValue` instead of overflowing (metakit used unchecked
 * `Long` arithmetic), so a pathological cost can only ever exceed the limit, never wrap around below it.
 */
opaque type Gas = Long

object Gas {

  val Zero: Gas = 0L
  val Max: Gas = Long.MaxValue

  def apply(amount: Long): Gas = if (amount < 0) 0L else amount

  extension (g: Gas) {
    def value: Long = g
    def +(h:  Gas): Gas = if (g > Long.MaxValue - h) Long.MaxValue else g + h
    def *(n:  Long): Gas = if (n <= 0 || g == 0) 0L else if (g > Long.MaxValue / n) Long.MaxValue else g * n
    def <=(h: Gas): Boolean = g <= h
    def -(h:  Gas): Gas = if (h >= g) 0L else g - h
  }

  given Ordering[Gas] = Ordering.Long
}

/** A charge per unit of an argument's size. */
final case class Rate(perUnit: Gas, unit: SizeUnit)

/**
 * What an operation costs: a base charge, plus a charge per unit of each argument's size. Every argument is charged at
 * the operation's rate, unless `byArgument` gives its position a rate of its own (a message that is only hashed costs
 * far less per byte than a proof that drives curve arithmetic). Every size charge is taken **before** the work it pays
 * for.
 */
final case class OpCost(
  base:       Gas,
  perUnit:    Gas,
  unit:       SizeUnit,
  byArgument: Map[Int, Rate] = Map.empty,
  extra:      Vector[Rate] = Vector.empty
) {
  def rate(position: Int): Rate = byArgument.getOrElse(position, Rate(perUnit, unit))

  /** Argument `position` is charged `perUnit` per `unit` instead. */
  def at(position: Int, perUnit: Long, unit: SizeUnit): OpCost =
    copy(byArgument = byArgument.updated(position, Rate(Gas(perUnit), unit)))

  /** Every argument is also charged `perUnit` per `unit`: a second term, such as a quadratic one. */
  def plus(perUnit: Long, unit: SizeUnit): OpCost = copy(extra = extra :+ Rate(Gas(perUnit), unit))
}

/**
 * How an argument is measured:
 *   - `Elements`: top-level entries of arrays and objects (1 for anything else), for operations that only touch the
 *     outermost collection (structure is shared, never copied);
 *   - `Weight`: [[xyz.kd5ujc.core.canonical.CanonicalValue.weight]], for operations that read content (text, bytes,
 *     numbers, nested values).
 */
enum SizeUnit {
  case None, Elements, Weight

  /**
   * For a number, the square of its 64-bit limbs, numerator and denominator together (`⌊bits/64⌋` each); 0 for anything
   * else. The gcd that normalises a quotient, or a sum or product of rationals, is quadratic in the operands' size.
   */
  case Limbs2

  /** [[Limbs2]] for a non-integer rational, and 0 for an integer: integer sums and products are linear. */
  case RationalLimbs2
}

/**
 * A gas schedule. Schedules are versioned data: a result is a function of the program, the input, the language
 * version and the schedule version, so every evaluation names its schedule (docs/specs/jlvm.md).
 */
final case class GasSchedule(
  version:        Int,
  ops:            Map[Op, OpCost],
  literal:        Gas,
  element:        Gas,
  varAccess:      Gas,
  perPathSegment: Gas,
  callbackRun:    Gas
) {
  def cost(op: Op): OpCost = ops.getOrElse(op, OpCost(Gas(1), Gas.Zero, SizeUnit.None))

  /** The size charge of `op` on its evaluated arguments: each argument at its position's rate. */
  def sized(op: Op, args: Vector[CanonicalValue]): Gas = {
    val c = cost(op)
    args.zipWithIndex.foldLeft(Gas.Zero) {
      case (total, (v, i)) => (c.rate(i) +: c.extra).foldLeft(total)((t, r) => t + r.perUnit * GasSchedule.units(r.unit, v))
    }
  }
}

object GasSchedule {

  /** One argument's size in `unit`. */
  def units(unit: SizeUnit, v: CanonicalValue): Long =
    unit match {
      case SizeUnit.None           => 0L
      case SizeUnit.Elements       => Values.elements(v)
      case SizeUnit.Weight         => v.weight
      case SizeUnit.Limbs2         => limbs(v, integers = true)
      case SizeUnit.RationalLimbs2 => limbs(v, integers = false)
    }

  private def limbs(v: CanonicalValue, integers: Boolean): Long =
    v match {
      case CanonicalValue.CInt(i) if integers => square((i.bitLength / 64).toLong)
      case CanonicalValue.CRational(r)        => square((r.numerator.bitLength / 64 + r.denominator.bitLength / 64).toLong)
      case _                                  => 0L
    }

  private def square(n: Long): Long = n * n

  private def c(base: Long, perUnit: Long = 0, unit: SizeUnit = SizeUnit.None): OpCost = OpCost(Gas(base), Gas(perUnit), unit)

  import Op.{Map => MapOp, None => NoneOp, Set => SetOp, Some => SomeOp, _}
  import SizeUnit.{Elements, Limbs2, RationalLimbs2, Weight}

  /**
   * Version 1. Base costs start from metakit's table; size terms now cover the work metakit left unpriced: everything
   * that reads content (comparison, arithmetic, text, bytes, hashing, signature messages) pays by weight, and
   * everything that walks a collection pays per element.
   */
  val V1: GasSchedule = GasSchedule(
    version = 1,
    // Calibrated at 150 gas/us on the direct driver (benchmarks: GasCalibration), the rate the
    // cryptographic operations are priced at. A per-unit rate of 1 is the floor for work that grows with its input,
    // even where the measured rate is lower.
    ops = scala.collection.immutable.Map(
      If       -> c(20),
      And      -> c(15),
      Or       -> c(15),
      Not      -> c(40),
      Truthy   -> c(40),
      Default  -> c(15),
      Let      -> c(30),
      Eq       -> c(50, 1, Weight),
      Ne       -> c(50, 1, Weight),
      StrictEq -> c(50, 1, Weight),
      StrictNe -> c(50, 1, Weight),
      Lt       -> c(55, 1, Weight),
      Le       -> c(55, 1, Weight),
      Gt       -> c(55, 1, Weight),
      Ge       -> c(55, 1, Weight),
      // integer sums and products are linear; rational ones, and every quotient, normalise through a quadratic gcd
      Add         -> c(140, 9, Weight).plus(8, RationalLimbs2),
      Sub         -> c(140, 10, Weight).plus(8, RationalLimbs2),
      Mul         -> c(100, 17, Weight).plus(10, RationalLimbs2),
      Div         -> c(200, 2, Weight).plus(8, Limbs2),
      Mod         -> c(200, 2, Weight).plus(8, Limbs2),
      Min         -> c(90, 1, Weight),
      Max         -> c(90, 1, Weight),
      Abs         -> c(60),
      Round       -> c(60, 1, Weight),
      RoundHalfUp -> c(60, 1, Weight),
      Floor       -> c(60, 1, Weight),
      Ceil        -> c(60, 1, Weight),
      // priced at its worst case, a 4096-bit rational result: the result's size is not known from the arguments
      Pow        -> c(50000),
      MapOp      -> c(40, 8, Elements),
      Filter     -> c(40, 8, Elements),
      Reduce     -> c(40, 8, Elements),
      All        -> c(40, 8, Elements),
      SomeOp     -> c(40, 8, Elements),
      NoneOp     -> c(40, 8, Elements),
      Find       -> c(40, 8, Elements),
      Count      -> c(45, 8, Elements),
      Merge      -> c(210, 1, Weight),
      In         -> c(70, 1, Weight),
      Intersect  -> c(50, 12, Weight),
      Unique     -> c(50, 18, Weight),
      Slice      -> c(45, 5, Elements),
      Reverse    -> c(55, 9, Elements),
      Flatten    -> c(60, 5, Weight),
      Length     -> c(45, 1, Weight),
      Cat        -> c(65, 1, Weight),
      Substr     -> c(75, 1, Weight),
      Lower      -> c(45, 1, Weight),
      Upper      -> c(50, 1, Weight),
      Join       -> c(110, 2, Weight),
      Split      -> c(85, 6, Weight),
      Trim       -> c(75, 1, Weight),
      StartsWith -> c(50, 1, Weight),
      EndsWith   -> c(50, 1, Weight),
      Keys       -> c(50, 12, Weight),
      Values     -> c(50, 12, Weight),
      Entries    -> c(50, 18, Weight),
      Get        -> c(30),
      Has        -> c(40),
      // one entry changes in O(log n): a map keeps its weight and its values' depths up to date
      SetOp       -> c(105),
      Unset       -> c(65),
      Exists      -> c(50, 1, Elements),
      Missing     -> c(50, 20, Elements),
      MissingSome -> c(60, 20, Elements),
      TypeOf      -> c(45),
      Hex         -> c(45, 1, Weight),
      Unhex       -> c(80, 3, Weight),
      HexToInt    -> c(80, 3, Weight),
      BytesToInt  -> c(45, 1, Weight),
      IntToBytes  -> c(70, 1, Weight),
      Sha256      -> c(80, 1, Weight),
      Sha3_256    -> c(130, 1, Weight),
      Keccak256   -> c(150, 1, Weight),
      Blake2b256  -> c(115, 1, Weight),
      // measured at 150 gas/us: secp256k1 (native) ~48us, BIP-340 ~53us, Ed25519 ~240us, BLS ~1.55ms plus ~170us a key
      Ed25519Verify      -> c(36000, 1, Weight),
      Secp256k1Verify    -> c(7200, 1, Weight),
      Bip340Verify       -> c(8000, 1, Weight),
      Ecrecover          -> c(30000),
      BlsVerify          -> c(233000, 1, Weight),
      BlsAggregateVerify -> c(230000, 25000, Elements),
      // measured at 150 gas/us: BN254 on gnark, a pairing ~480us plus ~25k gas a pair (weight ~195), Groth16 ~1.45ms;
      // ecvrf ~1ms. Close to EIP-1108. Through GasCalibration, Poseidon ~36us for one input and ~107-114us for four
      // (convex in the width, so the linear fit bounds two and three); pmt_verify ~73us a level, whose elements are its
      // siblings plus three.
      Poseidon      -> c(1800, 3900, Elements),
      PmtVerify     -> c(1000, 11000, Elements),
      Groth16Verify -> c(218000, 1, Weight),
      EcvrfVerify   -> c(150000, 1, Weight),
      Bn254Add      -> c(150),
      Bn254Mul      -> c(7700),
      Bn254Pairing  -> c(55000, 128, Weight),
      // ~300us per dlog leaf on BouncyCastle; a leaf of proposition and proof weighs ~105, a DH tuple ~215 (4 muls)
      // the message is only hashed: it pays the hashing rate (as blake2b256), not the curve rate
      SchnorrVerify -> c(5000, 430, Weight).at(1, 1, Weight),
      SigmaVerify   -> c(5000, 430, Weight).at(2, 1, Weight),
      // measured at ~150 gas/us: a 16-digest Merkle path (weight 548) in ~46us, a 129-record trie proof (3495) in ~700us
      MerkleVerify            -> c(500, 13, Weight),
      MerkleConsistencyVerify -> c(500, 13, Weight),
      TrieVerify              -> c(500, 30, Weight),
      TriePrefixVerify        -> c(1000, 30, Weight),
      TrieRangeVerify         -> c(1000, 30, Weight)
    ),
    // per node, measured: a literal in an array ~25, a var read ~29, a callback run ~38 (with its body), an untaken
    // if branch ~34 (its condition plus one element charge)
    literal = Gas(15),
    element = Gas(12),
    varAccess = Gas(18),
    perPathSegment = Gas(1),
    callbackRun = Gas(10)
  )

  val Latest: GasSchedule = V1
}

/** A gas limit for one evaluation. */
final case class GasLimit(amount: Gas)

object GasLimit {

  /** 2^24, Ethereum's per-transaction cap (EIP-7825): about 110ms of work at the schedule's 150 gas/us. */
  val Default: GasLimit = GasLimit(Gas(1L << 24))
}

/** The result of an evaluation: the gas it used, **whether it succeeded or failed**, and the value or the error. */
final case class Outcome(gasUsed: Gas, result: Either[JlvmError, CanonicalValue], warnings: Vector[Warning] = Vector.empty)
