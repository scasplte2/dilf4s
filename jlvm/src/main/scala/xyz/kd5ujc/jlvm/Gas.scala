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
final case class OpCost(base: Gas, perUnit: Gas, unit: SizeUnit, byArgument: Map[Int, Rate] = Map.empty) {
  def rate(position: Int): Rate = byArgument.getOrElse(position, Rate(perUnit, unit))

  /** Argument `position` is charged `perUnit` per `unit` instead. */
  def at(position: Int, perUnit: Long, unit: SizeUnit): OpCost =
    copy(byArgument = byArgument.updated(position, Rate(Gas(perUnit), unit)))
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
      case (total, (v, i)) =>
        val r = c.rate(i)
        val units = r.unit match {
          case SizeUnit.None     => 0L
          case SizeUnit.Elements => Values.elements(v)
          case SizeUnit.Weight   => v.weight
        }
        total + r.perUnit * units
    }
  }
}

object GasSchedule {

  private def c(base: Long, perUnit: Long = 0, unit: SizeUnit = SizeUnit.None): OpCost = OpCost(Gas(base), Gas(perUnit), unit)

  import Op.{Map => MapOp, None => NoneOp, Set => SetOp, Some => SomeOp, _}
  import SizeUnit.{Elements, Weight}

  /**
   * Version 1. Base costs start from metakit's table; size terms now cover the work metakit left unpriced: everything
   * that reads content (comparison, arithmetic, text, bytes, hashing, signature messages) pays by weight, and
   * everything that walks a collection pays per element.
   */
  val V1: GasSchedule = GasSchedule(
    version = 1,
    ops = scala.collection.immutable.Map(
      If                 -> c(10),
      And                -> c(2),
      Or                 -> c(2),
      Not                -> c(1),
      Truthy             -> c(1),
      Default            -> c(5),
      Let                -> c(10),
      Eq                 -> c(2, 1, Weight),
      Ne                 -> c(2, 1, Weight),
      StrictEq           -> c(2, 1, Weight),
      StrictNe           -> c(2, 1, Weight),
      Lt                 -> c(3, 1, Weight),
      Le                 -> c(3, 1, Weight),
      Gt                 -> c(3, 1, Weight),
      Ge                 -> c(3, 1, Weight),
      Add                -> c(5, 1, Weight),
      Sub                -> c(5, 1, Weight),
      Mul                -> c(8, 2, Weight),
      Div                -> c(10, 4, Weight),
      Mod                -> c(10, 4, Weight),
      Min                -> c(5, 1, Weight),
      Max                -> c(5, 1, Weight),
      Abs                -> c(2, 1, Weight),
      Round              -> c(3, 2, Weight),
      RoundHalfUp        -> c(3, 2, Weight),
      Floor              -> c(3, 2, Weight),
      Ceil               -> c(3, 2, Weight),
      Pow                -> c(20, 8, Weight),
      MapOp              -> c(10, 1, Elements),
      Filter             -> c(10, 1, Elements),
      Reduce             -> c(15, 1, Elements),
      All                -> c(10, 1, Elements),
      SomeOp             -> c(10, 1, Elements),
      NoneOp             -> c(10, 1, Elements),
      Find               -> c(10, 1, Elements),
      Count              -> c(5, 1, Elements),
      Merge              -> c(5, 1, Weight),
      In                 -> c(8, 1, Weight),
      Intersect          -> c(15, 1, Weight),
      Unique             -> c(20, 2, Weight),
      Slice              -> c(5, 1, Elements),
      Reverse            -> c(5, 1, Elements),
      Flatten            -> c(10, 1, Weight),
      Length             -> c(1, 1, Weight),
      Cat                -> c(5, 1, Weight),
      Substr             -> c(8, 1, Weight),
      Lower              -> c(3, 1, Weight),
      Upper              -> c(3, 1, Weight),
      Join               -> c(10, 1, Weight),
      Split              -> c(15, 2, Weight),
      Trim               -> c(5, 1, Weight),
      StartsWith         -> c(5, 1, Weight),
      EndsWith           -> c(5, 1, Weight),
      Keys               -> c(5, 1, Weight),
      Values             -> c(5, 1, Weight),
      Entries            -> c(10, 2, Weight),
      Get                -> c(3),
      Has                -> c(3),
      SetOp              -> c(5, 1, Elements),
      Unset              -> c(5, 1, Elements),
      Exists             -> c(5, 1, Elements),
      Missing            -> c(5, 1, Elements),
      MissingSome        -> c(15, 1, Elements),
      TypeOf             -> c(1),
      Hex                -> c(5, 1, Weight),
      Unhex              -> c(5, 1, Weight),
      HexToInt           -> c(10, 1, Weight),
      BytesToInt         -> c(10, 1, Weight),
      IntToBytes         -> c(10, 1, Weight),
      Sha256             -> c(60, 1, Weight),
      Sha3_256           -> c(60, 1, Weight),
      Blake2b256         -> c(60, 1, Weight),
      Ed25519Verify      -> c(30000, 1, Weight),
      Secp256k1Verify    -> c(30000, 1, Weight),
      Bip340Verify       -> c(30000, 1, Weight),
      BlsVerify          -> c(120000, 1, Weight),
      BlsAggregateVerify -> c(120000, 15000, Elements),
      Poseidon           -> c(150, 150, Elements),
      PmtVerify          -> c(200, 300, Elements),
      Groth16Verify      -> c(250000, 1, Weight),
      EcvrfVerify        -> c(50000, 1, Weight),
      Bn254Add           -> c(500),
      Bn254Mul           -> c(40000),
      Bn254Pairing       -> c(45000, 180, Weight),
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
    literal = Gas(1),
    element = Gas(1),
    varAccess = Gas(2),
    perPathSegment = Gas(1),
    callbackRun = Gas(1)
  )

  val Latest: GasSchedule = V1
}

/** A gas limit for one evaluation. */
final case class GasLimit(amount: Gas)

object GasLimit {
  val Default: GasLimit = GasLimit(Gas(1000000L))
}

/** The result of an evaluation: the gas it used, **whether it succeeded or failed**, and the value or the error. */
final case class Outcome(gasUsed: Gas, result: Either[JlvmError, CanonicalValue], warnings: Vector[Warning] = Vector.empty)
