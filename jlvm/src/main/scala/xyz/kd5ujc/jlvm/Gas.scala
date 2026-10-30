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

/**
 * What an operation costs: a base charge, plus a charge per unit of input size, where the unit is fixed per operation
 * (elements of a collection, bytes or characters, or 64-bit limbs of a number). Every size charge is taken **before**
 * the work it pays for.
 */
final case class OpCost(base: Gas, perUnit: Gas, unit: SizeUnit)

enum SizeUnit {
  case None, Elements, Bytes, Limbs
}

/**
 * A gas schedule. Schedules are versioned data: a result is a function of the program, the input, the language
 * version and the schedule version, so every evaluation names its schedule (docs/specs/jlvm.md).
 */
final case class GasSchedule(
  version:        Int,
  ops:            Map[Op, OpCost],
  varAccess:      Gas,
  perPathSegment: Gas,
  depthPenalty:   Gas,
  callbackRun:    Gas
) {
  def cost(op: Op): OpCost = ops.getOrElse(op, OpCost(Gas(1), Gas.Zero, SizeUnit.None))
}

object GasSchedule {

  private def c(base: Long, perUnit: Long = 0, unit: SizeUnit = SizeUnit.None): OpCost = OpCost(Gas(base), Gas(perUnit), unit)

  import Op.{Map => MapOp, None => NoneOp, Set => SetOp, Some => SomeOp, _}
  import SizeUnit.{Bytes, Elements, Limbs}

  /**
   * Version 1. Base costs start from metakit's table; size terms now cover the work metakit left unpriced (big numbers by
   * limb, text and bytes by length, hashing and signature messages by byte), and every op that walks a collection pays
   * per element.
   */
  val V1: GasSchedule = GasSchedule(
    version = 1,
    ops = scala.collection.immutable.Map(
      If                      -> c(10),
      And                     -> c(2),
      Or                      -> c(2),
      Not                     -> c(1),
      Truthy                  -> c(1),
      Default                 -> c(5),
      Let                     -> c(10),
      Eq                      -> c(2, 1, Elements),
      Ne                      -> c(2, 1, Elements),
      StrictEq                -> c(2, 1, Elements),
      StrictNe                -> c(2, 1, Elements),
      Lt                      -> c(3, 1, Limbs),
      Le                      -> c(3, 1, Limbs),
      Gt                      -> c(3, 1, Limbs),
      Ge                      -> c(3, 1, Limbs),
      Add                     -> c(5, 1, Limbs),
      Sub                     -> c(5, 1, Limbs),
      Mul                     -> c(8, 2, Limbs),
      Div                     -> c(10, 4, Limbs),
      Mod                     -> c(10, 4, Limbs),
      Min                     -> c(5, 1, Elements),
      Max                     -> c(5, 1, Elements),
      Abs                     -> c(2, 1, Limbs),
      Round                   -> c(3, 2, Limbs),
      RoundHalfUp             -> c(3, 2, Limbs),
      Floor                   -> c(3, 2, Limbs),
      Ceil                    -> c(3, 2, Limbs),
      Pow                     -> c(20, 8, Limbs),
      MapOp                   -> c(10, 1, Elements),
      Filter                  -> c(10, 1, Elements),
      Reduce                  -> c(15, 1, Elements),
      All                     -> c(10, 1, Elements),
      SomeOp                  -> c(10, 1, Elements),
      NoneOp                  -> c(10, 1, Elements),
      Find                    -> c(10, 1, Elements),
      Count                   -> c(5, 1, Elements),
      Merge                   -> c(5, 1, Elements),
      In                      -> c(8, 1, Elements),
      Intersect               -> c(15, 1, Elements),
      Unique                  -> c(20, 2, Elements),
      Slice                   -> c(5, 1, Elements),
      Reverse                 -> c(5, 1, Elements),
      Flatten                 -> c(10, 1, Elements),
      Length                  -> c(1),
      Cat                     -> c(5, 1, Bytes),
      Substr                  -> c(8, 1, Bytes),
      Lower                   -> c(3, 1, Bytes),
      Upper                   -> c(3, 1, Bytes),
      Join                    -> c(10, 1, Bytes),
      Split                   -> c(15, 2, Bytes),
      Trim                    -> c(5, 1, Bytes),
      StartsWith              -> c(5, 1, Bytes),
      EndsWith                -> c(5, 1, Bytes),
      Keys                    -> c(5, 1, Elements),
      Values                  -> c(5, 1, Elements),
      Entries                 -> c(10, 2, Elements),
      Get                     -> c(3),
      Has                     -> c(3),
      SetOp                   -> c(5, 1, Elements),
      Unset                   -> c(5, 1, Elements),
      Exists                  -> c(5, 1, Elements),
      Missing                 -> c(5, 1, Elements),
      MissingSome             -> c(15, 1, Elements),
      TypeOf                  -> c(1),
      Hex                     -> c(5, 1, Bytes),
      Unhex                   -> c(5, 1, Bytes),
      HexToInt                -> c(10, 1, Bytes),
      BytesToInt              -> c(10, 1, Bytes),
      IntToBytes              -> c(10, 1, Limbs),
      Sha256                  -> c(60, 1, Bytes),
      Sha3_256                -> c(60, 1, Bytes),
      Blake2b256              -> c(60, 1, Bytes),
      Ed25519Verify           -> c(30000, 1, Bytes),
      Secp256k1Verify         -> c(30000, 1, Bytes),
      Bip340Verify            -> c(30000, 1, Bytes),
      BlsVerify               -> c(120000, 1, Bytes),
      BlsAggregateVerify      -> c(120000, 15000, Elements),
      Poseidon                -> c(150, 150, Elements),
      PmtVerify               -> c(200, 300, Elements),
      Groth16Verify           -> c(250000, 1, Bytes),
      EcvrfVerify             -> c(50000, 1, Bytes),
      Bn254Add                -> c(500),
      Bn254Mul                -> c(40000),
      Bn254Pairing            -> c(45000, 35000, Elements),
      SchnorrVerify           -> c(45000, 1, Bytes),
      ProveDlogVerify         -> c(45000, 1, Bytes),
      ProveDhTupleVerify      -> c(85000, 1, Bytes),
      SigmaVerify             -> c(45000, 2000, Elements),
      MerkleVerify            -> c(500, 400, Elements),
      MerkleConsistencyVerify -> c(500, 400, Elements),
      TrieVerify              -> c(500, 400, Elements),
      TriePrefixVerify        -> c(1000, 800, Elements),
      TrieRangeVerify         -> c(1000, 800, Elements)
    ),
    varAccess = Gas(2),
    perPathSegment = Gas(1),
    depthPenalty = Gas(5),
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
final case class Outcome(gasUsed: Gas, result: Either[JlvmError, CanonicalValue])
