package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * An amount of gas: non-negative, and saturating at `Long.MaxValue` instead of overflowing (metakit used unchecked
 * `Long` arithmetic), so a pathological cost can only ever exceed the limit, never wrap around below it. The
 * saturation point lies above every limit ([[GasLimit.Max]]), so a limit at the maximum is metered like any other.
 */
opaque type Gas = Long

object Gas {

  val Zero: Gas = 0L

  /** The saturation point, above every limit: never a limit itself. */
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
 * Searching a map (argument 0) for a key (argument 1), as `get`, `has`, `set` and `unset` do: `perLevel`, plus `perBlock`
 * per [[Values.SearchBlock]] of the key's length, for each of the search's [[GasSchedule.levels]] (the map's depth bound,
 * plus one for the key itself, which is compared or measured once more).
 */
final case class Search(perLevel: Gas, perBlock: Gas)

/**
 * Merging maps (`merge` when every argument is a map): each entry of the maps after the first is set into the result,
 * a search of a map of at most all their entries. So `perLevel` per such entry, plus `perBlock` per
 * [[Values.SearchBlock]] of their weight (which bounds their keys' length), for each of [[GasSchedule.levels]] of the
 * entries of all of them.
 */
final case class Merging(perLevel: Gas, perBlock: Gas)

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
  extra:      Vector[Rate] = Vector.empty,
  search:     Option[Search] = scala.None,
  merging:    Option[Merging] = scala.None
) {
  def rate(position: Int): Rate = byArgument.getOrElse(position, Rate(perUnit, unit))

  /** Argument `position` is charged `perUnit` per `unit` instead. */
  def at(position: Int, perUnit: Long, unit: SizeUnit): OpCost =
    copy(byArgument = byArgument.updated(position, Rate(Gas(perUnit), unit)))

  /** Every argument is also charged `perUnit` per `unit`: a second term, such as a quadratic one. */
  def plus(perUnit: Long, unit: SizeUnit): OpCost = copy(extra = extra :+ Rate(Gas(perUnit), unit))

  /** The operation searches its map argument for its key argument ([[Search]]). */
  def searching(perLevel: Long, perBlock: Long): OpCost = copy(search = Option(Search(Gas(perLevel), Gas(perBlock))))

  /** The operation merges its map arguments ([[Merging]]). */
  def mergingMaps(perLevel: Long, perBlock: Long): OpCost = copy(merging = Option(Merging(Gas(perLevel), Gas(perBlock))))
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
   * For a number, the square of its 64-bit limbs, numerator and denominator together (`⌊bits/64⌋` each), plus
   * [[GasSchedule.GcdLimbs]] for a non-integer rational; 0 for anything else. The gcd that normalises a quotient, or a
   * sum or product of rationals, is quadratic in the operands' size, and has a fixed cost of its own.
   */
  case Limbs2

  /** [[Limbs2]] for a non-integer rational, and 0 for an integer: integer sums and products are linear. */
  case RationalLimbs2

  /** The weight of a non-integer rational, and 0 for anything else: comparing rationals multiplies across. */
  case RationalWeight

  /** 1 for every argument, whatever it holds: an operation takes and walks each, even an empty array. */
  case Arguments
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
  callbackRun:    Gas,
  rendering:      Gas
) {
  def cost(op: Op): OpCost = ops.getOrElse(op, OpCost(Gas(1), Gas.Zero, SizeUnit.None))

  /**
   * A path parsed at run time (a computed `var` path, a `missing` key) is split on `.`: charged at `split`'s rate per
   * unit of its weight, before it is parsed. A path has at most as many segments as its weight.
   */
  def pathText(path: CanonicalValue): Gas = cost(Op.Split).perUnit * path.weight

  /**
   * Rendering numbers as decimal text (`cat`, `join`): `rendering` per unit of `(4 + ⌊bits/64⌋)^2`, for the bits of each
   * expansion ([[Values.renderedBits]]). The conversion is quadratic in the expansion, which can be far larger than the
   * number's weight (`1/2^4095` weighs 66 and renders as 4,097 characters); [[GasSchedule.RenderLimbs]] are the fixed
   * cost of a
   * conversion, fitted with GasCalibration.
   */
  def rendered(numbers: Vector[CanonicalValue]): Gas =
    numbers.foldLeft(Gas.Zero)((g, v) => g + rendering * GasSchedule.square(GasSchedule.RenderLimbs + Values.renderedBits(v) / 64))

  /** The size charge of `op` on its evaluated arguments: each argument at its position's rate. */
  def sized(op: Op, args: Vector[CanonicalValue]): Gas = {
    val c = cost(op)
    val each = args.zipWithIndex.foldLeft(Gas.Zero) {
      case (total, (v, i)) => (c.rate(i) +: c.extra).foldLeft(total)((t, r) => t + r.perUnit * GasSchedule.units(r.unit, v))
    }
    val searched = c.search.fold(each)(s => each + GasSchedule.searched(s, args))
    c.merging.fold(searched)(mg => searched + GasSchedule.merged(mg, args))
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
      case SizeUnit.Arguments      => 1L
      case SizeUnit.RationalWeight =>
        v match {
          case CanonicalValue.CRational(_) => v.weight
          case _                           => 0L
        }
    }

  /** The fixed cost of a decimal conversion, in limbs ([[GasSchedule.rendered]]). */
  val RenderLimbs: Long = 6L

  /** The fixed cost of a gcd, in limbs ([[SizeUnit.Limbs2]]): measured on rationals of 256 bits and up. */
  val GcdLimbs: Long = 8L

  private def limbs(v: CanonicalValue, integers: Boolean): Long =
    v match {
      case CanonicalValue.CInt(i) if integers => square((i.bitLength / 64).toLong)
      case CanonicalValue.CRational(r)        => square(GcdLimbs + r.numerator.bitLength / 64 + r.denominator.bitLength / 64)
      case _                                  => 0L
    }

  private[jlvm] def square(n: Long): Long = n * n

  /**
   * A map's depth bound: `⌈log2(n + 1)⌉` for `n` entries, the height of a perfectly balanced tree. Scala's red-black
   * tree is at most twice that and Rust's AVL tree at most 1.45 times; the search rates absorb the factor.
   */
  def depth(entries: Long): Long = 64L - java.lang.Long.numberOfLeadingZeros(entries.max(0L))

  /** The levels a search of a map of `entries` entries pays for: its [[depth]], plus one for the key itself. */
  def levels(entries: Long): Long = depth(entries) + 1L

  /** [[Search]]'s charge on an operation's arguments: 0 unless argument 0 is a map and argument 1 text. */
  def searched(s: Search, args: Vector[CanonicalValue]): Gas =
    args match {
      case CanonicalValue.CMap(m) +: CanonicalValue.CText(k) +: _ =>
        (s.perLevel + s.perBlock * (k.length / Values.SearchBlock).toLong) * levels(m.size.toLong)
      case _ => Gas.Zero
    }

  /** [[Merging]]'s charge on an operation's arguments: 0 unless every one is a map. */
  def merged(mg: Merging, args: Vector[CanonicalValue]): Gas = {
    val maps = args.collect { case m: CanonicalValue.CMap => m }
    if (args.isEmpty || maps.size != args.size) Gas.Zero
    else {
      val total = maps.foldLeft(0L)((n, m) => n + m.entries.size.toLong)
      val later = maps.drop(1)
      val entries = later.foldLeft(0L)((n, m) => n + m.entries.size.toLong)
      val weight = later.foldLeft(Gas.Zero)((g, m) => g + Gas(m.weight))
      (mg.perLevel * entries + mg.perBlock * (weight.value / Values.SearchBlock)) * levels(total)
    }
  }

  private def c(base: Long, perUnit: Long = 0, unit: SizeUnit = SizeUnit.None): OpCost = OpCost(Gas(base), Gas(perUnit), unit)

  import Op.{Map => MapOp, None => NoneOp, Set => SetOp, Some => SomeOp, _}
  import SizeUnit.{Arguments, Elements, Limbs2, RationalLimbs2, RationalWeight, Weight}

  /**
   * Version 1. Everything that reads content (comparison, arithmetic, text, bytes, hashing, signature messages) pays by
   * weight, and everything that walks a collection pays per element.
   *
   * Priced against `secp256k1_verify`, the anchor (7,318 gas for an 18-byte message): each charge is at least the work's
   * time as a ratio to the anchor's, times the anchor's charge, on the slower of the two node backends (the JVM and
   * Rust), measured with `benchmarks` GasRatios (docs/specs/jlvm.md §7). A per-unit rate of 1 is the floor for work that
   * grows with its input, even where the measured rate is lower.
   */
  val V1: GasSchedule = GasSchedule(
    version = 1,
    ops = scala.collection.immutable.Map(
      If       -> c(20),
      And      -> c(15),
      Or       -> c(15),
      Not      -> c(25),
      Truthy   -> c(25),
      Default  -> c(15),
      Let      -> c(30),
      Eq       -> c(50, 1, Weight),
      Ne       -> c(50, 1, Weight),
      StrictEq -> c(50, 1, Weight),
      StrictNe -> c(50, 1, Weight),
      Lt       -> c(65, 1, Weight).plus(14, RationalWeight),
      Le       -> c(65, 1, Weight).plus(14, RationalWeight),
      Gt       -> c(65, 1, Weight).plus(14, RationalWeight),
      Ge       -> c(65, 1, Weight).plus(14, RationalWeight),
      // integer sums and products are linear; rational ones, and every quotient, normalise through a quadratic gcd
      Add         -> c(40, 4, Weight).plus(7, RationalLimbs2).plus(40, Elements),
      Sub         -> c(40, 3, Weight).plus(7, RationalLimbs2).plus(60, Elements),
      Mul         -> c(40, 10, Weight).plus(7, RationalLimbs2).plus(40, Elements),
      Div         -> c(340, 2, Weight).plus(9, Limbs2),
      Mod         -> c(340, 2, Weight).plus(9, Limbs2).plus(8, RationalLimbs2),
      Min         -> c(30, 1, Weight).plus(14, RationalWeight).plus(35, Elements),
      Max         -> c(30, 1, Weight).plus(14, RationalWeight).plus(35, Elements),
      Abs         -> c(60).plus(2, RationalLimbs2),
      Round       -> c(60, 8, Weight),
      RoundHalfUp -> c(140, 8, Weight),
      Floor       -> c(60, 8, Weight),
      Ceil        -> c(60, 8, Weight),
      // priced at its worst case, a 4096-bit rational result: the result's size is not known from the arguments
      Pow       -> c(20000),
      MapOp     -> c(40, 15, Elements),
      Filter    -> c(40, 15, Elements),
      Reduce    -> c(40, 8, Elements),
      All       -> c(40, 8, Elements),
      SomeOp    -> c(40, 8, Elements),
      NoneOp    -> c(40, 8, Elements),
      Find      -> c(40, 8, Elements),
      Count     -> c(45, 2, Elements),
      Merge     -> c(210, 8, Weight).mergingMaps(12, 2),
      In        -> c(70, 1, Weight).plus(1, Elements),
      Intersect -> c(50, 17, Weight),
      Unique    -> c(60, 23, Weight),
      Slice     -> c(120, 7, Elements),
      Reverse   -> c(55, 12, Elements),
      Flatten   -> c(70, 7, Weight),
      Length    -> c(45, 1, Weight),
      // each piece of cat and join (and each element cat concatenates) costs a few gas besides its bytes
      Cat        -> c(65, 1, Weight).plus(4, Elements).plus(14, Arguments),
      Substr     -> c(75, 1, Weight),
      Lower      -> c(45, 1, Weight),
      Upper      -> c(50, 1, Weight),
      Join       -> c(110, 1, Weight).plus(14, Elements),
      Split      -> c(85, 6, Weight),
      Trim       -> c(75, 1, Weight),
      StartsWith -> c(50, 1, Weight),
      EndsWith   -> c(50, 1, Weight),
      Keys       -> c(130, 5, Weight),
      Values     -> c(80, 7, Weight),
      Entries    -> c(160, 11, Weight),
      Get        -> c(40).searching(3, 1),
      Has        -> c(40).searching(3, 1),
      // a search per level of the map's depth bound (Search); the JVM's set and unset search twice, and Rust's set
      // on a map of 2^20 entries misses the cache at every level
      SetOp       -> c(60).searching(18, 2),
      Unset       -> c(60).searching(18, 2),
      Exists      -> c(60, 1, Elements),
      Missing     -> c(65, 20, Elements),
      MissingSome -> c(80, 20, Elements),
      TypeOf      -> c(60),
      Hex         -> c(45, 1, Weight),
      Unhex       -> c(80, 2, Weight),
      HexToInt    -> c(80, 4, Weight),
      BytesToInt  -> c(45, 1, Weight),
      IntToBytes  -> c(70, 2, Weight),
      Sha256      -> c(95, 1, Weight),
      Sha3_256    -> c(130, 1, Weight),
      Keccak256   -> c(150, 1, Weight),
      Blake2b256  -> c(115, 1, Weight),
      // the anchor: libsecp256k1 (native, JNI) on the JVM, k256 on Rust. Ed25519 (ZIP-215) is the pure-Scala
      // edwards25519 core on the JVM, its slowest node; BLS is blst (JNI) on the JVM and zkcrypto's bls12_381 on Rust
      Ed25519Verify   -> c(20000, 1, Weight),
      Secp256k1Verify -> c(7200, 1, Weight),
      Bip340Verify    -> c(9000, 1, Weight),
      Ecrecover       -> c(32500),
      BlsVerify       -> c(290000, 1, Weight),
      // a key is an element; the message is hashed to G2, so it pays the hashing rate by weight, as bls_verify's does
      BlsAggregateVerify -> c(250000, 33000, Elements).at(1, 1, Weight),
      // BN254 and Groth16: gnark (JNI) on the JVM, substrate-bn on Rust, which is the slower (a pairing about 2.3 times
      // the JVM's), so they are priced from Rust, well above EIP-1108. Poseidon is convex in the width, so the linear
      // fit bounds two and three inputs; pmt_verify's elements are its siblings plus three. ecvrf_verify's base pays for
      // 64 hash-to-curve counters: an alpha whose first k counters fail takes about 2^k tries to find (jlvm.md §7)
      Poseidon      -> c(1800, 3900, Elements),
      PmtVerify     -> c(1000, 11000, Elements),
      Groth16Verify -> c(480000, 1, Weight),
      EcvrfVerify   -> c(130000, 1, Weight),
      Bn254Add      -> c(700),
      Bn254Mul      -> c(14500),
      Bn254Pairing  -> c(135000, 470, Weight),
      // BouncyCastle on the JVM; a leaf of proposition and proof weighs ~105, a DH tuple ~215 (4 muls). The message is
      // only hashed: it pays the hashing rate (as blake2b256), not the curve rate
      SchnorrVerify -> c(5000, 450, Weight).at(1, 1, Weight),
      SigmaVerify   -> c(5000, 450, Weight).at(2, 1, Weight),
      // the listing checks are linear in the proof's weight and their bounds: priced from listings of 16 to 2048 entries
      MerkleVerify            -> c(1000, 8, Weight),
      MerkleConsistencyVerify -> c(500, 8, Weight),
      TrieVerify              -> c(500, 15, Weight),
      TriePrefixVerify        -> c(20000, 17, Weight),
      TrieRangeVerify         -> c(20000, 17, Weight)
    ),
    // per node, at the highest of ten JVM processes: a literal in an array up to ~19 (with its element charge); a var
    // read ~25 on a small map and up to 58 on one of 8000 entries (not monotone in the map's size, so one flat charge);
    // a map callback ~40 (with a literal body; map and filter pay for the result they build); an untaken if branch ~11
    // (its condition plus an element)
    literal = Gas(10),
    element = Gas(11),
    varAccess = Gas(70),
    perPathSegment = Gas(4),
    callbackRun = Gas(18),
    // per unit of (6 + bits/64)^2 of a decimal expansion: about 1 gas a unit for the widest numbers (Rust's 1/5^1700
    // the most), and the 6 limbs pay for a small integer's ~100
    rendering = Gas(2)
  )

  val Latest: GasSchedule = V1
}

/**
 * A gas limit for one evaluation: an amount from 0 to [[GasLimit.Max]]. Evaluation refuses one outside that range (a
 * negative one, or a larger one) as `Limit`, with no gas used, before it compiles the program (jlvm.md §7).
 */
final case class GasLimit private (requested: Long) {

  /** The limit as an amount of gas, once [[GasLimit.refusal]] has accepted it. */
  def amount: Gas = Gas(requested)
}

object GasLimit {

  /** A limit of `amount` gas (never negative: [[Gas]] is not). */
  @scala.annotation.targetName("ofGas")
  def apply(amount: Gas): GasLimit = new GasLimit(amount.value)

  /**
   * The limit a host asks for, as given: a negative one is kept, so [[refusal]] refuses it (a host's arithmetic gone
   * below zero) instead of its being taken as 0.
   */
  def of(requested: Long): GasLimit = new GasLimit(requested)

  /** 2^24, Ethereum's per-transaction cap (EIP-7825): about 70 ms of work on the JVM, 140 ms on Rust (jlvm.md §7). */
  val Default: GasLimit = new GasLimit(1L << 24)

  /** 2^53 - 1, the largest limit: exact in every language. */
  val Max: GasLimit = new GasLimit(xyz.kd5ujc.models.limits.JlvmMaxGasLimit)

  /** The refusal of a limit outside [0, [[Max]]]: a negative one, or one over the maximum, named by its bit length. */
  def refusal(limit: GasLimit): Option[JlvmError] =
    if (limit.requested < 0) Some(JlvmError.Limit("a gas limit is negative"))
    else
      Option.unless(limit.requested <= Max.requested)(
        JlvmError.Limit(s"a gas limit of ${64 - java.lang.Long.numberOfLeadingZeros(limit.requested)} bits is over 2^53 - 1")
      )
}

/** The result of an evaluation: the gas it used, **whether it succeeded or failed**, and the value or the error. */
final case class Outcome(gasUsed: Gas, result: Either[JlvmError, CanonicalValue], warnings: Vector[Warning] = Vector.empty)
