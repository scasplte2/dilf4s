package xyz.kd5ujc.core.hash

import java.math.BigInteger

import cats.{Eq, Show}

import scala.annotation.tailrec

sealed trait PoseidonError extends Product with Serializable

object PoseidonError {
  final case class InputCount(count: Int, max: Int) extends PoseidonError
  final case class NonCanonical(value: BigInt) extends PoseidonError
}

/**
 * An element of the BN254 scalar field, `0 <= value < r`, valid by construction: values enter only through
 * [[Fr.from]] (checked) or [[Fr.reduce]] (mod r), so everything typed `Fr` downstream (Poseidon inputs and outputs,
 * Merkle leaves, siblings, roots) needs no further range checks. Opaque: its `BigInt` is visible only inside [[Fr]].
 */
opaque type Fr = BigInt

object Fr {

  /** The BN254 scalar field modulus `r`. */
  val Modulus: BigInt = BigInt("21888242871839275222246405745257275088548364400416034343698204186575808495617")

  val Zero: Fr = BigInt(0)

  def from(value: BigInt): Either[PoseidonError, Fr] =
    Either.cond(value >= 0 && value < Modulus, value, PoseidonError.NonCanonical(value))

  def reduce(value: BigInt): Fr = value.mod(Modulus)

  extension (f: Fr) def value: BigInt = f

  given Eq[Fr] = Eq.fromUniversalEquals
  given Show[Fr] = Show.show(_.toString)
}

/**
 * Poseidon over the BN254 scalar field, byte-compatible with circomlib / circomlibjs (`poseidon([...])`): x^5 S-box,
 * 8 full rounds, width `t = inputs + 1`, state `[0, in_0, ..]`, output `state[0]`. Widths 2..5 (1..4 inputs): the
 * fixed-arity functions are total; [[hash]] checks only the input count.
 *
 * **Public data only**: `BigInt` arithmetic is variable time. Never hash secrets (note values, nullifier keys) here.
 */
object Poseidon {

  val MaxInputs: Int = 4

  def hash1(a: Fr): Fr = permute(PoseidonConstants.Width2, Vector(BigInt(0), a.value))
  def hash2(a: Fr, b: Fr): Fr = permute(PoseidonConstants.Width3, Vector(BigInt(0), a.value, b.value))
  def hash3(a: Fr, b: Fr, c: Fr): Fr = permute(PoseidonConstants.Width4, Vector(BigInt(0), a.value, b.value, c.value))
  def hash4(a: Fr, b: Fr, c: Fr, d: Fr): Fr = permute(PoseidonConstants.Width5, Vector(BigInt(0), a.value, b.value, c.value, d.value))

  /** Two-to-one compression for Merkle nodes: `poseidon([l, r])`. */
  def compress(l: Fr, r: Fr): Fr = hash2(l, r)

  def hash(inputs: Seq[Fr]): Either[PoseidonError, Fr] =
    PoseidonConstants.byWidth
      .get(inputs.size + 1)
      .toRight(PoseidonError.InputCount(inputs.size, MaxInputs))
      .map(params => permute(params, BigInt(0) +: inputs.toVector.map(_.value)))

  private val R: BigInteger = Fr.Modulus.bigInteger

  /** A width's constants as `BigInteger`, converted once. */
  private final case class Fast(constants: Vector[BigInteger], mds: Vector[Vector[BigInteger]], partialRounds: Int)

  private def converted(p: PoseidonConstants.Params): Fast =
    Fast(p.roundConstants.map(_.bigInteger), p.mds.map(_.map(_.bigInteger)), p.partialRounds)

  /** By width: hashing a `Params` as a key would hash every constant on each call. */
  private val fast: Map[Int, Fast] = PoseidonConstants.byWidth.map { case (w, p) => w -> converted(p) }

  private def pow5(a: BigInteger): BigInteger = {
    val a2 = a.multiply(a).remainder(R)
    a2.multiply(a2).remainder(R).multiply(a).remainder(R)
  }

  /**
   * Reductions are deferred where the bound allows: after adding a round constant an element is below 2r, and a
   * matrix row's sum of t products of a reduced constant with such an element stays far below 2^(2·254+4), so one
   * reduction per row suffices. Every state element is reduced again at the end of each round.
   */
  private def permute(params: PoseidonConstants.Params, state: Vector[BigInt]): Fr = {
    val t = state.size
    val f = fast.getOrElse(t, converted(params))
    val rf = PoseidonConstants.FullRounds
    @tailrec def loop(r: Int, s: Vector[BigInteger]): Vector[BigInteger] =
      if (r >= rf + f.partialRounds) s
      else {
        val full = r < rf / 2 || r >= rf / 2 + f.partialRounds
        val sbox = Vector.tabulate(t) { i =>
          val x = s(i).add(f.constants(r * t + i))
          if (full || i == 0) pow5(x) else x
        }
        loop(r + 1, f.mds.map(row => row.indices.foldLeft(BigInteger.ZERO)((acc, j) => acc.add(row(j).multiply(sbox(j)))).remainder(R)))
      }
    Fr.reduce(BigInt(loop(0, state.map(_.bigInteger)).headOption.getOrElse(BigInteger.ZERO)))
  }
}
