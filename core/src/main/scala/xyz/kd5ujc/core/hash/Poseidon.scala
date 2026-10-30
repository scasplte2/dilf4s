package xyz.kd5ujc.core.hash

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

  private val R = Fr.Modulus

  private def pow5(a: BigInt): BigInt = { val a2 = a * a % R; a2 * a2 % R * a % R }

  private def permute(params: PoseidonConstants.Params, state: Vector[BigInt]): Fr = {
    val t = state.size
    val (c, m) = (params.roundConstants, params.mds)
    val (rf, rp) = (PoseidonConstants.FullRounds, params.partialRounds)
    @tailrec def loop(r: Int, s: Vector[BigInt]): Vector[BigInt] =
      if (r >= rf + rp) s
      else {
        val ark = Vector.tabulate(t)(i => (s(i) + c(r * t + i)) % R)
        val sbox = if (r < rf / 2 || r >= rf / 2 + rp) ark.map(pow5) else ark.updated(0, pow5(ark(0)))
        loop(r + 1, m.map(row => row.zip(sbox).foldLeft(BigInt(0)) { case (acc, (mij, x)) => (acc + mij * x) % R }))
      }
    Fr.reduce(loop(0, state).headOption.getOrElse(BigInt(0)))
  }
}
