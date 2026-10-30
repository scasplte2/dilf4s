package xyz.kd5ujc.core.hash

import cats.syntax.all._

import scala.annotation.tailrec

sealed trait PoseidonError extends Product with Serializable

object PoseidonError {
  final case class InputCount(count: Int, max: Int) extends PoseidonError
  final case class NonCanonical(role: String, value: BigInt) extends PoseidonError
}

/**
 * Poseidon over the BN254 scalar field, byte-compatible with circomlib / circomlibjs (`poseidon([...])`): x^5 S-box,
 * 8 full rounds, width `t = inputs + 1`, state `[0, in_0, ..]`, output `state[0]`. Widths 2..5 (1..4 inputs).
 *
 * **Public data only**: `BigInt` arithmetic is variable time. Never hash secrets (note values, nullifier keys) here.
 */
object Poseidon {

  /** The BN254 scalar field modulus. */
  val R: BigInt = BigInt("21888242871839275222246405745257275088548364400416034343698204186575808495617")
  val MaxInputs: Int = 4

  def isCanonical(x: BigInt): Boolean = x >= 0 && x < R

  def hash(inputs: Seq[BigInt]): Either[PoseidonError, BigInt] =
    for {
      params <- PoseidonConstants.byWidth.get(inputs.size + 1).toRight(PoseidonError.InputCount(inputs.size, MaxInputs))
      _ <- inputs.zipWithIndex.traverse_ { case (x, i) => Either.cond(isCanonical(x), (), PoseidonError.NonCanonical(s"input[$i]", x)) }
    } yield permute(params, BigInt(0) +: inputs.toVector)

  /** `hash([l, r])` for canonical `l`, `r` (width 3): total, for Merkle nodes whose inputs are already field elements. */
  private[kd5ujc] def compressCanonical(l: BigInt, r: BigInt): BigInt = permute(PoseidonConstants.Width3, Vector(BigInt(0), l, r))

  def compress(l: BigInt, r: BigInt): Either[PoseidonError, BigInt] = hash(List(l, r))

  private def pow5(a: BigInt): BigInt = { val a2 = a * a % R; a2 * a2 % R * a % R }

  private def permute(params: PoseidonConstants.Params, state: Vector[BigInt]): BigInt = {
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
    loop(0, state).headOption.getOrElse(BigInt(0))
  }
}
