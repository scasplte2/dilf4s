package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8

import cats.Monad
import cats.data.State
import cats.syntax.all._

import scodec.bits.ByteVector

/**
 * Poseidon's constants for BN254, derived from first principles, and the optimised form [[Poseidon]] evaluates.
 *
 * The base constants are drawn as the Poseidon paper specifies (Grassi, Khovratovich, Rechberger, Roy and
 * Schofnegger, eprint 2019/458, Appendix F; the authors' `generate_parameters_grain.sage`): a Grain LFSR seeded with
 * the instance's parameters yields the round constants, by rejection below `r`, and then `2t` values reduced mod `r`
 * that define the Cauchy MDS matrix `1/(x_i + y_j)`.
 *
 * The optimised form is the paper's Appendix B, in the layout of Neptune and circomlibjs's `poseidon_opt`: round
 * constants moved through the linear layers, and each partial round's matrix factored into a sparse one.
 *
 * Test scope: every step that can fail returns an error, and main holds only [[PoseidonConstantsGen]]'s output.
 */
private[hash] object PoseidonDerivation {

  type Matrix = Vector[Vector[BigInt]]

  val R: BigInt = Fr.Modulus
  val FieldBits: Int = 254
  val FullRounds: Int = 8
  val PartialRounds: Map[Int, Int] = Map(2 -> 56, 3 -> 57, 4 -> 56, 5 -> 60)

  /** The 80-bit Grain register: position `i` is bit `i` of `lo` below 64, and bit `i - 64` of `hi` above. */
  final case class Grain(lo: Long, hi: Long)

  private type G[A] = State[Grain, A]

  /** One step: the new bit `b62 ^ b51 ^ b38 ^ b23 ^ b13 ^ b0` enters at position 79 and position 0 leaves. */
  private val step: G[Int] = State { g =>
    def b(i: Int): Int = ((g.lo >>> i) & 1L).toInt
    val next = b(62) ^ b(51) ^ b(38) ^ b(23) ^ b(13) ^ b(0)
    (Grain((g.lo >>> 1) | ((g.hi & 1L) << 63), (g.hi >>> 1) | (next.toLong << 15)), next)
  }

  /** Bits are drawn in pairs: the second is output if the first is 1; otherwise both are discarded. */
  private val bit: G[Int] = Monad[G].tailRecM(())(_ => (step, step).mapN((a, b) => Either.cond(a == 1, b, ())))

  /** `FieldBits` output bits, most significant first. */
  private val bits: G[BigInt] = bit.replicateA(FieldBits).map(_.foldLeft(BigInt(0))((v, b) => (v << 1) + b))

  /** A round constant: draws of `r` or more are discarded. */
  private val roundConstant: G[BigInt] = Monad[G].tailRecM(())(_ => bits.map(v => Either.cond(v < R, v, ())))

  /**
   * The seed, MSB first: field 1 (prime) in 2 bits, S-box 0 (`x^a`) in 4, field size in 12, `t` in 12, the full and
   * partial round counts in 10 each, then 30 ones. The first 160 steps are discarded.
   */
  def seeded(t: Int, partialRounds: Int): Grain = {
    def field(value: Int, width: Int): List[Int] = (width - 1 to 0 by -1).map(i => (value >> i) & 1).toList
    val seed = field(1, 2) ++ field(0, 4) ++ field(FieldBits, 12) ++ field(t, 12) ++ field(FullRounds, 10) ++
      field(partialRounds, 10) ++ List.fill(30)(1)
    val register = seed.zipWithIndex.foldLeft(Grain(0L, 0L)) {
      case (g, (b, i)) => if (i < 64) g.copy(lo = g.lo | (b.toLong << i)) else g.copy(hi = g.hi | (b.toLong << (i - 64)))
    }
    step.replicateA_(160).runS(register).value
  }

  // ---- field and matrix arithmetic mod r

  /** `a^(r-2)`: the inverse of a non-zero `a`, and 0 for 0 (callers rule 0 out). */
  def inverse(a: BigInt): BigInt = a.mod(R).modPow(R - 2, R)

  def apply(m: Matrix, v: Vector[BigInt]): Vector[BigInt] = m.map(row => row.zip(v).map(_ * _).sum.mod(R))

  /** The row vector `v` times `m`. */
  def applyRow(v: Vector[BigInt], m: Matrix): Vector[BigInt] = apply(m.transpose, v)

  def multiply(a: Matrix, b: Matrix): Matrix = a.map(row => applyRow(row, b))

  def identity(n: Int): Matrix = Vector.tabulate(n, n)((i, j) => if (i == j) BigInt(1) else BigInt(0))

  /** Gauss-Jordan elimination. */
  def invert(m: Matrix): Either[String, Matrix] = {
    val n = m.size
    (0 until n).toList
      .foldM(m.zip(identity(n)).map(_ ++ _)) { (a, c) =>
        (c until n).find(r => a(r)(c) != 0).toRight(s"singular matrix (column $c)").map { p =>
          val swapped = a.updated(c, a(p)).updated(p, a(c))
          val inv = inverse(swapped(c)(c))
          val pivot = swapped(c).map(x => (x * inv).mod(R))
          swapped.zipWithIndex.map {
            case (row, r) => if (r == c) pivot else row.zip(pivot).map((x, y) => (x - row(c) * y).mod(R))
          }
        }
      }
      .map(_.map(_.drop(n)))
  }

  // ---- the base constants

  final case class Base(t: Int, partialRounds: Int, roundConstants: Vector[BigInt], mds: Matrix)

  /**
   * The reference script redraws the Cauchy points if they repeat or if some `x_i + y_j = 0`, and redraws the matrix
   * until its invariant-subspace checks (its Algorithms 1-3) pass. Neither is implemented: the first draw is required to
   * be usable, which it is for every width here. The checks are not reimplemented, so a first draw they would reject
   * is not detected.
   */
  def base(t: Int): Either[String, Base] =
    PartialRounds.get(t).toRight(s"no parameters for width $t").flatMap { rp =>
      val draws = (roundConstant.replicateA((FullRounds + rp) * t), bits.map(_.mod(R)).replicateA(2 * t)).tupled
      draws.runA(seeded(t, rp)).value match {
        case (constants, points) =>
          val xs = points.toVector.take(t)
          val ys = points.toVector.drop(t)
          Either.cond(
            points.distinct.size == 2 * t && xs.forall(x => ys.forall(y => (x + y).mod(R) != 0)),
            Base(t, rp, constants.toVector, Vector.tabulate(t, t)((i, j) => inverse(xs(i) + ys(j)))),
            s"width $t: the first Cauchy draw is unusable (the reference script would redraw)"
          )
      }
    }

  /** The permutation as defined, over the base constants: the reference the optimised form is checked against. */
  def referencePermute(b: Base, state: Vector[BigInt]): BigInt = {
    val half = FullRounds / 2
    (0 until FullRounds + b.partialRounds)
      .foldLeft(state) { (s, r) =>
        val full = r < half || r >= half + b.partialRounds
        val added = s.indices.map(i => (s(i) + b.roundConstants(r * b.t + i)).mod(R)).toVector
        apply(b.mds, added.zipWithIndex.map((x, i) => if (full || i == 0) x.modPow(5, R) else x))
      }
      .headOption
      .getOrElse(BigInt(0))
  }

  // ---- the optimised form

  /** The tables of [[PoseidonParams]]: see there for their layout. */
  def optimize(b: Base): Either[String, PoseidonParams] = {
    val t = b.t
    val rp = b.partialRounds
    val half = FullRounds / 2
    def key(r: Int): Vector[BigInt] = b.roundConstants.slice(r * t, (r + 1) * t)
    invert(b.mds).flatMap { inv =>
      // A constant added after a matrix moves before it as M^-1 c. Through a partial round, all but its first element
      // also pass the S-box (which touches only element 0), so they merge into the round's own constant, and only a
      // scalar remains after the S-box. Folding from the last partial round back leaves one vector for the first.
      val (entering, scalars) = (0 until rp).foldLeft((key(half + rp), List.empty[BigInt])) {
        case ((acc, ks), i) =>
          val moved = apply(inv, acc)
          (key(half + rp - 1 - i).zip(moved.updated(0, BigInt(0))).map((a, c) => (a + c).mod(R)), moved(0) :: ks)
      }
      val constants = key(0) ++ (1 until half).flatMap(r => apply(inv, key(r))) ++ apply(inv, entering) ++ scalars ++
        (1 until half).flatMap(r => apply(inv, key(half + rp + r)))
      // X = A·B with B = diag(1, X^) and A sparse: B commutes with a partial round's S-box, so it moves into the
      // previous round's matrix. From the last partial round back; what remains is the matrix before the first.
      (0 until rp).toList
        .foldM((b.mds, List.empty[Vector[BigInt]])) {
          case ((x, sparse), _) =>
            val hat = x.drop(1).map(_.drop(1))
            invert(hat).map { hatInv =>
              val first = x.headOption.getOrElse(Vector.empty)
              val row = first.take(1) ++ applyRow(first.drop(1), hatInv)
              val column = x.drop(1).map(_.headOption.getOrElse(BigInt(0)))
              val diag = Vector.tabulate(t, t)((i, j) => if (i == 0 || j == 0) if (i == j) BigInt(1) else BigInt(0) else hat(i - 1)(j - 1))
              (multiply(diag, b.mds), (row ++ column) :: sparse)
            }
        }
        .map { case (pre, sparse) => PoseidonParams(t, rp, constants.toVector, sparse.flatten.toVector, b.mds, pre) }
    }
  }

  // ---- digests, for comparing against circomlibjs without its files

  /** SHA-256 over every table of a width, in a fixed order, decimal, matrices row by row (`new_i = Σ_j m_ij s_j`). */
  def digest(b: Base, o: PoseidonParams): String =
    Hasher[Sha256]
      .digest(
        ByteVector(
          List(b.roundConstants, b.mds.flatten, o.constants, o.sparse, o.mds.flatten, o.preSparse.flatten)
            .map(_.mkString(","))
            .mkString(";")
            .getBytes(UTF_8)
        )
      )
      .hex
}
