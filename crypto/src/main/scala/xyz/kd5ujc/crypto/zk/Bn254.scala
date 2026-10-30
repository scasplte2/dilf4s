package xyz.kd5ujc.crypto.zk

import cats.syntax.all._

import scala.annotation.tailrec

import scodec.bits.ByteVector

/**
 * BN254 (alt_bn128) in pure Scala `BigInt`: the base field, G1, G2 on the sextic twist, and the optimal ate pairing,
 * byte-compatible with the EVM precompiles (EIP-196/197).
 *
 * **Verification only.** All arithmetic is variable time, which is safe because it touches only public data (proofs,
 * public inputs, verifying keys). Never use it with secret scalars: proving needs constant-time arithmetic.
 */
object Bn254 {

  val P: BigInt = BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208583")
  val R: BigInt = BigInt("21888242871839275222246405745257275088548364400416034343698204186575808495617")

  private def md(a:  BigInt): BigInt = a.mod(P)
  private def inv(a: BigInt): BigInt = md(a).modPow(P - 2, P)

  // ------------------------------------------------------------------ Fp2 = Fp[i] / (i^2 + 1)

  final case class Fp2(a: BigInt, b: BigInt) {
    def +(o: Fp2): Fp2 = Fp2(md(a + o.a), md(b + o.b))
    def -(o: Fp2): Fp2 = Fp2(md(a - o.a), md(b - o.b))
    def *(o: Fp2): Fp2 = Fp2(md(a * o.a - b * o.b), md(a * o.b + b * o.a))
    def *(k: BigInt): Fp2 = Fp2(md(a * k), md(b * k))
    def unary_- : Fp2 = Fp2(md(-a), md(-b))
    def conj: Fp2 = Fp2(a, md(-b))
    def isZero: Boolean = a == 0 && b == 0

    /** `1 / (a + bi) = (a - bi) / (a^2 + b^2)`; zero maps to zero. */
    def inverse: Fp2 = { val n = inv(a * a + b * b); Fp2(md(a * n), md(-b * n)) }
    def /(o: Fp2): Fp2 = this * o.inverse

    def pow(e: BigInt): Fp2 = {
      @tailrec def go(base: Fp2, e: BigInt, acc: Fp2): Fp2 =
        if (e == 0) acc else go(base * base, e >> 1, if (e.testBit(0)) acc * base else acc)
      go(this, e, Fp2.One)
    }
  }

  object Fp2 {
    val Zero: Fp2 = Fp2(0, 0)
    val One: Fp2 = Fp2(1, 0)
  }

  /** `xi = 9 + i`; the twist is `y^2 = x^3 + 3 / xi`. */
  private val Xi = Fp2(9, 1)
  private val B2: Fp2 = Fp2(3, 0) / Xi
  private val FrobX: Fp2 = Xi.pow((P - 1) / 3)
  private val FrobY: Fp2 = Xi.pow((P - 1) / 2)

  // ------------------------------------------------------------------ Fp12 = Fp[w] / (w^12 - 18 w^6 + 82)

  /** Twelve coefficients of `w^0 .. w^11`; `w^6 = xi`. */
  final case class Fp12(c: Vector[BigInt]) {
    def *(o: Fp12): Fp12 = {
      val wide =
        (0 until 23).toVector.map(k => (math.max(0, k - 11) to math.min(11, k)).foldLeft(BigInt(0))((s, i) => s + c(i) * o.c(k - i)))
      val reduced = (22 to 12 by -1).foldLeft(wide) { (acc, k) =>
        acc.updated(k - 6, acc(k - 6) + 18 * acc(k)).updated(k - 12, acc(k - 12) - 82 * acc(k))
      }
      Fp12(reduced.take(12).map(md))
    }
    def pow(e: BigInt): Fp12 = {
      @tailrec def go(base: Fp12, e: BigInt, acc: Fp12): Fp12 =
        if (e == 0) acc else go(base * base, e >> 1, if (e.testBit(0)) acc * base else acc)
      go(this, e, Fp12.One)
    }
  }

  object Fp12 {
    val One: Fp12 = Fp12(BigInt(1) +: Vector.fill(11)(BigInt(0)))

    /** `a + b i = (a - 9b) + b w^6`, multiplied by `w^shift`. */
    def embed(x: Fp2, shift: Int): Fp12 =
      Fp12(Vector.tabulate(12)(k => if (k == shift) md(x.a - 9 * x.b) else if (k == shift + 6) x.b else BigInt(0)))
    def constant(x: BigInt): Fp12 = Fp12(md(x) +: Vector.fill(11)(BigInt(0)))
    def sum(xs:     Fp12*): Fp12 = Fp12(Vector.tabulate(12)(k => md(xs.map(_.c(k)).sum)))
  }

  // ------------------------------------------------------------------ groups (affine; None is the point at infinity)

  final case class G1(x: BigInt, y: BigInt)
  final case class G2(x: Fp2, y: Fp2)

  def onCurveG1(p: G1): Boolean = md(p.y * p.y) == md(p.x * p.x * p.x + 3)
  def onCurveG2(p: G2): Boolean = p.y * p.y == p.x * p.x * p.x + B2

  def addG1(p: Option[G1], q: Option[G1]): Option[G1] = (p, q) match {
    case (None, _) => q
    case (_, None) => p
    case (Some(a), Some(b)) =>
      if (a.x == b.x && md(a.y + b.y) == 0) None
      else {
        val m = if (a.x == b.x) md(3 * a.x * a.x * inv(2 * a.y)) else md((b.y - a.y) * inv(b.x - a.x))
        val x = md(m * m - a.x - b.x)
        Some(G1(x, md(m * (a.x - x) - a.y)))
      }
  }

  def mulG1(p: Option[G1], k: BigInt): Option[G1] = {
    @tailrec def go(base: Option[G1], e: BigInt, acc: Option[G1]): Option[G1] =
      if (e == 0) acc else go(addG1(base, base), e >> 1, if (e.testBit(0)) addG1(acc, base) else acc)
    go(p, k, None)
  }

  def negG1(p: Option[G1]): Option[G1] = p.map(a => G1(a.x, md(-a.y)))

  def addG2(p: Option[G2], q: Option[G2]): Option[G2] = (p, q) match {
    case (None, _) => q
    case (_, None) => p
    case (Some(a), Some(b)) =>
      if (a.x == b.x && (a.y + b.y).isZero) None
      else {
        val m = if (a.x == b.x) (a.x * a.x * 3) / (a.y * 2) else (b.y - a.y) / (b.x - a.x)
        val x = m * m - a.x - b.x
        Some(G2(x, m * (a.x - x) - a.y))
      }
  }

  def mulG2(p: Option[G2], k: BigInt): Option[G2] = {
    @tailrec def go(base: Option[G2], e: BigInt, acc: Option[G2]): Option[G2] =
      if (e == 0) acc else go(addG2(base, base), e >> 1, if (e.testBit(0)) addG2(acc, base) else acc)
    go(p, k, None)
  }

  /** G2 has a large cofactor: membership needs `r Q = O`. */
  def inSubgroupG2(p: G2): Boolean = mulG2(Some(p), R).isEmpty

  // ------------------------------------------------------------------ optimal ate pairing

  private val AteLoopCount = BigInt("29793968203157093288")
  private val FinalExponent: BigInt = (P.pow(12) - 1) / R

  /** The line through twisted `t1`, `t2` (tangent if equal) evaluated at `p`, as an element of Fp12. */
  private def line(t1: G2, t2: G2, p: G1): Fp12 =
    if (t1.x != t2.x || t1.y == t2.y) {
      val m = if (t1.x != t2.x) (t2.y - t1.y) / (t2.x - t1.x) else (t1.x * t1.x * 3) / (t1.y * 2)
      Fp12.sum(Fp12.embed(m * p.x, 1), Fp12.embed(t1.y - m * t1.x, 3), Fp12.constant(-p.y))
    } else Fp12.sum(Fp12.constant(p.x), Fp12.embed(-t1.x, 2))

  private def frobenius(q: G2): G2 = G2(q.x.conj * FrobX, q.y.conj * FrobY)

  /** The Miller loop for `e(p, q)`, without final exponentiation. */
  def miller(p: G1, q: G2): Fp12 = {
    val (f, r) = (63 to 0 by -1).foldLeft((Fp12.One, q)) {
      case ((f0, r0), i) =>
        val f1 = f0 * f0 * line(r0, r0, p)
        val r1 = addG2(Some(r0), Some(r0)).getOrElse(r0)
        if (AteLoopCount.testBit(i)) (f1 * line(r1, q, p), addG2(Some(r1), Some(q)).getOrElse(r1)) else (f1, r1)
    }
    val q1 = frobenius(q)
    val nq2 = { val f2 = frobenius(q1); G2(f2.x, -f2.y) }
    val f2 = f * line(r, q1, p)
    val r2 = addG2(Some(r), Some(q1)).getOrElse(r)
    f2 * line(r2, nq2, p)
  }

  /** `prod e(p_i, q_i) == 1`; pairs with an infinite point contribute 1. */
  def pairingProductIsOne(pairs: Seq[(Option[G1], Option[G2])]): Boolean =
    pairs.collect { case (Some(p), Some(q)) => miller(p, q) }
      .foldLeft(Fp12.One)(_ * _)
      .pow(FinalExponent) == Fp12.One

  // ------------------------------------------------------------------ EIP-196/197 encodings

  sealed trait ZkError extends Product with Serializable
  object ZkError {
    final case class Malformed(reason: String) extends ZkError
    final case class InvalidPoint(reason: String) extends ZkError
  }

  private def word(b: ByteVector): BigInt = BigInt(1, b.toArray)
  def be32(x:         BigInt): ByteVector = ByteVector.view(x.toByteArray).takeRight(32).padLeft(32)

  private def fieldElement(b: ByteVector, role: String): Either[ZkError, BigInt] = {
    val x = word(b)
    Either.cond(x < P, x, ZkError.Malformed(s"$role is not a canonical field element"))
  }

  /** 64 bytes `x || y`; `(0, 0)` is infinity; otherwise on the curve. */
  def decodeG1(b: ByteVector): Either[ZkError, Option[G1]] =
    for {
      _ <- Either.cond(b.size == 64L, (), ZkError.Malformed(s"G1 point must be 64 bytes, got ${b.size}"))
      x <- fieldElement(b.take(32), "G1 x")
      y <- fieldElement(b.drop(32), "G1 y")
      p <-
        if (x == 0 && y == 0) None.asRight
        else Either.cond(onCurveG1(G1(x, y)), Some(G1(x, y)), ZkError.InvalidPoint("G1 point is not on the curve"))
    } yield p

  def encodeG1(p: Option[G1]): ByteVector = p.fold(ByteVector.low(64))(a => be32(a.x) ++ be32(a.y))

  /** 128 bytes `x.imag || x.real || y.imag || y.real` (EIP-197 order); on the twist and in the order-r subgroup. */
  def decodeG2(b: ByteVector): Either[ZkError, Option[G2]] =
    for {
      _  <- Either.cond(b.size == 128L, (), ZkError.Malformed(s"G2 point must be 128 bytes, got ${b.size}"))
      xi <- fieldElement(b.take(32), "G2 x.imag")
      xr <- fieldElement(b.slice(32, 64), "G2 x.real")
      yi <- fieldElement(b.slice(64, 96), "G2 y.imag")
      yr <- fieldElement(b.drop(96), "G2 y.real")
      q = G2(Fp2(xr, xi), Fp2(yr, yi))
      p <-
        if (q.x.isZero && q.y.isZero) None.asRight
        else if (!onCurveG2(q)) ZkError.InvalidPoint("G2 point is not on the twist").asLeft
        else Either.cond(inSubgroupG2(q), Some(q), ZkError.InvalidPoint("G2 point is not in the order-r subgroup"))
    } yield p

  /** The ECPAIRING (0x08) precompile: `k * 192` bytes of `(G1, G2)` pairs; returns whether the product is 1. */
  def ecPairing(input: ByteVector): Either[ZkError, Boolean] =
    if (input.size % 192 != 0) ZkError.Malformed(s"pairing input must be a multiple of 192 bytes, got ${input.size}").asLeft
    else input.grouped(192).toList.traverse(c => (decodeG1(c.take(64)), decodeG2(c.drop(64))).tupled).map(pairingProductIsOne)

  /** ECADD (0x06): inputs shorter than 128 bytes are zero-padded, as in the EVM. */
  def ecAdd(input: ByteVector): Either[ZkError, ByteVector] = {
    val in = input.take(128).padRight(128)
    (decodeG1(in.take(64)), decodeG1(in.drop(64))).tupled.map { case (a, b) => encodeG1(addG1(a, b)) }
  }

  /** ECMUL (0x07): a G1 point and a 256-bit scalar; inputs shorter than 96 bytes are zero-padded. */
  def ecMul(input: ByteVector): Either[ZkError, ByteVector] = {
    val in = input.take(96).padRight(96)
    decodeG1(in.take(64)).map(p => encodeG1(mulG1(p, word(in.drop(64)))))
  }
}
