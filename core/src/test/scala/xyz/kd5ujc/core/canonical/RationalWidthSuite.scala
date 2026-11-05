package xyz.kd5ujc.core.canonical

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.syntax.apply._
import cats.syntax.either._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite

/**
 * A rational's numerator and denominator are at most [[CanonicalValue.MaxRationalBits]] wide (canonical-encoding.md §1), so
 * decoding one costs at most one gcd of bounded operands: work linear in the input, counted, never timed.
 */
object RationalWidthSuite extends SimpleIOSuite {

  private val Max = CanonicalValue.MaxRationalBits
  private val Edge = BigInt(1) << Max

  /** `(F(k + 1), F(k))` with `F(k + 1)` exactly `bits` wide: coprime, and the pair whose gcd takes the most steps. */
  private def fibonacci(bits: Int): (BigInt, BigInt) = {
    @tailrec
    def go(a: BigInt, b: BigInt): (BigInt, BigInt) = if (b.bitLength >= bits) (b, a) else go(b, a + b)
    go(BigInt(1), BigInt(1))
  }

  private def rational(n: BigInt, d: BigInt): CanonicalValue =
    CanonicalValue.number(Ratio.of(n, d).fold(e => sys.error(e.toString), identity)).fold(e => sys.error(e.toString), identity)

  /** Tag 30 around `[n, d]`, whatever `n` and `d` are. */
  private def rationalBytes(n: BigInt, d: BigInt): ByteVector =
    hex"d81e82" ++ CanonicalGens.integerBytes(n) ++ CanonicalGens.integerBytes(d)

  private def tooWide(e: Either[CanonicalError, CanonicalValue]): Boolean = e match {
    case Left(RationalTooWide(Max)) => true
    case _                          => false
  }

  /** Decodes through a [[Cbor.lowestTerms]] that counts its calls (each one gcd) and the widest operand it was given. */
  private def counted(bytes: ByteVector): IO[(Either[CanonicalError, CanonicalValue], Long, Long)] =
    IO((new AtomicLong, new AtomicLong)).flatMap {
      case (calls, widest) =>
        val reduce: Cbor.LowestTerms = (n, d) => {
          calls.incrementAndGet()
          widest.accumulateAndGet(math.max(n.bitLength, d.bitLength).toLong, math.max)
          Cbor.lowestTerms(n, d)
        }
        IO(Cbor.decode(bytes, Cbor.DefaultMaxDepth, KeyOrder.Default, Cbor.Numbers.Default.copy(lowestTerms = reduce))).map(r =>
          (r, calls.get, widest.get)
        )
    }

  pureTest("the bound is the widest bit length whose every integer has at most 10,000 digits") {
    expect
      .same(Max + 1, BigInt(10).pow(Ratio.DefaultMaxDecimalScale).bitLength)
      .and(expect(Edge < BigInt(10).pow(Ratio.DefaultMaxDecimalScale), "2^33219 < 10^10000"))
  }

  test("k maximal rationals take exactly k gcds, each of operands within the bound") {
    val (n, d) = fibonacci(Max)
    val k = 8
    val maximal = CanonicalValue.array(Vector.fill(k)(rational(n, d))).fold(e => sys.error(e.toString), identity)
    val bytes = Cbor.encode(maximal)
    counted(bytes).map {
      case (decoded, calls, widest) =>
        expect
          .same((Max, Max - 1), (n.bitLength, d.bitLength))
          .and(expect.same(Right(maximal), decoded))
          .and(expect(calls == k.toLong, s"$calls gcds for $k rationals"))
          .and(expect(widest == Max.toLong, s"the widest operand is $widest bits"))
          .and(expect(bytes.size > k * 8300L, s"each rational takes ${bytes.size / k} bytes"))
    }
  }

  test("a part one bit too wide is refused before any gcd, even when the rational is not in lowest terms either") {
    val cases = List(
      (Edge * 2, Edge + 2),
      (BigInt(2), Edge * 2),
      (-Edge - 1, BigInt(2)),
      (BigInt(1), -Edge * 2)
    )
    cases.foldLeft(IO.pure(success)) {
      case (acc, (n, d)) =>
        (acc, counted(rationalBytes(n, d))).mapN {
          case (soFar, (decoded, calls, _)) =>
            soFar
              .and(expect(tooWide(decoded), s"${n.bitLength} / ${d.bitLength} bits: ${decoded.leftMap(_.productPrefix)}"))
              .and(expect(calls == 0L, s"${n.bitLength} / ${d.bitLength} bits ran $calls gcds"))
        }
    }
  }

  pureTest("the bound is on bitLength, for either sign") {
    val accepted = List((Edge - 1, BigInt(2)), (-Edge, BigInt(3)), (BigInt(2), Edge - 1))
    val refused = List((Edge, BigInt(3)), (-Edge - 1, BigInt(2)), (BigInt(3), Edge))
    accepted
      .foldLeft(success) {
        case (acc, (n, d)) =>
          acc.and(expect(Cbor.decode(rationalBytes(n, d)) == Right(rational(n, d)), s"${n.bitLength} / ${d.bitLength} bits decodes"))
      }
      .and(refused.foldLeft(success) {
        case (acc, (n, d)) => acc.and(expect(tooWide(Cbor.decode(rationalBytes(n, d))), s"${n.bitLength} / ${d.bitLength} bits is refused"))
      })
  }

  pureTest("a wide numerator is refused before the denominator is read") {
    val wide = hex"d81e82" ++ CanonicalGens.integerBytes(Edge) ++ hex"6161"
    val narrow = hex"d81e82016161"
    expect(tooWide(Cbor.decode(wide)), "the numerator's width first")
      .and(expect(Cbor.decode(narrow).swap.exists(_.isInstanceOf[NonCanonical]), "a narrow numerator reads on to the denominator"))
  }

  pureTest("no rational is built that decoding refuses: the constructor checks the width after reducing") {
    def built(n: BigInt, d: BigInt) = Ratio.of(n, d).fold(e => sys.error(e.toString), identity)
    expect
      .same(Left(RationalTooWide(Max)), CanonicalValue.number(built(Edge, 3)))
      .and(expect.same(Left(RationalTooWide(Max)), CanonicalValue.number(built(3, Edge))))
      .and(expect.same(Left(RationalTooWide(Max)), CanonicalValue.number(built(Edge * 2, 6))))
      .and(expect.same(Right(CInt(2)), CanonicalValue.number(built(Edge * 2, Edge))))
      .and(expect.same(Left(RationalTooWide(Max)), Canonical[Ratio].canonical(built(Edge, 3))))
      .and(expect.same(Left(IntegerTooWide(Max)), CanonicalValue.number(built(Edge * Edge, 1))))
  }
}
