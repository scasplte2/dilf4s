package xyz.kd5ujc.core.canonical

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite

/**
 * An integer is at most [[CanonicalValue.MaxIntegerBits]] wide (canonical-encoding.md §1), for either sign: the only
 * constructor checks it, and a decoder refuses a wider bignum from its length, before it builds the integer. Work is
 * counted, never timed.
 */
object IntegerWidthSuite extends SimpleIOSuite {

  private val Max = CanonicalValue.MaxIntegerBits
  private val Edge = BigInt(1) << Max
  private val tooWide = IntegerTooWide(Max)

  /** The widest integers of either sign, and the narrowest past them. */
  private val accepted = List(Edge - 1, -Edge)
  private val refused = List(Edge, -Edge - 1)

  /** Decodes through a magnitude builder that counts its calls and the widest magnitude it built. */
  private def counted(bytes: ByteVector): IO[(Either[CanonicalError, CanonicalValue], Long, Long)] =
    IO((new AtomicLong, new AtomicLong)).flatMap {
      case (calls, widest) =>
        val magnitude: ByteVector => BigInt = b => {
          calls.incrementAndGet()
          widest.accumulateAndGet(b.size, math.max)
          Cbor.Numbers.Default.magnitude(b)
        }
        val numbers = Cbor.Numbers.Default.copy(magnitude = magnitude)
        IO(Cbor.decode(bytes, Cbor.DefaultMaxDepth, KeyOrder.Default, numbers)).map(r => (r, calls.get, widest.get))
    }

  pureTest("the bound is a rational part's, the widest whose every integer has at most 10,000 digits") {
    expect
      .same(CanonicalValue.MaxRationalBits, Max)
      .and(expect.same(Max + 1, BigInt(10).pow(CanonicalJson.MaxDigits).bitLength))
      .and(expect(Edge < BigInt(10).pow(CanonicalJson.MaxDigits), "2^33219 < 10^10000"))
  }

  pureTest("the only constructor checks the width, on bitLength, for either sign") {
    expect
      .same(accepted.map(n => Right(n.bitLength)), accepted.map(n => CanonicalValue.int(n).map(_.value.bitLength)))
      .and(expect.same(refused.map(_ => Left(tooWide)), refused.map(CanonicalValue.int)))
      .and(expect.same(List(Max + 1, Max + 1), refused.map(_.bitLength)))
      .and(expect.same(Right(CInt(Long.MinValue)), CanonicalValue.int(BigInt(Long.MinValue))))
  }

  pureTest("a number that reduces to an integer is checked as one, after reducing") {
    def built(n: BigInt, d: BigInt) = Ratio.of(n, d).fold(e => sys.error(e.toString), identity)
    expect
      .same(Left(tooWide), CanonicalValue.number(built(Edge, 1)))
      .and(expect.same(Left(tooWide), CanonicalValue.number(built(-Edge * 2, 2) - Ratio.integer(1))))
      .and(expect.same(CanonicalValue.int(Edge - 1), CanonicalValue.number(built((Edge - 1) * 3, 3))))
      .and(expect.same(Right(CInt(2)), CanonicalValue.number(built(Edge * 4, Edge * 2))))
      .and(expect.same(Left(tooWide), Canonical[BigInt].canonical(Edge)))
      .and(expect.same(Left(TypeMismatch("integer of 33220 bits outside the schema range")), Codec.uint.encode(Edge)))
      .and(expect.same(Left(tooWide), Codec.intRange(None, None).encode(Edge)))
  }

  test("decoding refuses a bignum one bit too wide, for either tag, before building its magnitude") {
    refused
      .foldLeft(IO.pure(success)) { (acc, n) =>
        acc.flatMap { soFar =>
          counted(CanonicalGens.integerBytes(n)).map {
            case (decoded, calls, _) =>
              soFar
                .and(expect.same(Left(tooWide), decoded))
                .and(expect(calls == 0L, s"${n.bitLength} bits built $calls magnitudes"))
          }
        }
      }
  }

  test("k of the widest integers build exactly k magnitudes, each within the bound, and decode to themselves") {
    val values = (accepted ++ accepted).map(CanonicalGens.int)
    val array = CanonicalValue.array(values.toVector).fold(e => sys.error(e.toString), identity)
    val bytes = CanonicalGens.encoded(array)
    counted(bytes).map {
      case (decoded, calls, widest) =>
        expect
          .same(Right(array), decoded)
          .and(expect.same(values.size.toLong, calls))
          .and(expect.same(4153L, widest))
          .and(
            expect.same(
              accepted.map(n => CanonicalGens.integerBytes(n)),
              accepted.map(n => CanonicalGens.encoded(CanonicalGens.int(n)): ByteVector)
            )
          )
    }
  }

  test("the bignum's form and length are checked before its width") {
    // 4154 bytes, the first zero: non-canonical, though a length alone would call it too wide
    val leadingZero = hex"c25a0000103a00" ++ ByteVector.fill(4153)(0xff)
    // a byte string claimed 2^32 - 1 bytes long, with 4 present: truncated, not too wide
    val truncated = hex"c25affffffff" ++ ByteVector.fill(4)(0xff)
    // 4153 bytes with a top byte of 0x07: 33219 bits, the widest; 0x08: 33220
    val widest = hex"c3591039" ++ ByteVector(0x07) ++ ByteVector.fill(4152)(0xff)
    val past = hex"c3591039" ++ ByteVector(0x08) ++ ByteVector.fill(4152)(0x00)
    for {
      a <- counted(leadingZero)
      b <- counted(truncated)
      c <- counted(widest)
      d <- counted(past)
    } yield
      expect(a._1.swap.exists(_.isInstanceOf[NonCanonical]), s"${a._1.left.map(_.productPrefix)}")
        .and(expect.same(0L, a._2))
        .and(expect(b._1.swap.exists(_.isInstanceOf[UnexpectedEnd]), s"${b._1.left.map(_.productPrefix)}"))
        .and(expect.same(0L, b._2))
        .and(expect.same(Right(CanonicalGens.int(-Edge)), c._1))
        .and(expect.same(Left(tooWide), d._1))
        .and(expect.same(0L, d._2))
  }

  test("a rational part too wide is still RationalTooWide, refused from its length before it is built") {
    counted(hex"d81e82" ++ CanonicalGens.integerBytes(Edge) ++ hex"03").map {
      case (decoded, calls, _) =>
        expect.same(Left(RationalTooWide(CanonicalValue.MaxRationalBits)), decoded).and(expect.same(0L, calls))
    }
  }

  pureTest("JSON reads an integer up to the bound and refuses one past it, for either sign") {
    val refusedTexts = List("9e9999", "-9e9999", "1e10000", "9" * 10000, "[" + "9" * 10000 + "e10000]")
    expect
      .same(accepted.map(n => Right(CanonicalGens.int(n))), accepted.map(n => CanonicalJson.parse(n.toString)))
      .and(expect.same(refused.map(_ => Left(tooWide)), refused.map(n => CanonicalJson.parse(n.toString))))
      .and(expect.same(List.fill(4)(10000), (accepted ++ refused).map(_.abs.toString.length)))
      .and(expect.same(Right(CanonicalGens.int(BigInt(8) * BigInt(10).pow(9999))), CanonicalJson.parse("8e9999")))
      .and(expect.same(refusedTexts.map(_ => Left(tooWide)), refusedTexts.map(CanonicalJson.parse(_))))
  }

  pureTest("a rational whose decimal scale is past the bound is refused from its denominator's factors") {
    val overLong = NotJsonRepresentable("over-long number")
    def rational(d: BigInt) = CanonicalValue.number(Ratio.of(1, d).fold(e => sys.error(e.toString), identity))
    expect
      .same(
        List(Left(CanonicalJson.RenderError.Canonical(overLong)), Left(CanonicalJson.RenderError.Canonical(overLong))),
        List(BigInt(2).pow(33218), BigInt(5).pow(14306)).map(d => rational(d).flatMap(CanonicalJson.render))
      )
      .and(expect.same(Right("0.0001"), rational(BigInt(10000)).flatMap(CanonicalJson.render)))
  }

  pureTest("every integer renders, the widest of either sign with 10,000 digits, and reads back") {
    val rendered = accepted.map(n => CanonicalJson.integerText(CanonicalGens.int(n)))
    expect
      .same(accepted.map(_.toString), rendered)
      .and(expect.same(List(10000, 10000), rendered.map(_.stripPrefix("-").length)))
      .and(expect.same(accepted.map(n => Right(CanonicalGens.int(n))), rendered.map(CanonicalJson.parse(_))))
  }
}
