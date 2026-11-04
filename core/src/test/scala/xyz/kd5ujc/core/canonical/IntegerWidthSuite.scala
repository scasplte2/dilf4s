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

  pureTest("the bound is a rational part's, the width of 10^10000") {
    expect
      .same(CanonicalValue.MaxRationalBits, Max)
      .and(expect.same(Max, BigInt(10).pow(CanonicalJson.MaxDigits).bitLength))
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
      .and(expect.same(Left(TypeMismatch("integer of 33221 bits outside the schema range")), Codec.uint.encode(Edge)))
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
    val bytes = Cbor.encode(array)
    counted(bytes).map {
      case (decoded, calls, widest) =>
        expect
          .same(Right(array), decoded)
          .and(expect.same(values.size.toLong, calls))
          .and(expect.same(4153L, widest))
          .and(
            expect.same(accepted.map(n => CanonicalGens.integerBytes(n)), accepted.map(n => Cbor.encode(CanonicalGens.int(n)): ByteVector))
          )
    }
  }

  test("the bignum's form and length are checked before its width") {
    // 4154 bytes, the first zero: non-canonical, though a length alone would call it too wide
    val leadingZero = hex"c25a0000103a00" ++ ByteVector.fill(4153)(0xff)
    // a byte string claimed 2^32 - 1 bytes long, with 4 present: truncated, not too wide
    val truncated = hex"c25affffffff" ++ ByteVector.fill(4)(0xff)
    // 4153 bytes with a top byte of 0x0f: 33220 bits, the widest; 0x10: 33221
    val widest = hex"c3591039" ++ ByteVector(0x0f) ++ ByteVector.fill(4152)(0xff)
    val past = hex"c3591039" ++ ByteVector(0x10) ++ ByteVector.fill(4152)(0x00)
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
    val ten = BigInt(10).pow(10000)
    expect
      .same(Right(CanonicalGens.int(ten)), CanonicalJson.parse("1e10000"))
      .and(expect.same(Right(CanonicalGens.int(-ten)), CanonicalJson.parse("-1e10000")))
      .and(expect.same(Right(CanonicalGens.int(ten * 3 / 2)), CanonicalJson.parse("1.5e10000")))
      .and(expect.same(Left(tooWide), CanonicalJson.parse("2e10000")))
      .and(expect.same(Left(tooWide), CanonicalJson.parse("-2e10000")))
      .and(expect.same(Left(tooWide), CanonicalJson.parse("[" + "9" * 10000 + "e10000]")))
      .and(expect.same(List(33220, 33221, 33221), List(ten, ten * 2, -ten * 2 - 1).map(_.bitLength)))
  }

  pureTest("every integer renders when it has at most 10000 digits; 10^10000 up to the bound cannot") {
    val overLong = NotJsonRepresentable("a number of more than 10000 digits or a scale above 10000")
    val nines = BigInt("9" * 10000)
    expect
      .same(Right("9" * 10000), CanonicalJson.render(CanonicalGens.int(nines)))
      .and(expect.same(Right("-" + "9" * 10000), CanonicalJson.render(CanonicalGens.int(-nines))))
      .and(expect.same(Left(overLong), CanonicalJson.render(CanonicalGens.int(nines + 1))))
      .and(expect.same(Left(overLong), CanonicalJson.render(CanonicalGens.int(Edge - 1))))
      .and(expect.same(Left(overLong), CanonicalJson.render(CanonicalGens.int(-Edge))))
  }
}
