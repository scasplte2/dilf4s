package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import org.scalacheck.Gen
import scodec.bits.ByteVector

object CanonicalGens {

  /** The integer `n`, which a test knows is within [[CanonicalValue.MaxIntegerBits]]. */
  def int(n: BigInt): CInt = CanonicalValue.int(n).fold(e => sys.error(e.toString), identity)

  /** A test value's canonical CBOR: every one is far within [[Cbor.MaxEncodedBytes]], so a refusal fails the test. */
  def encoded(v: CanonicalValue): FlatBytes = Cbor.encode(v).fold(e => sys.error(e.toString), identity)

  /**
   * The CBOR of the integer `n` in canonical form, of any width: the bytes [[Cbor.encode]] writes when `n` is a value,
   * and for a bignum too wide to be one, the bytes a decoder must refuse.
   */
  def integerBytes(n: BigInt): ByteVector = {
    val twoTo64 = BigInt(1) << 64
    def head(major: Int, argument: BigInt): ByteVector =
      if (argument < 24) ByteVector(major << 5 | argument.toInt)
      else {
        val (info, size) =
          if (argument < 0x100) (24, 1) else if (argument < 0x10000) (25, 2) else if (argument < (BigInt(1) << 32)) (26, 4) else (27, 8)
        ByteVector(major << 5 | info) ++ ByteVector(argument.toByteArray).takeRight(size.toLong).padLeft(size.toLong)
      }
    if (n.signum >= 0 && n < twoTo64) head(0, n)
    else if (n.signum < 0 && n >= -twoTo64) head(1, -1 - n)
    else {
      val (tag, magnitude) = if (n.signum >= 0) (2, n) else (3, -1 - n)
      val bytes = ByteVector(magnitude.toByteArray).dropWhile(_ == 0)
      head(6, tag) ++ head(2, bytes.size) ++ bytes
    }
  }

  val bigInt: Gen[BigInt] = Gen.oneOf(
    Gen.choose(-30L, 30L).map(BigInt(_)),
    Gen.long.map(BigInt(_)),
    Gen
      .choose(60, 200)
      .flatMap(bits => Gen.long.map(seed => BigInt(bits, new scala.util.Random(seed))))
      .flatMap(n => Gen.oneOf(n, -n, -n - 1))
  )

  val ratio: Gen[Ratio] =
    for {
      n <- bigInt
      d <- bigInt.map(_.abs + 1)
    } yield Ratio.of(n, d).fold(e => sys.error(e.toString), identity)

  private val char: Gen[String] = Gen.frequency(
    8 -> Gen.alphaNumChar.map(_.toString),
    2 -> Gen.oneOf("é", "中", "😀", "\u0000", "\"", "\\", "\u007f", "￿", "𐀀")
  )

  val text: Gen[String] = Gen.choose(0, 8).flatMap(n => Gen.listOfN(n, char).map(_.mkString))

  val scalar: Gen[CanonicalValue] = Gen.oneOf(
    Gen.const(CNull),
    Gen.oneOf(True, False),
    bigInt.map(int),
    ratio.map(r => CanonicalValue.number(r).fold(e => sys.error(e.toString), identity)),
    text.map(t => CanonicalValue.text(t).fold(e => sys.error(e.toString), identity)),
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => CBytes(ByteVector(bs)))
  )

  def value(depth: Int): Gen[CanonicalValue] =
    if (depth <= 0) scalar
    else
      Gen.frequency(
        4 -> scalar,
        1 -> Gen
          .choose(0, 4)
          .flatMap(Gen.listOfN(_, value(depth - 1)))
          .map(vs => CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)),
        1 -> Gen
          .choose(0, 4)
          .flatMap(Gen.listOfN(_, Gen.zip(text, value(depth - 1))))
          .map(kvs => CanonicalValue.map(kvs.toMap).fold(e => sys.error(e.toString), identity))
      )

  /** Terminating decimals only: `unscaled / 10^scale`. */
  val decimal: Gen[Ratio] =
    Gen.zip(bigInt, Gen.choose(0, 30)).map { case (u, k) => Ratio.fromDecimal(u, k).fold(e => sys.error(e.toString), identity) }

  private val jsonScalar: Gen[CanonicalValue] = Gen.oneOf(
    Gen.const(CNull),
    Gen.oneOf(True, False),
    bigInt.map(int),
    decimal.map(r => CanonicalValue.number(r).fold(e => sys.error(e.toString), identity)),
    text.map(t => CanonicalValue.text(t).fold(e => sys.error(e.toString), identity))
  )

  /** Values with a JSON rendering: no bytes, only terminating rationals. */
  def jsonValue(depth: Int): Gen[CanonicalValue] =
    if (depth <= 0) jsonScalar
    else
      Gen.frequency(
        4 -> jsonScalar,
        1 -> Gen
          .choose(0, 4)
          .flatMap(Gen.listOfN(_, jsonValue(depth - 1)))
          .map(vs => CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)),
        1 -> Gen
          .choose(0, 4)
          .flatMap(Gen.listOfN(_, Gen.zip(text, jsonValue(depth - 1))))
          .map(kvs => CanonicalValue.map(kvs.toMap).fold(e => sys.error(e.toString), identity))
      )
}
