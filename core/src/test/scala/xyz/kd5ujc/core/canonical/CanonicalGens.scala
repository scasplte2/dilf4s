package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import org.scalacheck.Gen
import scodec.bits.ByteVector

object CanonicalGens {

  val bigInt: Gen[BigInt] = Gen.oneOf(
    Gen.choose(-30L, 30L).map(BigInt(_)),
    Gen.long.map(BigInt(_)),
    Gen.choose(60, 200).flatMap(bits => Gen.long.map(seed => BigInt(bits, new scala.util.Random(seed)))).flatMap(n => Gen.oneOf(n, -n, -n - 1))
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
    bigInt.map(CInt(_)),
    ratio.map(CanonicalValue.number),
    text.map(t => CanonicalValue.text(t).fold(e => sys.error(e.toString), identity)),
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => CBytes(ByteVector(bs)))
  )

  def value(depth: Int): Gen[CanonicalValue] =
    if (depth <= 0) scalar
    else
      Gen.frequency(
        4 -> scalar,
        1 -> Gen.choose(0, 4).flatMap(Gen.listOfN(_, value(depth - 1))).map(vs => CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)),
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
    bigInt.map(CInt(_)),
    decimal.map(CanonicalValue.number),
    text.map(t => CanonicalValue.text(t).fold(e => sys.error(e.toString), identity))
  )

  /** Values with a JSON rendering: no bytes, only terminating rationals. */
  def jsonValue(depth: Int): Gen[CanonicalValue] =
    if (depth <= 0) jsonScalar
    else
      Gen.frequency(
        4 -> jsonScalar,
        1 -> Gen.choose(0, 4).flatMap(Gen.listOfN(_, jsonValue(depth - 1))).map(vs => CanonicalValue.array(vs.toVector).fold(e => sys.error(e.toString), identity)),
        1 -> Gen
          .choose(0, 4)
          .flatMap(Gen.listOfN(_, Gen.zip(text, jsonValue(depth - 1))))
          .map(kvs => CanonicalValue.map(kvs.toMap).fold(e => sys.error(e.toString), identity))
      )
}
