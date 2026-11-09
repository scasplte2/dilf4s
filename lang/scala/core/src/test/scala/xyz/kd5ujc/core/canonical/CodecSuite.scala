package xyz.kd5ujc.core.canonical

import cats.syntax.either._

import weaver.SimpleIOSuite

object CodecSuite extends SimpleIOSuite {

  private def txt(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  private val two64 = BigInt(1) << 64

  pureTest("CDDL integers follow RFC 8610: uint, nint and int are major types 0, 1 and either") {
    def fits(c: Codec[BigInt], n: BigInt): Boolean = c.encode(n).isRight && c.decoder.decode(CanonicalGens.int(n)).isRight
    expect(fits(Codec.uint, two64 - 1), "2^64 - 1 is a uint")
      .and(expect(!fits(Codec.uint, two64), "2^64 is not a uint"))
      .and(expect(!fits(Codec.uint, -1), "-1 is not a uint"))
      .and(expect(fits(Codec.nint, -two64), "-2^64 is a nint"))
      .and(expect(!fits(Codec.nint, -two64 - 1), "-2^64 - 1 is not a nint"))
      .and(expect(!fits(Codec.nint, 0), "0 is not a nint"))
      .and(expect(fits(Codec.int, -two64) && fits(Codec.int, two64 - 1), "int spans both major types"))
      .and(expect(!fits(Codec.int, two64) && !fits(Codec.int, -two64 - 1), "and no further"))
      .and(expect(Codec.uint.encode(two64).left.exists(_.isInstanceOf[CanonicalError.TypeMismatch]), "a typed error"))
  }

  pureTest("a name the schema lists twice counts once: it cannot make room for an unknown key") {
    val m = CanonicalValue
      .map(List("a" -> CanonicalValue.CInt(1), "unexpected" -> CanonicalValue.CInt(2)))
      .fold(e => sys.error(e.toString), identity)
    val record = CanonicalDecoder.record(List("a", "a"))(f => f.required[BigInt]("a")).decode(m)
    val chosen = Codec.selectAlternative(m, List((List("a"), List("a", "a"), CanonicalDecoder.record(List("a", "a"))(_ => 1.asRight))))
    val fits = CanonicalValue.map(List("a" -> CanonicalValue.CInt(1))).fold(e => sys.error(e.toString), identity)
    expect(record.left.exists(_.reason.startsWith("1 unknown key, the first \"unexpected\"")), s"record: $record")
      .and(expect(chosen.left.exists(_.reason.contains("no alternative")), s"selectAlternative: $chosen"))
      .and(expect.same(Right(BigInt(1)), CanonicalDecoder.record(List("a", "a"))(f => f.required[BigInt]("a")).decode(fits)))
      .and(
        expect.same(
          Right(1),
          Codec.selectAlternative(fits, List((List("a"), List("a", "a"), CanonicalDecoder.record(List("a", "a"))(_ => 1.asRight))))
        )
      )
  }

  pureTest("a refused integer is named by its size, never its digits") {
    val huge = BigInt(1) << 1000000
    val widest = CanonicalGens.int((BigInt(1) << CanonicalValue.MaxIntegerBits) - 1)
    expect
      .same(
        Left("integer of 33219 bits outside 0..18446744073709551615"),
        Codec.uint.decoder.decode(widest).left.map(_.reason)
      )
      .and(expect.same(Left(CanonicalError.TypeMismatch("integer of 1000001 bits outside the schema range")), Codec.uint.encode(huge)))
  }

  pureTest("a refused literal is named by the size of what came instead") {
    expect.same(
      Left("expected 7, got an integer of 33219 bits"),
      Codec.literalInt(7).decoder.decode(CanonicalGens.int((BigInt(1) << CanonicalValue.MaxIntegerBits) - 1)).left.map(_.reason)
    )
  }

  pureTest("a sized text string is measured in UTF-8 bytes (RFC 8610 .size), not UTF-16 units") {
    val codec = Codec.sizedText(2, Some(4))
    // "é" is 1 UTF-16 unit and 2 UTF-8 bytes; "😀" is 2 units and 4 bytes; "éé€" is 3 units and 7 bytes
    expect(codec.decoder.decode(txt("é")).isRight, "2 bytes fits 2..4")
      .and(expect(codec.decoder.decode(txt("😀")).isRight, "4 bytes fits 2..4"))
      .and(expect(codec.decoder.decode(txt("éé€")).isLeft, "7 bytes exceeds 4"))
      .and(expect(codec.decoder.decode(txt("a")).isLeft, "1 byte is below 2"))
      .and(expect(codec.encode("éé€").isLeft, "the encoder refuses what the decoder rejects"))
      .and(expect.same(Right(txt("é")), codec.encode("é")))
  }
}
