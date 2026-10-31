package xyz.kd5ujc.core.canonical

import weaver.SimpleIOSuite

object CodecSuite extends SimpleIOSuite {

  private def txt(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  private val two64 = BigInt(1) << 64

  pureTest("CDDL integers follow RFC 8610: uint, nint and int are major types 0, 1 and either") {
    def fits(c: Codec[BigInt], n: BigInt): Boolean = c.encode(n).isRight && c.decoder.decode(CanonicalValue.CInt(n)).isRight
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
