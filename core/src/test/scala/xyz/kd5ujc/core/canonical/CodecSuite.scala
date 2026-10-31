package xyz.kd5ujc.core.canonical

import weaver.SimpleIOSuite

object CodecSuite extends SimpleIOSuite {

  private def txt(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

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
