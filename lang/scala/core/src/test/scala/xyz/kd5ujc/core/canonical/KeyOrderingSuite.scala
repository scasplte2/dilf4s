package xyz.kd5ujc.core.canonical

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/** The encoder orders a map's keys without encoding them: by UTF-8 length, then by code point. */
object KeyOrderingSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(2000)

  private def txt(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  /** Code points either side of every UTF-8 width boundary and of the surrogate range. */
  private val keyChar: Gen[String] =
    Gen.oneOf("a", "z", "\u007f", "\u0080", "é", "߿", "ࠀ", "퟿", "", "￿", "𐀀", "😀", "􏿿")

  private val key: Gen[String] = Gen.choose(0, 6).flatMap(n => Gen.listOfN(n, keyChar).map(_.mkString))

  test("UTF-8 length, then code points, is the bytewise order of the encoded keys") {
    forall(Gen.zip(key, key)) {
      case (a, b) =>
        val (ea, eb) = (BufferedCbor.encode(txt(a)), BufferedCbor.encode(txt(b)))
        val byLength = Utf16.utf8Length(a).compare(Utf16.utf8Length(b))
        val ours = if (byLength != 0) byLength else Cbor.codePointCompare(a, b)
        expect(ours.sign == Cbor.compareBytes(ea, eb).sign, s"${a.toList} vs ${b.toList}: $ours")
    }
  }

  test("a map of such keys encodes as the old encoder, which sorted the encoded keys") {
    forall(Gen.listOfN(30, key)) { keys =>
      val m = CanonicalValue
        .map(keys.zipWithIndex.map { case (k, i) => k -> CanonicalValue.CInt(i) }.toMap)
        .fold(e => sys.error(e.toString), identity)
      expect(CanonicalGens.encoded(m) == BufferedCbor.encode(m))
    }
  }
}
