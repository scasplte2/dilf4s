package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.hash.{Hasher, Sha256}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object ByteSourceSuite extends SimpleIOSuite {

  pureTest("a source of a hundred thousand concatenations writes and hashes without growing the stack") {
    val n = 100000
    val chained = (0 until n).foldLeft(ByteSource.empty)((src, i) => src ++ ByteSource.byte(i & 0xff))
    val nested = (0 until n).foldLeft(ByteSource.empty)((src, i) => ByteSource.byte(i & 0xff) ++ src)
    val bytes = ByteVector((0 until n).map(i => (i & 0xff).toByte))
    expect(chained.toByteVector == bytes, "appended, in order")
      .and(expect(nested.toByteVector == bytes.reverse, "prepended, in order"))
      .and(expect(Hasher[Sha256].digestOf(chained) == Hasher[Sha256].digest(bytes), "hashed as the bytes"))
  }

  pureTest("a text window ends within the text, without Int overflow near the largest String") {
    val longest = Int.MaxValue
    expect
      .same(longest, Cbor.windowEnd(longest, longest - 10))
      .and(expect.same(longest, Cbor.windowEnd(longest, longest - Cbor.TextWindow)))
      .and(expect.same(longest - 1, Cbor.windowEnd(longest, longest - 1 - Cbor.TextWindow)))
      .and(expect.same(Cbor.TextWindow, Cbor.windowEnd(10 * Cbor.TextWindow, 0)))
      .and(expect.same(7, Cbor.windowEnd(7, 0)))
  }
}
