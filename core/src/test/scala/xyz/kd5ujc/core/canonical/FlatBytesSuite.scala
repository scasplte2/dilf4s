package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.core.canonical.CanonicalValue._

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** Canonical byte strings are flat ([[FlatBytes]]) however they were built, and flattening costs nothing when they are. */
object FlatBytesSuite extends SimpleIOSuite {

  private val n = 1 << 14

  /** A rope of `k` one-byte chunks built by prepending, as `x ++ acc` in a loop builds one: as deep as it is long. */
  private def prepended(k: Int): ByteVector = (0 until k).foldLeft(ByteVector.empty)((acc, i) => ByteVector(i.toByte) ++ acc)

  private def appended(k: Int): ByteVector = (0 until k).foldLeft(ByteVector.empty)((acc, i) => acc ++ ByteVector(i.toByte))

  private val ropes: List[(String, ByteVector)] = List(
    "a rope built by prepending" -> prepended(n),
    "a rope built by appending"  -> appended(n),
    "a populated buffer"         -> (ByteVector(3).bufferBy(n) ++ ByteVector.view(Array.fill[Byte](n - 1)(3))),
    "a buffer inside an append"  -> (ByteVector(9) ++ (ByteVector(3).bufferBy(n) ++ ByteVector.view(Array.fill[Byte](n - 2)(3))))
  )

  pureTest("a rope or buffer becomes flat in a canonical value, with the same bytes") {
    ropes.foldLeft(success) {
      case (acc, (name, b)) =>
        val v = CBytes(b)
        acc
          .and(expect(!FlatBytes.isFlat(b), s"$name: not flat to begin with"))
          .and(expect(FlatBytes.isFlat(v.value), s"$name: flat"))
          .and(expect(v.value == b, s"$name: the same bytes"))
    }
  }

  pureTest("an already-flat byte string is kept as it is, not copied") {
    val array = ByteVector.view(Array.fill[Byte](n)(1))
    val slice = array.drop(10).take(100)
    val fill = ByteVector.fill(n.toLong)(2)
    expect(CBytes(array).value eq array, "an array")
      .and(expect(CBytes(slice).value eq slice, "a slice of one"))
      .and(expect(CBytes(fill).value eq fill, "a lazy fill, which stays lazy"))
      .and(expect(CanonicalValue.bytes(array) == CBytes(array), "the factory agrees"))
  }

  pureTest("decoding a rope input gives flat byte strings, views of one array") {
    val value = CanonicalValue.array(Vector(CBytes(ByteVector.fill(5000L)(7)), CBytes(prepended(300)))).toOption.get
    val encoded = CanonicalGens.encoded(value)
    val rope = encoded.grouped(7L).foldLeft(ByteVector.empty)(_ ++ _)
    Cbor.decode(rope) match {
      case Right(decoded @ CArray(Vector(CBytes(a), CBytes(b)))) =>
        expect(decoded == value, "the same value")
          .and(expect(FlatBytes.isFlat(a) && FlatBytes.isFlat(b), "flat"))
          .and(expect(!FlatBytes.isFlat(rope), "the input was a rope"))
      case other => failure(s"decoded $other")
    }
  }

  pureTest("an encoding is one array") {
    val value = CanonicalValue.array(Vector.fill(100)(CBytes(prepended(50)))).toOption.get
    expect(FlatBytes.isFlat(CanonicalGens.encoded(value)))
  }

  pureTest("concat copies its parts once, end to end") {
    val parts = Vector(ByteVector(1, 2), prepended(100), ByteVector.empty, appended(30))
    val joined = FlatBytes.concat(parts)
    expect(FlatBytes.isFlat(joined)).and(expect(joined == parts.foldLeft(ByteVector.empty)(_ ++ _)))
  }
}
