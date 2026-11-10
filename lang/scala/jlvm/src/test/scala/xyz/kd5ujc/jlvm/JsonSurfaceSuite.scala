package xyz.kd5ujc.jlvm

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}
import xyz.kd5ujc.jlvm.JlvmError._

import scodec.bits.hex
import weaver.SimpleIOSuite

/** The JSON surface's conversions: stack-safe at the deepest value, and rendering without converting. */
object JsonSurfaceSuite extends SimpleIOSuite {

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def text(s:   String): CanonicalValue = get(CanonicalValue.text(s))
  private def arr(vs:   CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))
  private def obj(kvs:  (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs))

  /** `leaf` under `depth - 1` levels, alternating arrays and maps, each also holding a byte string and a hex text. */
  private def deep(leaf: CanonicalValue, bytes: CanonicalValue, depth: Int): CanonicalValue =
    (1 until depth).foldLeft(arr(leaf, bytes)) { (v, i) =>
      if (i % 2 == 0) arr(bytes, v) else obj("0xab" -> v, "x" -> bytes)
    }

  pureTest("fromCanonicalJson and toCanonicalJson convert every level of a value MaxDepth deep, and invert each other") {
    val asBytes = deep(CBytes(hex"0102"), CBytes(hex"ff"), CanonicalValue.MaxDepth)
    val asText = deep(text("0x0102"), text("0xff"), CanonicalValue.MaxDepth)
    expect
      .same(CanonicalValue.MaxDepth, asBytes.depth)
      .and(expect.same(asBytes, JsonSurface.fromCanonicalJson(asText)))
      .and(expect.same(Right(asText), JsonSurface.toCanonicalJson(asBytes)))
      .and(expect.same(asBytes.weight, JsonSurface.fromCanonicalJson(asText).weight))
  }

  pureTest("render writes bytes as 0x-hex text at every depth, and parse reads the text back as the value") {
    val asBytes = deep(CBytes(hex"0102"), CBytes(hex"ff"), CanonicalValue.MaxDepth)
    val rendered = JsonSurface.render(asBytes)
    expect
      .same(
        rendered,
        CanonicalJson.render(get(JsonSurface.toCanonicalJson(asBytes))).left.map(e => JlvmError.Malformed(MalformedReason.Render(e)))
      )
      .and(expect.same(Right(asBytes), rendered.flatMap(JsonSurface.parse)))
  }

  pureTest("text in 0x-hex form has no rendering at any depth; as a map key it is only text") {
    val hexText = deep(text("0x01"), CNull, CanonicalValue.MaxDepth)
    val keyed = obj("0x01" -> CInt(1))
    expect(JsonSurface.render(hexText).isLeft, "0x-hex text at the bottom")
      .and(expect.same(Right("""{"0x01":1}"""), JsonSurface.render(keyed)))
      .and(expect.same(keyed, JsonSurface.fromCanonicalJson(keyed)))
  }

  pureTest("toCanonicalJson refuses a value too heavy to render from its weight, before converting it") {
    val dag = (1 to 70).foldLeft(CInt(1): CanonicalValue)((v, _) => arr(v, v))
    expect(JsonSurface.toCanonicalJson(dag).isLeft, "a saturated weight")
  }

  pureTest("a byte string past the rendering limit is refused before its hex is built") {
    val big = CBytes(scodec.bits.ByteVector.fill(10)(1))
    expect
      .same(Left(CanonicalJson.RenderError.TooLarge(23L)), CanonicalJson.renderSurfaceWithin(big, 23L))
      .and(expect.same(Right("\"0x" + "01" * 10 + "\""), CanonicalJson.renderSurfaceWithin(big, 24L)))
  }
}
