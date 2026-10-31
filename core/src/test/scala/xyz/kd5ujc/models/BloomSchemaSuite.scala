package xyz.kd5ujc.models

import xyz.kd5ujc.core.bloom.{BloomFilter, BloomParams}
import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.models.bloom.{BloomFilter => Wire}

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * BloomFilter lives in core, below the generated models, so its codec is hand-written; this pins it byte-for-byte to
 * shared/schemas/bloom.cddl in both directions.
 */
object BloomSchemaSuite extends SimpleIOSuite with Checkers {

  private val items: Gen[List[ByteVector]] = Gen.listOf(Gen.listOfN(8, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_)))
  private val params: Gen[BloomParams] =
    Gen.zip(Gen.choose(6, 12), Gen.choose(1, 32)).map { case (e, k) => BloomParams(1 << e, k).toOption.get }
  implicit private val show: cats.Show[(BloomParams, List[ByteVector])] = cats.Show.fromToString

  test("the hand-written Bloom codec encodes and decodes exactly as the generated bloom-filter codec") {
    forall(Gen.zip(params, items)) {
      case (p, xs) =>
        val f = BloomFilter.of[Sha256](p, xs)
        val handWritten = BloomFilter.encode(f)
        val viaSchema = Wire.codec.encode(Wire(p.bits, p.hashes, f.bits.bytes))
        expect
          .same(viaSchema.map(Cbor.encode), Right(Cbor.encode(handWritten)))
          .and(expect.same(Right(Wire(p.bits, p.hashes, f.bits.bytes)), Wire.codec.decoder.decode(handWritten).left.map(_.message)))
          .and(expect.same(Right(f), BloomFilter.decode[Sha256](handWritten)))
    }
  }
}
