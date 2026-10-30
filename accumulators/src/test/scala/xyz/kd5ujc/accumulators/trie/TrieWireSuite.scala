package xyz.kd5ujc.accumulators.trie

import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.models.trie.{TrieNode, TrieSegment}

import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * The hashing fast path (NodeCodec, total and allocation-light) must produce exactly the bytes of the schema's
 * generated `trie-node` encoder, so shared/schemas/trie.cddl stays the single definition of node commitments.
 */
object TrieWireSuite extends SimpleIOSuite with Checkers {

  private val bytes: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val segment: Gen[BitVector] = Gen.choose(0, 40).flatMap(n => Gen.listOfN(n, Gen.oneOf(true, false)).map(BitVector.bits))
  private val node = for {
    s <- segment
    v <- Gen.option(bytes)
    l <- Gen.option(bytes)
    r <- Gen.option(bytes)
  } yield (s, v, l, r)
  implicit private val show: cats.Show[(BitVector, Option[ByteVector], Option[ByteVector], Option[ByteVector])] = cats.Show.fromToString
  implicit private val showBits: cats.Show[BitVector] = cats.Show.fromToString

  test("node commitments: the trusted encoder equals the generated trie-node encoder") {
    forall(node) {
      case (s, v, l, r) =>
        val wire = TrieNode(TrieSegment(s.size, s.toByteVector), v, l, r)
        expect.same(TrieNode.codec.encode(wire).map(Cbor.encode), Right(Cbor.encode(NodeCodec.branch(s, v, l, r))))
    }
  }

  test("segments decode only with exact length and zero padding") {
    forall(segment) { s =>
      val ok = TrieSegment(s.size, s.toByteVector)
      val padded = TrieSegment(s.size, s.toByteVector ++ ByteVector(0))
      expect
        .same(Right(s), NodeCodec.fromWire(ok))
        .and(expect(NodeCodec.fromWire(padded).isLeft, "an extra byte"))
        .and(
          expect(
            s.size % 8 == 0 || NodeCodec.fromWire(TrieSegment(s.size, s.padRight(s.size + 8 - s.size % 8).not.toByteVector)).isLeft,
            "non-zero padding"
          )
        )
    }
  }
}
