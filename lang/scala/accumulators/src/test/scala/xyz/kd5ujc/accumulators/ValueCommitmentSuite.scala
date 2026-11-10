package xyz.kd5ujc.accumulators

import cats.Show

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ValueCommitmentSuite extends SimpleIOSuite with Checkers {

  private val value: Gen[CanonicalValue] = Gen.oneOf(
    Gen.alphaNumStr.map(s => CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)),
    Gen.long.map(l => CanonicalValue.CInt(l)),
    Gen.oneOf(CanonicalValue.True, CanonicalValue.False, CanonicalValue.CNull),
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => CanonicalValue.CBytes(ByteVector(bs)))
  )

  private given Show[CanonicalValue] = Show.fromToString

  pureTest("bytes and a value whose CBOR equals them commit differently: 0x6161 is not \"a\"") {
    val a = CanonicalValue.text("a").fold(e => sys.error(e.toString), identity)
    expect
      .same(ByteVector(0x61, 0x61), xyz.kd5ujc.core.canonical.CanonicalGens.encoded(a))
      .and(expect(ValueCommitment.bytes(CanonicalValue.CBytes(ByteVector(0x61, 0x61))) != ValueCommitment.bytes(a), "tagged apart"))
  }

  test("for any value v, the bytes cbor(v) commit differently from v") {
    forall(value) { v =>
      val asBytes = CanonicalValue.CBytes(xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v))
      expect(v == asBytes || ValueCommitment.bytes(v) != ValueCommitment.bytes(asBytes), s"$v collides with its encoding")
    }
  }

  pureTest("a commitment of exactly the limit, tag included, is built; one byte under it is TooLarge with the limit") {
    val t = CanonicalValue.text("abc").fold(e => sys.error(e.toString), identity)
    val b = CanonicalValue.CBytes(ByteVector(1, 2, 3))
    val heavy =
      (1 to 70).foldLeft[CanonicalValue](t)((x, _) => CanonicalValue.array(Vector(x, x)).fold(e => sys.error(e.toString), identity))
    expect
      .same(Right(ByteVector(0x01, 0x63, 0x61, 0x62, 0x63)), ValueCommitment.bytesWithin(t, 5L))
      .and(expect.same(Left(Cbor.TooLarge(4L)), ValueCommitment.bytesWithin(t, 4L)))
      .and(expect.same(Right(ByteVector(0x00, 1, 2, 3)), ValueCommitment.bytesWithin(b, 4L)))
      .and(expect.same(Left(Cbor.TooLarge(3L)), ValueCommitment.bytesWithin(b, 3L)))
      .and(expect.same(Left(Cbor.TooLarge(0L)), ValueCommitment.bytesWithin(t, -1L)))
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), ValueCommitment.bytes(heavy)))
      .and(expect.same(Right(ByteVector(0x01, 0xf6)), ValueCommitment.bytesWithin(CanonicalValue.CNull, Long.MaxValue)))
  }

  pureTest("the streamed commitment refuses what the built one refuses, at the same limit, before it exists") {
    val t = CanonicalValue.text("abc").fold(e => sys.error(e.toString), identity)
    val b = CanonicalValue.CBytes(ByteVector(1, 2, 3))
    val heavy =
      (1 to 70).foldLeft[CanonicalValue](t)((x, _) => CanonicalValue.array(Vector(x, x)).fold(e => sys.error(e.toString), identity))
    val lazyBytes = CanonicalValue.CBytes(ByteVector.low(Long.MaxValue))
    val ceiling = Left(Cbor.TooLarge(Cbor.MaxEncodedBytes))
    def refusal(v: CanonicalValue, limit: Long) = ValueCommitment.sourceWithin(v, limit).map(_ => ())
    List(t -> 5L, t -> 4L, b -> 4L, b -> 3L, t -> -1L, t -> 0L)
      .foldLeft(success) {
        case (acc, (v, limit)) =>
          acc.and(expect(ValueCommitment.bytesWithin(v, limit).map(_ => ()) == refusal(v, limit), s"$v within $limit"))
      }
      .and(expect.same(ceiling, ValueCommitment.source(heavy).map(_ => ())))
      .and(expect.same(ceiling, ValueCommitment.source(lazyBytes).map(_ => ())))
  }

  pureTest("a value whose commitment is past the ceiling is refused, typed, by every hash that takes one") {
    import xyz.kd5ujc.accumulators.merkle.{MerkleHashing, MerkleTree}
    import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieError}
    import xyz.kd5ujc.accumulators.versioned.{VersionedTrie, VersionedTrieError}
    import xyz.kd5ujc.core.canonical.Canonical
    import xyz.kd5ujc.core.hash.Sha256
    given Canonical[CanonicalValue] = Canonical.instance(Right(_))
    given MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]
    val heavy = (1 to 70).foldLeft[CanonicalValue](CanonicalValue.CNull)((x, _) =>
      CanonicalValue.array(Vector(x, x)).fold(e => sys.error(e.toString), identity)
    )
    val trie = KeyMode.fixed(1).map(Trie.empty[Sha256](_)).fold(e => sys.error(e.toString), identity)
    expect
      .same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), MerkleTree.leaf[Sha256, CanonicalValue](heavy))
      .and(expect.same(Left(TrieError.TooLarge(Cbor.MaxEncodedBytes)), trie.putCanonical(ByteVector(1), heavy).map(_ => ())))
      .and(
        expect.same(
          Left(VersionedTrieError.TooLarge("the value's commitment", Cbor.MaxEncodedBytes)),
          VersionedTrie.valueHash[Sha256](heavy)
        )
      )
  }

  pureTest("a lazy byte string whose commitment passes the ceiling by its tag byte is refused before anything is hashed") {
    import java.util.concurrent.atomic.AtomicLong
    import xyz.kd5ujc.accumulators.trie.TrieHashing
    import xyz.kd5ujc.accumulators.versioned.{VersionedTrie, VersionedTrieError}
    import xyz.kd5ujc.core.canonical.ByteSource
    import xyz.kd5ujc.core.hash.{Digest, Hasher, Sha256}
    import scodec.bits.BitVector
    // a host's lazy views: their bytes are never read
    val within = CanonicalValue.CBytes(ByteVector.low(Cbor.MaxEncodedBytes - 1)) // the tag and these: the ceiling exactly
    val past = CanonicalValue.CBytes(ByteVector.low(Cbor.MaxEncodedBytes)) // one byte past it
    val hashed = new AtomicLong(0L)
    val inner = TrieHashing.tagged[Sha256]
    given TrieHashing[Sha256] = new TrieHashing[Sha256] {
      val hasher: Hasher[Sha256] = inner.hasher
      val empty: Digest[Sha256] = inner.empty
      def valueOf(bytes: ByteSource): Digest[Sha256] = { hashed.incrementAndGet(); inner.valueOf(bytes) }
      def leaf(segment:  BitVector, value: Digest[Sha256]): Digest[Sha256] = inner.leaf(segment, value)
      def branch(
        segment: BitVector,
        value:   Option[Digest[Sha256]],
        left:    Option[Digest[Sha256]],
        right:   Option[Digest[Sha256]]
      ): Digest[Sha256] =
        inner.branch(segment, value, left, right)
    }
    expect(ValueCommitment.source(within).isRight, "a commitment of exactly the ceiling is a source")
      .and(expect.same(Left(Cbor.TooLarge(Cbor.MaxEncodedBytes)), ValueCommitment.source(past).map(_ => ())))
      .and(
        expect.same(
          Left(VersionedTrieError.TooLarge("the value's commitment", Cbor.MaxEncodedBytes)),
          VersionedTrie.valueHash[Sha256](past)
        )
      )
      .and(expect.same(0L, hashed.get))
  }

  test("the commitment is injective on distinct values") {
    forall(Gen.zip(value, value)) {
      case (a, b) => expect((a == b) == (ValueCommitment.bytes(a) == ValueCommitment.bytes(b)))
    }
  }
}
