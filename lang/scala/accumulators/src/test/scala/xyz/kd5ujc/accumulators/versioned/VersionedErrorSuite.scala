package xyz.kd5ujc.accumulators.versioned

import xyz.kd5ujc.accumulators.ProofInput
import xyz.kd5ujc.accumulators.trie.TrieError
import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalValue, DecodeFailure, DecodeReason, Preview, ValueKind}
import xyz.kd5ujc.core.hash.HashError
import xyz.kd5ujc.models.versioned_trie.VersionedTrieHead

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object VersionedErrorSuite extends SimpleIOSuite {

  pureTest("wire decoding preserves the canonical cause and offset") {
    expect.same(
      Left(VersionedTrieError.Malformed(MalformedReason.Canonical(CanonicalError.UnexpectedEnd(0)))),
      VersionedTrieWire.decodeChunk[Long, H](ByteVector.empty)
    )
  }

  pureTest("proof decoding preserves the schema reason and nested field path") {
    val value = get(CanonicalValue.map(List("head" -> get(CanonicalValue.map(List("v" -> int(1)))))))
    val cause = DecodeFailure(List(Preview.of("head"), Preview.of("v")), DecodeReason.ExpectedType(Vector(ValueKind.Bytes), ValueKind.Int))
    expect.same(Left(VersionedTrieError.Malformed(MalformedReason.Decode(cause))), VersionedTrieWire.decodeProof[Long, H](value))
  }

  pureTest("head conversion preserves the hash algorithm and wrong length") {
    val head = VersionedTrieHead(vk(1L), ByteVector.empty, T.empty.bytes)
    expect.same(
      Left(VersionedTrieError.Malformed(MalformedReason.Digest(HashError.InvalidLength("SHA-256", 32, 0)))),
      VersionedTrieWire.fromHead[Long, H](head)
    )
  }

  pureTest("a shared oversized input is refused by weight before proof or delta shape decoding") {
    val value = (1 to 24).foldLeft(int(1))((v, _) => get(CanonicalValue.array(Vector(v, v))))
    val refused = Left(VersionedTrieError.Malformed(MalformedReason.ProofWeight(ProofInput.MaxProofWeight)))
    expect
      .same((1L << 25) - 1, value.weight)
      .and(expect.same(refused, VersionedTrieWire.decodeProof[Long, H](value)))
      .and(expect.same(refused, VersionedTrieWire.decodeDelta[Long, H](value)))
  }

  pureTest("delta structural failures have distinct typed reasons") {
    val replica = get(Replica.empty[Long, H])
    val head = VersionedTrie.Head[Long, H](1L, T.empty, T.empty)
    val written = List(key("a") -> int(1))
    val overlap = Delta(None, head, written, List(key("a")))
    val duplicate = Delta(None, head, written ++ written, Nil)
    expect
      .same(Left(VersionedTrieError.Malformed(MalformedReason.WriteRemoveOverlap)), Replica.applyDelta(replica, overlap))
      .and(expect.same(Left(VersionedTrieError.Malformed(MalformedReason.DuplicateWrite)), Replica.applyDelta(replica, duplicate)))
  }

  pureTest("trie conversion preserves causes and translates only the producer weight limit") {
    val cause = TrieError.MissingNode("ab")
    expect
      .same(VersionedTrieError.Trie(cause), fromTrie(cause))
      .and(expect.same(VersionedTrieError.TooLarge(LimitSubject.Proof, 123), fromTrie(TrieError.TooLarge(123))))
  }
}
