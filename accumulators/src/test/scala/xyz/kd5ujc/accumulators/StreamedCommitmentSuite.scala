package xyz.kd5ujc.accumulators

import xyz.kd5ujc.accumulators.merkle.{MerkleHashing, MerkleTree}
import xyz.kd5ujc.accumulators.trie.TrieHashing
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{BufferedCbor, CanonicalGens, CanonicalValue}
import xyz.kd5ujc.core.hash.{Keccak256, Sha256}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/**
 * Value leaves and trie values hash their commitment streamed; the old path built the commitment whole and hashed its
 * bytes ([[BufferedCbor.commitment]], the oracle). Both profiles, and both byte strings and other values.
 */
object StreamedCommitmentSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(300)

  private def same(name: String, v: CanonicalValue): weaver.Expectations = {
    val buffered = BufferedCbor.commitment(v)
    val tagged = MerkleHashing.tagged[Sha256]
    val rfc = MerkleHashing.rfc6962[Keccak256]
    expect(ValueCommitment.bytes(v) == Right(buffered), s"$name: the commitment")
      .and(expect(MerkleTree.leaf[Sha256, CanonicalValue](v) == Right(tagged.leaf(buffered)), s"$name: a tagged leaf"))
      .and(expect(rfc.leafOf(ValueCommitment.source(v)) == rfc.leaf(buffered), s"$name: an RFC 6962 leaf"))
      .and(expect(TrieHashing[Sha256].valueOf(ValueCommitment.source(v)) == TrieHashing[Sha256].value(buffered), s"$name: a trie value"))
  }

  private val cases: List[(String, CanonicalValue)] = List(
    "empty bytes"   -> CBytes(ByteVector.empty),
    "bytes"         -> CBytes(ByteVector.fill(70000)(9)),
    "text"          -> CanonicalValue.text("a" * 70000).fold(e => sys.error(e.toString), identity),
    "bytes as cbor" -> CanonicalValue.text("aa").fold(e => sys.error(e.toString), identity),
    "null"          -> CNull,
    "empty map"     -> EmptyMap
  )

  cases.foreach { case (name, v) => pureTest(s"a streamed commitment hashes as the buffered one: $name")(same(name, v)) }

  test("a streamed commitment hashes as the buffered one for generated values") {
    forall(CanonicalGens.value(4))(v => same(v.toString.take(60), v))
  }
}
