package xyz.kd5ujc.accumulators.merkle

import xyz.kd5ujc.accumulators.{ProofInput, ValueCommitment}
import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalValue}
import xyz.kd5ujc.core.hash.Sha256

import weaver.SimpleIOSuite

/**
 * `MerkleCodec.verifyLeaf`, the SDK form of `merkle_verify`, checks in the order every SDK does
 * (docs/specs/conformance.md §5): the head, then the claim, then the proof.
 */
object MerkleVerifyLeafSuite extends SimpleIOSuite {

  implicit private val M: MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]

  private val leaf = CanonicalValue.CInt(1)
  private val tree =
    MerkleTree.fromLeafHashes[Sha256](List(M.leafOf(ValueCommitment.source(leaf).fold(e => sys.error(e.toString), identity))))
  private val proof = tree.inclusionProof(0L).flatMap(MerkleCodec.encodeInclusion(_)).toOption.get
  private val heavy = BuiltValues.build(BuiltValues.ofWeight(ProofInput.MaxProofWeight + 1)).toOption.get

  pureTest("a head with a negative size is malformed, before the claim is weighed; a claim over the cap is TooLarge") {
    def verify(size: Long, claim: CanonicalValue) = MerkleCodec.verifyLeaf[Sha256](tree.root, size, claim, proof)
    expect(verify(-1L, leaf).left.exists(_.isInstanceOf[MerkleError.Encoding]), s"${verify(-1L, leaf)}")
      .and(expect(verify(-1L, heavy).left.exists(_.isInstanceOf[MerkleError.Encoding]), s"${verify(-1L, heavy)}"))
      .and(expect.same(Left(MerkleError.TooLarge(ProofInput.MaxProofWeight)), verify(1L, heavy)))
      .and(expect.same(Right(true), verify(1L, leaf)))
  }
}
