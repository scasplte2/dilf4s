package xyz.kd5ujc.accumulators.trie

import xyz.kd5ujc.accumulators.ProofInput
import xyz.kd5ujc.core.hash.{Hasher, Sha256}

import scodec.bits.{BitVector, ByteVector}
import weaver.SimpleIOSuite

/**
 * No producer emits a proof every verifier refuses (docs/specs/conformance.md §5): one heavier than
 * `ProofInput.MaxProofWeight` is `TooLarge` at encoding, and so at `Trie.prove`, which checks through the encoder.
 */
object TrieProducerSuite extends SimpleIOSuite {

  private val digest = Hasher[Sha256].digest(ByteVector.empty)

  /** A one-leaf proof whose segment is `bytes` long: its record weighs about that much. */
  private def leafOf(bytes: Long): TrieProof[Sha256] =
    TrieProof(Some(ProofNode.PLeaf(BitVector.low(bytes * 8), digest)))

  pureTest("a proof over MaxProofWeight is refused at encoding; one under it is encoded within the cap") {
    val over = TrieProofs.encode(leafOf(ProofInput.MaxProofWeight))
    val under = TrieProofs.encode(leafOf(1024))
    expect
      .same(Left(TrieError.TooLarge(ProofInput.MaxProofWeight)), over)
      .and(expect(under.exists(_.weight <= ProofInput.MaxProofWeight), s"$under"))
  }
}
