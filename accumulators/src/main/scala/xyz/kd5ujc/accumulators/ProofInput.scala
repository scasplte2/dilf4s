package xyz.kd5ujc.accumulators

import xyz.kd5ujc.core.canonical.CanonicalValue

/**
 * The bound on a verifier's caller input (docs/specs/conformance.md §5): a proof, delta or other record passed as a
 * canonical value. A value that shares structure is small in memory and can be exponentially heavy, and decoding one
 * walks it as written out, so every verifier entry point refuses a heavy input from its cached weight, in O(1), before
 * decoding it. The refusal is the verifier's malformed-input error, never `false`.
 */
object ProofInput {

  /** The heaviest input a verifier decodes: 2^24, four times the largest snapshot chunk, so range proofs fit. */
  val MaxProofWeight: Long = 1L << 24

  /** `v` if its weight is at most [[MaxProofWeight]]; otherwise `refuse` of a reason (not normative). */
  def bounded[E](v: CanonicalValue)(refuse: String => E): Either[E, CanonicalValue] =
    Either.cond(v.weight <= MaxProofWeight, v, refuse(s"the input weighs over $MaxProofWeight"))
}
