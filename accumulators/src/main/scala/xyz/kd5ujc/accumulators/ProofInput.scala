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
  val MaxProofWeight: Long = xyz.kd5ujc.models.limits.MaxProofWeight

  /** `v` if its weight is at most [[MaxProofWeight]]; otherwise `refuse` of a reason (not normative). */
  def bounded[E](v: CanonicalValue)(refuse: String => E): Either[E, CanonicalValue] =
    Either.cond(v.weight <= MaxProofWeight, v, refuse(s"the input weighs over $MaxProofWeight"))

  /**
   * The claims a verifier checks a proof against (a Merkle leaf, a trie value, a listing's values), which it commits
   * by `commit(v)`: together at most [[MaxProofWeight]], or `refuse` of that limit (`TooLarge`), in O(their number)
   * from their cached weights, before any is hashed and before the proof is decoded.
   */
  def claimed[E](values: Iterable[CanonicalValue])(refuse: Long => E): Either[E, Unit] = {
    val max = xyz.kd5ujc.models.limits.CanonicalMaxWeight
    val total = values.foldLeft(0L)((t, v) => if (t > max - v.weight) max else t + v.weight)
    Either.cond(total <= MaxProofWeight, (), refuse(MaxProofWeight))
  }
}
