package xyz.kd5ujc.crypto.sigma

/**
 * Malformed sigma input (docs/specs/sigma.md §6), by kind: the kinds every SDK reports and the vectors pin. A
 * well-formed proof that fails is `Right(false)`, never one of these. `reason` and `what` are messages, not normative.
 */
sealed trait SigmaError extends Product with Serializable {

  /** The refusal as text, for callers that report a message (the JLVM's `Crypto`). */
  def message: String
}

object SigmaError {

  /**
   * A proposition breaking §3: an unknown type, a missing or extra field, an identity statement point, a connective
   * with no children or more than 255, `k` outside `1..n`, more than 64 levels or 4096 leaves, or a canonical form
   * heavier than the heaviest valid proposition.
   */
  final case class InvalidProposition(reason: String) extends SigmaError {
    def message: String = reason
  }

  /** A group element that is not the canonical encoding of a point of secp256k1 (§1); `what` is its field. */
  final case class InvalidPoint(what: String, reason: String) extends SigmaError {
    def message: String = s"$what: $reason"
  }

  /** A proof whose length is not the one its proposition fixes (§5). */
  final case class ProofLength(expected: Long, actual: Long) extends SigmaError {
    def message: String = s"a proof of this proposition is $expected bytes, got $actual"
  }

  /** A response `z >= q`. */
  case object ResponseOutOfRange extends SigmaError {
    def message: String = "a response must be below the group order"
  }
}
