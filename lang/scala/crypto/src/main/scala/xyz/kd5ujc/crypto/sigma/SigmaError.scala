package xyz.kd5ujc.crypto.sigma

/**
 * Malformed sigma input (docs/specs/sigma.md §6), by kind: the kinds every SDK reports and the vectors pin, generated
 * from `shared/schemas/crypto-errors.cddl`. A well-formed proof that fails is `Right(false)`, never one of these.
 * `reason` and `what` are messages, not normative.
 */
export xyz.kd5ujc.models.crypto_errors.SigmaError

extension (e: SigmaError) {

  /** The refusal as text, for callers that report a message (the JLVM's `Crypto`). */
  def message: String = e match {
    case SigmaError.InvalidProposition(reason)    => reason
    case SigmaError.InvalidPoint(what, reason)    => s"$what: $reason"
    case SigmaError.ProofLength(expected, actual) => s"a proof of this proposition is $expected bytes, got $actual"
    case SigmaError.ResponseOutOfRange            => "a response must be below the group order"
  }
}
