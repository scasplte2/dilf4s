package xyz.kd5ujc.jlvm

/**
 * Every way a parse or an evaluation can fail; evaluation failures are reported together with the gas they used.
 * Generated from `shared/schemas/jlvm-errors.cddl`, with the Rust SDK's kinds and fields: the kind and the `op` of the
 * operation-level kinds are normative (docs/specs/jlvm.md §3); messages, a `Crypto` error's `cause` and `required` are
 * not.
 */
export xyz.kd5ujc.models.jlvm_errors.{JlvmCryptoCause => CryptoCause, JlvmCryptoCauseKind => CryptoCauseKind, JlvmError, JlvmErrorKind}

/** What the evaluator asks of an error beyond its generated fields. */
extension (e: JlvmError) {

  /** What was wrong; for `OutOfGas`, the gas it would have used and the limit. */
  def message: String = e match {
    case JlvmError.Malformed(m)           => m
    case JlvmError.Type(_, m)             => m
    case JlvmError.Arity(_, m)            => m
    case JlvmError.Arithmetic(_, m)       => m
    case JlvmError.Domain(_, m)           => m
    case JlvmError.Crypto(_, m, _)        => m
    case JlvmError.Limit(m)               => m
    case JlvmError.OutOfGas(required, at) => s"out of gas: $required required, $at available"
  }

  /** The site of an operation-level error, if the error has one (its normative `op`). */
  def site: Option[Site] = e match {
    case JlvmError.Type(op, _)       => Some(op)
    case JlvmError.Arity(op, _)      => Some(op)
    case JlvmError.Arithmetic(op, _) => Some(op)
    case JlvmError.Domain(op, _)     => Some(op)
    case JlvmError.Crypto(op, _, _)  => Some(op)
    case _: JlvmError.Malformed      => None
    case _: JlvmError.Limit          => None
    case _: JlvmError.OutOfGas       => None
  }
}
