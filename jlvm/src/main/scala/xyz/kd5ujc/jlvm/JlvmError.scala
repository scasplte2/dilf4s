package xyz.kd5ujc.jlvm

/** Every way a parse or an evaluation can fail; evaluation failures are reported together with the gas they used. */
sealed trait JlvmError extends Product with Serializable {
  def message: String
}

object JlvmError {

  /** Not a program of this language version: bad JSON, unknown operation, malformed var, too deep or too large. */
  final case class Malformed(message: String) extends JlvmError

  /** An operation received a value of the wrong kind. */
  final case class Type(op: String, message: String) extends JlvmError

  /** An operation received the wrong number of arguments. */
  final case class Arity(op: String, message: String) extends JlvmError

  /** Division by zero, an exponent out of range, a non-terminating rational where text is required, ... */
  final case class Arithmetic(op: String, message: String) extends JlvmError

  /** A value of the right kind outside the operation's domain: malformed hex, an empty separator, an integer too wide. */
  final case class Domain(op: String, message: String) extends JlvmError

  /** Malformed cryptographic input (a well-formed but invalid proof is `false`, not an error). */
  final case class Crypto(op: String, message: String) extends JlvmError

  /** A resource bound: value depth or size, evaluation depth. */
  final case class Limit(message: String) extends JlvmError

  final case class OutOfGas(required: Long, available: Long) extends JlvmError {
    def message: String = s"out of gas: $required required, $available available"
  }
}
