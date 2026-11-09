package xyz.kd5ujc.dilf4s.error;

/** Every way a JLVM parse or evaluation fails ({@code docs/specs/jlvm.md} section 3), or a null argument ({@link TypeMismatch}). The kind, an operation-level kind's {@code op} (the operation's wire name, or {@code var}) and a {@code Crypto} error's cause are normative; messages are not. */
public sealed interface JlvmError
  permits
    JlvmError.Malformed,
    JlvmError.Type,
    JlvmError.Arity,
    JlvmError.Arithmetic,
    JlvmError.Domain,
    JlvmError.Crypto,
    JlvmError.Limit,
    JlvmError.OutOfGas,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Malformed}. */
  record Malformed(String message) implements JlvmError {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code Type}. */
  record Type(String op, String message) implements JlvmError {
    @Override
    public String kind() {
      return "Type";
    }
  }

  /** {@code Arity}. */
  record Arity(String op, String message) implements JlvmError {
    @Override
    public String kind() {
      return "Arity";
    }
  }

  /** {@code Arithmetic}. */
  record Arithmetic(String op, String message) implements JlvmError {
    @Override
    public String kind() {
      return "Arithmetic";
    }
  }

  /** {@code Domain}. */
  record Domain(String op, String message) implements JlvmError {
    @Override
    public String kind() {
      return "Domain";
    }
  }

  /** {@code Crypto}. */
  record Crypto(String op, String message, java.util.Optional<CryptoCause> cause) implements JlvmError {
    @Override
    public String kind() {
      return "Crypto";
    }
  }

  /** {@code Limit}. */
  record Limit(String message) implements JlvmError {
    @Override
    public String kind() {
      return "Limit";
    }
  }

  /** {@code OutOfGas}. */
  record OutOfGas(long required, long available) implements JlvmError {
    @Override
    public String kind() {
      return "OutOfGas";
    }
  }
}
