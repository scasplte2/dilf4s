package xyz.kd5ujc.dilf4s.error;

/** A BN254 operation's refusal ({@code docs/specs/zk.md} section 1), or a null argument ({@link TypeMismatch}). */
public sealed interface ZkError
  permits
    ZkError.Malformed,
    ZkError.InvalidPoint,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Malformed}. */
  record Malformed(String reason) implements ZkError {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code InvalidPoint}. */
  record InvalidPoint(String reason) implements ZkError {
    @Override
    public String kind() {
      return "InvalidPoint";
    }
  }
}
