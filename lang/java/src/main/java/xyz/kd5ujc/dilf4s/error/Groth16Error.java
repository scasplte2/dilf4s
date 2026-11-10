package xyz.kd5ujc.dilf4s.error;

/** A Groth16 refusal ({@code docs/specs/zk.md} section 2), or a null argument ({@link TypeMismatch}). */
public sealed interface Groth16Error
  permits
    Groth16Error.Malformed,
    Groth16Error.InvalidProof,
    Groth16Error.PairingCheckFailed,
    Groth16Error.Rejected,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Malformed}. */
  record Malformed(String reason) implements Groth16Error {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code InvalidProof}. */
  record InvalidProof(String reason) implements Groth16Error {
    @Override
    public String kind() {
      return "InvalidProof";
    }
  }

  /** {@code PairingCheckFailed}. */
  record PairingCheckFailed() implements Groth16Error {
    @Override
    public String kind() {
      return "PairingCheckFailed";
    }
  }

  /** {@code Rejected}. */
  record Rejected(String reason) implements Groth16Error {
    @Override
    public String kind() {
      return "Rejected";
    }
  }
}
