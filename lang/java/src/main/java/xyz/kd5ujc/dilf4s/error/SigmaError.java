package xyz.kd5ujc.dilf4s.error;

/** Malformed sigma input ({@code docs/specs/sigma.md} section 6), or a null argument ({@link TypeMismatch}). A well-formed proof that fails is {@code false}, never one of these. */
public sealed interface SigmaError
  permits
    SigmaError.InvalidProposition,
    SigmaError.InvalidPoint,
    SigmaError.ProofLength,
    SigmaError.ResponseOutOfRange,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InvalidProposition}. */
  record InvalidProposition(String reason) implements SigmaError {
    @Override
    public String kind() {
      return "InvalidProposition";
    }
  }

  /** {@code InvalidPoint}. */
  record InvalidPoint(String what, String reason) implements SigmaError {
    @Override
    public String kind() {
      return "InvalidPoint";
    }
  }

  /** {@code ProofLength}. */
  record ProofLength(long expected, long actual) implements SigmaError {
    @Override
    public String kind() {
      return "ProofLength";
    }
  }

  /** {@code ResponseOutOfRange}. */
  record ResponseOutOfRange() implements SigmaError {
    @Override
    public String kind() {
      return "ResponseOutOfRange";
    }
  }
}
