package xyz.kd5ujc.dilf4s.error;

/** A hash refusal ({@code shared/schemas/hash-errors.cddl}), or a null argument ({@link TypeMismatch}). */
public sealed interface HashError
  permits
    HashError.InvalidLength,
    HashError.InvalidHex,
    HashError.InvalidDomainTag,
    HashError.InvalidExpansion,
    HashError.UnknownAlgorithm,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InvalidLength}. */
  record InvalidLength(String algorithm, long expected, long actual) implements HashError {
    @Override
    public String kind() {
      return "InvalidLength";
    }
  }

  /** {@code InvalidHex}. */
  record InvalidHex(long offset) implements HashError {
    @Override
    public String kind() {
      return "InvalidHex";
    }
  }

  /** {@code InvalidDomainTag}. */
  record InvalidDomainTag(Preview tag) implements HashError {
    @Override
    public String kind() {
      return "InvalidDomainTag";
    }
  }

  /** {@code InvalidExpansion}. */
  record InvalidExpansion(ExpansionRefusal refusal, String algorithm, long limit) implements HashError {
    @Override
    public String kind() {
      return "InvalidExpansion";
    }
  }

  /** {@code UnknownAlgorithm}. */
  record UnknownAlgorithm(Preview name) implements HashError {
    @Override
    public String kind() {
      return "UnknownAlgorithm";
    }
  }
}
