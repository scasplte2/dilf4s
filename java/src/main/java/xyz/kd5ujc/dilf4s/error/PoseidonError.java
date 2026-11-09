package xyz.kd5ujc.dilf4s.error;

/** Why Poseidon hashing or a field element was refused, or a null argument ({@link TypeMismatch}). */
public sealed interface PoseidonError
  permits
    PoseidonError.InputCount,
    PoseidonError.NonCanonical,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InputCount}. */
  record InputCount(long count, long max) implements PoseidonError {
    @Override
    public String kind() {
      return "InputCount";
    }
  }

  /** {@code NonCanonical}. */
  record NonCanonical(long bits) implements PoseidonError {
    @Override
    public String kind() {
      return "NonCanonical";
    }
  }
}
