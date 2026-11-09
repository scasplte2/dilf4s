package xyz.kd5ujc.dilf4s.error;

/** A KES refusal ({@code docs/specs/kes.md} section 5), or a null argument ({@link TypeMismatch}). */
public sealed interface KesError
  permits
    KesError.Malformed,
    KesError.PeriodOutOfRange,
    KesError.Exhausted,
    KesError.InconsistentKey,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Malformed}. */
  record Malformed(String what, String reason) implements KesError {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code PeriodOutOfRange}. */
  record PeriodOutOfRange(long period, long total) implements KesError {
    @Override
    public String kind() {
      return "PeriodOutOfRange";
    }
  }

  /** {@code Exhausted}. */
  record Exhausted(long period) implements KesError {
    @Override
    public String kind() {
      return "Exhausted";
    }
  }

  /** {@code InconsistentKey}. */
  record InconsistentKey(String reason) implements KesError {
    @Override
    public String kind() {
      return "InconsistentKey";
    }
  }
}
