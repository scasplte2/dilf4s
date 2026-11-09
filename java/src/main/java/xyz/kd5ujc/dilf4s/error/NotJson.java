package xyz.kd5ujc.dilf4s.error;

/** What JSON could not hold. */
public enum NotJson {
  /** {@code bytes}. */
  BYTES("bytes"),
  /** {@code non-terminating rational}. */
  NON_TERMINATING_RATIONAL("non-terminating rational"),
  /** {@code over-long number}. */
  OVER_LONG_NUMBER("over-long number");

  private final String wire;

  NotJson(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
