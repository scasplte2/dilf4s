package xyz.kd5ujc.dilf4s.error;

/** Why a derivation path was refused, its segments checked in order. */
public enum PathRefusal {
  /** {@code missingMaster}. */
  MISSING_MASTER("missingMaster"),
  /** {@code malformed}. */
  MALFORMED("malformed"),
  /** {@code leadingZero}. */
  LEADING_ZERO("leadingZero"),
  /** {@code tooManyDigits}. */
  TOO_MANY_DIGITS("tooManyDigits"),
  /** {@code outOfRange}. */
  OUT_OF_RANGE("outOfRange"),
  /** {@code tooDeep}. */
  TOO_DEEP("tooDeep");

  private final String wire;

  PathRefusal(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
