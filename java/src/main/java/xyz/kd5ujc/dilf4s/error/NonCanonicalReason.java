package xyz.kd5ujc.dilf4s.error;

/** The deterministic-encoding rule a CBOR item breaks. */
public enum NonCanonicalReason {
  /** {@code nonShortestArgument}. */
  NON_SHORTEST_ARGUMENT("nonShortestArgument"),
  /** {@code keyOrder}. */
  KEY_ORDER("keyOrder"),
  /** {@code nullMapValue}. */
  NULL_MAP_VALUE("nullMapValue"),
  /** {@code bignumLeadingZero}. */
  BIGNUM_LEADING_ZERO("bignumLeadingZero"),
  /** {@code bignumInRange}. */
  BIGNUM_IN_RANGE("bignumInRange"),
  /** {@code bignumNotBytes}. */
  BIGNUM_NOT_BYTES("bignumNotBytes"),
  /** {@code rationalNotPair}. */
  RATIONAL_NOT_PAIR("rationalNotPair"),
  /** {@code rationalNotInteger}. */
  RATIONAL_NOT_INTEGER("rationalNotInteger"),
  /** {@code rationalDenominator}. */
  RATIONAL_DENOMINATOR("rationalDenominator"),
  /** {@code rationalNotReduced}. */
  RATIONAL_NOT_REDUCED("rationalNotReduced");

  private final String wire;

  NonCanonicalReason(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
