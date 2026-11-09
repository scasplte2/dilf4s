package xyz.kd5ujc.dilf4s.error;

/** Why {@code expand_message_xmd} was refused, checked in this order. */
public enum ExpansionRefusal {
  /** {@code emptyDst}. */
  EMPTY_DST("emptyDst"),
  /** {@code outputLength}. */
  OUTPUT_LENGTH("outputLength");

  private final String wire;

  ExpansionRefusal(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
