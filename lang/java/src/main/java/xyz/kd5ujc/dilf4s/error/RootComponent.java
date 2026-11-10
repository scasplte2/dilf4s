package xyz.kd5ujc.dilf4s.error;

/** Which root of a versioned trie a recomputation disagreed on. */
public enum RootComponent {
  /** {@code root}. */
  ROOT("root"),
  /** {@code state}. */
  STATE("state"),
  /** {@code history}. */
  HISTORY("history"),
  /** {@code parent}. */
  PARENT("parent");

  private final String wire;

  RootComponent(String wire) {
    this.wire = wire;
  }

  /** Its name in the schema and the vectors. */
  public String wire() {
    return wire;
  }
}
