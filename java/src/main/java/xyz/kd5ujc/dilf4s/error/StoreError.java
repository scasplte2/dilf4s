package xyz.kd5ujc.dilf4s.error;

/** A store failure, as a versioned trie's {@code Store} refusal carries it. */
public sealed interface StoreError
  permits
    StoreError.Encoding,
    StoreError.NotFound,
    StoreError.VersionConflict,
    StoreError.UnknownVersion,
    StoreError.Pruned,
    StoreError.Backend,
    StoreError.Corrupt {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Encoding}. */
  record Encoding(String context, String reason) implements StoreError {
    @Override
    public String kind() {
      return "Encoding";
    }
  }

  /** {@code NotFound}. */
  record NotFound(long keyBytes, Preview prefix) implements StoreError {
    @Override
    public String kind() {
      return "NotFound";
    }
  }

  /** {@code VersionConflict}. */
  record VersionConflict(long expected, long actual) implements StoreError {
    @Override
    public String kind() {
      return "VersionConflict";
    }
  }

  /** {@code UnknownVersion}. */
  record UnknownVersion(long requested, long latest) implements StoreError {
    @Override
    public String kind() {
      return "UnknownVersion";
    }
  }

  /** {@code Pruned}. */
  record Pruned(long requested, long floor) implements StoreError {
    @Override
    public String kind() {
      return "Pruned";
    }
  }

  /** {@code Backend}. */
  record Backend(String operation, String reason) implements StoreError {
    @Override
    public String kind() {
      return "Backend";
    }
  }

  /** {@code Corrupt}. */
  record Corrupt(String reason) implements StoreError {
    @Override
    public String kind() {
      return "Corrupt";
    }
  }
}
