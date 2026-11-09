package xyz.kd5ujc.dilf4s.error;

/** A versioned trie refusal ({@code docs/specs/versioned-trie.md}), versions named by their keys' hex, or a null argument ({@link TypeMismatch}). */
public sealed interface VersionedTrieError
  permits
    VersionedTrieError.Key,
    VersionedTrieError.InvalidVersion,
    VersionedTrieError.Empty,
    VersionedTrieError.VersionNotIncreasing,
    VersionedTrieError.VersionConflict,
    VersionedTrieError.CommitRewrite,
    VersionedTrieError.NotRetained,
    VersionedTrieError.UnknownVersion,
    VersionedTrieError.RootMismatch,
    VersionedTrieError.NotCovered,
    VersionedTrieError.Malformed,
    VersionedTrieError.TooLarge,
    VersionedTrieError.TooDeep,
    VersionedTrieError.Store,
    VersionedTrieError.Trie,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Key}. */
  record Key(StateKeyError error) implements VersionedTrieError {
    @Override
    public String kind() {
      return "Key";
    }
  }

  /** {@code InvalidVersion}. */
  record InvalidVersion(String reason) implements VersionedTrieError {
    @Override
    public String kind() {
      return "InvalidVersion";
    }
  }

  /** {@code Empty}. */
  record Empty() implements VersionedTrieError {
    @Override
    public String kind() {
      return "Empty";
    }
  }

  /** {@code VersionNotIncreasing}. */
  record VersionNotIncreasing(String head, String next) implements VersionedTrieError {
    @Override
    public String kind() {
      return "VersionNotIncreasing";
    }
  }

  /** {@code VersionConflict}. */
  record VersionConflict(java.util.Optional<String> expected, java.util.Optional<String> actual) implements VersionedTrieError {
    @Override
    public String kind() {
      return "VersionConflict";
    }
  }

  /** {@code CommitRewrite}. */
  record CommitRewrite(String version) implements VersionedTrieError {
    @Override
    public String kind() {
      return "CommitRewrite";
    }
  }

  /** {@code NotRetained}. */
  record NotRetained(String version) implements VersionedTrieError {
    @Override
    public String kind() {
      return "NotRetained";
    }
  }

  /** {@code UnknownVersion}. */
  record UnknownVersion(String version) implements VersionedTrieError {
    @Override
    public String kind() {
      return "UnknownVersion";
    }
  }

  /** {@code RootMismatch}. */
  record RootMismatch(RootComponent component, String expected, String computed) implements VersionedTrieError {
    @Override
    public String kind() {
      return "RootMismatch";
    }
  }

  /** {@code NotCovered}. */
  record NotCovered(String what) implements VersionedTrieError {
    @Override
    public String kind() {
      return "NotCovered";
    }
  }

  /** {@code Malformed}. */
  record Malformed(String reason) implements VersionedTrieError {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code TooLarge}. */
  record TooLarge(String what, long limit) implements VersionedTrieError {
    @Override
    public String kind() {
      return "TooLarge";
    }
  }

  /** {@code TooDeep}. */
  record TooDeep(String what, long depth, long limit) implements VersionedTrieError {
    @Override
    public String kind() {
      return "TooDeep";
    }
  }

  /** {@code Store}. */
  record Store(StoreError error) implements VersionedTrieError {
    @Override
    public String kind() {
      return "Store";
    }
  }

  /** {@code Trie}. */
  record Trie(TrieError error) implements VersionedTrieError {
    @Override
    public String kind() {
      return "Trie";
    }
  }
}
