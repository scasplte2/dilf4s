package xyz.kd5ujc.dilf4s.error;

/** A trie refusal ({@code docs/specs/trie.md}), or a null argument ({@link TypeMismatch}). */
public sealed interface TrieError
  permits
    TrieError.InvalidKey,
    TrieError.MissingNode,
    TrieError.InvalidNode,
    TrieError.InvalidProof,
    TrieError.RootMismatch,
    TrieError.Encoding,
    TrieError.NotCovered,
    TrieError.TooLarge,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InvalidKey}. */
  record InvalidKey(String reason) implements TrieError {
    @Override
    public String kind() {
      return "InvalidKey";
    }
  }

  /** {@code MissingNode}. */
  record MissingNode(String hash) implements TrieError {
    @Override
    public String kind() {
      return "MissingNode";
    }
  }

  /** {@code InvalidNode}. */
  record InvalidNode(String reason) implements TrieError {
    @Override
    public String kind() {
      return "InvalidNode";
    }
  }

  /** {@code InvalidProof}. */
  record InvalidProof(String reason) implements TrieError {
    @Override
    public String kind() {
      return "InvalidProof";
    }
  }

  /** {@code RootMismatch}. */
  record RootMismatch(String expected, String computed) implements TrieError {
    @Override
    public String kind() {
      return "RootMismatch";
    }
  }

  /** {@code Encoding}. */
  record Encoding(String reason) implements TrieError {
    @Override
    public String kind() {
      return "Encoding";
    }
  }

  /** {@code NotCovered}. */
  record NotCovered(String what) implements TrieError {
    @Override
    public String kind() {
      return "NotCovered";
    }
  }

  /** {@code TooLarge}. */
  record TooLarge(long limit) implements TrieError {
    @Override
    public String kind() {
      return "TooLarge";
    }
  }
}
