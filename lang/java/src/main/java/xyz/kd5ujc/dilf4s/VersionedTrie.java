package xyz.kd5ujc.dilf4s;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalLong;

import xyz.kd5ujc.dilf4s.error.StateKeyError;
import xyz.kd5ujc.dilf4s.error.VersionedTrieError;
import xyz.kd5ujc.dilf4s.internal.Proofs;

/**
 * Versioned tries ({@code docs/specs/versioned-trie.md}): state keys and namespaces, version keys (unsigned integers of
 * a fixed width), and the verifiers of a versioned root's proofs. Every verifier binds the proof to the root first: a
 * proof of another root is a {@code RootMismatch}.
 */
public final class VersionedTrie {
  private VersionedTrie() {}

  /**
   * A versioned trie's head: its version, its state root and its history root. The arrays are copied in and out.
   *
   * @param version the head version
   * @param stateRoot the state trie's root
   * @param historyRoot the history trie's root
   */
  public record Head(long version, byte[] stateRoot, byte[] historyRoot) {
    public Head {
      stateRoot = Bytes.copy(stateRoot);
      historyRoot = Bytes.copy(historyRoot);
    }

    @Override
    public byte[] stateRoot() {
      return Bytes.copy(stateRoot);
    }

    @Override
    public byte[] historyRoot() {
      return Bytes.copy(historyRoot);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Head h && h.version == version && Arrays.equals(h.stateRoot, stateRoot) && Arrays.equals(h.historyRoot, historyRoot);
    }

    @Override
    public int hashCode() {
      return Long.hashCode(version) * 31 + Arrays.hashCode(stateRoot);
    }

    @Override
    public String toString() {
      return "Head(" + version + ", " + Bytes.hex(stateRoot) + ", " + Bytes.hex(historyRoot) + ")";
    }
  }

  /** A state proven against a root: answers about the keys the proof covers ({@code NotCovered} for the rest). */
  public static final class State {
    private final Proofs.State scala;

    private State(Proofs.State scala) {
      this.scala = scala;
    }

    /**
     * The value hash at {@code key}, or empty when the key is absent.
     *
     * @param key a state key
     * @return the hash, empty, or the refusal
     */
    public Result<Optional<byte[]>, VersionedTrieError> lookup(String key) {
      return key == null ? Result.err(Wrap.nullArgument("a key")) : scala.lookup(key);
    }

    /**
     * Whether {@code key} holds {@code value} (or, empty, is absent).
     *
     * @param key a state key
     * @param value the claimed value, or empty
     * @return the verdict, or the refusal
     */
    public Result<Boolean, VersionedTrieError> holds(String key, Optional<Value> value) {
      return key == null || value == null ? Result.err(Wrap.nullArgument("a key and a value")) : scala.holds(key, value.map(Wrap::scala));
    }

    /**
     * Every key in {@code namespace} and its value's hash, in key order; {@code NotCovered} unless the proof covers it.
     *
     * @param namespace a state namespace
     * @return the listing, or the refusal
     */
    public Result<List<Map.Entry<String, byte[]>>, VersionedTrieError> namespace(String namespace) {
      return namespace == null ? Result.err(Wrap.nullArgument("a namespace")) : scala.namespace(namespace).map(List::copyOf);
    }

    /**
     * Every key in {@code [from, until)} and its value's hash, in key order; {@code NotCovered} unless the proof covers
     * the range.
     *
     * @param from the first key
     * @param until the bound, or empty
     * @return the listing, or the refusal
     */
    public Result<List<Map.Entry<String, byte[]>>, VersionedTrieError> range(String from, Optional<String> until) {
      return from == null || until == null ? Result.err(Wrap.nullArgument("bounds")) : scala.range(from, until).map(List::copyOf);
    }
  }

  /**
   * {@code key}, if it is a state key: segments of {@code [a-z0-9][a-z0-9._-]*} joined by {@code /}, within the bounds.
   *
   * @param key the key
   * @return the key, or the refusal
   */
  public static Result<String, StateKeyError> stateKey(String key) {
    return key == null ? Result.err(Wrap.nullArgument("a key")) : Proofs.stateKey(key);
  }

  /**
   * {@code namespace}, if it is a state namespace with room for one more segment.
   *
   * @param namespace the namespace
   * @return the namespace, or the refusal
   */
  public static Result<String, StateKeyError> namespace(String namespace) {
    return namespace == null ? Result.err(Wrap.nullArgument("a namespace")) : Proofs.namespace(namespace);
  }

  /**
   * A version's key: its big-endian bytes at {@code width}.
   *
   * @param width the key width, in bytes
   * @param version the version, unsigned
   * @return the key, or the refusal
   */
  public static Result<byte[], VersionedTrieError> versionKey(int width, long version) {
    return Proofs.versionKey(width, version);
  }

  /**
   * The version a key names.
   *
   * @param width the key width, in bytes
   * @param key the key
   * @return the version, or the refusal
   */
  public static Result<Long, VersionedTrieError> version(int width, byte[] key) {
    return key == null ? Result.err(Wrap.nullArgument("a key")) : Proofs.versionOf(width, key);
  }

  /**
   * The current state a proof proves under {@code root}.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @return the state, or the refusal
   */
  public static Result<State, VersionedTrieError> verifyState(int width, Hashing.Algorithm algorithm, byte[] root, Value proof) {
    return algorithm == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root and a proof"))
      : Proofs.verifyState(width, algorithm, root, Wrap.scala(proof)).map(State::new);
  }

  /**
   * The state at {@code version} a historical proof proves under {@code root}.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @param version the version
   * @return the state, or the refusal ({@code UnknownVersion} for a version never committed)
   */
  public static Result<State, VersionedTrieError> verifyHistoricalKey(int width, Hashing.Algorithm algorithm, byte[] root, Value proof, long version) {
    return algorithm == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root and a proof"))
      : Proofs.verifyHistoricalKey(width, algorithm, root, Wrap.scala(proof), version).map(State::new);
  }

  /**
   * The head a proof proves under {@code root}.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @return the head, or the refusal
   */
  public static Result<Head, VersionedTrieError> verifyHead(int width, Hashing.Algorithm algorithm, byte[] root, Value proof) {
    return algorithm == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root and a proof"))
      : Proofs.verifyHead(width, algorithm, root, Wrap.scala(proof));
  }

  /**
   * The state root committed at {@code version}, or empty when none was.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @param version the version
   * @return the state root, empty, or the refusal
   */
  public static Result<Optional<byte[]>, VersionedTrieError> verifyVersion(int width, Hashing.Algorithm algorithm, byte[] root, Value proof, long version) {
    return algorithm == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root and a proof"))
      : Proofs.verifyVersion(width, algorithm, root, Wrap.scala(proof), version);
  }

  /**
   * Whether {@code claimed}, in version order, are exactly the versions committed in {@code [from, until)} and their
   * state roots.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @param from the first version
   * @param until the bound, or empty
   * @param claimed versions and their state roots
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, VersionedTrieError> verifyVersions(
    int width,
    Hashing.Algorithm algorithm,
    byte[] root,
    Value proof,
    long from,
    OptionalLong until,
    List<Map.Entry<Long, byte[]>> claimed
  ) {
    return algorithm == null || root == null || proof == null || until == null || claimed == null
        || claimed.stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null)
      ? Result.err(Wrap.nullArgument("an algorithm, a root, a proof, a bound and claims"))
      : Proofs.verifyVersions(width, algorithm, root, Wrap.scala(proof), from, until, claimed.stream().map(e -> Map.entry(e.getKey(), e.getValue().clone())).toList());
  }

  /**
   * The latest version at or before {@code version} and its state root, or empty when there is none.
   *
   * @param width the version key width
   * @param algorithm the hash
   * @param root the versioned root
   * @param proof the proof's canonical value
   * @param version the version
   * @return the answer, empty, or the refusal
   */
  public static Result<Optional<Map.Entry<Long, byte[]>>, VersionedTrieError> verifyAsOf(int width, Hashing.Algorithm algorithm, byte[] root, Value proof, long version) {
    return algorithm == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a root and a proof"))
      : Proofs.verifyAsOf(width, algorithm, root, Wrap.scala(proof), version);
  }
}
