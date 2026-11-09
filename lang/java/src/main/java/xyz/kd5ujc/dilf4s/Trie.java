package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import xyz.kd5ujc.dilf4s.error.EncodeError;
import xyz.kd5ujc.dilf4s.error.TrieError;
import xyz.kd5ujc.dilf4s.internal.Proofs;

/**
 * Binary Patricia tries ({@code docs/specs/trie.md}) under dilf4s's tagged hashing: roots, value hashes, and the
 * verifiers of the JLVM's {@code trie_verify}, {@code trie_prefix_verify} and {@code trie_range_verify}. A proof of
 * another trie is {@code false}; a malformed one, or a claim heavier than the proof bound, a {@link TrieError}.
 */
public final class Trie {
  private Trie() {}

  /** How keys are read: exactly {@code n} bytes, or up to {@code max} bytes. Valid by construction. */
  public static final class KeyMode {
    private final xyz.kd5ujc.accumulators.trie.KeyMode scala;

    private KeyMode(xyz.kd5ujc.accumulators.trie.KeyMode scala) {
      this.scala = scala;
    }

    /**
     * Whether keys are of one length.
     *
     * @return true for a fixed mode
     */
    public boolean fixed() {
      return Proofs.isFixed(scala);
    }

    /**
     * The fixed length, or the longest variable key, in bytes.
     *
     * @return the bytes
     */
    public int bytes() {
      return Proofs.modeBytes(scala);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof KeyMode m && m.fixed() == fixed() && m.bytes() == bytes();
    }

    @Override
    public int hashCode() {
      return bytes() * 2 + (fixed() ? 1 : 0);
    }

    @Override
    public String toString() {
      return Proofs.describe(scala);
    }
  }

  /**
   * Keys of exactly {@code bytes} bytes.
   *
   * @param bytes the length
   * @return the mode, or the refusal of a length outside the bound
   */
  public static Result<KeyMode, TrieError> fixed(int bytes) {
    return Proofs.fixed(bytes).map(KeyMode::new);
  }

  /**
   * Keys of up to {@code maxBytes} bytes, whose branches may hold values.
   *
   * @param maxBytes the longest key
   * @return the mode, or the refusal of a length outside the bound
   */
  public static Result<KeyMode, TrieError> variable(int maxBytes) {
    return Proofs.variable(maxBytes).map(KeyMode::new);
  }

  private static boolean anyNullEntry(List<? extends Map.Entry<byte[], ?>> entries) {
    return entries == null || entries.stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null);
  }

  /**
   * A value's hash in a trie: the tagged hash of its commitment ({@link ValueCommitment}).
   *
   * @param algorithm the hash
   * @param value the value
   * @return the hash, or {@code TooLarge}
   */
  public static Result<byte[], EncodeError> valueHash(Hashing.Algorithm algorithm, Value value) {
    return algorithm == null || value == null
      ? Result.err(Wrap.nullArgument("an algorithm and a value"))
      : Proofs.trieValue(algorithm, Wrap.scala(value));
  }

  /**
   * The root of the trie holding {@code entries}, each a key and its value's hash; a repeated key is refused.
   *
   * @param algorithm the hash
   * @param mode the key mode
   * @param entries keys and value hashes
   * @return the root, or the refusal
   */
  public static Result<byte[], TrieError> root(Hashing.Algorithm algorithm, KeyMode mode, List<Map.Entry<byte[], byte[]>> entries) {
    return algorithm == null || mode == null || anyNullEntry(entries)
      ? Result.err(Wrap.nullArgument("an algorithm, a mode and entries"))
      : Proofs.trieRoot(algorithm, mode.scala, entries.stream().map(e -> Map.entry(e.getKey().clone(), e.getValue().clone())).toList());
  }

  /** What a verified proof says of one key. */
  public sealed interface Lookup permits Lookup.Present, Lookup.Absent, Lookup.NotCovered {

    /**
     * The key holds a value with this hash. The array is copied in and out.
     *
     * @param valueHash the value's hash
     */
    record Present(byte[] valueHash) implements Lookup {
      public Present {
        valueHash = Bytes.copy(valueHash);
      }

      @Override
      public byte[] valueHash() {
        return Bytes.copy(valueHash);
      }

      @Override
      public boolean equals(Object other) {
        return other instanceof Present p && java.util.Arrays.equals(p.valueHash, valueHash);
      }

      @Override
      public int hashCode() {
        return java.util.Arrays.hashCode(valueHash);
      }

      @Override
      public String toString() {
        return "Present(" + Bytes.hex(valueHash) + ")";
      }
    }

    /** The key is absent. */
    record Absent() implements Lookup {}

    /** The proof does not reach the key: neither presence nor absence is established. */
    record NotCovered() implements Lookup {}
  }

  /**
   * A key and its value's hash, as a listing returns them. The arrays are copied in and out.
   *
   * @param key the key
   * @param valueHash the value's hash
   */
  public record Entry(byte[] key, byte[] valueHash) {
    public Entry {
      key = Bytes.copy(key);
      valueHash = Bytes.copy(valueHash);
    }

    @Override
    public byte[] key() {
      return Bytes.copy(key);
    }

    @Override
    public byte[] valueHash() {
      return Bytes.copy(valueHash);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Entry e && java.util.Arrays.equals(e.key, key) && java.util.Arrays.equals(e.valueHash, valueHash);
    }

    @Override
    public int hashCode() {
      return java.util.Arrays.hashCode(key);
    }

    @Override
    public String toString() {
      return "Entry(" + Bytes.hex(key) + ", " + Bytes.hex(valueHash) + ")";
    }
  }

  /** A proof whose root has been recomputed and checked: what it says of the keys it covers. */
  public static final class Verified {
    private final Proofs.VerifiedTrie scala;

    private Verified(Proofs.VerifiedTrie scala) {
      this.scala = scala;
    }

    /**
     * What the proof says of {@code key}.
     *
     * @param key the key
     * @return the lookup, or the refusal of a key the mode does not accept
     */
    public Result<Lookup, TrieError> lookup(byte[] key) {
      return key == null ? Result.err(Wrap.nullArgument("a key")) : scala.lookup(key);
    }

    /**
     * Every key under {@code prefix} and its value's hash, in key order, if the proof covers the whole subtree; empty
     * if any part of it is not covered.
     *
     * @param prefix the prefix
     * @return the listing, empty when not covered, or {@code TypeMismatch} for a null prefix
     */
    public Result<Optional<List<Entry>>, TrieError> prefix(byte[] prefix) {
      return prefix == null ? Result.err(Wrap.nullArgument("a prefix")) : Result.ok(scala.prefix(prefix).map(List::copyOf));
    }

    /**
     * Every key in {@code [from, until)} and its value's hash, if no uncovered part meets the range; empty otherwise.
     *
     * @param from the lower bound
     * @param until the upper bound, or empty
     * @return the listing, empty when not covered, or {@code TypeMismatch} for a null argument
     */
    public Result<Optional<List<Entry>>, TrieError> range(byte[] from, Optional<byte[]> until) {
      return from == null || until == null
        ? Result.err(Wrap.nullArgument("bounds"))
        : Result.ok(scala.range(from, until.map(byte[]::clone)).map(List::copyOf));
    }
  }

  /**
   * The proof decoded and its root recomputed under {@code mode}, enforcing the branch invariant and the key bound: a
   * proof of another root is {@code RootMismatch}.
   *
   * @param algorithm the hash
   * @param mode the key mode
   * @param root the root
   * @param proof the proof's canonical value
   * @return the verified proof, or the refusal
   */
  public static Result<Verified, TrieError> check(Hashing.Algorithm algorithm, KeyMode mode, byte[] root, Value proof) {
    return algorithm == null || mode == null || root == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a mode, a root and a proof"))
      : Proofs.trieCheck(algorithm, mode.scala, root, Wrap.scala(proof)).map(Verified::new);
  }

  /**
   * {@code trie_verify}: whether {@code key} holds {@code claim} (or, empty, is absent) in the trie of {@code root}.
   *
   * @param algorithm the hash
   * @param mode the key mode
   * @param root the root
   * @param key the key
   * @param claim the claimed value, or empty for absence (a null value claims absence too)
   * @param proof the proof's canonical value
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, TrieError> verify(Hashing.Algorithm algorithm, KeyMode mode, byte[] root, byte[] key, Optional<Value> claim, Value proof) {
    return algorithm == null || mode == null || root == null || key == null || claim == null || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a mode, a root, a key, a claim and a proof"))
      : Proofs.trieVerify(algorithm, mode.scala, root, key, claim.map(Wrap::scala), Wrap.scala(proof));
  }

  private static List<Map.Entry<byte[], xyz.kd5ujc.core.canonical.CanonicalValue>> claims(List<Map.Entry<byte[], Value>> entries) {
    return entries.stream().map(e -> Map.entry(e.getKey().clone(), Wrap.scala(e.getValue()))).toList();
  }

  /**
   * {@code trie_prefix_verify}: whether {@code entries}, in key order, are exactly the trie's entries under {@code
   * prefix}.
   *
   * @param algorithm the hash
   * @param mode the key mode
   * @param root the root
   * @param prefix the prefix
   * @param entries the claimed keys and values
   * @param proof the proof's canonical value
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, TrieError> verifyPrefix(
    Hashing.Algorithm algorithm,
    KeyMode mode,
    byte[] root,
    byte[] prefix,
    List<Map.Entry<byte[], Value>> entries,
    Value proof
  ) {
    return algorithm == null || mode == null || root == null || prefix == null || anyNullEntry(entries) || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a mode, a root, a prefix, entries and a proof"))
      : Proofs.triePrefixVerify(algorithm, mode.scala, root, prefix, claims(entries), Wrap.scala(proof));
  }

  /**
   * {@code trie_range_verify}: whether {@code entries}, in key order, are exactly the trie's entries from {@code from}
   * (inclusive) to {@code until} (exclusive; empty for no bound).
   *
   * @param algorithm the hash
   * @param mode the key mode
   * @param root the root
   * @param from the lower bound
   * @param until the upper bound, or empty
   * @param entries the claimed keys and values
   * @param proof the proof's canonical value
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, TrieError> verifyRange(
    Hashing.Algorithm algorithm,
    KeyMode mode,
    byte[] root,
    byte[] from,
    Optional<byte[]> until,
    List<Map.Entry<byte[], Value>> entries,
    Value proof
  ) {
    return algorithm == null || mode == null || root == null || from == null || until == null || anyNullEntry(entries) || proof == null
      ? Result.err(Wrap.nullArgument("an algorithm, a mode, a root, bounds, entries and a proof"))
      : Proofs.trieRangeVerify(algorithm, mode.scala, root, from, until.map(byte[]::clone), claims(entries), Wrap.scala(proof));
  }
}
