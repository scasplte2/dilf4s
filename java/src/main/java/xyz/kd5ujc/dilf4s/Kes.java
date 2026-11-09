package xyz.kd5ujc.dilf4s;

import java.util.Arrays;

import xyz.kd5ujc.dilf4s.error.KesError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * Key-evolving signatures ({@code docs/specs/kes.md}): dilf4s's sum and product compositions of Ed25519 over a hash, and
 * Cardano's sum. A key is its bytes and its period; every operation takes the construction it belongs to.
 */
public final class Kes {
  private Kes() {}

  /** A KES construction and its parameters, checked where it is used ({@code Malformed}). */
  public sealed interface Construction permits Construction.Sum, Construction.Product, Construction.CardanoSum {

    /**
     * dilf4s's sum composition: {@code 2^height} periods.
     *
     * @param hash the tree's hash
     * @param height the tree's height
     */
    record Sum(Hashing.Algorithm hash, int height) implements Construction {}

    /**
     * dilf4s's product composition: {@code 2^(superHeight + subHeight)} periods.
     *
     * @param hash the trees' hash
     * @param superHeight the outer tree's height
     * @param subHeight the inner trees' height
     */
    record Product(Hashing.Algorithm hash, int superHeight, int subHeight) implements Construction {}

    /**
     * Cardano's sum composition ({@code Sum6} at depth 6).
     *
     * @param depth the tree's depth
     */
    record CardanoSum(int depth) implements Construction {}
  }

  /**
   * A construction's sizes.
   *
   * @param totalPeriods how many periods a key signs for
   * @param keyLength a signing key's length, in bytes
   * @param signatureLength a signature's length, in bytes
   */
  public record Shape(long totalPeriods, long keyLength, long signatureLength) {}

  /**
   * A signing key at a period, and its verifying key. The arrays are copied in and out; {@code toString} never shows the
   * signing key.
   *
   * @param key the signing key's bytes
   * @param period its period
   * @param verifyingKey the verifying key
   */
  public record Key(byte[] key, long period, byte[] verifyingKey) {
    public Key {
      key = Bytes.copy(key);
      verifyingKey = Bytes.copy(verifyingKey);
    }

    @Override
    public byte[] key() {
      return Bytes.copy(key);
    }

    @Override
    public byte[] verifyingKey() {
      return Bytes.copy(verifyingKey);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Key k
        && java.security.MessageDigest.isEqual(k.key, key)
        && k.period == period
        && Arrays.equals(k.verifyingKey, verifyingKey);
    }

    @Override
    public int hashCode() {
      return Arrays.hashCode(verifyingKey) + Long.hashCode(period);
    }

    @Override
    public String toString() {
      return "Key(<redacted>, period=" + period + ", " + Bytes.hex(verifyingKey) + ")";
    }
  }

  private static boolean malformed(Construction c) {
    return switch (c) {
      case null -> true;
      case Construction.Sum s -> s.hash() == null;
      case Construction.Product p -> p.hash() == null;
      case Construction.CardanoSum d -> false;
    };
  }

  /**
   * The construction's sizes, or the refusal of its parameters.
   *
   * @param construction the construction
   * @return its shape, or {@code Malformed}
   */
  public static Result<Shape, KesError> shape(Construction construction) {
    return malformed(construction)
      ? Result.err(Wrap.nullArgument("a construction"))
      : Signatures.kesShape(construction).map(s -> new Shape(s[0], s[1], s[2]));
  }

  /**
   * Whether {@code signature} signs {@code message} at {@code period} under {@code verifyingKey}.
   *
   * @param construction the construction
   * @param verifyingKey the verifying key
   * @param period the period
   * @param message the message
   * @param signature the signature
   * @return the verdict, or the refusal ({@code Malformed}, {@code PeriodOutOfRange})
   */
  public static Result<Boolean, KesError> verify(Construction construction, byte[] verifyingKey, long period, byte[] message, byte[] signature) {
    return malformed(construction) || verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a construction, a key, a message and a signature"))
      : Signatures.kesVerify(construction, verifyingKey, period, message, signature);
  }

  /**
   * The key at period 0 from a 32-byte seed.
   *
   * @param construction the construction
   * @param seed 32 bytes
   * @return the key, or the refusal
   */
  public static Result<Key, KesError> generate(Construction construction, byte[] seed) {
    return malformed(construction) || seed == null
      ? Result.err(Wrap.nullArgument("a construction and a seed"))
      : Signatures.kesGenerate(construction, seed);
  }

  /**
   * The signature of {@code message} by {@code key} at its period.
   *
   * @param construction the construction
   * @param key the key
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], KesError> sign(Construction construction, Key key, byte[] message) {
    return malformed(construction) || key == null || key.key == null || message == null
      ? Result.err(Wrap.nullArgument("a construction, a key and a message"))
      : Signatures.kesSign(construction, key.key, key.period, message);
  }

  /**
   * The key at the next period; a key at its last period is {@code Exhausted}.
   *
   * @param construction the construction
   * @param key the key
   * @return the evolved key, or the refusal
   */
  public static Result<Key, KesError> evolve(Construction construction, Key key) {
    return malformed(construction) || key == null || key.key == null
      ? Result.err(Wrap.nullArgument("a construction and a key"))
      : Signatures.kesEvolve(construction, key.key, key.period);
  }

  /**
   * A key read back from its bytes at {@code period}.
   *
   * @param construction the construction
   * @param key the key's bytes
   * @param period its period
   * @return the key, or the refusal
   */
  public static Result<Key, KesError> load(Construction construction, byte[] key, long period) {
    return malformed(construction) || key == null
      ? Result.err(Wrap.nullArgument("a construction and a key"))
      : Signatures.kesLoad(construction, key, period);
  }
}
