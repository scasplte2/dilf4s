package xyz.kd5ujc.dilf4s;

import java.security.MessageDigest;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.KeyError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * What the signature schemes share ({@code docs/specs/signing.md}): key pairs, extended keys and derivation paths. The
 * schemes are {@link Ed25519}, {@link P256}, {@link Ristretto255} and {@link Icarus}; {@link Ecrecover}, {@link Vrf},
 * {@link Kes} and {@link Sigma} verify. Malformed input (a wrong length, a key that does not decode) is a typed error; a
 * well-formed signature that fails verification is {@code false}.
 *
 * <p>{@link Secp256k1} (ECDSA, BIP-340, tagged Schnorr, BIP-32) and {@link Bls} run on native libraries, which the
 * reference loads only in an effect: they take the loaded {@link Backends}. Signing that draws its own randomness, key
 * generation and the sigma prover are not here (they need {@code SecureRandom[F]}): sign with caller-supplied randomness
 * where a scheme offers it.
 */
public final class Signing {
  private Signing() {}

  /**
   * A signing key and its verifying key. The arrays are copied in and out; {@code toString} never shows the signing key,
   * and equality is constant time.
   *
   * @param signingKey the signing key
   * @param verifyingKey the verifying key
   */
  public record KeyPair(byte[] signingKey, byte[] verifyingKey) {
    public KeyPair {
      signingKey = Bytes.copy(signingKey);
      verifyingKey = Bytes.copy(verifyingKey);
    }

    @Override
    public byte[] signingKey() {
      return Bytes.copy(signingKey);
    }

    @Override
    public byte[] verifyingKey() {
      return Bytes.copy(verifyingKey);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof KeyPair p && MessageDigest.isEqual(p.signingKey, signingKey) && Arrays.equals(p.verifyingKey, verifyingKey);
    }

    @Override
    public int hashCode() {
      return Arrays.hashCode(verifyingKey);
    }

    @Override
    public String toString() {
      return "KeyPair(<redacted>, " + Bytes.hex(verifyingKey) + ")";
    }
  }

  /**
   * A key and its chain code, for hierarchical derivation. The arrays are copied in and out; {@code toString} never shows
   * the key.
   *
   * @param key the signing key
   * @param chainCode the chain code
   */
  public record ExtendedKey(byte[] key, byte[] chainCode) {
    public ExtendedKey {
      key = Bytes.copy(key);
      chainCode = Bytes.copy(chainCode);
    }

    @Override
    public byte[] key() {
      return Bytes.copy(key);
    }

    @Override
    public byte[] chainCode() {
      return Bytes.copy(chainCode);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof ExtendedKey k && MessageDigest.isEqual(k.key, key) && Arrays.equals(k.chainCode, chainCode);
    }

    @Override
    public int hashCode() {
      return Arrays.hashCode(chainCode);
    }

    @Override
    public String toString() {
      return "ExtendedKey(<redacted>, chainCode=" + Bytes.hex(chainCode) + ")";
    }
  }

  /**
   * A child index: {@code value} below 2^31, hardened or not (checked where it is used).
   *
   * @param value the index
   * @param hardened whether it is hardened ({@code '} or {@code h} in a path)
   */
  public record ChildIndex(long value, boolean hardened) {}

  /**
   * A child verifying key and its chain code, derived from the parent's public key alone (BIP-32 soft derivation). The
   * arrays are copied in and out.
   *
   * @param verifyingKey the child's compressed key
   * @param chainCode the child's chain code
   */
  public record PublicChild(byte[] verifyingKey, byte[] chainCode) {
    public PublicChild {
      verifyingKey = Bytes.copy(verifyingKey);
      chainCode = Bytes.copy(chainCode);
    }

    @Override
    public byte[] verifyingKey() {
      return Bytes.copy(verifyingKey);
    }

    @Override
    public byte[] chainCode() {
      return Bytes.copy(chainCode);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof PublicChild c && Arrays.equals(c.verifyingKey, verifyingKey) && Arrays.equals(c.chainCode, chainCode);
    }

    @Override
    public int hashCode() {
      return Arrays.hashCode(verifyingKey);
    }

    @Override
    public String toString() {
      return "PublicChild(" + Bytes.hex(verifyingKey) + ", " + Bytes.hex(chainCode) + ")";
    }
  }

  /**
   * A derivation path, {@code m} or {@code m/...}, its segments checked in order ({@code InvalidPath} with the first
   * rule broken).
   *
   * @param path the path
   * @return the indices, or the refusal
   */
  public static Result<List<ChildIndex>, KeyError> path(String path) {
    return path == null ? Result.err(Wrap.nullArgument("a path")) : Signatures.parsePath(path).map(List::copyOf);
  }

  /**
   * SLIP-10 Ed25519: the key at {@code path} from a 16 to 64 byte seed; hardened indices only.
   *
   * @param seed the seed
   * @param path the path
   * @return the key, or the refusal
   */
  public static Result<ExtendedKey, KeyError> slip10Ed25519(byte[] seed, List<ChildIndex> path) {
    return seed == null || path == null || path.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("a seed and a path"))
      : Signatures.slip10Derive(seed, List.copyOf(path));
  }
}
