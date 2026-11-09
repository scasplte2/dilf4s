package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * BLS signatures over BLS12-381 on blst, from the loaded {@link Backends}: Ethereum's proof-of-possession profile, keys
 * in G1 (48 bytes) and signatures in G2 (96 bytes), with aggregation, proofs of possession and EIP-2333 keys.
 */
public final class Bls {
  private Bls() {}

  private static boolean any(Object... xs) {
    return java.util.Arrays.stream(xs).anyMatch(Objects::isNull);
  }

  /**
   * Verification.
   *
   * @param backends the loaded backends
   * @param verifyingKey 48 bytes
   * @param message the message
   * @param signature 96 bytes
   * @return whether it verifies, or the refusal of a malformed key or signature
   */
  public static Result<Boolean, CryptoError> verify(Backends backends, byte[] verifyingKey, byte[] message, byte[] signature) {
    return any(backends, verifyingKey, message, signature)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and a signature"))
      : Signatures.blsVerify(backends.bls, verifyingKey, message, signature);
  }

  /**
   * The signature of {@code message}: deterministic.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> sign(Backends backends, byte[] signingKey, byte[] message) {
    return any(backends, signingKey, message) ? Result.err(Wrap.nullArgument("backends, a key and a message")) : Signatures.blsSign(backends.bls, signingKey, message);
  }

  /**
   * The verifying key of {@code signingKey}.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @return the key, or the refusal
   */
  public static Result<byte[], CryptoError> publicKey(Backends backends, byte[] signingKey) {
    return any(backends, signingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.blsPublicKey(backends.bls, signingKey);
  }

  /**
   * The key pair of a 32-byte seed.
   *
   * @param backends the loaded backends
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> fromSeed(Backends backends, byte[] seed) {
    return any(backends, seed) ? Result.err(Wrap.nullArgument("backends and a seed")) : Signatures.blsFromSeed(backends.bls, seed);
  }

  /**
   * {@code verifyingKey}, if it is a key: a compressed G1 point in the subgroup, not the identity.
   *
   * @param backends the loaded backends
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> checkVerifyingKey(Backends backends, byte[] verifyingKey) {
    return any(backends, verifyingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.blsCheckKey(backends.bls, verifyingKey);
  }

  /**
   * The aggregate of {@code signatures}.
   *
   * @param backends the loaded backends
   * @param signatures 96 bytes each
   * @return the aggregate, or the refusal
   */
  public static Result<byte[], CryptoError> aggregate(Backends backends, List<byte[]> signatures) {
    return backends == null || Bytes.anyNull(signatures)
      ? Result.err(Wrap.nullArgument("backends and signatures"))
      : Signatures.blsAggregate(backends.bls, List.copyOf(signatures));
  }

  /**
   * Whether {@code signature} signs {@code message} by every key (fast aggregate verification). The keys must carry
   * proofs of possession, checked by whoever admitted them: without that, a rogue key can forge an aggregate.
   *
   * @param backends the loaded backends
   * @param verifyingKeys the keys
   * @param message the message
   * @param signature the aggregate signature
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, CryptoError> fastAggregateVerify(Backends backends, List<byte[]> verifyingKeys, byte[] message, byte[] signature) {
    return any(backends, message, signature) || Bytes.anyNull(verifyingKeys)
      ? Result.err(Wrap.nullArgument("backends, keys, a message and a signature"))
      : Signatures.blsFastAggregateVerify(backends.bls, List.copyOf(verifyingKeys), message, signature);
  }

  /**
   * Whether {@code signature} aggregates a signature of each message by its key.
   *
   * @param backends the loaded backends
   * @param pairs keys and their messages
   * @param signature the aggregate signature
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, CryptoError> aggregateVerify(Backends backends, List<Map.Entry<byte[], byte[]>> pairs, byte[] signature) {
    return any(backends, pairs, signature) || pairs.stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null)
      ? Result.err(Wrap.nullArgument("backends, pairs and a signature"))
      : Signatures.blsAggregateVerify(backends.bls, pairs.stream().map(e -> Map.entry(e.getKey().clone(), e.getValue().clone())).toList(), signature);
  }

  /**
   * The proof of possession of {@code signingKey}.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @return the proof, or the refusal
   */
  public static Result<byte[], CryptoError> popProve(Backends backends, byte[] signingKey) {
    return any(backends, signingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.blsPopProve(backends.bls, signingKey);
  }

  /**
   * Whether {@code proof} is the proof of possession of {@code verifyingKey}.
   *
   * @param backends the loaded backends
   * @param verifyingKey 48 bytes
   * @param proof 96 bytes
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, CryptoError> popVerify(Backends backends, byte[] verifyingKey, byte[] proof) {
    return any(backends, verifyingKey, proof) ? Result.err(Wrap.nullArgument("backends, a key and a proof")) : Signatures.blsPopVerify(backends.bls, verifyingKey, proof);
  }

  /**
   * EIP-2333's master key of {@code seed} (at least 32 bytes).
   *
   * @param backends the loaded backends
   * @param seed the seed
   * @return the signing key, or the refusal
   */
  public static Result<byte[], CryptoError> eip2333Master(Backends backends, byte[] seed) {
    return any(backends, seed) ? Result.err(Wrap.nullArgument("backends and a seed")) : Signatures.blsEip2333Master(backends.bls, seed);
  }

  /**
   * EIP-2333's child of {@code parent} at {@code index} (below 2^32).
   *
   * @param backends the loaded backends
   * @param parent the parent signing key
   * @param index the index
   * @return the signing key, or the refusal
   */
  public static Result<byte[], CryptoError> eip2333Child(Backends backends, byte[] parent, long index) {
    return any(backends, parent) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.blsEip2333Child(backends.bls, parent, index);
  }
}
