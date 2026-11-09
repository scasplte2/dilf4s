package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * ECDSA over P-256 and SHA-256(message): RFC 6979 deterministic nonces, 33-byte compressed keys, 64-byte {@code r || s}
 * with low {@code s} only.
 */
public final class P256 {
  private P256() {}

  /**
   * Verification.
   *
   * @param verifyingKey 33 bytes, compressed
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal of a malformed key or signature
   */
  public static Result<Boolean, CryptoError> verify(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.p256Verify(verifyingKey, message, signature);
  }

  /**
   * The deterministic signature of {@code message}.
   *
   * @param signingKey 32 bytes, a scalar in {@code [1, n)}
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> sign(byte[] signingKey, byte[] message) {
    return signingKey == null || message == null
      ? Result.err(Wrap.nullArgument("a key and a message"))
      : Signatures.p256Sign(signingKey, message);
  }

  /**
   * The verifying key of {@code signingKey}.
   *
   * @param signingKey 32 bytes
   * @return the compressed verifying key, or the refusal
   */
  public static Result<byte[], CryptoError> publicKey(byte[] signingKey) {
    return signingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.p256PublicKey(signingKey);
  }

  /**
   * The key pair of a 32-byte seed, refused when the seed is not a scalar in {@code [1, n)}.
   *
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> fromSeed(byte[] seed) {
    return seed == null ? Result.err(Wrap.nullArgument("a seed")) : Signatures.p256FromSeed(seed);
  }

  /**
   * {@code verifyingKey}, if the scheme accepts it.
   *
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> checkVerifyingKey(byte[] verifyingKey) {
    return verifyingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.p256CheckKey(verifyingKey);
  }
}
