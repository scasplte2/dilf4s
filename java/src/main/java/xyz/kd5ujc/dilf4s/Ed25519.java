package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * Ed25519 (RFC 8032): 32-byte seeds and keys, 64-byte signatures. {@link #verify} is ZIP-215, the consensus rule;
 * {@link #verifyStrict} is RFC 8032's (canonical {@code A} and {@code R}); {@link #verifyLibsodium} is libsodium's.
 */
public final class Ed25519 {
  private Ed25519() {}

  /**
   * ZIP-215 verification.
   *
   * @param verifyingKey 32 bytes
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal of a malformed key or signature
   */
  public static Result<Boolean, CryptoError> verify(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.ed25519Verify(verifyingKey, message, signature);
  }

  /**
   * RFC 8032 verification: rejects non-canonical encodings that ZIP-215 accepts.
   *
   * @param verifyingKey 32 bytes
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal
   */
  public static Result<Boolean, CryptoError> verifyStrict(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.ed25519VerifyStrict(verifyingKey, message, signature);
  }

  /**
   * libsodium's {@code crypto_sign_ed25519_verify_detached} (Cardano's rule): stricter than ZIP-215.
   *
   * @param verifyingKey 32 bytes
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal
   */
  public static Result<Boolean, CryptoError> verifyLibsodium(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.ed25519VerifyLibsodium(verifyingKey, message, signature);
  }

  /**
   * The signature of {@code message}: deterministic.
   *
   * @param signingKey 32 bytes
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> sign(byte[] signingKey, byte[] message) {
    return signingKey == null || message == null
      ? Result.err(Wrap.nullArgument("a key and a message"))
      : Signatures.ed25519Sign(signingKey, message);
  }

  /**
   * The verifying key of {@code signingKey}.
   *
   * @param signingKey 32 bytes
   * @return the verifying key, or the refusal
   */
  public static Result<byte[], CryptoError> publicKey(byte[] signingKey) {
    return signingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.ed25519PublicKey(signingKey);
  }

  /**
   * The key pair of a 32-byte seed (the RFC 8032 secret key is the seed).
   *
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> fromSeed(byte[] seed) {
    return seed == null ? Result.err(Wrap.nullArgument("a seed")) : Signatures.ed25519FromSeed(seed);
  }

  /**
   * {@code verifyingKey}, if the scheme accepts it.
   *
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> checkVerifyingKey(byte[] verifyingKey) {
    return verifyingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.ed25519CheckKey(verifyingKey);
  }
}
