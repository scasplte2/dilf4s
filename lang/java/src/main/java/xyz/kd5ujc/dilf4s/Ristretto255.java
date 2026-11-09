package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/** Schnorr signatures over ristretto255: 32-byte keys, 64-byte signatures, nonces from the caller's randomness. */
public final class Ristretto255 {
  private Ristretto255() {}

  /** The nonce randomness {@link #sign} takes, in bytes. */
  public static final int RANDOMNESS_LENGTH = Signatures.RistrettoRandomnessLength();

  /**
   * Verification.
   *
   * @param verifyingKey 32 bytes
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal of a malformed key or signature
   */
  public static Result<Boolean, CryptoError> verify(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.ristrettoVerify(verifyingKey, message, signature);
  }

  /**
   * The signature of {@code message} with {@code randomness} for its nonce: fresh, secret randomness per signature.
   *
   * @param signingKey 32 bytes
   * @param message the message
   * @param randomness {@link #RANDOMNESS_LENGTH} bytes
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> sign(byte[] signingKey, byte[] message, byte[] randomness) {
    return signingKey == null || message == null || randomness == null
      ? Result.err(Wrap.nullArgument("a key, a message and randomness"))
      : Signatures.ristrettoSign(signingKey, message, randomness);
  }

  /**
   * The verifying key of {@code signingKey}.
   *
   * @param signingKey 32 bytes
   * @return the verifying key, or the refusal
   */
  public static Result<byte[], CryptoError> publicKey(byte[] signingKey) {
    return signingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.ristrettoPublicKey(signingKey);
  }

  /**
   * The key pair of a 32-byte seed.
   *
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> fromSeed(byte[] seed) {
    return seed == null ? Result.err(Wrap.nullArgument("a seed")) : Signatures.ristrettoFromSeed(seed);
  }

  /**
   * {@code verifyingKey}, if the scheme accepts it.
   *
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> checkVerifyingKey(byte[] verifyingKey) {
    return verifyingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.ristrettoCheckKey(verifyingKey);
  }
}
