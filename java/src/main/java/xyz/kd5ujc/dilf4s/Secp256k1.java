package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.error.KeyError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * secp256k1 on bitcoin-core's libsecp256k1, from the loaded {@link Backends}: ECDSA over SHA-256(message) (RFC 6979
 * nonces, 33-byte compressed keys, 64-byte low-S {@code r || s}); standard BIP-340 Schnorr (32-byte x-only keys, a
 * message of exactly 32 bytes); dilf4s's tagged Schnorr (BIP-340 over the tagged hash {@code dilf4s/sig/bip340/v1} of a
 * message of any length); and BIP-32. Schnorr signing takes the caller's 32 bytes of auxiliary randomness: signing that
 * draws its own is not here.
 */
public final class Secp256k1 {
  private Secp256k1() {}

  private static boolean any(Object... xs) {
    return java.util.Arrays.stream(xs).anyMatch(Objects::isNull);
  }

  /**
   * ECDSA verification.
   *
   * @param backends the loaded backends
   * @param verifyingKey 33 bytes, compressed
   * @param message the message
   * @param signature 64 bytes, low S
   * @return whether it verifies, or the refusal of a malformed key or signature
   */
  public static Result<Boolean, CryptoError> ecdsaVerify(Backends backends, byte[] verifyingKey, byte[] message, byte[] signature) {
    return any(backends, verifyingKey, message, signature)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and a signature"))
      : Signatures.ecdsaVerify(backends.secp256k1, verifyingKey, message, signature);
  }

  /**
   * The deterministic ECDSA signature of {@code message}.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes, a scalar in {@code [1, n)}
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> ecdsaSign(Backends backends, byte[] signingKey, byte[] message) {
    return any(backends, signingKey, message)
      ? Result.err(Wrap.nullArgument("backends, a key and a message"))
      : Signatures.ecdsaSign(backends.secp256k1, signingKey, message);
  }

  /**
   * The compressed verifying key of {@code signingKey}.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @return the key, or the refusal
   */
  public static Result<byte[], CryptoError> ecdsaPublicKey(Backends backends, byte[] signingKey) {
    return any(backends, signingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.ecdsaPublicKey(backends.secp256k1, signingKey);
  }

  /**
   * The ECDSA key pair of a 32-byte seed, refused when the seed is not a scalar in {@code [1, n)}.
   *
   * @param backends the loaded backends
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> ecdsaFromSeed(Backends backends, byte[] seed) {
    return any(backends, seed) ? Result.err(Wrap.nullArgument("backends and a seed")) : Signatures.ecdsaFromSeed(backends.secp256k1, seed);
  }

  /**
   * {@code verifyingKey}, if ECDSA accepts it.
   *
   * @param backends the loaded backends
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> ecdsaCheckVerifyingKey(Backends backends, byte[] verifyingKey) {
    return any(backends, verifyingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.ecdsaCheckKey(backends.secp256k1, verifyingKey);
  }

  /**
   * Standard BIP-340 verification of a 32-byte message.
   *
   * @param backends the loaded backends
   * @param verifyingKey 32 bytes, x-only
   * @param message 32 bytes
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal
   */
  public static Result<Boolean, CryptoError> bip340Verify(Backends backends, byte[] verifyingKey, byte[] message, byte[] signature) {
    return any(backends, verifyingKey, message, signature)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and a signature"))
      : Signatures.bip340Verify(backends.secp256k1, verifyingKey, message, signature);
  }

  /**
   * The standard BIP-340 signature of a 32-byte message with the caller's auxiliary randomness.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @param message 32 bytes
   * @param aux 32 bytes of fresh randomness
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> bip340Sign(Backends backends, byte[] signingKey, byte[] message, byte[] aux) {
    return any(backends, signingKey, message, aux)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and randomness"))
      : Signatures.bip340Sign(backends.secp256k1, signingKey, message, aux);
  }

  /**
   * The x-only verifying key of {@code signingKey}, for BIP-340 and the tagged scheme alike.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @return the key, or the refusal
   */
  public static Result<byte[], CryptoError> schnorrPublicKey(Backends backends, byte[] signingKey) {
    return any(backends, signingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.bip340PublicKey(backends.secp256k1, signingKey);
  }

  /**
   * The Schnorr key pair of a 32-byte seed, for BIP-340 and the tagged scheme alike.
   *
   * @param backends the loaded backends
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, CryptoError> schnorrFromSeed(Backends backends, byte[] seed) {
    return any(backends, seed) ? Result.err(Wrap.nullArgument("backends and a seed")) : Signatures.bip340FromSeed(backends.secp256k1, seed);
  }

  /**
   * {@code verifyingKey}, if BIP-340 accepts it (an x-only key that lifts to the curve).
   *
   * @param backends the loaded backends
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], CryptoError> schnorrCheckVerifyingKey(Backends backends, byte[] verifyingKey) {
    return any(backends, verifyingKey) ? Result.err(Wrap.nullArgument("backends and a key")) : Signatures.bip340CheckKey(backends.secp256k1, verifyingKey);
  }

  /**
   * dilf4s's tagged Schnorr verification of a message of any length.
   *
   * @param backends the loaded backends
   * @param verifyingKey 32 bytes, x-only
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal
   */
  public static Result<Boolean, CryptoError> taggedVerify(Backends backends, byte[] verifyingKey, byte[] message, byte[] signature) {
    return any(backends, verifyingKey, message, signature)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and a signature"))
      : Signatures.taggedVerify(backends.secp256k1, verifyingKey, message, signature);
  }

  /**
   * dilf4s's tagged Schnorr signature of {@code message} with the caller's auxiliary randomness.
   *
   * @param backends the loaded backends
   * @param signingKey 32 bytes
   * @param message the message
   * @param aux 32 bytes of fresh randomness
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> taggedSign(Backends backends, byte[] signingKey, byte[] message, byte[] aux) {
    return any(backends, signingKey, message, aux)
      ? Result.err(Wrap.nullArgument("backends, a key, a message and randomness"))
      : Signatures.taggedSign(backends.secp256k1, signingKey, message, aux);
  }

  /**
   * BIP-32: the key at {@code path} from a 16 to 64 byte seed.
   *
   * @param backends the loaded backends
   * @param seed the seed
   * @param path the indices
   * @return the key, or the refusal
   */
  public static Result<Signing.ExtendedKey, KeyError> bip32(Backends backends, byte[] seed, List<Signing.ChildIndex> path) {
    return any(backends, seed, path) || path.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("backends, a seed and a path"))
      : Signatures.bip32Derive(backends.secp256k1, seed, List.copyOf(path));
  }

  /**
   * BIP-32 soft derivation from a public key: the child at {@code index} (not hardened).
   *
   * @param backends the loaded backends
   * @param verifyingKey the parent's compressed key
   * @param chainCode the parent's chain code
   * @param index the index
   * @return the child, or the refusal
   */
  public static Result<Signing.PublicChild, KeyError> bip32PublicChild(Backends backends, byte[] verifyingKey, byte[] chainCode, Signing.ChildIndex index) {
    return any(backends, verifyingKey, chainCode, index)
      ? Result.err(Wrap.nullArgument("backends, a key, a chain code and an index"))
      : Signatures.bip32PublicChild(backends.secp256k1, verifyingKey, chainCode, index);
  }
}
