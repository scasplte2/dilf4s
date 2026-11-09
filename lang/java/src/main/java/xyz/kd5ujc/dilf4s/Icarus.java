package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Objects;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.error.KeyError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * Ed25519-BIP32 (Cardano's extended keys) with Icarus root derivation: 64-byte signing keys {@code kL || kR}, Ed25519
 * verifying keys and signatures, and BIP-32-style child keys (hardened and soft).
 */
public final class Icarus {
  private Icarus() {}

  /**
   * Verification, as Ed25519's ZIP-215.
   *
   * @param verifyingKey 32 bytes
   * @param message the message
   * @param signature 64 bytes
   * @return whether it verifies, or the refusal
   */
  public static Result<Boolean, CryptoError> verify(byte[] verifyingKey, byte[] message, byte[] signature) {
    return verifyingKey == null || message == null || signature == null
      ? Result.err(Wrap.nullArgument("a key, a message and a signature"))
      : Signatures.icarusVerify(verifyingKey, message, signature);
  }

  /**
   * The deterministic signature of {@code message} under an extended signing key.
   *
   * @param signingKey 64 bytes
   * @param message the message
   * @return the signature, or the refusal
   */
  public static Result<byte[], CryptoError> sign(byte[] signingKey, byte[] message) {
    return signingKey == null || message == null
      ? Result.err(Wrap.nullArgument("a key and a message"))
      : Signatures.icarusSign(signingKey, message);
  }

  /**
   * The verifying key of an extended signing key.
   *
   * @param signingKey 64 bytes
   * @return the verifying key, or the refusal
   */
  public static Result<byte[], CryptoError> publicKey(byte[] signingKey) {
    return signingKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.icarusPublicKey(signingKey);
  }

  /**
   * The Icarus root key from its 96-byte secret, clamped. The secret is PBKDF2-HMAC-SHA512 with the passphrase (UTF-8)
   * as the password and the BIP-39 entropy as the salt, 4096 rounds, 96 bytes; compute it with any PBKDF2 (JCA's
   * {@code PBKDF2WithHmacSHA512} takes a {@code char[]} password, BouncyCastle's {@code PKCS5S2ParametersGenerator}
   * bytes). The reference's own derivation from entropy is not offered: its PBKDF2 recurses as deep as its rounds.
   *
   * @param secret 96 bytes
   * @return the root key, or {@code InvalidDerivation}
   */
  public static Result<Signing.ExtendedKey, KeyError> root(byte[] secret) {
    return secret == null ? Result.err(Wrap.nullArgument("a secret")) : Signatures.icarusRoot(secret);
  }

  /**
   * The key at {@code path} below {@code root}.
   *
   * @param root the root (or any parent)
   * @param path the indices
   * @return the key, or the refusal
   */
  public static Result<Signing.ExtendedKey, KeyError> derive(Signing.ExtendedKey root, List<Signing.ChildIndex> path) {
    return root == null || root.key() == null || root.chainCode() == null || path == null || path.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("a key and a path"))
      : Signatures.icarusDerive(root, List.copyOf(path));
  }
}
