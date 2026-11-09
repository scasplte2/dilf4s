package xyz.kd5ujc.dilf4s;

import java.util.Optional;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/** Ethereum's {@code ecrecover}: the address of the secp256k1 key that signed a 32-byte digest. */
public final class Ecrecover {
  private Ecrecover() {}

  /**
   * The signer's 20-byte address; empty when the signature recovers no key (r or s out of range, high s, or r not an
   * x-coordinate). A wrong length or a {@code v} other than 27, 28, 0 or 1 is {@code InvalidEncoding}.
   *
   * @param digest 32 bytes
   * @param signature 65 bytes, {@code r || s || v}
   * @return the address, empty, or the refusal
   */
  public static Result<Optional<byte[]>, CryptoError> recover(byte[] digest, byte[] signature) {
    return digest == null || signature == null
      ? Result.err(Wrap.nullArgument("a digest and a signature"))
      : Signatures.ecrecover(digest, signature);
  }

  /**
   * The Ethereum address of a secp256k1 public key: the last 20 bytes of the Keccak-256 of its uncompressed {@code x ||
   * y}.
   *
   * @param publicKey a SEC1 key, 33 bytes compressed or 65 uncompressed
   * @return the address, or {@code InvalidEncoding} for bytes that are not a curve point
   */
  public static Result<byte[], CryptoError> address(byte[] publicKey) {
    return publicKey == null ? Result.err(Wrap.nullArgument("a key")) : Signatures.ethereumAddress(publicKey);
  }
}
