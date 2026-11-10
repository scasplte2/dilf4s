package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.CryptoError;

final class EcrecoverErrorTest {

  @Test
  void nativeFailuresRetainTheirProtocolCategory() {
    assertEquals(new CryptoError.InvalidKey("not a secp256k1 public key"),
      V.err(Ecrecover.address(new byte[0]), "invalid public key"));
    assertEquals(new CryptoError.InvalidEncoding("ecrecover digest", "the digest is 32 bytes, got 31"),
      V.err(Ecrecover.recover(new byte[31], new byte[65]), "short digest"));
    assertEquals(new CryptoError.InvalidEncoding("ecrecover signature", "a signature is 65 bytes (r ‖ s ‖ v), got 64"),
      V.err(Ecrecover.recover(new byte[32], new byte[64]), "short signature"));
    final byte[] signature = new byte[65];
    signature[64] = 2;
    assertEquals(new CryptoError.InvalidEncoding("ecrecover signature", "v is 27 or 28 (or 0 or 1), got 2"),
      V.err(Ecrecover.recover(new byte[32], signature), "invalid recovery ID"));
  }
}
