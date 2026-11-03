package xyz.kd5ujc.crypto.signing;

/**
 * The C ABI of dilf4s-jvm-ffi ({@code sdk/rust/jvm-ffi}), bound with JNA's direct mapping by {@code Edwards25519Native}:
 * JNA registers static native methods only, which Scala cannot declare. The native methods are private, and each
 * wrapper passes an array with its own length, so the library reads exactly the bytes of the arrays it is given and
 * writes only into {@code beta}. Callers pass distinct arrays that nothing else touches during the call
 * ({@code ByteVector.toArray} copies; {@code beta} is fresh).
 */
final class Edwards25519Ffi {

  private Edwards25519Ffi() {}

  private static native int dilf4s_jvm_ffi_abi_version();

  private static native int dilf4s_ed25519_check_key(byte[] key, int keyLen);

  private static native int dilf4s_ed25519_verify_zip215(byte[] key, int keyLen, byte[] msg, int msgLen, byte[] sig, int sigLen);

  private static native int dilf4s_ed25519_verify_libsodium(byte[] key, int keyLen, byte[] msg, int msgLen, byte[] sig, int sigLen);

  private static native int dilf4s_vrf_check_key(int suite, byte[] key, int keyLen);

  private static native int dilf4s_vrf_verify(
      int suite, byte[] key, int keyLen, byte[] alpha, int alphaLen, byte[] proof, int proofLen, byte[] beta, int betaLen);

  static int abiVersion() {
    return dilf4s_jvm_ffi_abi_version();
  }

  static int checkKey(byte[] key) {
    return dilf4s_ed25519_check_key(key, key.length);
  }

  static int verifyZip215(byte[] key, byte[] msg, byte[] sig) {
    return dilf4s_ed25519_verify_zip215(key, key.length, msg, msg.length, sig, sig.length);
  }

  static int verifyLibsodium(byte[] key, byte[] msg, byte[] sig) {
    return dilf4s_ed25519_verify_libsodium(key, key.length, msg, msg.length, sig, sig.length);
  }

  static int vrfCheckKey(int suite, byte[] key) {
    return dilf4s_vrf_check_key(suite, key, key.length);
  }

  static int vrfVerify(int suite, byte[] key, byte[] alpha, byte[] proof, byte[] beta) {
    return dilf4s_vrf_verify(suite, key, key.length, alpha, alpha.length, proof, proof.length, beta, beta.length);
  }
}
