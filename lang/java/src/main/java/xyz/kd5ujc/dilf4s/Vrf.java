package xyz.kd5ujc.dilf4s;

import java.util.Arrays;

import xyz.kd5ujc.dilf4s.error.VrfError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * ECVRF over edwards25519 ({@code docs/specs/vrf.md}): RFC 9381's TAI and ELL2 suites and Cardano's draft-03. Proofs
 * are 80 bytes and outputs 64; a refusal names why ({@code ChallengeMismatch} for a proof that does not verify).
 */
public final class Vrf {
  private Vrf() {}

  /** A VRF suite. */
  public enum Suite {
    /** {@code ECVRF-EDWARDS25519-SHA512-TAI} (RFC 9381). */
    TAI,
    /** {@code ECVRF-EDWARDS25519-SHA512-ELL2} (RFC 9381). */
    ELL2,
    /** Cardano's draft-03, with 64-byte {@code seed || pk} signing keys. */
    DRAFT03;

    /**
     * The suite's name in the specs and vectors.
     *
     * @return the name
     */
    public String wire() {
      return Signatures.vrfName(this);
    }
  }

  /**
   * A proof and its output. The arrays are copied in and out.
   *
   * @param pi the proof
   * @param beta the output
   */
  public record Proof(byte[] pi, byte[] beta) {
    public Proof {
      pi = Bytes.copy(pi);
      beta = Bytes.copy(beta);
    }

    @Override
    public byte[] pi() {
      return Bytes.copy(pi);
    }

    @Override
    public byte[] beta() {
      return Bytes.copy(beta);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Proof p && Arrays.equals(p.pi, pi) && Arrays.equals(p.beta, beta);
    }

    @Override
    public int hashCode() {
      return Arrays.hashCode(pi);
    }

    @Override
    public String toString() {
      return "Proof(" + Bytes.hex(pi) + ", " + Bytes.hex(beta) + ")";
    }
  }

  /**
   * The output {@code beta} of a verified proof.
   *
   * @param suite the suite
   * @param verifyingKey 32 bytes
   * @param alpha the input
   * @param pi 80 bytes
   * @return the output, or the refusal
   */
  public static Result<byte[], VrfError> verify(Suite suite, byte[] verifyingKey, byte[] alpha, byte[] pi) {
    return suite == null || verifyingKey == null || alpha == null || pi == null
      ? Result.err(Wrap.nullArgument("a suite, a key, an input and a proof"))
      : Signatures.vrfVerify(suite, verifyingKey, alpha, pi);
  }

  /**
   * The proof of {@code alpha} and its output.
   *
   * @param suite the suite
   * @param signingKey 32 bytes (64 for draft-03)
   * @param alpha the input
   * @return the proof, or the refusal
   */
  public static Result<Proof, VrfError> prove(Suite suite, byte[] signingKey, byte[] alpha) {
    return suite == null || signingKey == null || alpha == null
      ? Result.err(Wrap.nullArgument("a suite, a key and an input"))
      : Signatures.vrfProve(suite, signingKey, alpha);
  }

  /**
   * The verifying key of {@code signingKey}.
   *
   * @param suite the suite
   * @param signingKey the signing key
   * @return the verifying key, or the refusal
   */
  public static Result<byte[], VrfError> publicKey(Suite suite, byte[] signingKey) {
    return suite == null || signingKey == null
      ? Result.err(Wrap.nullArgument("a suite and a key"))
      : Signatures.vrfPublicKey(suite, signingKey);
  }

  /**
   * The key pair of a 32-byte seed (a draft-03 signing key is {@code seed || pk}).
   *
   * @param suite the suite
   * @param seed 32 bytes
   * @return the pair, or the refusal
   */
  public static Result<Signing.KeyPair, VrfError> fromSeed(Suite suite, byte[] seed) {
    return suite == null || seed == null ? Result.err(Wrap.nullArgument("a suite and a seed")) : Signatures.vrfFromSeed(suite, seed);
  }

  /**
   * {@code verifyingKey}, if the suite accepts it.
   *
   * @param suite the suite
   * @param verifyingKey the key
   * @return a copy of the key, or the refusal
   */
  public static Result<byte[], VrfError> checkVerifyingKey(Suite suite, byte[] verifyingKey) {
    return suite == null || verifyingKey == null
      ? Result.err(Wrap.nullArgument("a suite and a key"))
      : Signatures.vrfCheckKey(suite, verifyingKey);
  }
}
