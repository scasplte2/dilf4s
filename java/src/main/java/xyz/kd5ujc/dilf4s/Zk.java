package xyz.kd5ujc.dilf4s;

import java.math.BigInteger;
import java.util.List;
import java.util.Map;

import xyz.kd5ujc.dilf4s.error.Groth16Error;
import xyz.kd5ujc.dilf4s.error.ZkError;
import xyz.kd5ujc.dilf4s.internal.Proofs;

/**
 * BN254 as the EVM precompiles define it (EIP-196, EIP-197; {@code docs/specs/zk.md} section 1) and SP1's Groth16
 * wrapper (section 2). G1 points are 64 bytes {@code x || y} ({@code (0, 0)} the identity), G2 points 128 bytes in
 * EIP-197 order, scalars 32 bytes; every point read is validated. This runs on the reference's pure engine: the native
 * one (gnark) is loaded only in an effect, so it is not reached here.
 *
 * <p>A Groth16 proof that does not verify is refused with its reason ({@code PairingCheckFailed}, {@code InvalidProof},
 * {@code Rejected}), as the reference's is.
 */
public final class Zk {
  private Zk() {}

  /** An SP1 Groth16 proof's length: a 4-byte selector, then 11 words. */
  public static final long SP1_PROOF_LENGTH = Proofs.Sp1ProofLength();

  /** The SP1 verifying key root the wrapper pins. */
  public static final BigInteger SP1_VK_ROOT = Proofs.Sp1VkRoot();

  /**
   * {@code a + b} in G1.
   *
   * @param a 64 bytes
   * @param b 64 bytes
   * @return the sum, or the refusal
   */
  public static Result<byte[], ZkError> add(byte[] a, byte[] b) {
    return a == null || b == null ? Result.err(Wrap.nullArgument("two points")) : Proofs.bn254Add(a, b);
  }

  /**
   * {@code k * p} in G1.
   *
   * @param p 64 bytes
   * @param k 32 bytes, big-endian
   * @return the product, or the refusal
   */
  public static Result<byte[], ZkError> mul(byte[] p, byte[] k) {
    return p == null || k == null ? Result.err(Wrap.nullArgument("a point and a scalar")) : Proofs.bn254Mul(p, k);
  }

  /**
   * Whether the product of {@code e(g1, g2)} over the pairs is one; no pairs is true.
   *
   * @param pairs G1 (64 bytes) and G2 (128 bytes) points
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, ZkError> pairing(List<Map.Entry<byte[], byte[]>> pairs) {
    return pairs == null || pairs.stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null)
      ? Result.err(Wrap.nullArgument("pairs"))
      : Proofs.bn254Pairing(pairs.stream().map(e -> Map.entry(e.getKey().clone(), e.getValue().clone())).toList());
  }

  /**
   * SP1's Groth16 verification (v6): the program's verifying key and public values against the proof.
   *
   * @param programVKey 32 bytes
   * @param publicValues the public values
   * @param proof {@link #SP1_PROOF_LENGTH} bytes
   * @return {@link Verified}, or the refusal
   */
  public static Result<Verified, Groth16Error> sp1Verify(byte[] programVKey, byte[] publicValues, byte[] proof) {
    return programVKey == null || publicValues == null || proof == null
      ? Result.err(Wrap.nullArgument("a key, public values and a proof"))
      : Proofs.sp1Verify(programVKey, publicValues, proof);
  }

  /**
   * SP1's public values digest: {@code sha256(publicValues) mod 2^253}.
   *
   * @param publicValues the public values
   * @return the digest, or {@code TypeMismatch} for null
   */
  public static Result<BigInteger, Groth16Error> sp1PublicValuesHash(byte[] publicValues) {
    return publicValues == null ? Result.err(Wrap.nullArgument("public values")) : Result.ok(Proofs.sp1HashPublicValues(publicValues));
  }
}
