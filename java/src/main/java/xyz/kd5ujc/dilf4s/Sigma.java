package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.SigmaError;
import xyz.kd5ujc.dilf4s.internal.Signatures;

/**
 * Sigma protocols over secp256k1, byte-compatible with Ergo ({@code docs/specs/sigma.md}): propositions of discrete-log
 * and Diffie-Hellman-tuple leaves under AND, OR and k-of-n, as canonical values in their tagged-map form. A well-formed
 * proof that fails is {@code false}; malformed input is a {@link SigmaError}.
 */
public final class Sigma {
  private Sigma() {}

  /**
   * Whether {@code proof} proves {@code proposition} for {@code message} (the JLVM's {@code sigma_verify}).
   *
   * @param proposition the proposition, in its tagged-map form
   * @param proof the proof
   * @param message the message
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, SigmaError> verify(Value proposition, byte[] proof, byte[] message) {
    return proposition == null || proof == null || message == null
      ? Result.err(Wrap.nullArgument("a proposition, a proof and a message"))
      : Signatures.sigmaVerify(Wrap.scala(proposition), proof, message);
  }

  /**
   * Whether {@code proof} is a single-key sigma proof (Ergo's P2PK signature) of {@code message} under {@code key} (the
   * JLVM's {@code schnorr_verify}).
   *
   * @param key a 33-byte compressed point
   * @param message the message
   * @param proof 56 bytes
   * @return the verdict, or the refusal
   */
  public static Result<Boolean, SigmaError> schnorrVerify(byte[] key, byte[] message, byte[] proof) {
    return key == null || message == null || proof == null
      ? Result.err(Wrap.nullArgument("a key, a message and a proof"))
      : Signatures.schnorrVerify(key, message, proof);
  }

  /**
   * The length a proof of {@code proposition} has.
   *
   * @param proposition the proposition
   * @return the length, or the refusal
   */
  public static Result<Long, SigmaError> proofLength(Value proposition) {
    return proposition == null ? Result.err(Wrap.nullArgument("a proposition")) : Signatures.sigmaProofLength(Wrap.scala(proposition));
  }
}
