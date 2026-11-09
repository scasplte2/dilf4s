package xyz.kd5ujc.dilf4s.error;

/**
 * A typed value outside its schema (CanonicalError's {@code TypeMismatch}), or, in every family, an argument that is
 * not of its static type: in Java, a null. As the TypeScript SDK's {@code TypeMismatch}, it joins each family, so a
 * refusal of a null is a value, never an exception.
 *
 * @param reason what does not fit, never the value itself
 */
public record TypeMismatch(String reason)
  implements CanonicalError, HashError, CryptoError, KeyError, VrfError, KesError, SigmaError, ZkError, Groth16Error, MerkleError, PoseidonError, PoseidonMerkleError, TrieError, StateKeyError, VersionedTrieError, JlvmError, EncodeError {

  @Override
  public String kind() {
    return "TypeMismatch";
  }
}
