package xyz.kd5ujc.dilf4s.error;

/** A Merkle refusal ({@code docs/specs/merkle.md}), or a null argument ({@link TypeMismatch}). */
public sealed interface MerkleError
  permits
    MerkleError.IndexOutOfRange,
    MerkleError.InvalidSizes,
    MerkleError.InvalidProof,
    MerkleError.RootMismatch,
    MerkleError.Encoding,
    MerkleError.TooLarge,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code IndexOutOfRange}. */
  record IndexOutOfRange(java.math.BigInteger index, java.math.BigInteger size) implements MerkleError {
    @Override
    public String kind() {
      return "IndexOutOfRange";
    }
  }

  /** {@code InvalidSizes}. */
  record InvalidSizes(java.math.BigInteger oldSize, java.math.BigInteger newSize) implements MerkleError {
    @Override
    public String kind() {
      return "InvalidSizes";
    }
  }

  /** {@code InvalidProof}. */
  record InvalidProof(String reason) implements MerkleError {
    @Override
    public String kind() {
      return "InvalidProof";
    }
  }

  /** {@code RootMismatch}. */
  record RootMismatch(String expected, String computed) implements MerkleError {
    @Override
    public String kind() {
      return "RootMismatch";
    }
  }

  /** {@code Encoding}. */
  record Encoding(String reason) implements MerkleError {
    @Override
    public String kind() {
      return "Encoding";
    }
  }

  /** {@code TooLarge}. */
  record TooLarge(long limit) implements MerkleError {
    @Override
    public String kind() {
      return "TooLarge";
    }
  }
}
