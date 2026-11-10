package xyz.kd5ujc.dilf4s.error;

/** A Poseidon Merkle tree refusal, or a null argument ({@link TypeMismatch}). */
public sealed interface PoseidonMerkleError
  permits
    PoseidonMerkleError.Position,
    PoseidonMerkleError.Depth,
    PoseidonMerkleError.OccupiedPosition,
    PoseidonMerkleError.Encoding,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Position}. */
  record Position(long positionBits, long depth) implements PoseidonMerkleError {
    @Override
    public String kind() {
      return "Position";
    }
  }

  /** {@code Depth}. */
  record Depth(long depth) implements PoseidonMerkleError {
    @Override
    public String kind() {
      return "Depth";
    }
  }

  /** {@code OccupiedPosition}. */
  record OccupiedPosition(long positionBits) implements PoseidonMerkleError {
    @Override
    public String kind() {
      return "OccupiedPosition";
    }
  }

  /** {@code Encoding}. */
  record Encoding(String reason) implements PoseidonMerkleError {
    @Override
    public String kind() {
      return "Encoding";
    }
  }
}
