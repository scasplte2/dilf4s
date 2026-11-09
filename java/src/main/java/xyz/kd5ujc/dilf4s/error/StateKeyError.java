package xyz.kd5ujc.dilf4s.error;

/** Why a state key or namespace was refused ({@code docs/specs/versioned-trie.md} section 2), or a null argument ({@link TypeMismatch}). */
public sealed interface StateKeyError
  permits
    StateKeyError.Empty,
    StateKeyError.TooLong,
    StateKeyError.NotUtf8,
    StateKeyError.TooManySegments,
    StateKeyError.InvalidSegment,
    StateKeyError.SegmentTooLong,
    StateKeyError.NamespaceFull,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Empty}. */
  record Empty() implements StateKeyError {
    @Override
    public String kind() {
      return "Empty";
    }
  }

  /** {@code TooLong}. */
  record TooLong(long limit) implements StateKeyError {
    @Override
    public String kind() {
      return "TooLong";
    }
  }

  /** {@code NotUtf8}. */
  record NotUtf8() implements StateKeyError {
    @Override
    public String kind() {
      return "NotUtf8";
    }
  }

  /** {@code TooManySegments}. */
  record TooManySegments(Preview key, long limit) implements StateKeyError {
    @Override
    public String kind() {
      return "TooManySegments";
    }
  }

  /** {@code InvalidSegment}. */
  record InvalidSegment(Preview key, long segment) implements StateKeyError {
    @Override
    public String kind() {
      return "InvalidSegment";
    }
  }

  /** {@code SegmentTooLong}. */
  record SegmentTooLong(Preview key, long segment, long limit) implements StateKeyError {
    @Override
    public String kind() {
      return "SegmentTooLong";
    }
  }

  /** {@code NamespaceFull}. */
  record NamespaceFull(Preview namespace) implements StateKeyError {
    @Override
    public String kind() {
      return "NamespaceFull";
    }
  }
}
