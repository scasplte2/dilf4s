package xyz.kd5ujc.dilf4s.error;

/** Why a canonical value, encoding or JSON text was refused ({@code shared/schemas/codec-errors.cddl}). Its {@code TypeMismatch} is the shared {@link TypeMismatch}: a typed value outside its schema, or a null argument. */
public sealed interface CanonicalError
  permits
    CanonicalError.UnpairedSurrogate,
    CanonicalError.DuplicateKey,
    CanonicalError.RationalTooWide,
    CanonicalError.IntegerTooWide,
    CanonicalError.JsonSyntax,
    CanonicalError.InvalidNumber,
    CanonicalError.NotJsonRepresentable,
    TypeMismatch,
    CanonicalError.UnexpectedEnd,
    CanonicalError.TrailingBytes,
    CanonicalError.DepthExceeded,
    CanonicalError.NonCanonical,
    CanonicalError.Unsupported,
    CanonicalError.InvalidUtf8 {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code UnpairedSurrogate}. */
  record UnpairedSurrogate() implements CanonicalError {
    @Override
    public String kind() {
      return "UnpairedSurrogate";
    }
  }

  /** {@code DuplicateKey}. */
  record DuplicateKey(Preview key) implements CanonicalError {
    @Override
    public String kind() {
      return "DuplicateKey";
    }
  }

  /** {@code RationalTooWide}. */
  record RationalTooWide(long limit) implements CanonicalError {
    @Override
    public String kind() {
      return "RationalTooWide";
    }
  }

  /** {@code IntegerTooWide}. */
  record IntegerTooWide(long limit) implements CanonicalError {
    @Override
    public String kind() {
      return "IntegerTooWide";
    }
  }

  /** {@code JsonSyntax}. */
  record JsonSyntax(long offset, JsonSyntaxReason reason) implements CanonicalError {
    @Override
    public String kind() {
      return "JsonSyntax";
    }
  }

  /** {@code InvalidNumber}. */
  record InvalidNumber(java.util.OptionalLong length, java.util.OptionalLong offset, java.util.Optional<NumberBound> bound) implements CanonicalError {
    @Override
    public String kind() {
      return "InvalidNumber";
    }
  }

  /** {@code NotJsonRepresentable}. */
  record NotJsonRepresentable(NotJson what) implements CanonicalError {
    @Override
    public String kind() {
      return "NotJsonRepresentable";
    }
  }

  /** {@code UnexpectedEnd}. */
  record UnexpectedEnd(long offset) implements CanonicalError {
    @Override
    public String kind() {
      return "UnexpectedEnd";
    }
  }

  /** {@code TrailingBytes}. */
  record TrailingBytes(long offset) implements CanonicalError {
    @Override
    public String kind() {
      return "TrailingBytes";
    }
  }

  /** {@code DepthExceeded}. */
  record DepthExceeded(long limit) implements CanonicalError {
    @Override
    public String kind() {
      return "DepthExceeded";
    }
  }

  /** {@code NonCanonical}. */
  record NonCanonical(long offset, NonCanonicalReason reason) implements CanonicalError {
    @Override
    public String kind() {
      return "NonCanonical";
    }
  }

  /** {@code Unsupported}. */
  record Unsupported(long offset, UnsupportedItem item) implements CanonicalError {
    @Override
    public String kind() {
      return "Unsupported";
    }
  }

  /** {@code InvalidUtf8}. */
  record InvalidUtf8(long offset) implements CanonicalError {
    @Override
    public String kind() {
      return "InvalidUtf8";
    }
  }
}
