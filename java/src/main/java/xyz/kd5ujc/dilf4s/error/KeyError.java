package xyz.kd5ujc.dilf4s.error;

/** A key derivation the reference refuses, or a null argument ({@link TypeMismatch}). */
public sealed interface KeyError
  permits
    KeyError.InvalidEntropy,
    KeyError.InvalidMnemonic,
    KeyError.InvalidWordList,
    KeyError.InvalidPassphrase,
    KeyError.InvalidDerivation,
    KeyError.InvalidPath,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InvalidEntropy}. */
  record InvalidEntropy(String reason) implements KeyError {
    @Override
    public String kind() {
      return "InvalidEntropy";
    }
  }

  /** {@code InvalidMnemonic}. */
  record InvalidMnemonic(String reason) implements KeyError {
    @Override
    public String kind() {
      return "InvalidMnemonic";
    }
  }

  /** {@code InvalidWordList}. */
  record InvalidWordList(String reason, java.util.Optional<Preview> separator) implements KeyError {
    @Override
    public String kind() {
      return "InvalidWordList";
    }
  }

  /** {@code InvalidPassphrase}: a passphrase holding a code point Unicode 15.0 does not assign. */
  record InvalidPassphrase(String reason) implements KeyError {
    @Override
    public String kind() {
      return "InvalidPassphrase";
    }
  }

  /** {@code InvalidDerivation}. */
  record InvalidDerivation(String reason) implements KeyError {
    @Override
    public String kind() {
      return "InvalidDerivation";
    }
  }

  /** {@code InvalidPath}. */
  record InvalidPath(PathRefusal refusal, String reason) implements KeyError {
    @Override
    public String kind() {
      return "InvalidPath";
    }
  }
}
