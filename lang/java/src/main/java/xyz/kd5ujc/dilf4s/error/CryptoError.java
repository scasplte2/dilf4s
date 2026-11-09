package xyz.kd5ujc.dilf4s.error;

/** A signing key, verifying key or signature its scheme does not accept ({@code crypto-errors.cddl}), or a null argument ({@link TypeMismatch}). */
public sealed interface CryptoError
  permits
    CryptoError.InvalidEncoding,
    CryptoError.InvalidKey,
    CryptoError.BackendFailure,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code InvalidEncoding}. */
  record InvalidEncoding(String what, String reason) implements CryptoError {
    @Override
    public String kind() {
      return "InvalidEncoding";
    }
  }

  /** {@code InvalidKey}. */
  record InvalidKey(String reason) implements CryptoError {
    @Override
    public String kind() {
      return "InvalidKey";
    }
  }

  /** {@code BackendFailure}. */
  record BackendFailure(String operation, String reason) implements CryptoError {
    @Override
    public String kind() {
      return "BackendFailure";
    }
  }
}
