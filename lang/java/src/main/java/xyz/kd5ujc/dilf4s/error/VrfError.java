package xyz.kd5ujc.dilf4s.error;

/** A VRF refusal ({@code docs/specs/vrf.md} section 3), or a null argument ({@link TypeMismatch}). */
public sealed interface VrfError
  permits
    VrfError.Malformed,
    VrfError.InvalidPublicKey,
    VrfError.SmallOrderPublicKey,
    VrfError.InvalidGamma,
    VrfError.NonCanonicalScalar,
    VrfError.ChallengeMismatch,
    VrfError.KeyMismatch,
    VrfError.HashToCurveFailed,
    TypeMismatch {

  /** The kind, as the schema and the vectors name it. */
  String kind();

  /** {@code Malformed}. */
  record Malformed(String what, String reason) implements VrfError {
    @Override
    public String kind() {
      return "Malformed";
    }
  }

  /** {@code InvalidPublicKey}. */
  record InvalidPublicKey() implements VrfError {
    @Override
    public String kind() {
      return "InvalidPublicKey";
    }
  }

  /** {@code SmallOrderPublicKey}. */
  record SmallOrderPublicKey() implements VrfError {
    @Override
    public String kind() {
      return "SmallOrderPublicKey";
    }
  }

  /** {@code InvalidGamma}. */
  record InvalidGamma() implements VrfError {
    @Override
    public String kind() {
      return "InvalidGamma";
    }
  }

  /** {@code NonCanonicalScalar}. */
  record NonCanonicalScalar() implements VrfError {
    @Override
    public String kind() {
      return "NonCanonicalScalar";
    }
  }

  /** {@code ChallengeMismatch}. */
  record ChallengeMismatch() implements VrfError {
    @Override
    public String kind() {
      return "ChallengeMismatch";
    }
  }

  /** {@code KeyMismatch}. */
  record KeyMismatch() implements VrfError {
    @Override
    public String kind() {
      return "KeyMismatch";
    }
  }

  /** {@code HashToCurveFailed}. */
  record HashToCurveFailed(String reason) implements VrfError {
    @Override
    public String kind() {
      return "HashToCurveFailed";
    }
  }
}
