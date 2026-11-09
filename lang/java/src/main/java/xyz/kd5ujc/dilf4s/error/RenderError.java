package xyz.kd5ujc.dilf4s.error;

/**
 * Why a value was not rendered as JSON: JSON cannot represent it (bytes, a rational with no terminating decimal, a number
 * past the bounds the parser accepts; a null argument is {@link TypeMismatch} here), or its JSON passes the limit.
 */
public sealed interface RenderError permits RenderError.Canonical, TooLarge {

  /** The kind: {@code Canonical} or {@code TooLarge}. */
  String kind();

  /**
   * What JSON cannot hold.
   *
   * @param error the refusal
   */
  record Canonical(CanonicalError error) implements RenderError {
    @Override
    public String kind() {
      return "Canonical";
    }
  }
}
