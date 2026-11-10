package xyz.kd5ujc.dilf4s.error;

/**
 * An encoding or rendering longer than a byte limit: the limit as applied, never how far past it the output would go
 * ({@code docs/specs/conformance.md} section 3).
 *
 * @param limit the limit, in bytes
 */
public record TooLarge(long limit) implements EncodeError, RenderError {

  @Override
  public String kind() {
    return "TooLarge";
  }
}
