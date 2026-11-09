package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.CanonicalError;
import xyz.kd5ujc.dilf4s.error.RenderError;
import xyz.kd5ujc.dilf4s.internal.Values;

/**
 * The JSON mapping ({@code docs/specs/canonical-encoding.md} section 3): a strict parser (RFC 8259, a repeated member
 * name refused, numbers exact and bounded before any conversion) and a compact renderer with keys in code-point order.
 */
public final class Json {
  private Json() {}

  /** The most significant digits a number's significand or exponent may have. */
  public static final int MAX_DIGITS = Values.MaxDigits();

  /** The largest accepted {@code |fraction digits - exponent|} of a number. */
  public static final int MAX_SCALE = Values.MaxScale();

  /** The longest JSON {@link #render} writes, in UTF-8 bytes: 2^29 - 24. */
  public static final long MAX_RENDER_BYTES = Values.MaxRenderBytes();

  /**
   * JSON text as a canonical value.
   *
   * @param text the text
   * @return the value, or the refusal
   */
  public static Result<Value, CanonicalError> parse(String text) {
    return parse(text, MAX_SCALE);
  }

  /**
   * {@link #parse(String)} with the scale bound lowered to {@code maxScale}; a bound outside {@code 0..MAX_SCALE} is
   * {@code TypeMismatch}, before anything is read.
   *
   * @param text the text
   * @param maxScale the scale bound
   * @return the value, or the refusal
   */
  public static Result<Value, CanonicalError> parse(String text, int maxScale) {
    return text == null ? Result.err(Wrap.nullArgument("the text")) : Values.parse(text, maxScale).map(Wrap::of);
  }

  /**
   * Compact JSON: {@link #renderWithin} at {@link #MAX_RENDER_BYTES}. Bytes, a rational that does not terminate, and a
   * number past the parser's bounds have no JSON form.
   *
   * @param value the value
   * @return the text, or the refusal
   */
  public static Result<String, RenderError> render(Value value) {
    return renderWithin(value, MAX_RENDER_BYTES);
  }

  /**
   * {@link #render}, if the JSON is at most {@code maxBytes} long in UTF-8.
   *
   * @param value the value
   * @param maxBytes the limit
   * @return the text, or the refusal
   */
  public static Result<String, RenderError> renderWithin(Value value, long maxBytes) {
    return value == null
      ? Result.err(new RenderError.Canonical(Wrap.nullArgument("a value")))
      : Values.render(Wrap.scala(value), maxBytes);
  }
}
