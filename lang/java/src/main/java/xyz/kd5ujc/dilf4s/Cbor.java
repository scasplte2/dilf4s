package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.CanonicalError;
import xyz.kd5ujc.dilf4s.error.EncodeError;
import xyz.kd5ujc.dilf4s.internal.Values;

/**
 * The canonical CBOR encoding ({@code docs/specs/canonical-encoding.md} section 2): every value has exactly one
 * encoding, and the decoder accepts exactly those. Arrays in and out are copies.
 */
public final class Cbor {
  private Cbor() {}

  /** The longest encoding built, in bytes: 2^31 - 9. */
  public static final long MAX_ENCODED_BYTES = Values.MaxEncodedBytes();

  /**
   * The canonical encoding of {@code value}: {@link #encodeWithin} at {@link #MAX_ENCODED_BYTES}. A value with shared
   * parts can encode to far more than it occupies; past the ceiling that is {@code TooLarge}, from the weight alone when
   * the weight is over.
   *
   * @param value the value
   * @return the encoding, or the refusal
   */
  public static Result<byte[], EncodeError> encode(Value value) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Values.encode(Wrap.scala(value));
  }

  /**
   * The canonical encoding of {@code value}, if it is at most {@code maxBytes} long (a negative limit is 0, and no limit
   * is above {@link #MAX_ENCODED_BYTES}); past it, {@code TooLarge} with the limit as applied, before a byte is written.
   *
   * @param value the value
   * @param maxBytes the limit
   * @return the encoding, or the refusal
   */
  public static Result<byte[], EncodeError> encodeWithin(Value value, long maxBytes) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Values.encodeWithin(Wrap.scala(value), maxBytes);
  }

  /**
   * The length of {@code value}'s encoding, measured without encoding it, with the bounds of {@link #encodeWithin}.
   *
   * @param value the value
   * @param maxBytes the limit
   * @return the length, or the refusal
   */
  public static Result<Long, EncodeError> encodedLengthWithin(Value value, long maxBytes) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Values.encodedLengthWithin(Wrap.scala(value), maxBytes);
  }

  /**
   * The value {@code bytes} encode, nesting at most {@link Value#MAX_DEPTH} deep; anything but a canonical encoding is
   * refused with its offset and reason.
   *
   * @param bytes the encoding
   * @return the value, or the refusal
   */
  public static Result<Value, CanonicalError> decode(byte[] bytes) {
    return decode(bytes, Value.MAX_DEPTH);
  }

  /**
   * {@link #decode(byte[])} with a nesting limit of {@code maxDepth} (at most {@link Value#MAX_DEPTH}).
   *
   * @param bytes the encoding
   * @param maxDepth the deepest nesting accepted
   * @return the value, or the refusal
   */
  public static Result<Value, CanonicalError> decode(byte[] bytes, int maxDepth) {
    return bytes == null ? Result.err(Wrap.nullArgument("the bytes")) : Values.decode(bytes, maxDepth).map(Wrap::of);
  }
}
