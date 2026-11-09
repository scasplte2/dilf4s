package xyz.kd5ujc.dilf4s;

import xyz.kd5ujc.dilf4s.error.EncodeError;
import xyz.kd5ujc.dilf4s.internal.Hashes;

/**
 * The bytes a value commits to as a Merkle leaf or a trie value ({@code docs/specs/merkle.md} section 2): {@code 0x00
 * || b} for a byte string {@code b}, {@code 0x01 || cbor(v)} for any other value, so that bytes and the text with the
 * same encoding never commit alike.
 */
public final class ValueCommitment {
  private ValueCommitment() {}

  /** The tag of a byte string's commitment. */
  public static final byte BYTES_TAG = Hashes.BytesTag();

  /** The tag of any other value's commitment. */
  public static final byte CBOR_TAG = Hashes.CborTag();

  /**
   * The commitment: {@link #bytesWithin} at {@link Cbor#MAX_ENCODED_BYTES}.
   *
   * @param value the value
   * @return the commitment, or {@code TooLarge}
   */
  public static Result<byte[], EncodeError> bytes(Value value) {
    return bytesWithin(value, Cbor.MAX_ENCODED_BYTES);
  }

  /**
   * The commitment, if it is at most {@code maxBytes} long, tag included, refused before a byte is written.
   *
   * @param value the value
   * @param maxBytes the limit
   * @return the commitment, or {@code TooLarge}
   */
  public static Result<byte[], EncodeError> bytesWithin(Value value, long maxBytes) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Hashes.commitment(Wrap.scala(value), maxBytes);
  }
}
