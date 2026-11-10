package xyz.kd5ujc.dilf4s;

import java.security.MessageDigest;
import java.util.HexFormat;

import xyz.kd5ujc.dilf4s.error.EncodeError;
import xyz.kd5ujc.dilf4s.error.HashError;
import xyz.kd5ujc.dilf4s.internal.Hashes;

/**
 * Hashing ({@code docs/specs/canonical-encoding.md} section 4): the seven hash functions, digests that know their
 * algorithm, versioned domain tags, BIP-340-style tagged hashing {@code H(H(tag) || H(tag) || msg)}, content hashes of
 * canonical values, and {@code expand_message_xmd} (RFC 9380 section 5.3.1).
 */
public final class Hashing {
  private Hashing() {}

  /** The hash functions, by the names the specs and vectors use. */
  public enum Algorithm {
    /** SHA-256. */
    SHA_256,
    /** SHA-512. */
    SHA_512,
    /** SHA3-256. */
    SHA3_256,
    /** SHA3-512. */
    SHA3_512,
    /** Keccak-256 as Ethereum uses it (the pre-standard padding). */
    KECCAK_256,
    /** BLAKE2b-256. */
    BLAKE2B_256,
    /** BLAKE2b-512. */
    BLAKE2B_512;

    /**
     * Its name in the specs and vectors, e.g. {@code SHA-256}.
     *
     * @return the name
     */
    public String wire() {
      return Hashes.name(this);
    }

    /**
     * Its digest length, in bytes.
     *
     * @return the output size
     */
    public long outputSize() {
      return Hashes.outputSize(this);
    }

    /**
     * Its compression function's block size, in bytes.
     *
     * @return the block size
     */
    public long blockSize() {
      return Hashes.blockSize(this);
    }
  }

  /**
   * The hash function named {@code name} ({@code SHA-256}, ...); any other name is {@code UnknownAlgorithm}.
   *
   * @param name the name
   * @return the algorithm, or the refusal
   */
  public static Result<Algorithm, HashError> algorithm(String name) {
    return name == null ? Result.err(Wrap.nullArgument("a name")) : Hashes.named(name);
  }

  /** A digest of one algorithm: digests of different algorithms are never equal. Its bytes are copied in and out. */
  public static final class Digest {
    private final Algorithm algorithm;
    private final byte[] bytes;

    private Digest(Algorithm algorithm, byte[] bytes) {
      this.algorithm = algorithm;
      this.bytes = bytes;
    }

    /**
     * The algorithm.
     *
     * @return the algorithm
     */
    public Algorithm algorithm() {
      return algorithm;
    }

    /**
     * The digest, in a fresh array.
     *
     * @return a copy of the bytes
     */
    public byte[] bytes() {
      return bytes.clone();
    }

    /**
     * The digest in lowercase hex.
     *
     * @return the hex
     */
    public String hex() {
      return HexFormat.of().formatHex(bytes);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Digest d && d.algorithm == algorithm && MessageDigest.isEqual(d.bytes, bytes);
    }

    @Override
    public int hashCode() {
      return 31 * algorithm.hashCode() + java.util.Arrays.hashCode(bytes);
    }

    @Override
    public String toString() {
      return algorithm.wire() + ":" + hex();
    }
  }

  /** A digest from bytes the reference returned, already the algorithm's length: one copy, owned. */
  static Digest digestOf(Algorithm algorithm, byte[] owned) {
    return new Digest(algorithm, owned);
  }

  /**
   * {@code bytes} as a digest of {@code algorithm}, refused ({@code InvalidLength}) at any other length.
   *
   * @param algorithm the algorithm
   * @param bytes the digest
   * @return the digest, or the refusal
   */
  public static Result<Digest, HashError> digestFromBytes(Algorithm algorithm, byte[] bytes) {
    return algorithm == null || bytes == null
      ? Result.err(Wrap.nullArgument("an algorithm and bytes"))
      : Hashes.fromBytes(algorithm, bytes).map(b -> new Digest(algorithm, b));
  }

  /**
   * A digest from its hex: an optional {@code 0x}, then exactly twice the output size in hex digits, either case.
   *
   * @param algorithm the algorithm
   * @param hex the hex
   * @return the digest, or the refusal ({@code InvalidLength}, {@code InvalidHex})
   */
  public static Result<Digest, HashError> digestFromHex(Algorithm algorithm, String hex) {
    return algorithm == null || hex == null
      ? Result.err(Wrap.nullArgument("an algorithm and hex"))
      : Hashes.fromHex(algorithm, hex).map(b -> new Digest(algorithm, b));
  }

  /**
   * The digest of {@code data}.
   *
   * @param algorithm the algorithm
   * @param data the bytes
   * @return the digest, or {@code TypeMismatch} for a null argument
   */
  public static Result<Digest, HashError> digest(Algorithm algorithm, byte[] data) {
    return algorithm == null || data == null
      ? Result.err(Wrap.nullArgument("an algorithm and bytes"))
      : Result.ok(new Digest(algorithm, Hashes.digest(algorithm, data)));
  }

  /** A versioned domain-separation tag, e.g. {@code dilf4s/mpt/leaf/v1}: valid by construction ({@link #domainTag}). */
  public static final class DomainTag {
    private final String value;

    private DomainTag(String value) {
      this.value = value;
    }

    /**
     * The tag.
     *
     * @return the tag's text
     */
    public String value() {
      return value;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof DomainTag t && t.value.equals(value);
    }

    @Override
    public int hashCode() {
      return value.hashCode();
    }

    @Override
    public String toString() {
      return value;
    }
  }

  /**
   * {@code tag}, if it matches {@code ^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*\/v[0-9]+$}; else {@code InvalidDomainTag}. The
   * reference checks it with a regex that recurses once per segment: cold, a tag of about 1,245 segments (about 2,500
   * characters) overflows a 1 MB stack, so bound an untrusted tag's length first (the README's "Thread stacks").
   *
   * @param tag the tag
   * @return the tag, or the refusal
   */
  public static Result<DomainTag, HashError> domainTag(String tag) {
    return tag == null ? Result.err(Wrap.nullArgument("a tag")) : Hashes.domainTag(tag).map(DomainTag::new);
  }

  /**
   * {@code H(H(tag) || H(tag) || message)}.
   *
   * @param algorithm the algorithm
   * @param tag the tag
   * @param message the message
   * @return the digest, or {@code TypeMismatch} for a null argument
   */
  public static Result<Digest, HashError> taggedHash(Algorithm algorithm, DomainTag tag, byte[] message) {
    return algorithm == null || tag == null || message == null
      ? Result.err(Wrap.nullArgument("an algorithm, a tag and a message"))
      : Hashes.taggedHash(algorithm, tag.value, message).map(b -> new Digest(algorithm, b));
  }

  /**
   * The content hash of a value: the tagged hash of its canonical encoding, streamed and never built whole, within
   * {@link Cbor#MAX_ENCODED_BYTES}.
   *
   * @param algorithm the algorithm
   * @param tag the tag
   * @param value the value
   * @return the digest, or {@code TooLarge}
   */
  public static Result<Digest, EncodeError> contentHash(Algorithm algorithm, DomainTag tag, Value value) {
    return contentHashWithin(algorithm, tag, value, Cbor.MAX_ENCODED_BYTES);
  }

  /**
   * {@link #contentHash}, if the encoding is at most {@code maxBytes} long: a heavier value is refused from its weight,
   * and a refused value is never hashed, not even in part.
   *
   * @param algorithm the algorithm
   * @param tag the tag
   * @param value the value
   * @param maxBytes the limit
   * @return the digest, or {@code TooLarge}
   */
  public static Result<Digest, EncodeError> contentHashWithin(Algorithm algorithm, DomainTag tag, Value value, long maxBytes) {
    return algorithm == null || tag == null || value == null
      ? Result.err(Wrap.nullArgument("an algorithm, a tag and a value"))
      : Hashes.taggedValue(algorithm, tag.value, Wrap.scala(value), maxBytes).map(b -> new Digest(algorithm, b));
  }

  /**
   * The most bytes {@code algorithm} expands to: {@code min(255 * outputSize, 65535)}.
   *
   * @param algorithm the algorithm
   * @return the limit, or {@code TypeMismatch} for a null algorithm
   */
  public static Result<Long, HashError> expansionLimit(Algorithm algorithm) {
    return algorithm == null ? Result.err(Wrap.nullArgument("an algorithm")) : Result.ok(Hashes.expansionLimit(algorithm));
  }

  /**
   * {@code expand_message_xmd}: {@code length} uniform bytes from {@code message} under {@code dst}, refused
   * ({@code InvalidExpansion}) for an empty DST and for a length outside {@code 1..expansionLimit}, in that order.
   *
   * @param algorithm the algorithm
   * @param message the message
   * @param dst the domain separation tag
   * @param length the bytes asked for
   * @return the bytes, or the refusal
   */
  public static Result<byte[], HashError> expandMessageXmd(Algorithm algorithm, byte[] message, byte[] dst, long length) {
    return algorithm == null || message == null || dst == null
      ? Result.err(Wrap.nullArgument("an algorithm, a message and a DST"))
      : Hashes.expand(algorithm, message, dst, length);
  }
}
