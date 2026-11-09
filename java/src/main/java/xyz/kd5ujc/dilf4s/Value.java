package xyz.kd5ujc.dilf4s;

import java.math.BigInteger;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

import xyz.kd5ujc.core.canonical.CanonicalValue;
import xyz.kd5ujc.dilf4s.error.CanonicalError;
import xyz.kd5ujc.dilf4s.internal.Values;

/**
 * A value of the canonical data model ({@code docs/specs/canonical-encoding.md} section 1): null, a boolean, an
 * integer, a rational that is not an integer, text, a byte string, an array, or a map with text keys. Every value is
 * valid by construction: the factories refuse what the model does not hold (an integer wider than {@link
 * #MAX_INTEGER_BITS}, text with an unpaired surrogate, a repeated key, nesting past {@link #MAX_DEPTH}), and a map
 * holds no null values (null and absent are the same value).
 *
 * <p>A value is the reference's own, held: building one, reading a part, or passing it to an encoder never copies or
 * walks it, so a value with shared parts (an array holding one value many times) costs what it occupies, and the
 * weight-based refusals stay O(1). The kinds are final classes, not records, for that reason: a record exposes every
 * field. Switch over them with type patterns. {@code equals} and {@code hashCode} are the reference's value equality and
 * hash, which walk the value as written out: O(its weight), so a heavy value with shared parts is slow to compare or
 * hash (bound its {@link #weight} first); {@code toString} is bounded.
 */
public sealed interface Value permits Value.Null, Value.Bool, Value.Int, Value.Rational, Value.Text, Value.Bytes, Value.Array, Value.Map {

  /** The deepest nesting of arrays and maps. */
  int MAX_DEPTH = Values.MaxDepth();

  /** The widest integer, in bits ({@code BigInteger.bitLength}). */
  int MAX_INTEGER_BITS = Values.MaxIntegerBits();

  /** The widest numerator or denominator of a rational, in bits. */
  int MAX_RATIONAL_BITS = Values.MaxRationalBits();

  /**
   * A portable measure of size: one per node, plus the UTF-8 length of text and keys, the length of byte strings and
   * the 64-bit limbs of numbers. A shared part counts every time it occurs; it saturates at {@code Long.MAX_VALUE}.
   *
   * @return the weight
   */
  long weight();

  /**
   * The nesting depth: 0 for a scalar, one more than the deepest child for an array or a map.
   *
   * @return the depth
   */
  int depth();

  /** The null value. */
  static Value.Null nul() {
    return Null.INSTANCE;
  }

  /**
   * A boolean.
   *
   * @param value the boolean
   * @return the value
   */
  static Value.Bool bool(boolean value) {
    return new Bool(Values.ofBoolean(value));
  }

  /**
   * An integer: every {@code long} is within the bound.
   *
   * @param value the integer
   * @return the value
   */
  static Value.Int integer(long value) {
    return new Int(Values.ofLong(value));
  }

  /**
   * An integer, refused ({@code IntegerTooWide}) when wider than {@link #MAX_INTEGER_BITS}.
   *
   * @param value the integer
   * @return the value, or the refusal
   */
  static Result<Value.Int, CanonicalError> integer(BigInteger value) {
    return value == null ? Result.err(Wrap.nullArgument("the integer")) : Values.ofBigInteger(value).map(Int::new);
  }

  /**
   * {@code numerator / denominator} in the single numeric domain: an {@link Int} when it is integral, else a {@link
   * Rational} in lowest terms. A zero denominator is {@code TypeMismatch}; a part wider than {@link #MAX_RATIONAL_BITS}
   * is {@code RationalTooWide}.
   *
   * @param numerator the numerator
   * @param denominator the denominator
   * @return the value, or the refusal
   */
  static Result<Value, CanonicalError> number(BigInteger numerator, BigInteger denominator) {
    return numerator == null || denominator == null
      ? Result.err(Wrap.nullArgument("a numerator and a denominator"))
      : Values.number(numerator, denominator).map(Wrap::of);
  }

  /**
   * Text, refused ({@code UnpairedSurrogate}) when it holds an unpaired UTF-16 surrogate.
   *
   * @param value the text
   * @return the value, or the refusal
   */
  static Result<Value.Text, CanonicalError> text(String value) {
    return value == null ? Result.err(Wrap.nullArgument("the text")) : Values.text(value).map(Text::new);
  }

  /**
   * A byte string, holding a copy of {@code value}.
   *
   * @param value the bytes
   * @return the value, or {@code TypeMismatch} for a null array
   */
  static Result<Value.Bytes, CanonicalError> bytes(byte[] value) {
    return value == null ? Result.err(Wrap.nullArgument("the bytes")) : Result.ok(new Bytes(Values.bytes(value)));
  }

  /**
   * An array of {@code items}, in order; refused ({@code DepthExceeded}) when it would nest past {@link #MAX_DEPTH}.
   *
   * @param items the items
   * @return the value, or the refusal
   */
  static Result<Value.Array, CanonicalError> array(List<? extends Value> items) {
    return items == null || items.stream().anyMatch(Objects::isNull)
      ? Result.err(Wrap.nullArgument("the items"))
      : Values.array(items.stream().map(Wrap::scala).toList()).map(Array::new);
  }

  /**
   * A map of {@code entries}, read once in the caller's iteration order: a key with an unpaired surrogate is refused, a
   * repeated key (only possible from a caller's own {@code Map}) is {@code DuplicateKey}, an entry whose value is
   * {@link #nul()} is dropped, and nesting past {@link #MAX_DEPTH} is {@code DepthExceeded}. The keys are compared,
   * never hashed.
   *
   * @param entries the entries
   * @return the value, or the refusal
   */
  static Result<Value.Map, CanonicalError> map(java.util.Map<String, ? extends Value> entries) {
    return entries == null || entries.entrySet().stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null)
      ? Result.err(Wrap.nullArgument("the entries"))
      : Values.map(entries.entrySet().stream().map(e -> java.util.Map.entry(e.getKey(), Wrap.scala(e.getValue()))).toList())
        .map(Map::new);
  }

  /** The null value. */
  final class Null implements Value {
    static final Null INSTANCE = new Null();

    private Null() {}

    @Override
    public long weight() {
      return 1L;
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Null;
    }

    @Override
    public int hashCode() {
      return Values.hash(Values.nul());
    }

    @Override
    public String toString() {
      return "null";
    }
  }

  /** A boolean. */
  final class Bool implements Value {
    final CanonicalValue.CBool scala;

    Bool(CanonicalValue.CBool scala) {
      this.scala = scala;
    }

    /**
     * The boolean.
     *
     * @return the boolean
     */
    public boolean value() {
      return Values.boolOf(scala);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Bool b && Values.same(scala, b.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** An integer within {@link #MAX_INTEGER_BITS}. */
  final class Int implements Value {
    final CanonicalValue.CInt scala;

    Int(CanonicalValue.CInt scala) {
      this.scala = scala;
    }

    /**
     * The integer.
     *
     * @return the integer
     */
    public BigInteger value() {
      return Values.intOf(scala);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Int i && Values.same(scala, i.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** A rational that is not an integer, in lowest terms, with a denominator of at least 2. */
  final class Rational implements Value {
    final CanonicalValue.CRational scala;

    Rational(CanonicalValue.CRational scala) {
      this.scala = scala;
    }

    /**
     * The numerator, signed.
     *
     * @return the numerator
     */
    public BigInteger numerator() {
      return Values.numerator(scala);
    }

    /**
     * The denominator, at least 2.
     *
     * @return the denominator
     */
    public BigInteger denominator() {
      return Values.denominator(scala);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Rational r && Values.same(scala, r.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** Text, well-formed UTF-16. */
  final class Text implements Value {
    final CanonicalValue.CText scala;

    Text(CanonicalValue.CText scala) {
      this.scala = scala;
    }

    /**
     * The text.
     *
     * @return the text
     */
    public String value() {
      return Values.textOf(scala);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Text t && Values.same(scala, t.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** A byte string. */
  final class Bytes implements Value {
    final CanonicalValue.CBytes scala;

    Bytes(CanonicalValue.CBytes scala) {
      this.scala = scala;
    }

    /**
     * The bytes, in a fresh array: changing it changes no value.
     *
     * @return a copy of the bytes
     */
    public byte[] bytes() {
      return Values.bytesOf(scala);
    }

    /**
     * The length, in bytes.
     *
     * @return the length
     */
    public long size() {
      return Values.size(scala);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Bytes b && Values.same(scala, b.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** An array. */
  final class Array implements Value {
    final CanonicalValue.CArray scala;

    Array(CanonicalValue.CArray scala) {
      this.scala = scala;
    }

    /**
     * The number of items.
     *
     * @return the size
     */
    public int size() {
      return Values.size(scala);
    }

    /**
     * The item at {@code index}.
     *
     * @param index from 0
     * @return the item, or empty outside the array
     */
    public Optional<Value> get(int index) {
      return Values.item(scala, index).map(Wrap::of);
    }

    /**
     * The items, in order, as an unmodifiable list.
     *
     * @return the items
     */
    public List<Value> items() {
      return Values.items(scala).stream().map(Wrap::of).toList();
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return Values.depth(scala);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Array a && Values.same(scala, a.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }

  /** A map with text keys and no null values, its keys in UTF-16 code unit order ({@code String.compareTo}). */
  final class Map implements Value {
    final CanonicalValue.CMap scala;

    Map(CanonicalValue.CMap scala) {
      this.scala = scala;
    }

    /**
     * The number of entries.
     *
     * @return the size
     */
    public int size() {
      return Values.size(scala);
    }

    /**
     * The value at {@code key}.
     *
     * @param key the key
     * @return the value, or empty when the key is absent (or null)
     */
    public Optional<Value> get(String key) {
      return key == null ? Optional.empty() : Values.get(scala, key).map(Wrap::of);
    }

    /**
     * The keys, in order, as an unmodifiable list.
     *
     * @return the keys
     */
    public List<String> keys() {
      return List.copyOf(Values.keys(scala));
    }

    /**
     * The entries, as an unmodifiable map sorted by {@code String}'s natural order, the map's own.
     *
     * @return the entries
     */
    public SortedMap<String, Value> entries() {
      final TreeMap<String, Value> sorted = Values.entries(scala).stream()
        .collect(Collectors.toMap(e -> e.getKey(), e -> Wrap.of(e.getValue()), (a, b) -> a, () -> new TreeMap<String, Value>()));
      return Collections.unmodifiableSortedMap(sorted);
    }

    /**
     * This map with {@code key} set to {@code value}, or removed when {@code value} is {@link #nul()}, in O(log n).
     *
     * @param key the key
     * @param value its value
     * @return the new map, or the refusal (a key with an unpaired surrogate, nesting past {@link #MAX_DEPTH})
     */
    public Result<Value.Map, CanonicalError> with(String key, Value value) {
      return key == null || value == null
        ? Result.err(Wrap.nullArgument("a key and a value"))
        : Values.updated(scala, key, Wrap.scala(value)).map(Map::new);
    }

    /**
     * This map without {@code key}, in O(log n).
     *
     * @param key the key
     * @return the new map, or the refusal of a key with an unpaired surrogate
     */
    public Result<Value.Map, CanonicalError> without(String key) {
      return key == null ? Result.err(Wrap.nullArgument("the key")) : Values.removed(scala, key).map(Map::new);
    }

    @Override
    public long weight() {
      return Values.weight(scala);
    }

    @Override
    public int depth() {
      return Values.depth(scala);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Map m && Values.same(scala, m.scala);
    }

    @Override
    public int hashCode() {
      return Values.hash(scala);
    }

    @Override
    public String toString() {
      return Values.inspect(scala);
    }
  }
}
