/**
 * The canonical data model (docs/specs/canonical-encoding.md section 1).
 *
 * Every kind is a frozen class with a private constructor, so a value can only come from these factories, and every
 * invariant holds: rationals are reduced and never integers, text has no unpaired surrogates, maps have unique keys in
 * canonical order and no null values, and nothing nests deeper than {@link MAX_DEPTH}. Every constructible value
 * therefore has exactly one encoding, and decodes back to itself.
 */
import type { CanonicalError } from "./error.js";
import { err, ok, type Result } from "./result.js";
import { compareBytes, encodeUtf8, isWellFormed } from "./utf8.js";

/** The nesting limit: a scalar has depth 0, an array or map one more than its deepest element (1 when empty). */
export const MAX_DEPTH = 256;

export type Value = NullValue | BoolValue | IntValue | RationalValue | TextValue | BytesValue | ArrayValue | MapValue;

const depthExceeded: CanonicalError = { kind: "DepthExceeded", limit: MAX_DEPTH };

export class NullValue {
  readonly kind = "null";
  readonly depth = 0;
  static readonly instance: NullValue = new NullValue();
  private constructor() {
    Object.freeze(this);
  }
}

export class BoolValue {
  readonly kind = "bool";
  readonly depth = 0;
  static readonly true: BoolValue = new BoolValue(true);
  static readonly false: BoolValue = new BoolValue(false);
  private constructor(readonly value: boolean) {
    Object.freeze(this);
  }
  static of(value: boolean): BoolValue {
    return value ? BoolValue.true : BoolValue.false;
  }
}

export class IntValue {
  readonly kind = "int";
  readonly depth = 0;
  private constructor(readonly value: bigint) {
    Object.freeze(this);
  }
  static of(value: bigint): IntValue {
    return new IntValue(value);
  }
}

const abs = (n: bigint): bigint => (n < 0n ? -n : n);

function gcd(a: bigint, b: bigint): bigint {
  let x = abs(a);
  let y = abs(b);
  while (y !== 0n) [x, y] = [y, x % y];
  return x;
}

/** A rational `n/d` with `d >= 2` and `gcd(|n|, d) = 1`: never an integer. */
export class RationalValue {
  readonly kind = "rational";
  readonly depth = 0;
  private constructor(
    readonly numerator: bigint,
    readonly denominator: bigint,
  ) {
    Object.freeze(this);
  }

  /** `n/d` exactly as given, if it already satisfies the invariants (as a decoder must check). */
  static of(numerator: bigint, denominator: bigint): RationalValue | undefined {
    return denominator >= 2n && gcd(numerator, denominator) === 1n ? new RationalValue(numerator, denominator) : undefined;
  }

  /** The number `n/d` in the single numeric domain: an integer when it is one; `undefined` when `d` is zero. */
  static number(numerator: bigint, denominator: bigint): IntValue | RationalValue | undefined {
    if (denominator === 0n) return undefined;
    const g = gcd(numerator, denominator);
    const sign = denominator < 0n ? -1n : 1n;
    const n = (sign * numerator) / g;
    const d = (sign * denominator) / g;
    return d === 1n ? IntValue.of(n) : new RationalValue(n, d);
  }
}

export class TextValue {
  readonly kind = "text";
  readonly depth = 0;
  /** The UTF-8 encoding. */
  readonly #utf8: Uint8Array;
  private constructor(readonly value: string) {
    this.#utf8 = encodeUtf8(value);
    Object.freeze(this);
  }

  /** Text, if it has no unpaired surrogates. */
  static of(value: string): Result<TextValue, CanonicalError> {
    return isWellFormed(value) ? ok(new TextValue(value)) : err({ kind: "UnpairedSurrogate" });
  }

  /** A copy of the UTF-8 encoding. */
  utf8(): Uint8Array {
    return this.#utf8.slice();
  }
}

export class BytesValue {
  readonly kind = "bytes";
  readonly depth = 0;
  readonly #bytes: Uint8Array;
  private constructor(bytes: Uint8Array) {
    this.#bytes = bytes.slice();
    Object.freeze(this);
  }

  /** A byte string holding a copy of `bytes`. */
  static of(bytes: Uint8Array): BytesValue {
    return new BytesValue(bytes);
  }

  /** A copy of the bytes. */
  get bytes(): Uint8Array {
    return this.#bytes.slice();
  }

  get length(): number {
    return this.#bytes.length;
  }
}

const deepest = (values: Iterable<Value>): number => {
  let d = 0;
  for (const v of values) d = Math.max(d, v.depth);
  return d;
};

export class ArrayValue {
  readonly kind = "array";
  readonly items: readonly Value[];
  private constructor(
    items: readonly Value[],
    readonly depth: number,
  ) {
    this.items = Object.freeze([...items]);
    Object.freeze(this);
  }

  /** The array of `items`, if within the depth limit. */
  static of(items: readonly Value[]): Result<ArrayValue, CanonicalError> {
    const depth = deepest(items) + 1;
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(items, depth));
  }
}

/** A map entry: its key, the key's UTF-8, and its value. */
interface Entry {
  readonly key: string;
  readonly utf8: Uint8Array;
  readonly value: Value;
}

/**
 * The order of keys in an encoded map: their encodings compared bytewise (RFC 8949 section 4.2.1). For text keys that
 * is shorter UTF-8 first, then bytewise, because the head encodes the length in increasing order.
 */
export function canonicalKeyOrder(a: Uint8Array, b: Uint8Array): number {
  return a.length !== b.length ? a.length - b.length : compareBytes(a, b);
}

export class MapValue {
  readonly kind = "map";
  readonly #entries: readonly Entry[];
  readonly #index: ReadonlyMap<string, Value>;
  private constructor(
    entries: readonly Entry[],
    readonly depth: number,
  ) {
    this.#entries = entries;
    this.#index = new Map(entries.map((e) => [e.key, e.value]));
    Object.freeze(this);
  }

  /**
   * The map of `entries`, checked in order: a repeated key (even with a null value) is `DuplicateKey`, a key with an
   * unpaired surrogate is `UnpairedSurrogate`; null values are dropped; excess depth is `DepthExceeded`.
   */
  static of(entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> {
    const seen = new Set<string>();
    const kept: Entry[] = [];
    for (const [key, value] of entries) {
      if (seen.has(key)) return err({ kind: "DuplicateKey", key });
      if (!isWellFormed(key)) return err({ kind: "UnpairedSurrogate" });
      seen.add(key);
      if (value.kind !== "null") kept.push({ key, utf8: encodeUtf8(key), value });
    }
    kept.sort((a, b) => canonicalKeyOrder(a.utf8, b.utf8));
    const depth = deepest(kept.map((e) => e.value)) + 1;
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new MapValue(Object.freeze(kept), depth));
  }

  /** The value at `key`, if present. */
  get(key: string): Value | undefined {
    return this.#index.get(key);
  }

  /** The entries in canonical key order. */
  entries(): readonly (readonly [string, Value])[] {
    return this.#entries.map((e) => [e.key, e.value] as const);
  }

  /** The keys in canonical key order. */
  keys(): readonly string[] {
    return this.#entries.map((e) => e.key);
  }

  /** Each key's UTF-8 and value, in canonical key order. */
  encodedEntries(): readonly (readonly [Uint8Array, Value])[] {
    return this.#entries.map((e) => [e.utf8.slice(), e.value] as const);
  }

  get size(): number {
    return this.#entries.length;
  }
}

/** Factories for every kind of value. */
export const Value = {
  null: NullValue.instance,
  bool: (b: boolean): BoolValue => BoolValue.of(b),
  int: (n: bigint): IntValue => IntValue.of(n),
  number: (n: bigint, d: bigint): IntValue | RationalValue | undefined => RationalValue.number(n, d),
  text: (s: string): Result<TextValue, CanonicalError> => TextValue.of(s),
  bytes: (b: Uint8Array): BytesValue => BytesValue.of(b),
  array: (items: readonly Value[]): Result<ArrayValue, CanonicalError> => ArrayValue.of(items),
  map: (entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> => MapValue.of(entries),
} as const;
