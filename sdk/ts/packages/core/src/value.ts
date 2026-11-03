/**
 * The canonical data model (docs/specs/canonical-encoding.md section 1).
 *
 * Every kind is a frozen class with a private constructor and `#private` state, which makes it nominal: a value can
 * only come from these factories (an object literal of the same shape does not typecheck), and every invariant holds: rationals are reduced and never integers, text has no unpaired surrogates, maps have unique keys in
 * canonical order and no null values, and nothing nests deeper than {@link MAX_DEPTH}. Every constructible value
 * therefore has exactly one encoding, and decodes back to itself.
 *
 * # Sharing
 *
 * Values are immutable and share their parts by reference, so a value may occur many times inside another without
 * being copied: `[x, x]` nested `k` deep holds `2^k` leaves in `k` arrays. Each value's `weight` (its logical, unshared
 * size, see {@link MAX_WEIGHT}) and `depth` are computed once, at construction, from its parts' cached ones, so reading
 * either is O(1). So:
 * - anything that expands a value (`cbor.encode`, hashing, commitments, decoding into typed models) costs O(weight),
 *   not O(memory): bound the weight of a value not known to be small first, or use the budgeted `cbor.encodeWithin`,
 *   `cbor.encodedLengthWithin`, `json.renderWithin` and `valueCommitmentWithin`;
 * - {@link equals} never expands: it records the pairs of shared parts it has proven equal and skips them when they
 *   recur;
 * - {@link inspect} (and `toString` and Node's `util.inspect`) shows at most {@link DEBUG_BUDGET} units of weight and
 *   {@link DEBUG_DEPTH} levels, and elides the rest as `…`.
 *
 * No walk of a value recurses on the native stack: each keeps an explicit stack of at most {@link MAX_DEPTH} open
 * containers. Values decoded from CBOR or parsed from JSON are trees, so their weight is bounded by their input.
 */
import { bitLength } from "./bigint.js";
import type { CanonicalError } from "./error.js";
import { CANONICAL_MAX_DEPTH, CANONICAL_MAX_WEIGHT } from "./generated/limits.js";
import { toHex } from "./hex.js";
import { KeySet, search } from "./keyset.js";
import { err, ok, type Result } from "./result.js";
import { compareBytes, encodeUtf8, isWellFormed } from "./utf8.js";

/** The nesting limit: a scalar has depth 0, an array or map one more than its deepest element (1 when empty). */
export const MAX_DEPTH = CANONICAL_MAX_DEPTH;

/**
 * The greatest weight; weights saturate here (the Scala reference's `Long.MaxValue`), never wrap. A value's weight, as
 * Scala's `CanonicalValue.weight`, is one per node, plus the UTF-8 length of each text and map key, the length of each
 * byte string, and the whole 64-bit limbs of each number (Java `bitLength / 64`, numerator and denominator). A shared
 * part counts every time it occurs, so the weight bounds the work of any full traversal, and every canonical encoding
 * is at least as long as its value's weight.
 */
export const MAX_WEIGHT: bigint = CANONICAL_MAX_WEIGHT;

/** `w`, saturated at {@link MAX_WEIGHT}: every term is non-negative, so clamping the exact sum once is saturating each step. */
const saturated = (w: bigint): bigint => (w > MAX_WEIGHT ? MAX_WEIGHT : w);

/** The whole 64-bit limbs of a number of `bits` (Java) bits: its share of the weight. */
const limbs = (bits: number): bigint => BigInt(Math.floor(bits / 64));

export type Value = NullValue | BoolValue | IntValue | RationalValue | TextValue | BytesValue | ArrayValue | MapValue;

/**
 * A fresh plain `Uint8Array` holding `bytes`. `slice` would not do: on a Node `Buffer` it returns a view sharing memory.
 */
export function copyBytes(bytes: Uint8Array): Uint8Array {
  return Uint8Array.from(bytes);
}

const depthExceeded: CanonicalError = { kind: "DepthExceeded", limit: MAX_DEPTH };

/** Node's `util.inspect` hook, so a value prints through {@link inspect}, within its budget. */
const INSPECT: unique symbol = Symbol.for("nodejs.util.inspect.custom");

// Read-only views of private state for this package's own walks (encoding, rendering, equality), which must not copy
// before they have checked their budget. Set once, by each class's static block; never exported from the package.
let utf8Of!: (t: TextValue) => Uint8Array;
let bytesOf!: (b: BytesValue) => Uint8Array;
let entriesOf!: (m: MapValue) => readonly Entry[];

/** The stored UTF-8 of `t`, uncopied: internal to this package, and never written. */
export const utf8View = (t: TextValue): Uint8Array => utf8Of(t);
/** The stored bytes of `b`, uncopied: internal to this package, and never written. */
export const bytesView = (b: BytesValue): Uint8Array => bytesOf(b);
/** The stored entries of `m` in canonical key order, uncopied: internal to this package. */
export const entriesView = (m: MapValue): readonly Entry[] => entriesOf(m);

export class NullValue {
  readonly kind = "null";
  readonly weight: bigint = 1n;
  readonly #depth = 0;
  static readonly instance: NullValue = new NullValue();
  private constructor() {
    Object.freeze(this);
  }
  get depth(): 0 {
    return this.#depth;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

export class BoolValue {
  readonly kind = "bool";
  readonly depth = 0;
  readonly weight: bigint = 1n;
  readonly #value: boolean;
  static readonly true: BoolValue = new BoolValue(true);
  static readonly false: BoolValue = new BoolValue(false);
  private constructor(value: boolean) {
    this.#value = value;
    Object.freeze(this);
  }
  get value(): boolean {
    return this.#value;
  }
  static of(value: boolean): BoolValue {
    return value ? BoolValue.true : BoolValue.false;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

export class IntValue {
  readonly kind = "int";
  readonly depth = 0;
  /** `1 + bitLength / 64`. */
  readonly weight: bigint;
  readonly #value: bigint;
  readonly #bitLength: number;
  private constructor(value: bigint) {
    this.#value = value;
    this.#bitLength = bitLength(value);
    this.weight = saturated(1n + limbs(this.#bitLength));
    Object.freeze(this);
  }
  get value(): bigint {
    return this.#value;
  }
  /** Java's `BigInteger.bitLength`, which is also the bit length of the CBOR magnitude (`n`, or `-1 - n`). O(1). */
  get bitLength(): number {
    return this.#bitLength;
  }
  static of(value: bigint): IntValue {
    return new IntValue(value);
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
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
  /** `2 + bitLength(n) / 64 + bitLength(d) / 64`. */
  readonly weight: bigint;
  readonly #numerator: bigint;
  readonly #denominator: bigint;
  readonly #numeratorBits: number;
  readonly #denominatorBits: number;
  private constructor(numerator: bigint, denominator: bigint) {
    this.#numerator = numerator;
    this.#denominator = denominator;
    this.#numeratorBits = bitLength(numerator);
    this.#denominatorBits = bitLength(denominator);
    this.weight = saturated(2n + limbs(this.#numeratorBits) + limbs(this.#denominatorBits));
    Object.freeze(this);
  }

  /** The numerator, carrying the sign. */
  get numerator(): bigint {
    return this.#numerator;
  }

  /** The denominator, at least 2. */
  get denominator(): bigint {
    return this.#denominator;
  }

  /** The numerator's Java `bitLength`. O(1). */
  get numeratorBitLength(): number {
    return this.#numeratorBits;
  }

  /** The denominator's Java `bitLength`. O(1). */
  get denominatorBitLength(): number {
    return this.#denominatorBits;
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

  /**
   * `unscaled / 10^scale` in lowest terms. `10^scale`'s only primes are 2 and 5, so the common factor is
   * `2^min(v2, scale) 5^min(v5, scale)`: the twos go in one shift, and the fives by trial division by `5^(2^i)` from the
   * largest `2^i <= scale` down, which builds `min(v5, scale)` bit by bit (about 14 divisions for a scale of 10000).
   * The same result as {@link number} against `10^scale`, without its general gcd.
   */
  static decimal(unscaled: bigint, scale: number): IntValue | RationalValue {
    if (unscaled === 0n) return IntValue.of(0n);
    const sign = unscaled < 0n ? -1n : 1n;
    const magnitude = sign * unscaled;
    const twos = Math.min(bitLength(magnitude & -magnitude) - 1, scale);
    const powers: (readonly [number, bigint])[] = [];
    for (let e = 1, p = 5n; e <= scale; e *= 2, p *= p) powers.push([e, p]);
    const [rest, fives] = powers.reduceRight<readonly [bigint, number]>(
      ([n, count], [e, p]) => (count + e <= scale && n % p === 0n ? [n / p, count + e] : [n, count]),
      [magnitude >> BigInt(twos), 0],
    );
    const d = 5n ** BigInt(scale - fives) << BigInt(scale - twos);
    return d === 1n ? IntValue.of(sign * rest) : new RationalValue(sign * rest, d);
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

export class TextValue {
  readonly kind = "text";
  readonly depth = 0;
  /** `1 +` the UTF-8 length. */
  readonly weight: bigint;
  /** The UTF-8 encoding. */
  readonly #utf8: Uint8Array;
  private constructor(readonly value: string) {
    this.#utf8 = encodeUtf8(value);
    this.weight = 1n + BigInt(this.#utf8.length);
    Object.freeze(this);
  }

  static {
    utf8Of = (t) => t.#utf8;
  }

  /** Text, if it has no unpaired surrogates. */
  static of(value: string): Result<TextValue, CanonicalError> {
    return isWellFormed(value) ? ok(new TextValue(value)) : err({ kind: "UnpairedSurrogate" });
  }

  /** A copy of the UTF-8 encoding. */
  utf8(): Uint8Array {
    return copyBytes(this.#utf8);
  }

  /** The length of the UTF-8 encoding. O(1). */
  get utf8Length(): number {
    return this.#utf8.length;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

export class BytesValue {
  readonly kind = "bytes";
  readonly depth = 0;
  /** `1 +` the length. */
  readonly weight: bigint;
  readonly #bytes: Uint8Array;
  private constructor(bytes: Uint8Array) {
    this.#bytes = copyBytes(bytes);
    this.weight = 1n + BigInt(this.#bytes.length);
    Object.freeze(this);
  }

  static {
    bytesOf = (b) => b.#bytes;
  }

  /** A byte string holding a copy of `bytes`. */
  static of(bytes: Uint8Array): BytesValue {
    return new BytesValue(bytes);
  }

  /** A copy of the bytes. */
  get bytes(): Uint8Array {
    return copyBytes(this.#bytes);
  }

  get length(): number {
    return this.#bytes.length;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

/** The depth (one more than its deepest part's) and weight (one plus each part's) of a container of `parts`. */
function measure<T>(parts: readonly T[], part: (p: T) => readonly [number, bigint]): readonly [number, bigint] {
  const [deepest, weight] = parts.reduce<readonly [number, bigint]>(
    ([d, w], p) => {
      const [pd, pw] = part(p);
      return [Math.max(d, pd), w + pw];
    },
    [0, 1n],
  );
  return [deepest + 1, saturated(weight)];
}

export class ArrayValue {
  readonly kind = "array";
  readonly #items: readonly Value[];
  private constructor(
    items: readonly Value[],
    readonly depth: number,
    /** One plus each element's weight. */
    readonly weight: bigint,
  ) {
    this.#items = items;
    Object.freeze(this);
  }

  /** The elements, in order (a frozen array). */
  get items(): readonly Value[] {
    return this.#items;
  }

  /** The number of elements. */
  get length(): number {
    return this.#items.length;
  }

  /** The array of `items`, if within the depth limit. */
  static of(items: readonly Value[]): Result<ArrayValue, CanonicalError> {
    const kept = Object.freeze([...items]);
    const [depth, weight] = measure(kept, (v) => [v.depth, v.weight]);
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(kept, depth, weight));
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

/** A map entry: its key, the key's UTF-8, and its value. */
export interface Entry {
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

/** A map: its entries in canonical key order, found by binary search. Keys are compared, never hashed (see keyset.ts). */
export class MapValue {
  readonly kind = "map";
  readonly #entries: readonly Entry[];
  private constructor(
    entries: readonly Entry[],
    readonly depth: number,
    /** One plus each entry's `1 + key UTF-8 length + value weight`. */
    readonly weight: bigint,
  ) {
    this.#entries = entries;
    Object.freeze(this);
  }

  static {
    entriesOf = (m) => m.#entries;
  }

  /**
   * The map of `entries`, checked in order: a repeated key (even with a null value) is `DuplicateKey`, a key with an
   * unpaired surrogate is `UnpairedSurrogate`; null values are dropped; excess depth is `DepthExceeded`.
   */
  static of(entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> {
    const seen = new KeySet();
    const kept: Entry[] = [];
    for (const [key, value] of entries) {
      if (!seen.add(key)) return err({ kind: "DuplicateKey", key });
      if (!isWellFormed(key)) return err({ kind: "UnpairedSurrogate" });
      if (value.kind !== "null") kept.push({ key, utf8: encodeUtf8(key), value });
    }
    kept.sort((a, b) => canonicalKeyOrder(a.utf8, b.utf8));
    const [depth, weight] = measure(kept, (e) => [e.value.depth, 1n + BigInt(e.utf8.length) + e.value.weight]);
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new MapValue(Object.freeze(kept), depth, weight));
  }

  /**
   * The value at `key`, if present: O(|key| + log n). A key with an unpaired surrogate encodes to bytes no well-formed
   * key has, so it is never found.
   */
  get(key: string): Value | undefined {
    return this.#entries[search(this.#entries, (e) => e.utf8, encodeUtf8(key), canonicalKeyOrder)]?.value;
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
    return this.#entries.map((e) => [copyBytes(e.utf8), e.value] as const);
  }

  get size(): number {
    return this.#entries.length;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

// ------------------------------------------------------------------------------------------------ equality

// Equality is on content (the canonical encoding is the identity) and costs O(the distinct pairs of shared parts
// compared), never O(weight): a comparison keeps the pairs of containers, and of large texts, byte strings and numbers,
// it has proven equal, and skips them when they recur. Identity is only ever a fast path: the same object is the same
// value, and a pair is recorded only once its contents compared equal. Small scalars are compared outright.

/** The weight from which a text or byte string is memoised (one unit per byte). */
const LARGE_STRING = 64n;
/** The weight from which a number is memoised (one unit per 64-bit limb). */
const LARGE_NUMBER = 8n;

/** The pairs of parts one comparison has proven equal, by identity. */
class Proven {
  readonly #pairs = new Map<object, Set<object>>();

  has(a: object, b: object): boolean {
    return this.#pairs.get(a)?.has(b) === true;
  }

  add(a: object, b: object): void {
    const with_ = this.#pairs.get(a);
    if (with_ === undefined) this.#pairs.set(a, new Set([b]));
    else with_.add(b);
  }
}

/** Two containers whose parts are being compared, in order, and the next index. */
type Comparing =
  | { readonly map: false; readonly a: ArrayValue; readonly b: ArrayValue; at: number }
  | { readonly map: true; readonly a: MapValue; readonly b: MapValue; readonly x: readonly Entry[]; readonly y: readonly Entry[]; at: number };

/** Two scalars of one kind: a large pair already proven equal, or compared now (and a large pair recorded when equal). */
function scalar(proven: Proven, a: object, b: object, large: boolean, same: () => boolean): boolean {
  if (large && proven.has(a, b)) return true;
  const equal = same();
  if (equal && large) proven.add(a, b);
  return equal;
}

/** False when `a` and `b` differ; true when they are equal scalars, a pair already proven, or containers now open. */
function compare(proven: Proven, stack: Comparing[], a: Value, b: Value): boolean {
  if (a === b) return true;
  if (a.weight !== b.weight || a.depth !== b.depth) return false;
  switch (a.kind) {
    case "null":
      return b.kind === "null";
    case "bool":
      return b.kind === "bool" && a.value === b.value;
    case "int":
      return b.kind === "int" && scalar(proven, a, b, a.weight >= LARGE_NUMBER, () => a.value === b.value);
    case "rational":
      return b.kind === "rational" && scalar(proven, a, b, a.weight >= LARGE_NUMBER, () => a.numerator === b.numerator && a.denominator === b.denominator);
    case "text":
      return b.kind === "text" && scalar(proven, a, b, a.weight >= LARGE_STRING, () => a.value === b.value);
    case "bytes":
      return b.kind === "bytes" && scalar(proven, a, b, a.weight >= LARGE_STRING, () => compareBytes(bytesOf(a), bytesOf(b)) === 0);
    case "array":
      if (b.kind !== "array" || a.length !== b.length) return false;
      if (!proven.has(a, b)) stack.push({ map: false, a, b, at: 0 });
      return true;
    case "map":
      if (b.kind !== "map" || a.size !== b.size) return false;
      if (!proven.has(a, b)) stack.push({ map: true, a, b, x: entriesOf(a), y: entriesOf(b), at: 0 });
      return true;
  }
}

/**
 * Whether two values are equal (have the same canonical encoding). Iterative, with an explicit stack of at most
 * {@link MAX_DEPTH} open containers; O(1) when the weights or depths differ; and never expands a shared part twice.
 */
export function equals(a: Value, b: Value): boolean {
  const proven = new Proven();
  const stack: Comparing[] = [];
  if (!compare(proven, stack, a, b)) return false;
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.at++;
    // the lengths are equal, so both sides end together
    if (top.map) {
      const [x, y] = [top.x[i], top.y[i]];
      if (x === undefined || y === undefined) {
        stack.pop();
        proven.add(top.a, top.b);
      } else if (x.key !== y.key || !compare(proven, stack, x.value, y.value)) return false;
    } else {
      const [x, y] = [top.a.items[i], top.b.items[i]];
      if (x === undefined || y === undefined) {
        stack.pop();
        proven.add(top.a, top.b);
      } else if (!compare(proven, stack, x, y)) return false;
    }
  }
  return true;
}

// ------------------------------------------------------------------------------------------------ inspection

/** How much of a value {@link inspect} shows, in units of weight; the rest is elided as `…`. */
export const DEBUG_BUDGET = 1 << 16;

/** How many arrays and maps deep {@link inspect} shows; deeper ones are elided as `…`. */
export const DEBUG_DEPTH = 16;

/** Numbers up to this many bits are shown in decimal, wider ones by their bit length: decimal conversion is superlinear. */
const SHOWN_BITS = 256;

const ELIDED = "…";

function scalarText(v: NullValue | BoolValue | IntValue | RationalValue | TextValue | BytesValue): string {
  switch (v.kind) {
    case "null":
      return "null";
    case "bool":
      return v.value ? "true" : "false";
    case "int":
      return v.bitLength <= SHOWN_BITS ? v.value.toString() : `<int of ${v.bitLength.toString()} bits>`;
    case "rational":
      return v.numeratorBitLength <= SHOWN_BITS && v.denominatorBitLength <= SHOWN_BITS
        ? `${v.numerator.toString()}/${v.denominator.toString()}`
        : `<rational of ${v.numeratorBitLength.toString()}/${v.denominatorBitLength.toString()} bits>`;
    case "text":
      return JSON.stringify(v.value);
    case "bytes":
      return `h'${toHex(bytesOf(v))}'`;
  }
}

type Showing = { readonly map: false; readonly items: readonly Value[]; at: number } | { readonly map: true; readonly entries: readonly Entry[]; at: number };

/**
 * A readable form of `value` (CBOR diagnostic style: `[1, "a", h'00ff', 1/3, {"k": null}]`), bounded however often it
 * repeats a shared part: scalars spend their weight and containers and map keys spend theirs from a budget of
 * {@link DEBUG_BUDGET}, nothing deeper than {@link DEBUG_DEPTH} is shown, and what is not shown is `…`. Iterative.
 */
export function inspect(value: Value): string {
  const out: string[] = [];
  const stack: Showing[] = [];
  let units = BigInt(DEBUG_BUDGET);
  const spend = (w: bigint): boolean => {
    const affordable = w <= units;
    units = affordable ? units - w : 0n;
    return affordable;
  };
  const show = (v: Value): void => {
    if (v.kind !== "array" && v.kind !== "map") {
      out.push(spend(v.weight) ? scalarText(v) : ELIDED);
    } else if (!spend(1n) || stack.length >= DEBUG_DEPTH) {
      // an elided container still costs a unit, so the output stays within the budget
      out.push(ELIDED);
    } else {
      out.push(v.kind === "array" ? "[" : "{");
      stack.push(v.kind === "array" ? { map: false, items: v.items, at: 0 } : { map: true, entries: entriesOf(v), at: 0 });
    }
  };
  show(value);
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const length = top.map ? top.entries.length : top.items.length;
    const i = top.at++;
    if (i >= length) {
      out.push(top.map ? "}" : "]");
      stack.pop();
      continue;
    }
    if (i > 0) out.push(", ");
    const entry = top.map ? top.entries[i] : undefined;
    const item = top.map ? entry?.value : top.items[i];
    if (item === undefined || units === 0n || (entry !== undefined && !spend(1n + BigInt(entry.utf8.length)))) {
      out.push(ELIDED);
      top.at = length;
    } else {
      if (entry !== undefined) out.push(`${JSON.stringify(entry.key)}: `);
      show(item);
    }
  }
  return out.join("");
}

/** Factories for every kind of value, and equality. */
export const Value = {
  null: NullValue.instance,
  bool: (b: boolean): BoolValue => BoolValue.of(b),
  int: (n: bigint): IntValue => IntValue.of(n),
  number: (n: bigint, d: bigint): IntValue | RationalValue | undefined => RationalValue.number(n, d),
  text: (s: string): Result<TextValue, CanonicalError> => TextValue.of(s),
  bytes: (b: Uint8Array): BytesValue => BytesValue.of(b),
  array: (items: readonly Value[]): Result<ArrayValue, CanonicalError> => ArrayValue.of(items),
  map: (entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> => MapValue.of(entries),
  equals: (a: Value, b: Value): boolean => equals(a, b),
} as const;
