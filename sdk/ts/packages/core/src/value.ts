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
import { bitLengthOf } from "./bigint.js";
import { copyBytes, takeBytes } from "./bytes.js";
import type { CanonicalError, TypeMismatch } from "./error.js";
import { CANONICAL_MAX_DEPTH, CANONICAL_MAX_DIGITS, CANONICAL_MAX_INTEGER_BITS, CANONICAL_MAX_RATIONAL_BITS, CANONICAL_MAX_WEIGHT } from "./generated/limits.js";
import { toHexOf } from "./hex.js";
import { close, isResult, NOT_A_RESULT, NOT_ITERABLE, openWith, walksAsArray } from "./iterator.js";
import { KeySet, search } from "./keyset.js";
import { Preview } from "./preview.js";
import { err, ok, type Result } from "./result.js";
import { compareBytes, encodeUtf8, isWellFormed } from "./utf8.js";

// the typed-array helpers live in bytes.ts, re-exported for the modules that import them from here
export { isBytes, plainBytes, takeBytes } from "./bytes.js";

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
 * The key every constructor takes, private to this module. A constructor is private to TypeScript alone, so JavaScript
 * can still call one (`new IntValue(5, 3)`, `Reflect.construct`), but without this key what it builds is not minted.
 */
const KEY: unique symbol = Symbol("canonical value");

/** Its constructor returns the object it is given, so a subclass's private fields are installed on that object. */
// eslint-disable-next-line @typescript-eslint/no-extraneous-class -- the constructor's return is the point: see Minted
class Identity {
  constructor(o: object) {
    return o;
  }
}

/** `#minted in x`: set once, by {@link Minted}'s static block. */
let minted!: (x: object) => boolean;

/**
 * The one private brand of every kind of value, stamped onto a value as it is built with {@link KEY}: constructing a
 * `Minted` around an object installs `#minted` on that object (a derived class's fields go on what its base constructor
 * returns). So {@link isValue} is one check whatever the kind (a chain of eight per-class brands cost about 64 ns a
 * call where a call site saw many kinds), and the value classes need no common base, whose derived constructors made
 * building values about twice as slow.
 */
class Minted extends Identity {
  readonly #minted = true;
  static {
    minted = (x) => #minted in x;
  }
}

/** Brands `o` as a value: its constructor calls this, before it freezes, only when it was given {@link KEY}. */
const stamp = (o: object): void => {
  new Minted(o);
};

/**
 * Whether `x` is a canonical value made by this module's factories, in O(1): for what a caller may pass past the
 * types, such as a JavaScript caller's argument. It reads the one private `#minted` brand every value carries, stamped
 * when the value is built with the key, never the prototype or a public field and never a side table: an object
 * borrowing a value's prototype is not one, nor is one built by calling a constructor from outside, and a Proxy's traps
 * do not run.
 */
export function isValue(x: unknown): x is Value {
  return typeof x === "object" && x !== null && minted(x);
}

const depthExceeded: CanonicalError = { kind: "DepthExceeded", limit: MAX_DEPTH };

// What a caller passes past the types (from JavaScript, or a cast) is checked in O(1) before it is read, and refused as
// a TypeMismatch naming what was expected, never what was given.
const expected = (what: string): TypeMismatch => ({ kind: "TypeMismatch", reason: `expected ${what}` });
const notABoolean = expected("a boolean");
const notABigint = expected("a bigint");
const notAString = expected("a string");
const notBytes = expected("a Uint8Array");
const notAnArray = expected("an array");
const notEntries = expected("an iterable of [key, value] entries");
const notIterable: TypeMismatch = { kind: "TypeMismatch", reason: NOT_ITERABLE };
const notAResult: TypeMismatch = { kind: "TypeMismatch", reason: NOT_A_RESULT };
const scaleNotInteger: TypeMismatch = { kind: "TypeMismatch", reason: "the scale is not an integer" };

/** Node's `util.inspect` hook, so a value prints through {@link inspect}, within its budget. */
const INSPECT: unique symbol = Symbol.for("nodejs.util.inspect.custom");

// Read-only views of private state for this package's own walks (encoding, rendering, equality), which must not copy
// before they have checked their budget. Set once, by each class's static block; never exported from the package.
let utf8Of!: (t: TextValue) => Uint8Array;
let bytesOf!: (b: BytesValue) => Uint8Array;
let entriesOf!: (m: MapValue) => readonly Entry[];

let bytesOfOwn!: (bytes: Uint8Array) => BytesValue;
let arrayOfOwn!: (items: readonly Value[]) => Result<ArrayValue, CanonicalError>;
let mapOfOwn!: (entries: readonly (readonly [string, Value])[]) => Result<MapValue, CanonicalError>;

/**
 * A byte string holding a copy of `bytes`, a plain array this package holds (the CBOR decoder's input): {@link
 * BytesValue.of} without its type check. Internal to this package.
 */
export const bytesOfValues = (bytes: Uint8Array): BytesValue => bytesOfOwn(bytes);
/**
 * {@link ArrayValue.of} without its type checks, for this package's decoders: their items are values they made.
 * Internal to this package.
 */
export const arrayOfValues = (items: readonly Value[]): Result<ArrayValue, CanonicalError> => arrayOfOwn(items);
/**
 * {@link MapValue.of} without its type checks, for this package's decoders: their keys are strings and their values are
 * values they made. Internal to this package.
 */
export const mapOfValues = (entries: readonly (readonly [string, Value])[]): Result<MapValue, CanonicalError> => mapOfOwn(entries);

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
  static readonly instance: NullValue = new NullValue(KEY);
  private constructor(key: symbol) {
    if (key === KEY) stamp(this);
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
  static readonly true: BoolValue = new BoolValue(KEY, true);
  static readonly false: BoolValue = new BoolValue(KEY, false);
  private constructor(key: symbol, value: boolean) {
    if (key === KEY) stamp(this);
    this.#value = value;
    Object.freeze(this);
  }
  get value(): boolean {
    return this.#value;
  }
  /** The boolean `value`; a value that is not a boolean is a `TypeMismatch`, never read as a condition. */
  static of(value: boolean): Result<BoolValue, TypeMismatch> {
    return typeof (value as unknown) !== "boolean" ? err(notABoolean) : ok(value ? BoolValue.true : BoolValue.false);
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

/**
 * The widest integer, in Java `bitLength` bits (docs/specs/canonical-encoding.md section 1): the widest whose every
 * integer has at most 10,000 digits (`2^33219 < 10^10000`), so every integer renders as JSON; `8e9999` is an integer
 * and `9e9999` is not. Far past every JLVM number (4096 bits), and the width of a rational's parts too.
 * {@link IntValue.of} refuses a wider one, and a decoder refuses a wider bignum from its length, before it builds the
 * integer: no value is built that no decoder accepts, and none is so wide that what reads it (rendering, arithmetic,
 * hashing) is unbounded.
 */
export const MAX_INTEGER_BITS = CANONICAL_MAX_INTEGER_BITS;

/** An integer past {@link MAX_INTEGER_BITS}. */
export type IntegerTooWide = Extract<CanonicalError, { readonly kind: "IntegerTooWide" }>;

const integerTooWide: IntegerTooWide = { kind: "IntegerTooWide", limit: MAX_INTEGER_BITS };

/** An integer within {@link MAX_INTEGER_BITS}. */
export class IntValue {
  readonly kind = "int";
  readonly depth = 0;
  /** `1 + bitLength / 64`. */
  readonly weight: bigint;
  readonly #value: bigint;
  readonly #bitLength: number;
  private constructor(key: symbol, value: bigint, bits: number) {
    if (key === KEY) stamp(this);
    this.#value = value;
    this.#bitLength = bits;
    this.weight = saturated(1n + limbs(bits));
    Object.freeze(this);
  }
  get value(): bigint {
    return this.#value;
  }
  /** Java's `BigInteger.bitLength`, which is also the bit length of the CBOR magnitude (`n`, or `-1 - n`). O(1). */
  get bitLength(): number {
    return this.#bitLength;
  }
  /**
   * The integer `value`, refused when it is wider than {@link MAX_INTEGER_BITS}: the only constructor. A value that is
   * not a `bigint` (a `number` included, however integral) is a `TypeMismatch`.
   */
  static of(value: bigint): Result<IntValue, IntegerTooWide | TypeMismatch> {
    if (typeof (value as unknown) !== "bigint") return err(notABigint);
    const bits = bitLengthOf(value);
    return bits <= MAX_INTEGER_BITS ? ok(new IntValue(KEY, value, bits)) : err(integerTooWide);
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}

const abs = (n: bigint): bigint => (n < 0n ? -n : n);

/** Once the larger number fits 64 bits, Euclid's remainders are cheap: Lehmer's algorithm hands over to them. */
const LEHMER_FROM_BITS = 64;
/** The leading bits Lehmer's algorithm simulates Euclid on, in exact double arithmetic: every value stays below 2^31. */
const LEHMER_DIGIT = 30;

/** The bit length of `x >= 0`, given a bound `hint` on it: shifts that leave at most 64 bits, so O(1) each. */
function bitsWithin(x: bigint, hint: number): number {
  for (let h = hint; ; ) {
    const s = Math.max(h - 64, 0);
    const top = x >> BigInt(s);
    if (top !== 0n || s === 0) return top === 0n ? 0 : s + bitLengthOf(top);
    h = s;
  }
}

/** `x` as a bigint: itself, or what `BigInt` makes of it (an integral number's integer; a throw if it has none). */
const toBigint = (x: unknown): bigint => (typeof x === "bigint" ? x : BigInt(x as number));

/**
 * `gcd(|a|, |b|)`, and the multi-precision steps it took: Lehmer's algorithm (Knuth, TAOCP vol. 2, 4.5.2, Algorithm L).
 * A BigInt remainder costs about as much as twenty multiplications by a small number, so each step runs Euclid on the
 * leading 30 bits alone, in doubles, for as long as their quotients are certainly the true ones, then applies them to
 * the full numbers at once: about 13 bits a step, where Euclid takes a remainder per bit or two. 33,220-bit Fibonacci
 * neighbours (the slowest pair of that width) take about 2,500 steps instead of 47,849 remainders.
 */
export function gcdSteps(a: bigint, b: bigint): readonly [bigint, number] {
  // read as bigints, whatever was passed: a number would make a remainder NaN and the loop never end; BigInt converts
  // an integral number and throws on anything with no integer value, so every input ends
  const [p0, q0] = [toBigint(a), toBigint(b)];
  let [x, y] = abs(p0) >= abs(q0) ? [abs(p0), abs(q0)] : [abs(q0), abs(p0)];
  let bits = bitLengthOf(x);
  let steps = 0;
  for (; bits > LEHMER_FROM_BITS && y !== 0n; steps++) {
    const shift = BigInt(bits - LEHMER_DIGIT);
    let [xh, yh] = [Number(x >> shift), Number(y >> shift)];
    let [p, q, r, s] = [1, 0, 0, 1];
    while (yh + r !== 0 && yh + s !== 0) {
      const quotient = Math.floor((xh + p) / (yh + r));
      if (quotient !== Math.floor((xh + q) / (yh + s))) break;
      [p, r] = [r, p - quotient * r];
      [q, s] = [s, q - quotient * s];
      [xh, yh] = [yh, xh - quotient * yh];
    }
    // q = 0: not one quotient was certain (y is far shorter than x), so take one full remainder
    [x, y] = q === 0 ? [y, x % y] : [BigInt(p) * x + BigInt(q) * y, BigInt(r) * x + BigInt(s) * y];
    bits = bitsWithin(x, bits);
  }
  for (; y !== 0n; steps++) [x, y] = [y, x % y];
  return [x, steps];
}

const gcd = (a: bigint, b: bigint): bigint => gcdSteps(a, b)[0];

/**
 * The widest numerator or denominator a rational has, in Java `bitLength` bits (docs/specs/canonical-encoding.md
 * section 1): the integer bound, {@link MAX_INTEGER_BITS}, and far past every JLVM number (4096 bits). Checking lowest
 * terms takes a gcd, superlinear in its operands, so a decoder refuses a wider part before any gcd: a rational's
 * decoding then costs at most one gcd of two numbers this wide, about 4 KiB each, and stays linear in its input. Every constructor refuses a wider rational too, so no value is built that no decoder accepts.
 */
export const MAX_RATIONAL_BITS = CANONICAL_MAX_RATIONAL_BITS;

/** A rational past {@link MAX_RATIONAL_BITS}. */
export type RationalTooWide = Extract<CanonicalError, { readonly kind: "RationalTooWide" }>;

const tooWide: RationalTooWide = { kind: "RationalTooWide", limit: MAX_RATIONAL_BITS };

/**
 * Why {@link RationalValue.number} has no value: a zero denominator, an integer or a rational too wide, or a part that
 * is not a `bigint`.
 */
export type NumberError = { readonly kind: "DivisionByZero" } | RationalTooWide | IntegerTooWide | TypeMismatch;

/** A rational `n/d` with `d >= 2`, `gcd(|n|, d) = 1` and both within {@link MAX_RATIONAL_BITS}: never an integer. */
export class RationalValue {
  readonly kind = "rational";
  readonly depth = 0;
  /** `2 + bitLengthOf(n) / 64 + bitLengthOf(d) / 64`. */
  readonly weight: bigint;
  readonly #numerator: bigint;
  readonly #denominator: bigint;
  readonly #numeratorBits: number;
  readonly #denominatorBits: number;
  private constructor(key: symbol, numerator: bigint, denominator: bigint, numeratorBits: number, denominatorBits: number) {
    if (key === KEY) stamp(this);
    this.#numerator = numerator;
    this.#denominator = denominator;
    this.#numeratorBits = numeratorBits;
    this.#denominatorBits = denominatorBits;
    this.weight = saturated(2n + limbs(numeratorBits) + limbs(denominatorBits));
    Object.freeze(this);
  }

  /**
   * The only way to build one: callers have checked that it is reduced and not an integer, and this checks the width,
   * so no wider rational exists.
   */
  private static checked(numerator: bigint, denominator: bigint): RationalValue | undefined {
    const [n, d] = [bitLengthOf(numerator), bitLengthOf(denominator)];
    return n <= MAX_RATIONAL_BITS && d <= MAX_RATIONAL_BITS ? new RationalValue(KEY, numerator, denominator, n, d) : undefined;
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

  /**
   * `n/d` exactly as given, if it already satisfies the invariants (as a decoder must check): `d >= 2`, both within
   * {@link MAX_RATIONAL_BITS} (checked first, so a wide one runs no gcd), and in lowest terms.
   */
  static of(numerator: bigint, denominator: bigint): RationalValue | undefined {
    if (typeof (numerator as unknown) !== "bigint" || typeof (denominator as unknown) !== "bigint") return undefined;
    const valid = denominator >= 2n && bitLengthOf(numerator) <= MAX_RATIONAL_BITS && bitLengthOf(denominator) <= MAX_RATIONAL_BITS;
    return valid && gcd(numerator, denominator) === 1n ? RationalValue.checked(numerator, denominator) : undefined;
  }

  /**
   * The number `n/d` in the single numeric domain: an integer when it is one, refused when wider than
   * {@link MAX_INTEGER_BITS}, otherwise a reduced rational, refused when its numerator or denominator is wider than
   * {@link MAX_RATIONAL_BITS}. A part that is not a `bigint` (a `number`, or an object whose `valueOf` gives one,
   * included) is a `TypeMismatch`, checked first.
   */
  static number(numerator: bigint, denominator: bigint): Result<IntValue | RationalValue, NumberError> {
    if (typeof (numerator as unknown) !== "bigint" || typeof (denominator as unknown) !== "bigint") return err(notABigint);
    if (denominator === 0n) return err({ kind: "DivisionByZero" });
    const sign = denominator < 0n ? -1n : 1n;
    const [sn, sd] = [sign * numerator, sign * denominator];
    const g = gcd(sn, sd);
    const n = sn / g;
    const d = sd / g;
    if (d === 1n) return IntValue.of(n);
    const r = RationalValue.checked(n, d);
    return r === undefined ? err(tooWide) : ok(r);
  }

  /**
   * `unscaled / 10^scale` in lowest terms. `10^scale`'s only primes are 2 and 5, so the common factor is
   * `2^min(v2, scale) 5^min(v5, scale)`: the twos go in one shift, and the fives by trial division by `5^(2^i)` from the
   * largest `2^i <= scale` down, which builds `min(v5, scale)` bit by bit (about 14 divisions for a scale of 10000).
   * The same result as {@link number} against `10^scale`, without its general gcd; refused, like it, past
   * {@link MAX_RATIONAL_BITS} (a JSON decimal can be: `0.000...1` at a scale of 10,000 has the denominator
   * `10^10000`), and an integer past {@link MAX_INTEGER_BITS}. A scale more than {@link MAX_RATIONAL_BITS} past the
   * unscaled integer's bit length is refused at once: its twos alone leave the denominator wider than that, and the
   * powers of five it would build grow with the scale. A negative scale multiplies, exactly: `unscaled * 10^-scale`,
   * refused as {@link IntegerTooWide} from `-scale >= CANONICAL_MAX_DIGITS` before any power is built. An `unscaled`
   * that is not a `bigint` is a `TypeMismatch`, as is a scale that is not a `number` with an integer value (`NaN`, a
   * fraction, an infinity), which names no decimal. Nothing throws.
   */
  static decimal(unscaled: bigint, scale: number): Result<IntValue | RationalValue, RationalTooWide | IntegerTooWide | TypeMismatch> {
    if (typeof (unscaled as unknown) !== "bigint") return err(notABigint);
    if (!Number.isInteger(scale)) return err(scaleNotInteger);
    // a negative scale multiplies: 10^k is wider than every integer from k = CANONICAL_MAX_DIGITS (2^33219 < 10^10000)
    if (scale < 0) return unscaled === 0n ? IntValue.of(0n) : -scale >= CANONICAL_MAX_DIGITS ? err(integerTooWide) : IntValue.of(unscaled * 10n ** BigInt(-scale));
    if (unscaled === 0n) return IntValue.of(0n);
    const sign = unscaled < 0n ? -1n : 1n;
    const magnitude = sign * unscaled;
    // the denominator keeps 2^(scale - twos), and twos is at most the magnitude's bit length
    if (scale - bitLengthOf(magnitude) > MAX_RATIONAL_BITS) return err(tooWide);
    const twos = Math.min(bitLengthOf(magnitude & -magnitude) - 1, scale);
    const powers: (readonly [number, bigint])[] = [];
    for (let e = 1, p = 5n; e <= scale; e *= 2, p *= p) powers.push([e, p]);
    const [rest, fives] = powers.reduceRight<readonly [bigint, number]>(
      ([n, count], [e, p]) => (count + e <= scale && n % p === 0n ? [n / p, count + e] : [n, count]),
      [magnitude >> BigInt(twos), 0],
    );
    const d = 5n ** BigInt(scale - fives) << BigInt(scale - twos);
    if (d === 1n) return IntValue.of(sign * rest);
    const r = RationalValue.checked(sign * rest, d);
    return r === undefined ? err(tooWide) : ok(r);
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
  private constructor(key: symbol, readonly value: string) {
    if (key === KEY) stamp(this);
    this.#utf8 = encodeUtf8(value);
    this.weight = 1n + BigInt(this.#utf8.length);
    Object.freeze(this);
  }

  static {
    utf8Of = (t) => t.#utf8;
  }

  /** Text, if it has no unpaired surrogates; a value that is not a string is a `TypeMismatch`. */
  static of(value: string): Result<TextValue, CanonicalError> {
    if (typeof (value as unknown) !== "string") return err(notAString);
    return isWellFormed(value) ? ok(new TextValue(KEY, value)) : err({ kind: "UnpairedSurrogate" });
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
  /** `bytes` is a fresh copy, which the value now owns. */
  private constructor(key: symbol, bytes: Uint8Array) {
    if (key === KEY) stamp(this);
    this.#bytes = bytes;
    this.weight = 1n + BigInt(this.#bytes.length);
    Object.freeze(this);
  }

  static {
    bytesOf = (b) => b.#bytes;
    bytesOfOwn = (b) => new BytesValue(KEY, copyBytes(b));
  }

  /**
   * A byte string holding a copy of `bytes`, taken from the array's internal slots (`takeBytes`): its length is its
   * own, whatever a subclass's `length` says. A value that is not a `Uint8Array` (`isBytes`: another realm's and a
   * `Buffer` are) is a `TypeMismatch`, never copied as `Uint8Array.from` would read it.
   */
  static of(bytes: Uint8Array): Result<BytesValue, TypeMismatch> {
    const copy = takeBytes(bytes);
    return copy === undefined ? err(notBytes) : ok(new BytesValue(KEY, copy));
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
    key: symbol,
    items: readonly Value[],
    readonly depth: number,
    /** One plus each element's weight. */
    readonly weight: bigint,
  ) {
    if (key === KEY) stamp(this);
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

  /**
   * The array of `items`, if within the depth limit. Its length is read once and each item once, by index (never
   * through its iterator): `items` that is not an array, or an item that is not a canonical value (see {@link isValue}),
   * is a `TypeMismatch`, found in the one pass that copies the items and adds up their depth and weight.
   */
  static of(items: readonly Value[]): Result<ArrayValue, CanonicalError> {
    // the length read once: a real array's is a number, a Proxy's need not be (and comparing a symbol throws)
    const length: unknown = Array.isArray(items) ? items.length : undefined;
    if (typeof length !== "number") return err(notAnArray);
    const kept: Value[] = [];
    let [deepest, weight] = [0, 1n];
    for (let i = 0; i < length; i++) {
      const v: unknown = items[i];
      if (!isValue(v)) return err(expected(`a canonical value at index ${i.toString()}`));
      kept.push(v);
      deepest = Math.max(deepest, v.depth);
      weight += v.weight;
    }
    return deepest >= MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(KEY, Object.freeze(kept), deepest + 1, saturated(weight)));
  }

  static {
    arrayOfOwn = (items) => {
      const kept = Object.freeze([...items]);
      const [depth, weight] = measure(kept, (v) => [v.depth, v.weight]);
      return depth > MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(KEY, kept, depth, weight));
    };
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
    key: symbol,
    entries: readonly Entry[],
    readonly depth: number,
    /** One plus each entry's `1 + key UTF-8 length + value weight`. */
    readonly weight: bigint,
  ) {
    if (key === KEY) stamp(this);
    this.#entries = entries;
    Object.freeze(this);
  }

  static {
    entriesOf = (m) => m.#entries;
  }

  /**
   * The map of `entries`, checked in order: a repeated key (even with a null value) is `DuplicateKey`, a key with an
   * unpaired surrogate is `UnpairedSurrogate`; null values are dropped; excess depth is `DepthExceeded`.
   *
   * `entries` is read once, as `for...of` reads it, its iterator closed when an entry is refused or caller code throws
   * while one is read. What does not match the types is a `TypeMismatch`, found in that pass: `entries` that is neither
   * an object nor a function, an iterator `for...of` could not open or a result that is not an object (each of which
   * `for...of` throws a `TypeError` for), an entry that is not an array (read by index, never destructured), a key that
   * is not a string, or a value that is not a canonical value (see {@link isValue}).
   *
   * Unlike `for...of`, when caller code throws while an entry is read and the iterator's `return` then throws too, the
   * error that surfaces is `return`'s, the last one thrown (`for...of` keeps the first): keeping the first would need a
   * catch clause, which this SDK does not use.
   */
  static of(entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> {
    const given: unknown = entries;
    // an object, or a function, as for...of accepts either
    if (!isResult(given)) return err(notEntries);
    const method: unknown = Reflect.get(given, Symbol.iterator);
    const seen = new KeySet();
    const kept: Entry[] = [];
    // an array the intrinsic iterator would walk is read as it would, by index, with nothing to close
    if (walksAsArray(given, method)) {
      for (let i = 0; i < given.length; i++) {
        const refused = MapValue.entry(seen, kept, given[i], i);
        if (refused !== undefined) return err(refused);
      }
      return MapValue.closed(kept);
    }
    const read = openWith<unknown>(given, method);
    if (read === undefined) return err(notIterable);
    // as for...of: the iterator is closed for a refusal, or a throw, while an entry is read, never for one from its own
    // next() or from reading its result
    let reading = false;
    try {
      for (let i = 0; ; i++) {
        const r: unknown = read.next();
        if (!isResult(r)) return err(notAResult);
        if (r.done) return MapValue.closed(kept);
        const entry: unknown = r.value;
        reading = true;
        const refused = MapValue.entry(seen, kept, entry, i);
        if (refused !== undefined) return err(refused);
        reading = false;
      }
    } finally {
      if (reading) close(read);
    }
  }

  /** Checks and keeps the `i`th entry read: a `[string, Value]` array (read by index), with a key not seen before. */
  private static entry(seen: KeySet, kept: Entry[], entry: unknown, i: number): CanonicalError | undefined {
    if (!Array.isArray(entry)) return expected(`a [key, value] array at index ${i.toString()}`);
    const key: unknown = entry[0];
    const value: unknown = entry[1];
    if (typeof key !== "string") return expected(`a string key at index ${i.toString()}`);
    if (!isValue(value)) return expected(`a canonical value at index ${i.toString()}`);
    return MapValue.add(seen, kept, key, value);
  }

  /** Keeps the entry `key: value`, unless its key repeats one already seen or is not well-formed (why, then). */
  private static add(seen: KeySet, kept: Entry[], key: string, value: Value): CanonicalError | undefined {
    if (!seen.add(key)) return { kind: "DuplicateKey", key: Preview.of(key) };
    if (!isWellFormed(key)) return { kind: "UnpairedSurrogate" };
    if (value.kind !== "null") kept.push({ key, utf8: encodeUtf8(key), value });
    return undefined;
  }

  /** The map of the entries kept, in canonical key order, if within the depth limit. */
  private static closed(kept: Entry[]): Result<MapValue, CanonicalError> {
    kept.sort((a, b) => canonicalKeyOrder(a.utf8, b.utf8));
    const [depth, weight] = measure(kept, (e) => [e.value.depth, 1n + BigInt(e.utf8.length) + e.value.weight]);
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new MapValue(KEY, Object.freeze(kept), depth, weight));
  }

  static {
    mapOfOwn = (entries) => {
      const seen = new KeySet();
      const kept: Entry[] = [];
      for (const [key, value] of entries) {
        const refused = MapValue.add(seen, kept, key, value);
        if (refused !== undefined) return err(refused);
      }
      return MapValue.closed(kept);
    };
  }

  /**
   * The value at `key`, if present: O(|key| + log n). A key with an unpaired surrogate encodes to bytes no well-formed
   * key has, so it is never found; nor is a key that is not a string (from a JavaScript caller, or a cast), which is
   * never read.
   */
  get(key: string): Value | undefined {
    if (typeof (key as unknown) !== "string") return undefined;
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
 * Something that is not a canonical value (see {@link isValue}: from a JavaScript caller, or a cast) equals nothing,
 * itself included: the answer is `false`, found in O(1) before either side is read.
 */
export function equals(a: Value, b: Value): boolean {
  if (!isValue(a) || !isValue(b)) return false;
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
      return `h'${toHexOf(bytesOf(v))}'`;
  }
}

type Showing = { readonly map: false; readonly items: readonly Value[]; at: number } | { readonly map: true; readonly entries: readonly Entry[]; at: number };

/** What {@link inspect} shows for something that is not a canonical value: fixed, so the input is never read. */
export const NOT_A_VALUE = "<not a canonical value>";

/**
 * A readable form of `value` (CBOR diagnostic style: `[1, "a", h'00ff', 1/3, {"k": null}]`), bounded however often it
 * repeats a shared part: scalars spend their weight and containers and map keys spend theirs from a budget of
 * {@link DEBUG_BUDGET}, nothing deeper than {@link DEBUG_DEPTH} is shown, and what is not shown is `…`. Iterative.
 * Something that is not a canonical value (see {@link isValue}) is shown as {@link NOT_A_VALUE}, never read or
 * stringified.
 */
export function inspect(value: Value): string {
  if (!isValue(value)) return NOT_A_VALUE;
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
  /** The boolean `b`; a value that is not a boolean (from a JavaScript caller, or a cast) is a `TypeMismatch`. */
  bool: (b: boolean): Result<BoolValue, TypeMismatch> => BoolValue.of(b),
  /** The integer `n` ({@link IntValue.of}): refused past {@link MAX_INTEGER_BITS}, and a `TypeMismatch` if not a `bigint`. */
  int: (n: bigint): Result<IntValue, IntegerTooWide | TypeMismatch> => IntValue.of(n),
  /** `n / d` in lowest terms ({@link RationalValue.number}): a part that is not a `bigint` is a `TypeMismatch`. */
  number: (n: bigint, d: bigint): Result<IntValue | RationalValue, NumberError> => RationalValue.number(n, d),
  /** The text `s` ({@link TextValue.of}); a value that is not a string is a `TypeMismatch`. */
  text: (s: string): Result<TextValue, CanonicalError> => TextValue.of(s),
  /** A copy of the bytes `b` ({@link BytesValue.of}); a value that is not a `Uint8Array` is a `TypeMismatch`. */
  bytes: (b: Uint8Array): Result<BytesValue, TypeMismatch> => BytesValue.of(b),
  /** The array of `items` ({@link ArrayValue.of}): `items` that is not an array, or an item that is not a value, is a `TypeMismatch`. */
  array: (items: readonly Value[]): Result<ArrayValue, CanonicalError> => ArrayValue.of(items),
  /** The map of `entries` ({@link MapValue.of}): an entry that is not a `[string, Value]` array is a `TypeMismatch`. */
  map: (entries: Iterable<readonly [string, Value]>): Result<MapValue, CanonicalError> => MapValue.of(entries),
  equals: (a: Value, b: Value): boolean => equals(a, b),
} as const;
