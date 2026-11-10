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
 * either is O(1); a map is a persistent tree (tree.ts) whose nodes keep them, so {@link MapValue.set} and
 * {@link MapValue.delete} keep them exact in O(log n). So:
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
import { isArrayGuarded, readGuarded } from "./guard.js";
import { close, isArrayLength, isResult, NOT_A_RESULT, NOT_ITERABLE, openWith, readStep, walksAsArray } from "./iterator.js";
import { KeySet } from "./keyset.js";
import { Preview } from "./preview.js";
import { err, ok, type Result } from "./result.js";
import { ABSENT, built, canonicalKeyOrder, codePointOrder, cursor, found, inserted, listed, nodes, removed, type Entry, type Node } from "./tree.js";
import { compareBytes, encodeUtf8, isWellFormed } from "./utf8.js";

// the typed-array helpers live in bytes.ts, re-exported for the modules that import them from here
export { isBytes, plainBytes, takeBytes } from "./bytes.js";
// and the map's entry and key order live in tree.ts
export { canonicalKeyOrder, type Entry } from "./tree.js";

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
Object.freeze(Identity);
Object.freeze(Identity.prototype);

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
Object.freeze(Minted);
Object.freeze(Minted.prototype);

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

const depthExceeded: CanonicalError = Object.freeze({ kind: "DepthExceeded", limit: MAX_DEPTH });

// What a caller passes past the types (from JavaScript, or a cast) is checked in O(1) before it is read, and refused as
// a TypeMismatch naming what was expected, never what was given.
const expected = (what: string): TypeMismatch => Object.freeze({ kind: "TypeMismatch", reason: `expected ${what}` });
const notABoolean = expected("a boolean");
const notABigint = expected("a bigint");
const notAString = expected("a string");
const notBytes = expected("a Uint8Array");
const notAnArray = expected("an array");
const notEntries = expected("an iterable of [key, value] entries");
const notAKey = expected("a string key");
const notAMap = expected("a canonical map");
const notAValue = expected("a canonical value");
const notIterable: TypeMismatch = Object.freeze({ kind: "TypeMismatch", reason: NOT_ITERABLE });
const notAResult: TypeMismatch = Object.freeze({ kind: "TypeMismatch", reason: NOT_A_RESULT });
const scaleNotInteger: TypeMismatch = Object.freeze({ kind: "TypeMismatch", reason: "the scale is not an integer" });

/** Node's `util.inspect` hook, so a value prints through {@link inspect}, within its budget. */
const INSPECT: unique symbol = Symbol.for("nodejs.util.inspect.custom");

// Read-only views of private state for this package's own walks (encoding, rendering, equality), which must not copy
// before they have checked their budget. Set once, by each class's static block; never exported from the package.
let utf8Of!: (t: TextValue) => Uint8Array;
let bytesOf!: (b: BytesValue) => Uint8Array;
let entriesOf!: (m: MapValue) => readonly Entry[];
let cursorOf!: (m: MapValue) => () => Entry | undefined;
let codePointOf!: (m: MapValue) => readonly Entry[];
let keyTextOf!: (e: Entry) => TextValue;
let statsOf!: (m: MapValue) => TreeStats;
let rootOf!: (m: MapValue) => Node | undefined;

let bytesOfOwn!: (bytes: Uint8Array) => BytesValue;
let arrayOfOwn!: (items: readonly Value[]) => Result<ArrayValue, CanonicalError>;
let mapOfOwn!: (entries: readonly (readonly [string, Value])[]) => Result<MapValue, CanonicalError>;
let mapOfSorted!: (entries: readonly Entry[]) => Result<MapValue, CanonicalError>;

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
/**
 * The map of `entries`, which are already unique, well-formed, non-null and in canonical key order (the strict CBOR
 * decoder has checked all four): built in O(n), with no key compared, measured or encoded again. The map takes the
 * array, frozen, as its list of entries. Internal to this package.
 */
export const mapOfCanonical = (entries: readonly Entry[]): Result<MapValue, CanonicalError> => mapOfSorted(entries);

/** The stored UTF-8 of `t`, uncopied: internal to this package, and never written. */
export const utf8View = (t: TextValue): Uint8Array => utf8Of(t);
/** The stored bytes of `b`, uncopied: internal to this package, and never written. */
export const bytesView = (b: BytesValue): Uint8Array => bytesOf(b);
/**
 * The entries of `m` in canonical key order: the list it was built from, uncopied, or else listed from its tree, O(n),
 * and not kept. Internal to this package.
 */
export const entriesView = (m: MapValue): readonly Entry[] => entriesOf(m);
/**
 * The entries of `m` in canonical key order, one at each call, then `undefined`: from its list, or else a walk of its
 * tree holding O(height), so a walk of a map an update made lists nothing. Internal to this package (the encoder).
 */
export const entryCursor = (m: MapValue): (() => Entry | undefined) => cursorOf(m);
/**
 * The entries of `m` in code-point order of their keys (the order of their UTF-8 bytes), uncopied and never written
 * (a key's UTF-8 may be its text value's own): found from the canonical order in time linear in `m`'s weight
 * (`codePointOrder`), and kept only by a map that holds its list. Internal to this package (the JSON rendering): an
 * entry and its key's bytes are the map's own, so they never leave it; other packages read {@link codePointKeys} and
 * {@link codePointValues}.
 */
export const codePointView = (m: MapValue): readonly Entry[] => codePointOf(m);
/**
 * The keys of `m` as text values, in {@link codePointView}'s order: each shares its entry's UTF-8, which a text value
 * never hands out (`utf8()` copies), so none is checked or encoded again (a map's keys are well-formed, and their
 * UTF-8 is theirs). O(n), a fresh array at each call. Internal to this SDK: the JLVM's `keys` and `entries`.
 */
export const codePointKeys = (m: MapValue): readonly TextValue[] => codePointOf(m).map(keyTextOf);
/** The values of `m` in {@link codePointView}'s order: O(n), a fresh array at each call. Internal to this SDK. */
export const codePointValues = (m: MapValue): readonly Value[] => codePointOf(m).map((e) => e.value);

/** What a test reads of a map's tree: its height, and whether it holds a list of its entries, in either order. */
export interface TreeStats {
  readonly height: number;
  readonly listed: boolean;
  readonly listedByCodePoint: boolean;
}

/** For tests: `m`'s tree's height, and which lists of its entries it holds. O(1); internal to this SDK. */
export const treeStats = (m: MapValue): TreeStats => statsOf(m);

/**
 * For tests: how many nodes of `b`'s tree `a`'s does not hold, so an update's own nodes can be counted. O(the size of
 * both): `a`'s nodes are collected, then `b`'s walked, skipping every subtree it shares (a node is never changed, so a
 * shared node shares its subtree). Internal to this SDK.
 */
export function unsharedNodes(a: MapValue, b: MapValue): number {
  const theirs = new Set<object>(nodes(rootOf(a)));
  let fresh = 0;
  const todo: (Node | undefined)[] = [rootOf(b)];
  for (let t = todo.pop(); todo.length > 0 || t !== undefined; t = todo.pop()) {
    if (t === undefined || theirs.has(t)) continue;
    fresh++;
    todo.push(t.left, t.right);
  }
  return fresh;
}

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
Object.freeze(NullValue);
Object.freeze(NullValue.prototype);

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
Object.freeze(BoolValue);
Object.freeze(BoolValue.prototype);

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

const integerTooWide: IntegerTooWide = Object.freeze({ kind: "IntegerTooWide", limit: MAX_INTEGER_BITS });

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
Object.freeze(IntValue);
Object.freeze(IntValue.prototype);

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

const tooWide: RationalTooWide = Object.freeze({ kind: "RationalTooWide", limit: MAX_RATIONAL_BITS });

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
  static #checked(numerator: bigint, denominator: bigint): RationalValue | undefined {
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
    return valid && gcd(numerator, denominator) === 1n ? RationalValue.#checked(numerator, denominator) : undefined;
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
    const r = RationalValue.#checked(n, d);
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
    const r = RationalValue.#checked(sign * rest, d);
    return r === undefined ? err(tooWide) : ok(r);
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}
Object.freeze(RationalValue);
Object.freeze(RationalValue.prototype);

export class TextValue {
  readonly kind = "text";
  readonly depth = 0;
  /** `1 +` the UTF-8 length. */
  readonly weight: bigint;
  /** The UTF-8 encoding. */
  readonly #utf8: Uint8Array;
  /** `utf8`, when given, is `value`'s UTF-8 already, which the text then shares (a map's key: see `codePointKeys`). */
  private constructor(key: symbol, readonly value: string, utf8?: Uint8Array) {
    if (key === KEY) stamp(this);
    this.#utf8 = utf8 ?? encodeUtf8(value);
    this.weight = 1n + BigInt(this.#utf8.length);
    Object.freeze(this);
  }

  static {
    utf8Of = (t) => t.#utf8;
    keyTextOf = (e) => new TextValue(KEY, e.key, e.utf8);
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
Object.freeze(TextValue);
Object.freeze(TextValue.prototype);

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
Object.freeze(BytesValue);
Object.freeze(BytesValue.prototype);

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
    const length: unknown = isArrayGuarded(items) ? items.length : undefined;
    if (!isArrayLength(length)) return err(notAnArray);
    const kept: Value[] = [];
    let [deepest, weight] = [0, 1n];
    for (let i = 0; i < length; i++) {
      // guarded: an earlier item's read (a Proxy's trap) may have revoked the array, and no value is REVOKED
      const v: unknown = readGuarded(items, i);
      if (!isValue(v)) return err(expected(`a canonical value at index ${i.toString()}`));
      kept.push(v);
      deepest = Math.max(deepest, v.depth);
      weight += v.weight;
    }
    return deepest >= MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(KEY, Object.freeze(kept), deepest + 1, saturated(weight)));
  }

  static {
    arrayOfOwn = (items) => {
      // each item read once, by index, into an array of this module's own, and checked there: every caller holds values
      // this module made, so the check is O(1) and never refuses, but nothing else is ever minted into an array
      const kept: Value[] = [];
      let [deepest, weight] = [0, 1n];
      for (let i = 0; i < items.length; i++) {
        const v: unknown = items[i];
        if (!isValue(v)) return err(expected(`a canonical value at index ${i.toString()}`));
        kept.push(v);
        if (v.depth > deepest) deepest = v.depth;
        weight += v.weight;
      }
      return deepest >= MAX_DEPTH ? err(depthExceeded) : ok(new ArrayValue(KEY, Object.freeze(kept), deepest + 1, saturated(weight)));
    };
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}
Object.freeze(ArrayValue);
Object.freeze(ArrayValue.prototype);

/** A map's weight: one, plus its entries' exact sum, saturated at {@link MAX_WEIGHT} only here. */
const mapWeight = (root: Node | undefined): bigint => saturated(BigInt(root?.weight ?? 0) + 1n);

/**
 * A map: its entries in canonical key order, as a persistent AVL tree (tree.ts), the order Rust's `Map` keeps. Keys are
 * compared, never hashed (see keyset.ts).
 *
 * {@link get} and {@link has} search the tree, O(log n) comparisons. {@link set} and {@link delete} return a new map in
 * O(log n): they build one node per level of the search and share every other one with this map, which they leave as
 * it is. The depth and weight are kept by the tree, so they are O(1) and exact through any updates.
 *
 * A map holds nothing but its tree, and the list of entries it was built from, if it was (decoded, or from
 * {@link of}): never a listing made later, so versions that share their nodes share their memory too, however many are
 * walked. Encoding, hashing and counting, equality and inspection walk the list, or the tree itself, one entry at a
 * time (stopping where they stop: at a difference, at the budget); {@link entries} and the codecs list the entries
 * afresh. The code-point order (JSON rendering, the JLVM's listings) is found from the canonical one at each use, in
 * time linear in the weight, and kept only beside a list the map holds: a map built from entries keeps it once found.
 */
export class MapValue {
  readonly kind = "map";
  readonly #root: Node | undefined;
  readonly #size: number;
  /** The entries in canonical key order it was built from, if it was: never a listing made later. */
  readonly #canonical: readonly Entry[] | undefined;
  /**
   * Beside {@link #canonical} alone, the entries in code-point order of their keys, once found: a cache, written once,
   * of a pure function of the map, which at most doubles the memory of the list it was found from.
   */
  #codePoint: readonly Entry[] | undefined;
  private constructor(
    key: symbol,
    root: Node | undefined,
    size: number,
    canonical: readonly Entry[] | undefined,
    readonly depth: number,
    /** One plus each entry's `1 + key UTF-8 length + value weight`. */
    readonly weight: bigint,
  ) {
    if (key === KEY) stamp(this);
    this.#root = root;
    this.#size = size;
    this.#canonical = canonical;
    Object.freeze(this);
  }

  /** The map of the tree `root`, of `size` entries, nesting within the limit. */
  static #rooted(root: Node | undefined, size: number, canonical?: readonly Entry[]): MapValue {
    return new MapValue(KEY, root, size, canonical, (root?.deepest ?? 0) + 1, mapWeight(root));
  }

  /** The empty map. */
  static readonly empty: MapValue = MapValue.#rooted(undefined, 0, Object.freeze([]));

  /**
   * Whether `m` is a map this module minted: only then is its tree read. One built by calling the constructor from
   * outside holds a tree its caller chose (of anything, weighing anything), so it reads as empty, and nothing is ever
   * built from it.
   */
  static #isMinted(m: object): m is MapValue {
    return minted(m) && #root in m;
  }

  static {
    const none: readonly Entry[] = Object.freeze([]);
    entriesOf = (m) => (MapValue.#isMinted(m) ? (m.#canonical ?? Object.freeze(listed(m.#root))) : none);
    codePointOf = (m) => {
      if (!MapValue.#isMinted(m)) return none;
      const list = m.#canonical;
      return list === undefined ? Object.freeze(codePointOrder(listed(m.#root))) : (m.#codePoint ??= Object.freeze(codePointOrder(list)));
    };
    rootOf = (m) => (MapValue.#isMinted(m) ? m.#root : undefined);
    // the list when the map has one (read in the order its entries lie in memory), else a walk of the tree that makes
    // none: for the walks that read one entry at a time (encoding, equality, inspection), each holding O(height)
    cursorOf = (m) => {
      const list = MapValue.#isMinted(m) ? m.#canonical : none;
      if (list === undefined) return cursor(m.#root);
      let i = 0;
      return () => list[i++];
    };
    statsOf = (m) => {
      const own = MapValue.#isMinted(m);
      return Object.freeze({ height: rootOf(m)?.height ?? 0, listed: own && m.#canonical !== undefined, listedByCodePoint: own && m.#codePoint !== undefined });
    };
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
      for (let i = 0; ; i++) {
        // the length each step, as the array iterator reads it; a Proxy's may be one no array has, never looped up to
        // each read guarded: the iterator method's read, or an entry's, may have revoked the array (a Proxy's trap)
        const length: unknown = readGuarded(given, "length");
        if (!isArrayLength(length)) return err(notEntries);
        if (i >= length) break;
        const refused = MapValue.#entry(seen, kept, readGuarded(given, i), i);
        if (refused !== undefined) return err(refused);
      }
      return MapValue.#closed(kept);
    }
    const read = openWith<unknown>(given, method);
    if (read === undefined) return err(notIterable);
    // as for...of: the iterator is closed for a refusal, or a throw, while an entry is read, never for one from its own
    // next() or from reading its result
    let reading = false;
    try {
      for (let i = 0; ; i++) {
        const r = readStep(read.next());
        if (r === undefined) return err(notAResult);
        if (r.done) return MapValue.#closed(kept);
        const entry: unknown = r.value;
        reading = true;
        const refused = MapValue.#entry(seen, kept, entry, i);
        if (refused !== undefined) return err(refused);
        reading = false;
      }
    } finally {
      if (reading) close(read);
    }
  }

  /** Checks and keeps the `i`th entry read: a `[string, Value]` array (read by index), with a key not seen before. */
  static #entry(seen: KeySet, kept: Entry[], entry: unknown, i: number): CanonicalError | undefined {
    if (!isArrayGuarded(entry)) return expected(`a [key, value] array at index ${i.toString()}`);
    // guarded: the key's read (a Proxy's trap) may have revoked the entry; neither check takes REVOKED
    const key: unknown = readGuarded(entry, 0);
    const value: unknown = readGuarded(entry, 1);
    if (typeof key !== "string") return expected(`a string key at index ${i.toString()}`);
    if (!isValue(value)) return expected(`a canonical value at index ${i.toString()}`);
    return MapValue.#add(seen, kept, key, value);
  }

  /** Keeps the entry `key: value`, unless its key repeats one already seen or is not well-formed (why, then). */
  static #add(seen: KeySet, kept: Entry[], key: string, value: Value): CanonicalError | undefined {
    if (!seen.add(key)) return { kind: "DuplicateKey", key: Preview.of(key) };
    if (!isWellFormed(key)) return { kind: "UnpairedSurrogate" };
    if (value.kind !== "null") kept.push({ key, utf8: encodeUtf8(key), value });
    return undefined;
  }

  /** The map of the entries kept, in canonical key order, if within the depth limit. */
  static #closed(kept: Entry[]): Result<MapValue, CanonicalError> {
    kept.sort((a, b) => canonicalKeyOrder(a.utf8, b.utf8));
    return MapValue.#sorted(kept);
  }

  /** The map of `entries` (unique, non-null, in canonical key order), if within the depth limit: O(n), from the bottom up. */
  static #sorted(entries: readonly Entry[]): Result<MapValue, CanonicalError> {
    const root = built(entries);
    const depth = (root?.deepest ?? 0) + 1;
    return depth > MAX_DEPTH ? err(depthExceeded) : ok(new MapValue(KEY, root, entries.length, Object.freeze(entries), depth, mapWeight(root)));
  }

  static {
    mapOfOwn = (entries) => {
      const seen = new KeySet();
      const kept: Entry[] = [];
      for (const [key, value] of entries) {
        const refused = MapValue.#add(seen, kept, key, value);
        if (refused !== undefined) return err(refused);
      }
      return MapValue.#closed(kept);
    };
    mapOfSorted = (entries) => MapValue.#sorted(entries);
  }

  /**
   * The value at `key`, if present: O(|key| + log n). A key with an unpaired surrogate encodes to bytes no well-formed
   * key has, so it is never found; nor is a key that is not a string (from a JavaScript caller, or a cast), which is
   * never read.
   */
  get(key: string): Value | undefined {
    if (typeof (key as unknown) !== "string" || !MapValue.#isMinted(this)) return undefined;
    return found(this.#root, encodeUtf8(key))?.value;
  }

  /** Whether the map holds `key`: {@link get}'s search, and like it, false for a key that is not a string. */
  has(key: string): boolean {
    return this.get(key) !== undefined;
  }

  /**
   * This map with `key` set to `value`, or without `key` when `value` is null (a map never holds null, as in
   * {@link of}): O(|key| + log n), building one node per level of the search and sharing every other one with this
   * map, which is unchanged. Refused, in this order: a map this module did not mint (one built by calling the
   * constructor from JavaScript, whose tree its caller chose), a key that is not a string, or a value that is not a
   * canonical value (see {@link isValue}), as a `TypeMismatch`, none read further; a key with an unpaired surrogate as
   * `UnpairedSurrogate`, null value or not, as {@link of} refuses it; and a value of depth {@link MAX_DEPTH} or more as
   * `DepthExceeded`, since the map would nest one deeper.
   */
  set(key: string, value: Value): Result<MapValue, CanonicalError> {
    if (!MapValue.#isMinted(this)) return err(notAMap);
    if (typeof (key as unknown) !== "string") return err(notAKey);
    if (!isValue(value)) return err(notAValue);
    if (!isWellFormed(key)) return err({ kind: "UnpairedSurrogate" });
    if (value.kind === "null") return ok(this.delete(key));
    if (value.depth >= MAX_DEPTH) return err(depthExceeded);
    const [root, added] = inserted(this.#root, { key, utf8: encodeUtf8(key), value });
    return ok(MapValue.#rooted(root, added ? this.#size + 1 : this.#size));
  }

  /**
   * This map without `key`: O(|key| + log n), building one node per level of the search and sharing every other one
   * with this map, which is unchanged. A key the map does not hold gives this map itself; so does a key that is not a
   * string (from a JavaScript caller, or a cast), which is never read, and one with an unpaired surrogate, which no map
   * holds. Removing never deepens a map, so nothing is refused. A map this module did not mint is given back as it is,
   * its tree never read: nothing is built from it.
   */
  delete(key: string): MapValue {
    if (typeof (key as unknown) !== "string" || !MapValue.#isMinted(this)) return this;
    const root = removed(this.#root, encodeUtf8(key));
    return root === ABSENT ? this : MapValue.#rooted(root, this.#size - 1);
  }

  /** The entries in canonical key order. */
  entries(): readonly (readonly [string, Value])[] {
    return entriesOf(this).map((e) => [e.key, e.value] as const);
  }

  /** The keys in canonical key order. */
  keys(): readonly string[] {
    return entriesOf(this).map((e) => e.key);
  }

  /** Each key's UTF-8 and value, in canonical key order. */
  encodedEntries(): readonly (readonly [Uint8Array, Value])[] {
    return entriesOf(this).map((e) => [copyBytes(e.utf8), e.value] as const);
  }

  /** The number of entries. O(1). */
  get size(): number {
    return this.#size;
  }
  toString(): string {
    return inspect(this);
  }
  [INSPECT](): string {
    return inspect(this);
  }
}
Object.freeze(MapValue);
Object.freeze(MapValue.prototype);

/**
 * The keys of a map, checked once, and their canonical order: a map with exactly these keys is then built from their
 * values in O(n), with no key compared, measured or encoded (as Rust's `MapShape`). For the JLVM's object literals,
 * whose keys are known when a program compiles and whose map is built at every evaluation. Internal to this SDK. What
 * it builds is checked as {@link MapValue.of} checks it (keys distinct, well-formed strings, values canonical, each in
 * O(1) at build time), and only a shape {@link MapShape.of} made builds anything, so no map is minted from anything
 * else.
 */
export class MapShape {
  /** Each key and its UTF-8, in canonical key order, with the position of its value. */
  readonly #order: readonly (readonly [string, Uint8Array, number])[];
  /** Whether {@link MapShape.of} made this shape: one built by calling the constructor from outside builds nothing. */
  readonly #made: boolean;
  private constructor(key: symbol, order: readonly (readonly [string, Uint8Array, number])[]) {
    this.#made = key === KEY;
    this.#order = order;
    Object.freeze(this);
  }

  static #isMade(x: object): x is MapShape {
    return #made in x && x.#made;
  }

  /** The number of keys. */
  get size(): number {
    return MapShape.#isMade(this) ? this.#order.length : 0;
  }

  /**
   * The shape of `keys`, in the order their values will be given: O(n log n), once. `keys` that is not an array, or a
   * key that is not a string, is a `TypeMismatch`; a key with an unpaired surrogate is `UnpairedSurrogate` (both the
   * first in the given order); a key given twice is `DuplicateKey`.
   */
  static of(keys: readonly string[]): Result<MapShape, CanonicalError> {
    if (!isArrayGuarded(keys)) return err(notAnArray);
    const order: (readonly [string, Uint8Array, number])[] = [];
    for (let i = 0; i < keys.length; i++) {
      const key: unknown = keys[i];
      if (typeof key !== "string") return err(expected(`a string key at index ${i.toString()}`));
      if (!isWellFormed(key)) return err({ kind: "UnpairedSurrogate" });
      order.push([key, encodeUtf8(key), i]);
    }
    order.sort((a, b) => canonicalKeyOrder(a[1], b[1]));
    for (let i = 1; i < order.length; i++) {
      const [before, here] = [order[i - 1], order[i]];
      if (before !== undefined && here !== undefined && canonicalKeyOrder(before[1], here[1]) === 0) return err({ kind: "DuplicateKey", key: Preview.of(here[0]) });
    }
    return ok(new MapShape(KEY, Object.freeze(order)));
  }

  /**
   * The map of these keys to `values`, given in the keys' order (a missing one is null), with null values dropped:
   * O(n). `values` that is not an array, a value that is not a canonical value (see {@link isValue}), or a shape
   * {@link MapShape.of} did not make, is a `TypeMismatch`; a value that nests too deep for a map is `DepthExceeded`.
   */
  map(values: readonly Value[]): Result<MapValue, CanonicalError> {
    if (!MapShape.#isMade(this)) return err(expected("a map shape"));
    if (!isArrayGuarded(values)) return err(notAnArray);
    const entries: Entry[] = [];
    for (const [key, utf8, i] of this.#order) {
      const value: unknown = values[i];
      if (value === undefined) continue;
      if (!isValue(value)) return err(expected(`a canonical value at index ${i.toString()}`));
      if (value.kind !== "null") entries.push({ key, utf8, value });
    }
    return mapOfSorted(entries);
  }
}
Object.freeze(MapShape);
Object.freeze(MapShape.prototype);

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
Object.freeze(Proven);
Object.freeze(Proven.prototype);

/** Two containers whose parts are being compared, in order, and the next index. */
type Comparing =
  | { readonly map: false; readonly a: ArrayValue; readonly b: ArrayValue; at: number }
  | { readonly map: true; readonly a: MapValue; readonly b: MapValue; readonly x: () => Entry | undefined; readonly y: () => Entry | undefined; at: number };

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
      if (!proven.has(a, b)) stack.push({ map: true, a, b, x: cursorOf(a), y: cursorOf(b), at: 0 });
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
    // the lengths are equal, so both sides end together; a map's entries are walked only as far as the comparison goes
    if (top.map) {
      const [x, y] = [top.x(), top.y()];
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

type Showing = { readonly map: false; readonly items: readonly Value[]; at: number } | { readonly map: true; readonly next: () => Entry | undefined; readonly size: number; at: number };

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
      // a map's entries are read only as far as the budget goes, and never all listed for it
      stack.push(v.kind === "array" ? { map: false, items: v.items, at: 0 } : { map: true, next: cursorOf(v), size: v.size, at: 0 });
    }
  };
  show(value);
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const length = top.map ? top.size : top.items.length;
    const i = top.at++;
    if (i >= length) {
      out.push(top.map ? "}" : "]");
      stack.pop();
      continue;
    }
    if (i > 0) out.push(", ");
    const entry = top.map ? top.next() : undefined;
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
export const Value = Object.freeze({
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
} as const);
