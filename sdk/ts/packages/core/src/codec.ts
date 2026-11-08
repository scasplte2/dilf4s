/**
 * Both directions between typed data and the canonical data model, with the same shape checks: an encoder refuses
 * every value its decoder would reject, so a schema-described type never encodes to bytes outside its schema. This is
 * the runtime of the models generated from `shared/schemas` (`./generated`).
 *
 * A codec is a description ({@link CodecShape}): a leaf, which reads or writes one value whole, or a vector, text map,
 * record, union or deferred reference. One encoder and one decoder walk any codec iteratively, with an explicit stack
 * of the containers open (at most {@link MAX_DEPTH}), so no codec recurses on the native stack, however its schema nests
 * or refers to itself.
 */
import { bitLengthOf } from "./bigint.js";
import { plainBytes } from "./bytes.js";
import { decode as decodeCbor, encode as encodeCbor } from "./cbor.js";
import { decodeFailure as failure, type CanonicalError, type DecodeFailure, type TooLarge } from "./error.js";
import { isArrayGuarded, isRevokedProxy, readGuarded, REVOKED } from "./guard.js";
import { close as closeIterator, isArrayLength, NOT_A_RESULT, NOT_ITERABLE, open, readStep, type Reader } from "./iterator.js";
import { parse as parseJson } from "./json.js";
import { Preview } from "./preview.js";
import { search } from "./keyset.js";
import { err, ok, type Err, type Result } from "./result.js";
import { encodeUtf8, utf8LengthOf } from "./utf8.js";
import { arrayOfValues, BoolValue, BytesValue, canonicalKeyOrder, entriesView, IntValue, isValue, mapOfValues, MAX_DEPTH, type MapValue, MAX_WEIGHT, TextValue, type Entry, type Value } from "./value.js";

export type { Value } from "./value.js";
export { ok } from "./result.js";

/**
 * Encodes `T` to a canonical value and decodes it back. `depth` is the nesting level of the value being encoded (0 at
 * the root, which is the default): containers refuse to go past {@link MAX_DEPTH} before descending, so a deeply nested
 * model is a typed `DepthExceeded`, never a stack overflow. A depth that is not a safe integer from 0 (negative, a
 * fraction, `NaN`, infinite, past 2^53 - 1) is a `TypeMismatch`, before anything is read: the walk adds one level a
 * container and compares with the limit, which needs a whole number it can count exactly (a negative or `NaN` depth
 * would never reach the limit, and past 2^53 - 1 adding one is not exact).
 * A hand-made codec composing others passes on the depth it was given, plus one for each container it writes.
 *
 * Decoding walks the value as the schema reads it, so a value built with shared parts costs up to its weight, and every
 * byte string read is copied each time it occurs: decode a caller's value with {@link decodeWithin}. Neither direction
 * recurses: both walk {@link shape} with an explicit stack. A codec made otherwise than by this module's combinators
 * is called through its own `encode` and `decode` wherever it is composed, and recurses as it does.
 *
 * `encode` and `decode` are methods, so TypeScript compares a codec's type parameter bivariantly: a `Codec<"x">` is
 * accepted where a `Codec<string>` is expected. An explicit type argument to a combinator that is wider than its codec's
 * type (`required<string>(key, literalText("x"))`, `vector<string>(literalText("x"), ...)`) is therefore unchecked and
 * acts as a cast; a narrower one is refused, as `decode`'s result is still checked. Let inference pick it: the record
 * and union combinators then check each part against its property's type.
 *
 * A codec this module's combinators make is immutable at run time, as its `readonly` types say: the codec, its
 * {@link shape} and every array and object the shape holds are frozen when the combinator returns, and each combinator
 * copies the arrays and objects it is given (field lists, alternatives, key names, texts, enumeration cases) before
 * freezing, so changing them afterwards changes nothing. No caller, before or after a module that builds codecs
 * finishes loading, can change what such a codec accepts or produces. Freezing happens once, when the codec is built:
 * encoding and decoding do no work for it. What a combinator is given as code stays the caller's: a leaf's functions,
 * a deferred reference's function (the generated ones read an exported constant, which no importer can reassign) and a
 * codec made otherwise, which is called through its own methods.
 */
export interface Codec<T> {
  encode(value: T, depth?: number): Result<Value, CanonicalError>;
  decode(value: Value): Result<T, DecodeFailure>;
  /** How the codec is built: read by this module's encoder and decoder alone. */
  readonly shape: CodecShape;
}

/**
 * What a codec is, for the one encoder and the one decoder: a leaf reads or writes a value whole, never another codec;
 * the others are walked by them. Built only by this module's combinators, which keep each part's type, and frozen when
 * built, with every array and object it holds (see {@link Codec}).
 */
export type CodecShape =
  | { readonly kind: "leaf"; encode(value: unknown): Result<Value, CanonicalError>; decode(value: Value): Result<unknown, DecodeFailure> }
  | { readonly kind: "defer"; resolve(): Codec<unknown> }
  | { readonly kind: "vector"; readonly element: Codec<unknown>; readonly min: number; readonly max: number | null }
  | { readonly kind: "textMap"; readonly value: Codec<unknown> }
  | { readonly kind: "record"; readonly fields: readonly RecordField[]; readonly allowed: readonly string[] }
  | {
      readonly kind: "union";
      readonly name: string;
      readonly alternatives: readonly Alternative<unknown>[];
      /** Every field name of every alternative, which encoding reads to tell them apart. */
      readonly known: readonly string[];
      /** Each alternative's required and allowed wire keys, which decoding reads to tell them apart. */
      readonly keys: readonly (readonly [readonly string[], readonly string[]])[];
      readonly texts: readonly string[];
    };

/** One field of a record: its property, its wire key, its codec, and whether it may be absent. */
interface RecordField {
  readonly prop: string;
  readonly key: string;
  readonly codec: Codec<unknown>;
  readonly optional: boolean;
}

/**
 * The codecs this module's combinators made, whose `shape` is what they do. Any other object with a codec's methods
 * (one spread from a codec, its methods replaced, say) is called through its methods, so composing it keeps its checks.
 */
const made = new WeakSet<Codec<unknown>>();

/** A codec of `shape`, which the combinator built and holds alone, every array and object in it already frozen. */
function codecOf<T>(shape: CodecShape): Codec<T> {
  const codec: Codec<T> = Object.freeze({
    shape: Object.freeze(shape),
    encode: (value: T, depth = 0) =>
      Number.isSafeInteger(depth) && depth >= 0 ? encodeWith(codec, value, depth) : typeMismatch("the depth is not a safe integer from 0"),
    // the combinators that built `shape` keep its parts' types, so what it decodes is a `T`
    decode: (value: Value) => (isValue(value) ? (decodeWith(codec, value) as Result<T, DecodeFailure>) : notAValue()),
  });
  made.add(codec);
  return codec;
}

/** A codec that reads and writes a value whole, calling no other codec. */
export function leaf<T>(encode: (value: T) => Result<Value, CanonicalError>, decode: (value: Value) => Result<T, DecodeFailure>): Codec<T> {
  return codecOf({ kind: "leaf", encode, decode });
}

/** Why bytes or text are not a model: not canonical, or not the model's shape. */
export type ModelError = { readonly kind: "Canonical"; readonly error: CanonicalError } | { readonly kind: "Decode"; readonly error: DecodeFailure };

/**
 * `codec.decode(value)`, if `value` weighs at most `maxWeight`: a heavier value is refused in O(1), from its cached
 * weight, before any of it is read or copied. Decode a caller's value with this, never with `decode` alone. The budget
 * is first brought into the weight range `0..MAX_WEIGHT` (weights are at least 1 and saturate at `MAX_WEIGHT`, so no
 * outcome changes), so a refusal names a 64-bit budget, never a caller's integer of any size.
 */
export function decodeWithin<T>(codec: Codec<T>, value: Value, maxWeight: bigint): Result<T, DecodeFailure> {
  if (!isValue(value)) return notAValue();
  if (!isBigint(maxWeight)) return decodeFailure("the weight budget is not a bigint");
  const budget = maxWeight < 0n ? 0n : maxWeight > MAX_WEIGHT ? MAX_WEIGHT : maxWeight;
  return value.weight > budget ? err(failure(`the value weighs over ${budget.toString()}`)) : codec.decode(value);
}

/** The canonical CBOR of `value`, as `cbor.encode`: a model holding caller values (`any`) may share parts. */
export function toCbor<T>(codec: Codec<T>, value: T): Result<Uint8Array, CanonicalError | TooLarge> {
  const v = codec.encode(value);
  return v.ok ? encodeCbor(v.value) : v;
}

/** The typed value of canonical CBOR. */
export function fromCbor<T>(codec: Codec<T>, bytes: Uint8Array): Result<T, ModelError> {
  // cbor.decode refuses what is not bytes, and reads them through their own slots (plainBytes)
  const v = decodeCbor(bytes);
  if (!v.ok) return err({ kind: "Canonical", error: v.error });
  const d = codec.decode(v.value);
  return d.ok ? d : err({ kind: "Decode", error: d.error });
}

/** The typed value of JSON text (canonical-encoding.md section 3). */
export function fromJson<T>(codec: Codec<T>, text: string): Result<T, ModelError> {
  const v = parseJson(text);
  if (!v.ok) return err({ kind: "Canonical", error: v.error });
  const d = codec.decode(v.value);
  return d.ok ? d : err({ kind: "Decode", error: d.error });
}

/** A failed encoding: a value outside its schema. */
export function typeMismatch(reason: string): Err<CanonicalError> {
  return err({ kind: "TypeMismatch", reason });
}

/** A failed decoding at the value itself. */
export function decodeFailure(reason: string): Err<DecodeFailure> {
  return err(failure(reason));
}

const expected = (what: string, v: Value): Err<DecodeFailure> => decodeFailure(`expected ${what}, got ${v.kind}`);

// What a caller can pass past the types (a JavaScript caller, a cast, a missing field) is checked in O(1) before it is
// read, so it is refused as TypeMismatch rather than thrown on. These take `unknown`: the static type says it holds.
const isBigint = (x: unknown): x is bigint => typeof x === "bigint";
const isString = (x: unknown): x is string => typeof x === "string";
const notA = (what: string): Err<CanonicalError> => typeMismatch(`expected ${what}`);
const notAValue = (): Err<DecodeFailure> => decodeFailure("expected a canonical value");

/** Any canonical value, unchanged (CDDL `any`). */
export const any: Codec<Value> = leaf(
  (v) => (isValue(v) ? ok(v) : notA("a canonical value")),
  (v) => ok(v),
);

export const bool: Codec<boolean> = leaf(
  (b) => BoolValue.of(b),
  (v) => (v.kind === "bool" ? ok(v.value) : expected("bool", v)),
);

const show = (b: bigint | null): string => (b === null ? "" : b.toString());

/** A codec that refuses everything, both ways, for a schema built from bounds of the wrong type. */
const refusing = <T>(reason: string): Codec<T> => leaf<T>(() => typeMismatch(reason), () => decodeFailure(reason));

/** Whether `lo` and `hi` are size bounds: `lo` a number, `hi` a number or `null`, neither `NaN`. */
const sizeBounds = (lo: unknown, hi: unknown): boolean =>
  typeof lo === "number" && !Number.isNaN(lo) && (hi === null || (typeof hi === "number" && !Number.isNaN(hi)));
const NOT_SIZE_BOUNDS = "the schema's size bounds are not numbers";

/**
 * An integer in `lo..hi`, either side unbounded when `null`. Bounds that are not `bigint`s or `null` (from a JavaScript
 * caller, or a cast) make a codec that refuses every value, both ways, as `TypeMismatch` and a decode failure.
 */
export function intRange(lo: bigint | null, hi: bigint | null): Codec<bigint> {
  const bound = (b: unknown): boolean => b === null || typeof b === "bigint";
  if (!bound(lo) || !bound(hi)) return refusing("the schema's integer bounds are not bigints");
  const within = (n: bigint): boolean => (lo === null || n >= lo) && (hi === null || n <= hi);
  return leaf(
    (n) => {
      if (!isBigint(n)) return notA("a bigint");
      return within(n) ? IntValue.of(n) : typeMismatch(`integer of ${bitLengthOf(n).toString()} bits outside ${show(lo)}..${show(hi)}`);
    },
    (v) => {
      if (v.kind !== "int") return expected("int", v);
      return within(v.value) ? ok(v.value) : decodeFailure(`integer of ${bitLengthOf(v.value).toString()} bits outside ${show(lo)}..${show(hi)}`);
    },
  );
}

// CDDL integers follow RFC 8610: major type 0 (`uint`), major type 1 (`nint`), or either (`int`). A wider integer must
// be an explicit range in the schema.
/** CDDL `int`: -2^64..2^64-1. */
export const int: Codec<bigint> = intRange(-(2n ** 64n), 2n ** 64n - 1n);
/** CDDL `uint`: 0..2^64-1. */
export const uint: Codec<bigint> = intRange(0n, 2n ** 64n - 1n);
/** CDDL `nint`: -2^64..-1. */
export const nint: Codec<bigint> = intRange(-(2n ** 64n), -1n);

/**
 * A byte string of `lo` to `hi` bytes (`hi` unbounded when `null`). Bounds that are not numbers make a codec that
 * refuses every value, as for {@link intRange}.
 */
export function sizedBytes(lo: number, hi: number | null): Codec<Uint8Array> {
  if (!sizeBounds(lo, hi)) return refusing(NOT_SIZE_BOUNDS);
  const fits = (n: number): boolean => n >= lo && (hi === null || n <= hi);
  return leaf(
    (b) => {
      // the array's own length, never a subclass's `length`
      const view = plainBytes(b);
      if (view === undefined) return notA("a Uint8Array");
      return fits(view.length) ? BytesValue.of(view) : typeMismatch(`byte string of ${view.length.toString()} bytes outside the schema size`);
    },
    (v) => {
      if (v.kind !== "bytes") return expected("bytes", v);
      return fits(v.length) ? ok(v.bytes) : decodeFailure(`byte string of ${v.length.toString()} bytes, expected ${lo.toString()}..${hi === null ? "" : hi.toString()}`);
    },
  );
}

export const bytes: Codec<Uint8Array> = sizedBytes(0, null);

/**
 * Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes). Bounds that
 * are not numbers make a codec that refuses every value, as for {@link intRange}.
 */
export function sizedText(lo: number, hi: number | null): Codec<string> {
  if (!sizeBounds(lo, hi)) return refusing(NOT_SIZE_BOUNDS);
  const fits = (s: string): boolean => utf8LengthOf(s) >= lo && (hi === null || utf8LengthOf(s) <= hi);
  return leaf(
    (s) => {
      if (!isString(s)) return notA("a string");
      const t = TextValue.of(s);
      if (!t.ok) return t;
      return fits(s) ? t : typeMismatch(`text of ${utf8LengthOf(s).toString()} UTF-8 bytes outside the schema size`);
    },
    (v) => {
      if (v.kind !== "text") return expected("text", v);
      return fits(v.value) ? ok(v.value) : decodeFailure(`text of ${utf8LengthOf(v.value).toString()} UTF-8 bytes, expected ${lo.toString()}..${hi === null ? "" : hi.toString()}`);
    },
  );
}

export const text: Codec<string> = sizedText(0, null);

/**
 * The `.regexp` patterns the schemas use. The generator accepts only these, so every SDK checks them identically
 * without a regular-expression engine.
 */
export type TextPattern = "([0-9a-f]{2})*" | "0x([0-9a-f]{2})*" | "0|[1-9][0-9]*" | "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*";

const lowerHex = (s: string): boolean => s.length % 2 === 0 && /^[0-9a-f]*$/.test(s);

/** Whether the whole of `text` matches `pattern`. */
export function matches(pattern: TextPattern, text: string): boolean {
  switch (pattern) {
    case "([0-9a-f]{2})*":
      return lowerHex(text);
    case "0x([0-9a-f]{2})*":
      return text.startsWith("0x") && lowerHex(text.slice(2));
    case "0|[1-9][0-9]*":
      return /^(0|[1-9][0-9]*)$/.test(text);
    // a versioned-trie state key's grammar (versioned-trie.md section 2), without the length limits the domain checks
    case "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*":
      return /^[a-z0-9][a-z0-9._-]*(\/[a-z0-9][a-z0-9._-]*)*$/.test(text);
  }
}

/** Text matching a {@link TextPattern} (CDDL `text .regexp`). */
export function matching(pattern: TextPattern): Codec<string> {
  return leaf(
    (s) => {
      if (!isString(s)) return notA("a string");
      return matches(pattern, s) ? TextValue.of(s) : typeMismatch(`text does not match ${pattern}`);
    },
    (v) => {
      if (v.kind !== "text") return expected("text", v);
      return matches(pattern, v.value) ? ok(v.value) : decodeFailure(`${Preview.of(v.value).toString()} does not match ${pattern}`);
    },
  );
}

/** Exactly the text `literal`, as its literal type: any other value is refused, as decoding refuses it. */
export function literalText<S extends string>(literal: S): Codec<S> {
  return leaf(
    (s) => ((s as unknown) === literal ? TextValue.of(literal) : notA(JSON.stringify(literal))),
    (v) => {
      if (v.kind !== "text") return expected("text", v);
      return v.value === literal ? ok(literal) : decodeFailure(`expected ${JSON.stringify(literal)}, got ${Preview.of(v.value).toString()}`);
    },
  );
}

/** Exactly the integer `literal`, as its literal type: any other value is refused, as decoding refuses it. */
export function literalInt<N extends bigint>(literal: N): Codec<N> {
  return leaf(
    (n) => ((n as unknown) === literal ? IntValue.of(literal) : notA(literal.toString())),
    (v) => {
      if (v.kind !== "int") return expected("int", v);
      return v.value === literal ? ok(literal) : decodeFailure(`expected ${literal.toString()}, got an integer of ${bitLengthOf(v.value).toString()} bits`);
    },
  );
}

/** One of `texts`, for the text alternatives of a choice. */
export function oneOfText(t: string, texts: readonly string[]): Result<Value, CanonicalError> {
  if (!isString(t)) return notA("a string");
  return texts.includes(t) ? TextValue.of(t) : typeMismatch(`${Preview.of(t).toString()} is not an alternative`);
}

/** A labelled integer enumeration: each label is written as its integer. */
export function intEnum<L extends string>(name: string, cases: readonly (readonly [L, bigint])[]): Codec<L> {
  if (!isString(name)) return refusing(NOT_A_NAME);
  // a copy, frozen: the caller's array is read once, here, so changing it later changes nothing
  const copied = enumCases(cases);
  // the labels are the caller's, as given: the type says they are `L`s
  return copied === undefined ? refusing(NOT_CASES) : enumOf(name, copied as readonly (readonly [L, bigint])[]);
}

const NOT_CASES = "the schema's enumeration cases are not [label, bigint] pairs";

/** `x` as a frozen copy of `[label, integer]` pairs, read by index as {@link stringList} reads, or `undefined`. */
function enumCases(x: unknown): readonly (readonly [string, bigint])[] | undefined {
  const length: unknown = isArrayGuarded(x) ? (x as readonly unknown[]).length : undefined;
  if (!isArrayLength(length)) return undefined;
  const out: (readonly [string, bigint])[] = [];
  for (let i = 0; i < length; i++) {
    const pair: unknown = readGuarded(x as readonly unknown[], i);
    const label: unknown = isArrayGuarded(pair) ? readGuarded(pair, 0) : undefined;
    const wire: unknown = isArrayGuarded(pair) ? readGuarded(pair, 1) : undefined;
    if (!isString(label) || !isBigint(wire)) return undefined;
    out.push(Object.freeze([label, wire] as const));
  }
  return Object.freeze(out);
}

/** {@link intEnum} of a name and frozen cases this module holds. */
function enumOf<L extends string>(name: string, cases: readonly (readonly [L, bigint])[]): Codec<L> {
  return leaf(
    (label) => {
      if (!isString(label)) return notA(`a ${name} label`);
      const c = cases.find(([l]) => l === label);
      return c === undefined ? typeMismatch(`${Preview.of(label).toString()} is not a ${name}`) : IntValue.of(c[1]);
    },
    (v) => {
      if (v.kind !== "int") return expected("int", v);
      const c = cases.find(([, w]) => w === v.value);
      return c === undefined ? decodeFailure(`an integer of ${bitLengthOf(v.value).toString()} bits is not a ${name}`) : ok(c[0]);
    },
  );
}

// ------------------------------------------------------------------------------------------------ containers

/**
 * A codec referenced by name and resolved on use, so rules may refer to each other in any order, recursively. An
 * explicit wider `T` is unchecked and acts as a cast (see {@link Codec}): let inference pick it.
 */
export function defer<T>(codec: () => Codec<T>): Codec<T> {
  return codecOf({ kind: "defer", resolve: codec });
}

/**
 * A homogeneous array of `min` to `max` elements (`max` unbounded when `null`). An explicit wider `T` is unchecked and acts
 * as a cast (see {@link Codec}): let inference pick it. Bounds that are not numbers make a codec that refuses every
 * value, as for {@link intRange}.
 */
export function vector<T>(element: Codec<T>, min: number, max: number | null): Codec<readonly T[]> {
  if (!sizeBounds(min, max)) return refusing(NOT_SIZE_BOUNDS);
  return codecOf({ kind: "vector", element, min, max });
}

/**
 * A map with arbitrary text keys and values of one type (CDDL `{ * text => T }`), decoded in canonical key order. An
 * explicit wider `T` is unchecked and acts as a cast (see {@link Codec}): let inference pick it.
 */
export function textMap<T>(value: Codec<T>): Codec<ReadonlyMap<string, T>> {
  return codecOf({ kind: "textMap", value });
}

/**
 * A decoded text-keyed map: its entries in canonical key order, found by binary search. Its keys are the encoder's, so
 * they are compared, never hashed (a `Map` would hash them: see keyset.ts).
 */
class TextMap<T> implements ReadonlyMap<string, T> {
  readonly #entries: readonly (readonly [string, Uint8Array, T])[];
  constructor(entries: readonly (readonly [string, Uint8Array, T])[]) {
    this.#entries = entries;
    Object.freeze(this);
  }
  get size(): number {
    return this.#entries.length;
  }
  get(key: string): T | undefined {
    return this.#entries[this.#find(key)]?.[2];
  }
  has(key: string): boolean {
    return this.#find(key) >= 0;
  }
  forEach(f: (value: T, key: string, map: ReadonlyMap<string, T>) => void, thisArg?: unknown): void {
    for (const [k, , v] of this.#entries) f.call(thisArg, v, k, this);
  }
  entries(): MapIterator<[string, T]> {
    return this.#entries.map(([k, , v]): [string, T] => [k, v]).values();
  }
  keys(): MapIterator<string> {
    return this.#entries.map(([k]) => k).values();
  }
  values(): MapIterator<T> {
    return this.#entries.map(([, , v]) => v).values();
  }
  [Symbol.iterator](): MapIterator<[string, T]> {
    return this.entries();
  }
  #find(key: string): number {
    return search(this.#entries, (e) => e[1], encodeUtf8(key), canonicalKeyOrder);
  }
}

// ------------------------------------------------------------------------------------------------ records and unions

/**
 * A field present in every value of its record, written as `key`. The type is the field's exactly, in both
 * directions (`of`, never set, makes it invariant), so a record's fields are checked against its type. `V` is the
 * codec's own type when inferred: an explicit wider `V` (`required<string>(key, literalText("x"))`) is accepted, as
 * `Codec`'s methods are bivariant, and works as a cast does.
 */
export interface RequiredField<V> {
  readonly key: string;
  readonly codec: Codec<V>;
  readonly optional: false;
  readonly of?: (v: V) => V;
}

/** A field that may be absent: `undefined` is left out when encoding, never written as null. */
export interface OptionalField<V> {
  readonly key: string;
  readonly codec: Codec<V>;
  readonly optional: true;
  readonly of?: (v: V) => V;
}

/**
 * A field of a record that every value has. Let inference pick `V`, the codec's own type, which {@link record} then
 * checks against the property's type exactly. An explicit wider `V` is unchecked and acts as a cast:
 * `required<string>(key, literalText("x"))` compiles, and the literal codec then refuses any other value when encoding.
 */
export function required<V>(key: string, codec: Codec<V>): RequiredField<V> {
  return Object.freeze({ key, codec, optional: false });
}

/** An optional field of a record. As with {@link required}, an explicit wider `V` is unchecked and acts as a cast. */
export function optional<V>(key: string, codec: Codec<V>): OptionalField<V> {
  return Object.freeze({ key, codec, optional: true });
}

/** Every property of `T`, each with the field that reads and writes it: an optional property has an optional field. */
export type RecordFields<T> = { readonly [K in keyof Required<T>]: undefined extends T[K] ? OptionalField<Exclude<T[K], undefined>> : RequiredField<T[K]> };

/**
 * A map with the keys of `fields` and no others, read and written in the order `fields` lists them (an object's own
 * keys keep their order unless they are array indices, which no generated field name is): when encoding, the first
 * field refused is the refusal, and no field after it is read; when decoding, unknown keys are refused first, then the
 * first field missing or refused, under its key.
 */
export function record<T>(fields: RecordFields<T>): Codec<T> {
  // a copy, frozen: the caller's object and fields are read once, here, so changing them later changes nothing
  const list = fieldList(fields);
  return list === undefined ? refusing(NOT_FIELDS) : codecOf({ kind: "record", fields: list, allowed: Object.freeze(list.map((f) => f.key)) });
}

const NOT_FIELDS = "the schema's fields are not an object of record fields";

/**
 * `x`'s own enumerable properties, in their order, as frozen record fields (each read once: its key a string, its
 * `optional` a boolean), or `undefined` if `x` is not an object of such fields.
 */
function fieldList(x: unknown): readonly RecordField[] | undefined {
  if (typeof x !== "object" || x === null || isRevokedProxy(x)) return undefined;
  const out: RecordField[] = [];
  for (const prop of Object.keys(x)) {
    // guarded: an earlier field's read (a getter, a Proxy's trap) may have revoked the object
    const f: unknown = readGuarded(x, prop);
    if (typeof f !== "object" || f === null || isRevokedProxy(f)) return undefined;
    const key: unknown = readGuarded(f, "key");
    const codec: unknown = readGuarded(f, "codec");
    const optional: unknown = readGuarded(f, "optional");
    if (!isString(key) || codec === REVOKED || typeof optional !== "boolean") return undefined;
    // the field's codec is the caller's, as given: the type says it is one
    out.push(Object.freeze({ prop, key, codec: codec as Codec<unknown>, optional }));
  }
  return Object.freeze(out);
}

/**
 * The keys of a map that may be read as a record allowing `allowed`, which must hold all of them. `allowed` that is not
 * an array of strings (read by index, its length once) is a decode failure, before the map is read.
 */
export function fields(v: Value, allowed: readonly string[]): Result<MapValue, DecodeFailure> {
  if (!isValue(v)) return notAValue();
  const names = stringList(allowed);
  return names === undefined ? decodeFailure(NOT_NAMES) : fieldsOf(v, names);
}

/** {@link fields} of a value and names this package holds (a decode walk's, a schema's), unchecked. */
function fieldsOf(v: Value, allowed: readonly string[]): Result<MapValue, DecodeFailure> {
  if (v.kind !== "map") return expected("map", v);
  // the keys come in canonical order: the refusal counts the unknown ones and previews the first, never lists them
  const unknown = v.keys().filter((k) => !allowed.includes(k));
  const first = unknown[0];
  if (first !== undefined) return decodeFailure(`${unknown.length.toString()} unknown ${unknown.length === 1 ? "key" : "keys"}, the first ${Preview.of(first).toString()}`);
  return ok(v);
}

/** The refusal of a schema argument (key names, alternatives) that is not of its type. */
const NOT_NAMES = "the schema's names are not arrays of strings";
const NOT_A_NAME = "the schema's name is not a string";

/**
 * `x` as an array of strings, copied: read by index, its length once and each item once (never through its iterator
 * or its own methods), or `undefined` if it is not an array of strings.
 */
function stringList(x: unknown): readonly string[] | undefined {
  const length: unknown = isArrayGuarded(x) ? (x as readonly unknown[]).length : undefined;
  if (!isArrayLength(length)) return undefined;
  const out: string[] = [];
  for (let i = 0; i < length; i++) {
    // guarded: an earlier item's read (a Proxy's trap) may have revoked the array; REVOKED is no string
    const s: unknown = readGuarded(x as readonly unknown[], i);
    if (typeof s !== "string") return undefined;
    out.push(s);
  }
  return out;
}

/** `x` as an array of `[required, allowed]` pairs of string arrays, copied as {@link stringList} copies, or `undefined`. */
function recordList(x: unknown): readonly (readonly [readonly string[], readonly string[]])[] | undefined {
  const length: unknown = isArrayGuarded(x) ? (x as readonly unknown[]).length : undefined;
  if (!isArrayLength(length)) return undefined;
  const out: (readonly [readonly string[], readonly string[]])[] = [];
  for (let i = 0; i < length; i++) {
    const pair: unknown = readGuarded(x as readonly unknown[], i);
    const required = isArrayGuarded(pair) ? stringList(readGuarded(pair, 0)) : undefined;
    const allowed = isArrayGuarded(pair) ? stringList(readGuarded(pair, 1)) : undefined;
    if (required === undefined || allowed === undefined) return undefined;
    out.push([required, allowed]);
  }
  return out;
}

/** A record alternative of a union, `codec`, told apart by its field names when encoding and its wire keys when decoding. */
export interface Alternative<A> {
  readonly codec: Codec<A>;
  readonly required: readonly string[];
  readonly allowed: readonly string[];
  readonly keys: readonly [readonly string[], readonly string[]];
}

/**
 * The alternative `codec`, with its required and allowed field names, then its required and allowed wire keys. An
 * explicit wider `A` is unchecked and acts as a cast (see {@link Codec}); {@link union} checks that `A` is one of its members.
 */
export function alternative<A>(codec: Codec<A>, names: readonly [readonly string[], readonly string[]], keys: readonly [readonly string[], readonly string[]]): Alternative<A> {
  // copies, frozen; names or keys that are not arrays of strings are left absent (past the types), for union to refuse
  const named = namePair(names);
  const keyed = namePair(keys);
  const absent = undefined as never;
  return Object.freeze({ codec, required: named?.[0] ?? absent, allowed: named?.[1] ?? absent, keys: keyed ?? absent });
}

/** `x` as a frozen copy of an array of strings ({@link stringList}), or `undefined`. */
function frozenStrings(x: unknown): readonly string[] | undefined {
  const list = stringList(x);
  return list === undefined ? undefined : Object.freeze(list);
}

/** `x` as a frozen `[required, allowed]` pair of frozen string arrays, or `undefined`. */
function namePair(x: unknown): readonly [readonly string[], readonly string[]] | undefined {
  if (!isArrayGuarded(x)) return undefined;
  const required = frozenStrings(readGuarded(x, 0));
  const allowed = frozenStrings(readGuarded(x, 1));
  return required === undefined || allowed === undefined ? undefined : Object.freeze([required, allowed] as const);
}

/**
 * `x` as a frozen copy of union alternatives, each read once and copied as {@link alternative} copies (whether or not
 * it made them), or `undefined` if `x` is not an array of alternatives with string arrays for names.
 */
function alternativeList(x: unknown): readonly Alternative<unknown>[] | undefined {
  const length: unknown = isArrayGuarded(x) ? (x as readonly unknown[]).length : undefined;
  if (!isArrayLength(length)) return undefined;
  const out: Alternative<unknown>[] = [];
  for (let i = 0; i < length; i++) {
    const a: unknown = readGuarded(x as readonly unknown[], i);
    if (typeof a !== "object" || a === null || isRevokedProxy(a)) return undefined;
    const codec: unknown = readGuarded(a, "codec");
    const required = frozenStrings(readGuarded(a, "required"));
    const allowed = frozenStrings(readGuarded(a, "allowed"));
    const keys = namePair(readGuarded(a, "keys"));
    if (codec === REVOKED || required === undefined || allowed === undefined || keys === undefined) return undefined;
    // the alternative's codec is the caller's, as given: the type says it is one
    out.push(Object.freeze({ codec: codec as Codec<unknown>, required, allowed, keys }));
  }
  return Object.freeze(out);
}

/**
 * A choice of record alternatives and text alternatives (`texts`). Encoding takes a text as a text alternative, and an
 * object as the first record alternative it fits ({@link hasFields}); decoding takes the alternative
 * {@link selectAlternative} picks. The generator only accepts unions where at most one alternative fits. `U` is the
 * union's type, given explicitly by the generated models; each alternative's codec must decode to a member of it.
 */
export function union<U>(name: string, alternatives: readonly Alternative<U>[], texts: readonly Extract<U, string>[]): Codec<U> {
  if (!isString(name)) return refusing(NOT_A_NAME);
  // copies, frozen: the caller's arrays and alternatives are read once, here, so changing them later changes nothing
  const alts = alternativeList(alternatives);
  const own = frozenStrings(texts);
  if (alts === undefined || own === undefined) return refusing(NOT_NAMES);
  // read in UTF-16 order, once each: a getter may observe the order the fields are read in
  const names = alts.flatMap((a) => a.allowed);
  const known = Object.freeze(names.filter((k, i) => names.indexOf(k) === i).sort((a, b) => (a < b ? -1 : a > b ? 1 : 0)));
  return codecOf({ kind: "union", name, alternatives: alts, known, keys: Object.freeze(alts.map((a) => a.keys)), texts: own });
}

/**
 * Whether a record value `a` of a union has the fields of an alternative, by which of the union's field names
 * (`known`) it has a value for: every required field present and none outside the allowed ones. Fields are read as the
 * record encoder reads them, so getters count and unrelated properties do not.
 */
export function hasFields(a: object, required: readonly string[], allowed: readonly string[], known: readonly string[]): boolean {
  // the schema's names, as arrays of strings read by index: anything else fits nothing
  const [r, al, k] = [stringList(required), stringList(allowed), stringList(known)];
  return r !== undefined && al !== undefined && k !== undefined && hasFieldsOf(a, r, al, k);
}

/** {@link hasFields} with a schema this package holds, unchecked. */
function hasFieldsOf(a: object, required: readonly string[], allowed: readonly string[], known: readonly string[]): boolean {
  const x: unknown = a;
  if (((typeof x !== "object" || x === null) && typeof x !== "function") || isRevokedProxy(x)) return false;
  const present = known.filter((k) => readGuarded(a, k) !== undefined);
  // a record a getter revoked while its fields were read has no fields to fit
  return !isRevokedProxy(a) && required.every((k) => present.includes(k)) && present.every((k) => allowed.includes(k));
}

/**
 * The index of the alternative of a choice that `v` fits: the first record (given as its required and allowed wire
 * keys) that fits a map, then the text alternatives, numbered after the records.
 */
export function selectAlternative(v: Value, records: readonly (readonly [readonly string[], readonly string[]])[], texts: readonly string[]): Result<number, DecodeFailure> {
  if (!isValue(v)) return notAValue();
  const alternatives = recordList(records);
  const names = stringList(texts);
  return alternatives === undefined || names === undefined ? decodeFailure(NOT_NAMES) : selectAlternativeOf(v, alternatives, names);
}

/** {@link selectAlternative} of a value and schema this package holds, unchecked. */
function selectAlternativeOf(v: Value, alternatives: readonly (readonly [readonly string[], readonly string[]])[], names: readonly string[]): Result<number, DecodeFailure> {
  if (v.kind === "map") {
    const keys = v.keys();
    const i = alternatives.findIndex(([required, allowed]) => required.every((k) => keys.includes(k)) && keys.every((k) => allowed.includes(k)));
    return i >= 0 ? ok(i) : decodeFailure(`no alternative fits a map of ${keys.length.toString()} keys`);
  }
  if (v.kind === "text") {
    const i = names.indexOf(v.value);
    return i >= 0 ? ok(alternatives.length + i) : decodeFailure(`${Preview.of(v.value).toString()} is not an alternative`);
  }
  return decodeFailure("expected a map or one of the text alternatives");
}

/** The nesting limit was reached before encoding a container. */
export function depthExceeded(): Err<CanonicalError> {
  return err({ kind: "DepthExceeded", limit: MAX_DEPTH });
}

// ------------------------------------------------------------------------------------------------ the walks

/** A leaf, or a container: what references and unions stand for. */
type Reading = Exclude<CodecShape, { readonly kind: "defer" | "union" }>;

/** A reference or a union. */
type Choosing = Extract<CodecShape, { readonly kind: "defer" | "union" }>;

/**
 * What `codec` stands for, for `value`: references and unions followed (`choose` takes one step, to a codec, or to an
 * outcome) until a leaf, a container, or a codec this module did not make (`foreign`, called through its methods). A
 * step back to a shape already passed reads nothing new and would never end, so it is refused as `circular`. Each step
 * reads only the value's kind and keys, so this is O(the schema's chain).
 */
function follow<V, D>(
  codec: Codec<unknown>,
  value: V,
  choose: (s: Choosing, v: V) => { readonly to: Codec<unknown> } | { readonly done: D },
  circular: D,
): { readonly reading: Reading } | { readonly foreign: Codec<unknown> } | { readonly done: D } {
  // by identity, in O(1) a step: the shapes are the schema's, never caller data
  let passed: Set<CodecShape> | undefined;
  for (let c = codec; ; ) {
    if (!made.has(c)) return { foreign: c };
    const s = c.shape;
    if (s.kind !== "defer" && s.kind !== "union") return { reading: s };
    passed ??= new Set();
    if (passed.has(s)) return { done: circular };
    passed.add(s);
    const next = choose(s, value);
    if ("done" in next) return next;
    c = next.to;
  }
}

/**
 * Closes the iterators of the maps still open, from the frame at `i` outwards, as leaving `for...of` loops
 * early does: an outer one is closed even when an inner one's `return` throws. One frame a level, and a depth is a
 * safe integer from 0, so the recursion is at most {@link MAX_DEPTH} deep.
 *
 * Unlike `for...of`, the error that surfaces is the last one thrown: a `return` that throws replaces any error already
 * propagating (a caller's, or an inner `return`'s), where `for...of` keeps the first and drops the rest. Keeping the
 * first would need a catch clause, which this SDK does not use.
 */
function close(stack: readonly Encoding[], i: number = stack.length - 1): void {
  const frame = stack[i];
  if (frame === undefined) return;
  try {
    if (frame.kind === "textMap") closeIterator(frame.entries);
  } finally {
    close(stack, i - 1);
  }
}

/** A part still to encode, at `depth`, or the outcome of the part just encoded. */
type EncodeStep = { readonly codec: Codec<unknown>; readonly value: unknown; readonly depth: number; readonly done?: undefined } | { readonly done: Result<Value, CanonicalError> };

/**
 * A container being encoded: its parts, the depth of its parts, and what they encoded to. A vector's items are read by
 * index (`at`, the next one; its length read once, when it is opened), as `ArrayValue.of` reads them; a map's entries
 * from its own iterator, one at a time, after the one before is encoded (its key and value as each entry is read), as
 * a `for...of` reads them; a record's fields by index (`at`, the one being encoded).
 */
type Encoding =
  | { readonly kind: "vector"; readonly items: readonly unknown[]; readonly length: number; at: number; readonly element: Codec<unknown>; readonly depth: number; readonly out: Value[] }
  | { readonly kind: "textMap"; readonly entries: Reader<unknown>; key: string; readonly value: Codec<unknown>; readonly depth: number; readonly out: (readonly [string, Value])[] }
  | { readonly kind: "record"; readonly fields: readonly RecordField[]; readonly of: object; readonly depth: number; at: number; readonly out: (readonly [string, Value])[] };

/** The encoder: one loop, an explicit stack of the containers open, and nothing recursive. */
function encodeWith(root: Codec<unknown>, value: unknown, depth: number): Result<Value, CanonicalError> {
  const stack: Encoding[] = [];
  // the iterators still open when the walk ends, by a refusal or by caller code (a getter, an iterator, a hand-made
  // codec) throwing, are closed: a finished container's frame is already off the stack
  try {
    for (let step = openEncoding(root, value, depth, stack); ; ) {
      if (step.done === undefined) {
        step = openEncoding(step.codec, step.value, step.depth, stack);
        continue;
      }
      const top = stack.at(-1);
      if (!step.done.ok) return step.done;
      // a hand-made codec is caller code, and may give what is not a value: refused here, where its place is known, so
      // every part a container is built from is a value, and the containers are built without checking them again
      if (!isValue(step.done.value)) return typeMismatch(`a codec gave a non-value ${top === undefined ? "at the root" : place(top)}`);
      if (top === undefined) return step.done;
      step = nextEncoding(top, step.done.value, stack);
    }
  } finally {
    close(stack);
  }
}

/** Starts encoding `value` with `codec`: a leaf's outcome, or a container now open and its first part. */
function openEncoding(codec: Codec<unknown>, value: unknown, depth: number, stack: Encoding[]): EncodeStep {
  const followed = follow(
    codec,
    value,
    (s, v): { readonly to: Codec<unknown> } | { readonly done: Result<Value, CanonicalError> } => {
      if (s.kind === "defer") return { to: s.resolve() };
      if (typeof v === "string" && s.texts.length > 0) return { done: oneOfText(v, s.texts) };
      const fit = typeof v === "object" && v !== null ? s.alternatives.find((a) => hasFieldsOf(v, a.required, a.allowed, s.known)) : undefined;
      return fit === undefined ? { done: typeMismatch(`no alternative of ${s.name} fits`) } : { to: fit.codec };
    },
    typeMismatch("the codec refers to itself without writing a container"),
  );
  if ("done" in followed) return followed;
  if ("foreign" in followed) return { done: followed.foreign.encode(value, depth) };
  const s = followed.reading;
  switch (s.kind) {
    case "leaf":
      return { done: s.encode(value) };
    case "vector": {
      if (depth >= MAX_DEPTH) return { done: depthExceeded() };
      // read as ArrayValue.of reads one: the length once (a Proxy's need not be a number), then each item by index,
      // never through the array's iterator, so the size checked is the size written
      const length: unknown = isArrayGuarded(value) ? (value as readonly unknown[]).length : undefined;
      if (!isArrayLength(length)) return { done: typeMismatch("expected an array") };
      if (length < s.min || (s.max !== null && length > s.max)) return { done: typeMismatch(`array of ${length.toString()} elements outside the schema size`) };
      return opened({ kind: "vector", items: value as readonly unknown[], length, at: 0, element: s.element, depth: depth + 1, out: [] }, stack);
    }
    case "textMap": {
      if (depth >= MAX_DEPTH) return { done: depthExceeded() };
      // its iterator method is read once, by open (an `in` check first would run a Proxy's has trap and read it twice)
      if (typeof value !== "object" || value === null || isRevokedProxy(value)) return { done: typeMismatch("expected a map") };
      const entries = open<unknown>(value);
      if (entries === undefined) return { done: typeMismatch(NOT_ITERABLE) };
      return opened({ kind: "textMap", entries, key: "", value: s.value, depth: depth + 1, out: [] }, stack);
    }
    case "record": {
      if (depth >= MAX_DEPTH) return { done: depthExceeded() };
      if (typeof value !== "object" || value === null || isRevokedProxy(value)) return { done: typeMismatch("expected a record") };
      return opened({ kind: "record", fields: s.fields, of: value, depth: depth + 1, at: 0, out: [] }, stack);
    }
  }
}

/** A container just opened: its first part, or its outcome when it has none. */
function opened(frame: Encoding, stack: Encoding[]): EncodeStep {
  stack.push(frame);
  return frame.kind === "record" ? nextField(frame, stack) : nextPart(frame, stack);
}

/** Keeps the part just encoded, then gives the next one, or the container's outcome. */
function nextEncoding(top: Encoding, v: Value, stack: Encoding[]): EncodeStep {
  switch (top.kind) {
    case "vector":
      top.out.push(v);
      break;
    case "textMap":
      top.out.push([top.key, v]);
      break;
    case "record":
      top.out.push([top.fields[top.at]?.key ?? "", v]);
      top.at++;
      return nextField(top, stack);
  }
  return nextPart(top, stack);
}

/** Where in an open container the part being encoded goes, for a refusal: a field by its schema key, else its kind. */
function place(top: Encoding): string {
  switch (top.kind) {
    case "vector":
      return "for an element of an array";
    case "textMap":
      return "for a value of a text map";
    case "record":
      return `for the field ${JSON.stringify(top.fields[top.at]?.key ?? "")}`;
  }
}

/** The next element of an open vector, read by index, or the next entry of an open map, read from its iterator. */
function nextPart(top: Encoding & { readonly kind: "vector" | "textMap" }, stack: Encoding[]): EncodeStep {
  // off the stack while its own next() runs and its result is read (done, then value), as for...of does not close an
  // iterator for those; back on it for anything after, a map entry's reading included, which for...of does close it
  // for. Any truthy `done` ends it. A result that is no object, which for...of throws a TypeError for without closing
  // that iterator, is refused (the iterators around it are closed, as that TypeError would close them)
  if (top.kind === "vector") {
    if (top.at >= top.length) {
      stack.pop();
      return { done: arrayOfValues(top.out) };
    }
    // guarded: encoding an earlier element (a getter, a Proxy's trap) may have revoked the array
    const value: unknown = readGuarded(top.items, top.at++);
    return value === REVOKED ? { done: notA("an array") } : { codec: top.element, value, depth: top.depth };
  }
  stack.pop();
  const entry = readStep(top.entries.next());
  if (entry === undefined) return { done: typeMismatch(NOT_A_RESULT) };
  if (entry.done) return { done: mapOfValues(top.out) };
  const pair: unknown = entry.value;
  stack.push(top);
  // read by index, never destructured: an entry that is not an array, which destructuring throws a TypeError for, is
  // refused (and its iterator closed, as that TypeError would close it)
  if (!isArrayGuarded(pair)) return { done: notA("a map entry (a [key, value] array)") };
  // guarded: the key's read (a Proxy's trap) may have revoked the entry
  const key: unknown = readGuarded(pair, 0);
  const x: unknown = readGuarded(pair, 1);
  if (!isString(key)) return { done: notA("a text map key (a string)") };
  if (x === REVOKED) return { done: notA("a map entry (a [key, value] array)") };
  top.key = key;
  return { codec: top.value, value: x, depth: top.depth };
}

/** The next field of an open record with a value (an absent optional field is left out), or the record's map. */
function nextField(top: Encoding & { readonly kind: "record" }, stack: Encoding[]): EncodeStep {
  for (let f = top.fields[top.at]; f !== undefined; f = top.fields[++top.at]) {
    // guarded: an earlier field's read (a getter, a Proxy's trap) may have revoked the record
    const x: unknown = readGuarded(top.of, f.prop);
    if (x === REVOKED) return { done: notA("a record") };
    if (x !== undefined || !f.optional) return { codec: f.codec, value: x, depth: top.depth };
  }
  stack.pop();
  return { done: mapOfValues(top.out) };
}

/** A part still to decode, or the outcome of the part just decoded. */
type DecodeStep = { readonly codec: Codec<unknown>; readonly value: Value; readonly done?: undefined } | { readonly done: Result<unknown, DecodeFailure> };

/** A container being decoded: its parts, the one being decoded (`at`), and what they decoded to. */
type Decoding =
  | { readonly kind: "vector"; readonly items: readonly Value[]; readonly element: Codec<unknown>; at: number; readonly out: unknown[] }
  | { readonly kind: "textMap"; readonly entries: readonly Entry[]; readonly value: Codec<unknown>; at: number; readonly out: (readonly [string, Uint8Array, unknown])[] }
  | { readonly kind: "record"; readonly fields: readonly RecordField[]; readonly map: MapValue; at: number; readonly out: (readonly [string, unknown])[] };

/** The decoder: one loop, an explicit stack of the containers open, and nothing recursive. */
function decodeWith(root: Codec<unknown>, value: Value): Result<unknown, DecodeFailure> {
  const stack: Decoding[] = [];
  for (let step = openDecoding(root, value, stack); ; ) {
    if (step.done === undefined) {
      step = openDecoding(step.codec, step.value, stack);
      continue;
    }
    const top = stack.at(-1);
    if (top === undefined) return step.done;
    // a refusal is placed under the parts open, outermost first: its path is built once
    if (!step.done.ok) return err({ path: [...stack.map((d) => Preview.of(segment(d))), ...step.done.error.path], reason: step.done.error.reason });
    step = nextDecoding(top, step.done.value, stack);
  }
}

/** Where the part being decoded sits in its container: `[i]`, or a key. */
function segment(d: Decoding): string {
  switch (d.kind) {
    case "vector":
      return `[${d.at.toString()}]`;
    case "textMap":
      return d.entries[d.at]?.key ?? "";
    case "record":
      return d.fields[d.at]?.key ?? "";
  }
}

/** Starts decoding `value` with `codec`: a leaf's outcome, or a container now open and its first part. */
function openDecoding(codec: Codec<unknown>, value: Value, stack: Decoding[]): DecodeStep {
  const followed = follow(
    codec,
    value,
    (s, v): { readonly to: Codec<unknown> } | { readonly done: Result<unknown, DecodeFailure> } => {
      if (s.kind === "defer") return { to: s.resolve() };
      const i = selectAlternativeOf(v, s.keys, s.texts);
      if (!i.ok) return { done: i };
      const alt = s.alternatives[i.value];
      return alt === undefined ? { done: ok(s.texts[i.value - s.alternatives.length]) } : { to: alt.codec };
    },
    decodeFailure("the codec refers to itself without reading a container"),
  );
  if ("done" in followed) return followed;
  if ("foreign" in followed) return { done: followed.foreign.decode(value) };
  const s = followed.reading;
  switch (s.kind) {
    case "leaf":
      return { done: s.decode(value) };
    case "vector": {
      if (value.kind !== "array") return { done: expected("array", value) };
      const n = value.items.length;
      if (n < s.min || (s.max !== null && n > s.max)) return { done: decodeFailure(`array of ${n.toString()} elements, expected ${s.min.toString()}..${s.max === null ? "" : s.max.toString()}`) };
      const frame: Decoding & { readonly kind: "vector" } = { kind: "vector", items: value.items, element: s.element, at: 0, out: [] };
      stack.push(frame);
      return nextItem(frame, stack);
    }
    case "textMap": {
      if (value.kind !== "map") return { done: expected("map", value) };
      const frame: Decoding & { readonly kind: "textMap" } = { kind: "textMap", entries: entriesView(value), value: s.value, at: 0, out: [] };
      stack.push(frame);
      return nextItem(frame, stack);
    }
    case "record": {
      const m = fieldsOf(value, s.allowed);
      if (!m.ok) return { done: m };
      const frame: Decoding & { readonly kind: "record" } = { kind: "record", fields: s.fields, map: m.value, at: 0, out: [] };
      stack.push(frame);
      return nextMember(frame, stack);
    }
  }
}

/** Keeps the part just decoded, then gives the next one, or the container's outcome. */
function nextDecoding(top: Decoding, v: unknown, stack: Decoding[]): DecodeStep {
  switch (top.kind) {
    case "vector":
      top.out.push(v);
      break;
    case "textMap": {
      const e = top.entries[top.at];
      if (e !== undefined) top.out.push([e.key, e.utf8, v]);
      break;
    }
    case "record":
      top.out.push([top.fields[top.at]?.prop ?? "", v]);
      top.at++;
      return nextMember(top, stack);
  }
  top.at++;
  return nextItem(top, stack);
}

/** The next element or entry of an open vector or map, or its outcome. */
function nextItem(top: Decoding & { readonly kind: "vector" | "textMap" }, stack: Decoding[]): DecodeStep {
  if (top.kind === "vector") {
    const item = top.items[top.at];
    if (item !== undefined) return { codec: top.element, value: item };
    stack.pop();
    return { done: ok(top.out) };
  }
  const e = top.entries[top.at];
  if (e !== undefined) return { codec: top.value, value: e.value };
  stack.pop();
  return { done: ok(new TextMap(top.out)) };
}

/**
 * The next field of an open record that is present, or its outcome: an absent optional field is read as `undefined`,
 * a missing required one is refused (at the record, under no key of its own), and the last gives the record.
 */
function nextMember(top: Decoding & { readonly kind: "record" }, stack: Decoding[]): DecodeStep {
  for (let f = top.fields[top.at]; f !== undefined; f = top.fields[++top.at]) {
    const x = top.map.get(f.key);
    if (x !== undefined) return { codec: f.codec, value: x };
    if (!f.optional) {
      stack.pop();
      return { done: decodeFailure(`missing required key ${JSON.stringify(f.key)}`) };
    }
    top.out.push([f.prop, undefined]);
  }
  stack.pop();
  // fromEntries defines each property as data, so no field name (not even `__proto__`) reaches the prototype
  return { done: ok(Object.fromEntries(top.out)) };
}
