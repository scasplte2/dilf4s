/**
 * Both directions between typed data and the canonical data model, with the same shape checks: an encoder refuses
 * every value its decoder would reject, so a schema-described type never encodes to bytes outside its schema. This is
 * the runtime of the models generated from `shared/schemas` (`./generated`).
 */
import { decode as decodeCbor, encode as encodeCbor } from "./cbor.js";
import { at, decodeFailure as failure, type CanonicalError, type DecodeFailure } from "./error.js";
import { parse as parseJson } from "./json.js";
import { err, ok, type Err, type Ok, type Result } from "./result.js";
import { utf8Length } from "./utf8.js";
import { ArrayValue, BoolValue, BytesValue, IntValue, MapValue, TextValue, type Value } from "./value.js";

export type { Value } from "./value.js";
export { ok } from "./result.js";

/** Encodes `T` to a canonical value and decodes it back. */
export interface Codec<T> {
  encode(value: T): Result<Value, CanonicalError>;
  decode(value: Value): Result<T, DecodeFailure>;
}

/** Why bytes or text are not a model: not canonical, or not the model's shape. */
export type ModelError = { readonly kind: "Canonical"; readonly error: CanonicalError } | { readonly kind: "Decode"; readonly error: DecodeFailure };

/** The canonical CBOR of `value`. */
export function toCbor<T>(codec: Codec<T>, value: T): Result<Uint8Array, CanonicalError> {
  const v = codec.encode(value);
  return v.ok ? ok(encodeCbor(v.value)) : v;
}

/** The typed value of canonical CBOR. */
export function fromCbor<T>(codec: Codec<T>, bytes: Uint8Array): Result<T, ModelError> {
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

/** Any canonical value, unchanged (CDDL `any`). */
export const any: Codec<Value> = { encode: (v) => ok(v), decode: (v) => ok(v) };

export const bool: Codec<boolean> = {
  encode: (b) => ok(BoolValue.of(b)),
  decode: (v) => (v.kind === "bool" ? ok(v.value) : expected("bool", v)),
};

const show = (b: bigint | null): string => (b === null ? "" : b.toString());

/** An integer in `lo..hi`, either side unbounded when `null`. */
export function intRange(lo: bigint | null, hi: bigint | null): Codec<bigint> {
  const within = (n: bigint): boolean => (lo === null || n >= lo) && (hi === null || n <= hi);
  return {
    encode: (n) => (within(n) ? ok(IntValue.of(n)) : typeMismatch(`integer ${n.toString()} outside ${show(lo)}..${show(hi)}`)),
    decode: (v) => {
      if (v.kind !== "int") return expected("int", v);
      return within(v.value) ? ok(v.value) : decodeFailure(`integer ${v.value.toString()} outside ${show(lo)}..${show(hi)}`);
    },
  };
}

// CDDL integers follow RFC 8610: major type 0 (`uint`), major type 1 (`nint`), or either (`int`). A wider integer must
// be an explicit range in the schema.
/** CDDL `int`: -2^64..2^64-1. */
export const int: Codec<bigint> = intRange(-(2n ** 64n), 2n ** 64n - 1n);
/** CDDL `uint`: 0..2^64-1. */
export const uint: Codec<bigint> = intRange(0n, 2n ** 64n - 1n);
/** CDDL `nint`: -2^64..-1. */
export const nint: Codec<bigint> = intRange(-(2n ** 64n), -1n);

/** A byte string of `lo` to `hi` bytes (`hi` unbounded when `null`). */
export function sizedBytes(lo: number, hi: number | null): Codec<Uint8Array> {
  const fits = (n: number): boolean => n >= lo && (hi === null || n <= hi);
  return {
    encode: (b) => (fits(b.length) ? ok(BytesValue.of(b)) : typeMismatch(`byte string of ${b.length.toString()} bytes outside the schema size`)),
    decode: (v) => {
      if (v.kind !== "bytes") return expected("bytes", v);
      return fits(v.length) ? ok(v.bytes) : decodeFailure(`byte string of ${v.length.toString()} bytes, expected ${lo.toString()}..${hi === null ? "" : hi.toString()}`);
    },
  };
}

export const bytes: Codec<Uint8Array> = sizedBytes(0, null);

/** Text whose UTF-8 encoding is `lo` to `hi` bytes long (RFC 8610 `.size` on a text string counts bytes). */
export function sizedText(lo: number, hi: number | null): Codec<string> {
  const fits = (s: string): boolean => utf8Length(s) >= lo && (hi === null || utf8Length(s) <= hi);
  return {
    encode: (s) => {
      const t = TextValue.of(s);
      if (!t.ok) return t;
      return fits(s) ? t : typeMismatch(`text of ${utf8Length(s).toString()} UTF-8 bytes outside the schema size`);
    },
    decode: (v) => {
      if (v.kind !== "text") return expected("text", v);
      return fits(v.value) ? ok(v.value) : decodeFailure(`text of ${utf8Length(v.value).toString()} UTF-8 bytes, expected ${lo.toString()}..${hi === null ? "" : hi.toString()}`);
    },
  };
}

export const text: Codec<string> = sizedText(0, null);

/**
 * The `.regexp` patterns the schemas use. The generator accepts only these, so every SDK checks them identically
 * without a regular-expression engine.
 */
export type TextPattern = "([0-9a-f]{2})*" | "0x([0-9a-f]{2})*" | "0|[1-9][0-9]*";

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
  }
}

/** Text matching a {@link TextPattern} (CDDL `text .regexp`). */
export function matching(pattern: TextPattern): Codec<string> {
  return {
    encode: (s) => (matches(pattern, s) ? TextValue.of(s) : typeMismatch(`text does not match ${pattern}`)),
    decode: (v) => {
      if (v.kind !== "text") return expected("text", v);
      return matches(pattern, v.value) ? ok(v.value) : decodeFailure(`${JSON.stringify(v.value)} does not match ${pattern}`);
    },
  };
}

/** Exactly the text `literal`, as its literal type. */
export function literalText<S extends string>(literal: S): Codec<S> {
  return {
    encode: () => TextValue.of(literal),
    decode: (v) => {
      if (v.kind !== "text") return expected("text", v);
      return v.value === literal ? ok(literal) : decodeFailure(`expected ${JSON.stringify(literal)}, got ${JSON.stringify(v.value)}`);
    },
  };
}

/** Exactly the integer `literal`, as its literal type. */
export function literalInt<N extends bigint>(literal: N): Codec<N> {
  return {
    encode: () => ok(IntValue.of(literal)),
    decode: (v) => {
      if (v.kind !== "int") return expected("int", v);
      return v.value === literal ? ok(literal) : decodeFailure(`expected ${literal.toString()}, got ${v.value.toString()}`);
    },
  };
}

/** One of `texts`, for the text alternatives of a choice. */
export function oneOfText(t: string, texts: readonly string[]): Result<Value, CanonicalError> {
  return texts.includes(t) ? TextValue.of(t) : typeMismatch(`${JSON.stringify(t)} is not an alternative`);
}

/** A codec referenced by name and resolved on use, so rules may refer to each other in any order, recursively. */
export function defer<T>(codec: () => Codec<T>): Codec<T> {
  return { encode: (a) => codec().encode(a), decode: (v) => codec().decode(v) };
}

/** A homogeneous array of `min` to `max` elements (`max` unbounded when `null`). */
export function vector<T>(element: Codec<T>, min: number, max: number | null): Codec<readonly T[]> {
  const fits = (n: number): boolean => n >= min && (max === null || n <= max);
  return {
    encode: (xs) => {
      if (!fits(xs.length)) return typeMismatch(`array of ${xs.length.toString()} elements outside the schema size`);
      const items: Value[] = [];
      for (const x of xs) {
        const e = element.encode(x);
        if (!e.ok) return e;
        items.push(e.value);
      }
      return ArrayValue.of(items);
    },
    decode: (v) => {
      if (v.kind !== "array") return expected("array", v);
      if (!fits(v.items.length)) return decodeFailure(`array of ${v.items.length.toString()} elements, expected ${min.toString()}..${max === null ? "" : max.toString()}`);
      const out: T[] = [];
      for (const [i, item] of v.items.entries()) {
        const d = element.decode(item);
        if (!d.ok) return err(at(d.error, `[${i.toString()}]`));
        out.push(d.value);
      }
      return ok(out);
    },
  };
}

/** A map with arbitrary text keys and values of one type (CDDL `{ * text => T }`). */
export function textMap<T>(value: Codec<T>): Codec<ReadonlyMap<string, T>> {
  return {
    encode: (m) => {
      const entries: [string, Value][] = [];
      for (const [k, x] of m) {
        const e = value.encode(x);
        if (!e.ok) return e;
        entries.push([k, e.value]);
      }
      return MapValue.of(entries);
    },
    decode: (v) => {
      if (v.kind !== "map") return expected("map", v);
      const out = new Map<string, T>();
      for (const [k, x] of v.entries()) {
        const d = value.decode(x);
        if (!d.ok) return err(at(d.error, k));
        out.set(k, d.value);
      }
      return ok(out);
    },
  };
}

/** A labelled integer enumeration: each label is written as its integer. */
export function intEnum<L extends string>(name: string, cases: readonly (readonly [L, bigint])[]): Codec<L> {
  return {
    encode: (label) => {
      const c = cases.find(([l]) => l === label);
      return c === undefined ? typeMismatch(`${label} is not a ${name}`) : ok(IntValue.of(c[1]));
    },
    decode: (v) => {
      if (v.kind !== "int") return expected("int", v);
      const c = cases.find(([, w]) => w === v.value);
      return c === undefined ? decodeFailure(`${v.value.toString()} is not a ${name}`) : ok(c[0]);
    },
  };
}

// ------------------------------------------------------------------------------------------------ records

type Entry = Result<readonly [string, Value] | undefined, CanonicalError>;

/** One entry of an encoded record. */
export function field<T>(key: string, codec: Codec<T>, value: T): Entry {
  const v = codec.encode(value);
  return v.ok ? ok([key, v.value] as const) : v;
}

/** One optional entry: `undefined` (an absent field) is left out, never written as null. */
export function optionalField<T>(key: string, codec: Codec<T>, value: T | undefined): Entry {
  return value === undefined ? ok(undefined) : field(key, codec, value);
}

/** The map of a record's entries. */
export function record(entries: readonly Entry[]): Result<Value, CanonicalError> {
  const kept: (readonly [string, Value])[] = [];
  for (const e of entries) {
    if (!e.ok) return e;
    if (e.value !== undefined) kept.push(e.value);
  }
  return MapValue.of(kept);
}

/** The entries of a record being decoded: a map with no keys beyond those allowed. */
export interface Fields {
  required<T>(key: string, codec: Codec<T>): Result<T, DecodeFailure>;
  optional<T>(key: string, codec: Codec<T>): Result<T | undefined, DecodeFailure>;
}

/** The fields of `v`, which must be a map whose keys are all in `allowed`. */
export function fields(v: Value, allowed: readonly string[]): Ok<Fields> | Err<DecodeFailure> {
  if (v.kind !== "map") return expected("map", v);
  const unknown = v.keys().filter((k) => !allowed.includes(k));
  if (unknown.length > 0) return decodeFailure(`unknown keys ${[...unknown].sort().join(", ")}`);
  const decodeAt = <T>(key: string, codec: Codec<T>, x: Value): Result<T, DecodeFailure> => {
    const d = codec.decode(x);
    return d.ok ? d : err(at(d.error, key));
  };
  return ok({
    required: <T>(key: string, codec: Codec<T>): Result<T, DecodeFailure> => {
      const x = v.get(key);
      return x === undefined ? decodeFailure(`missing required key ${JSON.stringify(key)}`) : decodeAt(key, codec, x);
    },
    optional: <T>(key: string, codec: Codec<T>): Result<T | undefined, DecodeFailure> => {
      const x = v.get(key);
      return x === undefined ? ok(undefined) : decodeAt(key, codec, x);
    },
  });
}

/**
 * Whether a record value of a union is the alternative `T` (named by its codec), by its present (non-`undefined`)
 * fields: every required field present and none outside the allowed ones. The generator only accepts unions where at
 * most one fits.
 */
export function hasFields<T extends object>(a: object, _alternative: Codec<T>, required: readonly string[], allowed: readonly string[]): a is T {
  const present = Object.entries(a)
    .filter(([, x]) => x !== undefined)
    .map(([k]) => k);
  return required.every((k) => present.includes(k)) && present.every((k) => allowed.includes(k));
}

/**
 * The index of the alternative of a choice that `v` fits: the first record (given as its required and allowed wire
 * keys) that fits a map, then the text alternatives, numbered after the records.
 */
export function selectAlternative(v: Value, records: readonly (readonly [readonly string[], readonly string[]])[], texts: readonly string[]): Result<number, DecodeFailure> {
  if (v.kind === "map") {
    const keys = v.keys();
    const i = records.findIndex(([required, allowed]) => required.every((k) => keys.includes(k)) && keys.every((k) => allowed.includes(k)));
    return i >= 0 ? ok(i) : decodeFailure(`no alternative has keys ${[...keys].sort().join(", ")}`);
  }
  if (v.kind === "text") {
    const i = texts.indexOf(v.value);
    return i >= 0 ? ok(records.length + i) : decodeFailure(`${JSON.stringify(v.value)} is not an alternative`);
  }
  return decodeFailure("expected a map or one of the text alternatives");
}
