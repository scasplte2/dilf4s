/**
 * Value-level semantics shared by every operation (`Values.scala`): truthiness, kinds, numbers, bounds, paths,
 * ordering, text, substring search and deduplication.
 */
import { ArrayValue, BoolValue, BytesValue, cbor, equals, IntValue, models, NullValue, TextValue, type CanonicalError, type MapValue, type RationalValue, type Result, type Value } from "@kd5ujc/dilf4s-core";
import { compareCodePoints } from "@kd5ujc/dilf4s-core/internal";

import { limit, typeError, type JlvmError } from "./error.js";
import type { JlvmOp } from "./op.js";
import { RadixIndex, radixKey, type RadixKey } from "./radix.js";

/** The largest number, in bits of the numerator and of the denominator (jlvm.md section 2). */
export const MAX_NUMBER_BITS = models.limits.JLVM_MAX_NUMBER_BITS;

/** The largest weight an operation may produce (jlvm.md section 2). */
export const MAX_RESULT_WEIGHT = BigInt(models.limits.JLVM_MAX_VALUE_WEIGHT);

/** Keys and path segments count once more per block of this many UTF-16 units. */
export const KEY_BLOCK = 32;

/** A map search pays once more per this many UTF-16 units of its key, at each level. */
export const SEARCH_BLOCK = 128;

/** A result: a value, or a language error. */
export type Out<T = Value> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: JlvmError };

export const good = <T>(value: T): Out<T> => ({ ok: true, value });
export const bad = <T = Value>(error: JlvmError): Out<T> => ({ ok: false, error });

export const NULL: NullValue = NullValue.instance;
export const TRUE: BoolValue = BoolValue.true;
export const FALSE: BoolValue = BoolValue.false;

/** The boolean `b`. */
export const bool = (b: boolean): BoolValue => (b ? TRUE : FALSE);

/**
 * A count or a length as an integer. `IntValue.of` refuses only past 33219 bits, far past any count, so the refusal is
 * never reached; it would be a `Limit`.
 */
export function count(n: number | bigint): Out<IntValue> {
  const r = IntValue.of(BigInt(n));
  return r.ok ? good(r.value) : bad(limit("a count past the integer bound"));
}

/** JsonLogic truthiness: null, false, 0, "", empty bytes, [] and {} are falsy. */
export function truthy(v: Value): boolean {
  switch (v.kind) {
    case "null":
      return false;
    case "bool":
      return v.value;
    case "int":
      return v.value !== 0n;
    case "rational":
      return true;
    case "text":
      return v.value.length > 0;
    case "bytes":
      return v.length > 0;
    case "array":
      return v.length > 0;
    case "map":
      return v.size > 0;
  }
}

/** The kind name `typeof` reports. There is one number kind. */
export function kindName(v: Value): string {
  switch (v.kind) {
    case "null":
      return "null";
    case "bool":
      return "boolean";
    case "int":
    case "rational":
      return "number";
    case "text":
      return "string";
    case "bytes":
      return "bytes";
    case "array":
      return "array";
    case "map":
      return "object";
  }
}

/** A number: core's integer or reduced rational. */
export type Num = IntValue | RationalValue;

export const isNumber = (v: Value): v is Num => v.kind === "int" || v.kind === "rational";

/** The `Limit` of a number past {@link MAX_NUMBER_BITS}. */
export const tooLarge = (op: JlvmOp): JlvmError => limit(`${op}: a number exceeds ${MAX_NUMBER_BITS.toString()} bits`);

/** Whether a number's parts are within {@link MAX_NUMBER_BITS}, from their cached bit lengths: O(1). */
export function fits(x: Num): boolean {
  return x.kind === "int" ? x.bitLength <= MAX_NUMBER_BITS : x.numeratorBitLength <= MAX_NUMBER_BITS && x.denominatorBitLength <= MAX_NUMBER_BITS;
}

/** A number operand, within {@link MAX_NUMBER_BITS}: checked from its cached bit lengths, before any arithmetic. */
export function numeric(op: JlvmOp, v: Value): Out<Num> {
  if (!isNumber(v)) return bad(typeError(op, `expected a number, got ${kindName(v)}`));
  return fits(v) ? good(v) : bad(tooLarge(op));
}

/** An integer operand, within {@link MAX_NUMBER_BITS}. */
export function integer(op: JlvmOp, v: Value): Out<IntValue> {
  if (v.kind !== "int") return bad(typeError(op, `expected an integer, got ${kindName(v)}`));
  return v.bitLength <= MAX_NUMBER_BITS ? good(v) : bad(tooLarge(op));
}

/** A number result, within {@link MAX_NUMBER_BITS}. */
export function num(op: JlvmOp, x: Num): Out<Num> {
  return fits(x) ? good(x) : bad(tooLarge(op));
}

/** `v`, if its weight is within {@link MAX_RESULT_WEIGHT}: O(1), from its cached weight. `place` names the construct. */
export function bounded<T extends Value>(place: string, v: T): Out<T> {
  return v.weight <= MAX_RESULT_WEIGHT ? good(v) : bad(limit(`${place}: the result exceeds weight ${MAX_RESULT_WEIGHT.toString()}`));
}

/** A text result: its weight, then the text. Text built from well-formed text is well-formed. */
export function text(place: string, s: string): Out<TextValue> {
  const t = TextValue.of(s);
  return t.ok ? bounded(place, t.value) : bad(limit(`${place}: text with an unpaired surrogate`));
}

/** A byte-string result, within the weight bound. */
export function bytes(place: string, b: Uint8Array): Out<BytesValue> {
  const v = BytesValue.of(b);
  return v.ok ? bounded(place, v.value) : bad(limit(`${place}: not bytes`));
}

/** The `Limit` of a container that would nest deeper than the data model allows. */
const tooDeep = (place: string): JlvmError => limit(`${place}: nests deeper than ${models.limits.CANONICAL_MAX_DEPTH.toString()}`);

/**
 * An array result: depth, then weight. Core's own constructor builds it, as for any caller: a copy of `items`, each
 * read once and checked a value (which every value the machine holds is), O(n), as the elements were paid for. Its
 * one refusal for them is the depth.
 */
export function array(place: string, items: readonly Value[]): Out<ArrayValue> {
  const a = ArrayValue.of(items);
  return a.ok ? bounded(place, a.value) : bad(tooDeep(place));
}

/** A map result: depth (the only refusal a map the machine builds can meet), then weight. */
export function mapResult(place: string, m: Result<MapValue, CanonicalError>): Out<MapValue> {
  return m.ok ? bounded(place, m.value) : bad(tooDeep(place));
}

/** The elements of an array. */
export const itemsOf = (a: ArrayValue): readonly Value[] => a.items;

/** `0|[1-9][0-9]{0,8}`, as an index. */
function index(seg: string): number | undefined {
  const n = seg.length;
  if (n === 0 || n > 9) return undefined;
  if (seg === "0") return 0;
  const first = seg.charCodeAt(0);
  if (first < 0x31 || first > 0x39) return undefined;
  for (let i = 1; i < n; i++) {
    const c = seg.charCodeAt(i);
    if (c < 0x30 || c > 0x39) return undefined;
  }
  return Number(seg);
}

/** The value at `path`: an object segment is a key; an array segment is a decimal index without leading zeros. Anything absent is null. */
export function at(v: Value, path: readonly string[]): Value {
  let here = v;
  for (const seg of path) {
    let next: Value | undefined;
    if (here.kind === "map") next = here.get(seg);
    else if (here.kind === "array") {
      const i = index(seg);
      next = i === undefined ? undefined : here.items[i];
    }
    if (next === undefined) return NULL;
    here = next;
  }
  return here;
}

/** `a / b` of two numbers' parts: `[numerator, denominator]`. */
export function parts(x: Num): readonly [bigint, bigint] {
  return x.kind === "int" ? [x.value, 1n] : [x.numerator, x.denominator];
}

/** The order of two numbers, exactly: their cross products. */
export function compareNumbers(a: Num, b: Num): number {
  if (a.kind === "int" && b.kind === "int") return a.value < b.value ? -1 : a.value > b.value ? 1 : 0;
  const [an, ad] = parts(a);
  const [bn, bd] = parts(b);
  const x = an * bd;
  const y = bn * ad;
  return x < y ? -1 : x > y ? 1 : 0;
}

/** Text compares by Unicode code point; numbers numerically; anything else cannot be ordered. */
export function compare(op: JlvmOp, a: Value, b: Value): Out<number> {
  if (a.kind === "text" && b.kind === "text") return good(compareCodePoints(a.value, b.value));
  if (isNumber(a) && isNumber(b)) {
    const x = numeric(op, a);
    if (!x.ok) return x;
    const y = numeric(op, b);
    if (!y.ok) return y;
    return good(compareNumbers(x.value, y.value));
  }
  return bad(typeError(op, `cannot order ${kindName(a)} and ${kindName(b)}`));
}

// ---- text

/** The Unicode `White_Space` property (PropList.txt), listed explicitly. */
function whiteSpace(c: number): boolean {
  return (c >= 0x09 && c <= 0x0d) || c === 0x20 || c === 0x85 || c === 0xa0 || c === 0x1680 || (c >= 0x2000 && c <= 0x200a) || c === 0x2028 || c === 0x2029 || c === 0x202f || c === 0x205f || c === 0x3000;
}

/** Trims Unicode `White_Space` from both ends (every such code point is a single UTF-16 unit). */
export function trim(s: string): string {
  let from = 0;
  let to = s.length;
  for (; from < to && whiteSpace(s.charCodeAt(from)); from++);
  for (; to > from && whiteSpace(s.charCodeAt(to - 1)); to--);
  return s.slice(from, to);
}

/** ASCII letters only: every other code point is unchanged. */
export function asciiCase(s: string, upper: boolean): string {
  const [lo, hi, shift] = upper ? [0x61, 0x7a, -32] : [0x41, 0x5a, 32];
  let out = "";
  let start = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c >= lo && c <= hi) {
      out += s.slice(start, i) + String.fromCharCode(c + shift);
      start = i + 1;
    }
  }
  return out + s.slice(start);
}

/** The number of code points of a well-formed string. */
export function codePoints(s: string): number {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    // a high surrogate starts a pair, which is one code point
    if (c < 0xdc00 || c > 0xdfff) n++;
  }
  return n;
}

/** The UTF-16 offset of code point `k` of `s` (its length when `k` is past the end). */
export function codePointOffset(s: string, k: number): number {
  let seen = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c >= 0xdc00 && c <= 0xdfff) continue;
    if (seen === k) return i;
    seen++;
  }
  return s.length;
}

/** The bits of a number's decimal expansion, bounded from its size alone (`Values.renderedBits`); 0 for anything else. */
export function renderedBits(v: Value): number {
  if (v.kind === "int") return v.bitLength;
  if (v.kind === "rational") return v.numeratorBitLength + Math.floor((7 * v.denominatorBitLength + 2) / 3);
  return 0;
}

/** A path segment's count: 1, plus one per {@link KEY_BLOCK} of its UTF-16 length. */
export const segmentCount = (s: string): number => Math.floor(s.length / KEY_BLOCK) + 1;

/** A computed path's segment: text as {@link segmentCount}, anything else (an index) as one. */
export const pathSegments = (v: Value): number => (v.kind === "text" ? segmentCount(v.value) : 1);

/** Top-level size: entries of arrays and maps, 1 for anything else. */
export function elements(v: Value): number {
  return v.kind === "array" ? v.length : v.kind === "map" ? v.size : 1;
}

/**
 * Whether the needle (`m` units, its `i`th `needle(i)`) occurs in the haystack (`n` units): Knuth-Morris-Pratt, linear
 * in both lengths. Text is searched by UTF-16 unit: in well-formed text a match never starts or ends inside a
 * surrogate pair (the needle would hold an unpaired surrogate), so this is the code-point search.
 */
function kmp(n: number, haystack: (i: number) => number, m: number, needle: (i: number) => number): boolean {
  if (m === 0) return true;
  // failure[i]: the length of the longest proper prefix of needle[0..i] that is also its suffix
  const failure = new Int32Array(m);
  for (let i = 1, k = 0; i < m; i++) {
    const c = needle(i);
    for (; k > 0 && needle(k) !== c; k = failure[k - 1] ?? 0);
    if (needle(k) === c) k++;
    failure[i] = k;
  }
  for (let i = 0, k = 0; i < n; i++) {
    const c = haystack(i);
    for (; k > 0 && needle(k) !== c; k = failure[k - 1] ?? 0);
    if (needle(k) === c) {
      k++;
      if (k === m) return true;
    }
  }
  return false;
}

/** Whether `needle` is a substring of `haystack`, in linear time. */
export const containsText = (haystack: string, needle: string): boolean =>
  kmp(haystack.length, (i) => haystack.charCodeAt(i), needle.length, (i) => needle.charCodeAt(i));

/** Whether `needle` is a byte substring of `haystack`, in linear time. */
export const containsBytes = (haystack: Uint8Array, needle: Uint8Array): boolean =>
  kmp(haystack.length, (i) => haystack[i] ?? -1, needle.length, (i) => needle[i] ?? -2);

// ---- typed byte keys for deduplicating

/**
 * An upper bound on a canonical encoding's bytes per unit of the value's weight: a head is at most 9 bytes for a node
 * of weight at least 1, a text or byte string adds its length (which its weight counts), and a number of `l` whole
 * limbs takes at most `8 (l + 1)` bytes and a tag besides, for weight at least `1 + l`.
 */
const ENCODING_PER_WEIGHT = 32n;

/**
 * The longest sort key encoded: 2^31 - 9 bytes, the reference's encoding ceiling (`Cbor.MaxEncodedBytes`). A longer
 * key is a `Limit` on every backend (jlvm.md section 7), never an allocation.
 */
const MAX_KEY_BYTES = cbor.MAX_ENCODED_BYTES;

const RANKS: Readonly<Record<Value["kind"], number>> = { null: 0, bool: 1, int: 2, rational: 3, text: 4, bytes: 5, array: 6, map: 7 };

/** Typed byte keys preserve structural equality and the existing compound-key encoding ceiling. */
function keyOf(op: JlvmOp, value: Value): Out<RadixKey> {
  const rank = RANKS[value.kind];
  if (value.kind === "null") return good(radixKey(rank, 0, () => 0));
  if (value.kind === "bool") return good(radixKey(rank, 1, () => Number(value.value)));
  if (value.kind === "text") return good(radixKey(rank, 2 * value.value.length, (i) => {
    const c = value.value.charCodeAt(Math.floor(i / 2));
    return i % 2 === 0 ? c >>> 8 : c & 255;
  }));
  if (value.kind === "bytes") {
    if (value.length > MAX_KEY_BYTES) return bad(limit(`${op}: a byte string past ${MAX_KEY_BYTES.toString()} bytes`));
    const bytes = value.bytes;
    return good(radixKey(rank, bytes.length, (i) => bytes[i] ?? 0));
  }
  const budget = value.weight * ENCODING_PER_WEIGHT;
  const cap = budget > BigInt(MAX_KEY_BYTES) ? MAX_KEY_BYTES : Number(budget);
  const encoded = cbor.encodeWithin(value, cap);
  return encoded.ok
    ? good(radixKey(rank, encoded.value.length, (i) => encoded.value[i] ?? 0))
    : bad(limit(`${op}: the encoding exceeds ${cap.toString()} bytes`));
}

/** First occurrences, in input order, with work linear in the typed keys' total length. */
export function distinct(op: JlvmOp, vs: readonly Value[]): Out<readonly Value[]> {
  const index = new RadixIndex();
  const kept: Value[] = [];
  for (const value of vs) {
    const key = keyOf(op, value);
    if (!key.ok) return key;
    if (index.add(key.value).added) kept.push(value);
  }
  return good(kept);
}

/** Builds all of `ys` before probing `xs`, stopping at its first absent value or refusal. */
export function subset(op: JlvmOp, xs: readonly Value[], ys: readonly Value[]): Out<boolean> {
  const index = new RadixIndex();
  for (const value of ys) {
    const key = keyOf(op, value);
    if (!key.ok) return key;
    index.add(key.value);
  }
  for (const value of xs) {
    const key = keyOf(op, value);
    if (!key.ok) return key;
    if (!index.contains(key.value).found) return good(false);
  }
  return good(true);
}

// ---- equality

/**
 * Structural equality of canonical values, by core's `equals`: O(1) when the kinds, weights or depths differ, and it
 * never expands a shared part twice.
 */
export const equal = (a: Value, b: Value): boolean => equals(a, b);

// ---- the JSON surface's bytes

/**
 * Whether a string is `0x` followed by an even number of lowercase hex digits, the JSON surface's form of bytes: read
 * in place, nothing allocated. Core's `fromHex` takes either case and no prefix, so it is not this form.
 */
export function isHexText(s: string): boolean {
  if (s.length < 2 || s.charCodeAt(0) !== 0x30 || s.charCodeAt(1) !== 0x78 || s.length % 2 !== 0) return false;
  for (let i = 2; i < s.length; i++) if (nibble(s.charCodeAt(i)) < 0) return false;
  return true;
}

/** The bytes a string denotes in the JSON surface, if it has that form ({@link isHexText}): allocated once it does. */
export function hexBytes(s: string): Uint8Array | undefined {
  if (!isHexText(s)) return undefined;
  const out = new Uint8Array((s.length - 2) / 2);
  for (let i = 0; i < out.length; i++) out[i] = (nibble(s.charCodeAt(2 + 2 * i)) << 4) | nibble(s.charCodeAt(3 + 2 * i));
  return out;
}

/** A lowercase hex digit's value, or -1. */
const nibble = (c: number): number => (c >= 0x30 && c <= 0x39 ? c - 0x30 : c >= 0x61 && c <= 0x66 ? c - 0x57 : -1);

/** Integers that fit 64 bits, as text, for a computed path's segments. */
export function smallIntegerText(i: bigint): string | undefined {
  return i >= -(2n ** 63n) && i < 2n ** 63n ? i.toString() : undefined;
}
