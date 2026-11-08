/**
 * Value-level semantics shared by every operation (`Values.scala`): truthiness, kinds, numbers, bounds, paths,
 * ordering, text, substring search and deduplication.
 */
import { BoolValue, BytesValue, cbor, equals, IntValue, models, NullValue, TextValue, type RationalValue } from "@kd5ujc/dilf4s-core";

import { limit, typeError, type JlvmError } from "./error.js";
import { compareCodePoints, JArray, type AnyArray, type Heap, type JMap, type JValue } from "./jvalue.js";
import type { JlvmOp } from "./op.js";

/** The largest number, in bits of the numerator and of the denominator (jlvm.md section 2). */
export const MAX_NUMBER_BITS = models.limits.JLVM_MAX_NUMBER_BITS;

/** The largest weight an operation may produce (jlvm.md section 2). */
export const MAX_RESULT_WEIGHT = BigInt(models.limits.JLVM_MAX_VALUE_WEIGHT);

/** Keys and path segments count once more per block of this many UTF-16 units. */
export const KEY_BLOCK = 32;

/** A map search pays once more per this many UTF-16 units of its key, at each level. */
export const SEARCH_BLOCK = 128;

/** A result: a value, or a language error. */
export type Out<T = JValue> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: JlvmError };

export const good = <T>(value: T): Out<T> => ({ ok: true, value });
export const bad = <T = JValue>(error: JlvmError): Out<T> => ({ ok: false, error });

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

/** Java's `BigInteger.bitLength`: the bits of the minimal two's-complement form, without the sign bit. */
export function bitLengthOf(n: bigint): number {
  const m = n < 0n ? -n - 1n : n;
  if (m < 0x100000000n) return 32 - Math.clz32(Number(m));
  const hex = m.toString(16);
  return (hex.length - 1) * 4 + 32 - Math.clz32(Number.parseInt(hex.charAt(0), 16));
}

/** JsonLogic truthiness: null, false, 0, "", empty bytes, [] and {} are falsy. */
export function truthy(v: JValue): boolean {
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
export function kindName(v: JValue): string {
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

export const isNumber = (v: JValue): v is Num => v.kind === "int" || v.kind === "rational";

/** The `Limit` of a number past {@link MAX_NUMBER_BITS}. */
export const tooLarge = (op: JlvmOp): JlvmError => limit(`${op}: a number exceeds ${MAX_NUMBER_BITS.toString()} bits`);

/** Whether a number's parts are within {@link MAX_NUMBER_BITS}, from their cached bit lengths: O(1). */
export function fits(x: Num): boolean {
  return x.kind === "int" ? x.bitLength <= MAX_NUMBER_BITS : x.numeratorBitLength <= MAX_NUMBER_BITS && x.denominatorBitLength <= MAX_NUMBER_BITS;
}

/** A number operand, within {@link MAX_NUMBER_BITS}: checked from its cached bit lengths, before any arithmetic. */
export function numeric(op: JlvmOp, v: JValue): Out<Num> {
  if (!isNumber(v)) return bad(typeError(op, `expected a number, got ${kindName(v)}`));
  return fits(v) ? good(v) : bad(tooLarge(op));
}

/** An integer operand, within {@link MAX_NUMBER_BITS}. */
export function integer(op: JlvmOp, v: JValue): Out<IntValue> {
  if (v.kind !== "int") return bad(typeError(op, `expected an integer, got ${kindName(v)}`));
  return v.bitLength <= MAX_NUMBER_BITS ? good(v) : bad(tooLarge(op));
}

/** A number result, within {@link MAX_NUMBER_BITS}. */
export function num(op: JlvmOp, x: Num): Out<Num> {
  return fits(x) ? good(x) : bad(tooLarge(op));
}

/** `v`, if its weight is within {@link MAX_RESULT_WEIGHT}: O(1), from its cached weight. `place` names the construct. */
export function bounded<T extends JValue>(place: string, v: T): Out<T> {
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

/** An array result: depth, then weight. */
export function array(place: string, items: readonly JValue[]): Out<JArray> {
  const a = JArray.of(items);
  return a === undefined ? bad(limit(`${place}: nests deeper than ${models.limits.CANONICAL_MAX_DEPTH.toString()}`)) : bounded(place, a);
}

/** A map result: depth, then weight. */
export function mapResult(place: string, m: JMap | undefined): Out<JMap> {
  return m === undefined ? bad(limit(`${place}: nests deeper than ${models.limits.CANONICAL_MAX_DEPTH.toString()}`)) : bounded(place, m);
}

/** The elements of an array. */
export const itemsOf = (a: AnyArray): readonly JValue[] => a.items;

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
export function at(v: JValue, path: readonly string[]): JValue {
  let here = v;
  for (const seg of path) {
    let next: JValue | undefined;
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
export function compare(op: JlvmOp, a: JValue, b: JValue): Out<number> {
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
export function renderedBits(v: JValue): number {
  if (v.kind === "int") return v.bitLength;
  if (v.kind === "rational") return v.numeratorBitLength + Math.floor((7 * v.denominatorBitLength + 2) / 3);
  return 0;
}

/** A path segment's count: 1, plus one per {@link KEY_BLOCK} of its UTF-16 length. */
export const segmentCount = (s: string): number => Math.floor(s.length / KEY_BLOCK) + 1;

/** A computed path's segment: text as {@link segmentCount}, anything else (an index) as one. */
export const pathSegments = (v: JValue): number => (v.kind === "text" ? segmentCount(v.value) : 1);

/** Top-level size: entries of arrays and maps, 1 for anything else. */
export function elements(v: JValue): number {
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

// ---- an order for deduplicating

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

/** A sort key for deduplicating: kinds first; scalars then compare structurally, and arrays and maps by their canonical encoding. */
interface SortKey {
  readonly rank: number;
  readonly value: JValue;
  readonly encoding: Uint8Array | undefined;
}

function compareBytes(a: Uint8Array, b: Uint8Array): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const d = (a[i] ?? 0) - (b[i] ?? 0);
    if (d !== 0) return d;
  }
  return a.length - b.length;
}

const RANKS: Readonly<Record<JValue["kind"], number>> = { null: 0, bool: 1, int: 2, rational: 3, text: 4, bytes: 5, array: 6, map: 7 };

/**
 * The sort key of `value`. A collection's encoding is budgeted by its weight, which the operation has paid for, and
 * capped at {@link MAX_KEY_BYTES}: a value heavier than the cap is refused from its weight, before anything is written.
 */
function sortKey(heap: Heap, op: JlvmOp, value: JValue): Out<SortKey> {
  const rank = RANKS[value.kind];
  if (value.kind === "bytes" && value.length > MAX_KEY_BYTES) return bad(limit(`${op}: a byte string past ${MAX_KEY_BYTES.toString()} bytes`));
  // a byte string is its own key, copied out once rather than at every comparison
  if (value.kind === "bytes") return good({ rank, value, encoding: value.bytes });
  if (value.kind !== "array" && value.kind !== "map") return good({ rank, value, encoding: undefined });
  const budget = value.weight * ENCODING_PER_WEIGHT;
  const cap = budget > BigInt(MAX_KEY_BYTES) ? MAX_KEY_BYTES : Number(budget);
  const core = heap.value(value);
  if (core === undefined) return bad(limit(`${op}: a value nests too deep`));
  const e = cbor.encodeWithin(core, cap);
  return e.ok ? good({ rank, value, encoding: e.value }) : bad(limit(`${op}: the encoding exceeds ${cap.toString()} bytes`));
}

function compareKeys(x: SortKey, y: SortKey): number {
  if (x.rank !== y.rank) return x.rank - y.rank;
  const [a, b] = [x.value, y.value];
  if (a.kind === "bool" && b.kind === "bool") return Number(a.value) - Number(b.value);
  if (a.kind === "int" && b.kind === "int") return a.value < b.value ? -1 : a.value > b.value ? 1 : 0;
  if (a.kind === "rational" && b.kind === "rational") {
    if (a.numerator !== b.numerator) return a.numerator < b.numerator ? -1 : 1;
    return a.denominator < b.denominator ? -1 : a.denominator > b.denominator ? 1 : 0;
  }
  if (a.kind === "text" && b.kind === "text") return compareCodePoints(a.value, b.value);
  return x.encoding !== undefined && y.encoding !== undefined ? compareBytes(x.encoding, y.encoding) : 0;
}

/** `vs` without repeats, keeping each value's first occurrence, in order: a stable sort and a scan, O(n log n). */
export function distinct(heap: Heap, op: JlvmOp, vs: readonly JValue[]): Out<readonly JValue[]> {
  const keyed: (readonly [SortKey, number])[] = [];
  for (const [i, v] of vs.entries()) {
    const k = sortKey(heap, op, v);
    if (!k.ok) return k;
    keyed.push([k.value, i]);
  }
  keyed.sort((a, b) => compareKeys(a[0], b[0]));
  const firsts: number[] = [];
  let previous: SortKey | undefined;
  for (const [key, i] of keyed) {
    if (previous === undefined || compareKeys(previous, key) !== 0) firsts.push(i);
    previous = key;
  }
  firsts.sort((a, b) => a - b);
  return good(firsts.map((i) => vs[i] ?? NULL));
}

/** Whether every element of `xs` is in `ys`: a binary search of `ys` in that order, O((m + n) log n). */
export function subset(heap: Heap, op: JlvmOp, xs: readonly JValue[], ys: readonly JValue[]): Out<boolean> {
  const sorted: SortKey[] = [];
  for (const y of ys) {
    const k = sortKey(heap, op, y);
    if (!k.ok) return k;
    sorted.push(k.value);
  }
  sorted.sort(compareKeys);
  for (const x of xs) {
    const k = sortKey(heap, op, x);
    if (!k.ok) return k;
    let [lo, hi] = [0, sorted.length];
    for (; lo < hi; ) {
      const mid = (lo + hi) >>> 1;
      const c = compareKeys(sorted[mid] ?? k.value, k.value);
      if (c < 0) lo = mid + 1;
      else hi = mid;
    }
    const found = sorted[lo];
    if (found === undefined || compareKeys(found, k.value) !== 0) return good(false);
  }
  return good(true);
}

// ---- equality

/**
 * Structural equality of canonical values: O(1) when the kinds, weights or depths differ; otherwise both sides as core
 * values (each container converted once), compared by core's `equals`, which never expands a shared part twice.
 */
export function equal(heap: Heap, a: JValue, b: JValue): boolean {
  if (a === b) return true;
  if (a.kind !== b.kind || a.weight !== b.weight || a.depth !== b.depth) return false;
  const x = heap.value(a);
  const y = heap.value(b);
  return x !== undefined && y !== undefined && equals(x, y);
}

// ---- the JSON surface's bytes

/** The bytes a string denotes in the JSON surface, if it is `0x` followed by an even number of lowercase hex digits. */
export function hexBytes(s: string): Uint8Array | undefined {
  if (s.length < 2 || s.charCodeAt(0) !== 0x30 || s.charCodeAt(1) !== 0x78 || s.length % 2 !== 0) return undefined;
  const out = new Uint8Array((s.length - 2) / 2);
  for (let i = 0; i < out.length; i++) {
    const hi = nibble(s.charCodeAt(2 + 2 * i));
    const lo = nibble(s.charCodeAt(3 + 2 * i));
    if (hi < 0 || lo < 0) return undefined;
    out[i] = (hi << 4) | lo;
  }
  return out;
}

/** A lowercase hex digit's value, or -1. */
const nibble = (c: number): number => (c >= 0x30 && c <= 0x39 ? c - 0x30 : c >= 0x61 && c <= 0x66 ? c - 0x57 : -1);

/** Integers that fit 64 bits, as text, for a computed path's segments. */
export function smallIntegerText(i: bigint): string | undefined {
  return i >= -(2n ** 63n) && i < 2n ** 63n ? i.toString() : undefined;
}
