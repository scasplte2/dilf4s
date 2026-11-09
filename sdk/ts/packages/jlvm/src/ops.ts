/**
 * Operations on already-evaluated values (`PureOps.scala`, `sdk/rust/jlvm/src/ops.rs`). Typing is strict: a value of
 * the wrong kind is a `Type` error, never coerced; the one exception is metakit's handling of an absent value (null)
 * in `in`, `intersect`, `startsWith`, `endsWith`, `keys`, `values`, `entries`, `cat` and `join`.
 *
 * Each operation checks its arguments in the reference's order (its `tupled`/`traverse` are fail-fast, left to
 * right), so the first error is the same. {@link STRICT} has exactly one handler per strict operation: its type is a
 * record over every {@link StrictOp}, so a missing or extra one does not compile.
 */
import { cbor, json, models, type HashAlgorithm } from "@kd5ujc/dilf4s-core";
import { bitLengthOf, digestOf, toHexOf, utf8LengthOf } from "@kd5ujc/dilf4s-core/internal";

import { arithmetic, arity, domain, limit, typeError, type JlvmError } from "./error.js";
import { JMap, type AnyMap, type Heap, type JValue } from "./jvalue.js";
import type { StrictOp } from "./op.js";
import * as ratio from "./ratio.js";
import {
  array,
  asciiCase,
  bad,
  bool,
  bounded,
  bytes,
  codePointOffset,
  compareNumbers,
  codePoints,
  compare,
  containsBytes,
  containsText,
  count,
  distinct,
  equal,
  FALSE,
  good,
  hexBytes,
  integer,
  isNumber,
  kindName,
  mapResult,
  MAX_NUMBER_BITS,
  MAX_RESULT_WEIGHT,
  NULL,
  num,
  numeric,
  parts,
  subset,
  text,
  tooLarge,
  trim,
  truthy,
  type Num,
  type Out,
} from "./values.js";

/** The largest exponent magnitude `pow` accepts. */
export const MAX_EXPONENT = models.limits.JLVM_MAX_EXPONENT;

/** The largest `int_to_bytes` width. */
export const MAX_INT_BYTES = models.limits.JLVM_MAX_INT_BYTES;

/** A strict operation's handler: its evaluated arguments, and the evaluation's heap. */
export type Handler = (args: readonly JValue[], heap: Heap) => Out;

// ---- argument shapes

function unary(op: StrictOp, args: readonly JValue[], f: (a: JValue) => Out): Out {
  const [a] = args;
  return args.length === 1 && a !== undefined ? f(a) : bad(arity(op, `takes 1 argument, got ${args.length.toString()}`));
}

function binary(op: StrictOp, args: readonly JValue[], f: (a: JValue, b: JValue) => Out): Out {
  const [a, b] = args;
  return args.length === 2 && a !== undefined && b !== undefined ? f(a, b) : bad(arity(op, `takes 2 arguments, got ${args.length.toString()}`));
}

const numUnary = (op: StrictOp, args: readonly JValue[], f: (x: Num) => Out<Num>): Out =>
  unary(op, args, (v) => {
    const x = numeric(op, v);
    if (!x.ok) return x;
    const r = f(x.value);
    return r.ok ? num(op, r.value) : r;
  });

const numBinary = (op: StrictOp, args: readonly JValue[], f: (x: Num, y: Num) => Out<Num>): Out =>
  binary(op, args, (a, b) => {
    const x = numeric(op, a);
    if (!x.ok) return x;
    const y = numeric(op, b);
    if (!y.ok) return y;
    const r = f(x.value, y.value);
    return r.ok ? num(op, r.value) : r;
  });

const integerOf = (op: StrictOp, n: bigint): Out<Num> => {
  const r = count(n);
  return r.ok ? r : bad(tooLarge(op));
};

const expected = (op: StrictOp, what: string, v: JValue): Out<never> => bad(typeError(op, `expected ${what}, got ${kindName(v)}`));

function str(op: StrictOp, v: JValue): Out<string> {
  return v.kind === "text" ? good(v.value) : expected(op, "a string", v);
}

function arr(op: StrictOp, v: JValue): Out<readonly JValue[]> {
  return v.kind === "array" ? good(v.items) : expected(op, "an array", v);
}

function obj(op: StrictOp, v: JValue): Out<AnyMap> {
  return v.kind === "map" ? good(v) : expected(op, "an object", v);
}

function textKey(op: StrictOp, v: JValue): Out<string> {
  return v.kind === "text" ? good(v.value) : expected(op, "a string key", v);
}

function rawBytes(op: StrictOp, v: JValue): Out<Uint8Array> {
  return v.kind === "bytes" ? good(v.bytes) : expected(op, "bytes", v);
}

/** An integer index; any integer, since every use clamps it to a collection's size. */
function index(op: StrictOp, v: JValue): Out<bigint> {
  return v.kind === "int" ? good(v.value) : expected(op, "an integer", v);
}

/**
 * The array of `vs` with arrays' elements spread and other values appended, in order: its weight is found from the
 * arguments' cached weights first, so one past the bound is a `Limit` before any element is copied (a spread of
 * shared arrays could otherwise ask for more elements than memory holds).
 */
function spreading(op: StrictOp, vs: readonly JValue[]): Out {
  const weight = vs.reduce((w, v) => w + (v.kind === "array" ? v.weight - 1n : v.weight), 1n);
  return weight > MAX_RESULT_WEIGHT ? bad(limit(`${op}: the result exceeds weight ${MAX_RESULT_WEIGHT.toString()}`)) : array(op, spreadAll(vs));
}

/** Arrays' elements spread, and other values appended, in order. */
function spreadAll(vs: readonly JValue[]): JValue[] {
  const out: JValue[] = [];
  for (const v of vs) {
    if (v.kind === "array") for (const x of v.items) out.push(x);
    else out.push(v);
  }
  return out;
}

/** JavaScript `slice` bounds: a negative index counts from the end; the result is clamped to `[0, n]`. */
function bound(i: bigint, n: number): number {
  const len = BigInt(n);
  const at = i < 0n ? (len + i < 0n ? 0n : len + i) : i < len ? i : len;
  return Number(at);
}

// ---- comparison and arithmetic

/** `<` and `<=` also take three arguments, meaning "between". */
function ordering(op: StrictOp, args: readonly JValue[], holds: (c: number) => boolean, between: boolean): Out {
  const [a, b, c] = args;
  if (args.length === 2 && a !== undefined && b !== undefined) {
    const x = compare(op, a, b);
    return x.ok ? good(bool(holds(x.value))) : x;
  }
  if (between && args.length === 3 && a !== undefined && b !== undefined && c !== undefined) {
    const x = compare(op, a, b);
    if (!x.ok) return x;
    const y = compare(op, b, c);
    return y.ok ? good(bool(holds(x.value) && holds(y.value))) : y;
  }
  return bad(arity(op, between ? "takes 2 or 3 arguments" : "takes 2 arguments"));
}

/** Every argument as a number within the bound, in order. */
export function operands(op: StrictOp, args: readonly JValue[]): Out<readonly Num[]> {
  const out: Num[] = [];
  for (const v of args) {
    const x = numeric(op, v);
    if (!x.ok) return x;
    out.push(x.value);
  }
  return good(out);
}

/**
 * `min` or `max`: the argument `keep` chooses, returned as it is. It is already a canonical number, so it is not
 * rebuilt (which would reduce a rational again, through a gcd).
 */
function fold(op: StrictOp, args: readonly JValue[], keep: (c: number) => boolean): Out {
  const rs = operands(op, args);
  if (!rs.ok) return rs;
  const [first, ...rest] = rs.value;
  if (first === undefined) return bad(arity(op, "takes at least 1 argument"));
  return good(rest.reduce((a, b) => (keep(compareNumbers(a, b)) ? a : b), first));
}

/** `+` or `*` of no operands. */
export function identity(op: "+" | "*"): Out<Num> {
  return integerOf(op, op === "*" ? 1n : 0n);
}

/** One step of `+` or `*`: every intermediate is checked, so a fold never builds a number past the bound. */
export function combine(op: "+" | "*", acc: Num, x: Num): Out<Num> {
  const r = op === "*" ? ratio.multiply(op, acc, x) : ratio.add(op, acc, x);
  return r.ok ? num(op, r.value) : r;
}

/** `+` and `*` uncharged, from their identities (the machine folds them in charged steps instead). */
function total(op: "+" | "*", args: readonly JValue[]): Out {
  const rs = operands(op, args);
  if (!rs.ok) return rs;
  let acc = identity(op);
  for (const x of rs.value) {
    if (!acc.ok) return acc;
    acc = combine(op, acc.value, x);
  }
  return acc;
}

function subtract(op: StrictOp, args: readonly JValue[]): Out {
  const rs = operands(op, args);
  if (!rs.ok) return rs;
  const [a, b] = rs.value;
  const r = rs.value.length === 1 && a !== undefined ? ratio.negate(op, a) : rs.value.length === 2 && a !== undefined && b !== undefined ? ratio.subtract(op, a, b) : undefined;
  if (r === undefined) return bad(arity(op, `takes 1 or 2 arguments, got ${rs.value.length.toString()}`));
  return r.ok ? num(op, r.value) : r;
}

/** An integer exponent in `-999..999`; the result's size is checked before it is computed. */
function power(op: StrictOp, base: Num, exponent: Num): Out<Num> {
  const e = exponent.kind === "int" && exponent.value >= -BigInt(MAX_EXPONENT) && exponent.value <= BigInt(MAX_EXPONENT) ? Number(exponent.value) : undefined;
  if (e === undefined) return bad(arithmetic(op, `the exponent must be an integer in -${MAX_EXPONENT.toString()}..${MAX_EXPONENT.toString()}`));
  const [n, d] = parts(base);
  const bits = Math.max(bitLengthOf(n), bitLengthOf(d)) * Math.abs(e);
  if (bits > MAX_NUMBER_BITS) return bad(limit(`${op}: the result would exceed ${MAX_NUMBER_BITS.toString()} bits`));
  return ratio.power(op, base, e);
}

// ---- text

/** A piece of `cat` or `join`: a string, a number (exact decimal, paid for by the rendering charge) or a boolean; null is empty. */
function piece(op: "cat" | "join", v: JValue): Out<string> {
  switch (v.kind) {
    case "text":
      return good(v.value);
    case "null":
      return good("");
    case "bool":
      return good(v.value ? "true" : "false");
    case "int": {
      const i = integer(op, v);
      return i.ok ? good(i.value.value.toString()) : i;
    }
    case "rational": {
      const r = numeric(op, v);
      if (!r.ok) return r;
      // the exact decimal text, when it terminates (and is within JSON's bounds, which a JLVM number always is)
      const s = json.decimal(v);
      return s.ok
        ? s
        : bad(arithmetic(op, `a rational of ${v.numeratorBitLength.toString()}/${v.denominatorBitLength.toString()} bits has no finite decimal form`));
    }
    case "bytes":
    case "array":
    case "map":
      return bad(typeError(op, `cannot render ${kindName(v)} as text`));
  }
}

/** The weight of the text `pieces` make with `separators` more bytes. */
const projected = (pieces: readonly string[], separators: number): bigint => pieces.reduce((n, p) => n + BigInt(utf8LengthOf(p)), 1n) + BigInt(separators);

/** A text result's weight, checked before the text is built. */
function fitting(op: "cat" | "join", weight: bigint): JlvmError | undefined {
  return weight <= MAX_RESULT_WEIGHT ? undefined : limit(`${op}: the result would exceed weight ${MAX_RESULT_WEIGHT.toString()}`);
}

const allOf = (args: readonly JValue[], kind: JValue["kind"]): boolean => args.length > 0 && args.every((v) => v.kind === kind);

/** The values `cat` or `join` renders as decimal text: their numbers, priced before rendering. */
export function rendered(op: "cat" | "join", args: readonly JValue[]): readonly JValue[] {
  if (op === "cat") return allOf(args, "array") || allOf(args, "bytes") ? [] : args.filter(isNumber);
  const [a] = args;
  return args.length === 2 && a?.kind === "array" ? a.items.filter(isNumber) : [];
}

/** Concatenates text; when every argument is an array, or every one is bytes, concatenates those instead. */
export function cat(args: readonly JValue[]): Out {
  const op = "cat";
  if (allOf(args, "array")) return spreading(op, args);
  if (allOf(args, "bytes")) {
    // the size charge paid for every byte; the weight is checked before the copy
    const length = args.reduce((n, v) => n + (v.kind === "bytes" ? v.length : 0), 0);
    if (BigInt(length) + 1n > MAX_RESULT_WEIGHT) return bad(limit(`${op}: the result exceeds weight ${MAX_RESULT_WEIGHT.toString()}`));
    const out = new Uint8Array(length);
    let at = 0;
    for (const v of args) {
      if (v.kind !== "bytes") continue;
      out.set(v.bytes, at);
      at += v.length;
    }
    return bytes(op, out);
  }
  const pieces: string[] = [];
  for (const v of args) {
    const p = piece(op, v);
    if (!p.ok) return p;
    pieces.push(p.value);
  }
  const over = fitting(op, projected(pieces, 0));
  return over === undefined ? text(op, pieces.join("")) : bad(over);
}

/** `[text, start]` or `[text, start, length]`, counted in code points; a negative length drops from the end. */
function substr(args: readonly JValue[]): Out {
  const op = "substr";
  const [s, start, len] = args;
  if (args.length < 2 || args.length > 3 || s === undefined || start === undefined) return bad(arity(op, "takes [text, start] or [text, start, length]"));
  const t = str(op, s);
  if (!t.ok) return t;
  const i = index(op, start);
  if (!i.ok) return i;
  const l = len === undefined ? undefined : index(op, len);
  if (l !== undefined && !l.ok) return l;
  const n = codePoints(t.value);
  const from = bound(i.value, n);
  const to = l === undefined ? n : l.value < 0n ? bound(l.value, n) : from + Number(l.value < BigInt(n - from) ? l.value : BigInt(n - from));
  const taken = Math.max(to, from) - from;
  const a = codePointOffset(t.value, from);
  return text(op, t.value.slice(a, codePointOffset(t.value, from + taken)));
}

/** Null on either side means "not present" and gives `false`. */
function affix(op: "startsWith" | "endsWith", args: readonly JValue[]): Out {
  const [s, p] = args;
  if (args.length !== 2 || s === undefined || p === undefined) return bad(arity(op, `takes 2 arguments, got ${args.length.toString()}`));
  if (s.kind === "null" || (s.kind === "text" && p.kind === "null")) return good(FALSE);
  const a = str(op, s);
  if (!a.ok) return a;
  const b = str(op, p);
  if (!b.ok) return b;
  return good(bool(op === "startsWith" ? a.value.startsWith(b.value) : a.value.endsWith(b.value)));
}

/** A `join` whose pieces are rendered and whose result fits: what remains is to pay for the separators and build it. */
export interface Joining {
  readonly pieces: readonly string[];
  readonly separator: string;
}

/** The separator bytes the result will copy. */
export function separatorBytes(j: Joining): number {
  const n = j.pieces.length;
  return n === 0 ? 0 : Math.min(utf8LengthOf(j.separator) * (n - 1), Number.MAX_SAFE_INTEGER);
}

/** Validates and renders a `join`'s pieces, and rejects a result past the weight bound, without building it. */
export function joining(args: readonly JValue[]): Out<Joining> {
  const op = "join";
  const [a, sep] = args;
  if (args.length !== 2 || a === undefined || sep === undefined) return bad(arity(op, `takes 2 arguments, got ${args.length.toString()}`));
  const vs = arr(op, a);
  if (!vs.ok) return vs;
  const separator = str(op, sep);
  if (!separator.ok) return separator;
  const pieces: string[] = [];
  for (const v of vs.value) {
    const p = piece(op, v);
    if (!p.ok) return p;
    pieces.push(p.value);
  }
  const j = { pieces, separator: separator.value };
  const over = fitting(op, projected(pieces, separatorBytes(j)));
  return over === undefined ? good(j) : bad(over);
}

/** Builds a checked `join`. */
export function joined(j: Joining): Out {
  return text("join", j.pieces.join(j.separator));
}

function split(args: readonly JValue[]): Out {
  const op = "split";
  return binary(op, args, (a, b) => {
    const t = str(op, a);
    if (!t.ok) return t;
    const s = str(op, b);
    if (!s.ok) return s;
    if (s.value.length === 0) return bad(domain(op, "the separator must not be empty"));
    const pieces: JValue[] = [];
    for (const p of t.value.split(s.value)) {
      const x = text(op, p);
      if (!x.ok) return x;
      pieces.push(x.value);
    }
    return array(op, pieces);
  });
}

/** Code points of a string, elements of an array, bytes of a byte string, entries of an object. */
function length(v: JValue): Out {
  const op = "length";
  switch (v.kind) {
    case "text":
      return count(codePoints(v.value));
    case "array":
      return count(v.length);
    case "bytes":
      return count(v.length);
    case "map":
      return count(v.size);
    case "null":
    case "bool":
    case "int":
    case "rational":
      return bad(typeError(op, `cannot take the length of ${kindName(v)}`));
  }
}

// ---- collections

/** Objects merge left to right (last wins); otherwise arrays are concatenated and other values appended. */
function merge(args: readonly JValue[], heap: Heap): Out {
  const op = "merge";
  const maps = args.filter((v): v is AnyMap => v.kind === "map");
  if (maps.length !== args.length) return spreading(op, args);
  const [first, ...rest] = maps;
  if (first === undefined) return good(JMap.empty);
  if (rest.length === 0) return bounded(op, first);
  // the first map, then each later entry set over it (last wins) in O(log n), sharing the rest
  let acc = heap.own(first);
  for (const m of rest) {
    for (const [k, x] of heap.own(m).entries()) {
      const next = acc.set(k, x);
      if (next === undefined) return mapResult(op, undefined);
      acc = next;
    }
  }
  return mapResult(op, acc);
}

/** Substring, byte substring, or array membership; a null needle is never present. */
function contains(n: JValue, h: JValue, heap: Heap): Out {
  if (n.kind === "null") return good(FALSE);
  if (n.kind === "text" && h.kind === "text") return good(bool(containsText(h.value, n.value)));
  if (n.kind === "bytes" && h.kind === "bytes") return good(bool(containsBytes(h.bytes, n.bytes)));
  if (h.kind === "array") return good(bool(h.items.some((x) => equal(heap, x, n))));
  return bad(typeError("in", `cannot look for ${kindName(n)} in ${kindName(h)}`));
}

/** Whether every element of the first array is in the second; null is the empty set. */
function intersect(a: JValue, b: JValue, heap: Heap): Out {
  const op = "intersect";
  if (a.kind === "null") return good(bool(true));
  if (a.kind === "array" && b.kind === "null") return good(FALSE);
  const xs = arr(op, a);
  if (!xs.ok) return xs;
  const ys = arr(op, b);
  if (!ys.ok) return ys;
  const r = subset(heap, op, xs.value, ys.value);
  return r.ok ? good(bool(r.value)) : r;
}

/** `[array, start]` or `[array, start, end]`, with JavaScript bounds. */
function slice(args: readonly JValue[]): Out {
  const op = "slice";
  const [a = NULL, s = NULL, e] = args;
  if (args.length < 2 || args.length > 3) return bad(arity(op, "takes [array, start] or [array, start, end]"));
  const vs = arr(op, a);
  if (!vs.ok) return vs;
  const from = index(op, s);
  if (!from.ok) return from;
  const to = e === undefined ? undefined : index(op, e);
  if (to !== undefined && !to.ok) return to;
  const n = vs.value.length;
  const i = bound(from.value, n);
  const j = Math.max(to === undefined ? n : bound(to.value, n), i);
  return array(op, vs.value.slice(i, j));
}

// ---- objects

/** Keys, values or `[key, value]` pairs in code-point key order; null gives null. */
function listing(op: "keys" | "values" | "entries", args: readonly JValue[], heap: Heap): Out {
  return unary(op, args, (v) => {
    if (v.kind === "null") return good(NULL);
    const m = obj(op, v);
    if (!m.ok) return m;
    // in code-point order of the keys, the order of their UTF-8 bytes: the machine's own maps keep it
    const entries = m.value instanceof JMap ? m.value.entries() : heap.own(m.value).entries();
    const out: JValue[] = [];
    for (const [k, x] of entries) {
      if (op === "values") {
        out.push(x);
        continue;
      }
      const key = text(op, k);
      if (!key.ok) return key;
      if (op === "keys") out.push(key.value);
      else {
        const pair = array(op, [key.value, x]);
        if (!pair.ok) return pair;
        out.push(pair.value);
      }
    }
    return array(op, out);
  });
}

function get(args: readonly JValue[]): Out {
  const op = "get";
  const [m, k, d] = args;
  if (args.length < 2 || args.length > 3 || m === undefined || k === undefined) return bad(arity(op, "takes [object, key] or [object, key, default]"));
  const o = obj(op, m);
  if (!o.ok) return o;
  const key = str(op, k);
  if (!key.ok) return key;
  return good(o.value.get(key.value) ?? d ?? NULL);
}

/** Setting a key to null removes it: canonical maps never hold nulls. */
function set(args: readonly JValue[], heap: Heap): Out {
  const op = "set";
  const [m, k, v] = args;
  if (args.length !== 3 || m === undefined || k === undefined || v === undefined) return bad(arity(op, `takes [object, key, value], got ${args.length.toString()} arguments`));
  const o = obj(op, m);
  if (!o.ok) return o;
  const key = textKey(op, k);
  if (!key.ok) return key;
  // O(log n), sharing every untouched subtree; a null value removes the key
  return mapResult(op, heap.own(o.value).set(key.value, v));
}

// ---- bytes

/** Bytes, or text of the form `0x` + lowercase hex. */
function hexInput(op: StrictOp, v: JValue): Out<Uint8Array> {
  if (v.kind === "bytes") return good(v.bytes);
  if (v.kind === "text") {
    const b = hexBytes(v.value);
    return b === undefined ? bad(domain(op, "expected 0x followed by an even number of lowercase hex digits")) : good(b);
  }
  return expected(op, "bytes or hex text", v);
}

/** Hashes bytes, or the UTF-8 encoding of a string. */
function hashed(op: StrictOp, algorithm: HashAlgorithm, args: readonly JValue[]): Out {
  return unary(op, args, (v) => {
    // the bytes are a value's own copy, and the algorithm one of core's: hashed as they are
    if (v.kind === "bytes") return bytes(op, digestOf(algorithm, v.bytes));
    if (v.kind === "text") return bytes(op, digestOf(algorithm, v.utf8()));
    return expected(op, "bytes or a string", v);
  });
}

/** A big-endian unsigned integer, within the number bound: its significant bytes are counted before it is built. */
function unsignedInt(op: StrictOp, b: Uint8Array): Out {
  let first = 0;
  for (; first < b.length && b[first] === 0; first++);
  if ((b.length - first) * 8 > MAX_NUMBER_BITS) return bad(tooLarge(op));
  const n = first === b.length ? 0n : BigInt(`0x${toHexOf(b.subarray(first))}`);
  const r = integerOf(op, n);
  return r.ok ? num(op, r.value) : r;
}

/** Big-endian and unsigned: minimal (`0` is empty), or left-padded to a fixed width. */
function intToBytes(args: readonly JValue[]): Out {
  const op = "int_to_bytes";
  const unsigned = (v: JValue): Out<Uint8Array> => {
    const i = integer(op, v);
    if (!i.ok) return i;
    // core's minimal big-endian bytes (empty for 0), which refuse only a negative integer
    const m = cbor.magnitudeBytes(i.value.value);
    return m.ok ? good(m.value) : bad(domain(op, "negative integer"));
  };
  const [i, w] = args;
  if (args.length === 1 && i !== undefined) {
    const raw = unsigned(i);
    return raw.ok ? bytes(op, raw.value) : raw;
  }
  if (args.length !== 2 || i === undefined || w === undefined) return bad(arity(op, "takes [integer] or [integer, width]"));
  const raw = unsigned(i);
  if (!raw.ok) return raw;
  const n = integer(op, w);
  if (!n.ok) return n;
  if (n.value.value < 0n || n.value.value > BigInt(MAX_INT_BYTES)) return bad(domain(op, `the width must be in 0..${MAX_INT_BYTES.toString()}`));
  const width = Number(n.value.value);
  if (raw.value.length > width) return bad(domain(op, `the integer does not fit in ${width.toString()} bytes`));
  const out = new Uint8Array(width);
  out.set(raw.value, width - raw.value.length);
  return bytes(op, out);
}

/**
 * The strict operations, one handler each (`ops.rs` `apply`): for `+`, `*`, `cat` and `join`, whose charged steps the
 * machine runs itself, the uncharged reference.
 */
export const STRICT: Readonly<Record<StrictOp, Handler>> = Object.freeze({
  "!": (args) => unary("!", args, (v) => good(bool(!truthy(v)))),
  "!!": (args) => unary("!!", args, (v) => good(bool(truthy(v)))),
  "==": (args, heap) => binary("==", args, (a, b) => good(bool(equal(heap, a, b)))),
  "===": (args, heap) => binary("===", args, (a, b) => good(bool(equal(heap, a, b)))),
  "!=": (args, heap) => binary("!=", args, (a, b) => good(bool(!equal(heap, a, b)))),
  "!==": (args, heap) => binary("!==", args, (a, b) => good(bool(!equal(heap, a, b)))),
  "<": (args) => ordering("<", args, (c) => c < 0, true),
  "<=": (args) => ordering("<=", args, (c) => c <= 0, true),
  ">": (args) => ordering(">", args, (c) => c > 0, false),
  ">=": (args) => ordering(">=", args, (c) => c >= 0, false),
  "+": (args) => total("+", args),
  "*": (args) => total("*", args),
  min: (args) => fold("min", args, (c) => c <= 0),
  max: (args) => fold("max", args, (c) => c >= 0),
  "-": (args) => subtract("-", args),
  "/": (args) => numBinary("/", args, (a, b) => ratio.divide("/", a, b)),
  "%": (args) => numBinary("%", args, (a, b) => ratio.remainder("%", a, b)),
  pow: (args) => numBinary("pow", args, (a, b) => power("pow", a, b)),
  abs: (args) => numUnary("abs", args, (x) => ratio.absolute("abs", x)),
  round: (args) => numUnary("round", args, (x) => integerOf("round", ratio.roundHalfEven(x))),
  round_half_up: (args) => numUnary("round_half_up", args, (x) => integerOf("round_half_up", ratio.roundHalfUp(x))),
  floor: (args) => numUnary("floor", args, (x) => integerOf("floor", ratio.floor(x))),
  ceil: (args) => numUnary("ceil", args, (x) => integerOf("ceil", ratio.ceil(x))),
  cat: (args) => cat(args),
  substr: (args) => substr(args),
  lower: (args) =>
    unary("lower", args, (v) => {
      const s = str("lower", v);
      return s.ok ? text("lower", asciiCase(s.value, false)) : s;
    }),
  upper: (args) =>
    unary("upper", args, (v) => {
      const s = str("upper", v);
      return s.ok ? text("upper", asciiCase(s.value, true)) : s;
    }),
  trim: (args) =>
    unary("trim", args, (v) => {
      const s = str("trim", v);
      return s.ok ? text("trim", trim(s.value)) : s;
    }),
  startsWith: (args) => affix("startsWith", args),
  endsWith: (args) => affix("endsWith", args),
  join: (args) => {
    const j = joining(args);
    return j.ok ? joined(j.value) : j;
  },
  split: (args) => split(args),
  length: (args) => unary("length", args, length),
  merge: (args, heap) => merge(args, heap),
  in: (args, heap) => binary("in", args, (n, h) => contains(n, h, heap)),
  intersect: (args, heap) => binary("intersect", args, (a, b) => intersect(a, b, heap)),
  unique: (args, heap) =>
    unary("unique", args, (v) => {
      const vs = arr("unique", v);
      if (!vs.ok) return vs;
      const d = distinct(heap, "unique", vs.value);
      return d.ok ? array("unique", d.value) : d;
    }),
  reverse: (args) =>
    unary("reverse", args, (v) => {
      const vs = arr("reverse", v);
      return vs.ok ? array("reverse", [...vs.value].reverse()) : vs;
    }),
  flatten: (args) =>
    unary("flatten", args, (v) => {
      const vs = arr("flatten", v);
      return vs.ok ? spreading("flatten", vs.value) : vs;
    }),
  slice: (args) => slice(args),
  count: (args) =>
    unary("count", args, (v) => {
      const vs = arr("count", v);
      return vs.ok ? count(vs.value.length) : vs;
    }),
  keys: (args, heap) => listing("keys", args, heap),
  values: (args, heap) => listing("values", args, heap),
  entries: (args, heap) => listing("entries", args, heap),
  get: (args) => get(args),
  has: (args) =>
    binary("has", args, (m, k) => {
      const o = obj("has", m);
      if (!o.ok) return o;
      const key = str("has", k);
      return key.ok ? good(bool(o.value.get(key.value) !== undefined)) : key;
    }),
  set: (args, heap) => set(args, heap),
  unset: (args, heap) =>
    binary("unset", args, (m, k) => {
      const o = obj("unset", m);
      if (!o.ok) return o;
      const key = textKey("unset", k);
      // the map it builds is a result, bounded as set's is (jlvm.md section 2): removing a key from a host map heavier
      // than the bound leaves a map that may be heavier still
      return key.ok ? bounded("unset", heap.own(o.value).removed(key.value)) : key;
    }),
  exists: (args) => good(bool(!args.some((v) => v.kind === "null"))),
  typeof: (args) => unary("typeof", args, (v) => text("typeof", kindName(v))),
  hex: (args) =>
    unary("hex", args, (v) => {
      if (v.kind !== "bytes") return expected("hex", "bytes", v);
      // the text is twice as long as the bytes, so its weight is checked before it is built
      if (BigInt(2 * v.length + 3) > MAX_RESULT_WEIGHT) return bad(limit(`hex: the result would exceed weight ${MAX_RESULT_WEIGHT.toString()}`));
      return text("hex", `0x${toHexOf(v.bytes)}`);
    }),
  unhex: (args) =>
    unary("unhex", args, (v) => {
      const b = hexInput("unhex", v);
      return b.ok ? bytes("unhex", b.value) : b;
    }),
  hex_to_int: (args) =>
    unary("hex_to_int", args, (v) => {
      const b = hexInput("hex_to_int", v);
      return b.ok ? unsignedInt("hex_to_int", b.value) : b;
    }),
  bytes_to_int: (args) =>
    unary("bytes_to_int", args, (v) => {
      const b = rawBytes("bytes_to_int", v);
      return b.ok ? unsignedInt("bytes_to_int", b.value) : b;
    }),
  int_to_bytes: (args) => intToBytes(args),
  sha256: (args) => hashed("sha256", "SHA-256", args),
  sha3_256: (args) => hashed("sha3_256", "SHA3-256", args),
  keccak256: (args) => hashed("keccak256", "Keccak-256", args),
  blake2b256: (args) => hashed("blake2b256", "BLAKE2b-256", args),
});
