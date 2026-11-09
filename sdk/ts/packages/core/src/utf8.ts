/**
 * UTF-8 and UTF-16 well-formedness, exact: `TextEncoder` silently replaces lone surrogates with U+FFFD and
 * `TextDecoder` drops a leading byte-order mark, either of which would change bytes without an error, so every result
 * here is the one the hand-written forms below give.
 *
 * Where the host has them, the engine's `String.prototype.isWellFormed` and the host's `TextEncoder` and `TextDecoder`
 * are read once when this module loads, each kept only if it agrees with the hand-written form on a probe (so neither
 * a later change to them nor a host that differs can change a result), and used only where they cannot differ: the
 * encoder on text already found well-formed, the decoder on bytes already found to be ASCII. They make long text
 * faster; short text stays by hand, where a call to the host costs more than the work.
 */

import { isBytes, isStableBuffer, ownBytes, UNBOUNDED } from "./bytes.js";
import type { TypeMismatch } from "./error.js";
import { err, ok, type Result } from "./result.js";

const isHigh = (u: number): boolean => u >= 0xd800 && u <= 0xdbff;
const isLow = (u: number): boolean => u >= 0xdc00 && u <= 0xdfff;

/** Whether `s` holds a surrogate pair at `i` (past the end, `charCodeAt` is `NaN`, no low surrogate). */
const pairAt = (s: string, i: number): boolean => isHigh(s.charCodeAt(i)) && isLow(s.charCodeAt(i + 1));

const NO_ARGUMENTS: readonly unknown[] = Object.freeze([]);

/** Whether every surrogate in `s` is part of a high/low pair. */
export function isWellFormed(s: string): boolean {
  return s.length >= HOST_CHECK_FROM && engineWellFormed !== undefined ? engineWellFormed(s) : wellFormedByHand(s);
}

function wellFormedByHand(s: string): boolean {
  for (let i = 0; i < s.length; i++) {
    const u = s.charCodeAt(i);
    if (u < 0xd800 || u > 0xdfff) continue;
    if (!pairAt(s, i)) return false;
    i++;
  }
  return true;
}

/**
 * The UTF-8 encoding of well-formed text (check with {@link isWellFormed} first). An unpaired surrogate is written as
 * the three bytes of its own code point, never as U+FFFD: bytes that no well-formed text encodes to, so a key holding
 * one matches no key a map holds.
 */
export function encodeUtf8(s: string): Uint8Array {
  return (s.length >= HOST_ENCODE_FROM && hostEncode !== undefined && isWellFormed(s) ? hostEncode(s) : undefined) ?? encodeByHand(s);
}

/** {@link encodeUtf8} by hand: the length first, then each code point written once. */
function encodeByHand(s: string): Uint8Array {
  const out = new Uint8Array(utf8LengthOf(s));
  for (let i = 0, j = 0; i < s.length; i++) {
    const u = s.charCodeAt(i);
    // a pair's code point, read at its high surrogate; any other unit, an unpaired surrogate too, is its own
    const pair = pairAt(s, i);
    j = put(out, j, pair ? 0x10000 + ((u - 0xd800) << 10) + (s.charCodeAt(i + 1) - 0xdc00) : u);
    if (pair) i++;
  }
  return out;
}

/** Writes the code point `c` in UTF-8 at `out[j]`, and gives the index after it. */
function put(out: Uint8Array, j: number, c: number): number {
  if (c < 0x80) {
    out[j] = c;
    return j + 1;
  }
  if (c < 0x800) {
    out[j] = 0xc0 | (c >> 6);
    out[j + 1] = 0x80 | (c & 0x3f);
    return j + 2;
  }
  if (c < 0x10000) {
    out[j] = 0xe0 | (c >> 12);
    out[j + 1] = 0x80 | ((c >> 6) & 0x3f);
    out[j + 2] = 0x80 | (c & 0x3f);
    return j + 3;
  }
  out[j] = 0xf0 | (c >> 18);
  out[j + 1] = 0x80 | ((c >> 12) & 0x3f);
  out[j + 2] = 0x80 | ((c >> 6) & 0x3f);
  out[j + 3] = 0x80 | (c & 0x3f);
  return j + 4;
}

/**
 * The UTF-8 length of text, without encoding it; an unpaired surrogate counts 3, as its replacement U+FFFD does. A value
 * that is not a string (from a JavaScript caller, or a cast) is a `TypeMismatch`, never iterated.
 */
export function utf8Length(s: string): Result<number, TypeMismatch> {
  return typeof (s as unknown) === "string" ? ok(utf8LengthOf(s)) : err({ kind: "TypeMismatch", reason: "expected a string" });
}

/**
 * {@link utf8Length} of a string this SDK holds, unchecked: the length of {@link encodeUtf8}'s bytes, counted from the
 * UTF-16 units in one pass. Internal to this SDK, through `@kd5ujc/dilf4s-core/internal`.
 */
export function utf8LengthOf(s: string): number {
  // one byte per unit, counted already: a unit below U+0800 adds one more, a pair (two units, four bytes) two, and any
  // other unit (an unpaired surrogate too) two
  let n = s.length;
  for (let i = 0; i < s.length; i++) {
    const u = s.charCodeAt(i);
    if (u < 0x80) continue;
    if (u < 0x800) n += 1;
    else {
      n += 2;
      if (pairAt(s, i)) i++;
    }
  }
  return n;
}

const byteAt = (a: Uint8Array, i: number): number => a[i] ?? -1;
const cont = (a: Uint8Array, i: number): boolean => (byteAt(a, i) & 0xc0) === 0x80 && byteAt(a, i) >= 0;
const within = (a: Uint8Array, i: number, lo: number, hi: number): boolean => byteAt(a, i) >= lo && byteAt(a, i) <= hi;

/** The width of the well-formed UTF-8 sequence at `i` (RFC 3629: shortest form, no surrogates), or 0. */
function sequenceWidth(a: Uint8Array, i: number): number {
  const b = byteAt(a, i);
  if (b < 0) return 0;
  if (b < 0x80) return 1;
  if (b >= 0xc2 && b <= 0xdf && cont(a, i + 1)) return 2;
  if (b === 0xe0 && within(a, i + 1, 0xa0, 0xbf) && cont(a, i + 2)) return 3;
  if (((b >= 0xe1 && b <= 0xec) || b === 0xee || b === 0xef) && cont(a, i + 1) && cont(a, i + 2)) return 3;
  if (b === 0xed && within(a, i + 1, 0x80, 0x9f) && cont(a, i + 2)) return 3;
  if (b === 0xf0 && within(a, i + 1, 0x90, 0xbf) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  if (b >= 0xf1 && b <= 0xf3 && cont(a, i + 1) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  if (b === 0xf4 && within(a, i + 1, 0x80, 0x8f) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  return 0;
}

/** Whether every byte of `a` is ASCII. */
function isAscii(a: Uint8Array): boolean {
  for (let i = 0; i < a.length; i++) if (byteAt(a, i) >= 0x80) return false;
  return true;
}

/**
 * The text of well-formed UTF-8, or `undefined` if `a` is not well-formed, or not a `Uint8Array` at all (from a
 * JavaScript caller, or a cast). The bytes pass through core's byte boundary (`ownBytes`, unbounded): measured by their own slots
 * (a subclass's `length` is not believed), and a view of stable memory or else one copy, so the validation and the
 * decoding read one snapshot.
 */
export function decodeUtf8(a: Uint8Array): string | undefined {
  // text of any length decodes
  const bytes = ownBytes(a, UNBOUNDED);
  return bytes === undefined || typeof bytes === "number" ? undefined : utf8Text(bytes);
}

/**
 * {@link decodeUtf8} of this package's own plain bytes, unchecked: for the CBOR decoder. ASCII is decoded by the host's
 * `TextDecoder` when its memory is stable ({@link isStableBuffer}: this realm's own, unshared, non-resizable
 * `ArrayBuffer`, which a decoder takes as it is), as ASCII holds no byte-order mark and nothing to replace; anything
 * else by hand.
 */
export function utf8Text(a: Uint8Array): string | undefined {
  return (a.length >= HOST_DECODE_FROM && hostDecode !== undefined && isAscii(a) && isStableBuffer(a) ? hostDecode(a) : undefined) ?? textByHand(a);
}

/** The UTF-16 units {@link textByHand} gathers before making them one string. */
const UNIT_CHUNK = 4096;

/**
 * The text of `a`, or `undefined` if it is not well-formed UTF-8, by hand, in one pass: each sequence checked whole
 * ({@link sequenceWidth}, an ASCII byte alone) and kept as its UTF-16 units, each chunk of them made one string.
 */
function textByHand(a: Uint8Array): string | undefined {
  const parts: string[] = [];
  const units: number[] = [];
  for (let i = 0; i < a.length; ) {
    const b0 = byteAt(a, i);
    const w = b0 >= 0 && b0 < 0x80 ? 1 : sequenceWidth(a, i);
    if (w === 0) return undefined;
    const c =
      w === 1
        ? b0
        : w === 2
          ? ((b0 & 0x1f) << 6) | (byteAt(a, i + 1) & 0x3f)
          : w === 3
            ? ((b0 & 0x0f) << 12) | ((byteAt(a, i + 1) & 0x3f) << 6) | (byteAt(a, i + 2) & 0x3f)
            : ((b0 & 0x07) << 18) | ((byteAt(a, i + 1) & 0x3f) << 12) | ((byteAt(a, i + 2) & 0x3f) << 6) | (byteAt(a, i + 3) & 0x3f);
    if (c < 0x10000) units.push(c);
    else units.push(0xd800 + ((c - 0x10000) >> 10), 0xdc00 + ((c - 0x10000) & 0x3ff));
    if (units.length >= UNIT_CHUNK) parts.push(String.fromCharCode(...units.splice(0)));
    i += w;
  }
  parts.push(String.fromCharCode(...units));
  return parts.join("");
}

/**
 * Code-point order of two strings (the order of their UTF-8 bytes): UTF-16 order, except that a surrogate pair (a code
 * point past U+FFFF) sorts after every other unit. Exact for any strings, an unpaired surrogate read as the code point
 * of its own unit, as iterating the string reads it. Linear in the common prefix, read unit by unit in place, and O(1)
 * for the same string. Internal to this SDK, through `@kd5ujc/dilf4s-core/internal`.
 */
export function compareCodePoints(a: string, b: string): number {
  // the engine's own comparison finds equal text (often the same string) without a loop here
  if (a === b) return 0;
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const x = a.charCodeAt(i);
    const y = b.charCodeAt(i);
    if (x !== y) return differing(a, b, i, x, y);
  }
  // one is a prefix of the other, which is then less, its code points too: where it ends in a high surrogate, alone,
  // the other's next unit may pair with it, past U+FFFF
  return a.length - b.length;
}

/**
 * The order of `a` and `b` at `i`, where their units first differ (`x` and `y`), by the code points holding them. After
 * a high surrogate both hold, a low surrogate completes a pair, past U+FFFF, where the other's high surrogate stands
 * alone, at most U+DBFF; two lows complete two pairs, ordered by their lows. Otherwise both code points start at `i`.
 */
function differing(a: string, b: string, i: number, x: number, y: number): number {
  const after = i > 0 && isHigh(a.charCodeAt(i - 1));
  const pairA = after && isLow(x);
  const pairB = after && isLow(y);
  if (pairA || pairB) return pairA && pairB ? x - y : pairA ? 1 : -1;
  return (a.codePointAt(i) ?? x) - (b.codePointAt(i) ?? y);
}

/** Bytewise order of two byte strings: unsigned, a prefix first. Internal to this SDK, through `@kd5ujc/dilf4s-core/internal`. */
export function compareBytes(a: Uint8Array, b: Uint8Array): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const d = (a[i] ?? 0) - (b[i] ?? 0);
    if (d !== 0) return d;
  }
  return a.length - b.length;
}

// ---------------------------------------------------------------------------------------------- the host's own forms

/** Text at least this long, in UTF-16 units, is checked by the engine's `isWellFormed`: below it the call costs more. */
const HOST_CHECK_FROM = 8;

/**
 * Text at least this long, in UTF-16 units, is encoded by the host's `TextEncoder`. Each call to it costs about a
 * microsecond whatever the length, as its output is a new buffer kept off the heap (V8 keeps a typed array of more
 * than 64 bytes there, so a hand-encoded one of that size costs as much); below this, by hand is no slower.
 */
const HOST_ENCODE_FROM = 256;

/**
 * ASCII at least this long, in bytes, is decoded by the host's `TextDecoder`: below it, by hand is no slower. Other
 * UTF-8 always is by hand, which is as fast as the host's there.
 */
const HOST_DECODE_FROM = 16;

/** Text of every UTF-8 width and at each width's bounds, a byte-order mark first: what the host's forms must agree on. */
const PROBE = "\ufeff\u0000\u007f\u0080\u07ff\u0800\ud7ff\ue000\ufffd\uffff\u{10000}\u{10ffff}";

/** Text with and without unpaired surrogates, where {@link wellFormedByHand} and the engine must agree. */
const WELL_FORMED_PROBES: readonly string[] = Object.freeze([PROBE, "\ud800", "\udfff", "a\udbff", "\udc00\ud800", "\ud800\u{10000}", `${PROBE}\ud800${PROBE}`]);

/**
 * `new globalThis[name](...args)` and its prototype's own `method`, called on it with one argument, read once when this
 * module loads: `undefined` unless the host has a constructor by that name with that method.
 */
function hostMethod(name: string, args: readonly unknown[], method: string): ((x: unknown) => unknown) | undefined {
  const C: unknown = Reflect.get(globalThis, name);
  const proto: unknown = typeof C === "function" ? Reflect.get(C, "prototype") : undefined;
  const m: unknown = typeof proto === "object" && proto !== null ? Reflect.get(proto, method) : undefined;
  if (typeof C !== "function" || typeof m !== "function") return undefined;
  const self: unknown = Reflect.construct(C, args);
  return (x) => {
    const r: unknown = Reflect.apply(m, self, [x]);
    return r;
  };
}

/** The engine's `String.prototype.isWellFormed` (ES2024), if it has it and it agrees with {@link wellFormedByHand}. */
const engineWellFormed = ((): ((s: string) => boolean) | undefined => {
  const f: unknown = Reflect.get(String.prototype, "isWellFormed");
  if (typeof f !== "function") return undefined;
  const check = (s: string): boolean => Reflect.apply(f, s, NO_ARGUMENTS) === true;
  return WELL_FORMED_PROBES.every((s) => check(s) === wellFormedByHand(s)) ? check : undefined;
})();

/** The host's `TextEncoder#encode`, if it has one and it agrees with {@link encodeByHand}. */
const hostEncode = ((): ((s: string) => Uint8Array | undefined) | undefined => {
  const encode = hostMethod("TextEncoder", NO_ARGUMENTS, "encode");
  if (encode === undefined) return undefined;
  const bytes = (s: string): Uint8Array | undefined => {
    const out = encode(s);
    return isBytes(out) ? out : undefined;
  };
  const probe = bytes(PROBE);
  return probe !== undefined && compareBytes(probe, encodeByHand(PROBE)) === 0 ? bytes : undefined;
})();

/** The host's `TextDecoder#decode` of UTF-8, keeping a byte-order mark, if it has one and it agrees with {@link textByHand}. */
const hostDecode = ((): ((a: Uint8Array) => string | undefined) | undefined => {
  const decode = hostMethod("TextDecoder", ["utf-8", Object.freeze({ fatal: false, ignoreBOM: true })], "decode");
  if (decode === undefined) return undefined;
  const text = (a: Uint8Array): string | undefined => {
    const s = decode(a);
    return typeof s === "string" ? s : undefined;
  };
  const probe = encodeByHand(PROBE);
  return text(probe) === textByHand(probe) ? text : undefined;
})();
