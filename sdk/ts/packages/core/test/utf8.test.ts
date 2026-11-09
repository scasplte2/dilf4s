import { describe, expect, it } from "vitest";

import { cbor, codec, decodeUtf8, MapValue, Preview, utf8Length, Value } from "../src/index.js";
import { compareBytes, compareCodePoints, utf8LengthOf } from "../src/internal.js";
import { encodeUtf8, isWellFormed, utf8Text } from "../src/utf8.js";
import { expectOk } from "./vectors.js";

/**
 * Core's UTF-8 helpers against references written independently of them: the code points `for...of` reads, the host's
 * `TextEncoder` for well-formed text and its fatal `TextDecoder` for bytes. Random text mixes every UTF-8 width, each
 * width's bounds, and surrogates that pair or stand alone (anywhere, the first and the last unit included), at lengths
 * around each switch between a hand-written form and the host's (8 and 256 UTF-16 units, 16 bytes).
 */

/** A seeded generator (mulberry32), so every run checks the same cases. */
function rng(seed: number): () => number {
  let a = seed;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const pick = <T>(r: () => number, xs: readonly T[]): T => xs[Math.floor(r() * xs.length)] ?? (xs[0] as T);

/** Code points at the bounds of each UTF-8 width, and the byte-order mark. */
const BOUNDS = [0x00, 0x7f, 0x80, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xfeff, 0xfffd, 0xffff, 0x10000, 0x10ffff];

/** One piece of random text: a code point of each width, a bound, or a surrogate unit, which may pair or stand alone. */
function piece(r: () => number, surrogates: boolean): string {
  const x = r();
  if (x < 0.3) return String.fromCharCode(0x20 + Math.floor(r() * 0x5f));
  if (x < 0.45) return String.fromCharCode(0x80 + Math.floor(r() * 0x780));
  if (x < 0.6) {
    const c = 0x800 + Math.floor(r() * 0xf000);
    return String.fromCharCode(c >= 0xd800 ? c + 0x800 : c);
  }
  if (x < 0.75) return String.fromCodePoint(0x10000 + Math.floor(r() * 0x100000));
  if (x < 0.9 || !surrogates) return String.fromCodePoint(pick(r, BOUNDS));
  return String.fromCharCode(0xd800 + Math.floor(r() * 0x800));
}

/** The lengths text is drawn at: each side of every switch, and longer. */
const LENGTHS = [0, 1, 2, 3, 7, 8, 9, 15, 16, 17, 31, 64, 127, 128, 255, 256, 257, 300, 511, 512, 1000];

/**
 * Random text of exactly `n` UTF-16 units, cut wherever `n` falls (so a pair may be cut too), or of well-formed text
 * at least `n` long when `surrogates` is false.
 */
function text(r: () => number, n: number, surrogates: boolean): string {
  let s = "";
  while (s.length < n) s += piece(r, surrogates);
  return surrogates ? s.slice(0, n) : s;
}

const isSurrogate = (u: number): boolean => u >= 0xd800 && u <= 0xdfff;

/** The code points of `s`, as `for...of` reads them: an unpaired surrogate is its own. */
const codePoints = (s: string): number[] => Array.from(s, (ch) => ch.codePointAt(0) ?? -1);

const referenceWellFormed = (s: string): boolean => codePoints(s).every((c) => !isSurrogate(c));

const encoder = new TextEncoder();

/**
 * Generalized UTF-8: each run of well-formed text as `TextEncoder` writes it, and each unpaired surrogate as the three
 * bytes of its own value (or, with `replaced`, as `TextEncoder` writes it: U+FFFD).
 */
function referenceEncode(s: string, replaced = false): number[] {
  const out: number[] = [];
  let run = "";
  for (const ch of s) {
    const c = ch.codePointAt(0) ?? -1;
    if (!isSurrogate(c)) {
      run += ch;
      continue;
    }
    out.push(...encoder.encode(run), ...(replaced ? [0xef, 0xbf, 0xbd] : [0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f)]));
    run = "";
  }
  out.push(...encoder.encode(run));
  return out;
}

/** The order of the code-point sequences. */
function referenceOrder(a: string, b: string): number {
  const [x, y] = [codePoints(a), codePoints(b)];
  for (let i = 0; i < Math.min(x.length, y.length); i++) if (x[i] !== y[i]) return Math.sign((x[i] ?? 0) - (y[i] ?? 0));
  return Math.sign(x.length - y.length);
}

/** The JLVM's own order before it took core's (`jvalue.ts`): UTF-16 order with the surrogates moved past U+FFFF. */
function previousJlvmOrder(a: string, b: string): number {
  const fixup = (c: number): number => (c >= 0xd800 ? (c >= 0xe000 ? c - 0x800 : c + 0x2000) : c);
  for (let i = 0; i < Math.min(a.length, b.length); i++) if (a.charCodeAt(i) !== b.charCodeAt(i)) return Math.sign(fixup(a.charCodeAt(i)) - fixup(b.charCodeAt(i)));
  return Math.sign(a.length - b.length);
}

/** The JLVM's own UTF-8 length before it took core's: 2 per surrogate unit, so exact only for well-formed text. */
function previousJlvmLength(s: string): number {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    n += c < 0x80 ? 1 : c < 0x800 ? 2 : isSurrogate(c) ? 2 : 3;
  }
  return n;
}

const fatal = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

/** Strict UTF-8, as the host's fatal decoder reads it (a byte-order mark kept), or `undefined` where it refuses. */
function referenceDecode(b: Uint8Array): string | undefined {
  try {
    return fatal.decode(b);
  } catch {
    return undefined;
  }
}

/** Random text: every length, with and without surrogates, a fixed number of each, and the edge strings. */
function texts(seed: number, perLength: number, surrogates: boolean): string[] {
  const r = rng(seed);
  return LENGTHS.flatMap((n) => Array.from({ length: perLength }, () => text(r, n, surrogates)));
}

const EDGES: readonly string[] = [
  "",
  "\ud800",
  "\udc00",
  "\u{10ffff}",
  "\udc00\ud800",
  "\ud800\u{10000}",
  "\u{10000}\udc00",
  "a\ud800",
  "\udfffa",
  `${"a".repeat(255)}\u{10000}`,
  `${"a".repeat(300)}\ud800`,
  `\udc00${"a".repeat(300)}`,
  `${"\u00e9".repeat(200)}\udbff`,
  "\u{10000}".repeat(200),
  `\ufeff${"a".repeat(300)}`,
];

describe("core's UTF-8 helpers agree with independent references", { timeout: 60_000 }, () => {
  const all = [...EDGES, ...texts(1, 24, true), ...texts(2, 12, false)];

  it("covers every width, both kinds of surrogate, pairs and lone ones, and both sides of every switch", () => {
    const cps = all.flatMap(codePoints);
    expect(cps.some((c) => c < 0x80), "1-byte").toBe(true);
    expect(cps.some((c) => c >= 0x80 && c < 0x800), "2-byte").toBe(true);
    expect(cps.some((c) => c >= 0x800 && c < 0x10000 && !isSurrogate(c)), "3-byte").toBe(true);
    expect(cps.some((c) => c >= 0x10000), "4-byte").toBe(true);
    expect(cps.some((c) => c >= 0xd800 && c <= 0xdbff), "a lone high surrogate").toBe(true);
    expect(cps.some((c) => c >= 0xdc00 && c <= 0xdfff), "a lone low surrogate").toBe(true);
    for (const c of BOUNDS) expect(cps.includes(c), `the bound U+${c.toString(16)}`).toBe(true);
    const ill = all.filter((s) => !referenceWellFormed(s));
    expect(ill.some((s) => s.length >= 256), "long text with a lone surrogate").toBe(true);
    expect(all.some((s) => s.length >= 256 && referenceWellFormed(s)), "long well-formed text").toBe(true);
    expect(ill.some((s) => isSurrogate(s.charCodeAt(s.length - 1)) && s.length > 256), "a lone surrogate last, in long text").toBe(true);
  });

  it("isWellFormed is whether every code point for...of reads is a scalar value", () => {
    for (const s of all) expect(isWellFormed(s), JSON.stringify(s.slice(0, 40))).toBe(referenceWellFormed(s));
  });

  it("encodeUtf8 is TextEncoder's UTF-8 for well-formed text, and an unpaired surrogate's own three bytes otherwise", () => {
    for (const s of all) {
      const got = [...encodeUtf8(s)];
      const what = `${s.length.toString()} units: ${JSON.stringify(s.slice(0, 24))}`;
      expect(got, what).toEqual(referenceEncode(s));
      // TextEncoder differs only where a surrogate stands alone, which it writes as U+FFFD
      expect([...encoder.encode(s)], `${what}, TextEncoder`).toEqual(referenceEncode(s, true));
      if (referenceWellFormed(s)) expect(got, `${what}, well-formed`).toEqual([...encoder.encode(s)]);
    }
  });

  it("utf8LengthOf is the length of the encoding, an unpaired surrogate counting 3, and the JLVM's old count on well-formed text", () => {
    for (const s of all) {
      const n = referenceEncode(s).length;
      expect(utf8LengthOf(s), JSON.stringify(s.slice(0, 40))).toBe(n);
      expect(utf8Length(s), "the public form").toEqual({ ok: true, value: n });
      if (referenceWellFormed(s)) expect(previousJlvmLength(s), "the JLVM's old count").toBe(n);
    }
  });

  it("Preview keeps the first 32 code points, a lone surrogate as U+FFFD, and the whole text's UTF-8 length", () => {
    for (const s of all) {
      const p = Preview.of(s);
      const cps = codePoints(s);
      const head = cps.slice(0, 32);
      expect(p.head, JSON.stringify(s.slice(0, 40))).toBe(String.fromCodePoint(...head.map((c) => (isSurrogate(c) ? 0xfffd : c))));
      expect(p.length).toBe(referenceEncode(s).length);
      expect(p.truncated).toBe(cps.length > 32);
    }
  });

  it("compareCodePoints orders by code point for every pair of strings over the surrogates' bounds, up to three units long", () => {
    const alphabet = ["a", "\u07ff", "\ud7ff", "\ud800", "\udbff", "\udc00", "\udfff", "\ue000", "\uffff"];
    const strings = [""];
    for (let k = 0; k < 3; k++) for (const s of strings.filter((x) => x.length === k)) for (const u of alphabet) strings.push(s + u);
    expect(strings.length, "every string of up to three units").toBe(1 + 9 + 81 + 729);
    const wrong: string[] = [];
    for (const a of strings) {
      for (const b of strings) {
        const got = Math.sign(compareCodePoints(a, b));
        if (got !== referenceOrder(a, b)) wrong.push(`${JSON.stringify(a)} ${JSON.stringify(b)}`);
        if (referenceWellFormed(a) && referenceWellFormed(b) && got !== previousJlvmOrder(a, b)) wrong.push(`JLVM ${JSON.stringify(a)} ${JSON.stringify(b)}`);
      }
    }
    expect(wrong).toEqual([]);
  });

  it("compareCodePoints orders random text by code point, and well-formed text as its UTF-8 bytes and as the JLVM did", () => {
    const r = rng(3);
    for (const a of all) {
      // b shares a prefix with a, so the first difference falls anywhere, a pair's halves included
      const cut = Math.floor(r() * (a.length + 1));
      const b = a.slice(0, cut) + text(r, Math.floor(r() * 4), true);
      const what = `${JSON.stringify(a.slice(Math.max(0, cut - 4), cut + 4))} at ${cut.toString()}`;
      expect(Math.sign(compareCodePoints(a, b)), what).toBe(referenceOrder(a, b));
      expect(Math.sign(compareCodePoints(b, a)), `${what}, reversed`).toBe(referenceOrder(b, a));
      expect(compareCodePoints(a, a.slice(0)), "the same text").toBe(0);
      if (referenceWellFormed(a) && referenceWellFormed(b)) {
        expect(Math.sign(compareCodePoints(a, b)), `${what}, as bytes`).toBe(Math.sign(compareBytes(encoder.encode(a), encoder.encode(b))));
        expect(Math.sign(compareCodePoints(a, b)), `${what}, as the JLVM did`).toBe(previousJlvmOrder(a, b));
      }
    }
  });

  it("compareBytes is unsigned bytewise order, a prefix first", () => {
    const r = rng(4);
    for (let k = 0; k < 2000; k++) {
      const a = Uint8Array.from({ length: Math.floor(r() * 6) }, () => pick(r, [0x00, 0x01, 0x7f, 0x80, 0xff]));
      const b = Uint8Array.from({ length: Math.floor(r() * 6) }, () => pick(r, [0x00, 0x01, 0x7f, 0x80, 0xff]));
      const want = Math.sign(Buffer.compare(a, b));
      expect(Math.sign(compareBytes(a, b)), `${a.join(",")} ${b.join(",")}`).toBe(want);
    }
  });
});

/** One piece of random bytes: valid sequences of each width, a byte-order mark, and every kind of malformed sequence. */
function bytePiece(r: () => number): number[] {
  const x = r();
  if (x < 0.5) return referenceEncode(piece(r, false));
  if (x < 0.55) return [0xef, 0xbb, 0xbf];
  const cont = (): number => 0x80 + Math.floor(r() * 0x40);
  return pick(r, [
    [0x80 + Math.floor(r() * 0x40)], // a lone continuation byte
    [pick(r, [0xc0, 0xc1]), cont()], // an overlong 2-byte form
    [0xe0, 0x80 + Math.floor(r() * 0x20), cont()], // an overlong 3-byte form
    [0xed, 0xa0 + Math.floor(r() * 0x20), cont()], // a surrogate
    [0xf0, 0x80 + Math.floor(r() * 0x10), cont(), cont()], // an overlong 4-byte form
    [0xf4, 0x90 + Math.floor(r() * 0x30), cont(), cont()], // past U+10FFFF
    [0xf5 + Math.floor(r() * 11)], // a byte no UTF-8 holds
    [0xe1 + Math.floor(r() * 0xc), cont()], // a sequence cut short
    [0xf1, cont(), cont()],
  ]);
}

/** `ArrayBuffer` with ES2024's resizable form, which the ES2022 library types do not declare. */
const Resizable = ArrayBuffer as unknown as new (length: number, options: { maxByteLength: number }) => ArrayBuffer;

describe("utf8Text and decodeUtf8 agree with the host's strict decoder", { timeout: 60_000 }, () => {
  it("round-trips the text encodeUtf8 writes, through each path", () => {
    for (const s of texts(5, 12, false)) {
      const b = encodeUtf8(s);
      expect(utf8Text(b), JSON.stringify(s.slice(0, 40))).toBe(s);
      expect(decodeUtf8(b)).toBe(s);
    }
  });

  it("decodes random bytes, valid or not, as the fatal TextDecoder does: a byte-order mark kept, anything malformed refused", () => {
    const r = rng(6);
    const cases: Uint8Array[] = [];
    for (const n of [0, 1, 2, 3, 4, 15, 16, 17, 40, 300]) {
      for (let k = 0; k < 40; k++) {
        const bytes: number[] = [];
        // mostly valid, so a malformed sequence falls anywhere
        while (bytes.length < n) bytes.push(...(r() < 0.85 ? referenceEncode(piece(r, false)) : bytePiece(r)));
        cases.push(Uint8Array.from(bytes.slice(0, n)));
      }
      // ASCII alone, which the host decodes, and ASCII with one other byte at either end
      const ascii = Array.from({ length: n }, () => Math.floor(r() * 0x80));
      cases.push(Uint8Array.from(ascii), Uint8Array.from([...ascii, 0xc3, 0xa9]), Uint8Array.from([0xef, 0xbb, 0xbf, ...ascii]), Uint8Array.from([...ascii, 0x80]));
    }
    const refused = cases.filter((b) => referenceDecode(b) === undefined).length;
    expect(refused, "some cases are malformed").toBeGreaterThan(50);
    expect(cases.length - refused, "and some are not").toBeGreaterThan(50);
    for (const b of cases) {
      const what = `[${[...b.subarray(0, 24)].map((x) => x.toString(16)).join(" ")}] of ${b.length.toString()}`;
      expect(utf8Text(b), what).toBe(referenceDecode(b));
      expect(decodeUtf8(b), what).toBe(referenceDecode(b));
    }
  });

  it("ASCII in memory that is shared or resizable decodes the same: decodeUtf8 copies it, the CBOR decoder decodes it by hand", () => {
    const ascii = Uint8Array.from({ length: 40 }, (_, i) => 0x41 + (i % 26));
    const want = String.fromCharCode(...ascii);
    const shared = new Uint8Array(new SharedArrayBuffer(40));
    shared.set(ascii);
    expect(decodeUtf8(shared), "shared memory").toBe(want);
    const resizable = new Uint8Array(new Resizable(40, { maxByteLength: 80 }));
    resizable.set(ascii);
    expect(decodeUtf8(resizable), "a resizable buffer").toBe(want);
    expect(utf8Text(resizable), "a resizable buffer, read in place").toBe(want);
    const item = new Uint8Array(new Resizable(42, { maxByteLength: 84 }));
    item.set([0x78, 40, ...ascii]);
    const v = expectOk(cbor.decode(item), "a text item in a resizable buffer");
    expect(v.kind === "text" ? v.value : undefined).toBe(want);
  });
});

describe("an unpaired surrogate never finds the U+FFFD key TextEncoder would write it as", () => {
  const text = (s: string): Value => expectOk(Value.text(s), "text");

  it("in a map, and in a codec's text map, for short and long keys", () => {
    for (const tail of ["", "a".repeat(300)]) {
      const replaced = `\ufffd${tail}`;
      const m = expectOk(MapValue.of([[replaced, text("found")]]), "a map");
      expect(m.get(replaced), "the key itself").toEqual(text("found"));
      expect(m.get(`\ud800${tail}`), "a lone high surrogate").toBeUndefined();
      expect(m.get(`\udfff${tail}`), "a lone low surrogate").toBeUndefined();
      const decoded = expectOk(codec.textMap(codec.text).decode(m), "a text map");
      expect(decoded.get(replaced)).toBe("found");
      expect(decoded.get(`\ud800${tail}`), "in a codec's text map").toBeUndefined();
    }
  });

  it("whatever String.prototype.isWellFormed or TextEncoder become once core has loaded", () => {
    const savedCheck = Object.getOwnPropertyDescriptor(String.prototype, "isWellFormed");
    const savedEncode = Object.getOwnPropertyDescriptor(TextEncoder.prototype, "encode");
    expect(savedCheck, "the engine has isWellFormed").toBeDefined();
    const lone = `${"a".repeat(300)}\ud800`;
    const wellFormed = "\u00e9".repeat(300);
    // found before TextEncoder changes, which the reference itself calls
    const want = referenceEncode(wellFormed);
    const m = expectOk(MapValue.of([[`${"a".repeat(300)}\ufffd`, text("found")]]), "a map");
    try {
      Object.defineProperty(String.prototype, "isWellFormed", { value: () => true, configurable: true, writable: true });
      Object.defineProperty(TextEncoder.prototype, "encode", { value: () => Uint8Array.of(0xef, 0xbf, 0xbd), configurable: true, writable: true });
      expect(isWellFormed(lone), "still checked").toBe(false);
      expect([...encodeUtf8(lone).subarray(-3)], "still its own bytes").toEqual([0xed, 0xa0, 0x80]);
      expect([...encodeUtf8(wellFormed)], "still its UTF-8").toEqual(want);
      expect(m.get(lone), "still not found").toBeUndefined();
      expect(Value.text(lone).ok, "still refused as text").toBe(false);
    } finally {
      if (savedCheck !== undefined) Object.defineProperty(String.prototype, "isWellFormed", savedCheck);
      if (savedEncode !== undefined) Object.defineProperty(TextEncoder.prototype, "encode", savedEncode);
    }
    expect(Object.getOwnPropertyDescriptor(String.prototype, "isWellFormed"), "restored").toEqual(savedCheck);
    expect(Object.getOwnPropertyDescriptor(TextEncoder.prototype, "encode"), "restored").toEqual(savedEncode);
  });
});
