import { describe, expect, it, onTestFinished } from "vitest";

import * as cborModule from "../src/cbor.js";
import { Feeding, STAGED_BYTES, encodeIntoWithin, type Absorber } from "../src/cbor.js";
import { valueCommitmentInto, valueCommitmentIntoWithin } from "../src/commitment.js";
import * as core from "../src/index.js";
import {
  cbor,
  DomainTag,
  fromHex,
  HASH_ALGORITHMS,
  hashCommitment,
  hashParts,
  json,
  MAX_DEPTH,
  models,
  startHash,
  TaggedHash,
  toHex,
  Value,
  valueCommitment,
  valueCommitmentWithin,
  type HashAlgorithm,
} from "../src/index.js";
import { bytesView, utf8View, type TextValue } from "../src/value.js";
import { expectOk, loadVectors } from "./vectors.js";

const tag = expectOk(DomainTag.of("dilf4s/test/streaming/v1"), "tag");
const text = (s: string): TextValue => expectOk(Value.text(s), "text");
const array = (items: readonly Value[]): Value => expectOk(Value.array(items), "array");
const map = (entries: readonly (readonly [string, Value])[]): Value => expectOk(Value.map(entries), "map");

/** The old path, kept here only as the oracle: the whole encoding (or commitment) built, then hashed. */
const bufferedContent = (t: TaggedHash, v: Value): Uint8Array => t.hash(expectOk(cbor.encode(v), "encodes"));
const bufferedCommitment = (t: TaggedHash, v: Value): Uint8Array => t.hash(expectOk(valueCommitment(v), "commits"));

function streamedCommitment(t: TaggedHash, v: Value): Uint8Array {
  const h = t.start();
  expectOk(valueCommitmentInto(h, v), "commits");
  return h.digest();
}

function checkAll(name: string, v: Value): void {
  for (const a of HASH_ALGORITHMS) {
    const t = TaggedHash.of(a, tag);
    expect(toHex(expectOk(t.content(v), "hashes")), `${name}: content hash under ${a}`).toBe(toHex(bufferedContent(t, v)));
    expect(toHex(streamedCommitment(t, v)), `${name}: commitment hash under ${a}`).toBe(toHex(bufferedCommitment(t, v)));
  }
}

function nested(depth: number, leaf: Value, withMaps: boolean): Value {
  let v = leaf;
  for (let i = 1; i < depth; i++) v = withMaps && i % 2 === 0 ? map([["k", v]]) : array([v]);
  return v;
}

function edgeCases(): [string, Value][] {
  const cases: [string, Value][] = [
    ["null", Value.null],
    ["false", Value.bool(false)],
    ["true", Value.bool(true)],
    ["empty text", text("")],
    ["empty bytes", Value.bytes(new Uint8Array(0))],
    ["empty array", array([])],
    ["empty map", map([])],
    ["non-ascii text", text("é\u{1F600}\u{FFFF}\u{E000}")],
    ["rational", expectOk(Value.number(-7n, 3n), "a rational")],
    ["big rational", expectOk(Value.number((1n << 200n) + 1n, (1n << 70n) + 3n), "a rational")],
    ["long text", text("x".repeat(70_000))],
    ["long bytes", Value.bytes(new Uint8Array(70_000).fill(0xab))],
    ["deep arrays", nested(MAX_DEPTH, Value.int(1n), false)],
    ["deep maps and arrays", nested(MAX_DEPTH, text("leaf"), true)],
    ["keys of every length order", map(["", "a", "b", "aa", "z", "zz", "é", "\u{1F600}", "\u{FFFF}", "\u{E000}"].map((k, i) => [k, Value.int(BigInt(i))] as const))],
  ];
  for (const n of [0n, 23n, 24n, 255n, 256n, 65_535n, 65_536n, (1n << 32n) - 1n, 1n << 32n, (1n << 64n) - 1n]) {
    cases.push([`int ${String(n)}`, Value.int(n)], [`int -1-${String(n)}`, Value.int(-n - 1n)]);
  }
  for (const n of [23, 24, 255, 256, STAGED_BYTES - 1, STAGED_BYTES, STAGED_BYTES + 1, 65_535, 65_536]) {
    cases.push([`text of ${String(n)}`, text("t".repeat(n))], [`bytes of ${String(n)}`, Value.bytes(new Uint8Array(n).fill(7))], [`array of ${String(n)}`, array(Array<Value>(n).fill(Value.null))]);
  }
  for (const bits of [64n, 65n, 127n, 128n, 129n, 256n, 2048n + 3n]) {
    const p = 1n << bits;
    for (const n of [p - 1n, p, p + 1n, -p - 1n, -p, -p + 1n]) cases.push([`bignum ${String(n)}`, Value.int(n)]);
  }
  return cases;
}

/** A deterministic generator (xorshift32), so the generated values are the same on every run. */
class Gen {
  constructor(private seed: number) {}
  next(): number {
    let x = this.seed;
    x ^= x << 13;
    x ^= x >>> 17;
    x ^= x << 5;
    this.seed = x >>> 0;
    return this.seed;
  }
  below(n: number): number {
    return this.next() % n;
  }
  text(): string {
    const alphabet = ["a", "b", "z", "é", "\u{FFFF}", "\u{E000}", "\u{1F600}", "0"];
    return Array.from({ length: this.below(6) }, () => alphabet[this.below(8)]).join("");
  }
  value(depth: number): Value {
    const scalar = depth === 0 || this.below(3) === 0;
    const k = this.below(8);
    if (scalar) {
      if (k === 0) return Value.null;
      if (k === 1) return Value.bool(this.below(2) === 0);
      if (k === 2) return Value.int(BigInt(this.next()) << BigInt(this.below(80)));
      if (k === 3) return Value.int(-(BigInt(this.next()) << BigInt(this.below(140))));
      if (k === 4) return expectOk(Value.number(BigInt(this.next()) - 2n ** 31n, BigInt(this.below(1000) + 1)), "a rational");
      if (k === 5) return text(this.text());
      return Value.bytes(Uint8Array.from({ length: this.below(40) }, () => this.below(256)));
    }
    if (k % 2 === 0) return array(Array.from({ length: this.below(5) }, () => this.value(depth - 1)));
    const entries = new Map<string, Value>();
    for (let i = this.below(5); i > 0; i--) entries.set(this.text(), this.value(depth - 1));
    return map([...entries]);
  }
}

describe("streamed hashing equals the buffered path", { timeout: 60_000 }, () => {
  it("for every canonical vector", () => {
    const vectors = loadVectors("canonical_encoding_v1.json", models.canonicalVectors.CanonicalVectors);
    let n = 0;
    for (const c of vectors.json) {
      if (!("cbor" in c)) continue;
      checkAll(c.json.slice(0, 40), expectOk(json.parse(c.json), "parses"));
      n++;
    }
    for (const c of vectors.model) {
      checkAll(c.value, expectOk(cbor.decode(expectOk(fromHex(c.cbor), "hex")), "decodes"));
      n++;
    }
    expect(n).toBeGreaterThan(0);
  });

  it.each(edgeCases())("for %s", (name, v) => {
    checkAll(name, v);
  });

  it("for 2000 generated values", () => {
    const g = new Gen(0x9e3779b9);
    for (let i = 0; i < 2000; i++) checkAll(`generated #${String(i)}`, g.value(6));
  });

  it("absorbing exactly the prefix and the encoding, once", () => {
    for (const [name, v] of edgeCases()) {
      const encoded = expectOk(cbor.encode(v), "encodes");
      let absorbed = 0;
      const counting: Absorber = { update: (d: Uint8Array) => (absorbed += d.length) };
      expect(encodeIntoWithin(new Feeding(counting), v, cbor.MAX_ENCODED_BYTES), name).toEqual({ ok: true, value: encoded.length });
      expect(absorbed, `${name}: every byte once`).toBe(encoded.length);
      absorbed = 0;
      expectOk(valueCommitmentInto(counting, v), "commits");
      expect(absorbed, `${name}: the commitment, once`).toBe(expectOk(valueCommitment(v), "commits").length);
    }
  });
});

describe("streamed hashing refuses as the buffered path does", { timeout: 60_000 }, () => {
  const cases: [string, Value][] = [
    ["an int", Value.int(1n << 70n)],
    ["text", text("é".repeat(300))],
    ["bytes", Value.bytes(new Uint8Array(300).fill(1))],
    ["a nested value", nested(40, map([["k", text("v".repeat(50))]]), true)],
    ["null", Value.null],
  ];
  it.each(cases)("for %s, at every limit around its length", (_, v) => {
    const t = TaggedHash.of("SHA-256", tag);
    const length = expectOk(cbor.encode(v), "encodes").length;
    for (const limit of [0, 1, length - 2, length - 1, length, length + 1, length + 2, 0.5, -1, Number.NaN, Infinity]) {
      const buffered = cbor.encodeWithin(v, limit);
      const streamed = t.contentWithin(v, limit);
      expect(streamed.ok, `content within ${String(limit)}`).toBe(buffered.ok);
      if (!streamed.ok && !buffered.ok) expect(streamed.error, `content refusal within ${String(limit)}`).toEqual(buffered.error);
      if (streamed.ok && buffered.ok) expect(toHex(streamed.value)).toBe(toHex(t.hash(buffered.value)));

      const committed = valueCommitmentWithin(v, limit);
      const h = t.start();
      const fed = valueCommitmentIntoWithin(h, v, limit);
      expect(fed.ok, `commitment within ${String(limit)}`).toBe(committed.ok);
      if (!fed.ok && !committed.ok) expect(fed.error, `commitment refusal within ${String(limit)}`).toEqual(committed.error);
      if (fed.ok && committed.ok) expect(toHex(h.digest())).toBe(toHex(t.hash(committed.value)));
    }
  });
});

describe("the hashing path's memory is bounded", { timeout: 60_000 }, () => {
  /** What reaches the hash: how much, and the longest chunk that is not a text or byte string of the value, lent in place. */
  /** What reaches the hash: how much, the longest chunk, and how many chunks share memory with the value's own bytes. */
  function pieces(v: Value, own: readonly Uint8Array[]): { total: number; longestStaged: number; peak: number; borrowed: number } {
    let total = 0;
    let longestStaged = 0;
    let borrowed = 0;
    const into: Absorber = {
      update: (d: Uint8Array) => {
        total += d.length;
        longestStaged = Math.max(longestStaged, d.length);
        if (own.some((o) => o.buffer === d.buffer)) borrowed++;
      },
    };
    const feeding = new Feeding(into);
    expectOk(encodeIntoWithin(feeding, v, cbor.MAX_ENCODED_BYTES), "streams");
    return { total, longestStaged, peak: feeding.peak, borrowed };
  }

  function check(values: [string, Value, Uint8Array[]][]): void {
    for (const [name, v, lent] of values) {
      const p = pieces(v, lent);
      expect(p.total, `${name}: every byte`).toBe(expectOk(cbor.encodedLengthWithin(v, cbor.MAX_ENCODED_BYTES), "measures"));
      expect(p.peak, `${name}: staged at most STAGED_BYTES`).toBeLessThanOrEqual(STAGED_BYTES);
      expect(p.longestStaged, `${name}: every chunk is staged`).toBeLessThanOrEqual(STAGED_BYTES);
      expect(p.borrowed, `${name}: no chunk is the value's own memory`).toBe(0);
    }
  }

  it.each([1_000, 100_000])("for structures of about %i bytes", (n) => {
    check([
      ["wide", array(Array.from({ length: n / 10 }, (_, i) => Value.int(BigInt(i) << 40n))), []],
      ["keyed", map(Array.from({ length: n / 20 }, (_, i) => [`k${String(i)}`, Value.bool(i % 2 === 0)] as const)), []],
      ["big", Value.int(1n << BigInt(n)), []],
    ]);
  });

  it.each([1_000, 10_000_000])("for strings of about %i bytes", (n) => {
    const longText = text("é".repeat(n / 2));
    const longBytes = Value.bytes(new Uint8Array(n).fill(0x5a));
    check([
      ["text", longText, [utf8View(longText)]],
      ["bytes", longBytes, [bytesView(longBytes)]],
      ["both in an array", array([longText, longBytes]), [utf8View(longText), bytesView(longBytes)]],
    ]);
  });

  it("and hashing the commitment of a large value matches a plain hash", () => {
    const a: HashAlgorithm = "SHA-256";
    const v = Value.bytes(new Uint8Array(1 << 20).fill(3));
    const h = startHash(a);
    expectOk(valueCommitmentInto(h, v), "commits");
    expect(toHex(h.digest())).toBe(toHex(hashParts(a, [expectOk(valueCommitment(v), "commits")])));
  });
});

describe("the streaming internals stay internal", () => {
  it("the public API hands no caller's hash a value's own bytes", () => {
    const names = new Set([...Object.keys(core), ...Object.keys(cbor)]);
    for (const internal of ["Feeding", "encodeIntoWithin", "valueCommitmentInto", "valueCommitmentIntoWithin", "STAGED_BYTES"]) {
      expect(names.has(internal), `${internal} is not exported`).toBe(false);
    }
  });

  it("the public cbor namespace is everything cbor.ts exports but the streaming internals", () => {
    const internals = new Set(["Feeding", "encodeIntoWithin", "STAGED_BYTES"]);
    const expected = Object.keys(cborModule).filter((k) => !internals.has(k)).sort();
    expect(Object.keys(cbor).sort()).toEqual(expected);
  });

  it("a borrowed contentWithin never hands a value's bytes to a caller's hash", () => {
    const bytes = Value.bytes(new Uint8Array(2048).fill(7));
    const long = text("t".repeat(2048));
    for (const v of [bytes, long, array([bytes, long])]) {
      const writes: Uint8Array[] = [];
      const evil = { update: (d: Uint8Array) => (writes.push(d), d.fill(42), evil), digest: () => new Uint8Array(32) };
      const receiver = { algorithm: "SHA-256", start: () => evil };
      // eslint-disable-next-line @typescript-eslint/unbound-method -- the point is a method borrowed onto another receiver
      const borrowed = TaggedHash.prototype.contentWithin;
      expect(() => Reflect.apply(borrowed, receiver, [v, cbor.MAX_ENCODED_BYTES]), "refused at the brand check").toThrow(TypeError);
      expect(writes.length, "nothing reached the caller's hash").toBe(0);
    }
    expect(toHex(bytes.bytes), "the value is unchanged").toBe("07".repeat(2048));
  });

  it("a hash whose prototype a caller replaced still never sees a value's own bytes", () => {
    const proto = Object.getPrototypeOf(startHash("SHA-256")) as { update: (d: Uint8Array) => unknown };
    const original = proto.update;
    const seen: Uint8Array[] = [];
    proto.update = function (this: unknown, d: Uint8Array) {
      seen.push(d);
      return original.call(this, d);
    };
    onTestFinished(() => {
      proto.update = original;
    });
    const bytes = Value.bytes(new Uint8Array(4096).fill(7));
    const long = text("t".repeat(4096));
    const own = [bytesView(bytes), utf8View(long)];
    for (const v of [bytes, long, array([bytes, long])]) {
      expectOk(TaggedHash.of("SHA-256", tag).content(v), "hashes");
      expectOk(hashCommitment("SHA-256", [], v), "commits");
    }
    expect(seen.length, "the replaced update ran").toBeGreaterThan(0);
    expect(seen.filter((d) => own.some((o) => o.buffer === d.buffer)).length, "no update saw a value's memory").toBe(0);
  });

  it("hashCommitment hashes a prefix and the commitment, as the buffered path does", () => {
    const prefix = [Uint8Array.of(1, 2, 3), Uint8Array.of(4)];
    for (const [name, v] of edgeCases()) {
      for (const a of HASH_ALGORITHMS) {
        const buffered = hashParts(a, [...prefix, expectOk(valueCommitment(v), "commits")]);
        expect(toHex(expectOk(hashCommitment(a, prefix, v), "hashes")), `${name} under ${a}`).toBe(toHex(buffered));
      }
    }
  });
});
