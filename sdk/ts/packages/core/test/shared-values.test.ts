import { inspect as nodeInspect } from "node:util";

import { describe, expect, it } from "vitest";

import {
  cbor,
  DEBUG_BUDGET,
  DEBUG_DEPTH,
  DomainTag,
  equals,
  fromHex,
  inspect,
  json,
  MAX_DEPTH,
  MAX_INTEGER_BITS,
  MAX_WEIGHT,
  models,
  TaggedHash,
  toHex,
  Value,
  valueCommitment,
  valueCommitmentWithin,
} from "../src/index.js";
import { boolValue, bytesValue, expectOk, int, loadVectors, readVectors } from "./vectors.js";

const bytes = (hex: string): Uint8Array => expectOk(fromHex(hex), "hex");
const text = (s: string): Value => expectOk(Value.text(s), "text");
const array = (items: readonly Value[]): Value => expectOk(Value.array(items), "array");
const map = (entries: readonly (readonly [string, Value])[]): Value => expectOk(Value.map(entries), "map");

/** `[x, x]` nested `k` deep around `leaf`: `k` arrays, `2^k` leaves. */
const doubled = (leaf: Value, k: number): Value => Array.from({ length: k }).reduce<Value>((v) => array([v, v]), leaf);

const utf8Bytes = (s: string): number => new TextEncoder().encode(s).length;

/** Java's `bitLength`, counted independently of core's: the binary digits of `n`, or of `-1 - n` when negative. */
const javaBits = (n: bigint): number => {
  const m = n < 0n ? -1n - n : n;
  return m === 0n ? 0 : m.toString(2).length;
};

/** The weight by its definition, recounted by a full (recursive: test values are shallow) walk. */
function recount(v: Value): bigint {
  const limbs = (n: bigint): bigint => BigInt(Math.floor(javaBits(n) / 64));
  switch (v.kind) {
    case "null":
    case "bool":
      return 1n;
    case "int":
      return 1n + limbs(v.value);
    case "rational":
      return 2n + limbs(v.numerator) + limbs(v.denominator);
    case "text":
      return 1n + BigInt(utf8Bytes(v.value));
    case "bytes":
      return 1n + BigInt(v.length);
    case "array":
      return v.items.reduce((w, x) => w + recount(x), 1n);
    case "map":
      return v.entries().reduce((w, [k, x]) => w + 1n + BigInt(utf8Bytes(k)) + recount(x), 1n);
  }
}

const saturate = (w: bigint): bigint => (w > MAX_WEIGHT ? MAX_WEIGHT : w);

/** Every sub-value of `v`, `v` first. */
function walk(v: Value): Value[] {
  const parts = v.kind === "array" ? v.items : v.kind === "map" ? v.entries().map(([, x]) => x) : [];
  return [v, ...parts.flatMap(walk)];
}

/** Every value in the canonical-encoding vectors (accepted JSON, model CBOR, the file itself), and every part of each. */
function vectorValues(): Value[] {
  const vectors = loadVectors("canonical_encoding_v1.json", models.canonicalVectors.CanonicalVectors);
  const roots = [
    expectOk(json.parse(readVectors("canonical_encoding_v1.json")), "the vector file"),
    ...vectors.json.flatMap((c) => ("cbor" in c ? [expectOk(cbor.decode(bytes(c.cbor)), "decodes")] : [])),
    ...vectors.model.map((c) => expectOk(cbor.decode(bytes(c.cbor)), "decodes")),
    ...vectors.renderRejections.map((c) => expectOk(cbor.decode(bytes(c.cbor)), "decodes")),
  ];
  return roots.flatMap(walk);
}

const elapsed = (f: () => void): number => {
  const start = performance.now();
  f();
  return performance.now() - start;
};

/** Expanding any of these values would never finish (2^60 leaves), so a generous limit still tells it apart, under load too. */
const HANG = 20_000;

describe("weight", { timeout: 60_000 }, () => {
  it("is cached as Scala defines it, on every value in the vectors", () => {
    const values = vectorValues();
    for (const v of values) expect(v.weight, inspect(v)).toBe(saturate(recount(v)));
    expect(values.length).toBeGreaterThan(1000);
  });

  it("counts whole limbs of Java's bitLength, and UTF-8 bytes", () => {
    const pow = (e: bigint): bigint => 2n ** e;
    const cases: (readonly [bigint, bigint])[] = [
      [0n, 1n],
      [-1n, 1n],
      [pow(63n) - 1n, 1n],
      [pow(63n), 2n],
      [-pow(63n), 1n],
      [-pow(63n) - 1n, 2n],
      [pow(64n), 2n],
      [-pow(64n), 2n],
      [-pow(127n), 2n],
      [-pow(128n), 3n],
    ];
    for (const [n, w] of cases) {
      expect(int(n).weight, `weight of an integer of ${javaBits(n).toString()} bits`).toBe(w);
      expect(int(n).bitLength, "bitLength").toBe(javaBits(n));
    }
    // a negative power-of-two numerator: Java counts 63 bits, so no whole limb
    expect(expectOk(Value.number(-pow(63n), 3n), "-2^63/3").weight, "-2^63/3").toBe(2n);
    expect(expectOk(Value.number(1n, pow(64n)), "1/2^64").weight, "1/2^64").toBe(3n);
    expect(text("é😀").weight, "text weighs its UTF-8 bytes").toBe(7n);
    expect(bytesValue(new Uint8Array(5)).weight).toBe(6n);
    expect(map([["é", int(1n)], ["n", Value.null]]).weight, "1 + (1 + 2 + 1); the null entry is dropped").toBe(5n);
  });

  it("of a doubling value is its logical size, and saturates", () => {
    // text "a" weighs 2; [x, x] weighs 1 + 2w, so after k doublings 3·2^k - 1, past 2^63 - 1 from k = 62
    let v = text("a");
    for (let k = 1; k <= MAX_DEPTH; k++) {
      v = array([v, v]);
      expect(v.weight, `weight after ${k.toString()} doublings`).toBe(k <= 61 ? 3n * (1n << BigInt(k)) - 1n : MAX_WEIGHT);
      expect(v.depth).toBe(k);
    }
    expect(Value.array([v, v]).ok, "one doubling too many").toBe(false);
    // maps add one per entry and the key's bytes: {a: x, b: x} weighs 1 + 2(2 + w)
    let m: Value = Value.null;
    let expected = 1n;
    for (let k = 1; k <= MAX_DEPTH; k++) {
      m = map([
        ["a", m],
        ["b", m],
      ]);
      expected = k === 1 ? 1n : saturate(2n * expected + 5n);
      expect(m.weight, `map weight after ${k.toString()} doublings`).toBe(expected);
    }
    expect(m.weight).toBe(MAX_WEIGHT);
  });
});

describe("budgeted encoding", { timeout: 60_000 }, () => {
  it("agrees with the encoder on every vector value, and no encoding is shorter than its weight", () => {
    for (const v of vectorValues()) {
      const full = expectOk(cbor.encode(v), "encodes");
      const n = full.length;
      expect(BigInt(n) >= v.weight, `${toHex(full)}: ${n.toString()} bytes under weight ${v.weight.toString()}`).toBe(true);
      const within = expectOk(cbor.encodeWithin(v, n), "at the exact length");
      expect(toHex(within)).toBe(toHex(full));
      expect(within.buffer.byteLength, "no more than the limit is held").toBeLessThanOrEqual(n);
      expect(expectOk(cbor.encodedLengthWithin(v, n), "length")).toBe(n);
      const over = cbor.encodeWithin(v, n - 1);
      expect(over.ok, "one byte short").toBe(false);
      if (!over.ok) {
        expect(over.error, "the limit, and nothing measured").toEqual({ kind: "TooLarge", limit: n - 1 });
      }
      expect(cbor.encodedLengthWithin(v, n - 1).ok).toBe(false);
    }
  });

  it("refuses a shared expansion without expanding it", () => {
    const ms = elapsed(() => {
      // 2^60 leaves in 60 arrays: refused by weight alone
      const dag = doubled(text("a"), 60);
      expect(cbor.encodeWithin(dag, 1 << 20)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 1 << 20 } });
      expect(cbor.encodedLengthWithin(dag, 1 << 20).ok).toBe(false);
      // within the weight bound but over in bytes (each 2^64 - 1 weighs 2 and takes 9 bytes): refused by the byte count
      const ints = doubled(int(2n ** 64n - 1n), 17);
      expect(ints.weight < 1n << 20n, "passes the weight check").toBe(true);
      const e = cbor.encodeWithin(ints, 1 << 20);
      expect(e, "refused at the limit").toEqual({ ok: false, error: { kind: "TooLarge", limit: 1 << 20 } });
      expect(expectOk(cbor.encodedLengthWithin(ints, 1 << 21), "counted")).toBe(expectOk(cbor.encode(ints), "encodes").length);
    });
    expect(ms).toBeLessThan(HANG);
  });

  it("refuses a saturated weight with the limit alone", () => {
    const saturated = doubled(text("a"), MAX_DEPTH);
    expect(saturated.weight).toBe(MAX_WEIGHT);
    // a limit past the ceiling is the ceiling: an encoding is built in one array
    expect(cbor.encodeWithin(saturated, Number.MAX_SAFE_INTEGER)).toEqual({ ok: false, error: { kind: "TooLarge", limit: cbor.MAX_ENCODED_BYTES } });
    expect(cbor.encodedLengthWithin(saturated, Infinity)).toEqual({ ok: false, error: { kind: "TooLarge", limit: cbor.MAX_ENCODED_BYTES } });
    expect(valueCommitmentWithin(saturated, 2 ** 40)).toEqual({ ok: false, error: { kind: "TooLarge", limit: cbor.MAX_ENCODED_BYTES } });
    expect(cbor.MAX_ENCODED_BYTES).toBe(2 ** 31 - 9);
    // and so is a content hash's, streamed or not: refused from the weight before anything is hashed
    const tag = expectOk(DomainTag.of("dilf4s/test/v1"), "tag");
    expect(TaggedHash.of("SHA-256", tag).contentWithin(saturated, Infinity)).toEqual({ ok: false, error: { kind: "TooLarge", limit: cbor.MAX_ENCODED_BYTES } });
    expect(valueCommitmentWithin(saturated, 100)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 100 } });
  });

  it("the unbudgeted forms are budgeted at the ceiling: a typed refusal, never a thrown allocation failure", () => {
    const tag = expectOk(DomainTag.of("dilf4s/test/v1"), "tag");
    const refused = { ok: false, error: { kind: "TooLarge", limit: cbor.MAX_ENCODED_BYTES } };
    // 2^40 leaves: a weight over the ceiling, refused before anything is allocated
    const dag = doubled(text("a"), 40);
    const ms = elapsed(() => {
      expect(cbor.encode(dag)).toEqual(refused);
      expect(valueCommitment(dag)).toEqual(refused);
      expect(TaggedHash.of("SHA-256", tag).content(dag)).toEqual(refused);
    });
    expect(ms).toBeLessThan(HANG);
    // and a small value encodes as before
    expect(toHex(expectOk(cbor.encode(text("a")), "encodes"))).toBe("6161");
  });

  it("reads an odd limit as a whole number of bytes, never as unbounded", () => {
    const v = text("abc");
    expect(expectOk(cbor.encodeWithin(v, 4.9), "4 bytes").length).toBe(4);
    for (const limit of [Number.NaN, -1, -Infinity, 3.99]) {
      const r = cbor.encodeWithin(v, limit);
      expect(r.ok, `limit ${limit.toString()}`).toBe(false);
      expect(r.ok ? undefined : r.error, `limit ${limit.toString()}`).toEqual({ kind: "TooLarge", limit: limit === 3.99 ? 3 : 0 });
    }
    expect(expectOk(cbor.encodeWithin(v, Infinity), "Infinity is the largest limit").length).toBe(4);
  });

  it("streams integers: every power of two and its neighbours as the reference encodes them", () => {
    /** Materialised bytes, as the original encoder built them. */
    const reference = (n: bigint): string => {
      const negative = n < 0n;
      const m = negative ? -1n - n : n;
      const major = negative ? 1 : 0;
      if (m < 2n ** 64n) {
        const hexArg = (len: number): string => m.toString(16).padStart(2 * len, "0");
        const head = m < 24n ? (major * 32 + Number(m)).toString(16).padStart(2, "0") : "";
        if (head !== "") return head;
        const [info, len] = m < 0x100n ? [24, 1] : m < 0x10000n ? [25, 2] : m < 0x100000000n ? [26, 4] : [27, 8];
        return (major * 32 + info).toString(16).padStart(2, "0") + hexArg(len);
      }
      const hex = m.toString(16);
      const body = hex.length % 2 === 0 ? hex : `0${hex}`;
      const length = expectOk(cbor.encodeWithin(bytesValue(bytes(body)), Number.MAX_SAFE_INTEGER), "bytes");
      return (negative ? "c3" : "c2") + toHex(length);
    };
    for (let e = 0n; e <= 4096n; e += e < 80n ? 1n : 61n) {
      for (const n of [2n ** e - 1n, 2n ** e, 2n ** e + 1n, -(2n ** e) + 1n, -(2n ** e), -(2n ** e) - 1n]) {
        const v = int(n);
        const encoded = toHex(expectOk(cbor.encode(v), "encodes"));
        expect(encoded, `±2^${e.toString()}`).toBe(reference(n));
        expect(toHex(expectOk(cbor.encodeWithin(v, encoded.length / 2), "budgeted"))).toBe(encoded);
        const back = expectOk(cbor.decode(expectOk(cbor.encode(v), "encodes")), "decodes");
        expect(back.kind === "int" && back.value === n, "round trip").toBe(true);
      }
    }
  });

  // a wider bignum is refused from its length, before it is built (integer-width.test.ts)
  it("encodes and decodes the widest integer", () => {
    const n = -(1n << BigInt(MAX_INTEGER_BITS)) + 12345n;
    const encoded = expectOk(cbor.encode(int(n)), "encodes");
    expect(encoded.length).toBe(4157);
    const back = expectOk(cbor.decode(encoded), "decodes");
    expect(back.kind === "int" && back.value === n).toBe(true);
  });
});

describe("rendering and commitments", { timeout: 60_000 }, () => {
  it("renderWithin agrees with render on every vector value, and no JSON is shorter (in UTF-8) than its weight", () => {
    let rendered = 0;
    for (const v of vectorValues()) {
      const r = json.renderWithin(v, json.MAX_RENDER_BYTES);
      if (!r.ok) {
        expect(json.renderWithin(v, Number.MAX_SAFE_INTEGER)).toEqual(r);
        continue;
      }
      const n = utf8Bytes(r.value);
      expect(BigInt(n) >= v.weight, `${r.value}: ${n.toString()} bytes under weight ${v.weight.toString()}`).toBe(true);
      expect(json.renderWithin(v, n)).toEqual(r);
      const over = json.renderWithin(v, n - 1);
      expect(over, r.value).toEqual({ ok: false, error: { kind: "TooLarge", limit: n - 1 } });
      rendered++;
    }
    expect(rendered).toBeGreaterThan(500);
  });

  it("refuses to render, commit to or hash a shared expansion without expanding it", () => {
    const dag = doubled(text("a"), 60);
    const tag = expectOk(DomainTag.of("dilf4s/test/v1"), "tag");
    const ms = elapsed(() => {
      expect(json.renderWithin(dag, 1 << 20)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 1 << 20 } });
      const r = json.renderWithin(dag, json.MAX_RENDER_BYTES);
      expect(!r.ok && r.error.kind, "the default limit holds too").toBe("TooLarge");
      expect(valueCommitmentWithin(dag, 1 << 20)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 1 << 20 } });
      expect(TaggedHash.of("SHA-256", tag).contentWithin(dag, 1 << 20).ok).toBe(false);
      // a doubling under the weight bound but over in JSON (each 2^64 - 1 weighs 2 and takes 20 digits)
      const ints = doubled(int(2n ** 64n - 1n), 16);
      expect(ints.weight < 1n << 20n, "passes the weight check").toBe(true);
      const e = json.renderWithin(ints, 1 << 20);
      expect(e).toEqual({ ok: false, error: { kind: "TooLarge", limit: 1 << 20 } });
    });
    expect(ms).toBeLessThan(HANG);
  });

  it("never takes a limit past what a string can hold", () => {
    // 2^10 MiB of text in 10 arrays and one 1 MiB text: past the string ceiling, so refused at once, never thrown
    const big = doubled(text("x".repeat(1 << 20)), 10);
    for (const limit of [Infinity, 2 ** 31, Number.MAX_SAFE_INTEGER]) {
      expect(json.renderWithin(big, limit)).toEqual({ ok: false, error: { kind: "TooLarge", limit: json.MAX_RENDER_BYTES } });
    }
  });

  it("counts escaped text and keys before escaping them", () => {
    // every escape form, quotes, and one, two, three and four UTF-8 bytes
    const all = `${Array.from({ length: 0x20 }, (_, c) => String.fromCharCode(c)).join("")}"\\/aé€😀\u007f`;
    for (const v of [text(all), map([[all, text(all)]]), array([text(all), map([["\n", int(1n)]])])]) {
      const r = expectOk(json.renderWithin(v, json.MAX_RENDER_BYTES), "renders");
      const n = utf8Bytes(r);
      expect(json.renderWithin(v, n), "at the exact length").toEqual({ ok: true, value: r });
      expect(json.renderWithin(v, n - 1).ok, "one byte short").toBe(false);
    }
    // NUL escapes to six bytes: refused by the escaped length, counted rather than built
    const nuls = text("\u0000".repeat(200_000));
    expect(json.renderWithin(nuls, 200_001)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 200_001 } });
    const key = map([["\u0000".repeat(200_000), int(1n)]]);
    expect(json.renderWithin(key, 200_010)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 200_010 } });
  });

  it("a budgeted commitment is the commitment, within its limit, tag included", () => {
    const tag = expectOk(DomainTag.of("dilf4s/test/v1"), "tag");
    const h = TaggedHash.of("SHA-256", tag);
    for (const v of [text("a"), bytesValue(Uint8Array.of(1, 2, 3)), array([int(1n), Value.null]), bytesValue(new Uint8Array(0))]) {
      const c = expectOk(valueCommitment(v), "encodes");
      expect(toHex(expectOk(valueCommitmentWithin(v, c.length), "fits"))).toBe(toHex(c));
      const over = valueCommitmentWithin(v, c.length - 1);
      expect(over, "the caller's limit, tag included").toEqual({ ok: false, error: { kind: "TooLarge", limit: c.length - 1 } });
      const n = expectOk(cbor.encode(v), "encodes").length;
      expect(toHex(expectOk(h.contentWithin(v, n), "content"))).toBe(toHex(expectOk(h.content(v), "encodes")));
      expect(h.contentWithin(v, n - 1).ok).toBe(false);
    }
  });
});

describe("equality", { timeout: 60_000 }, () => {
  it("is content equality, on every vector value and its decoded copy", () => {
    const values = vectorValues();
    for (const v of values) expect(equals(v, expectOk(cbor.decode(expectOk(cbor.encode(v), "encodes")), "copy")), inspect(v)).toBe(true);
    for (const [i, a] of values.entries()) {
      const b = values[(i * 7919 + 13) % values.length] ?? a;
      expect(equals(a, b), `${inspect(a)} = ${inspect(b)}`).toBe(toHex(expectOk(cbor.encode(a), "encodes")) === toHex(expectOk(cbor.encode(b), "encodes")));
    }
    expect(Value.equals(array([text("x"), int(1n)]), array([text("x"), int(1n)]))).toBe(true);
    expect(equals(array([text("x"), int(1n)]), array([text("x"), int(2n)]))).toBe(false);
    expect(equals(map([["k", int(1n)]]), map([["l", int(1n)]])), "keys differ").toBe(false);
    expect(equals(int(1n), boolValue(true)), "kinds differ, same weight").toBe(false);
  });

  it("of shared values never expands them", () => {
    const ms = elapsed(() => {
      // separately built, so no part is shared between them: 2^60 leaves each
      const a = doubled(text("a"), 60);
      const b = doubled(text("a"), 60);
      expect(equals(a, b), "content-equal DAGs").toBe(true);
      // one leaf differs, at the bottom of the second half: the first half is proven equal once, then skipped
      const c = array([doubled(text("a"), 59), doubled(array([text("a"), text("b")]), 58)]);
      expect(c.weight, "same weight, so no early refusal").toBe(a.weight);
      expect(c.depth).toBe(a.depth);
      expect(equals(a, c), "one differing leaf").toBe(false);
      expect(equals(c, a), "either way round").toBe(false);
      const mapDag = (leaf: Value): Value =>
        Array.from({ length: 60 }).reduce<Value>(
          (v) =>
            map([
              ["k", v],
              ["l", v],
            ]),
          leaf,
        );
      expect(equals(mapDag(int(1n)), mapDag(int(1n))), "equal map DAGs").toBe(true);
      expect(equals(mapDag(int(1n)), mapDag(int(2n))), "unequal map DAGs").toBe(false);
    });
    expect(ms).toBeLessThan(HANG);
  });

  it("compares each pair of large payloads once", () => {
    const n = 10_000;
    const mib = 1 << 20;
    // separately built (outside the timing), equal: without the memo, n arrays cost n full comparisons of 1 MiB
    const kinds: (readonly [string, () => Value])[] = [
      ["texts", () => text("x".repeat(mib))],
      ["byte strings", () => bytesValue(new Uint8Array(mib).fill(7))],
      ["integers", () => int((1n << BigInt(MAX_INTEGER_BITS)) - 1n)],
    ];
    for (const [what, make] of kinds) {
      const [a, b] = [make(), make()];
      const [single, other] = [array([a]), array([b])];
      const [many, others] = [array(new Array<Value>(n).fill(a)), array(new Array<Value>(n).fill(b))];
      const one = elapsed(() => {
        expect(equals(single, other), what).toBe(true);
      });
      const all = elapsed(() => {
        expect(equals(many, others), what).toBe(true);
      });
      // a ratio, so load slows both sides alike: unmemoised it would be about n
      expect(all, `${what}: ${all.toFixed(1)} ms for ${n.toString()} against ${one.toFixed(1)} ms for one`).toBeLessThan((n / 10) * Math.max(one, 1));
    }
    const other = array(new Array<Value>(n).fill(text(`y${"x".repeat(mib - 1)}`)));
    expect(equals(array(new Array<Value>(n).fill(text("x".repeat(mib)))), other), "a differing text").toBe(false);
  });
});

describe("inspect", { timeout: 60_000 }, () => {
  it("shows a small value in full, through toString and util.inspect too", () => {
    const small = map([
      ["a", array([int(1n), text("x")])],
      ["b", bytesValue(Uint8Array.of(0, 255))],
      ["c", expectOk(Value.number(1n, 3n), "1/3")],
    ]);
    expect(inspect(small)).toBe(`{"a": [1, "x"], "b": h'00ff', "c": 1/3}`);
    expect(String(small)).toBe(inspect(small));
    expect(nodeInspect(small)).toBe(inspect(small));
    expect(inspect(int(1n << 300n))).toBe("<int of 301 bits>");
  });

  it("is bounded however often a part repeats, and however large or deep a value is", () => {
    const ms = elapsed(() => {
      const shown = inspect(doubled(text("a"), 60));
      expect(shown.length).toBeLessThan(64 * DEBUG_BUDGET);
      expect(shown).toContain("…");
      const big = inspect(array([text("x".repeat(1 << 20))]));
      expect(big).toBe("[…]");
      const deep = Array.from({ length: MAX_DEPTH }).reduce<Value>((v) => array([v]), Value.null);
      const nested = inspect(deep);
      expect(nested).toBe(`${"[".repeat(DEBUG_DEPTH)}…${"]".repeat(DEBUG_DEPTH)}`);
    });
    expect(ms).toBeLessThan(HANG);
  });
});

describe("stack safety", { timeout: 60_000 }, () => {
  it("every walk takes the deepest value without recursion", () => {
    const deepArray = Array.from({ length: MAX_DEPTH }).reduce<Value>((v) => array([v, int(1n)]), Value.null);
    const deepMap = Array.from({ length: MAX_DEPTH }).reduce<Value>((v) => map([["k", v]]), int(1n));
    for (const v of [deepArray, deepMap]) {
      const encoded = expectOk(cbor.encode(v), "encodes");
      expect(equals(v, expectOk(cbor.decode(encoded), "decodes"))).toBe(true);
      expect(json.renderWithin(v, json.MAX_RENDER_BYTES).ok).toBe(true);
      expect(expectOk(json.parse(expectOk(json.renderWithin(v, json.MAX_RENDER_BYTES), "renders")), "parses").weight).toBe(v.weight);
    }
  });
});
