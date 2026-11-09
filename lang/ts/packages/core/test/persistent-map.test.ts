/**
 * A map is a persistent AVL tree (src/tree.ts): `set` and `delete` build O(log n) nodes and share the rest, the depth
 * and weight stay exact, and every walk sees the entries in canonical order, or in code-point order through the map's
 * code-point view. Checked against a reference model (a map built afresh by `MapValue.of` at every step) over seeded
 * random sequences of updates, and counted, never timed: the tree's height and its unshared nodes, the bytes the
 * code-point order reads, and whether a listing was made.
 */
import { describe, expect, it } from "vitest";

import { ArrayValue, BytesValue, cbor, DEBUG_BUDGET, hashCommitment, inspect, json, MapValue, MAX_DEPTH, MAX_WEIGHT, RationalValue, TextValue, Value, type Value as V } from "../src/index.js";
import { codePointOrder, type Entry } from "../src/tree.js";
import { arrayOfValues, codePointKeys, codePointView, entriesView, isValue, MapShape, treeStats, unsharedNodes } from "../src/value.js";
import { expectOk, hexOf, int } from "./vectors.js";

/** `x` as a `T`, past the types, as a JavaScript caller passes it. */
const bad = (x: unknown): never => x as never;

const text = (s: string): V => expectOk(Value.text(s), "text");
const arr = (vs: readonly V[]): V => expectOk(ArrayValue.of(vs), "an array");
const mapOf = (es: readonly (readonly [string, V])[]): MapValue => expectOk(MapValue.of(es), "a map");
/** An integer nested `d` arrays deep. */
const deep = (d: number): V => Array.from({ length: d }).reduce<V>((x) => arr([x]), int(BigInt(d)));
/** `[x, x]` nested 63 deep: a weight past 2^63, held in 63 arrays. */
const huge = Array.from({ length: 63 }).reduce<V>((x) => arr([x, x]), int(1n));

/** Keys whose canonical order (shorter UTF-8 first) and code-point order differ, and that share long prefixes. */
const KEYS: readonly string[] = [
  "",
  "a",
  "b",
  "z",
  "aa",
  "ab",
  "ba",
  "é",
  "ÿ",
  "ࠀ",
  "｡",
  "",
  "￿",
  "\u{1F600}",
  "\u{10000}",
  "\u{10FFFF}",
  "a\u{1F600}",
  "a｡",
  ...Array.from({ length: 24 }, (_, i) => `k${i.toString()}`),
  ...Array.from({ length: 12 }, (_, i) => `${"p".repeat(40)}${i.toString(36)}`),
  ...Array.from({ length: 8 }, (_, i) => "q".repeat(i + 1)),
];

/** A seeded generator (a 32-bit linear congruential one): the same sequence every run. */
function generator(seed: number): (n: number) => number {
  let s = seed >>> 0;
  return (n) => {
    s = (Math.imul(s, 1664525) + 1013904223) >>> 0;
    return s % n;
  };
}

/** UTF-8 bytes in order: code-point order, the reference for the code-point view. */
const utf8 = (s: string): Uint8Array => new TextEncoder().encode(s);
function byBytes(a: string, b: string): number {
  const [x, y] = [utf8(a), utf8(b)];
  for (let i = 0; i < Math.min(x.length, y.length); i++) if (x[i] !== y[i]) return (x[i] ?? 0) - (y[i] ?? 0);
  return x.length - y.length;
}

/** The AVL bound on the height of `n` entries. */
const avlHeight = (n: number): number => Math.floor(1.4405 * Math.log2(n + 2) - 0.3277);

/** The canonical CBOR of `v` as hex, or its refusal (a weight past the encoding ceiling is refused from the weight). */
function encoded(v: V): string {
  const e = cbor.encode(v);
  return e.ok ? hexOf(e.value) : `refused ${e.error.kind}`;
}

/** Every observation of `m` that must agree with the reference map of the same entries. */
function agrees(m: MapValue, model: ReadonlyMap<string, V>, step: string): void {
  const ref = mapOf([...model]);
  expect(m.size, `${step}: size`).toBe(ref.size);
  expect(m.depth, `${step}: depth`).toBe(ref.depth);
  expect(m.weight, `${step}: weight`).toBe(ref.weight);
  expect(m.keys(), `${step}: keys in canonical order`).toEqual(ref.keys());
  const [mine, theirs] = [m.entries(), ref.entries()];
  expect(mine.every(([k, v], i) => theirs[i]?.[0] === k && theirs[i][1] === v), `${step}: the same entries, the same values`).toBe(true);
  expect(Value.equals(m, ref), `${step}: equal to the reference`).toBe(true);
  expect(encoded(m), `${step}: canonical CBOR`).toBe(encoded(ref));
  expect(
    codePointView(m).map((e) => e.key),
    `${step}: the code-point view`,
  ).toEqual([...model.keys()].sort(byBytes));
  for (const [k, v] of model) expect(m.get(k), `${step}: get ${JSON.stringify(k)}`).toBe(v);
  for (const k of ["absent", "\uD800", "k99"]) expect(m.has(k), `${step}: has ${JSON.stringify(k)}`).toBe(model.has(k));
  expect(treeStats(m).height, `${step}: within the AVL height`).toBeLessThanOrEqual(avlHeight(m.size));
}

describe("a persistent map agrees with one built afresh, through any sequence of updates", { timeout: 120_000 }, () => {
  const values: readonly (() => V)[] = [
    () => int(7n),
    () => text("v".repeat(70)),
    () => deep(1),
    () => deep(4),
    () => deep(9),
    () => huge,
    () => Value.null,
  ];

  for (const seed of [1, 2, 3]) {
    it(`seed ${seed.toString()}: sets, replacements, removals and null values`, () => {
      const next = generator(seed);
      let m = MapValue.empty;
      const model = new Map<string, V>();
      for (let step = 0; step < 1500; step++) {
        const key = KEYS[next(KEYS.length)] ?? "";
        const before = m;
        const beforeBytes = encoded(before);
        if (next(3) === 0) {
          m = m.delete(key);
          model.delete(key);
        } else {
          const v = values[next(values.length)]?.() ?? Value.null;
          m = expectOk(m.set(key, v), `set ${JSON.stringify(key)}`);
          if (v.kind === "null") model.delete(key);
          else model.set(key, v);
        }
        expect(encoded(before), `step ${step.toString()}: the map updated is unchanged`).toBe(beforeBytes);
        agrees(m, model, `seed ${seed.toString()} step ${step.toString()}`);
      }
    });
  }

  it("a weight past 2^63 saturates, and is exact again once the heavy entries are gone", () => {
    let m = expectOk(MapValue.empty.set("small", int(1n)), "small");
    const small = m.weight;
    m = expectOk(m.set("huge", huge), "huge");
    expect(m.weight).toBe(MAX_WEIGHT);
    m = expectOk(m.set("again", huge), "twice");
    expect(m.weight).toBe(MAX_WEIGHT);
    m = m.delete("huge");
    expect(m.weight, "one heavy entry left").toBe(MAX_WEIGHT);
    m = m.delete("again");
    expect(m.weight, "exact once both are gone").toBe(small);
    expect(m.weight).toBe(mapOf([["small", int(1n)]]).weight);
  });

  it("removing the deepest value lowers the depth exactly, however many values share a depth", () => {
    let m = mapOf([
      ["a", deep(7)],
      ["b", deep(7)],
      ["c", deep(2)],
      ["d", int(1n)],
    ]);
    expect(m.depth).toBe(8);
    m = m.delete("a");
    expect(m.depth, "b still nests 7 deep").toBe(8);
    m = m.delete("b");
    expect(m.depth).toBe(3);
    m = m.delete("c");
    expect(m.depth).toBe(1);
    m = m.delete("d");
    expect([m.depth, m.size, m.weight]).toEqual([1, 0, 1n]);
  });
});

describe("an update builds O(log n) nodes, shares the rest, and lists nothing", { timeout: 120_000 }, () => {
  const sizes = [1 << 10, 1 << 14, 1 << 16];
  const key = (i: number): string => `k${i.toString().padStart(8, "0")}`;

  it("keys set in order, the worst case for an unbalanced tree, then every other one removed, stay within the AVL height", () => {
    for (const n of sizes) {
      let m = MapValue.empty;
      for (let i = 0; i < n; i++) m = expectOk(m.set(key(i), int(BigInt(i))), "set");
      expect(m.size).toBe(n);
      expect(treeStats(m).height, `${n.toString()} entries`).toBeLessThanOrEqual(avlHeight(n));
      for (let i = 0; i < n; i += 2) m = m.delete(key(i));
      expect(m.size).toBe(n / 2);
      expect(treeStats(m).height, `${(n / 2).toString()} entries after removals`).toBeLessThanOrEqual(avlHeight(n / 2));
      // built one update at a time, never walked: no listing was made, of this map or of any before it
      expect(treeStats(m)).toMatchObject({ listed: false, listedByCodePoint: false });
    }
  });

  it("each update builds at most two nodes per level and one rotation's more, sharing every other node", () => {
    for (const n of sizes) {
      const m = mapOf(Array.from({ length: n }, (_, i) => [key(i), int(BigInt(i))] as const));
      const bound = 2 * avlHeight(n) + 3;
      let worst = 0;
      for (const k of [key(0), key(n >> 1), key(n - 1), "new", "zzz", key(n >> 2)]) {
        const set = expectOk(m.set(k, int(-1n)), "set");
        const removed = m.delete(k);
        const fresh = [unsharedNodes(m, set), unsharedNodes(m, removed)];
        worst = Math.max(worst, ...fresh);
        for (const f of fresh) expect(f, `${k} in ${n.toString()}`).toBeLessThanOrEqual(bound);
        expect(unsharedNodes(m, set), `set ${k}: at least the path`).toBeGreaterThan(0);
      }
      expect(worst, "the count is not vacuous").toBeGreaterThanOrEqual(avlHeight(n) / 2);
      // an absent key's removal is the map itself
      expect(m.delete("absent")).toBe(m);
    }
  });

  it("a walk that may stop early lists nothing: inspection within its budget, equality at the first difference", () => {
    const n = 20_000;
    let a = expectOk(MapValue.empty.set("a", text("x".repeat(DEBUG_BUDGET))), "a heavy first value");
    let b = expectOk(MapValue.empty.set("b", text("x".repeat(DEBUG_BUDGET))), "another first key");
    for (let i = 0; i < n; i++) {
      a = expectOk(a.set(key(i), int(1n)), "set");
      b = expectOk(b.set(key(i), int(1n)), "set");
    }
    // the first value is over the budget: everything after it is elided, and only that much of the tree is walked
    expect(inspect(a)).toBe('{"a": …, …}');
    expect(String(a)).toBe('{"a": …, …}');
    // the same size, weight and depth, the first keys apart: unequal, found at the first entry
    expect([a.size, a.weight, a.depth]).toEqual([b.size, b.weight, b.depth]);
    expect(Value.equals(a, b)).toBe(false);
    for (const m of [a, b]) expect(treeStats(m), "neither map is listed").toMatchObject({ listed: false, listedByCodePoint: false });
    // equal maps compare to the end, a decoded one from its list, still listing neither
    expect(Value.equals(a, expectOk(cbor.decode(expectOk(cbor.encode(a), "encodes")), "decodes"))).toBe(true);
    expect(treeStats(a).listed, "encoding walks it, keeping nothing").toBe(false);
  });

  it("a map an update made holds nothing but its tree, whatever walks it; one built from entries holds their list", () => {
    let m = MapValue.empty;
    for (let i = 0; i < 1000; i++) m = expectOk(m.set(key(i), int(BigInt(i))), "set");
    const before = m;
    for (let i = 0; i < 1000; i += 3) m = m.delete(key(i));
    m = expectOk(m.set("new", int(1n)), "set");
    expect(m.get(key(1))).toEqual(int(1n));
    // encoding, hashing, equality, inspection, listing, rendering: each walks or lists afresh, and keeps nothing
    const bytes = hexOf(cbor.encode(m));
    expect(hexOf(cbor.encode(m))).toBe(bytes);
    expect(hashCommitment("SHA-256", [], m).ok).toBe(true);
    expect(Value.equals(m, expectOk(cbor.decode(expectOk(cbor.encode(m), "encodes")), "decodes"))).toBe(true);
    expect(inspect(m).length).toBeGreaterThan(0);
    expect(m.entries().length).toBe(m.size);
    expect(codePointView(m).length).toBe(m.size);
    expect(expectOk(json.renderWithin(m, 1 << 20), "renders").length).toBeGreaterThan(0);
    for (const v of [before, m]) expect(treeStats(v)).toMatchObject({ listed: false, listedByCodePoint: false });
    expect(entriesView(m), "a fresh listing each time").not.toBe(entriesView(m));
    // a map built from entries holds their list from the start, and its code-point view once found
    const built = mapOf([["a", int(1n)], ["bb", int(2n)]]);
    expect(treeStats(built)).toMatchObject({ listed: true, listedByCodePoint: false });
    expect(entriesView(built)).toBe(entriesView(built));
    expect(codePointView(built)).toBe(codePointView(built));
    expect(treeStats(built).listedByCodePoint).toBe(true);
    expect(treeStats(expectOk(cbor.decode(expectOk(cbor.encode(m), "encodes")), "decodes") as MapValue).listed).toBe(true);
  });

  it("hashing versions that share their nodes keeps no listing of any of them", () => {
    let m = MapValue.empty;
    for (let i = 0; i < 2000; i++) m = expectOk(m.set(key(i), int(BigInt(i))), "set");
    const versions = Array.from({ length: 50 }, (_, i) => expectOk(m.set(`v${i.toString()}`, int(1n)), "a version"));
    for (const v of versions) {
      expect(hashCommitment("SHA-256", [], v).ok).toBe(true);
      expect(cbor.encodedLengthWithin(v, 1 << 20).ok).toBe(true);
    }
    for (const v of versions) expect(treeStats(v)).toMatchObject({ listed: false, listedByCodePoint: false });
    // and they still share all but their own paths
    for (const v of versions) expect(unsharedNodes(m, v)).toBeLessThanOrEqual(2 * avlHeight(2001) + 3);
  });
});

describe("the code-point view", () => {
  /** Entries of `keys` whose UTF-8 counts every byte read, in canonical order. */
  function counted(keys: readonly string[]): { readonly entries: readonly Entry[]; readonly reads: () => number; readonly weight: bigint } {
    let reads = 0;
    const m = mapOf(keys.map((k) => [k, int(1n)] as const));
    const entries = entriesView(m).map((e) => ({
      key: e.key,
      value: e.value,
      utf8: new Proxy(e.utf8, {
        get: (t, name) => {
          if (typeof name === "string" && /^[0-9]+$/.test(name)) reads++;
          return Reflect.get(t, name) as unknown;
        },
      }),
    }));
    return { entries, reads: () => reads, weight: m.weight };
  }

  const shapes: readonly (readonly [string, readonly string[]])[] = [
    ["one length", Array.from({ length: 4096 }, (_, i) => `k${i.toString().padStart(5, "0")}`)],
    ["a counter's keys", Array.from({ length: 4096 }, (_, i) => `k${i.toString()}`)],
    ["each key a prefix of the next", Array.from({ length: 512 }, (_, i) => "x".repeat(i))],
    ["a long shared prefix, many lengths", Array.from({ length: 2048 }, (_, i) => `${"p".repeat(200)}${"y".repeat(i % 61)}-${i.toString(36)}`)],
    ["many lengths, short", Array.from({ length: 4096 }, (_, i) => `${"z".repeat(i % 97)}-${i.toString(36)}`)],
    ["mixed planes", Array.from({ length: 2048 }, (_, i) => `${["", "｡", "\u{1F600}", "é", ""][i % 5] ?? ""}${i.toString(36)}${"\u{10000}".repeat(i % 3)}`)],
  ];

  it("is code-point order, and reads fewer bytes than 10 per unit of the map's weight", () => {
    for (const [name, keys] of shapes) {
      const { entries, reads, weight } = counted(keys);
      const out = codePointOrder(entries).map((e) => e.key);
      expect(out, name).toEqual([...keys].sort(byBytes));
      expect(reads(), `${name}: ${reads().toString()} bytes read for weight ${weight.toString()}`).toBeLessThan(10 * Number(weight));
    }
  });

  it("of keys of one length is the canonical order itself, read in O(1)", () => {
    const { entries, reads } = counted(Array.from({ length: 1000 }, (_, i) => `k${i.toString().padStart(4, "0")}`));
    expect(codePointOrder(entries)).toBe(entries);
    expect(reads()).toBe(0);
  });

  it("is the order of JSON members", () => {
    const keys = ["\u{1F600}", "｡", "a", "", "é", "aa", "b"];
    const m = mapOf(keys.map((k) => [k, int(1n)] as const));
    expect(expectOk(json.renderWithin(m, 1000), "renders")).toBe('{"":1,"a":1,"aa":1,"b":1,"é":1,"｡":1,"😀":1}');
  });

  it("gives its keys as text values, minted, in the same order and equal to the keys as text", () => {
    const keys = ["\u{1F600}", "｡", "a", "", "é", "aa", "b"];
    const m = mapOf(keys.map((k) => [k, int(1n)] as const));
    const texts = codePointKeys(m);
    expect(texts.map((t) => t.value)).toEqual(codePointView(m).map((e) => e.key));
    for (const t of texts) {
      expect(isValue(t), t.value).toBe(true);
      expect(Value.equals(t, text(t.value)), t.value).toBe(true);
      expect(hexOf(cbor.encode(t)), t.value).toBe(hexOf(cbor.encode(text(t.value))));
    }
  });
});

describe("set, delete and has refuse what a caller passes past the types", () => {
  const one = int(1n);
  const m = mapOf([["a", one]]);
  const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });

  it("set: a key that is not a string, then a value that is not a value, then an unpaired surrogate, then the depth", () => {
    for (const k of [1, null, undefined, {}, Symbol("k"), new String("a")]) expect(m.set(bad(k), one)).toEqual(mismatch("expected a string key"));
    for (const v of [1, 1n, "a", null, undefined, {}, Object.create(MapValue.prototype)]) expect(m.set("b", bad(v))).toEqual(mismatch("expected a canonical value"));
    expect(m.set(bad(1), bad(1)), "the key first").toEqual(mismatch("expected a string key"));
    expect(m.set("\uD800", one)).toEqual({ ok: false, error: { kind: "UnpairedSurrogate" } });
    expect(m.set("\uDC00a", Value.null), "a null value too, as MapValue.of refuses it").toEqual({ ok: false, error: { kind: "UnpairedSurrogate" } });
    expect(m.set("b", deep(MAX_DEPTH))).toEqual({ ok: false, error: { kind: "DepthExceeded", limit: MAX_DEPTH } });
    const deepest = expectOk(m.set("b", deep(MAX_DEPTH - 1)), "a value one level within");
    expect(deepest.depth).toBe(MAX_DEPTH);
    expect(m.size, "the map refused is unchanged").toBe(1);
  });

  it("delete and has: a key that is not a string, or holds an unpaired surrogate, is never held", () => {
    for (const k of [1, null, undefined, {}, Symbol("k"), "\uD800", "b"]) {
      expect(m.delete(bad(k)), typeof k).toBe(m);
      expect(m.has(bad(k)), typeof k).toBe(false);
    }
    expect(m.has("a")).toBe(true);
    expect(m.delete("a").size).toBe(0);
  });

  it("set with null removes, and the empty map is a frozen value", () => {
    expect(expectOk(m.set("a", Value.null), "removes").size).toBe(0);
    expect(expectOk(m.set("absent", Value.null), "nothing to remove")).toBe(m);
    expect(isValue(MapValue.empty)).toBe(true);
    expect(Object.isFrozen(MapValue.empty)).toBe(true);
    expect(hexOf(cbor.encode(MapValue.empty))).toBe("a0");
    expect(Value.equals(MapValue.empty, mapOf([]))).toBe(true);
  });
});

describe("nothing is minted from what a caller built", () => {
  const one = int(1n);
  const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });
  // a genuine byte string of a million bytes, in a tree that claims it weighs 1
  const heavy = expectOk(BytesValue.of(new Uint8Array(1_000_000)), "bytes");
  const fakeRoot = { entry: { key: "a", utf8: Uint8Array.of(0x61), value: heavy }, left: undefined, right: undefined, height: 1, deepest: 0, weight: 1 };
  const forged = Reflect.construct(MapValue as unknown as new (...args: unknown[]) => MapValue, [Symbol("canonical value"), fakeRoot, 1, undefined, 1, 1n]);

  it("a map built by calling the constructor is no map: set refuses it, delete gives it back, and its tree is never read", () => {
    expect(isValue(forged)).toBe(false);
    expect(forged.set("b", one)).toEqual(mismatch("expected a canonical map"));
    expect(forged.set("a", Value.null)).toEqual(mismatch("expected a canonical map"));
    const removed = forged.delete("a");
    expect(removed, "given back as it is").toBe(forged);
    expect(isValue(removed)).toBe(false);
    expect([forged.get("a"), forged.has("a"), forged.entries(), forged.keys(), forged.encodedEntries()]).toEqual([undefined, false, [], [], []]);
    expect([entriesView(forged), codePointView(forged), codePointKeys(forged)]).toEqual([[], [], []]);
    expect(treeStats(forged)).toEqual({ height: 0, listed: false, listedByCodePoint: false });
    expect(unsharedNodes(MapValue.empty, forged)).toBe(0);
    // and on something that is not a map at all, the methods answer as for a forged one, never throwing
    const notAMap = {} as MapValue;
    expect(MapValue.prototype.set.call(notAMap, "a", one)).toEqual(mismatch("expected a canonical map"));
    expect(MapValue.prototype.delete.call(notAMap, "a")).toBe(notAMap);
    expect(MapValue.prototype.get.call(notAMap, "a")).toBeUndefined();
    expect(MapValue.prototype.get.call(heavy as unknown as MapValue, "a"), "a value of another kind").toBeUndefined();
  });

  it("the constructors' helpers are private to the class at run time, not to the types alone", () => {
    const callable = (c: object): string[] => Object.getOwnPropertyNames(c).filter((k) => typeof Reflect.get(c, k) === "function");
    expect(callable(MapValue).sort()).toEqual(["of"]);
    expect(callable(RationalValue).sort()).toEqual(["decimal", "number", "of"]);
    expect(callable(MapShape).sort()).toEqual(["of"]);
    for (const name of ["rooted", "sorted", "closed", "add", "entry", "checked"]) {
      expect(Reflect.get(MapValue, name), name).toBeUndefined();
      expect(Reflect.get(RationalValue, name), name).toBeUndefined();
    }
  });

  it("the internal builders check what they are given: no array or map of anything but values, no forged shape", () => {
    expect(arrayOfValues(bad([one, 5]))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(arrayOfValues(bad([one, Object.create(MapValue.prototype)]))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(MapShape.of(bad("ab"))).toEqual(mismatch("expected an array"));
    expect(MapShape.of(bad(["a", 5]))).toEqual(mismatch("expected a string key at index 1"));
    const shape = expectOk(MapShape.of(["a", "b"]), "a shape");
    expect(shape.map(bad([one, 5]))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(shape.map(bad(5))).toEqual(mismatch("expected an array"));
    const fakeShape = Reflect.construct(MapShape as unknown as new (...args: unknown[]) => MapShape, [Symbol("shape"), [["a", Uint8Array.of(0x62), 0]]]);
    expect(fakeShape.map([one])).toEqual(mismatch("expected a map shape"));
    expect(fakeShape.size).toBe(0);
  });

  it("an array keeps each item as it was read and checked, once: an accessor or an overridden slice changes nothing", () => {
    for (const build of [arrayOfValues, (xs: readonly V[]) => ArrayValue.of(xs)]) {
      let reads = 0;
      const items: V[] = [one];
      // the first read is a value of weight 1, every later one a million bytes
      Object.defineProperty(items, 1, { enumerable: true, get: () => (reads++ === 0 ? one : heavy) });
      Object.defineProperty(items, "slice", { value: () => [one, heavy] });
      const a = expectOk(build(items), "builds");
      expect([a.length, a.weight], "the weight of what was checked").toEqual([2, 3n]);
      expect(a.items[1], "the item kept is the one read").toBe(one);
      expect(Object.getOwnPropertyDescriptor(a.items, 1), "a data property, frozen").toEqual({ value: one, writable: false, enumerable: true, configurable: false });
      expect(hexOf(cbor.encode(a))).toBe("820101");
    }
  });

  it("core's internal entry point hands out no map's own entries or key bytes", async () => {
    const internal: Record<string, unknown> = await import("../src/internal.js");
    for (const name of ["codePointView", "entriesView", "arrayOfValues", "mapOfValues", "mapOfCanonical", "utf8View"]) expect(Object.keys(internal), name).not.toContain(name);
    const m = mapOf([["a", one]]);
    const [key] = codePointKeys(m);
    if (key === undefined) throw new Error("a key");
    const bytes = key.utf8();
    bytes[0] = 0x62;
    expect([m.get("a"), m.get("b"), hexOf(cbor.encode(key))], "the copy's change reaches neither the map nor the key").toEqual([one, undefined, "6161"]);
  });
});

describe("a map shape builds the map of its keys from their values", () => {
  it("in canonical order whatever the order given, dropping nulls, and refuses repeats, unpaired surrogates and depth", () => {
    const keys = ["\u{1F600}", "b", "aa", "", "é"];
    const shape = expectOk(MapShape.of(keys), "a shape");
    expect(shape.size).toBe(5);
    const values = [int(1n), text("x"), Value.null, deep(3), int(5n)];
    const built = expectOk(shape.map(values), "builds");
    const ref = mapOf(keys.map((k, i) => [k, values[i] ?? Value.null] as const));
    expect(hexOf(cbor.encode(built))).toBe(hexOf(cbor.encode(ref)));
    expect([built.size, built.depth, built.weight]).toEqual([ref.size, ref.depth, ref.weight]);
    expect(MapShape.of(["a", "b", "a"])).toEqual({ ok: false, error: { kind: "DuplicateKey", key: expect.anything() as unknown } });
    expect(MapShape.of(["a", "\uD800"])).toEqual({ ok: false, error: { kind: "UnpairedSurrogate" } });
    expect(expectOk(MapShape.of(["a"]), "a shape").map([deep(MAX_DEPTH)])).toEqual({ ok: false, error: { kind: "DepthExceeded", limit: MAX_DEPTH } });
    expect(expectOk(MapShape.of([]), "no keys").map([]).ok).toBe(true);
  });
});

describe("a decoded map is the tree of its entries, built without comparing a key", () => {
  it("decodes to the map MapValue.of builds, and updates as one", () => {
    const m = mapOf(KEYS.map((k, i) => [k, int(BigInt(i))] as const));
    const decoded = expectOk(cbor.decode(expectOk(cbor.encode(m), "encodes")), "decodes");
    if (decoded.kind !== "map") throw new Error("a map");
    expect(Value.equals(decoded, m)).toBe(true);
    expect(treeStats(decoded).height).toBeLessThanOrEqual(avlHeight(decoded.size));
    const set = expectOk(decoded.set("new", int(1n)), "set");
    expect(set.size).toBe(m.size + 1);
    expect(decoded.get("k3")).toEqual(int(BigInt(KEYS.indexOf("k3"))));
    const key = expectOk(TextValue.of("k3"), "text");
    expect(decoded.get(key.value)).toBe(decoded.entries().find(([k]) => k === "k3")?.[1]);
  });
});
