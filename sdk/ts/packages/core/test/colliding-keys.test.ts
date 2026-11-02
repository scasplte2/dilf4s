import { afterEach, describe, expect, it, vi } from "vitest";

import { cbor, codec, json, MapValue, Value } from "../src/index.js";
import { KeySet } from "../src/keyset.js";
import { expectOk } from "./vectors.js";

// V8 hashes an integer-like string by its value, with no seed: keys 65536 apart share a bucket of a `Map` or `Set`, and
// n of them cost O(n^2) to insert. A map's keys are the caller's, so they must only ever be compared: no `Map` or `Set`
// may see one. Nothing here measures time: the test watches every `Map` and `Set` lookup and insertion instead.
describe("caller-chosen map keys are compared, never hashed", () => {
  const n = 1 << 12;
  const keys = Array.from({ length: n }, (_, i) => String(i * 65536));
  const isKey = (x: unknown): boolean => typeof x === "string" && /^[0-9]+$/.test(x) && Number(x) % 65536 === 0;

  const watch = () => [
    vi.spyOn(Map.prototype, "get"),
    vi.spyOn(Map.prototype, "set"),
    vi.spyOn(Map.prototype, "has"),
    vi.spyOn(Set.prototype, "add"),
    vi.spyOn(Set.prototype, "has"),
  ];
  const hashed = (spies: ReturnType<typeof watch>): number => spies.reduce((sum, s) => sum + s.mock.calls.filter((args) => isKey(args[0])).length, 0);

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("the watch sees a key put in a Map", () => {
    const spies = watch();
    new Map<string, number>().set(keys[1] ?? "", 1);
    expect(hashed(spies)).toBe(1);
  });

  it("building, looking up, encoding, decoding and parsing a map", () => {
    const spies = watch();
    const m = expectOk(MapValue.of(keys.map((k) => [k, Value.int(1n)] as const)), "builds");
    const decoded = expectOk(cbor.decode(expectOk(cbor.encode(m), "encodes")), "decodes");
    const parsed = expectOk(json.parse(`{${keys.map((k) => `"${k}":1`).join(",")}}`), "parses");
    const typed = expectOk(codec.textMap(codec.uint).decode(m), "decodes as a typed map");
    const repeated = MapValue.of([...keys, keys[7] ?? ""].map((k) => [k, Value.int(1n)] as const));
    const repeatedJson = json.parse(`{${[...keys, keys[7] ?? ""].map((k) => `"${k}":1`).join(",")}}`);
    expect(hashed(spies)).toBe(0);
    expect(m.size).toBe(n);
    expect(m.get(keys[9] ?? "")).toEqual(Value.int(1n));
    expect(m.get("1")).toBeUndefined();
    expect(m.get("\ud800")).toBeUndefined();
    expect(decoded.kind === "map" && decoded.get(keys[n - 1] ?? "") !== undefined).toBe(true);
    expect(parsed.kind === "map" && parsed.size === n).toBe(true);
    expect([typed.size, typed.get(keys[3] ?? ""), typed.has("3"), [...typed.keys()].length]).toEqual([n, 1n, false, n]);
    expect(repeated).toEqual({ ok: false, error: { kind: "DuplicateKey", key: keys[7] } });
    expect(repeatedJson).toEqual({ ok: false, error: { kind: "DuplicateKey", key: keys[7] } });
    expect(hashed(spies)).toBe(0);
  });

  it("a key set finds every key it holds and none it does not, in any insertion order", () => {
    const set = new KeySet();
    const shuffled = keys.map((k, i) => keys[(i * 2654435761) % n] ?? k);
    const added = shuffled.map((k) => set.add(k));
    expect(added.filter((a) => a).length).toBe(new Set(shuffled).size);
    expect(keys.every((k) => set.has(k) === shuffled.includes(k))).toBe(true);
    expect(set.has("1")).toBe(false);
    expect(set.add(shuffled[0] ?? "")).toBe(false);
  });
});
