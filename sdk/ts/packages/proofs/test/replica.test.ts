import { afterEach, describe, expect, it, vi } from "vitest";

import { type Value } from "@kd5ujc/dilf4s-core";

import { trie, versioned } from "../src/index.js";
import { TRIE } from "../src/trie-internal.js";
import { expectErr, expectOk, int, hexOf } from "./vectors.js";

// The replica's tries (docs/specs/versioned-trie.md section 7): updated incrementally, equal to a full rebuild after
// every delta (deletes, re-inserts, an emptied state, refused deltas), hashing only the changed keys' paths whatever
// the state's size, and never putting a peer's key in a `Map` or `Set`. Work is counted (node hashes, hashed-table
// calls), never timed.

const vt = expectOk(versioned.VersionedTrie.of("SHA-256", versioned.VersionKey.long), "profile");
const key = (s: string): versioned.StateKey => expectOk(versioned.StateKey.parse(s), `state key ${s}`);
const vkey = (v: bigint): Uint8Array => expectOk(vt.versionKey.encode(v), "vkey");
const hex = (b: Uint8Array): string => hexOf(b);

type Delta = versioned.Delta<bigint>;
type Head = versioned.Head<bigint>;

/** The reference: the state and history as plain maps, with roots rebuilt from scratch by `trieRoot`. */
interface Model {
  readonly state: ReadonlyMap<string, readonly [versioned.StateKey, Value]>;
  readonly history: ReadonlyMap<string, readonly [bigint, Uint8Array]>;
  readonly head: Head | undefined;
}

const emptyModel: Model = { state: new Map(), history: new Map(), head: undefined };

const sortedState = (m: Model): (readonly [versioned.StateKey, Value])[] => [...m.state.values()].sort((a, b) => (a[0].value < b[0].value ? -1 : a[0].value > b[0].value ? 1 : 0));

/** The delta writing `upserts` and removing `removes` at `version`, its head rebuilt from the whole state. */
function step(m: Model, version: bigint, upserts: readonly (readonly [versioned.StateKey, Value])[], removes: readonly versioned.StateKey[]): [Delta, Model] {
  const state = new Map(m.state);
  for (const k of removes) state.delete(k.value);
  for (const [k, v] of upserts) state.set(k.value, [k, v]);
  const stateRoot = expectOk(trie.trieRoot(vt.hashing, vt.stateMode, [...state.values()].map(([k, v]) => [k.bytes(), expectOk(vt.valueHash(v), "hash")] as const)), "state root");
  const history = new Map(m.history);
  history.set(hex(vkey(version)), [version, stateRoot]);
  const historyRoot = expectOk(trie.trieRoot(vt.hashing, vt.historyMode, [...history.values()].map(([v, s]) => [vkey(v), expectOk(vt.bytesHash(s), "bytes hash")] as const)), "history root");
  const head: Head = { version, stateRoot, historyRoot };
  const delta: Delta = { ...(m.head === undefined ? {} : { parent: m.head }), head, upserts, removes };
  return [delta, { state, history, head }];
}

const snapshotOf = (m: Model): versioned.Snapshot<bigint> => {
  if (m.head === undefined) throw new Error("no head");
  return { head: m.head, entries: sortedState(m), history: [...m.history.values()].sort((a, b) => (a[0] < b[0] ? -1 : 1)) };
};

const entriesOf = (r: versioned.Replica<bigint>): unknown => r.entries().map(([k, v]) => [k.value, v]);

/** xorshift64*: a fixed, seeded stream. */
function rng(seed: bigint): (n: number) => number {
  let x = seed;
  const mask = (1n << 64n) - 1n;
  return (n) => {
    x ^= x >> 12n;
    x = (x ^ (x << 25n)) & mask;
    x ^= x >> 27n;
    return Number(((x * 0x2545f4914f6cdd1dn) & mask) % BigInt(n));
  };
}

/** Keys where the trie restructures: prefixes of each other (branch values), shared namespaces, single-bit splits. */
const universe = ["a", "a/b", "a/b/c", "a/b/c/d", "a/c", "ab", "ab/c", "b", "b0", "b1", "b2", "c", "fiber/1", "fiber/2", "fiber/10", "fiberx/1", "z", "z/z", "0", "00", "01", "1", "q.r", "q-r", "q_r"].map(key);

describe("a replica's incremental roots", { timeout: 60_000 }, () => {
  it("equal a full rebuild after every delta, and a refused delta leaves the replica as it was", () => {
    const forged = new Uint8Array(32).fill(0xab);
    for (let seed = 1n; seed <= 8n; seed++) {
      const next = rng(seed * 0x9e3779b97f4a7c15n);
      let model = emptyModel;
      let replica = expectOk(versioned.Replica.empty(vt), "Replica.empty");
      for (let i = 1; i <= 80; i++) {
        const version = BigInt(i * 3);
        // every 20th delta empties the state; the next ones re-insert into it. A remove may name an absent key, which
        // changes nothing
        const chosen = new Map<string, readonly [versioned.StateKey, Value | undefined]>();
        if (i % 20 === 0) for (const [k] of model.state.values()) chosen.set(k.value, [k, undefined]);
        else
          for (let j = next(5); j >= 0; j--) {
            const k = universe[next(universe.length)] ?? key("a");
            chosen.set(k.value, [k, next(3) === 0 ? undefined : int(BigInt(next(1000)))]);
          }
        const upserts = [...chosen.values()].flatMap(([k, v]) => (v === undefined ? [] : [[k, v] as const]));
        const removes = [...chosen.values()].flatMap(([k, v]) => (v === undefined ? [k] : []));
        const [delta, after] = step(model, version, upserts, removes);
        const at = `seed ${seed.toString()} step ${i.toString()}`;

        const before = [expectOk(replica.root(), at), entriesOf(replica), replica.size];
        expect(expectErr(replica.applyDelta({ ...delta, head: { ...delta.head, stateRoot: forged } }), at)).toMatchObject({ kind: "RootMismatch", component: "state" });
        expect(expectErr(replica.applyDelta({ ...delta, head: { ...delta.head, historyRoot: forged } }), at)).toMatchObject({ kind: "RootMismatch", component: "history" });
        expect([expectOk(replica.root(), at), entriesOf(replica), replica.size]).toEqual(before);

        const applied = expectOk(replica.applyDelta(delta), at);
        // the replica it was applied to is unchanged
        expect([expectOk(replica.root(), at), entriesOf(replica), replica.size]).toEqual(before);
        replica = applied;
        model = after;
        expect(replica.head).toEqual(model.head);
        expect(entriesOf(replica)).toEqual(sortedState(model).map(([k, v]) => [k.value, v]));
        expect(replica.size).toBe(model.state.size);
        for (const k of universe) expect(expectOk(replica.get(k), "get")).toEqual(model.state.get(k.value)?.[1]);
        expect(replica.history().map(([v, s]) => [v, hex(s)])).toEqual([...model.history.values()].map(([v, s]) => [v, hex(s)]));
        if (i % 20 === 0) expect([replica.size, hex(replica.head?.stateRoot ?? new Uint8Array(0))]).toEqual([0, hex(vt.hashing.empty)]);
        if (i % 7 === 0) {
          const restored = expectOk(versioned.Replica.fromSnapshot(vt, model.head ?? delta.head, snapshotOf(model)), at);
          expect([expectOk(restored.root(), at), entriesOf(restored)]).toEqual([expectOk(replica.root(), at), entriesOf(replica)]);
        }
      }
    }
  });
});

describe("a replica's work per delta", { timeout: 60_000 }, () => {
  const n = 1 << 12;
  const families: readonly (readonly [string, readonly versioned.StateKey[]])[] = [
    ["ordinary", Array.from({ length: n }, (_, i) => key(`k/${i.toString().padStart(4, "0")}`))],
    // keys sharing one Java String hash code: every mix of "an" and "c0"
    ["java-colliding", Array.from({ length: n }, (_, i) => key(Array.from({ length: 12 }, (_, b) => ((i >> b) & 1 ? "c0" : "an")).join("")))],
    // keys V8 hashes alike (integer-like strings 65536 apart share a bucket)
    ["v8-colliding", Array.from({ length: n }, (_, i) => key(String(i * 65536)))],
  ];

  afterEach(() => {
    vi.restoreAllMocks();
  });

  const watchNodes = () => [vi.spyOn(TRIE, "leafBits"), vi.spyOn(TRIE, "branchBits")];
  const calls = (spies: readonly { readonly mock: { readonly calls: readonly unknown[] } }[]): number => spies.reduce((t, s) => t + s.mock.calls.length, 0);

  /** Bit positions at which `keys` do not all agree (past a key's end counts): every branch on a path sits at one. */
  const varyingBits = (keys: readonly Uint8Array[]): number => {
    const longest = Math.max(...keys.map((k) => k.length)) * 8;
    const bit = (k: Uint8Array, i: number): number | undefined => (i < k.length * 8 ? ((k[i >> 3] ?? 0) >> (7 - (i & 7))) & 1 : undefined);
    const first = keys[0] ?? new Uint8Array(0);
    let count = 0;
    for (let i = 0; i < longest; i++) if (keys.some((k) => bit(k, i) !== bit(first, i))) count++;
    return count;
  };

  /**
   * The most node hashes one delta may make: two top-trie roots (the parent check and the claimed parent, five node
   * hashes each), and for each changed key and the new version its path re-hashed with at most one lowered or merged
   * node and one new leaf.
   */
  const bound = (changes: number, stateDepth: number, historyDepth: number): number => 2 * 5 + changes * (stateDepth + 1 + 2) + (historyDepth + 1 + 2);

  const seeded = (keys: readonly versioned.StateKey[]): [versioned.Replica<bigint>, Model] => {
    const [delta, model] = step(emptyModel, 1n, keys.map((k, i) => [k, int(BigInt(i))] as const), []);
    return [expectOk(versioned.Replica.fromSnapshot(vt, delta.head, snapshotOf(model)), "restores"), model];
  };

  /** A delta changing three keys (one updated, one removed, one new): its node hashes, and their bound. */
  const threeChanges = (keys: readonly versioned.StateKey[]): [number, number] => {
    const [replica, model] = seeded(keys);
    const m = keys.length;
    const fresh = key("fresh/key");
    const [delta] = step(model, 2n, [[keys[Math.floor(m / 3)] ?? fresh, int(-1n)], [fresh, int(-2n)]], [keys[Math.floor((2 * m) / 3)] ?? fresh]);
    const spies = watchNodes();
    expectOk(replica.applyDelta(delta), "applies");
    const hashes = calls(spies);
    vi.restoreAllMocks();
    return [hashes, bound(3, varyingBits([...keys, fresh].map((k) => k.bytes())), varyingBits([vkey(1n), vkey(2n)]))];
  };

  for (const [name, keys] of families) {
    it(`hashes only the changed keys' paths: ${name} keys`, () => {
      const spies = watchNodes();
      seeded(keys);
      const built = calls(spies);
      vi.restoreAllMocks();
      // the reference root and the restore each hash every node once, and there is a node per key
      expect(built).toBeGreaterThanOrEqual(2 * n);
      const [hashes, limit] = threeChanges(keys);
      expect(hashes).toBeLessThanOrEqual(limit);
      expect(hashes * 40).toBeLessThan(built);
    });
  }

  it("the same delta on a state 64 times smaller makes about as many node hashes", () => {
    const ordinary = families[0]?.[1] ?? [];
    const [small] = threeChanges(ordinary.slice(0, n / 64));
    const [large] = threeChanges(ordinary);
    expect(large).toBeLessThanOrEqual(small + 3 * 8);
  });

  it("a deep key costs its depth, and a key beside it only its own", () => {
    // every prefix of a 255-character key that is itself a key: each a branch value above the next
    const long = Array.from({ length: 4 }, () => "a".repeat(63)).join("/");
    const chain = Array.from({ length: long.length }, (_, i) => versioned.StateKey.parse(long.slice(0, i + 1))).flatMap((r) => (r.ok ? [r.value] : []));
    const bulk = families[0]?.[1] ?? [];
    const [replica, model] = seeded([...chain, ...bulk]);
    const deepest = chain[chain.length - 1] ?? key("a");
    const [d2, m2] = step(model, 2n, [[deepest, int(7n)]], []);
    let spies = watchNodes();
    const r2 = expectOk(replica.applyDelta(d2), "deep");
    const deep = calls(spies);
    vi.restoreAllMocks();
    expect(deep).toBeGreaterThan(chain.length);
    expect(deep).toBeLessThanOrEqual(bound(1, chain.length + 1, 2));
    const [d3] = step(m2, 3n, [[bulk[7] ?? key("a"), int(7n)]], []);
    spies = watchNodes();
    expectOk(r2.applyDelta(d3), "shallow");
    const shallow = calls(spies);
    expect(shallow).toBeLessThanOrEqual(bound(1, varyingBits(bulk.map((k) => k.bytes())) + 1, 2));
  });

  it("no Map or Set sees a peer's key, restoring or applying a delta", () => {
    const keys = families[2]?.[1] ?? [];
    const names = new Set(keys.map((k) => k.value));
    const isPeerKey = (x: unknown): boolean => typeof x === "string" && /^[0-9]+$/.test(x) && Number(x) % 65536 === 0;
    const [delta, model] = step(emptyModel, 1n, keys.map((k, i) => [k, int(BigInt(i))] as const), []);
    const [next] = step(model, 2n, keys.slice(0, 64).map((k) => [k, int(-1n)] as const), keys.slice(64, 128));
    const watch = [vi.spyOn(Map.prototype, "get"), vi.spyOn(Map.prototype, "set"), vi.spyOn(Map.prototype, "has"), vi.spyOn(Set.prototype, "add"), vi.spyOn(Set.prototype, "has")];
    const replica = expectOk(versioned.Replica.fromSnapshot(vt, delta.head, snapshotOf(model)), "restores");
    const applied = expectOk(replica.applyDelta(next), "applies");
    expect(expectOk(applied.get(keys[0] ?? key("a")), "get")).toEqual(int(-1n));
    expect(expectOk(applied.get(keys[64] ?? key("a")), "get")).toBeUndefined();
    expect(applied.size).toBe(n - 64);
    const hashed = watch.reduce((t, s) => t + s.mock.calls.filter((args) => isPeerKey(args[0])).length, 0);
    expect(hashed).toBe(0);
    expect(names.size).toBe(n);
  });

  it("the watch sees a key put in a Map", () => {
    const watch = vi.spyOn(Map.prototype, "set");
    new Map<string, number>().set(String(65536 * 3), 1);
    expect(watch.mock.calls.filter((args) => args[0] === "196608").length).toBe(1);
  });
});
