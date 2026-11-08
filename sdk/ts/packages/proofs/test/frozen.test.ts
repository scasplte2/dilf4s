// The proofs package's classes, their prototypes, its exported objects and tables (the trie's operations table, the
// Poseidon tables, the version keys) and the refusals it shares are frozen when their modules load
// (canonical-encoding.md section 1, "Immutable models"), as core's are (core/test/frozen.test.ts, whose walk and write
// attempts this repeats).
import { readdirSync, readFileSync } from "node:fs";
import { runInNewContext } from "node:vm";
import { describe, expect, it } from "vitest";

import { Value } from "@kd5ujc/dilf4s-core";

import { merkle, poseidon, trie, versioned } from "../src/index.js";
import { WIDTH3 } from "../src/poseidon-constants.js";
import { Oversized } from "../src/check.js";
import { TRIE } from "../src/trie-internal.js";
import { expectOk } from "./vectors.js";

/** `x` as a `T`, past the types, as a JavaScript caller passes it. */
const bad = (x: unknown): never => x as never;

/** The realm's own prototypes (`Object.prototype`, `Map.prototype`, the iterators'...), where the walk stops. */
const intrinsics: ReadonlySet<unknown> = new Set([
  Object.getPrototypeOf([].values()),
  Object.getPrototypeOf(Object.getPrototypeOf([].values())),
  Object.getPrototypeOf(Uint8Array),
  Object.getPrototypeOf(Uint8Array.prototype),
  ...Object.getOwnPropertyNames(globalThis).flatMap((name): unknown[] => {
    const g: unknown = Reflect.get(globalThis, name);
    const p: unknown = typeof g === "function" ? Reflect.get(g, "prototype") : undefined;
    return typeof p === "object" && p !== null ? [p, Object.getPrototypeOf(p) as unknown] : [];
  }),
]);

/** Whether `f` is a class: its `prototype` is fixed, as a class's is and a plain function's is not. */
const isClass = (f: unknown): boolean => typeof f === "function" && Object.getOwnPropertyDescriptor(f, "prototype")?.writable === false;

/**
 * Every object and class reachable from `roots` through own properties (values and accessors), prototypes and a
 * class's superclass, up to the realm's own: iterative, with a visited set. A plain function is not entered.
 */
function reachable(roots: readonly unknown[]): readonly object[] {
  const seen = new Set<object>();
  const todo = [...roots];
  while (todo.length > 0) {
    const x = todo.pop();
    const entered = (typeof x === "object" && x !== null) || isClass(x);
    if (!entered || seen.has(x as object) || intrinsics.has(x)) continue;
    const o = x as object;
    seen.add(o);
    todo.push(Object.getPrototypeOf(o));
    for (const key of Reflect.ownKeys(o)) {
      const d = Object.getOwnPropertyDescriptor(o, key);
      if (d !== undefined) todo.push(d.value, Reflect.get(d, "get"), Reflect.get(d, "set"));
    }
  }
  return [...seen];
}

const sha256 = expectOk(merkle.Rfc6962MerkleHashing.of("SHA-256"), "the RFC 6962 profile");
const [leafA, leafB] = [expectOk(sha256.leaf(Uint8Array.of(1)), "leaf a"), expectOk(sha256.leaf(Uint8Array.of(2)), "leaf b")];
const merkleRoot = expectOk(merkle.merkleRoot(sha256, [leafA, leafB]), "a two-leaf root");
const inclusion = expectOk(merkle.InclusionProof.of(0n, 2n, [leafB]), "leaf a's proof");

const hashing = expectOk(trie.TrieHashing.of("SHA-256"), "the trie's hashing");
const mode = expectOk(trie.KeyMode.fixed(1), "fixed(1)");
const valueHash = expectOk(hashing.valueHash(Value.null), "null's value hash");
const trieRoot = expectOk(trie.trieRoot(hashing, mode, [[Uint8Array.of(1), valueHash]]), "a one-key trie's root");
const trieProof = expectOk(trie.TrieProof.fromRecords(hashing, [{ s: { n: 8n, b: Uint8Array.of(1) }, v: valueHash }]), "its proof");

const leaf = expectOk(poseidon.Fr.of(5n), "a field element");
const poseidonProof = expectOk(poseidon.PoseidonMerkleProof.of(0n, [poseidon.Fr.zero]), "a one-level proof");
const poseidonRoot = expectOk(poseidon.computeRoot(leaf, poseidonProof), "its root");

const vt = expectOk(versioned.VersionedTrie.of("SHA-256", versioned.VersionKey.long), "a versioned trie");

/** One instance of each class a caller is given, as the walk's further roots. */
function instances(): readonly unknown[] {
  const tagged = expectOk(merkle.TaggedMerkleHashing.of("SHA-256"), "the tagged profile");
  return [
    sha256,
    tagged,
    inclusion,
    expectOk(merkle.ConsistencyProof.of(1n, 2n, [leafB]), "a consistency proof"),
    hashing,
    mode,
    expectOk(trie.KeyMode.variable(4), "variable(4)"),
    expectOk(trie.Segment.of(8n, Uint8Array.of(1)), "a segment"),
    trieProof,
    expectOk(trie.check(hashing, mode, trieRoot, trieProof), "a verified proof"),
    leaf,
    poseidonProof,
    vt,
    expectOk(versioned.StateKey.parse("a/b"), "a state key"),
    expectOk(versioned.StateNamespace.parse("a"), "a namespace"),
    expectOk(versioned.Replica.empty(vt), "an empty replica"),
    expectOk(vt.versionKey.encode(1n), "a version key's bytes, the caller's own"),
  ];
}

describe("the proofs package's classes, their prototypes and its exported objects are frozen", { timeout: 60_000 }, () => {
  it("every object and class reachable from any of its modules, as shipped (entry points or not), and its instances", async () => {
    // each module's exports, as a caller importing the shipped file by its URL gets them
    const base = new URL("../src/", import.meta.url);
    const files = readdirSync(base, { recursive: true, encoding: "utf8" }).filter((f) => f.endsWith(".ts"));
    expect(files, "the package's modules").toEqual(expect.arrayContaining(["trie-internal.ts", "poseidon-constants.ts", "bits.ts"]));
    const modules = await Promise.all(files.map(async (f) => (await import(/* @vite-ignore */ new URL(f, base).href)) as Record<string, unknown>));
    // a version key's encoding is a fresh array of the one caller's, as any bytes a function returns are
    const roots = [...modules.flatMap((m) => Object.values(m)), ...instances().slice(0, -1)];
    const reached = reachable(roots);
    expect(reached.filter(isClass).length, "the package's classes").toBeGreaterThanOrEqual(19);
    expect(reached, "the trie's operations table and the Poseidon tables").toEqual(expect.arrayContaining([TRIE, WIDTH3, WIDTH3.mds, WIDTH3.mds[0], WIDTH3.constants]));
    // a module namespace is not frozen, but no code can write to it
    const namespace = (o: object): boolean => Object.prototype.toString.call(o) === "[object Module]";
    expect(reached.filter((o) => !Object.isFrozen(o) && !namespace(o)).map((o) => (isClass(o) ? `class ${(o as { name: string }).name}` : Object.prototype.toString.call(o)))).toEqual([]);
  });

  it("the versioned trie's slots are the bytes of their keys, as numbers", () => {
    expect([versioned.VERSION_SLOT, versioned.STATE_SLOT, versioned.HISTORY_SLOT]).toEqual([0, 1, 2]);
  });

  it("no class in proofs or signing keeps state in a TypeScript-private field, which JavaScript reads as any other", () => {
    // `private x` is a property of the frozen instance, its value as reachable as a public one's; `#x` is not
    const tsPrivate = /^\s*(?:private|protected)\s+(?:readonly\s+)?\w+\s*[:;?=]/m;
    const sources = ["../src/", "../../signing/src/"].flatMap((dir) => {
      const base = new URL(dir, import.meta.url);
      return readdirSync(base, { recursive: true, encoding: "utf8" })
        .filter((f) => f.endsWith(".ts"))
        .map((f) => [`${dir}${f}`, readFileSync(new URL(f, base), "utf8")] as const);
    });
    expect(sources.length, "the two packages' modules").toBeGreaterThan(20);
    expect(sources.filter(([, text]) => tsPrivate.test(text)).map(([f]) => f)).toEqual([]);
    expect(tsPrivate.test("  private readonly top: Node | undefined,"), "the scan finds one").toBe(true);
  });
});

type Mode = "strict" | "sloppy";

/** The outcome of `body` (an expression over `t`, `k` and `x`) run as script in `mode`: `ok:` its value, or `throw:` the error's name. */
function attempt(mode: Mode, body: string, t: object, k: PropertyKey, x: unknown): string {
  const script = `${mode === "strict" ? '"use strict";' : ""}(function () { try { return "ok:" + String(${body}); } catch (e) { return "throw:" + e.name; } })()`;
  return runInNewContext(script, { t, k, x }) as string;
}

/**
 * Every way a caller might change `t[k]` to `x`, and what each gives in each mode. A frozen object refuses each one:
 * strict code gets a TypeError for an assignment, sloppy code has it ignored, and `Reflect` answers false in both.
 */
const writes: readonly { readonly name: string; readonly body: string; readonly strict: string; readonly sloppy: string }[] = [
  { name: "assignment", body: "(t[k] = x, 'done')", strict: "throw:TypeError", sloppy: "ok:done" },
  { name: "defineProperty", body: "Object.defineProperty(t, k, { value: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
  { name: "Reflect.set", body: "Reflect.set(t, k, x)", strict: "ok:false", sloppy: "ok:false" },
  { name: "Reflect.defineProperty", body: "Reflect.defineProperty(t, k, { value: x })", strict: "ok:false", sloppy: "ok:false" },
  { name: "delete", body: "delete t[k]", strict: "throw:TypeError", sloppy: "ok:false" },
  { name: "setPrototypeOf", body: "Object.setPrototypeOf(t, { [k]: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
];

describe("no write to a proof class, its prototype or the package's tables changes a verdict", () => {
  it("InclusionProof, TrieProof and the classes and tables their checks read, in strict and sloppy code", () => {
    const probe = (): readonly unknown[] => [
      merkle.verifyInclusion(sha256, merkleRoot, leafA, inclusion),
      merkle.verifyInclusion(sha256, merkleRoot, leafB, inclusion),
      trie.verify(hashing, mode, trieRoot, trieProof).ok,
      trie.verify(hashing, mode, new Uint8Array(32), trieProof),
      expectOk(trie.check(hashing, mode, trieRoot, trieProof), "verified").lookup(Uint8Array.of(1)),
      expectOk(trie.check(hashing, mode, trieRoot, trieProof), "verified").lookup(Uint8Array.of(2)),
      poseidon.verifyInclusion(leaf, poseidonProof, poseidonRoot),
      poseidon.verifyInclusion(poseidon.Fr.zero, poseidonProof, poseidonRoot),
      expectOk(poseidon.hash([leaf]), "a hash").value,
      vt.checkValue(expectOk(versioned.StateKey.parse("a/b"), "a key"), Value.null),
    ];
    const before = probe();
    expect(before.slice(0, 4), "the verdicts").toEqual([{ ok: true, value: true }, { ok: true, value: false }, true, { ok: true, value: null }]);
    const lie = (): unknown => ({ ok: true, value: true });
    const targets: readonly (readonly [string, object, PropertyKey, unknown])[] = [
      ["InclusionProof.prototype.path", merkle.InclusionProof.prototype, "path", []],
      ["InclusionProof.prototype.encode", merkle.InclusionProof.prototype, "encode", lie],
      ["InclusionProof.of", merkle.InclusionProof, "of", lie],
      ["an inclusion proof's index", inclusion, "index", 1n],
      ["Rfc6962MerkleHashing.prototype.node", merkle.Rfc6962MerkleHashing.prototype, "node", lie],
      ["a profile's digest size", sha256, "size", 0],
      ["TrieProof.prototype.isEmpty", trie.TrieProof.prototype, "isEmpty", true],
      ["TrieProof.fromRecords", trie.TrieProof, "fromRecords", lie],
      ["TrieHashing.prototype.valueHash", trie.TrieHashing.prototype, "valueHash", lie],
      ["KeyMode.prototype.checkKey", trie.KeyMode.prototype, "checkKey", lie],
      ["VerifiedTrieProof.prototype.lookup", trie.VerifiedTrieProof.prototype, "lookup", lie],
      ["the trie's operations table: check", TRIE, "check", lie],
      ["the trie's operations table: lookup", TRIE, "lookup", lie],
      ["Fr.prototype.value", poseidon.Fr.prototype, "value", 0n],
      ["a Poseidon table's round constants", WIDTH3.constants, 0, 0n],
      ["a Poseidon table's matrix", WIDTH3.mds, 0, [0n, 0n, 0n]],
      ["a Poseidon table", WIDTH3, "partialRounds", 0],
      ["the version keys", versioned.VersionKey, "long", {}],
      ["StateKey.parse", versioned.StateKey, "parse", lie],
    ];
    for (const m of ["strict", "sloppy"] as const) {
      for (const [what, t, k, x] of targets) {
        expect(Object.getOwnPropertyDescriptor(t, k), `${what} is its own`).toBeDefined();
        for (const w of writes) expect(attempt(m, w.body, t, k, x), `${w.name} on ${what}, ${m}`).toBe(w[m]);
      }
    }
    expect(probe(), "what the proofs answer").toEqual(before);
  });
});

describe("no shipped module gives a caller what builds a genuine instance", () => {
  it("no symbol any module exports is a constructor's token: what a constructor builds with one is refused", async () => {
    const base = new URL("../src/", import.meta.url);
    const files = readdirSync(base, { recursive: true, encoding: "utf8" }).filter((f) => f.endsWith(".ts"));
    const modules = await Promise.all(files.map(async (f) => (await import(/* @vite-ignore */ new URL(f, base).href)) as Record<string, unknown>));
    const symbols = modules.flatMap((m) => Object.values(m)).filter((x): x is symbol => typeof x === "symbol");
    const node = { kind: "leaf", segment: { bytes: Uint8Array.of(1), length: 8 }, value: valueHash };
    for (const token of [...symbols, Symbol("dilf4s-proofs")]) {
      const forged = Reflect.construct(trie.TrieProof, [token, node]) as trie.TrieProof;
      expect(trie.check(hashing, mode, trieRoot, forged), `a TrieProof built with ${String(token)}`).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
      const path = [leafB];
      const inclusionForged = Reflect.construct(merkle.InclusionProof, [token, 0n, 2n, path]) as merkle.InclusionProof;
      expect(merkle.verifyInclusion(sha256, merkleRoot, leafA, inclusionForged), `an InclusionProof built with ${String(token)}`).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
    }
  });

  it("the trie's table builds a proof from copies of a caller's records: changing them afterwards changes no lookup", () => {
    const segment = Uint8Array.of(1);
    const v = Uint8Array.from(valueHash);
    const built = TRIE.fromRecords(hashing, [{ s: { n: 8n, b: segment }, v }]);
    expect(built.ok, "a proof").toBe(true);
    if (!built.ok) return;
    const verified = expectOk(trie.check(hashing, mode, trieRoot, built.value), "verified");
    segment[0] = 2;
    v.fill(0);
    expect(verified.lookup(Uint8Array.of(1)), "the key it verified").toEqual({ ok: true, value: { kind: "present", valueHash } });
    expect(verified.lookup(Uint8Array.of(2)), "the key the caller wrote").toEqual({ ok: true, value: { kind: "absent" } });
  });

  it("an Oversized built from outside is none: a version of it is refused, never thrown on or kept as bytes", () => {
    const fromOutside = (length: unknown): unknown => Reflect.construct(Oversized, [Symbol("dilf4s-proofs: a measured length"), length]);
    const bytesKey = expectOk(versioned.VersionKey.bytes(4), "a byte version key");
    for (const length of [Uint8Array.of(0, 0, 0, 7), null, 4]) {
      const forged = fromOutside(length);
      expect(Oversized.is(forged), "no Oversized").toBe(false);
      expect(Reflect.get(forged as object, "length"), "no length of the caller's").toBe(0);
      expect(bytesKey.encode(bad(forged)), "the key refuses it").toEqual({ ok: false, error: { kind: "TypeMismatch", reason: "expected a Uint8Array" } });
    }
  });
});

describe("a refusal the package gives more than one caller cannot change what the next is given", () => {
  const probes: readonly (readonly [string, () => { readonly ok: boolean; readonly error?: unknown }])[] = [
    ["not a profile", () => merkle.verifyInclusion(bad(null), merkleRoot, leafA, inclusion)],
    ["not an inclusion proof", () => merkle.verifyInclusion(sha256, merkleRoot, leafA, bad({}))],
    ["not bytes", () => merkle.verifyInclusion(sha256, bad("root"), leafA, inclusion)],
    ["not a trie hashing", () => trie.verify(bad(1), mode, trieRoot, trieProof)],
    ["not a key mode", () => trie.verify(hashing, bad(1), trieRoot, trieProof)],
    ["not a trie proof", () => trie.verify(hashing, mode, trieRoot, bad([]))],
    ["not a field element", () => poseidon.compress(bad(1n), leaf)],
    ["not a bigint", () => poseidon.Fr.of(bad(1))],
    ["not a Poseidon proof", () => poseidon.verifyInclusion(leaf, bad(null), poseidonRoot)],
    ["not a versioned trie", () => versioned.VersionedTrie.prototype.checkValue.call(bad({}), expectOk(versioned.StateKey.parse("a"), "a key"), Value.null)],
    ["not a string", () => versioned.StateKey.parse(bad(1))],
  ];

  for (const [what, probe] of probes) {
    it(what, () => {
      const first = probe();
      const again = probe();
      expect(first.ok, "a refusal").toBe(false);
      const shared = !again.ok && again.error === first.error;
      expect(!shared || Object.isFrozen(first.error), "an error given to every caller is frozen").toBe(true);
      expect(again !== first || Object.isFrozen(first), "a result given to every caller is frozen").toBe(true);
      const before = structuredClone(first.error);
      for (const m of ["strict", "sloppy"] as const) {
        for (const w of writes) {
          attempt(m, w.body, first.error as object, "kind", "Poisoned");
          attempt(m, w.body, first, "ok", true);
        }
        attempt(m, "(t.reason = x, 'done')", first.error as object, "reason", "poisoned");
      }
      expect(again.error, "what the next caller is given").toEqual(before);
      expect(probe()).toEqual({ ok: false, error: before });
    });
  }

  it("a lookup's shared answers (absent, not covered) are frozen", () => {
    const verified = expectOk(trie.check(hashing, mode, trieRoot, trieProof), "verified");
    const absent = expectOk(verified.lookup(Uint8Array.of(2)), "an absent key");
    expect(absent, "absent").toEqual({ kind: "absent" });
    expect(Object.isFrozen(absent), "the shared absent answer").toBe(true);
  });
});

describe("a populated replica and a finished snapshot reader hold state no caller can reach or change", { timeout: 60_000 }, () => {
  /**
   * A replica of `n` keys at `versions[0]`, then one key changed at `versions[1]` (so the two share every other subtree),
   * and a reader that has read the second's snapshot.
   */
  function populated<V>(profile: versioned.VersionedTrie<V>, versions: readonly [V, V], n: number) {
    const keys = Array.from({ length: n }, (_, i) => expectOk(versioned.StateKey.parse(`k/${i.toString().padStart(3, "0")}`), "a key"));
    const int = (i: number): Value => expectOk(Value.int(BigInt(i)), "a value");
    const states = [keys.map((k, i) => [k, int(i)] as const), keys.map((k, i) => [k, int(i === 0 ? -1 : i)] as const)] as const;
    const roots = states.map((s) => expectOk(trie.trieRoot(profile.hashing, profile.stateMode, s.map(([k, v]) => [k.bytes(), expectOk(profile.valueHash(v), "a hash")] as const)), "state root"));
    const history = versions.map((u, i) => [u, roots[i] ?? new Uint8Array(0)] as const);
    const heads = versions.map((version, i) => ({
      version,
      stateRoot: roots[i] ?? new Uint8Array(0),
      historyRoot: expectOk(trie.trieRoot(profile.hashing, profile.historyMode, history.slice(0, i + 1).map(([u, s]) => [expectOk(profile.versionKey.encode(u), "a key"), expectOk(profile.bytesHash(s), "a hash")] as const)), "history root"),
    }));
    const [first, second] = [heads[0], heads[1]];
    if (first === undefined || second === undefined) throw new Error("two heads");
    const older = expectOk(expectOk(versioned.Replica.empty(profile), "empty").applyDelta({ head: first, upserts: states[0], removes: [] }), "a populated replica");
    const replica = expectOk(older.applyDelta({ parent: first, head: second, upserts: [states[1][0] ?? [keys[0] ?? expectOk(versioned.StateKey.parse("k"), "k"), int(0)]], removes: [] }), "the next");
    const chunks: readonly versioned.SnapshotChunk<V>[] = [
      { kind: "head", head: second },
      { kind: "state", entries: states[1] },
      { kind: "history", versions: history },
    ];
    const reader = chunks.reduce((r, c) => expectOk(r.feed(c), "a chunk"), expectOk(versioned.SnapshotReader.start(profile, second), "a reader"));
    return { keys, replica, reader };
  }

  const bytesKey = expectOk(versioned.VersionKey.bytes(4), "a byte version key");
  const byBytes = expectOk(versioned.VersionedTrie.of("SHA-256", bytesKey), "a byte-versioned trie");
  /** The checks, for one profile's replica and reader. */
  function checks<V>(what: string, { keys, replica, reader }: ReturnType<typeof populated<V>>): void {
    it(`${what}: every object reachable from them is frozen`, () => {
      // a finished snapshot is the one caller's own copy (its arrays and bytes fresh each time), as a result is
      const reached = reachable([replica, reader]);
      expect(reached.filter((o) => !Object.isFrozen(o)).map((o) => Object.prototype.toString.call(o))).toEqual([]);
    });

    it(`${what}: changing what they hand out changes nothing they answer`, () => {
      const probe = (): unknown => [replica.head, replica.root(), keys.map((k) => replica.get(k)), replica.history(), replica.entries(), reader.trusted, reader.finish()];
      const before = structuredClone(probe());
      const flip = (b: unknown): void => {
        if (b instanceof Uint8Array && b.length > 0) b[0] = (b[0] ?? 0) ^ 0xff;
      };
      const head = replica.head;
      flip(head?.stateRoot);
      flip(head?.version);
      const [u, s] = replica.history()[0] ?? [];
      flip(u);
      flip(s);
      flip(reader.trusted.stateRoot);
      flip(reader.trusted.version);
      const snapshot = expectOk(reader.finish(), "a snapshot");
      flip(snapshot.head.historyRoot);
      flip(snapshot.history[0]?.[1]);
      flip(snapshot.history[0]?.[0]);
      expect(Object.isFrozen(snapshot.entries[0]), "a snapshot's entry").toBe(true);
      expect(Reflect.set(snapshot.entries[0] ?? [], 1, Value.null), "an entry's value").toBe(false);
      expect(structuredClone(probe())).toEqual(before);
    });
  }

  it("a byte version a caller passed in, changed afterwards, changes nothing kept, whatever its realm or accessors", () => {
    const foreign = runInNewContext("new Uint8Array([0, 0, 0, 7])") as Uint8Array;
    const odd = Uint8Array.of(0, 0, 0, 8);
    Object.defineProperty(odd, Symbol.iterator, { get: () => 123 });
    const { keys, replica, reader } = populated(byBytes, [foreign, odd], 4);
    const probe = (): unknown => [replica.head, replica.root(), replica.history(), reader.trusted, reader.finish(), keys.map((k) => replica.get(k))];
    const before = structuredClone(probe());
    foreign[3] = 0x77;
    odd[3] = 0x78;
    const head = replica.head;
    if (head === null) throw new Error("a head");
    head.version[3] = 0x79;
    expect(structuredClone(probe()), "the replica and the reader").toEqual(before);
    const restored = versioned.Replica.fromSnapshot(byBytes, replica.head ?? head, { head: replica.head ?? head, entries: replica.entries(), history: replica.history() });
    expect(restored.ok, "the replica restores from what it hands out").toBe(true);
  });

  it("a byte version longer than any version key's is never copied: it is refused by its length", () => {
    const huge = new Uint8Array(16 << 20);
    const head = { version: huge, stateRoot: new Uint8Array(32), historyRoot: new Uint8Array(32) };
    const before = process.memoryUsage().arrayBuffers;
    const reader = versioned.SnapshotReader.start(byBytes, head);
    const delta = expectOk(versioned.Replica.empty(byBytes), "empty").applyDelta({ head, upserts: [], removes: [] });
    expect(process.memoryUsage().arrayBuffers - before, "no copy of the 16 MiB version").toBeLessThan(1 << 20);
    expect(reader.ok, "the reader starts: the version is encoded when the history is read").toBe(true);
    expect(delta, "the delta").toMatchObject({ ok: false, error: { kind: "InvalidVersion" } });
  });

  it("a byte version in a resizable buffer, resized while its head is read or afterwards, is judged and kept as first measured", () => {
    const Resizable = ArrayBuffer as unknown as new (length: number, options: { maxByteLength: number }) => ArrayBuffer & { resize(length: number): void };
    const roots = { stateRoot: new Uint8Array(32), historyRoot: new Uint8Array(32) };
    const shrinking = (): { readonly head: object; readonly version: Uint8Array } => {
      const buffer = new Resizable(1025, { maxByteLength: 2048 });
      const version = new Uint8Array(buffer);
      const head = {
        version,
        get stateRoot(): Uint8Array {
          buffer.resize(4);
          version.set([0, 0, 0, 7]);
          return roots.stateRoot;
        },
        historyRoot: roots.historyRoot,
      };
      return { head, version };
    };
    const delta = expectOk(versioned.Replica.empty(byBytes), "empty").applyDelta({ head: bad(shrinking().head), upserts: [], removes: [] });
    expect(delta, "refused by the length first measured").toEqual({ ok: false, error: { kind: "InvalidVersion", reason: "a version key is 4 bytes, got 1025" } });
    const { head, version } = shrinking();
    const reader = expectOk(versioned.SnapshotReader.start(byBytes, bad(head)), "a reader");
    const kept = reader.trusted.version;
    expect(kept, "never the caller's array").not.toBe(version);
    version.set([0, 0, 0, 8]);
    expect(reader.trusted.version, "what the reader keeps").toEqual(kept);
    expect(reader.feed({ kind: "head", head: bad(head) }).ok, "the version, measured at 1025 bytes, is no 4-byte key").toBe(false);
  });

  checks("a bigint-versioned trie", populated(vt, [7n, 8n], 40));
  checks("a byte-versioned trie", populated(byBytes, [Uint8Array.of(0, 0, 0, 7), Uint8Array.of(0, 0, 0, 8)], 40));
});
