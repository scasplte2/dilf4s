import { describe, expect, it } from "vitest";

import { cbor, models, toHex, Value } from "@kd5ujc/dilf4s-core";

import { versioned } from "../src/index.js";
import { bytes, expectErr, expectOk, loadVectors, valueOf } from "./vectors.js";

const vectors = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors);
const versionKey = expectOk(versioned.VersionKey.unsignedLong(Number(vectors.width)), "version key");
const vt = expectOk(versioned.VersionedTrie.of("SHA-256", versionKey), "profile");
const root = bytes(vectors.root);
const key = (s: string): versioned.StateKey => expectOk(versioned.StateKey.parse(s), `state key ${s}`);
const ns = (s: string): versioned.StateNamespace => expectOk(versioned.StateNamespace.parse(s), `namespace ${s}`);
const proofOf = (hex: string): versioned.VersionedTrieProof<bigint> => expectOk(vt.decodeProof(valueOf(hex)), "proof decodes");
const finalHead = expectOk(vt.decodeHead(valueOf(vectors.head.cbor)), "head decodes");

const entriesHex = (es: readonly versioned.StateEntry[]): unknown => es.map(([k, h]) => [k.value, toHex(h)]);

describe("versioned_trie_v1.json", () => {
  it("the empty state root", () => {
    expect(toHex(vt.hashing.empty)).toBe(vectors.emptyStateRoot);
  });

  it("the head KAT: the head's wire form and its top-trie root", () => {
    const h = vectors.head;
    expect(finalHead.version).toBe(h.version);
    expect(toHex(expectOk(versionKey.encode(h.version), "vkey"))).toBe(h.vkey);
    expect([toHex(finalHead.stateRoot), toHex(finalHead.historyRoot)]).toEqual([h.stateRoot, h.historyRoot]);
    expect(toHex(cbor.encode(expectOk(vt.encodeHead(finalHead), "encodes")))).toBe(h.cbor);
    expect(toHex(expectOk(vt.root(finalHead), "root"))).toBe(h.root);
    expect(h.root).toBe(vectors.root);
  });

  it("every commit's delta replays through a replica to its roots", () => {
    let replica = versioned.Replica.empty(vt);
    for (const c of vectors.commits) {
      const delta = expectOk(vt.decodeDelta(valueOf(c.delta)), `delta ${c.version.toString()}`);
      expect(delta.upserts.map(([k, v]) => [k.value, toHex(cbor.encode(v))])).toEqual(c.upserts.map((u) => [u.key, u.value]));
      expect(delta.removes.map((k) => k.value)).toEqual(c.removes);
      replica = expectOk(replica.applyDelta(delta), `applies ${c.version.toString()}`);
      expect(replica.head?.version).toBe(c.version);
      expect(toHex(replica.head?.stateRoot ?? new Uint8Array(0))).toBe(c.stateRoot);
      expect(toHex(replica.head?.historyRoot ?? new Uint8Array(0))).toBe(c.historyRoot);
      expect(toHex(expectOk(replica.root(), "root") ?? new Uint8Array(0))).toBe(c.root);
    }
    expect(toHex(expectOk(replica.root(), "root") ?? new Uint8Array(0))).toBe(vectors.root);
  });

  it("the snapshot restores, chunk by chunk, to the root", () => {
    const chunks = vectors.snapshot.chunks.map(bytes);
    for (const c of chunks) {
      const decoded = expectOk(vt.decodeChunk(c), "chunk decodes");
      if (decoded.kind !== "head") expect(decoded.kind === "state" ? decoded.entries.length : decoded.versions.length).toBeLessThanOrEqual(Number(vectors.snapshot.chunkEntries));
    }
    const replica = expectOk(versioned.Replica.restore(vt, finalHead, chunks), "restores");
    expect(toHex(expectOk(replica.root(), "root") ?? new Uint8Array(0))).toBe(vectors.root);
    expect(replica.history().map(([v]) => v)).toEqual(vectors.commits.map((c) => c.version));
    expect(replica.get(key("oracle/a/b"))).toEqual(expectOk(Value.text("a"), "text"));
  });

  it.each(vectors.stateKeys.map((c) => [JSON.stringify(c.key).slice(0, 40), c] as const))("state key %s", (_, c) => {
    expect(versioned.StateKey.parse(c.key).ok, "the grammar with its limits").toBe(c.valid);
    const text = Value.text(c.key);
    if (text.ok) expect(models.versionedTrie.StateKey.decode(text.value).ok, "the schema pattern").toBe(c.schema);
  });

  describe.each(vectors.proofs.map((p) => [p.name, p] as const))("proof %s", (_, p) => {
    const proof = proofOf(p.cbor);
    const at = p.at;

    it("the head binds to the root", () => {
      expect(expectOk(vt.sameHead(proof.head, finalHead), "compares")).toBe(true);
    });

    switch (p.kind) {
      case "state":
      case "historical":
        it("answers its claims and listings", () => {
          const state = expectOk(p.kind === "state" ? vt.verifyState(root, proof) : vt.verifyHistoricalKey(root, proof, at ?? -1n), "verifies");
          for (const c of p.claims ?? []) {
            const claimed = c.value === undefined ? null : valueOf(c.value);
            expect(expectOk(state.holds(key(c.key), claimed), "holds"), c.key).toBe(true);
            const looked = expectOk(state.lookup(key(c.key)), "lookup");
            expect(looked === null ? null : toHex(looked)).toBe(claimed === null ? null : toHex(vt.valueHash(claimed)));
            if (claimed !== null) expect(expectOk(state.holds(key(c.key), Value.int(424242n)), "holds"), "another value").toBe(false);
          }
          for (const l of p.listings ?? []) {
            expect(entriesHex(expectOk(state.namespace(ns(l.namespace)), "listing")), l.namespace).toEqual(l.entries.map((e) => [e.key, toHex(vt.valueHash(valueOf(e.value)))]));
          }
        });
        break;
      case "version":
        it("answers whether the version was committed", () => {
          const r = expectOk(vt.verifyVersion(root, proof, at ?? -1n), "verifies");
          expect(r === null ? undefined : toHex(r)).toBe(p.answer?.stateRoot);
          expect(expectErr(vt.verifyVersion(root, proof, (at ?? 0n) + 1n), "another version").kind).toBe("Malformed");
        });
        break;
      case "asOf":
        it("answers the latest version at or before its time", () => {
          const r = expectOk(vt.verifyAsOf(root, proof, at ?? -1n), "verifies");
          expect(r === null ? {} : { version: r.version, stateRoot: toHex(r.stateRoot) }).toEqual(p.answer);
        });
        break;
      case "head":
        it("proves the head invariant", () => {
          const h = expectOk(vt.verifyHead(root, proof), "verifies");
          expect(expectOk(vt.sameHead(h, finalHead), "compares")).toBe(true);
        });
        break;
      case "versions":
        it("lists exactly the committed versions in its range", () => {
          const claimed = (p.versions ?? []).map((v) => [v.version ?? -1n, bytes(v.stateRoot ?? "")] as const);
          expect(expectOk(vt.verifyVersions(root, proof, at ?? -1n, p.until ?? null, claimed), "verifies")).toBe(true);
          expect(expectOk(vt.verifyVersions(root, proof, at ?? -1n, p.until ?? null, claimed.slice(1)), "verdict"), "a version left out").toBe(false);
          const forged = claimed.map(([v, s], i) => [v, i === 0 ? vt.hashing.empty : s] as const);
          expect(expectOk(vt.verifyVersions(root, proof, at ?? -1n, p.until ?? null, forged), "verdict"), "another state root").toBe(false);
        });
        break;
    }

    it("against another root, the binding fails", () => {
      expect(expectErr(vt.verifyHead(vt.hashing.empty, proof), "mismatch")).toMatchObject({ kind: "RootMismatch", component: "root" });
    });
  });

  it("the proof kinds the reference emits are all covered", () => {
    expect(new Set(vectors.proofs.map((p) => p.kind))).toEqual(new Set(["state", "historical", "version", "asOf", "head", "versions"]));
  });
});

describe("versioned trie checks", () => {
  const byName = (name: string): versioned.VersionedTrieProof<bigint> => proofOf(vectors.proofs.find((p) => p.name === name)?.cbor ?? "");

  it("a proof checked as the wrong kind is refused, never answered", () => {
    expect(expectErr(vt.verifyState(root, byName("key-at-version-1")), "historical as current").kind).toBe("Malformed");
    expect(expectErr(vt.verifyHead(root, byName("current-keys")), "no history").kind).toBe("Malformed");
    expect(expectErr(vt.verifyVersion(root, byName("current-keys"), 1n), "no at").kind).toBe("Malformed");
    expect(expectErr(vt.verifyHistoricalKey(root, byName("version-3-never-committed"), 3n), "never committed").kind).toBe("UnknownVersion");
  });

  it("a head that does not hash to the root, and a top trie of another head", () => {
    const p = byName("head");
    const forgedHead = { ...p.head, stateRoot: vt.hashing.empty };
    expect(expectErr(vt.verifyHead(root, { ...p, head: forgedHead }), "forged head")).toMatchObject({ kind: "RootMismatch", component: "root" });
    const otherRoot = expectOk(vt.root(forgedHead), "root");
    const e = expectErr(vt.verifyHead(otherRoot, { ...p, head: forgedHead }), "top of another head");
    expect(e.kind).toBe("Trie");
  });

  it("a state query the proof does not reach is NotCovered", () => {
    const state = expectOk(vt.verifyState(root, byName("current-keys")), "verifies");
    expect(expectErr(state.range(key("a"), undefined), "not covered").kind).toBe("NotCovered");
  });

  it("as-of answers must not be after the time", () => {
    const p = byName("as-of-299");
    expect(expectErr(vt.verifyAsOf(root, p, 255n), "after").kind).toBe("Malformed");
  });

  it("decoding: malformed shapes, digests and keys", () => {
    expect(expectErr(vt.decodeProof(Value.null), "not a map").kind).toBe("Malformed");
    expect(expectErr(vt.decodeHead(expectOk(models.versionedTrie.VersionedTrieHead.encode({ v: new Uint8Array(7), s: new Uint8Array(32), h: new Uint8Array(32) }), "head")), "short vkey").kind).toBe("InvalidVersion");
    expect(expectErr(vt.decodeHead(expectOk(models.versionedTrie.VersionedTrieHead.encode({ v: new Uint8Array(8), s: new Uint8Array(31), h: new Uint8Array(32) }), "head")), "short root").kind).toBe("Malformed");
    expect(expectErr(vt.decodeHead(expectOk(models.versionedTrie.VersionedTrieHead.encode({ v: Uint8Array.of(0x80, 0, 0, 0, 0, 0, 0, 0), s: new Uint8Array(32), h: new Uint8Array(32) }), "head")), "2^63").kind).toBe("InvalidVersion");
    const delta = models.versionedTrie.VersionedTrieDelta.encode({ head: { v: new Uint8Array(8), s: new Uint8Array(32), h: new Uint8Array(32) }, upserts: [], removes: ["a".repeat(65)] });
    expect(expectErr(vt.decodeDelta(expectOk(delta, "delta")), "long segment").kind).toBe("Key");
    expect(expectErr(vt.decodeChunk(new Uint8Array(versioned.MAX_CHUNK_BYTES + 1)), "too large").kind).toBe("TooLarge");
    expect(expectErr(vt.decodeChunk(Uint8Array.of(0xff)), "not CBOR").kind).toBe("Malformed");
  });

  it("state keys and namespaces", () => {
    expect(expectErr(versioned.StateKey.parse("a/"), "trailing slash").reason).toMatch(/segment ''/);
    expect(versioned.StateKey.fromBytes(Uint8Array.of(0xc3, 0xa9)).ok).toBe(false);
    expect(versioned.StateNamespace.parse(Array.from({ length: 16 }, () => "a").join("/")).ok).toBe(false);
    expect(versioned.StateNamespace.parse(Array.from({ length: 15 }, () => "a").join("/")).ok).toBe(true);
    expect(versioned.StateNamespace.parse("x".repeat(64) + "/" + "x".repeat(64) + "/" + "x".repeat(64) + "/" + "x".repeat(61)).ok).toBe(false);
    expect(ns("fiber").contains(key("fiber/1"))).toBe(true);
    expect(ns("fiber").contains(key("fiberx/1"))).toBe(false);
  });

  it("version keys", () => {
    expect(toHex(expectOk(versioned.VersionKey.long.encode(300n), "encodes"))).toBe("000000000000012c");
    expect(expectErr(versioned.VersionKey.long.encode(2n ** 63n), "too big").kind).toBe("InvalidVersion");
    expect(expectErr(versioned.VersionKey.long.encode(-1n), "negative").kind).toBe("InvalidVersion");
    const w2 = expectOk(versioned.VersionKey.unsignedLong(2), "width 2");
    expect(toHex(expectOk(w2.encode(65535n), "encodes"))).toBe("ffff");
    expect(expectErr(w2.encode(65536n), "too big").kind).toBe("InvalidVersion");
    const u8 = expectOk(versioned.VersionKey.unsigned(8), "unsigned 8");
    expect(expectOk(u8.decode(bytes("ffffffffffffffff")), "decodes")).toBe(2n ** 64n - 1n);
    expect(versioned.VersionKey.unsignedLong(9).ok).toBe(false);
    expect(versioned.VersionKey.unsigned(1025).ok).toBe(false);
  });
});

describe("snapshots and replicas", () => {
  const chunks = vectors.snapshot.chunks.map(bytes);
  const feedAll = (cs: readonly Uint8Array[]): versioned.VersionedTrieError | undefined => {
    let reader = versioned.SnapshotReader.start(vt, finalHead);
    for (const c of cs) {
      const r = reader.feedBytes(c);
      if (!r.ok) return r.error;
      reader = r.value;
    }
    const f = reader.finish();
    return f.ok ? undefined : f.error;
  };
  const reason = (e: versioned.VersionedTrieError | undefined): string => (e?.kind === "Malformed" ? e.reason : JSON.stringify(e?.kind));

  it("the reader refuses reordered, truncated, foreign and doubled snapshots at the first bad chunk", () => {
    expect(feedAll(chunks)).toBeUndefined();
    const [head, ...rest] = chunks;
    if (head === undefined) throw new Error("a head chunk");
    expect(reason(feedAll(rest))).toMatch(/starts with its head/);
    expect(reason(feedAll([head, head]))).toMatch(/one head/);
    expect(reason(feedAll(chunks.slice(0, -1)))).toMatch(/ends before the head version/);
    expect(reason(feedAll([head, ...[...rest].reverse()]))).toMatch(/precede|does not follow|after|nothing follows/);
    expect(reason(feedAll([...chunks, chunks[chunks.length - 1] ?? head]))).toMatch(/nothing follows/);
    const stateChunks = rest.filter((c) => expectOk(vt.decodeChunk(c), "decodes").kind === "state");
    if (stateChunks.length > 1) expect(reason(feedAll([head, stateChunks[1] ?? head, stateChunks[0] ?? head]))).toMatch(/does not follow/);
    const other = { ...finalHead, stateRoot: vt.hashing.empty };
    const foreign = versioned.SnapshotReader.start(vt, other).feedBytes(head);
    expect(reason(foreign.ok ? undefined : foreign.error)).toMatch(/not the trusted head/);
  });

  it("restoring checks the roots against the trusted head", () => {
    const snapshot = { head: finalHead, entries: [] as const, history: [[finalHead.version, finalHead.stateRoot] as const] };
    expect(expectErr(versioned.Replica.fromSnapshot(vt, finalHead, snapshot), "dropped entries")).toMatchObject({ kind: "RootMismatch", component: "state" });
  });

  it("a replica owns its roots: mutating what goes in or comes out leaves it intact", () => {
    const [d1, d2] = vectors.commits.map((c) => expectOk(vt.decodeDelta(valueOf(c.delta)), "delta"));
    if (d1 === undefined || d2 === undefined) throw new Error("two deltas");
    const one = expectOk(versioned.Replica.empty(vt).applyDelta(d1), "first");
    const root = expectOk(one.root(), "root");
    // the caller's delta head, the head getter and the history listing are all the caller's to scribble on
    d1.head.stateRoot.fill(0);
    d1.head.historyRoot.fill(0);
    one.head?.stateRoot.fill(0);
    for (const [, stateRoot] of one.history()) stateRoot.fill(0);
    expect(expectOk(one.root(), "root after")).toEqual(root);
    expectOk(one.applyDelta(d2), "the next delta still applies");

    const restored = expectOk(versioned.Replica.restore(vt, finalHead, vectors.snapshot.chunks.map(bytes)), "restores");
    for (const [, stateRoot] of restored.history()) stateRoot.fill(0);
    expect(restored.history().every(([, r]) => r.some((b) => b !== 0))).toBe(true);
  });

  it("a replica refuses deltas that do not chain, go backwards, or claim another state", () => {
    const deltas = vectors.commits.map((c) => expectOk(vt.decodeDelta(valueOf(c.delta)), "delta"));
    const [d1, d2] = deltas;
    if (d1 === undefined || d2 === undefined) throw new Error("two deltas");
    const empty = versioned.Replica.empty(vt);
    expect(expectErr(empty.applyDelta(d2), "gap")).toMatchObject({ kind: "RootMismatch", component: "parent" });
    const one = expectOk(empty.applyDelta(d1), "first");
    expect(expectErr(one.applyDelta(d1), "replay")).toMatchObject({ kind: "RootMismatch", component: "parent" });
    expect(expectErr(one.applyDelta({ ...d2, head: { ...d2.head, version: 1n } }), "not increasing").kind).toBe("VersionNotIncreasing");
    expect(expectErr(one.applyDelta({ ...d2, upserts: d2.upserts.slice(1) }), "dropped write")).toMatchObject({ kind: "RootMismatch", component: "state" });
    expect(expectErr(one.applyDelta({ ...d2, head: { ...d2.head, historyRoot: vt.hashing.empty } }), "forged history")).toMatchObject({ kind: "RootMismatch", component: "history" });
    const k = key("fiber/1");
    expect(expectErr(one.applyDelta({ ...d2, upserts: [[k, Value.int(1n)]], removes: [k] }), "both").kind).toBe("Malformed");
    expect(expectErr(one.applyDelta({ ...d2, upserts: [[k, Value.int(1n)], [k, Value.int(2n)]] }), "twice").kind).toBe("Malformed");
    expect(expectErr(one.applyDelta({ ...d2, upserts: [[k, Value.null]] }), "null").kind).toBe("Malformed");
    let deep: Value = Value.int(0n);
    for (let i = 0; i < 254; i++) deep = expectOk(Value.array([deep]), "array");
    expect(expectErr(one.applyDelta({ ...d2, upserts: [[k, deep]] }), "too deep").kind).toBe("TooDeep");
    const big = Value.bytes(new Uint8Array(versioned.MAX_VALUE_BYTES));
    expect(expectErr(one.applyDelta({ ...d2, upserts: [[k, big]] }), "too large").kind).toBe("TooLarge");
  });
});
