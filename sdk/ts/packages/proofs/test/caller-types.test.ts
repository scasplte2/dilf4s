import { once } from "node:events";
import { runInNewContext } from "node:vm";
import { Worker } from "node:worker_threads";

import { describe, expect, it } from "vitest";

import { models, Value } from "@kd5ujc/dilf4s-core";

import { merkle, poseidon, trie, versioned } from "../src/index.js";
import { bytes, expectErr, expectOk, int, loadVectors, valueOf } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;
const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });
const notBytes = mismatch("expected a Uint8Array");
const notAValue = mismatch("expected a canonical value");
/** A test label for anything a caller may pass: an object is not read, so a Proxy's traps do not run for it. */
const label = (x: unknown): string => (typeof x === "object" && x !== null ? "an object" : `${typeof x} ${String(x)}`);

/** A Uint8Array subclass whose `length` says `claimed` whatever it holds. */
function liar(bytes: readonly number[], claimed: number): Uint8Array {
  class Liar extends Uint8Array {
    override get length(): number {
      return claimed;
    }
  }
  return new Liar(bytes);
}

/** A Proxy of `target` whose every trap is recorded. */
function trapped<T extends object>(target: T): { readonly proxy: T; readonly traps: string[] } {
  const traps: string[] = [];
  const handler: ProxyHandler<T> = {};
  for (const name of ["get", "has", "ownKeys", "getOwnPropertyDescriptor", "getPrototypeOf", "apply", "construct", "set", "defineProperty"] as const) {
    Reflect.set(handler, name, (...args: unknown[]) => {
      traps.push(name);
      return Reflect.apply(Reflect.get(Reflect, name) as (...a: unknown[]) => unknown, undefined, args);
    });
  }
  return { proxy: new Proxy(target, handler), traps };
}

/** A view of a SharedArrayBuffer holding `bytes`. */
function shared(bytes: readonly number[]): Uint8Array {
  const view = new Uint8Array(new SharedArrayBuffer(bytes.length));
  view.set(bytes);
  return view;
}

const empty = new Uint8Array(0);
const foreign = (bytes: readonly number[]): Uint8Array => runInNewContext(`Uint8Array.of(${bytes.join(",")})`) as Uint8Array;

/** Values that are no bytes at all, each fresh: null, undefined, a number, objects posing as bytes. */
const notBytesCases = (): unknown[] => [null, undefined, 5, "ab", { length: 2, 0: 0x61, 1: 0x62 }, { [Symbol.toStringTag]: "Uint8Array", length: 2 }, [0x61, 0x62], new Uint16Array(2)];
/** Values that are no canonical value: null, undefined, a number, objects posing as one (a null value included). */
const notValueCases = (): unknown[] => [undefined, 5, "a", { kind: "null" }, { kind: "int", value: 1n }, Object.create(Object.getPrototypeOf(int(1n)) as object)];

// ------------------------------------------------------------------ a small trie: "a" -> 1, "ab" -> 2, variable(8)

const T = expectOk(trie.TrieHashing.of("SHA-256"), "hashing");
const variable = expectOk(trie.KeyMode.variable(8), "mode");
const v1 = expectOk(T.valueHash(int(1n)), "v1");
const v2 = expectOk(T.valueHash(int(2n)), "v2");
const full: Value = expectOk(
  models.trie.TrieProof.encode([
    { s: { n: 8n, b: Uint8Array.of(0x61) }, v: v1, c: "left" },
    { s: { n: 7n, b: Uint8Array.of(0xc4) }, v: v2 },
  ]),
  "records",
);
const root = expectOk(trie.trieRoot(T, variable, [[Uint8Array.of(0x61), v1], [Uint8Array.of(0x61, 0x62), v2]]), "root");
const proof = expectOk(trie.TrieProof.decode(T, full), "proof");
const verified = (): trie.VerifiedTrieProof => {
  const v = expectOk(trie.verify(T, variable, root, proof), "verifies");
  if (v === null) throw new Error("root mismatch");
  return v;
};

describe("a trie key is read by its internal length, for the check and the walk alike", () => {
  it("a key lying about its length never proves a present key absent", () => {
    // "ab" claiming one byte: read as "a" (the old misreading), it would walk to "a" or prove "ab" absent
    const ab = liar([0x61, 0x62], 1);
    expect(expectOk(trie.trieVerify(T, variable, root, ab, null, full), "verdict"), "absence of the present key ab").toBe(false);
    expect(expectOk(trie.trieVerify(T, variable, root, ab, int(2n), full), "verdict"), "ab holds 2").toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, ab, int(1n), full), "verdict"), "ab is not a's value").toBe(false);
    // "a" claiming three bytes is "a"
    const a = liar([0x61], 3);
    expect(expectOk(trie.trieVerify(T, variable, root, a, null, full), "verdict"), "absence of the present key a").toBe(false);
    expect(expectOk(trie.trieVerify(T, variable, root, a, int(1n), full), "verdict")).toBe(true);
    expect(expectOk(verified().lookup(ab), "lookup")).toEqual({ kind: "present", valueHash: v2 });
    // a key over the mode's bound, claiming to fit, is refused by its own length
    const nine = liar([1, 2, 3, 4, 5, 6, 7, 8, 9], 1);
    expect(expectErr(variable.checkKey(nine), "too long").kind).toBe("InvalidKey");
    expect(expectErr(verified().lookup(nine), "too long").kind).toBe("InvalidKey");
    expect(expectErr(trie.trieVerify(T, variable, root, nine, null, full), "too long").kind).toBe("InvalidKey");
  });

  it("listing bounds, a prefix and claimed entries are read by their internal length", () => {
    const v = verified();
    expect(expectOk(v.prefix(liar([0x61, 0x62], 0)), "prefix")).toEqual({ kind: "listed", entries: [{ key: Uint8Array.of(0x61, 0x62), valueHash: v2 }] });
    expect(expectOk(v.range(liar([0x61, 0x62], 0)), "range")).toEqual({ kind: "listed", entries: [{ key: Uint8Array.of(0x61, 0x62), valueHash: v2 }] });
    const claim = [{ key: liar([0x61, 0x62], 1), valueHash: liar([...v2], 0) }];
    expect(expectOk(v.listsPrefix(Uint8Array.of(0x61, 0x62), claim), "verdict")).toBe(true);
    expect(expectOk(trie.triePrefixVerify(T, variable, root, liar([0x61, 0x62], 0), [[liar([0x61, 0x62], 1), int(2n)]], full), "verdict")).toBe(true);
  });

  it("a trie root built from lying keys is the root of what they hold", () => {
    expect(expectOk(trie.trieRoot(T, variable, [[liar([0x61], 5), v1], [liar([0x61, 0x62], 1), v2]]), "root")).toEqual(root);
  });

  it("an iterator that hands out one buffer again with new contents is read as it was at each step", () => {
    function* reused(): Generator<readonly [Uint8Array, Uint8Array]> {
      const key = new Uint8Array(2);
      const digest = new Uint8Array(32);
      for (const [k, d] of [
        [Uint8Array.of(0x61), v1],
        [Uint8Array.of(0x61, 0x62), v2],
      ] as const) {
        digest.set(d);
        yield [k.length === 1 ? key.subarray(0, 1).fill(0x61) : key.fill(0x61).fill(0x62, 1), digest];
      }
    }
    expect(expectOk(trie.trieRoot(T, variable, reused()), "root")).toEqual(root);
  });

  it("a Proxy reporting a length no array has is refused, never looped over", { timeout: 5_000 }, () => {
    for (const length of [Number.POSITIVE_INFINITY, 2 ** 53, 1.5, -1]) {
      const fake = new Proxy([], { get: (t, name) => (name === "length" ? length : name === "0" ? [Uint8Array.of(0x61), int(1n)] : (Reflect.get(t, name) as unknown)) });
      expect(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), fake, full), String(length)).toEqual(mismatch("expected an array of [key, value] entries"));
    }
  });
});

describe("the trie refuses an argument of the wrong type as TypeMismatch", () => {
  it("bytes that are not bytes, at every byte argument", () => {
    const v = verified();
    for (const x of notBytesCases()) {
      expect(variable.checkKey(bad(x)), label(x)).toEqual(notBytes);
      expect(v.lookup(bad(x)), label(x)).toEqual(notBytes);
      expect(v.prefix(bad(x)), label(x)).toEqual(notBytes);
      expect(v.range(bad(x)), label(x)).toEqual(notBytes);
      expect(v.range(Uint8Array.of(0), bad(x)), label(x)).toEqual(x === undefined ? { ok: true, value: { kind: "listed", entries: [{ key: Uint8Array.of(0x61), valueHash: v1 }, { key: Uint8Array.of(0x61, 0x62), valueHash: v2 }] } } : notBytes);
      expect(v.listsPrefix(bad(x), []), label(x)).toEqual(notBytes);
      expect(trie.trieVerify(T, variable, bad(x), Uint8Array.of(0x61), null, full), label(x)).toEqual(notBytes);
      expect(trie.trieVerify(T, variable, root, bad(x), null, full), label(x)).toEqual(notBytes);
      expect(trie.triePrefixVerify(T, variable, root, bad(x), [], full), label(x)).toEqual(notBytes);
      expect(trie.trieRangeVerify(T, variable, root, bad(x), null, [], full), label(x)).toEqual(notBytes);
      expect(trie.check(T, variable, bad(x), proof), label(x)).toEqual(notBytes);
      expect(trie.verify(T, variable, bad(x), proof), label(x)).toEqual(notBytes);
      expect(T.bytesHash(bad(x)), label(x)).toEqual(notBytes);
      expect(trie.Segment.of(8n, bad(x)), label(x)).toEqual(notBytes);
    }
    // a null root never verifies: it was the empty root's stand-in nowhere, and is refused before anything is read
    expect(trie.trieVerify(T, variable, bad(null), Uint8Array.of(0x62), null, full)).toEqual(notBytes);
  });

  it("a claimed value that is not a canonical value is never a claim of absence", () => {
    const v = verified();
    expect(expectOk(v.lookup(Uint8Array.of(0x61)), "present").kind).toBe("present");
    for (const x of notValueCases()) {
      // a plain { kind: "null" } posing as the null value would have claimed the present key "a" absent
      expect(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), bad(x), full), label(x)).toEqual(x === undefined ? notAValue : notAValue);
      expect(trie.trieVerify(T, variable, root, Uint8Array.of(0x62), bad(x), full), label(x)).toEqual(notAValue);
      expect(T.valueHash(bad(x)), label(x)).toEqual(notAValue);
      expect(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), Value.null, bad(x)), label(x)).toEqual(notAValue);
      expect(trie.TrieProof.decode(T, bad(x)), label(x)).toEqual(notAValue);
    }
    expect(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), bad(null), full), "null is the claim of absence").toEqual({ ok: true, value: false });
  });

  it("profiles, modes, proofs and segments not made here are refused, and none of a Proxy's traps runs", () => {
    const { proxy: hashingProxy, traps: t1 } = trapped(T);
    const { proxy: modeProxy, traps: t2 } = trapped(variable);
    const { proxy: proofProxy, traps: t3 } = trapped(proof);
    const { proxy: keyProxy, traps: t4 } = trapped(Uint8Array.of(0x61));
    expect(trie.trieVerify(hashingProxy, variable, root, Uint8Array.of(0x61), null, full)).toEqual(mismatch("expected a TrieHashing"));
    expect(trie.trieVerify(T, modeProxy, root, Uint8Array.of(0x61), null, full)).toEqual(mismatch("expected a KeyMode"));
    expect(trie.check(T, variable, root, proofProxy)).toEqual(mismatch("expected a TrieProof"));
    expect(trie.trieVerify(T, variable, root, keyProxy, null, full)).toEqual(notBytes);
    expect([...t1, ...t2, ...t3, ...t4]).toEqual([]);
    const fakes: unknown[] = [null, 5, { algorithm: "SHA-256", size: 32 }, Object.create(trie.TrieHashing.prototype)];
    for (const f of fakes) {
      expect(trie.TrieProof.decode(bad(f), full), label(f)).toEqual(mismatch("expected a TrieHashing"));
      expect(trie.trieRoot(bad(f), variable, []), label(f)).toEqual(mismatch("expected a TrieHashing"));
      expect(trie.TrieProof.fromRecords(bad(f), []), label(f)).toEqual(mismatch("expected a TrieHashing"));
    }
    expect(trie.check(T, bad({ kind: "variable", bytes: 8 }), root, proof)).toEqual(mismatch("expected a KeyMode"));
    expect(trie.check(T, variable, root, bad({ isEmpty: true }))).toEqual(mismatch("expected a TrieProof"));
    expect(T.leafHash(bad({ length: 8, bytes: Uint8Array.of(0x61) }), v1)).toEqual(mismatch("expected a Segment"));
    for (const a of [null, 5, "SHA-1", "sha-256"]) expect(trie.TrieHashing.of(bad(a)), label(a)).toEqual(mismatch("expected a HashAlgorithm"));
    for (const n of ["8", 8n, null]) {
      expect(trie.KeyMode.fixed(bad(n)), label(n)).toEqual(mismatch("expected a number"));
      expect(trie.KeyMode.variable(bad(n)), label(n)).toEqual(mismatch("expected a number"));
    }
    expect(trie.Segment.of(bad(8), Uint8Array.of(0x61))).toEqual(mismatch("expected a bigint"));
  });

  it("claimed entries and records are read by index, each once, and refused by their place", () => {
    expect(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), bad(null), full)).toEqual(mismatch("expected an array of [key, value] entries"));
    expect(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), bad([[Uint8Array.of(0x61), int(1n)], [Uint8Array.of(0x61, 0x62), { kind: "int" }]]), full)).toEqual(
      mismatch("expected a canonical value at index 1"),
    );
    expect(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), bad([[null, int(1n)]]), full)).toEqual(mismatch("expected a Uint8Array key at index 0"));
    expect(verified().listsPrefix(new Uint8Array(0), bad([{ key: Uint8Array.of(0x61) }]))).toEqual(mismatch("expected a TrieEntry of Uint8Arrays at index 0"));
    expect(trie.TrieProof.fromRecords(T, bad([{ h: "00" }]))).toEqual(mismatch("expected a trie record of its types at index 0"));
    expect(trie.TrieProof.fromRecords(T, bad([{ s: { n: 8, b: Uint8Array.of(0x61) }, v: v1 }]))).toEqual(mismatch("expected a trie record of its types at index 0"));
    expect(trie.trieRoot(T, variable, bad([[Uint8Array.of(0x61), "v1"]]))).toEqual(mismatch("expected a [key, valueHash] array of Uint8Arrays at index 0"));
    expect(trie.trieRoot(T, variable, bad(5))).toEqual(mismatch("expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)"));
    // a Proxy of the entries array is an array: each index is read once
    const entries = [[Uint8Array.of(0x61), int(1n)] as const, [Uint8Array.of(0x61, 0x62), int(2n)] as const];
    const reads: string[] = [];
    const counted = new Proxy(entries, {
      get: (t, name) => {
        reads.push(String(name));
        return Reflect.get(t, name) as unknown;
      },
    });
    expect(expectOk(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), counted, full), "verdict")).toBe(true);
    expect(reads).toEqual(["length", "0", "1"]);
  });

  it("another realm's bytes and a SharedArrayBuffer's are bytes", () => {
    const sab = shared([0x61, 0x62]);
    expect(expectOk(trie.trieVerify(T, variable, foreign([...root]), foreign([0x61, 0x62]), int(2n), full), "foreign")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, sab, int(2n), full), "shared")).toBe(true);
    expect(expectOk(verified().lookup(sab), "shared")).toEqual({ kind: "present", valueHash: v2 });
    expect(expectOk(trie.trieRoot(T, variable, [[foreign([0x61]), foreign([...v1])], [sab, shared([...v2])]]), "root")).toEqual(root);
  });
});

// ------------------------------------------------------------------ Merkle

const tagged = expectOk(merkle.TaggedMerkleHashing.of("SHA-256"), "tagged");
const rfc = expectOk(merkle.Rfc6962MerkleHashing.of("SHA-256"), "rfc");
const leaves = [int(0n), int(1n), int(2n)];
const leafHashes = leaves.map((l) => expectOk(tagged.leaf(l), "leaf"));
const mroot = expectOk(merkle.merkleRoot(tagged, leafHashes), "root");
const l01 = expectOk(tagged.node(leafHashes[0] ?? new Uint8Array(0), leafHashes[1] ?? new Uint8Array(0)), "node");
const inclusion2 = expectOk(merkle.InclusionProof.of(2n, 3n, [l01]), "proof");
const inclusionValue = expectOk(inclusion2.encode(), "encodes");

describe("the Merkle verifiers refuse an argument of the wrong type as TypeMismatch", () => {
  it("roots, leaf hashes and paths that are not bytes; a null root never verifies", () => {
    for (const x of notBytesCases()) {
      expect(merkle.checkInclusion(tagged, bad(x), leafHashes[2] ?? new Uint8Array(0), inclusion2), label(x)).toEqual(notBytes);
      expect(merkle.verifyInclusion(tagged, bad(x), leafHashes[2] ?? new Uint8Array(0), inclusion2), label(x)).toEqual(notBytes);
      expect(merkle.verifyInclusion(tagged, mroot, bad(x), inclusion2), label(x)).toEqual(notBytes);
      expect(merkle.merkleVerify(tagged, { root: bad(x), size: 3n }, int(2n), inclusionValue), label(x)).toEqual(mismatch("expected head.root to be a Uint8Array"));
      expect(merkle.merkleRoot(tagged, [bad(x)]), label(x)).toEqual(mismatch("expected a Uint8Array at index 0"));
      expect(merkle.InclusionProof.of(0n, 1n, [bad(x)]), label(x)).toEqual(mismatch("expected a Uint8Array at index 0"));
      expect(merkle.ConsistencyProof.of(0n, 1n, [bad(x)]), label(x)).toEqual(mismatch("expected a Uint8Array at index 0"));
      expect(rfc.leaf(bad(x)), label(x)).toEqual(notBytes);
      expect(rfc.leafWeight(bad(x)), label(x)).toEqual(notBytes);
      expect(tagged.node(bad(x), l01), label(x)).toEqual(notBytes);
      expect(merkle.merkleVerify(rfc, { root: mroot, size: 3n }, bad(x), inclusionValue), label(x)).toEqual(notBytes);
    }
    expect(expectOk(merkle.verifyInclusion(tagged, mroot, leafHashes[2] ?? new Uint8Array(0), inclusion2), "verdict")).toBe(true);
    expect(merkle.merkleRoot(tagged, bad(null))).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(merkle.InclusionProof.of(bad(2), 3n, [l01])).toEqual(mismatch("expected a bigint"));
    expect(merkle.ConsistencyProof.of(1n, bad("3"), [])).toEqual(mismatch("expected a bigint"));
  });

  it("a leaf that is not a canonical value is refused before its weight is read", () => {
    for (const x of [null, ...notValueCases()]) {
      expect(tagged.leaf(bad(x)), label(x)).toEqual(notAValue);
      expect(tagged.leafWeight(bad(x)), label(x)).toEqual(notAValue);
      expect(merkle.merkleVerify(tagged, { root: mroot, size: 3n }, bad(x), inclusionValue), label(x)).toEqual(notAValue);
      expect(merkle.merkleVerify(tagged, { root: mroot, size: 3n }, int(2n), bad(x)), label(x)).toEqual(notAValue);
      expect(merkle.InclusionProof.decode(tagged, bad(x)), label(x)).toEqual(notAValue);
    }
    expect(expectOk(merkle.merkleVerify(tagged, { root: mroot, size: 3n }, int(2n), inclusionValue), "verdict")).toBe(true);
  });

  it("profiles and proofs not made here are refused, with no Proxy trap and no caller code run", () => {
    let called = 0;
    const custom = {
      profile: "tagged",
      algorithm: "SHA-256",
      size: 32,
      empty: mroot,
      leaf: () => ((called += 1), { ok: true, value: mroot }),
      leafWeight: () => ((called += 1), { ok: true, value: 1n }),
      node: () => ((called += 1), { ok: true, value: mroot }),
    };
    const notAProfile = mismatch("expected a TaggedMerkleHashing or an Rfc6962MerkleHashing");
    expect(merkle.verifyInclusion(bad(custom), mroot, mroot, inclusion2)).toEqual(notAProfile);
    expect(merkle.merkleVerify(bad(custom), { root: mroot, size: 3n }, int(2n), inclusionValue)).toEqual(notAProfile);
    expect(merkle.merkleRoot(bad(custom), [])).toEqual(notAProfile);
    expect(called, "no method of the caller's object ran").toBe(0);
    const { proxy, traps } = trapped(tagged);
    expect(merkle.merkleRoot(proxy, [])).toEqual(notAProfile);
    expect(traps).toEqual([]);
    const fakeProof = { index: 2n, size: 3n, path: [l01] };
    expect(merkle.verifyInclusion(tagged, mroot, leafHashes[2] ?? new Uint8Array(0), bad(fakeProof))).toEqual(mismatch("expected an InclusionProof"));
    expect(merkle.verifyConsistency(tagged, mroot, mroot, bad(fakeProof))).toEqual(mismatch("expected a ConsistencyProof"));
    for (const a of [null, 1, "MD5"]) {
      expect(merkle.TaggedMerkleHashing.of(bad(a)), label(a)).toEqual(mismatch("expected a HashAlgorithm"));
      expect(merkle.Rfc6962MerkleHashing.of(bad(a)), label(a)).toEqual(mismatch("expected a HashAlgorithm"));
    }
  });

  it("a head's fields are read once each, and a head that is not an object is refused", () => {
    const reads: string[] = [];
    const head = {
      get root(): Uint8Array {
        reads.push("root");
        return mroot;
      },
      get size(): bigint {
        reads.push("size");
        return 3n;
      },
    };
    expect(expectOk(merkle.merkleVerify(tagged, head, int(2n), inclusionValue), "verdict")).toBe(true);
    expect(reads).toEqual(["root", "size"]);
    expect(merkle.merkleVerify(tagged, bad(null), int(2n), inclusionValue)).toEqual(mismatch("expected head to be a MerkleHead"));
    expect(merkle.merkleVerify(tagged, { root: mroot, size: bad(3) }, int(2n), inclusionValue)).toEqual(mismatch("expected a bigint"));
    expect(merkle.merkleConsistencyVerify(tagged, bad(undefined), { root: mroot, size: 3n }, inclusionValue)).toEqual(mismatch("expected oldHead to be a MerkleHead"));
  });

  it("a lying root or path is read by its internal length; a shared or foreign one is bytes, and a path is copied", () => {
    expect(expectOk(merkle.verifyInclusion(tagged, liar([...mroot], 3), liar([...(leafHashes[2] ?? [])], 99), inclusion2), "verdict")).toBe(true);
    const short = liar([...mroot].slice(0, 31), 32);
    expect(expectErr(merkle.checkInclusion(tagged, short, leafHashes[2] ?? new Uint8Array(0), inclusion2), "short").kind).toBe("Encoding");
    const sab = shared([...l01]);
    const p = expectOk(merkle.InclusionProof.of(2n, 3n, [sab]), "proof");
    sab.fill(0);
    expect(expectOk(merkle.verifyInclusion(tagged, foreign([...mroot]), foreign([...(leafHashes[2] ?? [])]), p), "verdict"), "the path as it was").toBe(true);
    expect(expectOk(merkle.merkleVerify(rfc, { root: expectOk(merkle.merkleRoot(rfc, [expectOk(rfc.leaf(shared([1])), "leaf")]), "root"), size: 1n }, shared([1]), expectOk(expectOk(merkle.InclusionProof.of(0n, 1n, []), "p").encode(), "e")), "rfc")).toBe(true);
  });
});

// ------------------------------------------------------------------ versioned

const vectors = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors);
const vt = expectOk(versioned.VersionedTrie.of("SHA-256", versioned.VersionKey.long), "profile");
const key = (s: string): versioned.StateKey => expectOk(versioned.StateKey.parse(s), "key");
const finalHead = expectOk(vt.decodeHead(valueOf(vectors.head.cbor)), "head");

describe("the versioned trie refuses an argument of the wrong type as TypeMismatch", () => {
  it("state keys and namespaces from what is not text or bytes, unread", () => {
    const hostile = {
      toString: (): string => {
        throw new Error("read");
      },
    };
    for (const x of [null, undefined, 5, hostile, ["fiber"]]) {
      expect(versioned.StateKey.parse(bad(x)), label(x)).toEqual(mismatch("expected a string"));
      expect(versioned.StateNamespace.parse(bad(x)), label(x)).toEqual(mismatch("expected a string"));
    }
    for (const x of notBytesCases()) expect(versioned.StateKey.fromBytes(bad(x)), label(x)).toEqual(notBytes);
    // bytes are read by their own length: "fiber/1" claiming 2 bytes is "fiber/1"
    const bytes = liar([...new TextEncoder().encode("fiber/1")], 2);
    expect(expectOk(versioned.StateKey.fromBytes(bytes), "key").value).toBe("fiber/1");
    expect(expectOk(versioned.StateKey.fromBytes(foreign([0x61])), "foreign").value).toBe("a");
    const ns = expectOk(versioned.StateNamespace.parse("fiber"), "ns");
    expect(ns.contains(bad({ value: "fiber/1" }))).toEqual(mismatch("expected a StateKey"));
    expect(expectOk(ns.contains(key("fiber/1")), "contains")).toBe(true);
  });

  it("checkValue, valueHash and bytesHash refuse what is not of their type, before reading it", () => {
    for (const x of [null, ...notValueCases()]) {
      expect(vt.checkValue(key("fiber/1"), bad(x)), label(x)).toEqual(notAValue);
      expect(vt.valueHash(bad(x)), label(x)).toEqual(notAValue);
    }
    expect(vt.checkValue(bad({ value: "fiber/1", preview: () => null }), int(1n))).toEqual(mismatch("expected a StateKey"));
    expect(vt.bytesHash(bad("ab"))).toEqual(notBytes);
    expect(vt.checkValue(key("fiber/1"), int(1n)).ok).toBe(true);
  });

  it("version keys: a version that is not a bigint, bytes that are not bytes, a width that is not a number", () => {
    const long = versioned.VersionKey.long;
    for (const v of [1, "1", null, undefined]) expect(long.encode(bad(v)), label(v)).toEqual(mismatch("expected a bigint"));
    expect(long.decode(bad(null))).toEqual(mismatch("expected a Uint8Array"));
    expect(versioned.VersionKey.unsignedLong(bad("8"))).toEqual(mismatch("expected a number"));
    expect(expectOk(long.decode(liar([0, 0, 0, 0, 0, 0, 0, 1], 3)), "decode")).toBe(1n);
  });

  it("a caller's version key is checked, and what it returns is checked before it is used", () => {
    for (const k of [null, 5, {}, { width: "8", encode: () => null, decode: () => null }, { width: 8, encode: 1, decode: () => null }]) {
      expect(versioned.VersionedTrie.of("SHA-256", bad(k)), label(k)).toEqual(mismatch("expected a VersionKey"));
    }
    const junk = expectOk(versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: () => bad(null), decode: () => bad(5) }), "profile");
    const head = { version: 1n, stateRoot: new Uint8Array(32), historyRoot: new Uint8Array(32) };
    expect(junk.root(head)).toEqual(mismatch("expected a version key's result to be a Result of its type"));
    const short = expectOk(versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: () => ({ ok: true, value: new Uint8Array(3) }), decode: () => ({ ok: true, value: 1n }) }), "profile");
    expect(short.root(head), "an encoding of another width").toEqual(mismatch("expected a version key's result to be a Result of its type"));
    // a caller's error is copied into a refusal of the module's, never handed on as it is
    const refusing = expectOk(
      versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: () => bad({ ok: false, error: { kind: "Trie", error: null } }), decode: () => bad({ ok: false, error: { kind: "InvalidVersion", reason: "no" } }) }),
      "profile",
    );
    expect(refusing.root(head)).toEqual({ ok: false, error: { kind: "InvalidVersion", reason: "the version key refused the version" } });
    expect(refusing.decodeHead(valueOf(vectors.head.cbor))).toEqual({ ok: false, error: { kind: "InvalidVersion", reason: "no" } });
    // a well-behaved custom key gives what VersionKey.long gives
    const custom = expectOk(
      versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: (v) => versioned.VersionKey.long.encode(v), decode: (b) => versioned.VersionKey.long.decode(b) }),
      "profile",
    );
    expect(expectOk(custom.root(finalHead), "root")).toEqual(expectOk(vt.root(finalHead), "root"));
    for (const a of [null, "MD5"]) expect(versioned.VersionedTrie.of(bad(a), versioned.VersionKey.long), label(a)).toEqual(mismatch("expected a HashAlgorithm"));
  });

  it("heads, proofs and roots that are not of their types, read once each", () => {
    for (const x of notBytesCases()) {
      expect(vt.root({ ...finalHead, stateRoot: bad(x) }), label(x)).toEqual(mismatch("expected the head's roots to be Uint8Arrays"));
      expect(vt.verifyHead(bad(x), { head: finalHead, top: proof }), label(x)).toEqual(notBytes);
    }
    expect(vt.root(bad(null))).toEqual(mismatch("expected the head to be a Head"));
    expect(vt.verifyHead(new Uint8Array(32), bad(null))).toEqual(mismatch("expected a VersionedTrieProof"));
    expect(vt.verifyHead(new Uint8Array(32), bad({ head: finalHead, top: {} }))).toEqual(mismatch("expected the proof's top to be a TrieProof"));
    for (const x of [null, ...notValueCases()]) {
      expect(vt.decodeProof(bad(x)), label(x)).toEqual(notAValue);
      expect(vt.decodeDelta(bad(x)), label(x)).toEqual(notAValue);
      expect(vt.decodeHead(bad(x)), label(x)).toEqual(notAValue);
    }
    for (const x of notBytesCases()) expect(vt.decodeChunk(bad(x)), label(x)).toEqual(notBytes);
    const reads: string[] = [];
    const head = {
      get version(): bigint {
        reads.push("version");
        return finalHead.version;
      },
      get stateRoot(): Uint8Array {
        reads.push("stateRoot");
        return finalHead.stateRoot;
      },
      get historyRoot(): Uint8Array {
        reads.push("historyRoot");
        return finalHead.historyRoot;
      },
    };
    expect(expectOk(vt.root(head), "root")).toEqual(expectOk(vt.root(finalHead), "root"));
    expect(reads).toEqual(["version", "stateRoot", "historyRoot"]);
  });

  it("the trusted root is the one given: a proof's getter cannot swap it for another commit's", () => {
    const stateProof = expectOk(vt.decodeProof(valueOf(vectors.proofs.find((p) => p.name === "current-keys")?.cbor ?? "")), "a state proof");
    const right = bytes(vectors.root);
    expect(vt.verifyState(right, stateProof).ok, "it verifies against its root").toBe(true);
    const trusted = new Uint8Array(right.length);
    const swapping = {
      get head(): unknown {
        trusted.set(right);
        return stateProof.head;
      },
      top: stateProof.top,
      ...(stateProof.state === undefined ? {} : { state: stateProof.state }),
    };
    expect(expectErr(vt.verifyState(trusted, bad(swapping)), "the root given, all zeros").kind).toBe("RootMismatch");
  });

  it("verifyVersions: a claim's getter cannot swap the trusted root either", () => {
    const p = vectors.proofs.find((x) => x.kind === "versions");
    if (p === undefined) throw new Error("a versions fixture");
    const vproof = expectOk(vt.decodeProof(valueOf(p.cbor)), "proof");
    const right = bytes(vectors.root);
    const claims = (p.versions ?? []).map((v) => [v.version ?? -1n, bytes(v.stateRoot ?? "")] as const);
    expect(expectOk(vt.verifyVersions(right, vproof, p.at ?? -1n, p.until ?? null, claims), "verifies")).toBe(true);
    const trusted = new Uint8Array(right.length);
    const swapping: unknown[] = [...claims];
    Object.defineProperty(swapping, 0, {
      get: () => {
        trusted.set(right);
        return claims[0];
      },
    });
    expect(expectErr(vt.verifyVersions(trusted, vproof, p.at ?? -1n, p.until ?? null, bad(swapping)), "the root given, all zeros").kind).toBe("RootMismatch");
  });

  it("a profile built by calling its constructor from outside verifies nothing", () => {
    const forged = Reflect.construct(versioned.VersionedTrie as unknown as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), vt.hashing, vt.versionKey, [variable, variable, variable]]) as versioned.VersionedTrie<bigint>;
    const stateProof = expectOk(vt.decodeProof(valueOf(vectors.proofs.find((x) => x.name === "current-keys")?.cbor ?? "")), "a state proof");
    expect(forged.verifyState(bytes(vectors.root), stateProof)).toEqual(mismatch("expected a VersionedTrie"));
    expect(forged.verifyHistoricalKey(bytes(vectors.root), stateProof, 1n)).toEqual(mismatch("expected a VersionedTrie"));
    expect(forged.root(finalHead)).toEqual(mismatch("expected a VersionedTrie"));
    expect(forged.decodeDelta(int(1n))).toEqual(mismatch("expected a VersionedTrie"));
  });

  it("a verified state, a reader or a replica built by calling a constructor from outside answers nothing", () => {
    const forge = (c: unknown, args: readonly unknown[]): unknown => Reflect.construct(c as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), ...args]);
    const fakeVerified = Reflect.construct(trie.VerifiedTrieProof as unknown as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), variable, undefined]);
    const state = forge(versioned.VerifiedState, [fakeVerified, T]) as versioned.VerifiedState;
    // it held a proof no root was checked against, and its holds(key, null) claimed absence: true
    expect(state.holds(key("fiber/1"), null)).toEqual(mismatch("expected a VerifiedState"));
    expect(state.lookup(key("fiber/1"))).toEqual(mismatch("expected a VerifiedState"));
    const reader = forge(versioned.SnapshotReader, [vt, finalHead, "done", undefined, undefined, [], []]) as versioned.SnapshotReader<bigint>;
    expect(reader.finish()).toEqual(mismatch("expected a SnapshotReader"));
    const replica = forge(versioned.Replica, [vt, finalHead, {}, {}]) as versioned.Replica<bigint>;
    expect(replica.root()).toEqual(mismatch("expected a Replica"));
    expect(replica.get(key("fiber/1"))).toEqual(mismatch("expected a Replica"));
  });

  it("verified state: a key that is not a StateKey, and a value posing as null, are refused, never answered", () => {
    const deltas = vectors.commits.map((c) => expectOk(vt.decodeDelta(valueOf(c.delta)), "delta"));
    const replica = deltas.reduce((r, d) => expectOk(r.applyDelta(d), "applies"), expectOk(versioned.Replica.empty(vt), "empty"));
    expect(expectOk(replica.root(), "root")).not.toBeNull();
    for (const x of [null, "oracle/a/b", { value: "oracle/a/b" }]) expect(replica.get(bad(x)), label(x)).toEqual(mismatch("expected a StateKey"));
    expect(expectOk(replica.get(key("oracle/a/b")), "get")).toEqual(expectOk(Value.text("a"), "text"));
    for (const x of [null, 5, { head: finalHead }, { head: finalHead, upserts: [[key("a"), { kind: "null" }]], removes: [] }, { head: finalHead, upserts: [], removes: ["a"] }]) {
      expect(expectErr(replica.applyDelta(bad(x)), label(x)).kind, label(x)).toBe("TypeMismatch");
    }
  });

  it("snapshots: a reader's profile and head, chunks of the wrong type, and a restore's chunks that are not bytes", () => {
    expect(versioned.SnapshotReader.start(bad({}), finalHead)).toEqual(mismatch("expected a VersionedTrie"));
    expect(versioned.SnapshotReader.start(vt, bad(null))).toEqual(mismatch("expected the trusted head to be a Head"));
    const reader = expectOk(versioned.SnapshotReader.start(vt, finalHead), "reader");
    for (const c of [null, 5, { kind: "unknown" }, { kind: "state", entries: [["a", int(1n)]] }, { kind: "history", versions: [[1n, "root"]] }]) {
      expect(expectErr(reader.feed(bad(c)), label(c)).kind, label(c)).toBe("TypeMismatch");
    }
    for (const x of notBytesCases()) expect(reader.feedBytes(bad(x)), label(x)).toEqual(notBytes);
    expect(expectErr(versioned.Replica.restore(vt, finalHead, bad([null])), "a null chunk").kind).toBe("TypeMismatch");
    expect(expectErr(versioned.Replica.restore(vt, finalHead, bad(5)), "no iterable").kind).toBe("TypeMismatch");
    expect(versioned.Replica.fromSnapshot(vt, finalHead, bad({ head: finalHead, entries: null, history: [] }))).toEqual(mismatch("expected the snapshot's entries"));
    expect(versioned.Replica.empty(bad(null))).toEqual(mismatch("expected a VersionedTrie"));
  });
});

// ------------------------------------------------------------------ Poseidon

describe("Poseidon refuses an argument of the wrong type as TypeMismatch", () => {
  const fr = (n: bigint): poseidon.Fr => expectOk(poseidon.Fr.of(n), "fr");

  it("field elements are made here, never duck-typed, and an equality with what is not one is false", () => {
    const fake = { value: 1n, equals: () => true };
    for (const x of [null, 1, "1", fake, Object.create(poseidon.Fr.prototype)]) {
      expect(poseidon.compress(bad(x), fr(1n)), label(x)).toEqual(mismatch("expected an Fr"));
      expect(poseidon.hash([fr(1n), bad(x)]), label(x)).toEqual(mismatch("expected an Fr at index 1"));
      expect(fr(1n).equals(bad(x)), label(x)).toBe(false);
    }
    for (const n of [1, "1", null, { valueOf: () => 1n }]) {
      expect(poseidon.Fr.of(bad(n)), label(n)).toEqual(mismatch("expected a bigint"));
      expect(poseidon.Fr.reduce(bad(n)), label(n)).toEqual(mismatch("expected a bigint"));
    }
    expect(poseidon.hash(bad(null))).toEqual(mismatch("expected an array of Fr"));
    expect(expectErr(poseidon.hash([fr(1n), fr(1n), fr(1n), fr(1n), fr(1n)]), "five").kind).toBe("InputCount");
  });

  it("a root that is not an Fr never verifies, and a proof that is not one is refused", () => {
    const p = expectOk(poseidon.PoseidonMerkleProof.of(0n, [fr(7n)]), "proof");
    const root = expectOk(poseidon.computeRoot(fr(3n), p), "root");
    expect(expectOk(poseidon.verifyInclusion(fr(3n), p, root), "verdict")).toBe(true);
    const fakeRoot = { value: root.value, equals: () => true };
    expect(poseidon.verifyInclusion(fr(3n), p, bad(fakeRoot))).toEqual(mismatch("expected an Fr"));
    expect(poseidon.verifyAbsence(p, bad(null))).toEqual(mismatch("expected an Fr"));
    expect(poseidon.verifyInclusion(fr(3n), bad({ position: 0n, siblings: [fr(7n)] }), root)).toEqual(mismatch("expected a PoseidonMerkleProof"));
    expect(poseidon.computeRoot(bad(3n), p)).toEqual(mismatch("expected an Fr"));
    expect(poseidon.PoseidonMerkleProof.of(bad(0), [fr(7n)])).toEqual(mismatch("expected a bigint"));
    expect(poseidon.PoseidonMerkleProof.of(0n, bad([7n]))).toEqual(mismatch("expected an Fr at index 0"));
    for (const x of [null, ...notValueCases()]) expect(poseidon.PoseidonMerkleProof.decode(bad(x)), label(x)).toEqual(notAValue);
  });

  it("depths that are not numbers and leaves that are not [bigint, Fr] pairs", () => {
    for (const d of ["8", 8n, null]) {
      expect(poseidon.zeroHashes(bad(d)), label(d)).toEqual(mismatch("expected a number"));
      expect(poseidon.treeRoot(bad(d), []), label(d)).toEqual(mismatch("expected a number"));
    }
    expect(poseidon.treeRoot(8, bad([[1, fr(1n)]]))).toEqual(mismatch("expected a [bigint, Fr] array at index 0"));
    expect(poseidon.treeRoot(8, bad([[1n, 1n]]))).toEqual(mismatch("expected a [bigint, Fr] array at index 0"));
    expect(poseidon.treeRoot(8, bad(null)).ok).toBe(false);
    expect(expectOk(poseidon.treeRoot(8, new Map([[1n, fr(1n)]])), "a Map").equals(expectOk(poseidon.treeRoot(8, [[1n, fr(1n)]]), "an array"))).toBe(true);
  });
});

describe("what a constructor builds when JavaScript calls it from outside is refused by every check", () => {
  /** A constructor call without the package's token: TypeScript's `private` does not stop JavaScript. */
  const forge = (c: unknown, args: readonly unknown[]): unknown => Reflect.construct(c as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), ...args]);

  it("trie: a mode, segment, hashing or proof", () => {
    const mode = forge(trie.KeyMode, ["variable", 8]);
    const hashing = forge(trie.TrieHashing, ["SHA-256"]);
    const forgedProof = forge(trie.TrieProof, [undefined]);
    const segment = forge(trie.Segment, [{ bytes: Uint8Array.of(0x61), length: 8 }]);
    expect(trie.check(T, bad(mode), root, proof)).toEqual(mismatch("expected a KeyMode"));
    expect(trie.check(bad(hashing), variable, root, proof)).toEqual(mismatch("expected a TrieHashing"));
    // the forged proof of the empty trie would have verified against the empty root
    expect(trie.check(T, variable, T.empty, bad(forgedProof))).toEqual(mismatch("expected a TrieProof"));
    expect(T.leafHash(bad(segment), v1)).toEqual(mismatch("expected a Segment"));
    // a forged constructor reads nothing it is given: a junk algorithm is never hashed with
    expect(() => forge(trie.TrieHashing, [{}])).not.toThrow();
  });

  it("a verified trie proof answers nothing it was not made to, and a forged profile mints no proof", () => {
    const fake = Reflect.construct(trie.VerifiedTrieProof as unknown as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), variable, { kind: "leaf" }]) as trie.VerifiedTrieProof;
    expect(fake.lookup(Uint8Array.of(0x61))).toEqual(mismatch("expected a VerifiedTrieProof"));
    expect(fake.prefix(empty)).toEqual(mismatch("expected a VerifiedTrieProof"));
    expect(fake.range(empty)).toEqual(mismatch("expected a VerifiedTrieProof"));
    expect(fake.listsPrefix(empty, [])).toEqual(mismatch("expected a VerifiedTrieProof"));
    expect(fake.listsRange(empty, undefined, [])).toEqual(mismatch("expected a VerifiedTrieProof"));
    const profile = forge(versioned.VersionedTrie, [{ size: 1 }, versioned.VersionKey.long, [variable, variable, variable]]) as versioned.VersionedTrie<bigint>;
    expect(profile.decodeProof(int(1n))).toEqual(mismatch("expected a VersionedTrie"));
  });

  it("an entry's getter cannot change the trusted root, nor a key read before it", () => {
    // another trie, "b" -> 1, and its proof: a getter swapping the root to this one must not make the claim verify
    const otherRoot = expectOk(trie.trieRoot(T, variable, [[Uint8Array.of(0x62), v1]]), "other root");
    const otherProof: Value = expectOk(models.trie.TrieProof.encode([{ s: { n: 8n, b: Uint8Array.of(0x62) }, v: v1 }]), "other proof");
    const trusted = Uint8Array.from(root);
    const swapping = [
      [
        Uint8Array.of(0x62),
        {
          get kind(): never {
            throw new Error("never read: it is not a value");
          },
        },
      ],
    ];
    Object.defineProperty(swapping, 0, {
      get: () => {
        trusted.set(otherRoot);
        return [Uint8Array.of(0x62), int(1n)];
      },
    });
    expect(trie.triePrefixVerify(T, variable, trusted, new Uint8Array(0), bad(swapping), otherProof)).toEqual({ ok: true, value: false });
    // a pair whose value's getter rewrites its key: the key was copied before the value was read
    const rewriting = (): unknown[] => {
      const key = Uint8Array.of(0x61);
      const pair: unknown[] = [key];
      Object.defineProperty(pair, 1, {
        get: () => {
          key[0] = 0x62;
          return int(1n);
        },
      });
      return pair;
    };
    expect(expectOk(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61), bad([rewriting(), [Uint8Array.of(0x61, 0x62), int(2n)]]), full), "verdict"), "a holds 1, read as a").toBe(true);
  });

  it("a shared answer cannot be changed by a caller", () => {
    const absent = expectOk(verified().lookup(Uint8Array.of(0x63)), "absent");
    expect(Object.isFrozen(absent)).toBe(true);
    expect(Object.isFrozen(variable.checkKey(Uint8Array.of(1)))).toBe(true);
  });

  it("a record's getter that detaches a field read before it changes nothing read: each field is copied as read", () => {
    const b = Uint8Array.of(0x61);
    const record = {
      s: { n: 8n, b },
      get v(): Uint8Array {
        // detach the segment's bytes, already read
        structuredClone(b.buffer, { transfer: [b.buffer] });
        return v1;
      },
    };
    const p = expectOk(trie.TrieProof.fromRecords(T, bad([record])), "a one-leaf proof");
    const leafOnly = expectOk(trie.trieRoot(T, expectOk(trie.KeyMode.fixed(1), "fixed(1)"), [[Uint8Array.of(0x61), v1]]), "root");
    expect(expectOk(trie.verify(T, expectOk(trie.KeyMode.fixed(1), "fixed(1)"), leafOnly, p), "verifies")).not.toBeNull();
  });

  it("an iterable Proxy of an array reporting a length no array has is refused", { timeout: 5_000 }, () => {
    for (const length of [Number.POSITIVE_INFINITY, -1, "2", 1.5, Symbol("length")]) {
      const entries = new Proxy([[Uint8Array.of(0x61), v1]], { get: (t, name) => (name === "length" ? length : (Reflect.get(t, name) as unknown)) });
      expect(expectErr(trie.trieRoot(T, variable, bad(entries)), String(length)).kind).toBe("TypeMismatch");
      const leaves = new Proxy([[1n, expectOk(poseidon.Fr.of(1n), "fr")]], { get: (t, name) => (name === "length" ? length : (Reflect.get(t, name) as unknown)) });
      expect(expectErr(poseidon.treeRoot(4, bad(leaves)), String(length)).kind).toBe("TypeMismatch");
    }
  });

  it("merkle, versioned and poseidon", () => {
    const tagged2 = forge(merkle.TaggedMerkleHashing, ["SHA-256"]);
    expect(merkle.merkleRoot(bad(tagged2), [])).toEqual(mismatch("expected a TaggedMerkleHashing or an Rfc6962MerkleHashing"));
    const p = forge(merkle.InclusionProof, [2n, 3n, [l01]]);
    expect(merkle.verifyInclusion(tagged, mroot, leafHashes[2] ?? new Uint8Array(0), bad(p))).toEqual(mismatch("expected an InclusionProof"));
    const k = forge(versioned.StateKey, ["fiber/1"]);
    expect(vt.checkValue(bad(k), int(1n))).toEqual(mismatch("expected a StateKey"));
    const profile = forge(versioned.VersionedTrie, [T, versioned.VersionKey.long, [variable, variable, variable]]);
    expect(versioned.Replica.empty(bad(profile))).toEqual(mismatch("expected a VersionedTrie"));
    const fr = forge(poseidon.Fr, [5n]);
    expect(poseidon.compress(bad(fr), bad(fr))).toEqual(mismatch("expected an Fr"));
    expect(expectOk(poseidon.Fr.of(0n), "zero").equals(bad(forge(poseidon.Fr, [0n])))).toBe(false);
    expect(() => forge(poseidon.PoseidonMerkleProof, [0n, 5])).not.toThrow();
  });
});

describe("a revoked Proxy is refused as a TypeMismatch at every array and object argument, never thrown on", () => {
  const revoked = (): never => {
    const { proxy, revoke } = Proxy.revocable([], {});
    revoke();
    return bad(proxy);
  };

  it("merkle, trie, poseidon and versioned", () => {
    expect(merkle.InclusionProof.of(0n, 1n, revoked())).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(merkle.merkleRoot(tagged, revoked())).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(merkle.merkleVerify(tagged, revoked(), int(2n), inclusionValue)).toEqual(mismatch("expected head to be a MerkleHead"));
    expect(trie.trieRoot(T, variable, revoked())).toEqual(mismatch("expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)"));
    expect(trie.trieRoot(T, variable, bad([revoked()]))).toEqual(mismatch("expected a [key, valueHash] array of Uint8Arrays at index 0"));
    expect(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), revoked(), full)).toEqual(mismatch("expected an array of [key, value] entries"));
    expect(trie.TrieProof.fromRecords(T, bad([revoked()]))).toEqual(mismatch("expected a trie record of its types at index 0"));
    expect(poseidon.hash(revoked())).toEqual(mismatch("expected an array of Fr"));
    expect(poseidon.treeRoot(4, revoked()).ok).toBe(false);
    expect(vt.verifyState(bytes(vectors.root), revoked())).toEqual(mismatch("expected a VersionedTrieProof"));
    expect(vt.root(revoked())).toEqual(mismatch("expected the head to be a Head"));
    expect(versioned.VersionedTrie.of("SHA-256", revoked())).toEqual(mismatch("expected a VersionKey"));
  });

  it("a revoked Proxy of a function: a version key's methods, an iterable's iterator, next and return", () => {
    const revokedFn = (): (() => never) => {
      const { proxy, revoke } = Proxy.revocable(() => bad(undefined), {});
      revoke();
      return proxy;
    };
    const key = versioned.VersionKey.long;
    expect(versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: revokedFn(), decode: (b) => key.decode(b) })).toEqual(mismatch("expected a VersionKey"));
    // revoked after the profile was made: refused at the call
    const { proxy: encode, revoke } = Proxy.revocable((v: bigint) => key.encode(v), {});
    const later = expectOk(versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode, decode: (b) => key.decode(b) }), "profile");
    revoke();
    expect(later.root(finalHead)).toEqual(mismatch("expected a VersionKey"));
    expect(trie.trieRoot(T, variable, bad({ [Symbol.iterator]: revokedFn() }))).toEqual(mismatch("expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)"));
    expect(trie.trieRoot(T, variable, bad({ [Symbol.iterator]: () => ({ next: revokedFn() }) }))).toEqual(mismatch("expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)"));
    // next itself a Proxy, revoked by caller code (an entry's getter) mid-iteration; return revoked too
    let revokeNext = (): void => undefined;
    const entry = (): unknown[] => {
      const e: unknown[] = [Uint8Array.of(0x61)];
      Object.defineProperty(e, 1, {
        get: () => {
          revokeNext();
          return v1;
        },
      });
      return e;
    };
    const { proxy: next, revoke: revokeIt } = Proxy.revocable(() => ({ done: false, value: entry() }), {});
    revokeNext = revokeIt;
    const iterable = { [Symbol.iterator]: () => ({ next, return: revokedFn() }) };
    expect(expectErr(trie.trieRoot(T, variable, bad(iterable)), "a revoked next").kind).toBe("TypeMismatch");
  });
});

/** A Proxy of `target` that revokes itself as its first read returns: every later read of it would throw. */
function revokingAfterFirstRead<T extends object>(target: T): T {
  const { proxy, revoke } = Proxy.revocable(target, {
    get: (t, name) => {
      const v: unknown = Reflect.get(t, name);
      revoke();
      return v;
    },
  });
  return proxy;
}

describe("a query is fixed before its claim is read: a claim's getter cannot rewrite the prefix or bounds", () => {
  // the claim [ab -> 2] is the listing under "ab" and of ["ab", ...), never under "cb" or of ["cb", ...)
  // the getter rewrites the query to `to`: "ab" for a prefix or lower bound, 0xff for an upper bound
  const rewritingPairs = (query: Uint8Array, to: readonly number[] = [0x61, 0x62]): unknown[] => {
    const entries: unknown[] = [];
    Object.defineProperty(entries, 0, {
      get: () => {
        query.set(to);
        return [Uint8Array.of(0x61, 0x62), int(2n)];
      },
    });
    entries.length = 1;
    return entries;
  };
  const rewritingClaim = (query: Uint8Array, to: readonly number[] = [0x61, 0x62]): unknown[] => [
    {
      get key(): Uint8Array {
        query.set(to);
        return Uint8Array.of(0x61, 0x62);
      },
      valueHash: v2,
    },
  ];
  const cb = (): Uint8Array => Uint8Array.of(0x63, 0x62);

  it("triePrefixVerify and trieRangeVerify", () => {
    expect(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61, 0x62), bad([[Uint8Array.of(0x61, 0x62), int(2n)]]), full), "the claim, unrewritten").toEqual({ ok: true, value: true });
    expect(trie.triePrefixVerify(T, variable, root, cb(), bad(rewritingPairs(cb())), full)).toEqual({ ok: true, value: false });
    const prefix = cb();
    expect(trie.triePrefixVerify(T, variable, root, prefix, bad(rewritingPairs(prefix)), full), "the prefix rewritten by the claim's getter").toEqual({ ok: true, value: false });
    const from = cb();
    expect(trie.trieRangeVerify(T, variable, root, from, null, bad(rewritingPairs(from)), full), "the lower bound rewritten").toEqual({ ok: true, value: false });
    // ["ab", "a") is empty ("a" sorts first); ["ab", 0xff) holds ab
    const until = Uint8Array.of(0x61);
    expect(trie.trieRangeVerify(T, variable, root, Uint8Array.of(0x61, 0x62), until, bad(rewritingPairs(until, [0xff])), full), "the upper bound rewritten").toEqual({ ok: true, value: false });
  });

  it("VerifiedTrieProof.listsPrefix and listsRange", () => {
    const v = verified();
    expect(v.listsPrefix(Uint8Array.of(0x61, 0x62), [{ key: Uint8Array.of(0x61, 0x62), valueHash: v2 }]), "the claim, unrewritten").toEqual({ ok: true, value: true });
    const prefix = cb();
    expect(v.listsPrefix(prefix, bad(rewritingClaim(prefix))), "the prefix rewritten by the claim's getter").toEqual({ ok: true, value: false });
    const from = cb();
    expect(v.listsRange(from, undefined, bad(rewritingClaim(from))), "the lower bound rewritten").toEqual({ ok: true, value: false });
    const until = Uint8Array.of(0x61);
    expect(v.listsRange(Uint8Array.of(0x61, 0x62), until, bad(rewritingClaim(until, [0xff]))), "the upper bound rewritten").toEqual({ ok: true, value: false });
  });
});

describe("an object or array a getter revokes while it is read is refused as a TypeMismatch, never thrown on", () => {
  const leaf = Uint8Array.of(0x61);
  const notIterable = mismatch("expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)");

  it("merkle paths and heads, trie entries, records, claims and iterator results", () => {
    expect(merkle.InclusionProof.of(0n, 2n, revokingAfterFirstRead([leaf]))).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(merkle.merkleVerify(tagged, revokingAfterFirstRead({ root: mroot, size: 3n }), int(2n), inclusionValue)).toEqual(mismatch("expected a bigint"));
    expect(trie.trieRoot(T, variable, revokingAfterFirstRead([[leaf, v1]]))).toEqual(notIterable);
    expect(trie.trieRoot(T, variable, bad([revokingAfterFirstRead([leaf, v1])]))).toEqual(mismatch("expected a [key, valueHash] array of Uint8Arrays at index 0"));
    const result = bad({ [Symbol.iterator]: () => ({ next: () => revokingAfterFirstRead({ done: false, value: [leaf, v1] }) }) });
    expect(trie.trieRoot(T, variable, result)).toEqual(mismatch("an iterator's next() gave a result that is not an object"));
    expect(trie.TrieProof.fromRecords(T, bad([revokingAfterFirstRead({ s: { n: 8n, b: leaf }, v: v1 })]))).toEqual(mismatch("expected a trie record of its types at index 0"));
    expect(trie.triePrefixVerify(T, variable, root, empty, bad(revokingAfterFirstRead([[leaf, int(1n)]])), full)).toEqual(mismatch("expected an array of [key, value] entries"));
    expect(trie.triePrefixVerify(T, variable, root, empty, bad([revokingAfterFirstRead([leaf, int(1n)])]), full)).toEqual(mismatch("expected a canonical value at index 0"));
    expect(verified().listsPrefix(empty, bad([revokingAfterFirstRead({ key: leaf, valueHash: v1 })]))).toEqual(mismatch("expected a TrieEntry of Uint8Arrays at index 0"));
  });

  it("Poseidon inputs and leaves", () => {
    const one = expectOk(poseidon.Fr.of(1n), "fr");
    expect(poseidon.hash(revokingAfterFirstRead([one, one]))).toEqual(mismatch("expected an Fr at index 0"));
    expect(poseidon.treeRoot(4, bad([revokingAfterFirstRead([1n, one])]))).toEqual(mismatch("expected a [bigint, Fr] array at index 0"));
  });

  it("versioned heads, proofs, deltas and a version key's results", () => {
    expect(vt.root(revokingAfterFirstRead({ ...finalHead }))).toEqual(mismatch("expected the head's roots to be Uint8Arrays"));
    expect(vt.verifyState(bytes(vectors.root), revokingAfterFirstRead({ head: finalHead, top: proof }))).toEqual(mismatch("expected the proof's top to be a TrieProof"));
    const key = versioned.VersionKey.long;
    const revokingResult = expectOk(
      versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: (v) => revokingAfterFirstRead({ ...key.encode(v) }), decode: (b) => key.decode(b) }),
      "profile",
    );
    expect(revokingResult.root(finalHead)).toEqual(mismatch("expected a version key's result to be a Result of its type"));
    const revokingError = expectOk(
      versioned.VersionedTrie.of<bigint>("SHA-256", { width: 8, encode: () => ({ ok: false, error: revokingAfterFirstRead({ kind: "TypeMismatch", reason: "r" }) }), decode: (b) => key.decode(b) }),
      "profile",
    );
    expect(revokingError.root(finalHead)).toEqual(mismatch("expected a version key's result to be a Result of its type"));
  });
});

describe("a versioned query is encoded before the claims or the proof are read: no getter can redirect it", () => {
  // an object-valued version key, as a caller may define: a version is { n }, encoded as VersionKey.long encodes n
  interface Ver {
    n: bigint;
  }
  const long = versioned.VersionKey.long;
  const objectKey: versioned.VersionKey<Ver> = {
    width: 8,
    encode: (v) => long.encode(v.n),
    decode: (b) => {
      const r = long.decode(b);
      return r.ok ? { ok: true, value: { n: r.value } } : r;
    },
  };
  const ot = expectOk(versioned.VersionedTrie.of<Ver>("SHA-256", objectKey), "an object-keyed profile");
  const root = bytes(vectors.root);
  const proofNamed = (name: string): versioned.VersionedTrieProof<Ver> => expectOk(ot.decodeProof(valueOf(vectors.proofs.find((p) => p.name === name)?.cbor ?? "")), name);
  /** `proof`, its head read through a getter that runs `then` first. */
  const redirecting = (proof: versioned.VersionedTrieProof<Ver>, then: () => void): versioned.VersionedTrieProof<Ver> =>
    bad({
      get head() {
        then();
        return proof.head;
      },
      top: proof.top,
      ...(proof.at === undefined ? {} : { at: proof.at }),
      ...(proof.history === undefined ? {} : { history: proof.history }),
      ...(proof.state === undefined ? {} : { state: proof.state }),
    });

  it("verifyVersions: a claim's getter cannot move the range's bounds", () => {
    const p = vectors.proofs.find((x) => x.name === "versions-2-until-300");
    if (p === undefined) throw new Error("the versions fixture");
    const proof = proofNamed("versions-2-until-300");
    const claims = (p.versions ?? []).map((v) => [{ n: v.version ?? -1n }, bytes(v.stateRoot ?? "")] as const);
    expect(expectOk(ot.verifyVersions(root, proof, { n: 2n }, { n: 300n }, claims), "the claim of [2, 300)")).toBe(true);
    // [3, 300) holds 255 and 256 only: a getter that moves `from` back to 2 must not make the [2, 255, 256] claim verify
    const from = { n: 3n };
    const moving: unknown[] = [...claims];
    Object.defineProperty(moving, 0, {
      get: () => {
        from.n = 2n;
        return claims[0];
      },
    });
    expect(ot.verifyVersions(root, proof, from, { n: 300n }, bad(moving)), "the query as given, [3, 300)").toEqual({ ok: true, value: false });
  });

  it("the head's version is encoded once: a version that revokes itself after its first read gives a verdict, never a throw", () => {
    for (const name of ["current-keys", "head"]) {
      const proof = proofNamed(name);
      const once = (): versioned.VersionedTrieProof<Ver> =>
        bad({ ...proof, head: { ...proof.head, version: revokingAfterFirstRead({ ...proof.head.version }) } });
      expect(ot.verifyHead(root, once()).ok, `${name}: verifyHead`).toBe(name === "head");
      if (name === "current-keys") expect(ot.verifyState(root, once()).ok, `${name}: verifyState`).toBe(true);
    }
  });

  it("a snapshot's versions are encoded once, as they are fed: a version that revokes itself after its first read restores", () => {
    const trustedHead = expectOk(ot.decodeHead(valueOf(vectors.head.cbor)), "the head");
    let reader = expectOk(versioned.SnapshotReader.start(ot, trustedHead), "reader");
    for (const c of vectors.snapshot.chunks.map(bytes)) reader = expectOk(reader.feedBytes(c), "fed");
    const snapshot = expectOk(reader.finish(), "the snapshot");
    const once = { ...snapshot, history: snapshot.history.map(([v, s]) => [revokingAfterFirstRead({ ...v }), s] as const) };
    const restored = expectOk(versioned.Replica.fromSnapshot(ot, trustedHead, once), "restores, every version read once");
    expect(restored.history().length, "every version kept").toBe(snapshot.history.length);
  });

  it("verifyVersions: a later claim's getter cannot rewrite an earlier claim's version", () => {
    const p = vectors.proofs.find((x) => x.name === "versions-2-until-300");
    if (p === undefined) throw new Error("the versions fixture");
    const proof = proofNamed("versions-2-until-300");
    const roots = (p.versions ?? []).map((v) => bytes(v.stateRoot ?? ""));
    // [2, 300) holds 2, 255 and 256: a claim whose first version is 3 is false
    const first = { n: 3n };
    const claim = (): unknown[] => [[first, roots[0]], [{ n: 255n }, roots[1]], [{ n: 256n }, roots[2]]];
    expect(ot.verifyVersions(root, proof, { n: 2n }, { n: 300n }, bad(claim())), "the claim as read").toEqual({ ok: true, value: false });
    const rewriting = claim();
    const second: unknown[] = [{ n: 255n }];
    Object.defineProperty(second, 1, {
      get: () => {
        first.n = 2n;
        return roots[1];
      },
    });
    rewriting[1] = second;
    expect(ot.verifyVersions(root, proof, { n: 2n }, { n: 300n }, bad(rewriting)), "version 3 was read, and encoded, first").toEqual({ ok: true, value: false });
  });

  it("verifyVersion, verifyHistoricalKey and verifyAsOf: a proof's getter cannot change the version asked about", () => {
    const v255 = proofNamed("version-255");
    expect(ot.verifyVersion(root, v255, { n: 255n }).ok, "version 255, asked as 255").toBe(true);
    const u = { n: 3n };
    expect(expectErr(ot.verifyVersion(root, redirecting(v255, () => (u.n = 255n)), u), "asked as 3").kind).toBe("Malformed");
    const k1 = proofNamed("key-at-version-1");
    expect(ot.verifyHistoricalKey(root, k1, { n: 1n }).ok, "the state at 1").toBe(true);
    const h = { n: 3n };
    expect(expectErr(ot.verifyHistoricalKey(root, redirecting(k1, () => (h.n = 1n)), h), "asked as 3").kind).toBe("Malformed");
    const asOf = proofNamed("as-of-299");
    expect(ot.verifyAsOf(root, asOf, { n: 299n }).ok, "as of 299").toBe(true);
    const t = { n: 0n };
    expect(expectErr(ot.verifyAsOf(root, redirecting(asOf, () => (t.n = 299n)), t), "asked as of 0").kind).toBe("Malformed");
  });
});

describe("a hashing profile, mode or namespace built by calling its constructor from outside computes nothing", () => {
  const forge = (c: unknown, args: readonly unknown[]): unknown => Reflect.construct(c as new (...a: unknown[]) => object, [Symbol("dilf4s-proofs"), ...args]);

  it("Merkle and trie hashing: every method refuses, and the reported algorithm is never the caller's", () => {
    const rfc = forge(merkle.Rfc6962MerkleHashing, ["bogus"]) as merkle.Rfc6962MerkleHashing;
    const notAProfile = mismatch("expected a TaggedMerkleHashing or an Rfc6962MerkleHashing");
    expect(rfc.leaf(empty), "rfc leaf").toEqual(notAProfile);
    expect(rfc.leafWeight(empty), "rfc leafWeight").toEqual(notAProfile);
    expect(rfc.node(empty, empty), "rfc node").toEqual(notAProfile);
    expect([rfc.algorithm, rfc.size], "rfc reports its own default").toEqual(["SHA-256", 32]);
    const tagged512 = forge(merkle.TaggedMerkleHashing, ["SHA-512"]) as merkle.TaggedMerkleHashing;
    expect(tagged512.leaf(int(1n)), "tagged leaf").toEqual(notAProfile);
    expect(tagged512.leafWeight(int(1n)), "tagged leafWeight").toEqual(notAProfile);
    expect(tagged512.node(empty, empty), "tagged node").toEqual(notAProfile);
    expect([tagged512.algorithm, tagged512.size], "never SHA-512 with 32-byte hashes").toEqual(["SHA-256", 32]);
    const t512 = forge(trie.TrieHashing, ["SHA-512"]) as trie.TrieHashing;
    const notAHashing = mismatch("expected a TrieHashing");
    const segment = expectOk(trie.Segment.of(0n, empty), "segment");
    expect(t512.valueHash(int(1n)), "valueHash").toEqual(notAHashing);
    expect(t512.bytesHash(empty), "bytesHash").toEqual(notAHashing);
    expect(t512.leafHash(segment, v1), "leafHash").toEqual(notAHashing);
    expect(t512.branchHash(segment, v1, undefined, undefined), "branchHash").toEqual(notAHashing);
    expect([t512.algorithm, t512.size], "never SHA-512 with 32-byte hashes").toEqual(["SHA-256", 32]);
  });

  it("modes, keys, namespaces, proofs, profiles and replicas keep none of the caller's arguments", () => {
    const mode = forge(trie.KeyMode, ["variable", Symbol("bytes")]) as trie.KeyMode;
    expect(mode.checkKey(empty), "checkKey").toEqual(mismatch("expected a KeyMode"));
    expect([mode.kind, mode.bytes, mode.toString()], "a mode's defaults").toEqual(["fixed", 0, "fixed(0)"]);
    const ns = forge(versioned.StateNamespace, [Symbol("ns")]) as versioned.StateNamespace;
    expect(ns.contains(key("fiber/1")), "contains").toEqual(mismatch("expected a StateNamespace"));
    expect([ns.value, ns.toString()], "a namespace's defaults").toEqual(["", ""]);
    const sk = forge(versioned.StateKey, [Symbol("key")]) as versioned.StateKey;
    expect(sk.value, "a key's default").toBe("");
    const inclusion = forge(merkle.InclusionProof, [Symbol("index"), Symbol("size"), [v1]]) as merkle.InclusionProof;
    expect([inclusion.index, inclusion.size, inclusion.path], "an inclusion proof's defaults").toEqual([0n, 0n, []]);
    const consistency = forge(merkle.ConsistencyProof, [Symbol("old"), Symbol("new"), [v1]]) as merkle.ConsistencyProof;
    expect([consistency.oldSize, consistency.newSize], "a consistency proof's defaults").toEqual([0n, 0n]);
    const pm = forge(poseidon.PoseidonMerkleProof, [Symbol("position"), []]) as poseidon.PoseidonMerkleProof;
    expect(pm.position, "a Poseidon proof's default").toBe(0n);
    // the modes are never read: a forged profile with none is built without a throw
    const profile = forge(versioned.VersionedTrie, [Symbol("hashing"), Symbol("key"), null]) as versioned.VersionedTrie<bigint>;
    expect([profile.hashing.algorithm, profile.versionKey], "a profile's defaults").toEqual(["SHA-256", versioned.VersionKey.long]);
    const replica = forge(versioned.Replica, [vt, revokingAfterFirstRead({}), revokingAfterFirstRead({}), revokingAfterFirstRead({})]) as versioned.Replica<bigint>;
    expect([replica.head, replica.size, replica.entries(), replica.history()], "a replica answers as an empty one").toEqual([null, 0, [], []]);
  });
});

describe("an iterator's next is called directly, never through bind", () => {
  it("a next Proxy that revokes itself as its length or name is read is still called once, and refused only when revoked", () => {
    for (const property of ["length", "name"]) {
      const { proxy: next, revoke } = Proxy.revocable(() => ({ done: true, value: undefined }), {
        get: (t, name) => {
          const v: unknown = Reflect.get(t, name);
          if (name === property) revoke();
          return v;
        },
        getOwnPropertyDescriptor: (t, name) => {
          const d = Reflect.getOwnPropertyDescriptor(t, name);
          if (name === property) revoke();
          return d;
        },
      });
      // nothing reads its length or name, so it is never revoked, and the empty iterable gives the empty trie's root
      expect(trie.trieRoot(T, variable, bad({ [Symbol.iterator]: () => ({ next }) })), property).toEqual({ ok: true, value: T.empty });
    }
  });
});

describe("a snapshot chunk's size bound is checked before any of its items is read", () => {
  it("state and history chunks past MAX_CHUNK_ENTRIES, or empty, read no item", { timeout: 5_000 }, () => {
    const reader = expectOk(versioned.SnapshotReader.start(vt, finalHead), "reader");
    for (const length of [versioned.MAX_CHUNK_ENTRIES + 1, 2 ** 32 - 1, 0]) {
      let reads = 0;
      const items = new Proxy([], {
        get: (t, name) => {
          if (name === "length") return length;
          if (typeof name === "string" && /^[0-9]+$/.test(name)) reads += 1;
          return Reflect.get(t, name) as unknown;
        },
      });
      for (const chunk of [{ kind: "state", entries: items }, { kind: "history", versions: items }]) {
        expect(expectErr(reader.feed(bad(chunk)), `${chunk.kind} ${length.toString()}`).kind).toBe("Malformed");
      }
      expect(reads, `no item read at length ${length.toString()}`).toBe(0);
    }
  });
});

describe("a digest of the wrong size is refused without being copied", () => {
  // measured as the bytes held in ArrayBuffers: a 64 MiB copy shows as 64 MiB more, a view as nothing (a collection
  // between the reads can only lower the count, so a copy is never missed by an increase that is not there)
  const held = (): number => process.memoryUsage().arrayBuffers;
  const big = new Uint8Array(64 << 20);

  it("merkleRoot's leaf hashes, and a trie record's stub and value digests and segment bytes", () => {
    const before = held();
    expect(expectErr(merkle.merkleRoot(tagged, [big, big, big]), "a 64 MiB leaf hash").kind, "merkleRoot").toBe("Encoding");
    expect(expectErr(trie.TrieProof.fromRecords(T, bad([{ h: big }])), "a 64 MiB stub").kind, "a stub").toBe("Encoding");
    expect(expectErr(trie.TrieProof.fromRecords(T, bad([{ s: { n: 8n, b: Uint8Array.of(0x61) }, v: big }])), "a 64 MiB value").kind, "a leaf's value").toBe("Encoding");
    expect(expectErr(trie.TrieProof.fromRecords(T, bad([{ s: { n: 8n, b: big }, v: v1 }])), "a 64 MiB segment").kind, "a segment").toBe("Encoding");
    expect(held() - before, "no copy of the 64 MiB buffer was made").toBeLessThan(1 << 20);
  });
});

describe("a lookup reads one snapshot of its key: memory another thread writes cannot prove a present key absent", () => {
  // fixed(1) holding 0x00 and 0xff: a root branch (no segment, both children), then each leaf's remaining seven bits
  const fixed1 = expectOk(trie.KeyMode.fixed(1), "fixed(1)");
  const both = expectOk(trie.trieRoot(T, fixed1, [[Uint8Array.of(0x00), v1], [Uint8Array.of(0xff), v2]]), "root");
  const records: Value = expectOk(
    models.trie.TrieProof.encode([
      { s: { n: 0n, b: new Uint8Array(0) }, c: "both" },
      { s: { n: 7n, b: Uint8Array.of(0x00) }, v: v1 },
      { s: { n: 7n, b: Uint8Array.of(0xfe) }, v: v2 },
    ]),
    "records",
  );

  it("trieVerify and VerifiedTrieProof.lookup, while a worker alternates a shared key byte between the two keys", { timeout: 30_000 }, async () => {
    const v = expectOk(trie.verify(T, fixed1, both, expectOk(trie.TrieProof.decode(T, records), "proof")), "verifies");
    if (v === null) throw new Error("the proof's root is the trie's");
    expect(expectOk(v.lookup(Uint8Array.of(0xff)), "0xff").kind, "both keys are present").toBe("present");
    const shared = new SharedArrayBuffer(1);
    const worker = new Worker("const a = new Uint8Array(require('node:worker_threads').workerData); for (;;) { a[0] = 0x00; a[0] = 0xff; }", { eval: true, workerData: shared });
    await once(worker, "online");
    const key = new Uint8Array(shared);
    let absent = 0;
    let absentLookups = 0;
    for (let i = 0; i < 2000; i++) {
      if (expectOk(trie.trieVerify(T, fixed1, both, key, null, records), "a verdict")) absent += 1;
      if (expectOk(v.lookup(key), "a lookup").kind !== "present") absentLookups += 1;
    }
    await worker.terminate();
    expect(absent, "trieVerify never proves either present key absent").toBe(0);
    expect(absentLookups, "lookup never answers absent for either present key").toBe(0);
  });
});

describe("trieRoot checks each entry as it reads it: the first invalid one stops the reading, uncopied", () => {
  it("a key invalid for the mode, or a digest of the wrong size, closes the iterator at once", () => {
    const fixed1 = expectOk(trie.KeyMode.fixed(1), "fixed(1)");
    for (const [name, first, kind] of [
      ["a two-byte key", [Uint8Array.of(1, 2), v1], "InvalidKey"],
      ["a short digest", [Uint8Array.of(1), Uint8Array.of(1)], "Encoding"],
    ] as const) {
      let reads = 0;
      let closed = 0;
      const entries = {
        [Symbol.iterator]: () => ({
          next: () => ((reads += 1), { done: false, value: reads === 1 ? first : [Uint8Array.of(reads & 0xff), v1] }),
          return: () => ((closed += 1), { done: true }),
        }),
      };
      expect(expectErr(trie.trieRoot(T, fixed1, bad(entries)), name).kind, name).toBe(kind);
      expect([reads, closed], `${name}: one entry read, the iterator closed`).toEqual([1, 1]);
    }
    const big = new Uint8Array(64 << 20);
    const before = process.memoryUsage().arrayBuffers;
    expect(expectErr(trie.trieRoot(T, fixed1, [[big, v1]]), "a 64 MiB key").kind).toBe("InvalidKey");
    expect(expectErr(trie.trieRoot(T, fixed1, [[Uint8Array.of(1), big]]), "a 64 MiB digest").kind).toBe("Encoding");
    expect(process.memoryUsage().arrayBuffers - before, "neither was copied").toBeLessThan(1 << 20);
  });
});
