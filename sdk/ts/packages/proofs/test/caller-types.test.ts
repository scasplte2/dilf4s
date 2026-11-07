import { runInNewContext } from "node:vm";

import { describe, expect, it } from "vitest";

import { models, Value } from "@kd5ujc/dilf4s-core";

import { merkle, poseidon, trie, versioned } from "../src/index.js";
import { expectErr, expectOk, int, loadVectors, valueOf } from "./vectors.js";

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
      expect(vt.verifyHead(bad(x), bad({})), label(x)).toEqual(notBytes);
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
