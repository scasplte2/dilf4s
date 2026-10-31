import { describe, expect, it } from "vitest";

import { BytesValue, models, toHex, Value, type Result } from "@kd5ujc/dilf4s-core";

import { trie } from "../src/index.js";
import { bytes, expectErr, expectOk, loadVectors, valueOf } from "./vectors.js";

const vectors = loadVectors("trie_v1.json", models.trieVectors.TrieVectors);
const T = trie.TrieHashing.of("SHA-256");

const modeOf = (m: models.trieVectors.TrieMode): trie.KeyMode =>
  expectOk("fixed" in m ? trie.KeyMode.fixed(Number(m.fixed)) : trie.KeyMode.variable(Number(m.variable)), "mode");

const listingHex = (l: trie.Listing): unknown => (l.kind === "listed" ? l.entries.map((e) => ({ key: toHex(e.key), valueHash: toHex(e.valueHash) })) : "notCovered");

const lookupHex = (l: trie.Lookup): unknown => (l.kind === "present" ? { present: toHex(l.valueHash) } : l.kind);

const verifiedOf = (mode: trie.KeyMode, root: Uint8Array, proof: trie.TrieProof): trie.VerifiedTrieProof => {
  const v = expectOk(trie.verify(T, mode, root, proof), "verifies");
  if (v === null) throw new Error("root mismatch");
  return v;
};

describe("trie_v1.json", () => {
  it("the empty root", () => {
    expect(toHex(T.empty)).toBe(vectors.emptyRoot);
  });

  it("node hashes", () => {
    const { leaf, branch } = vectors.nodeHashes;
    const ls = expectOk(trie.Segment.of(leaf.segment.bits, bytes(leaf.segment.bytes)), "segment");
    expect(toHex(T.leafHash(ls, bytes(leaf.valueHash)))).toBe(leaf.hash);
    const bs = expectOk(trie.Segment.of(branch.segment.bits, bytes(branch.segment.bytes)), "segment");
    expect(toHex(T.branchHash(bs, bytes(branch.valueHash), bytes(branch.left), undefined))).toBe(branch.hash);
  });

  describe.each(vectors.fixtures.map((f) => [f.name, f] as const))("%s", (_, f) => {
    const mode = modeOf(f.mode);
    const root = bytes(f.root);
    const proof = expectOk(trie.TrieProof.decode(T, valueOf(f.proof.cbor)), "decodes");

    it("entries hash as the reference does, and build the root", () => {
      const built = expectOk(trie.trieRoot(T, mode, f.entries.map((e) => [bytes(e.key), bytes(e.valueHash)] as const)), "builds");
      expect(toHex(built)).toBe(f.root);
      const reversed = expectOk(trie.trieRoot(T, mode, [...f.entries].reverse().map((e) => [bytes(e.key), bytes(e.valueHash)] as const)), "builds");
      expect(toHex(reversed), "independent of order").toBe(f.root);
    });

    it("the proof re-encodes to the same records", () => {
      const encoded = expectOk(proof.encode(), "encodes");
      expect(encoded).toEqual(valueOf(f.proof.cbor));
    });

    it("lookups", () => {
      const v = verifiedOf(mode, root, proof);
      for (const l of f.proof.lookups) expect(lookupHex(expectOk(v.lookup(bytes(l.key)), "lookup")), l.key).toEqual(l.result);
    });

    it("prefix listings", () => {
      const v = verifiedOf(mode, root, proof);
      for (const p of f.proof.prefixListings) expect(listingHex(v.prefix(bytes(p.prefix))), p.prefix).toEqual(p.entries);
    });

    it("range listings", () => {
      const v = verifiedOf(mode, root, proof);
      for (const r of f.proof.rangeListings) {
        const expected = r.result === "notCovered" ? "notCovered" : r.result.entries;
        expect(listingHex(v.range(bytes(r.from), r.until === undefined ? undefined : bytes(r.until))), `[${r.from}, ${r.until ?? ""})`).toEqual(expected);
      }
    });

    it("every range the proof was made for is covered", () => {
      const v = verifiedOf(mode, root, proof);
      for (const r of f.proof.ranges) expect(v.range(bytes(r.from), r.until === undefined ? undefined : bytes(r.until)).kind, `[${r.from}, ${r.until ?? ""})`).toBe("listed");
    });

    it("trie_range_verify: not covered is an error; a covered range with a wrong claim is false", () => {
      // the fixtures hash raw value bytes (Trie.putValue), which no JLVM claim can name, so only wrong claims are checked here
      for (const r of f.proof.rangeListings) {
        const until = r.until === undefined ? null : bytes(r.until);
        const verdict = trie.trieRangeVerify(T, mode, root, bytes(r.from), until, [[bytes(f.entries[0]?.key ?? ""), Value.int(0n)]], valueOf(f.proof.cbor));
        if (r.result === "notCovered") expect(expectErr(verdict, "not covered").kind).toBe("NotCovered");
        else expect(expectOk(verdict, "verdict")).toBe(false);
      }
    });

    it("the proof does not verify against another root", () => {
      const other = Uint8Array.from(root, (b, i) => (i === 0 ? b ^ 1 : b));
      expect(expectOk(trie.verify(T, mode, other, proof), "verdict")).toBeNull();
      const mismatch = expectErr(trie.check(T, mode, other, proof), "mismatch");
      expect(mismatch.kind).toBe("RootMismatch");
    });
  });
});

// a trie of committed values, as the JLVM claims them
const variable = expectOk(trie.KeyMode.variable(8), "mode");
const fixed2 = expectOk(trie.KeyMode.fixed(2), "mode");
const text = (s: string): Value => expectOk(Value.text(s), "text");
const committed: readonly (readonly [Uint8Array, Value])[] = [
  [Uint8Array.of(0x61), text("a")],
  [Uint8Array.of(0x61, 0x62), BytesValue.of(Uint8Array.of(0x61, 0x61))],
  [Uint8Array.of(0x62), Value.int(7n)],
  [Uint8Array.of(0x62, 0x00, 0x01), Value.bool(true)],
];
const committedRoot = expectOk(trie.trieRoot(T, variable, committed.map(([k, v]) => [k, T.valueHash(v)] as const)), "root");

/**
 * A full proof of a small trie, built by hand from its records: a variable(8) trie of "a" -> 1, "ab" -> 2, with root
 * Branch(segment "a" (8 bits), value, right: Leaf("ab" bits after the branch bit)).
 */
function records(value: readonly models.trie.TrieRecord[]): Value {
  return expectOk(models.trie.TrieProof.encode(value), "records encode");
}

describe("JLVM trie operations", () => {
  // the whole trie as a proof: a prefix listing of everything needs it fully expanded
  const segment = (n: bigint, b: number[]): models.trie.TrieSegment => ({ n, b: Uint8Array.from(b) });
  const v1 = T.valueHash(Value.int(1n));
  const v2 = T.valueHash(Value.int(2n));
  // "a" = 0x61 = 0110 0001, "ab" = 0x61 0x62: after the branch at bit 8 with bit 0 (0x62 = 0110 0010), 7 bits remain: 110 0010
  const full = records([
    { s: segment(8n, [0x61]), v: v1, c: "left" },
    { s: segment(7n, [0xc4]), v: v2 },
  ]);
  const hashOf = (r: Result<Uint8Array, trie.TrieError>): Uint8Array => expectOk(r, "root");
  const root = hashOf(trie.trieRoot(T, variable, [[Uint8Array.of(0x61), v1], [Uint8Array.of(0x61, 0x62), v2]]));

  it("a hand-built proof decodes and verifies", () => {
    const p = expectOk(trie.TrieProof.decode(T, full), "decodes");
    const v = verifiedOf(variable, root, p);
    expect(lookupHex(expectOk(v.lookup(Uint8Array.of(0x61, 0x62)), "lookup"))).toEqual({ present: toHex(v2) });
    expect(lookupHex(expectOk(v.lookup(Uint8Array.of(0x61, 0x63)), "lookup"))).toBe("absent");
    expect(listingHex(v.prefix(new Uint8Array(0)))).toEqual([
      { key: "61", valueHash: toHex(v1) },
      { key: "6162", valueHash: toHex(v2) },
    ]);
    expect(listingHex(v.range(Uint8Array.of(0x61, 0x00)))).toEqual([{ key: "6162", valueHash: toHex(v2) }]);
    expect(listingHex(v.range(new Uint8Array(0), Uint8Array.of(0x61, 0x62)))).toEqual([{ key: "61", valueHash: toHex(v1) }]);
  });

  it("trie_verify: present, absent, wrong value, null value, wrong root", () => {
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), Value.int(1n), full), "verdict")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), Value.int(2n), full), "verdict")).toBe(false);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), null, full), "verdict")).toBe(false);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x62), null, full), "verdict")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x62), Value.null, full), "verdict")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x62), Value.int(1n), full), "verdict")).toBe(false);
    expect(expectOk(trie.trieVerify(T, variable, T.empty, Uint8Array.of(0x61), Value.int(1n), full), "verdict")).toBe(false);
  });

  it("bytes and the text whose CBOR they are commit apart", () => {
    const k = Uint8Array.of(0x6b);
    const r = hashOf(trie.trieRoot(T, variable, [[k, T.valueHash(text("a"))]]));
    const proof = records([{ s: segment(8n, [0x6b]), v: T.valueHash(text("a")) }]);
    expect(expectOk(trie.trieVerify(T, variable, r, k, text("a"), proof), "verdict")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, r, k, BytesValue.of(Uint8Array.of(0x61, 0x61)), proof), "verdict")).toBe(false);
  });

  it("not covered is an error, never false", () => {
    const stubbed = records([{ s: segment(8n, [0x61]), v: v1, c: "left" }, { h: T.leafHash(expectOk(trie.Segment.of(7n, Uint8Array.of(0xc4)), "segment"), v2) }]);
    expect(expectErr(trie.trieVerify(T, variable, root, Uint8Array.of(0x61, 0x62), Value.int(2n), stubbed), "not covered").kind).toBe("NotCovered");
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), Value.int(1n), stubbed), "verdict")).toBe(true);
    expect(expectErr(trie.triePrefixVerify(T, variable, root, new Uint8Array(0), [], stubbed), "not covered").kind).toBe("NotCovered");
    expect(expectErr(trie.trieRangeVerify(T, variable, root, Uint8Array.of(0x61, 0x00), null, [], stubbed), "not covered").kind).toBe("NotCovered");
    // a range that avoids the stub is covered
    expect(expectOk(trie.trieRangeVerify(T, variable, root, new Uint8Array(0), Uint8Array.of(0x61), [], stubbed), "verdict")).toBe(true);
    // [0x00, 0x6100) reaches the stub: at the bit level, keys between 0x61 and 0x6100 lie below it
    expect(expectErr(trie.trieRangeVerify(T, variable, root, new Uint8Array(0), Uint8Array.of(0x61, 0x00), [[Uint8Array.of(0x61), Value.int(1n)]], stubbed), "not covered").kind).toBe("NotCovered");
  });

  it("prefix and range verification compare the whole listing", () => {
    const all: readonly trie.ClaimedEntry[] = [
      [Uint8Array.of(0x61), Value.int(1n)],
      [Uint8Array.of(0x61, 0x62), Value.int(2n)],
    ];
    expect(expectOk(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61), all, full), "verdict")).toBe(true);
    expect(expectOk(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61), all.slice(1), full), "verdict")).toBe(false);
    expect(expectOk(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61), [...all].reverse(), full), "verdict")).toBe(false);
    expect(expectOk(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x62), [], full), "verdict")).toBe(true);
    expect(expectOk(trie.trieRangeVerify(T, variable, root, new Uint8Array(0), null, all, full), "verdict")).toBe(true);
    expect(expectOk(trie.trieRangeVerify(T, variable, root, Uint8Array.of(0x61, 0x62), null, all.slice(1), full), "verdict")).toBe(true);
    expect(expectOk(trie.trieRangeVerify(T, variable, root, Uint8Array.of(0x61, 0x62), Uint8Array.of(0x61, 0x62), [], full), "verdict")).toBe(true);
    expect(expectErr(trie.triePrefixVerify(T, variable, root, Uint8Array.of(0x61), [[Uint8Array.of(0x61), Value.null]], full), "null entry").kind).toBe("Encoding");
  });

  it("a key invalid for the mode is InvalidKey", () => {
    expect(expectErr(trie.trieVerify(T, fixed2, root, Uint8Array.of(0x61), null, full), "invalid key").kind).toBe("InvalidKey");
    expect(expectErr(trie.trieVerify(T, variable, root, new Uint8Array(9), null, full), "invalid key").kind).toBe("InvalidKey");
    expect(expectErr(trie.KeyMode.fixed(0), "mode").kind).toBe("InvalidKey");
    expect(expectErr(trie.KeyMode.fixed(1025), "mode").kind).toBe("InvalidKey");
    expect(expectErr(trie.KeyMode.variable(65536), "mode").kind).toBe("InvalidKey");
  });

  it("a structure the mode forbids is InvalidProof, an error", () => {
    // in fixed(2), a branch carrying a value, and a leaf ending at the wrong depth
    expect(expectErr(trie.trieVerify(T, fixed2, root, Uint8Array.of(0x61, 0x62), null, full), "branch value").kind).toBe("InvalidProof");
    const shortLeaf = records([{ s: segment(8n, [0x61]), v: v1 }]);
    expect(expectErr(trie.trieVerify(T, fixed2, root, Uint8Array.of(0x61, 0x62), null, shortLeaf), "short leaf").kind).toBe("InvalidProof");
    // in variable(1), a leaf below the key-length bound
    expect(expectErr(trie.trieVerify(T, expectOk(trie.KeyMode.variable(1), "mode"), root, Uint8Array.of(0x61), null, full), "too deep").kind).toBe("InvalidProof");
    // a leaf ending off a byte boundary in variable mode
    const oddLeaf = records([{ s: segment(7n, [0x60]), v: v1 }]);
    expect(expectErr(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), null, oddLeaf), "odd leaf").kind).toBe("InvalidProof");
  });

  it("malformed records are Encoding errors", () => {
    const decode = (rs: readonly models.trie.TrieRecord[]): { kind: string; reason: string } => {
      const e = expectErr(trie.TrieProof.decode(T, records(rs)), "malformed");
      return { kind: e.kind, reason: "reason" in e ? e.reason : "" };
    };
    expect(decode([{ s: segment(8n, [0x61]), v: v1, c: "both" }, { h: v1 }]).reason).toMatch(/unfinished/);
    expect(decode([{ h: v1 }, { h: v1 }]).reason).toMatch(/trailing/);
    expect(decode([{ s: segment(8n, [0x61]), c: "left" }, { h: v1 }]).reason).toMatch(/branch needs/);
    expect(decode([{ s: segment(7n, [0x61]), v: v1 }]).reason).toMatch(/padding/);
    expect(decode([{ s: segment(9n, [0x61]), v: v1 }]).reason).toMatch(/bits in/);
    expect(decode([{ s: segment(8n, [0x61, 0x00]), v: v1 }]).reason).toMatch(/bits in/);
    // bigint division truncates toward zero, so (-8 + 7) / 8 = 0 bytes: a negative length must be refused outright
    expect(expectErr(trie.Segment.of(-8n, new Uint8Array(0)), "negative segment").kind).toBe("Encoding");
    expect(expectErr(trie.TrieProof.fromRecords(T, [{ s: segment(-8n, []), v: v1 }]), "negative record").kind).toBe("Encoding");
    expect(decode([{ h: v1.slice(1) }]).reason).toMatch(/digest/);
    expect(expectErr(trie.TrieProof.decode(T, Value.null), "not an array").kind).toBe("Encoding");
    expect(expectErr(trie.trieVerify(T, variable, root.slice(1), Uint8Array.of(0x61), null, full), "short root").kind).toBe("Encoding");
  });

  it("the empty proof proves the empty trie", () => {
    const empty = records([]);
    expect(expectOk(trie.trieVerify(T, variable, T.empty, Uint8Array.of(0x61), null, empty), "verdict")).toBe(true);
    expect(expectOk(trie.trieVerify(T, variable, root, Uint8Array.of(0x61), null, empty), "verdict")).toBe(false);
    expect(expectOk(trie.trieRangeVerify(T, variable, T.empty, new Uint8Array(0), null, [], empty), "verdict")).toBe(true);
  });

  it("a 5000-level proof decodes, verifies and lists without recursion", () => {
    const mode = expectOk(trie.KeyMode.variable(65535), "mode");
    const n = 5000;
    // keys of 1..n+1 zero bytes: a branch per key ending, each with a value and one left child, then a leaf
    const recs: models.trie.TrieRecord[] = [{ s: segment(8n, [0]), v: v1, c: "left" }];
    for (let i = 1; i < n; i++) recs.push({ s: segment(7n, [0]), v: v1, c: "left" });
    recs.push({ s: segment(7n, [0]), v: v2 });
    const r = expectOk(trie.trieRoot(T, mode, Array.from({ length: n + 1 }, (_, i) => [new Uint8Array(i + 1), i === n ? v2 : v1] as const)), "root");
    const p = expectOk(trie.TrieProof.decode(T, records(recs)), "decodes");
    const v = verifiedOf(mode, r, p);
    expect(lookupHex(expectOk(v.lookup(new Uint8Array(n + 1)), "lookup"))).toEqual({ present: toHex(v2) });
    const all = v.prefix(new Uint8Array(0));
    expect(all.kind === "listed" ? all.entries.length : -1).toBe(n + 1);
    const tail = v.range(new Uint8Array(n));
    expect(tail.kind === "listed" ? tail.entries.length : -1).toBe(2);
    expect(expectOk(p.encode(), "encodes")).toEqual(records(recs));
  });

  it("committed values build a root, and a deep chain verifies without recursion", () => {
    expect(committedRoot).toHaveLength(32);
    // 65535-byte keys sharing all but the last bit: a single branch with a 524279-bit segment
    const mode = expectOk(trie.KeyMode.variable(65535), "mode");
    const a = new Uint8Array(65535);
    const b = Uint8Array.from(a);
    b[65534] = 1;
    const r = expectOk(trie.trieRoot(T, mode, [[a, v1], [b, v2]]), "root");
    expect(r).toHaveLength(32);
    // a deep unary chain: keys 0x00^i for i = 0..2000, each a prefix of the next
    const chain = Array.from({ length: 2001 }, (_, i) => [new Uint8Array(i), v1] as const);
    const deep = expectOk(trie.trieRoot(T, mode, chain), "root");
    expect(deep).toHaveLength(32);
  });
});

describe("trie_v1.json malformed proofs", () => {
  it("decoding refuses the mode-free rules (Encoding); verification the mode's (InvalidProof)", () => {
    expect(vectors.malformed.length).toBeGreaterThan(10);
    for (const c of vectors.malformed) {
      const p = trie.TrieProof.decode(T, valueOf(c.cbor));
      const r = p.ok ? trie.verify(T, modeOf(c.mode), bytes(c.root), p.value) : p;
      expect(r.ok ? "accepted" : r.error.kind, c.name).toBe(c.error);
    }
  });
});
