import { createHash } from "node:crypto";

import { afterEach, describe, expect, it, vi } from "vitest";

import { models, Value } from "@kd5ujc/dilf4s-core";

import { poseidon } from "../src/index.js";
import { FULL_ROUNDS, WIDTH2, WIDTH3, WIDTH4, WIDTH5, type PoseidonParams } from "../src/poseidon-constants.js";
import { expectErr, expectOk, loadVectors, readRepoFile } from "./vectors.js";

const vectors = loadVectors("poseidon_v1.json", models.poseidonVectors.PoseidonVectors);
const fr = (d: string | bigint): poseidon.Fr => expectOk(poseidon.Fr.of(BigInt(d)), "field element");
const widths: readonly PoseidonParams[] = [WIDTH2, WIDTH3, WIDTH4, WIDTH5];

describe("poseidon_v1.json", () => {
  it.each(vectors.poseidon.map((v) => [v.inputs.join(","), v] as const))("poseidon(%s)", (_, v) => {
    expect(expectOk(poseidon.hash(v.inputs.map(fr)), "hashes").toString()).toBe(v.output);
  });

  it.each(vectors.builds.map((b) => [b.entries.map((e) => `${e.position}:${e.leaf}`).join(","), b] as const))("a tree built from the entries [%s] in order", (_, b) => {
    const entries = b.entries.map((e) => [BigInt(e.position), fr(e.leaf)] as const);
    expect(expectOk(poseidon.treeRoot(Number(b.depth), entries), "root").toString()).toBe(b.root);
  });

  const m = vectors.merkle;
  const depth = Number(m.depth);
  const root = fr(m.root);
  const leaves = new Map([...m.leaves].map(([p, l]) => [BigInt(p), fr(l)] as const));

  it("the tree root from its leaves", () => {
    expect(expectOk(poseidon.treeRoot(depth, leaves), "root").toString()).toBe(m.root);
  });

  it.each(m.proofs.map((p) => [p.position, p] as const))("the proof of position %s verifies for its leaf and no other", (_, p) => {
    const proof = expectOk(poseidon.PoseidonMerkleProof.of(BigInt(p.position), p.siblings.map(fr)), "proof");
    expect(proof.depth).toBe(depth);
    const leaf = leaves.get(BigInt(p.position)) ?? poseidon.Fr.zero;
    expect(poseidon.verifyInclusion(leaf, proof, root)).toBe(true);
    expect(poseidon.verifyInclusion(poseidon.Fr.reduce(leaf.value + 1n), proof, root), "a tampered leaf").toBe(false);
    expect(poseidon.verifyAbsence(proof, root), "absence holds exactly when the leaf is empty").toBe(leaf.equals(poseidon.Fr.zero));
    const siblings = [...proof.siblings];
    siblings[3] = poseidon.Fr.reduce((siblings[3]?.value ?? 0n) + 1n);
    expect(poseidon.verifyInclusion(leaf, expectOk(poseidon.PoseidonMerkleProof.of(proof.position, siblings), "proof"), root), "a tampered sibling").toBe(false);
    const decoded = expectOk(poseidon.PoseidonMerkleProof.decode(expectOk(proof.encode(), "encodes")), "decodes");
    expect(decoded.position).toBe(proof.position);
    expect(decoded.siblings.map((s) => s.value)).toEqual(proof.siblings.map((s) => s.value));
  });
});

describe("Poseidon", () => {
  it("input counts and canonical elements", () => {
    expect(expectErr(poseidon.hash([]), "no inputs")).toEqual({ kind: "InputCount", count: 0, max: 4 });
    expect(expectErr(poseidon.hash([1n, 2n, 3n, 4n, 5n].map(fr)), "five inputs").kind).toBe("InputCount");
    expect(expectErr(poseidon.Fr.of(poseidon.FR_MODULUS), "r").kind).toBe("NonCanonical");
    expect(expectErr(poseidon.Fr.of(-1n), "negative").kind).toBe("NonCanonical");
    expect(poseidon.Fr.reduce(poseidon.FR_MODULUS + 5n).value).toBe(5n);
    expect(poseidon.compress(fr(1n), fr(2n)).value).toBe(expectOk(poseidon.hash([fr(1n), fr(2n)]), "hash").value);
  });

  it("zero hashes chain, and the empty tree's root is zero(depth)", () => {
    const z = expectOk(poseidon.zeroHashes(8), "zeros");
    expect(z).toHaveLength(9);
    for (let i = 0; i < 8; i++) expect(poseidon.compress(z[i] ?? poseidon.Fr.zero, z[i] ?? poseidon.Fr.zero).equals(z[i + 1] ?? poseidon.Fr.zero)).toBe(true);
    expect(expectOk(poseidon.treeRoot(8, new Map()), "root").equals(z[8] ?? poseidon.Fr.zero)).toBe(true);
    expect(expectErr(poseidon.zeroHashes(0), "depth 0").kind).toBe("Depth");
    expect(expectErr(poseidon.zeroHashes(257), "depth 257").kind).toBe("Depth");
    expect(expectErr(poseidon.treeRoot(8, new Map([[256n, fr(1n)]])), "position").kind).toBe("Position");
  });

  it("proof decoding: positions within 2^depth, elements below r", () => {
    const encode = (position: bigint, siblings: readonly bigint[]): Value => expectOk(models.poseidon.PoseidonMerkleProof.encode({ position, siblings }), "encodes");
    expect(expectErr(poseidon.PoseidonMerkleProof.decode(encode(4n, [0n, 0n])), "position").kind).toBe("Position");
    expect(expectOk(poseidon.PoseidonMerkleProof.decode(encode(3n, [0n, 0n])), "decodes").position).toBe(3n);
    expect(expectOk(poseidon.PoseidonMerkleProof.decode(encode(0n, [])), "depth 0").depth).toBe(0);
    expect(expectErr(poseidon.PoseidonMerkleProof.decode(Value.null), "not a map").kind).toBe("Encoding");
    expect(expectErr(poseidon.PoseidonMerkleProof.of(-1n, []), "negative").kind).toBe("Position");
  });

  it("the tables are Scala's checked-in tables", () => {
    const src = readRepoFile("core/src/main/scala/xyz/kd5ujc/core/hash/PoseidonConstants.scala");
    expect(src).toContain(`val FullRounds: Int = ${FULL_ROUNDS.toString()}`);
    for (const w of widths) {
      const start = src.indexOf(`private object PoseidonWidth${w.t.toString()} {`);
      const end = src.indexOf("\n}", start);
      const body = src.slice(start, end);
      const section = (name: string, next: string | undefined): string => body.slice(body.indexOf(`${name} = Vector(`), next === undefined ? body.length : body.indexOf(`${next} = Vector(`));
      const numbers = (s: string): bigint[] => [...s.matchAll(/BigInt\("(\d+)"\)/g)].map((x) => BigInt(x[1] ?? ""));
      const rows = (s: string): bigint[][] => s.split("Vector(").slice(2).map(numbers);
      expect(body).toContain(`partialRounds = ${w.partialRounds.toString()},`);
      expect(numbers(section("constants", "sparse")), `width ${w.t.toString()} constants`).toEqual(w.constants);
      expect(numbers(section("sparse", "mds")), `width ${w.t.toString()} sparse`).toEqual(w.sparse);
      expect(rows(section("mds", "preSparse")), `width ${w.t.toString()} mds`).toEqual(w.mds);
      expect(rows(section("preSparse", undefined)), `width ${w.t.toString()} preSparse`).toEqual(w.preSparse);
      expect(w.constants).toHaveLength(8 * w.t + w.partialRounds);
      expect(w.sparse).toHaveLength((2 * w.t - 1) * w.partialRounds);
      for (const x of [...w.constants, ...w.sparse, ...w.mds.flat(), ...w.preSparse.flat()]) expect(x < poseidon.FR_MODULUS && x >= 0n).toBe(true);
    }
  });

  it("the tables' digest is pinned", () => {
    const canonical = widths
      .map((w) => [w.t.toString(), w.partialRounds.toString(), w.constants.join(","), w.sparse.join(","), w.mds.flat().join(","), w.preSparse.flat().join(",")].join(";"))
      .join("|");
    expect(createHash("sha256").update(canonical).digest("hex")).toBe("5256c4656304dfef72d933d6daff7a22a363b60d49a723d7146d730b7b99b530");
  });
});

// V8 hashes a bigint by its low 64 bits, with no seed: positions 2^64 apart share a bucket of a `Map` or `Set`, and n
// of them cost O(n^2) to insert. Positions are the caller's, so `treeRoot` must only ever compare them. Nothing here
// measures time: the test watches every `Map` and `Set` lookup and insertion of a bigint instead.
describe("treeRoot: caller-chosen positions are compared, never hashed", () => {
  type Placed = readonly [bigint, poseidon.Fr];

  /** The previous algorithm, a `Map` per level, as the reference. */
  const reference = (depth: number, leaves: readonly Placed[]): poseidon.Fr => {
    const zeros = expectOk(poseidon.zeroHashes(depth), "zeros");
    let level = new Map<bigint, poseidon.Fr>();
    for (const [p, l] of leaves) {
      if (l.equals(poseidon.Fr.zero)) level.delete(p);
      else level.set(p, l);
    }
    for (let i = 0; i < depth; i++) {
      const zero = zeros[i] ?? poseidon.Fr.zero;
      const next = new Map<bigint, poseidon.Fr>();
      for (const p of level.keys()) {
        const parent = p >> 1n;
        if (!next.has(parent)) next.set(parent, poseidon.compress(level.get(parent << 1n) ?? zero, level.get((parent << 1n) | 1n) ?? zero));
      }
      level = next;
    }
    return level.get(0n) ?? zeros[depth] ?? poseidon.Fr.zero;
  };

  const watch = () => [vi.spyOn(Map.prototype, "get"), vi.spyOn(Map.prototype, "set"), vi.spyOn(Map.prototype, "has"), vi.spyOn(Set.prototype, "add"), vi.spyOn(Set.prototype, "has")];
  const hashed = (spies: ReturnType<typeof watch>): number => spies.reduce((sum, s) => sum + s.mock.calls.filter((args) => typeof args[0] === "bigint").length, 0);

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("the watch sees a bigint put in a Map", () => {
    const spies = watch();
    new Map<bigint, number>().set(1n, 1);
    expect(hashed(spies)).toBe(1);
  });

  it("positions 2^64 apart: no Map or Set sees one, and the root is the reference's", () => {
    // depth 69 holds 32 positions 2^64 apart; Poseidon is slow here, so the tree stays small
    const depth = 69;
    const leaves: Placed[] = Array.from({ length: 32 }, (_, i) => [BigInt(i) << 64n, fr(BigInt(i + 1))] as const);
    const expected = reference(depth, leaves);
    const spies = watch();
    const root = expectOk(poseidon.treeRoot(depth, leaves), "root");
    expect(hashed(spies)).toBe(0);
    expect(root.equals(expected)).toBe(true);
  }, 120_000);

  it("entries apply in order: a later entry for a position replaces an earlier one, and a later 0 empties it", () => {
    // a fixed pseudo-random walk over a depth-6 tree, so positions repeat and siblings pair up
    const leaves: Placed[] = Array.from({ length: 200 }, (_, i) => [BigInt((i * 37 + 11) % 64), fr(BigInt(i % 7 === 0 ? 0 : i))] as const);
    for (const n of [0, 1, 2, 3, 63, 64, 65, 200]) {
      const some = leaves.slice(0, n);
      expect(expectOk(poseidon.treeRoot(6, some), "root").equals(reference(6, some)), `the first ${n.toString()} entries`).toBe(true);
    }
    const one = fr(1n);
    const two = fr(2n);
    expect(expectOk(poseidon.treeRoot(6, [[5n, one], [5n, two]]), "replaced").equals(expectOk(poseidon.treeRoot(6, [[5n, two]]), "two"))).toBe(true);
    expect(expectOk(poseidon.treeRoot(6, [[5n, one], [5n, poseidon.Fr.zero]]), "emptied").equals(expectOk(poseidon.zeroHashes(6), "zeros")[6] ?? one)).toBe(true);
    expect(expectErr(poseidon.treeRoot(6, [[5n, one], [64n, one]]), "out of range")).toEqual({ kind: "Position", position: 64n, depth: 6 });
  });

  it("an iterable that yields one tuple again with new contents builds the tree of what it yielded", () => {
    const one = fr(1n);
    const two = fr(2n);
    function* reused(): Generator<readonly [bigint, poseidon.Fr]> {
      const entry: [bigint, poseidon.Fr] = [0n, one];
      yield entry;
      entry[0] = 1n;
      entry[1] = two;
      yield entry;
    }
    const expected = expectOk(poseidon.treeRoot(2, [[0n, one], [1n, two]]), "independent entries");
    expect(expectOk(poseidon.treeRoot(2, reused()), "reused tuple").equals(expected)).toBe(true);
  });
});

describe("PoseidonMerkleProof.of: the depth is checked before any work in it", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("an over-deep path is refused without reading a sibling or making a 2^depth bigint", () => {
    const reads: PropertyKey[] = [];
    // a sparse array of 2^31 slots costs nothing to make; 2^(2^31) is past V8's largest bigint, so computing it throws
    const deep = new Proxy(new Array<poseidon.Fr>(2 ** 31), {
      get: (target, key, receiver) => {
        if (key !== "length") reads.push(key);
        return Reflect.get(target, key, receiver) as unknown;
      },
    });
    const made = vi.spyOn(globalThis, "BigInt");
    const refused = poseidon.PoseidonMerkleProof.of(0n, deep);
    const largest = made.mock.calls.reduce((max, [x]) => (typeof x === "number" && x > max ? x : max), 0);
    expect(refused).toEqual({ ok: false, error: { kind: "Position", position: 0n, depth: 2 ** 31 } });
    expect(reads).toEqual([]);
    expect(largest).toBeLessThanOrEqual(poseidon.MAX_DEPTH);
  });

  it("depth MAX_DEPTH is accepted, and one more is refused", () => {
    const zeros = (n: number): poseidon.Fr[] => Array.from({ length: n }, () => poseidon.Fr.zero);
    expect(expectOk(poseidon.PoseidonMerkleProof.of((1n << 256n) - 1n, zeros(256)), "depth 256").depth).toBe(256);
    expect(expectErr(poseidon.PoseidonMerkleProof.of(0n, zeros(257)), "depth 257")).toEqual({ kind: "Position", position: 0n, depth: 257 });
  });
});
