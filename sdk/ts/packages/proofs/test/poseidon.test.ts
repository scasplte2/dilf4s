import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

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
