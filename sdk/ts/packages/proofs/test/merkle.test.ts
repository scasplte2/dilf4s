import { describe, expect, it } from "vitest";

import { BytesValue, cbor, models, toHex, Value, valueCommitment } from "@kd5ujc/dilf4s-core";

import { merkle } from "../src/index.js";
import { bytes, expectErr, expectOk, loadVectors, valueOf } from "./vectors.js";

const vectors = loadVectors("merkle_v1.json", models.merkleVectors.MerkleVectors);
const tagged = merkle.TaggedMerkleHashing.of("SHA-256");
const rfc = merkle.Rfc6962MerkleHashing.of("SHA-256");
const leaves = vectors.leaves.map(bytes);
const leafHashes = vectors.leafHashes.map(bytes);

const taggedRoot = (n: number): Uint8Array => expectOk(merkle.merkleRoot(tagged, leafHashes.slice(0, n)), "root");
const rfcRoot = (n: number): Uint8Array =>
  expectOk(
    merkle.merkleRoot(
      rfc,
      leaves.slice(0, n).map((l) => rfc.leaf(l)),
    ),
    "root",
  );


describe("merkle_v1.json", () => {
  it("the empty roots", () => {
    expect(toHex(taggedRoot(0))).toBe(vectors.trees[0]?.root);
    expect(toHex(rfc.empty)).toBe(vectors.trees[0]?.rfc6962Root);
  });

  describe.each(vectors.trees.map((t) => [Number(t.size), t] as const))("tree of %i leaves", (size, tree) => {
    const profiles = [
      { name: "tagged", hashing: tagged, root: taggedRoot, leafHash: (i: number) => leafHashes[i], inclusion: tree.inclusion, consistency: tree.consistency, treeRoot: tree.root },
      { name: "rfc6962", hashing: rfc, root: rfcRoot, leafHash: (i: number) => rfc.leaf(leaves[i] ?? new Uint8Array(0)), inclusion: tree.rfc6962Inclusion, consistency: tree.rfc6962Consistency, treeRoot: tree.rfc6962Root },
    ] as const;

    it.each(profiles.map((p) => [p.name, p] as const))("%s root from the leaves", (_, p) => {
      expect(toHex(p.root(size))).toBe(p.treeRoot);
    });

    it.each(profiles.map((p) => [p.name, p] as const))("%s inclusion proofs verify, and fail for any other leaf", (_, p) => {
      expect(p.inclusion).toHaveLength(size);
      const root = bytes(p.treeRoot);
      for (const [i, hex] of p.inclusion.entries()) {
        const proof = expectOk(merkle.InclusionProof.decode(p.hashing, valueOf(hex)), `proof ${i.toString()}`);
        expect(proof.index).toBe(BigInt(i));
        expect(proof.size).toBe(BigInt(size));
        expect(toHex(cbor.encode(expectOk(proof.encode(), "encodes"))), "re-encodes").toBe(hex);
        const leafHash = p.leafHash(i) ?? new Uint8Array(0);
        expect(expectOk(merkle.verifyInclusion(p.hashing, root, leafHash, proof), "verifies"), `leaf ${i.toString()}`).toBe(true);
        const other = p.leafHash((i + 1) % 8) ?? new Uint8Array(0);
        expect(expectOk(merkle.verifyInclusion(p.hashing, root, other, proof), "verdict"), "another leaf").toBe(false);
      }
    });

    it.each(profiles.map((p) => [p.name, p] as const))("%s consistency proofs from every smaller size verify", (_, p) => {
      expect(p.consistency).toHaveLength(size + 1);
      const newRoot = bytes(p.treeRoot);
      for (const [m, hex] of p.consistency.entries()) {
        const proof = expectOk(merkle.ConsistencyProof.decode(p.hashing, valueOf(hex)), `proof from ${m.toString()}`);
        expect([proof.oldSize, proof.newSize]).toEqual([BigInt(m), BigInt(size)]);
        expect(toHex(cbor.encode(expectOk(proof.encode(), "encodes")))).toBe(hex);
        const oldRoot = p.root(m);
        expect(expectOk(merkle.verifyConsistency(p.hashing, oldRoot, newRoot, proof), "verifies"), `from ${m.toString()}`).toBe(true);
        expect(expectOk(merkle.merkleConsistencyVerify(p.hashing, { root: oldRoot, size: BigInt(m) }, { root: newRoot, size: BigInt(size) }, valueOf(hex)), "jlvm")).toBe(true);
        if (m > 0 && m < size) {
          const wrong = p.root(m - 1);
          expect(expectOk(merkle.verifyConsistency(p.hashing, wrong, newRoot, proof), "verdict"), "a wrong old root").toBe(false);
        }
      }
    });
  });

  it("value commitments hash as the leaf of a value", () => {
    for (const c of vectors.valueCommitments) {
      expect(toHex(valueCommitment(valueOf(c.cbor))), "commit(v)").toBe(c.commitment);
      expect(toHex(tagged.leaf(valueOf(c.cbor))), "leaf hash").toBe(c.leafHash);
    }
  });
});

describe("JLVM merkle_verify", () => {
  const tree = vectors.trees.find((t) => t.size === 5n);
  if (tree === undefined) throw new Error("a tree of 5 leaves");
  const root = bytes(tree.root);
  const proof2 = valueOf(tree.inclusion[2] ?? "");

  it("a value leaf verifies through its commitment, a bytes leaf with the same CBOR does not", () => {
    const leafValue = Value.text("a");
    if (!leafValue.ok) throw new Error("text");
    const hashes = [tagged.leaf(leafValue.value), ...leafHashes.slice(1, 5)];
    const r = expectOk(merkle.merkleRoot(tagged, hashes), "root");
    const proof = valueOf(tree.inclusion[0] ?? "");
    expect(expectOk(merkle.merkleVerify(tagged, { root: r, size: 5n }, leafValue.value, proof), "verdict")).toBe(true);
    expect(expectOk(merkle.merkleVerify(tagged, { root: r, size: 5n }, BytesValue.of(Uint8Array.of(0x61)), proof), "verdict")).toBe(false);
  });

  it("a head whose size differs from the proof's is false, even with the right root", () => {
    expect(expectOk(merkle.merkleVerify(tagged, { root, size: 6n }, Value.null, proof2), "verdict")).toBe(false);
  });

  it("a tampered path is false", () => {
    const p = expectOk(merkle.InclusionProof.decode(tagged, proof2), "decodes");
    const path = [...p.path];
    path[0] = Uint8Array.from(path[0] ?? [], (b, i) => (i === 0 ? b ^ 1 : b));
    const tampered = expectOk(merkle.InclusionProof.of(p.index, p.size, path), "constructs");
    expect(expectOk(merkle.verifyInclusion(tagged, root, leafHashes[2] ?? new Uint8Array(0), tampered), "verdict")).toBe(false);
    expect(expectErr(merkle.checkInclusion(tagged, root, leafHashes[2] ?? new Uint8Array(0), tampered), "fails").kind).toBe("RootMismatch");
  });

  it("a path of the wrong length is InvalidProof, so false", () => {
    const p = expectOk(merkle.InclusionProof.decode(tagged, proof2), "decodes");
    const longer = expectOk(merkle.InclusionProof.of(p.index, p.size, [...p.path, root]), "constructs");
    const shorter = expectOk(merkle.InclusionProof.of(p.index, p.size, p.path.slice(1)), "constructs");
    expect(expectErr(merkle.checkInclusion(tagged, root, leafHashes[2] ?? new Uint8Array(0), longer), "fails").kind).toBe("InvalidProof");
    expect(expectErr(merkle.checkInclusion(tagged, root, leafHashes[2] ?? new Uint8Array(0), shorter), "fails").kind).toBe("InvalidProof");
    expect(expectOk(merkle.verifyInclusion(tagged, root, leafHashes[2] ?? new Uint8Array(0), longer), "verdict")).toBe(false);
  });

  it("malformed proofs and heads are typed errors", () => {
    const shortDigest = models.merkle.MerkleInclusion.encode({ index: 0n, size: 2n, path: [new Uint8Array(31)] });
    expect(expectErr(merkle.merkleVerify(tagged, { root, size: 2n }, Value.null, expectOk(shortDigest, "encodes")), "fails").kind).toBe("Encoding");
    const outOfRange = models.merkle.MerkleInclusion.encode({ index: 2n, size: 2n, path: [] });
    expect(expectErr(merkle.merkleVerify(tagged, { root, size: 2n }, Value.null, expectOk(outOfRange, "encodes")), "fails")).toEqual({ kind: "IndexOutOfRange", index: 2n, size: 2n });
    expect(expectErr(merkle.merkleVerify(tagged, { root, size: 5n }, Value.null, Value.null), "fails").kind).toBe("Encoding");
    expect(expectErr(merkle.merkleVerify(tagged, { root: root.slice(1), size: 5n }, Value.null, proof2), "fails").kind).toBe("Encoding");
    expect(expectErr(merkle.merkleVerify(tagged, { root, size: -1n }, Value.null, proof2), "fails").kind).toBe("Encoding");
    const backwards = models.merkle.MerkleConsistency.encode({ old: 3n, new: 2n, path: [] });
    expect(expectErr(merkle.merkleConsistencyVerify(tagged, { root, size: 3n }, { root, size: 2n }, expectOk(backwards, "encodes")), "fails")).toEqual({ kind: "InvalidSizes", oldSize: 3n, newSize: 2n });
  });

  it("consistency: heads whose sizes differ from the proof's are false", () => {
    const hex = tree.consistency[3] ?? "";
    expect(expectOk(merkle.merkleConsistencyVerify(tagged, { root: taggedRoot(3), size: 2n }, { root, size: 5n }, valueOf(hex)), "verdict")).toBe(false);
    expect(expectOk(merkle.merkleConsistencyVerify(tagged, { root: taggedRoot(3), size: 3n }, { root, size: 5n }, valueOf(hex)), "verdict")).toBe(true);
    expect(expectOk(merkle.merkleConsistencyVerify(tagged, { root: taggedRoot(3), size: 3n }, { root: taggedRoot(4), size: 5n }, valueOf(hex)), "verdict")).toBe(false);
  });

  it("the rfc6962 profile takes raw bytes and never verifies under the tagged hashing", () => {
    const rfcTree = bytes(tree.rfc6962Root);
    const p = valueOf(tree.rfc6962Inclusion[2] ?? "");
    expect(expectOk(merkle.merkleVerify(rfc, { root: rfcTree, size: 5n }, leaves[2] ?? new Uint8Array(0), p), "verdict")).toBe(true);
    expect(expectOk(merkle.merkleVerify(tagged, { root: rfcTree, size: 5n }, BytesValue.of(leaves[2] ?? new Uint8Array(0)), p), "verdict")).toBe(false);
  });
});

describe("merkle_v1.json malformed proofs", () => {
  it("each is refused at decode with the recorded kind", () => {
    expect(vectors.malformed.length).toBeGreaterThan(10);
    for (const c of vectors.malformed) {
      const proof = valueOf(c.cbor);
      const r = c.kind === "inclusion" ? merkle.InclusionProof.decode(tagged, proof) : merkle.ConsistencyProof.decode(tagged, proof);
      expect(r.ok ? "accepted" : r.error.kind, c.name).toBe(c.error);
    }
  });
});

describe("refusals name a number by its size", () => {
  it("a head size of any size", () => {
    const head = { root: new Uint8Array(32), size: 1n << 1_000_000n };
    expect(expectErr(merkle.merkleVerify(tagged, head, Value.null, Value.null), "refused")).toMatchObject({ reason: "head.size of 1000001 bits is not a tree size" });
  });
});
