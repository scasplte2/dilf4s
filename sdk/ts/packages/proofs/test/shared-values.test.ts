import { describe, expect, it } from "vitest";

import { MAX_WEIGHT, models, Value } from "@kd5ujc/dilf4s-core";

import { versioned } from "../src/index.js";
import { bytes, expectErr, expectOk, loadVectors, valueOf } from "./vectors.js";

const vectors = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors);
const versionKey = expectOk(versioned.VersionKey.unsignedLong(Number(vectors.width)), "version key");
const vt = expectOk(versioned.VersionedTrie.of("SHA-256", versionKey), "profile");
const key = (s: string): versioned.StateKey => expectOk(versioned.StateKey.parse(s), `state key ${s}`);

/** `[x, x]` nested `k` deep around `leaf`: `k` arrays, `2^k` leaves. */
const doubled = (leaf: Value, k: number): Value => Array.from({ length: k }).reduce<Value>((v) => expectOk(Value.array([v, v]), "array"), leaf);

const elapsed = (f: () => void): number => {
  const start = performance.now();
  f();
  return performance.now() - start;
};

/** Expanding any of these values would never finish (2^60 leaves), so a generous limit still tells it apart, under load too. */
const HANG = 20_000;

describe("versioned values built with shared parts", { timeout: 60_000 }, () => {
  const k = key("fiber/1");
  const dag = doubled(expectOk(Value.text("a"), "text"), 60);

  it("checkValue measures without encoding: refused by weight, or at the first byte past 1 MiB", () => {
    const ms = elapsed(() => {
      expect(expectErr(vt.checkValue(k, dag), "2^60 leaves")).toEqual({ kind: "TooLarge", what: "the value of fiber/1", size: Number(dag.weight), max: versioned.MAX_VALUE_BYTES });
      // within the weight bound but over in bytes (each 2^64 - 1 weighs 2 and takes 9 bytes)
      const ints = doubled(Value.int(2n ** 64n - 1n), 17);
      const e = expectErr(vt.checkValue(k, ints), "over in bytes");
      expect(e.kind === "TooLarge" && e.size > versioned.MAX_VALUE_BYTES, "a size past the limit").toBe(true);
      expect(vt.checkValue(k, doubled(Value.int(2n ** 64n - 1n), 16)).ok, "1.2 MiB of logical size, 655 KiB of CBOR").toBe(true);
      // the weight saturates within the depth limit: the size is the saturated weight
      const saturated = doubled(expectOk(Value.text("a"), "text"), versioned.MAX_VALUE_DEPTH);
      expect(saturated.weight).toBe(MAX_WEIGHT);
      expect(expectErr(vt.checkValue(k, saturated), "saturated")).toMatchObject({ kind: "TooLarge", size: Number(MAX_WEIGHT) });
      // depth is checked first
      expect(expectErr(vt.checkValue(k, doubled(Value.null, versioned.MAX_VALUE_DEPTH + 1)), "too deep").kind).toBe("TooDeep");
    });
    expect(ms).toBeLessThan(HANG);
  });

  it("holds looks the key up first, then refuses a claim no commit could store without hashing it", () => {
    const proof = vectors.proofs.find((p) => p.kind === "state" && (p.claims ?? []).some((c) => c.value !== undefined));
    const claim = proof?.claims?.find((c) => c.value !== undefined);
    expect(claim, "a state proof with a present key").toBeDefined();
    if (proof === undefined || claim?.value === undefined) return;
    const state = expectOk(vt.verifyState(bytes(vectors.root), expectOk(vt.decodeProof(valueOf(proof.cbor)), "proof")), "verifies");
    const present = key(claim.key);
    const ms = elapsed(() => {
      expect(expectOk(state.holds(present, valueOf(claim.value ?? "")), "the stored value"), "true for the stored value").toBe(true);
      expect(expectOk(state.holds(present, dag), "a doubling claim"), "false, unhashed").toBe(false);
      expect(expectOk(state.holds(present, null), "absence"), "false: present").toBe(false);
    });
    expect(ms).toBeLessThan(HANG);
  });

  it("deltas and snapshot chunks refuse such a value before hashing it", () => {
    const empty = vt.hashing.empty;
    const ms = elapsed(() => {
      const delta = { head: { version: 1n, stateRoot: empty, historyRoot: empty }, upserts: [[k, dag] as const], removes: [] };
      expect(expectErr(versioned.Replica.empty(vt).applyDelta(delta), "delta").kind).toBe("TooLarge");
      const head = { version: 1n, stateRoot: empty, historyRoot: empty };
      const reader = expectOk(versioned.SnapshotReader.start(vt, head).feed({ kind: "head", head }), "head");
      expect(expectErr(reader.feed({ kind: "state", entries: [[k, dag]] }), "chunk").kind).toBe("TooLarge");
    });
    expect(ms).toBeLessThan(HANG);
  });
});
