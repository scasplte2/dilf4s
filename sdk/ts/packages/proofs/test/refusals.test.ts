/**
 * `shared/vectors/refusals_v1.json` (docs/specs/conformance.md): every case run as section 6 of the spec defines its
 * check, and its outcome compared with the vector's: "accepted", or the refusal's kind and normative fields.
 */
import { describe, expect, it } from "vitest";

import { models, Value } from "@kd5ujc/dilf4s-core";

import { merkle, poseidon, trie, versioned } from "../src/index.js";
import { bytes, expectOk, loadVectors, valueOf } from "./vectors.js";

type Case = models.refusalVectors.RefusalCase;
type Outcome = models.refusalVectors.RefusalOutcome;

const vectors = loadVectors("refusals_v1.json", models.refusalVectors.RefusalVectors);
const vt = expectOk(versioned.VersionedTrie.of("SHA-256", expectOk(versioned.VersionKey.unsignedLong(8), "version key")), "profile");

/** `template` with every text `"$"` replaced by `v`; templates are a few levels deep. */
function fill(template: Value, v: Value): Value {
  if (template.kind === "text") return template.value === "$" ? v : template;
  if (template.kind === "array") return expectOk(Value.array(template.items.map((x) => fill(x, v))), "array");
  if (template.kind === "map") return expectOk(Value.map(template.entries().map(([k, x]) => [k, fill(x, v)] as const)), "map");
  return template;
}

/** A `built-value`: every occurrence is the one value, shared, so building never costs the weight. */
function built(b: models.refusalVectors.BuiltValue): Value {
  return (b.steps ?? []).reduce<Value>((v, step) => {
    if ("copies" in step) return expectOk(Value.array(Array.from({ length: Number(step.copies) }, () => v)), "copies");
    const t = valueOf(step.template);
    return Array.from({ length: Number(step.times) }).reduce<Value>((x) => fill(t, x), v);
  }, valueOf(b.leaf));
}

/** An outcome as the vectors record it. */
const accepted: Outcome = "accepted";

function versionedOutcome(e: versioned.VersionedTrieError): Outcome {
  if (e.kind === "TooLarge") return { kind: e.kind, limit: BigInt(e.limit) };
  if (e.kind === "TooDeep") return { kind: e.kind, limit: BigInt(e.limit), depth: BigInt(e.depth) };
  if (e.kind === "RootMismatch") return { kind: e.kind, component: e.component };
  if (e.kind === "Trie") return { kind: `Trie.${e.error.kind}` };
  return { kind: e.kind };
}

const of =
  <E extends { readonly kind: string }>(outcome: (e: E) => Outcome) =>
  (r: { readonly ok: true } | { readonly ok: false; readonly error: E }): Outcome =>
    r.ok ? accepted : outcome(r.error);

const byKind = (e: { readonly kind: string }): Outcome => ({ kind: e.kind });

/** The case's outcome, by its check (conformance.md section 6): the first refusal, or "accepted". */
function run(c: Case): Outcome {
  const input = (): Value => built(c.input ?? { leaf: "" });
  const stateKey = (): versioned.StateKey => expectOk(versioned.StateKey.parse(c.stateKey ?? ""), "state key");
  const root = (): Uint8Array => bytes(c.root ?? "");
  const v = of(versionedOutcome);
  switch (c.check) {
    case "vt-value":
      return v(vt.checkValue(stateKey(), input()));
    case "vt-chunk":
      return v(vt.decodeChunk(new Uint8Array(Number(c.zeros))));
    case "vt-delta":
      return v(vt.decodeDelta(input()));
    case "vt-proof":
      return v(vt.decodeProof(input()));
    case "vt-state": {
      const p = vt.decodeProof(input());
      if (!p.ok) return v(p);
      const s = vt.verifyState(root(), p.value);
      return v(s.ok ? s.value.lookup(stateKey()) : s);
    }
    case "vt-historical": {
      const p = vt.decodeProof(input());
      if (!p.ok) return v(p);
      const s = vt.verifyHistoricalKey(root(), p.value, c.at ?? 0n);
      return v(s.ok ? s.value.lookup(stateKey()) : s);
    }
    case "vt-replica": {
      let replica = versioned.Replica.empty(vt);
      for (const d of c.inputs ?? []) {
        const delta = vt.decodeDelta(built(d));
        if (!delta.ok) return v(delta);
        const next = replica.applyDelta(delta.value);
        if (!next.ok) return v(next);
        replica = next.value;
      }
      return accepted;
    }
    case "merkle-inclusion": {
      const hashing = merkle.TaggedMerkleHashing.of("SHA-256");
      const p = merkle.InclusionProof.decode(hashing, input());
      return of(byKind)(p.ok ? merkle.checkInclusion(hashing, root(), bytes(c.leafHash ?? ""), p.value) : p);
    }
    case "merkle-consistency": {
      const hashing = merkle.TaggedMerkleHashing.of("SHA-256");
      const p = merkle.ConsistencyProof.decode(hashing, input());
      return of(byKind)(p.ok ? merkle.checkConsistency(hashing, bytes(c.oldRoot ?? ""), root(), p.value) : p);
    }
    case "trie-lookup": {
      const m = c.mode ?? { fixed: 0n };
      const mode = expectOk("fixed" in m ? trie.KeyMode.fixed(Number(m.fixed)) : trie.KeyMode.variable(Number(m.variable)), "mode");
      const hashing = trie.TrieHashing.of("SHA-256");
      const p = trie.TrieProof.decode(hashing, input());
      if (!p.ok) return byKind(p.error);
      const verified = trie.check(hashing, mode, root(), p.value);
      if (!verified.ok) return byKind(verified.error);
      const found = verified.value.lookup(bytes(c.key ?? ""));
      if (!found.ok) return byKind(found.error);
      return found.value.kind === "notCovered" ? { kind: "NotCovered" } : accepted;
    }
    case "poseidon-proof":
      return of(byKind)(poseidon.PoseidonMerkleProof.decode(input()));
  }
}

describe("refusals_v1.json", { timeout: 120_000 }, () => {
  it("has a case for every check", () => {
    expect(new Set(vectors.cases.map((c) => c.check)).size).toBe(11);
  });

  for (const c of vectors.cases) {
    it(c.name, () => {
      const start = performance.now();
      expect(run(c)).toEqual(c.outcome);
      expect(performance.now() - start, "bounded work").toBeLessThan(20_000);
    });
  }
});
