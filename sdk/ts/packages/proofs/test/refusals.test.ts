/**
 * `shared/vectors/refusals_v1.json` (docs/specs/conformance.md): every case run as section 6 of the spec defines its
 * check, and its outcome compared with the vector's: "accepted", or the refusal's kind and normative fields.
 */
import { describe, expect, it } from "vitest";

import { fromHex, models, Value, type TypeMismatch } from "@kd5ujc/dilf4s-core";

import { MAX_PROOF_WEIGHT, merkle, poseidon, trie, versioned } from "../src/index.js";
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

/** A refusal of no family the vectors know (a `TypeMismatch`): equal to no row, so the case fails and shows it. */
const unexpected = (e: TypeMismatch): Outcome => ({ unexpected: e.reason }) as unknown as Outcome;

/** A versioned-trie refusal as the vectors record it: its kind, the kind it wraps, and its normative fields. */
function versionedOutcome(e: versioned.VersionedRefusal): Outcome {
  if (e.kind === "TypeMismatch") return unexpected(e);
  const segment = e.kind === "Key" && (e.error.kind === "InvalidSegment" || e.error.kind === "SegmentTooLong") ? BigInt(e.error.segment) : undefined;
  const cause = e.kind === "Trie" ? { trie: e.error.kind } : e.kind === "Key" ? { stateKey: e.error.kind, segment } : undefined;
  const limit = e.kind === "TooLarge" || e.kind === "TooDeep" ? BigInt(e.limit) : undefined;
  const depth = e.kind === "TooDeep" ? BigInt(e.depth) : undefined;
  const component = e.kind === "RootMismatch" ? e.component : undefined;
  return { versionedTrie: e.kind, cause, limit, depth, component };
}

const of =
  <E extends { readonly kind: string }>(outcome: (e: E) => Outcome) =>
  (r: { readonly ok: true } | { readonly ok: false; readonly error: E }): Outcome =>
    r.ok ? accepted : outcome(r.error);

const merkleOutcome = (e: merkle.MerkleError | TypeMismatch): Outcome =>
  e.kind === "TypeMismatch" ? unexpected(e) : { merkle: e.kind, limit: e.kind === "TooLarge" ? BigInt(e.limit) : undefined };

const trieOutcome = (e: trie.TrieError | TypeMismatch): Outcome =>
  e.kind === "TypeMismatch" ? unexpected(e) : { trie: e.kind, limit: e.kind === "TooLarge" ? BigInt(e.limit) : undefined };

const modeOf = (m: models.refusalVectors.RefusalMode | undefined): trie.KeyMode => {
  const x = m ?? { fixed: 0n };
  return expectOk("fixed" in x ? trie.KeyMode.fixed(Number(x.fixed)) : trie.KeyMode.variable(Number(x.variable)), "mode");
};

/** The case's outcome, by its check (conformance.md section 6): the first refusal, or "accepted". */
function run(c: Case, rootOverride?: Uint8Array): Outcome {
  const input = (): Value => built(c.input ?? { leaf: "" });
  const stateKey = (): versioned.StateKey => expectOk(versioned.StateKey.parse(c.stateKey ?? ""), "state key");
  const root = (): Uint8Array => rootOverride ?? bytes(c.root ?? "");
  const v = of(versionedOutcome);
  switch (c.check) {
    case "vt-value":
      return v(vt.checkValue(stateKey(), input()));
    case "vt-chunk":
      return v(vt.decodeChunk(c.chunk === undefined ? new Uint8Array(Number(c.zeros)) : bytes(c.chunk)));
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
      let replica = expectOk(versioned.Replica.empty(vt), "Replica.empty");
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
      const hashing = expectOk(merkle.TaggedMerkleHashing.of("SHA-256"), "TaggedMerkleHashing.of");
      const p = merkle.InclusionProof.decode(hashing, input());
      return of(merkleOutcome)(p.ok ? merkle.checkInclusion(hashing, root(), bytes(c.leafHash ?? ""), p.value) : p);
    }
    case "merkle-consistency": {
      const hashing = expectOk(merkle.TaggedMerkleHashing.of("SHA-256"), "TaggedMerkleHashing.of");
      const p = merkle.ConsistencyProof.decode(hashing, input());
      return of(merkleOutcome)(p.ok ? merkle.checkConsistency(hashing, bytes(c.oldRoot ?? ""), root(), p.value) : p);
    }
    case "trie-lookup": {
      const mode = modeOf(c.mode);
      const hashing = expectOk(trie.TrieHashing.of("SHA-256"), "TrieHashing.of");
      const p = trie.TrieProof.decode(hashing, input());
      if (!p.ok) return trieOutcome(p.error);
      const verified = trie.check(hashing, mode, root(), p.value);
      if (!verified.ok) return trieOutcome(verified.error);
      const found = verified.value.lookup(bytes(c.key ?? ""));
      if (!found.ok) return trieOutcome(found.error);
      return found.value.kind === "notCovered" ? { trie: "NotCovered" } : accepted;
    }
    case "vt-commit": {
      // the weight of the delta it would serve at version 1 (no parent; the head's version key and roots at their
      // widths), checked first, then each value as a commit checks it: what the store refuses before writing
      const upserts: models.versionedTrie.VersionedTrieUpsert[] = (c.batch ?? []).map((u) => ({ k: u.stateKey, v: built(u.value) }));
      // what each value's depth and weight alone refuse, then the delta's weight, then each value's measured length
      for (const u of upserts.filter((x) => x.v.depth > versioned.MAX_VALUE_DEPTH || x.v.weight > BigInt(versioned.MAX_VALUE_BYTES))) {
        const deep = vt.checkValue(expectOk(versioned.StateKey.parse(u.k), "state key"), u.v);
        if (!deep.ok) return v(deep);
      }
      const head = { v: expectOk(fromHex("0000000000000001"), "vkey"), s: new Uint8Array(32), h: new Uint8Array(32) };
      const delta = expectOk(models.versionedTrie.VersionedTrieDelta.encode({ head, upserts, removes: [] }), "delta");
      if (delta.weight > MAX_PROOF_WEIGHT) return { versionedTrie: "TooLarge", limit: MAX_PROOF_WEIGHT };
      for (const u of upserts) {
        const checked = vt.checkValue(expectOk(versioned.StateKey.parse(u.k), "state key"), u.v);
        if (!checked.ok) return v(checked);
      }
      return accepted;
    }
    case "merkle-verify": {
      const hashing = expectOk(merkle.TaggedMerkleHashing.of("SHA-256"), "TaggedMerkleHashing.of");
      const head = { root: root(), size: c.size ?? 0n };
      return of(merkleOutcome)(merkle.merkleVerify(hashing, head, built(c.claim ?? { leaf: "" }), input()));
    }
    case "trie-verify": {
      const claim = c.claim === undefined ? null : built(c.claim);
      return of(trieOutcome)(trie.trieVerify(expectOk(trie.TrieHashing.of("SHA-256"), "TrieHashing.of"), modeOf(c.mode), root(), bytes(c.key ?? ""), claim, input()));
    }
    case "trie-prefix-verify": {
      const entries: readonly trie.ClaimedEntry[] = (c.entries ?? []).map((e) => [bytes(e.key), built(e.value)] as const);
      return of(trieOutcome)(trie.triePrefixVerify(expectOk(trie.TrieHashing.of("SHA-256"), "TrieHashing.of"), modeOf(c.mode), root(), bytes(c.prefix ?? ""), entries, input()));
    }
    case "poseidon-proof":
      return of((e: poseidon.PoseidonMerkleError | TypeMismatch): Outcome =>
        e.kind === "TypeMismatch" ? unexpected(e) : { poseidonMerkle: e.kind, depth: e.kind === "Position" ? BigInt(e.depth) : undefined },
      )(poseidon.PoseidonMerkleProof.decode(input()));
  }
}

describe("refusals_v1.json", { timeout: 120_000 }, () => {
  it("has a case for every check", () => {
    expect(new Set(vectors.cases.map((c) => c.check)).size).toBe(15);
  });

  for (const c of vectors.cases) {
    it(c.name, () => {
      const start = performance.now();
      expect(run(c)).toEqual(c.outcome);
      expect(performance.now() - start, "bounded work").toBeLessThan(20_000);
    });
  }

  // the boundary refuses nothing past a bound: a root longer than the digest is refused where a shorter wrong-sized one
  // is, so each precedence a short root pins holds for a long one too, even one too long to copy
  it("each precedence pinned with a 31-byte root holds with a 33-byte root and with a 64 MiB shared one, uncopied", () => {
    const pinned = vectors.cases.filter((c) => c.name.includes("31-byte root") && (c.check === "trie-verify" || c.check === "merkle-verify"));
    expect(pinned.length, "the cases pinning it").toBeGreaterThanOrEqual(3);
    const huge = new Uint8Array(new SharedArrayBuffer(64 << 20));
    for (const c of pinned) {
      const long = Uint8Array.from([...bytes(c.root ?? ""), 0, 0]);
      expect(run(c, long), `${c.name}, 33 bytes`).toEqual(c.outcome);
      const before = process.memoryUsage().arrayBuffers;
      expect(run(c, huge), `${c.name}, 64 MiB`).toEqual(c.outcome);
      expect(process.memoryUsage().arrayBuffers - before, `${c.name}: the 64 MiB root was not copied`).toBeLessThan(1 << 20);
    }
  });
});

/**
 * The gate of conformance.md section 6, total over each family's generated kinds: a `Record` keyed by the kind union,
 * so a kind added to the schema does not compile until it is listed, as pinned by a row or exempt (a kind no input
 * reaches through an SDK's verifier). The rows are refusals_v1.json's (each of its check's family), the `malformed`
 * rows of merkle_v1.json and trie_v1.json, a versioned trie's `Trie` cause, and versioned_trie_v1.json's version errors.
 */
describe("every kind of the proof families has a row, or is exempt", () => {
  type Pin = "row" | "exempt";
  const vtKinds: Readonly<Record<versioned.VersionedTrieError["kind"], Pin>> = {
    Key: "row",
    InvalidVersion: "row",
    Empty: "exempt",
    VersionNotIncreasing: "row",
    VersionConflict: "exempt",
    CommitRewrite: "exempt",
    NotRetained: "exempt",
    UnknownVersion: "row",
    RootMismatch: "row",
    NotCovered: "row",
    Malformed: "row",
    TooLarge: "row",
    TooDeep: "row",
    Store: "exempt",
    Trie: "row",
  };
  const merkleKinds: Readonly<Record<merkle.MerkleError["kind"], Pin>> = {
    IndexOutOfRange: "row",
    InvalidSizes: "row",
    InvalidProof: "row",
    RootMismatch: "row",
    Encoding: "row",
    TooLarge: "row",
  };
  const trieKinds: Readonly<Record<trie.TrieError["kind"], Pin>> = {
    InvalidKey: "row",
    MissingNode: "exempt",
    InvalidNode: "exempt",
    InvalidProof: "row",
    RootMismatch: "row",
    Encoding: "row",
    NotCovered: "row",
    TooLarge: "row",
  };
  const poseidonKinds: Readonly<Record<poseidon.PoseidonMerkleError["kind"], Pin>> = {
    Position: "row",
    Depth: "exempt",
    OccupiedPosition: "exempt",
    Encoding: "row",
  };
  const merkleRows = loadVectors("merkle_v1.json", models.merkleVectors.MerkleVectors).malformed.map((r) => r.error);
  const trieRows = loadVectors("trie_v1.json", models.trieVectors.TrieVectors).malformed.map((r) => r.error);
  const versionRows = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors).versionErrors.map((r) => r.outcome.kind);
  const outcomes = vectors.cases.map((c) => [c.check, c.outcome] as const);
  const family = (check: string): string => (check.startsWith("vt-") ? "versionedTrie" : check.startsWith("merkle-") ? "merkle" : check.startsWith("trie-") ? "trie" : "poseidonMerkle");

  it("every row is of its check's family", () => {
    for (const [check, o] of outcomes) if (o !== "accepted") expect(Object.keys(o), check).toContain(family(check));
  });

  const refused = outcomes.flatMap(([, o]) => (o === "accepted" ? [] : [o]));
  const vtRows = new Set<string>([...refused.flatMap((o) => ("versionedTrie" in o ? [o.versionedTrie] : [])), ...versionRows]);
  const merklePinned = new Set<string>([...refused.flatMap((o) => ("merkle" in o ? [o.merkle] : [])), ...merkleRows]);
  const triePinned = new Set<string>([
    ...refused.flatMap((o) => ("trie" in o ? [o.trie] : [])),
    ...refused.flatMap((o) => ("versionedTrie" in o && o.cause !== undefined && "trie" in o.cause ? [o.cause.trie] : [])),
    ...trieRows,
  ]);
  const poseidonPinned = new Set<string>(refused.flatMap((o) => ("poseidonMerkle" in o ? [o.poseidonMerkle] : [])));
  const families: readonly (readonly [string, Readonly<Record<string, Pin>>, Set<string>])[] = [
    ["versioned trie", vtKinds, vtRows],
    ["merkle", merkleKinds, merklePinned],
    ["trie", trieKinds, triePinned],
    ["poseidon", poseidonKinds, poseidonPinned],
  ];
  for (const [name, kinds, rows] of families) {
    it(`${name}: a row exactly for each kind not exempt`, () => {
      expect(Object.entries(kinds).filter(([k, pin]) => (pin === "row") !== rows.has(k))).toEqual([]);
      expect([...rows].filter((k) => !(k in kinds))).toEqual([]);
    });
  }
});
