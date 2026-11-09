// `shared/vectors/sigma_v1.json` (Ergo's proofs) and `sigma_dilf4s_v1.json` (the Scala prover's), both with Ergo's
// verdict on every case (`docs/specs/sigma.md`), `sigma_refusals_v1.json`'s calls and outcomes, then the malformed
// inputs of sections 1, 3 and 6, as Rust's `signing/tests/sigma_vectors.rs` checks them.
//
// Each case goes through the JLVM's path: the proposition in its canonical form, decoded by `Proposition.fromValue`.
// The verdicts are exact: a tampered challenge or message is `false`, never an error, and a proof of the wrong length
// is the `ProofLength` error (where Ergo ignores trailing bytes).
import { runInNewContext } from "node:vm";

import { describe, expect, it, test } from "vitest";

import { ArrayValue, BytesValue, cbor, IntValue, MapValue, models, TextValue, type Result, type Value } from "@kd5ujc/dilf4s-core";
import { secp256k1 } from "@noble/curves/secp256k1.js";

import { sigma, type SigmaError, type SigmaRefusal } from "../src/index.js";
import { expectOk, h, loadVectors } from "./vectors.js";

type SigmaCase = models.sigmaVectors.SigmaCase;
type SigmaProposition = models.sigmaVectors.SigmaProposition;
type Outcome = models.sigmaRefusalVectors.SigmaOutcome;

const { Proposition, MAX_DEPTH, MAX_LEAVES, MAX_WEIGHT } = sigma;

const files = {
  "sigma_v1.json": loadVectors("sigma_v1.json", models.sigmaVectors.SigmaVectors),
  "sigma_dilf4s_v1.json": loadVectors("sigma_dilf4s_v1.json", models.sigmaVectors.SigmaVectors),
} as const;
const refusals = loadVectors("sigma_refusals_v1.json", models.sigmaRefusalVectors.SigmaRefusalVectors);

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;

// ------------------------------------------------------------------ canonical values

const text = (s: string): Value => expectOk(TextValue.of(s), "text");
const bytes = (b: Uint8Array): Value => expectOk(BytesValue.of(b), "bytes");
const int = (n: bigint): Value => expectOk(IntValue.of(n), "an integer");
const array = (vs: readonly Value[]): Value => expectOk(ArrayValue.of(vs), "an array");
const map = (entries: readonly (readonly [string, Value])[]): Value => expectOk(MapValue.of(entries), "a map");

/** The canonical form of a vector proposition: its hex fields as bytes, as the JLVM holds them. */
function toValue(p: SigmaProposition): Value {
  const entries: (readonly [string, Value])[] = [["type", text(p.type)]];
  for (const key of ["pk", "g", "h", "u", "v"] as const) {
    const x = p[key];
    if (x !== undefined) entries.push([key, bytes(h(x))]);
  }
  if (p.k !== undefined) entries.push(["k", int(p.k)]);
  if (p.children !== undefined) entries.push(["children", array(p.children.map(toValue))]);
  return map(entries);
}

const generator = (): Uint8Array => h("0x0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798");

/** `02 ‖ x` with `x = 5`, which has no `y` on secp256k1. */
const offCurve = (): Uint8Array => Uint8Array.from({ length: 33 }, (_, i) => (i === 0 ? 2 : i === 32 ? 5 : 0));

const dlogOf = (pk: Uint8Array): Value => map([["type", text("dlog")], ["pk", bytes(pk)]]);
const dhTupleOf = (g: Uint8Array, hh: Uint8Array, u: Uint8Array, v: Uint8Array): Value =>
  map([["type", text("dhtuple")], ["g", bytes(g)], ["h", bytes(hh)], ["u", bytes(u)], ["v", bytes(v)]]);
const connective = (ty: string, children: readonly Value[]): Value => map([["type", text(ty)], ["children", array(children)]]);
const thresholdOf = (k: bigint, children: readonly Value[]): Value => map([["type", text("threshold")], ["k", int(k)], ["children", array(children)]]);

/** The refusal's kind as its name, or `Ok`. */
const kind = (r: Result<unknown, SigmaRefusal>): string => (r.ok ? "Ok" : r.error.kind);

/** The reason of an `InvalidProposition`, or the kind of any other outcome. */
function reason(v: Value): string {
  const r = Proposition.fromValue(v);
  return !r.ok && r.error.kind === "InvalidProposition" ? r.error.reason : kind(r);
}

/** The schema's outcome of a verification: a result, or the refusal's kind. */
const outcome = (r: Result<boolean, SigmaRefusal>): string => (r.ok ? (r.value ? "accepted" : "rejected") : r.error.kind);

// ------------------------------------------------------------------ the Ergo-checked files

interface Tally {
  cases: number;
  valid: number;
  rejected: number;
  responseOutOfRange: number;
  wrongLength: number;
  ergoAcceptsTrailing: number;
}

function check(c: SigmaCase, tally: Tally): void {
  const what = `${c.name} (${c.kind})`;
  const [proof, message] = [h(c.proof), h(c.message)];
  const value = toValue(c.proposition);
  const prop = expectOk(Proposition.fromValue(value), `${what}: proposition`);
  const ours = sigma.verifyValue(value, proof, message);
  expect(sigma.verify(prop, proof, message), `${what}: verifyValue and verify agree`).toEqual(ours);
  if (c.proposition.type === "dlog") expect(sigma.verifyDlogBytes(h(c.proposition.pk ?? ""), message, proof), `${what}: verifyDlogBytes agrees`).toEqual(ours);
  tally.cases += 1;
  if (c.kind === "valid") {
    expect(c.ergo, `${what}: Ergo accepts`).toBe(true);
    expect(ours, `${what}: accepted`).toEqual({ ok: true, value: true });
    expect(proof.length, `${what}: its proof's length`).toBe(prop.proofLength);
    tally.valid += 1;
  } else if (c.kind === "trailing-byte" || c.kind === "truncated") {
    const offBy = c.kind === "trailing-byte" ? prop.proofLength + 1 : prop.proofLength - 1;
    expect(ours, `${what}: malformed by its length`).toEqual({ ok: false, error: { kind: "ProofLength", expected: prop.proofLength, actual: offBy } });
    tally.wrongLength += 1;
    if (c.kind === "trailing-byte" && c.ergo) tally.ergoAcceptsTrailing += 1;
  } else {
    expect(c.ergo, `${what}: Ergo rejects`).toBe(false);
    if (c.kind === "flipped-response" && !ours.ok) {
      expect(ours.error, `${what}: a flipped response past the order`).toEqual({ kind: "ResponseOutOfRange" });
      tally.responseOutOfRange += 1;
    } else {
      expect(ours, `${what}: rejected`).toEqual({ ok: true, value: false });
      tally.rejected += 1;
    }
  }
}

describe("sigma_v1.json and sigma_dilf4s_v1.json: every case, Ergo's verdict", { timeout: 60_000 }, () => {
  for (const [file, vectors] of Object.entries(files)) {
    test(`${file}: 20 valid proofs, 60 tampered, 40 of the wrong length`, () => {
      const tally: Tally = { cases: 0, valid: 0, rejected: 0, responseOutOfRange: 0, wrongLength: 0, ergoAcceptsTrailing: 0 };
      for (const c of vectors.cases) check(c, tally);
      expect(tally.cases, `${file}: case count`).toBe(120);
      expect(tally.valid, `${file}: valid proofs`).toBe(20);
      expect(tally.wrongLength, `${file}: trailing and truncated proofs`).toBe(40);
      expect(tally.rejected + tally.responseOutOfRange, `${file}: tampered proofs`).toBe(60);
      expect(tally.ergoAcceptsTrailing, `${file}: Ergo ignores trailing bytes, so some trailing-byte case is true`).toBeGreaterThan(0);
    });
  }

  test("the vectors cover every connective and threshold shape", () => {
    const names = files["sigma_v1.json"].cases.map((c) => c.name);
    expect(names.some((n) => n.startsWith("ZeroJoin")), "ZeroJoin").toBe(true);
    expect(names.some((n) => n.includes("3-of-5")), "3-of-5").toBe(true);
  });

  test("the reference's outcome on every case of the two files, in order (ergoCases, dilf4sCases)", () => {
    for (const [file, outcomes] of [
      ["sigma_v1.json", refusals.ergoCases],
      ["sigma_dilf4s_v1.json", refusals.dilf4sCases],
    ] as const) {
      const cases = files[file].cases;
      expect(cases.length, `${file}: one outcome per case`).toBe(outcomes.length);
      cases.forEach((c, i) => {
        expect(outcome(sigma.verifyValue(toValue(c.proposition), h(c.proof), h(c.message))), `${file} case ${String(i)} ${c.name} (${c.kind})`).toBe(outcomes[i]);
      });
    }
  });
});

// ------------------------------------------------------------------ sigma_refusals_v1.json

describe("sigma_refusals_v1.json", { timeout: 60_000 }, () => {
  it.each(refusals.cases.map((c) => [c.name, c] as const))("%s", (_, c) => {
    // the proposition as canonical CBOR, so that a malformed one can be written
    const proposition = expectOk(cbor.decode(h(c.proposition)), `${c.name}: canonical CBOR`);
    expect(outcome(sigma.verifyValue(proposition, h(c.proof), h(c.message)))).toBe(c.outcome);
  });

  test("coverage (conformance.md section 6): every SigmaError kind and both results have a row", () => {
    // a Record over the SDK's kinds: a kind added to sigma-error does not compile until it is listed here
    const coverage: Record<SigmaError["kind"] | "accepted" | "rejected", Outcome> = {
      accepted: "accepted",
      rejected: "rejected",
      InvalidProposition: "InvalidProposition",
      InvalidPoint: "InvalidPoint",
      ProofLength: "ProofLength",
      ResponseOutOfRange: "ResponseOutOfRange",
    };
    // and every outcome the schema names is one of the SDK's
    const shape = models.sigmaRefusalVectors.SigmaOutcome.shape;
    const schema = shape.kind === "union" ? shape.texts : [];
    expect([...schema].sort()).toEqual(Object.values(coverage).sort());
    const pinned = new Set<string>(refusals.cases.map((c) => c.outcome));
    expect(Object.values(coverage).filter((k) => !pinned.has(k))).toEqual([]);
    expect(refusals.cases.length).toBe(20);
  });
});

// ------------------------------------------------------------------ malformed input

/** A valid `dlog` case of Ergo's file: its key, message and proof. */
function validDlog(): readonly [Uint8Array, Uint8Array, Uint8Array] {
  const c = files["sigma_v1.json"].cases.find((x) => x.kind === "valid" && x.proposition.type === "dlog");
  if (c === undefined) throw new Error("no valid dlog case");
  return [h(c.proposition.pk ?? ""), h(c.message), h(c.proof)];
}

describe("sigma: malformed input (sections 1, 3 and 6)", { timeout: 60_000 }, () => {
  it("group elements decode only in canonical form", () => {
    const g = generator();
    const decode = (pk: Uint8Array): string => kind(Proposition.fromValue(dlogOf(pk)));
    const uncompressed = h("0x0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8");
    const prefix04 = Uint8Array.from(g, (b, i) => (i === 0 ? 4 : b));
    const beyond = Uint8Array.from({ length: 33 }, (_, i) => (i === 0 ? 2 : 0xff));
    // x = p, the field modulus itself
    const atP = Uint8Array.from([2, ...h("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f")]);
    expect(decode(g)).toBe("Ok");
    expect(decode(uncompressed), "uncompressed").toBe("InvalidPoint");
    expect(decode(prefix04), "prefix 04 at 33 bytes").toBe("InvalidPoint");
    expect(decode(beyond), "x beyond the field").toBe("InvalidPoint");
    expect(decode(atP), "x = p").toBe("InvalidPoint");
    expect(decode(offCurve()), "x = 5 has no y").toBe("InvalidPoint");
    expect(decode(g.subarray(1)), "short").toBe("InvalidPoint");
    expect(decode(Uint8Array.from(g, (b, i) => (i === 0 ? 3 : b))), "-G").toBe("Ok");
    expect(Proposition.fromValue(dlogOf(beyond))).toEqual({ ok: false, error: { kind: "InvalidPoint", what: "pk", reason: "not a point of secp256k1: x is not below the field modulus" } });
    expect(Proposition.fromValue(dlogOf(g.subarray(1)))).toEqual({ ok: false, error: { kind: "InvalidPoint", what: "pk", reason: "a group element is 33 bytes, got 32" } });
    // a proof of dlog(G) is no proof of dlog(-G)
    const [, message, proof] = validDlog();
    expect(sigma.verifyDlogBytes(Uint8Array.from(g, (b, i) => (i === 0 ? 3 : b)), message, proof)).toEqual({ ok: true, value: false });
  });

  it("the identity is no statement point, and a tuple's points are all decoded before any is checked against it", () => {
    const [g, o] = [generator(), new Uint8Array(33)];
    expect(kind(Proposition.fromValue(dlogOf(o))), "dlog(O)").toBe("InvalidProposition");
    expect(reason(dhTupleOf(g, g, o, o)), "u = v = O").toBe("u is the identity, for which anyone can prove knowledge (w = 0)");
    expect(reason(dhTupleOf(o, g, g, g)), "an identity base").toBe("g is the identity, for which anyone can prove knowledge (w = 0)");
    expect(reason(dhTupleOf(o, g, g, offCurve())), "an identity g, then v off the curve").toBe("InvalidPoint");
    expect(sigma.verifyDlogBytes(o, Uint8Array.of(0x6d), new Uint8Array(56))).toEqual({ ok: false, error: { kind: "InvalidProposition", reason: "pk is the identity, for which anyone can prove knowledge (w = 0)" } });
    expect(kind(sigma.verifyDlogBytes(g.subarray(1), Uint8Array.of(0x6d), new Uint8Array(56))), "a short key").toBe("InvalidPoint");
  });

  it("connectives are bounded and never trivial", () => {
    const leaf = dlogOf(generator());
    const leaves = (n: number): Value[] => Array.from({ length: n }, () => leaf);
    const decode = (v: Value): string => kind(Proposition.fromValue(v));
    expect(decode(connective("and", [])), "empty and").toBe("InvalidProposition");
    expect(decode(connective("or", [])), "empty or").toBe("InvalidProposition");
    expect(decode(thresholdOf(0n, leaves(1))), "k = 0").toBe("InvalidProposition");
    expect(decode(thresholdOf(2n, leaves(1))), "k > n").toBe("InvalidProposition");
    expect(decode(connective("and", leaves(255))), "255 children").toBe("Ok");
    expect(decode(connective("and", leaves(256))), "256 children").toBe("InvalidProposition");
    expect(decode(thresholdOf(1n, leaves(256))), "a threshold of 256").toBe("InvalidProposition");

    const nest = (depth: number): Value => Array.from({ length: depth - 1 }).reduce<Value>((c) => connective("and", [c]), leaf);
    expect(expectOk(Proposition.fromValue(nest(MAX_DEPTH)), "64 deep").depth).toBe(64);
    expect(decode(nest(MAX_DEPTH + 1)), "65 deep").toBe("InvalidProposition");

    const wide = (groups: number, each: number): Value => connective("and", Array.from({ length: groups }, () => connective("and", leaves(each))));
    expect(expectOk(Proposition.fromValue(wide(16, 255)), "4080 leaves").leaves).toBe(4080);
    expect(decode(wide(17, 241)), "4097 leaves").toBe("InvalidProposition");
  });

  it("the canonical form takes exactly its type's fields", () => {
    const g = bytes(generator());
    const dlog = map([["type", text("dlog")], ["pk", g]]);
    const decode = (v: Value): string => kind(Proposition.fromValue(v));
    expect(decode(dlog)).toBe("Ok");
    expect(decode(text("dlog")), "not an object").toBe("InvalidProposition");
    expect(decode(map([["type", text("xor")], ["pk", g]])), "type").toBe("InvalidProposition");
    expect(decode(map([["pk", g]])), "no type").toBe("InvalidProposition");
    expect(decode(map([["type", text("dlog")], ["pk", g], ["g", g]])), "an extra field").toBe("InvalidProposition");
    expect(decode(map([["type", text("dlog")], ["pk", text("02")]])), "pk text").toBe("InvalidProposition");
    expect(decode(map([["type", text("dlog")], ["pk", bytes(offCurve())]]))).toBe("InvalidPoint");
    expect(decode(connective("and", [dlog])), "and").toBe("Ok");
    expect(decode(map([["type", text("and")], ["children", dlog]]))).toBe("InvalidProposition");
    const threshold = (k: Value): Value => map([["type", text("threshold")], ["k", k], ["children", array([dlog, dlog])]]);
    expect(decode(threshold(int(2n))), "2-of-2").toBe("Ok");
    expect(decode(threshold(int(3n))), "3-of-2").toBe("InvalidProposition");
    expect(decode(threshold(int(-1n))), "k = -1").toBe("InvalidProposition");
    expect(reason(threshold(int(1n << 40n))), "k beyond an Int").toBe("k must be an integer");
    expect(reason(threshold(int(2n ** 31n))), "k = 2^31").toBe("k must be an integer");
    expect(reason(threshold(int(-(2n ** 31n)))), "k = -2^31 fits, and is out of range").toBe("a threshold needs 1 <= k <= 2 children, got k = -2147483648");
    expect(reason(threshold(text("2"))), "k text").toBe("k must be an integer");
  });

  /** `n` children, the first `badCount` of them with an off-curve point. */
  const withBadFirst = (n: number, badCount: number): Value[] => Array.from({ length: n }, (_, i) => dlogOf(i < badCount ? offCurve() : generator()));

  /** `groups` `or`s of `each` leaves under an `and`, the last leaf replaced by `last`. */
  function grouped(groups: number, each: number, last: Value): Value {
    const leaves: Value[] = Array.from({ length: groups * each }, () => dlogOf(generator()));
    leaves[leaves.length - 1] = last;
    return connective("and", Array.from({ length: groups }, (_, i) => connective("or", leaves.slice(i * each, (i + 1) * each))));
  }

  it("a connective wider than 255 is refused before its children are decoded", () => {
    const tooWide = "a connective has at most 255 children";
    expect(reason(connective("and", withBadFirst(256, 1))), "256 children, child 0 off the curve").toBe(tooWide);
    expect(reason(connective("or", withBadFirst(256, 256))), "256 children, all off the curve").toBe(tooWide);
    expect(reason(thresholdOf(0n, withBadFirst(256, 1))), "the width before k = 0").toBe(tooWide);
    expect(reason(thresholdOf(1n, withBadFirst(256, 1))), "a threshold of 256").toBe(tooWide);
    // within the width, the children are decoded before the connective's other checks
    expect(reason(connective("and", withBadFirst(255, 1))), "255 children, child 0 off the curve").toBe("InvalidPoint");
    expect(reason(thresholdOf(0n, withBadFirst(2, 1))), "a bad child before k = 0").toBe("InvalidPoint");
    expect(reason(connective("and", []))).toBe("a connective needs at least one child: an empty one holds for anyone");
  });

  it("the 4097th leaf is refused before its points are decoded", () => {
    const tooMany = "a proposition has at most 4096 leaves";
    expect(reason(grouped(16, 255, dlogOf(generator()))), "4080 leaves").toBe("Ok");
    expect(reason(grouped(32, 128, dlogOf(offCurve()))), "4096 leaves, the last off the curve").toBe("InvalidPoint");
    expect(reason(grouped(32, 128, dlogOf(generator()))), "4096 leaves").toBe("Ok");
    expect(reason(grouped(17, 241, dlogOf(generator()))), "4097 leaves").toBe(tooMany);
    expect(reason(grouped(17, 241, dlogOf(offCurve()))), "4097 leaves, the last off the curve").toBe(tooMany);
    // a leaf's own fields are checked before the bound
    expect(reason(grouped(17, 241, map([["type", text("dlog")]]))), "4097 leaves, the last without its pk").toBe("dlog takes exactly the fields pk");
  });

  it("depth is checked once the children are decoded", () => {
    const chain = (leaf: Value): Value => Array.from({ length: MAX_DEPTH }).reduce<Value>((c) => connective("and", [c]), leaf);
    expect(reason(chain(dlogOf(generator()))), "65 deep").toBe("a proposition nests at most 64 deep");
    expect(reason(chain(dlogOf(offCurve()))), "65 deep over a point off the curve").toBe("InvalidPoint");
  });

  /**
   * The heaviest valid proposition (section 3): a one-`k` threshold over 255 more, which share 4096 chains of 61
   * one-`k` thresholds, each over a `dhtuple` leaf at depth 64.
   */
  function heaviest(): Value {
    const g = generator();
    const chain = Array.from({ length: 61 }).reduce<Value>((c) => thresholdOf(1n, [c]), dhTupleOf(g, g, g, g));
    return thresholdOf(1n, Array.from({ length: 255 }, (_, i) => thresholdOf(1n, Array.from({ length: i < 16 ? 17 : 16 }, () => chain))));
  }

  it("a proposition heavier than the heaviest valid one is refused before it is decoded", () => {
    const top = heaviest();
    expect(top.weight, "the heaviest valid proposition weighs exactly the bound").toBe(MAX_WEIGHT);
    const p = expectOk(Proposition.fromValue(top), "the heaviest");
    expect([p.depth, p.leaves]).toEqual([MAX_DEPTH, MAX_LEAVES]);
    expect(p.proofLength, "the root challenge, a response per leaf and n - k coefficients per threshold").toBe(24 + 4096 * 32 + 24 * (255 - 1) + 24 * (16 * (17 - 1) + 239 * (16 - 1)));

    // a dlog weighs 15 plus its key's length
    const at = (n: bigint): Value => dlogOf(new Uint8Array(Number(n)).fill(2));
    expect(at(MAX_WEIGHT - 15n).weight).toBe(MAX_WEIGHT);
    expect(reason(at(MAX_WEIGHT - 15n)), "at the bound, the key is decoded (and refused by its length)").toBe("InvalidPoint");
    const over = "a proposition weighs at most 7900416";
    expect(reason(at(MAX_WEIGHT - 14n)), "one over the bound, before the key is decoded").toBe(over);
    expect(reason(connective("and", [dlogOf(offCurve()), dlogOf(new Uint8Array(7_900_400).fill(2))])), "before the first child").toBe(over);
    expect(reason(text("x".repeat(7_900_416))), "before the type").toBe(over);
    expect(reason(text("x".repeat(7_900_415))), "a value at the bound").toBe("a proposition is an object");
  });

  it("a deep canonical form is malformed, not a stack overflow", () => {
    const deep = Array.from({ length: 120 }).reduce<Value>((c) => connective("or", [c]), dlogOf(generator()));
    expect(kind(Proposition.fromValue(deep))).toBe("InvalidProposition");
  });

  it("proofs of the wrong length or with z beyond the order are malformed", () => {
    const [pk, message, proof] = validDlog();
    expect(sigma.verifyDlogBytes(pk, message, proof)).toEqual({ ok: true, value: true });
    expect(sigma.verifyDlogBytes(pk, message, proof.subarray(0, 55))).toEqual({ ok: false, error: { kind: "ProofLength", expected: 56, actual: 55 } });
    expect(sigma.verifyDlogBytes(pk, message, new Uint8Array(0))).toEqual({ ok: false, error: { kind: "ProofLength", expected: 56, actual: 0 } });
    const high = Uint8Array.from(proof, (b, i) => (i >= 24 ? 0xff : b));
    expect(sigma.verifyDlogBytes(pk, message, high)).toEqual({ ok: false, error: { kind: "ResponseOutOfRange" } });
    // q itself is out of range; q - 1 is a response
    const q = h("0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141");
    const atQ = Uint8Array.from(proof);
    atQ.set(q, 24);
    expect(sigma.verifyDlogBytes(pk, message, atQ)).toEqual({ ok: false, error: { kind: "ResponseOutOfRange" } });
    atQ[55] = 0x40;
    expect(sigma.verifyDlogBytes(pk, message, atQ)).toEqual({ ok: true, value: false });
  });

  it("zero responses and zero challenges are well formed: a rejection, never a refusal or a throw", () => {
    const [pk, message] = validDlog();
    const g = generator();
    expect(sigma.verifyDlogBytes(pk, message, new Uint8Array(56)), "z = 0 and e = 0").toEqual({ ok: true, value: false });
    for (const value of [dhTupleOf(g, pk, pk, g), connective("or", [dlogOf(pk), dlogOf(g)]), thresholdOf(1n, [dlogOf(pk), dhTupleOf(g, g, pk, pk)])]) {
      const p = expectOk(Proposition.fromValue(value), "proposition");
      expect(sigma.verify(p, new Uint8Array(p.proofLength), message)).toEqual({ ok: true, value: false });
    }
  });

  it("a proof is bound to its proposition", () => {
    const [pk, message, proof] = validDlog();
    const leaf = dlogOf(pk);
    const and = expectOk(Proposition.fromValue(connective("and", [leaf])), "and");
    expect(and.proofLength).toBe(56);
    expect(sigma.verify(and, proof, message), "and(dlog) has its own transcript").toEqual({ ok: true, value: false });
    const or = expectOk(Proposition.fromValue(connective("or", [leaf, leaf])), "or");
    expect(or.proofLength).toBe(24 + 24 + 64);
    const twoOfThree = expectOk(Proposition.fromValue(thresholdOf(2n, [leaf, leaf, leaf])), "2-of-3");
    expect(twoOfThree.proofLength).toBe(24 + 24 + 96);
    expect(kind(sigma.verify(or, proof, message))).toBe("ProofLength");
  });
});

// ------------------------------------------------------------------ nothing recurses

const stackDepth = (): number => (new Error().stack ?? "").split("\n").length;

/**
 * The deepest native stack at which `f` calls `Uint8Array.prototype.subarray`: the probe the decoder (a point's `x`)
 * and the proof walk (each challenge and response it reads) call at the bottom of a deep proposition. A walk that
 * recursed would call it deeper, a frame or more per level. (Core's values and the package's classes are frozen.)
 */
function deepestSubarray(f: () => unknown): number {
  const proto = Object.getPrototypeOf(Uint8Array.prototype) as object;
  const descriptor = Object.getOwnPropertyDescriptor(proto, "subarray");
  const original = descriptor?.value as (this: Uint8Array, ...args: number[]) => Uint8Array;
  let deepest = 0;
  const limit = Error.stackTraceLimit;
  Error.stackTraceLimit = Infinity;
  Object.defineProperty(proto, "subarray", {
    configurable: true,
    writable: true,
    value: function (this: Uint8Array, ...args: number[]) {
      deepest = Math.max(deepest, stackDepth());
      return original.apply(this, args);
    },
  });
  try {
    f();
  } finally {
    if (descriptor !== undefined) Object.defineProperty(proto, "subarray", descriptor);
    Error.stackTraceLimit = limit;
  }
  return deepest;
}

describe("sigma: decoding and verifying are iterative", { timeout: 60_000 }, () => {
  const [pk, message] = validDlog();
  /** `n` levels alternating `and` and `or`, each a leaf beside the rest. */
  const nested = (n: number): Value => Array.from({ length: n - 1 }).reduce<Value>((c, _, i) => connective(i % 2 === 0 ? "and" : "or", [dlogOf(pk), c]), dlogOf(pk));
  const work = (v: Value): number =>
    deepestSubarray(() => {
      const p = expectOk(Proposition.fromValue(v), "proposition");
      expect(sigma.verify(p, new Uint8Array(p.proofLength).fill(1), message)).toEqual({ ok: true, value: false });
    });

  it("a proposition 64 deep is decoded and verified at the native stack depth of one 2 deep", () => {
    const shallow = work(nested(2));
    expect(shallow, "the probe ran").toBeGreaterThan(0);
    expect(work(nested(MAX_DEPTH))).toBe(shallow);
  });

  it("a canonical form 120 deep is walked to its leaf at the depth of one 2 deep, then refused", () => {
    const chain = (n: number): Value => Array.from({ length: n - 1 }).reduce<Value>((c) => connective("or", [c]), dlogOf(pk));
    const decodeDepth = (v: Value): number => deepestSubarray(() => Proposition.fromValue(v));
    expect(decodeDepth(chain(120))).toBe(decodeDepth(chain(2)));
    expect(reason(chain(120))).toBe("a proposition nests at most 64 deep");
  });
});

// ------------------------------------------------------------------ caller types and the byte boundary

describe("sigma: what a caller passes past the types", { timeout: 60_000 }, () => {
  const [pk, message, proof] = validDlog();
  const prop = expectOk(Proposition.fromValue(dlogOf(pk)), "dlog");
  const notBytes = { ok: false, error: { kind: "TypeMismatch", reason: "expected a Uint8Array" } };
  const notBytesCases = (): unknown[] => [null, undefined, 0, "", {}, { length: 0 }, [], new Uint16Array(56), Array.from(proof)];

  it("arguments of the wrong type are a TypeMismatch, never a verdict, checked in argument order", () => {
    for (const x of notBytesCases()) {
      expect(sigma.verify(prop, bad(x), message), `a proof: ${String(x)}`).toEqual(notBytes);
      expect(sigma.verify(prop, proof, bad(x)), `a message: ${String(x)}`).toEqual(notBytes);
      expect(sigma.verifyValue(dlogOf(pk), bad(x), message)).toEqual(notBytes);
      expect(sigma.verifyValue(dlogOf(pk), proof, bad(x))).toEqual(notBytes);
      expect(sigma.verifyDlogBytes(bad(x), message, proof)).toEqual(notBytes);
      expect(sigma.verifyDlogBytes(pk, bad(x), proof)).toEqual(notBytes);
      expect(sigma.verifyDlogBytes(pk, message, bad(x))).toEqual(notBytes);
    }
    // the types first: a malformed proposition with a proof that is not bytes is a TypeMismatch
    expect(sigma.verifyValue(dlogOf(new Uint8Array(33)), bad(null), message)).toEqual(notBytes);
    expect(sigma.verifyDlogBytes(new Uint8Array(33), message, bad(null))).toEqual(notBytes);
  });

  it("a proposition must be one this package decoded, and a value one core made", () => {
    const notProposition = { ok: false, error: { kind: "TypeMismatch", reason: "expected a sigma Proposition" } };
    const fakes: unknown[] = [null, {}, dlogOf(pk), Object.create(Proposition.prototype), Reflect.construct(Proposition, [Symbol("token"), [], 1, 1]), new Proxy(prop, {})];
    for (const f of fakes) expect(sigma.verify(bad(f), proof, message)).toEqual(notProposition);
    const notValue = { ok: false, error: { kind: "TypeMismatch", reason: "expected a canonical value" } };
    for (const f of [null, {}, { kind: "map", weight: 1n }, prop, new Proxy(dlogOf(pk), {})]) {
      expect(Proposition.fromValue(bad(f))).toEqual(notValue);
      expect(sigma.verifyValue(bad(f), proof, message)).toEqual(notValue);
    }
  });

  it("a proof is measured before it is read: one past its length is refused by that length, uncopied", () => {
    const huge = new Uint8Array(new SharedArrayBuffer(16 << 20));
    expect(sigma.verify(prop, huge, message)).toEqual({ ok: false, error: { kind: "ProofLength", expected: 56, actual: 16 << 20 } });
    expect(sigma.verifyValue(dlogOf(pk), huge, message)).toEqual({ ok: false, error: { kind: "ProofLength", expected: 56, actual: 16 << 20 } });
    expect(sigma.verifyDlogBytes(pk, message, huge)).toEqual({ ok: false, error: { kind: "ProofLength", expected: 56, actual: 16 << 20 } });
    expect(sigma.verifyDlogBytes(new Uint8Array(1 << 20), message, proof)).toEqual({ ok: false, error: { kind: "InvalidPoint", what: "pk", reason: "a group element is 33 bytes, got 1048576" } });
  });

  it("bytes are read by their internal length; another realm's and shared bytes are bytes", () => {
    class Liar extends Uint8Array {
      override get length(): number {
        return 0;
      }
    }
    const foreign = (b: Uint8Array): Uint8Array => runInNewContext(`Uint8Array.of(${Array.from(b).join(",")})`) as Uint8Array;
    const shared = (b: Uint8Array): Uint8Array => {
      const v = new Uint8Array(new SharedArrayBuffer(b.length));
      v.set(b);
      return v;
    };
    const accepted = { ok: true, value: true };
    expect(sigma.verify(prop, new Liar(proof), new Liar(message))).toEqual(accepted);
    expect(sigma.verifyDlogBytes(new Liar(pk), new Liar(message), new Liar(proof))).toEqual(accepted);
    expect(sigma.verifyDlogBytes(foreign(pk), foreign(message), foreign(proof))).toEqual(accepted);
    expect(sigma.verifyDlogBytes(shared(pk), shared(message), shared(proof))).toEqual(accepted);
    expect(sigma.verifyValue(dlogOf(pk), shared(proof), foreign(message))).toEqual(accepted);
  });
});

// ------------------------------------------------------------------ work and timings

/** `n` distinct keys `i·G`, `i` from 1. */
const keys = (n: number): Uint8Array[] => Array.from({ length: n }, (_, i) => secp256k1.Point.BASE.multiply(BigInt(i + 1)).toBytes(true));

/**
 * A well-formed proof of `p`: its exact length, every byte below `0x80` (xorshift32), so every response is below `q`
 * and no challenge is zero. It runs the whole walk, every commitment and the hash, and is rejected: the same work as
 * an accepted proof.
 */
function wellFormed(p: sigma.Proposition): Uint8Array {
  let state = 0x9e3779b9;
  return Uint8Array.from({ length: p.proofLength }, () => {
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return state & 0x7f;
  });
}

describe("sigma: well-formed proofs of larger shapes", { timeout: 60_000 }, () => {
  const ks = keys(255);
  const [g, hh, u, v] = [ks[0], ks[1], ks[2], ks[3]] as [Uint8Array, Uint8Array, Uint8Array, Uint8Array];
  const sixteen = ks.slice(0, 16);
  const shapes: readonly (readonly [string, Value])[] = [
    ["one dlog leaf", dlogOf(g)],
    ["one dhtuple leaf", dhTupleOf(g, hh, u, v)],
    ["an 8-of-16 threshold", thresholdOf(8n, sixteen.map(dlogOf))],
    ["a 1-of-16 threshold", thresholdOf(1n, sixteen.map(dlogOf))],
    // 64 levels alternating and and or, each a leaf beside the rest: 64 leaves
    ["and/or 64 deep", ks.slice(1, 64).reduce<Value>((c, k, i) => connective(i % 2 === 0 ? "and" : "or", [dlogOf(k), c]), dlogOf(g))],
    // the most GF(2^192) work one node does: 255 evaluations of a polynomial of degree 254
    ["a 1-of-255 threshold", thresholdOf(1n, ks.map(dlogOf))],
  ];

  it.each(shapes)("%s: a rejection, not a refusal", (_, value) => {
    const p = expectOk(Proposition.fromValue(value), "proposition");
    expect(sigma.verify(p, wellFormed(p), Uint8Array.of(1))).toEqual({ ok: true, value: false });
  });

  it("timings (supporting evidence, never asserted)", () => {
    const rows = shapes.map(([name, value]) => {
      const p = expectOk(Proposition.fromValue(value), name);
      const proof = wellFormed(p);
      const runs = 5;
      const decodeStart = performance.now();
      for (let i = 0; i < runs; i++) expectOk(Proposition.fromValue(value), name);
      const decoded = (performance.now() - decodeStart) / runs;
      const start = performance.now();
      for (let i = 0; i < runs; i++) sigma.verify(p, proof, Uint8Array.of(1));
      const verified = (performance.now() - start) / runs;
      return `${name} (depth ${String(p.depth)}, ${String(p.leaves)} leaves, ${String(p.proofLength)}-byte proof): decode ${decoded.toFixed(2)} ms, verify ${verified.toFixed(2)} ms`;
    });
    console.log(rows.join("\n"));
    expect(rows.length).toBe(shapes.length);
  });
});
