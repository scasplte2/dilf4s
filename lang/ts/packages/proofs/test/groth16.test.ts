// Groth16 and SP1's wrapper (zk.md section 2) against zk_v1.json, as the Rust SDK's zk tests run them: every sp1Groth16
// row gives the row's verdict or refusal kind. Then what the rows leave implicit: the key's checks, the general verifier
// on the rows' own proofs, the reference's refusal order where two refusals meet, and what is not of its type.
import { runInNewContext } from "node:vm";

import { models, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { bn254 } from "@noble/curves/bn254.js";
import { describe, expect, it } from "vitest";

import { zk } from "../src/index.js";
import { bytes, expectOk, loadVectors } from "./vectors.js";

const { groth16 } = zk;
const { sp1 } = groth16;

const vectors = loadVectors("zk_v1.json", models.zkVectors.ZkVectors);
const rows = vectors.sp1Groth16;

type Got = { ok: true; value: undefined } | { ok: false; error: { kind: string } };
const outcome = (r: Got): string => (r.ok ? "verified" : r.error.kind);
/** A refusal's reason: not normative, but it names which check refused, so a test can pin their order. */
const reasonOf = (r: Result<unknown, zk.groth16.Groth16Error | TypeMismatch>): string | undefined => (r.ok || !("reason" in r.error) ? undefined : r.error.reason);

const P = bn254.fields.Fp.ORDER;
const R = bn254.fields.Fr.ORDER;
const be32 = (n: bigint): string => n.toString(16).padStart(64, "0");
const G1 = bytes(be32(1n) + be32(2n));
const q = bn254.G2.Point.BASE.toAffine();
const G2 = bytes(be32(q.x.c1) + be32(q.x.c0) + be32(q.y.c1) + be32(q.y.c0));
const ZERO64 = new Uint8Array(64);
const ZERO128 = new Uint8Array(128);

const vk = expectOk(sp1.verifyingKey(), "SP1's verifying key");
const [premium] = rows;
if (premium === undefined) throw new Error("zk_v1.json has sp1Groth16 rows");

/** A row's proof (its last 256 bytes) and the public inputs SP1 derives for it. */
function general(row: models.zkVectors.ZkSp1Groth16): { proof: Uint8Array; inputs: bigint[] } {
  const framed = bytes(row.proofBytes);
  const word = (i: number): bigint => BigInt(`0x${row.proofBytes.slice(8 + 64 * i, 8 + 64 * (i + 1))}`);
  const digest = expectOk(sp1.hashPublicValues(bytes(row.publicValues)), "the public values' digest");
  return { proof: framed.subarray(100), inputs: [BigInt(`0x${row.programVKey}`), digest, word(0), word(1), word(2)] };
}

/** `b` with `bytes` written at `at`. */
function patched(b: Uint8Array, at: number, with_: Uint8Array): Uint8Array {
  const out = new Uint8Array(b);
  out.set(with_, at);
  return out;
}

describe("zk_v1.json sp1Groth16", { timeout: 120_000 }, () => {
  it("every row, under a key built once", () => {
    expect(rows).toHaveLength(16);
    expect(vk.publicInputs).toBe(5);
    for (const c of rows) expect(outcome(sp1.verifyWith(vk, bytes(c.programVKey), bytes(c.publicValues), bytes(c.proofBytes))), c.name).toBe(c.result);
  });

  it("sp1.verify builds its own key", () => {
    expect(sp1.verify(bytes(premium.programVKey), bytes(premium.publicValues), bytes(premium.proofBytes))).toEqual({ ok: true, value: undefined });
  });

  it("every Groth16Error kind has a row, and a row verifies", () => {
    const shape = models.cryptoErrors.Groth16ErrorKind.shape;
    const kinds = shape.kind === "union" ? shape.texts : [];
    expect(kinds).toEqual(["Malformed", "InvalidProof", "PairingCheckFailed", "Rejected"]);
    for (const k of kinds) expect(rows.some((c) => c.result === k), `a row with ${k}`).toBe(true);
    expect(rows.some((c) => c.result === "verified")).toBe(true);
  });

  it("the public values' digest is below 2^253, and its constants are SP1's", () => {
    const h = expectOk(sp1.hashPublicValues(new TextEncoder().encode("any public values")), "a digest");
    expect(h < 1n << 253n).toBe(true);
    expect(sp1.SELECTOR).toBe(0x4388a21c);
    expect(sp1.PROOF_BYTES).toBe(356);
    expect(sp1.VK_ROOT).toBe(0x002f850ee998974d6cc00e50cd0814b098c05bfade466d28573240d057f25352n);
    expect(premium.proofBytes.slice(0, 8)).toBe(sp1.SELECTOR.toString(16));
  });
});

describe("groth16.verify", { timeout: 120_000 }, () => {
  it("verifies each row's proof for SP1's public inputs, and refuses as the SP1 rows do", () => {
    for (const c of rows.filter((r) => r.result === "verified" || r.result === "PairingCheckFailed" || r.result === "InvalidProof")) {
      if (bytes(c.programVKey).length !== 32 || bytes(c.proofBytes).length !== 356) continue;
      const { proof, inputs } = general(c);
      expect(outcome(groth16.verify(vk, proof, inputs)), c.name).toBe(c.result);
    }
  });

  it("each input must be in [0, r), and there must be one per IC point after IC_0", () => {
    const { proof, inputs } = general(premium);
    const [x0, x1, x2, x3] = inputs as [bigint, bigint, bigint, bigint, bigint];
    expect(outcome(groth16.verify(vk, proof, [R, x1, x2, x3, 0n]))).toBe("InvalidProof");
    expect(outcome(groth16.verify(vk, proof, [-1n, x1, x2, x3, 0n]))).toBe("InvalidProof");
    expect(groth16.verify(vk, proof, [x0, x1, x2, x3])).toMatchObject({ ok: false, error: { kind: "Malformed", reason: "expected 5 public inputs, got 4" } });
    expect(outcome(groth16.verify(vk, proof, [...inputs, 0n]))).toBe("Malformed");
    // the count is refused from the length, before any input is read
    const reads: PropertyKey[] = [];
    const counting = new Proxy([1n, 2n, 3n], { get: (t, k, r) => (reads.push(k), Reflect.get(t, k, r) as unknown) });
    expect(outcome(groth16.verify(vk, proof, counting))).toBe("Malformed");
    expect(reads).toEqual(["length"]);
  });

  it("refuses in the reference's order where two refusals meet", () => {
    const { proof, inputs } = general(premium);
    const offCurve = patched(proof, 32, bytes(be32(3n)));
    const bNonCanonical = patched(proof, 64, bytes(be32(P)));
    const cNonCanonical = patched(proof, 192, bytes(be32(P)));
    const aAtP = patched(proof, 0, bytes(be32(P)));
    // the proof's width, then the identities, then A's coordinates, all before the inputs
    expect(reasonOf(groth16.verify(vk, proof.subarray(1), [1n]))).toBe("a proof is 256 bytes, got 255");
    expect(outcome(groth16.verify(vk, patched(proof, 0, ZERO64), [R]))).toBe("InvalidProof");
    expect(reasonOf(groth16.verify(vk, patched(patched(proof, 0, ZERO64), 64, bytes(be32(P))), inputs))).toBe("A is the identity");
    expect(reasonOf(groth16.verify(vk, aAtP, [1n]))).toBe("A.x is not a canonical field element");
    // the inputs before any point is decoded: an input not in Fr before a malformed B
    expect(outcome(groth16.verify(vk, bNonCanonical, [R, ...inputs.slice(1)]))).toBe("InvalidProof");
    // -A, then B, then C: A off the curve before a malformed B, a malformed B before a malformed C
    expect(outcome(groth16.verify(vk, patched(offCurve, 64, bytes(be32(P))), inputs))).toBe("InvalidProof");
    expect(outcome(groth16.verify(vk, bNonCanonical, inputs))).toBe("Malformed");
    expect(reasonOf(groth16.verify(vk, patched(bNonCanonical, 192, bytes(be32(P))), inputs))).toBe("G2 x.imag is not a canonical field element");
    expect(reasonOf(groth16.verify(vk, cNonCanonical, inputs))).toBe("G1 x is not a canonical field element");
  });

  it("a proof is bytes, a key one this module built, the inputs an array of bigints: else a TypeMismatch", () => {
    const { proof, inputs } = general(premium);
    const forged = Reflect.construct(groth16.VerifyingKey, [Symbol("not the token"), {}]) as zk.groth16.VerifyingKey;
    expect(forged.publicInputs).toBe(0);
    expect(outcome(groth16.verify(forged, proof, []))).toBe("TypeMismatch");
    expect(outcome(groth16.verify({ publicInputs: 5 } as never, proof, inputs))).toBe("TypeMismatch");
    expect(outcome(groth16.verify(vk, "00" as never, inputs))).toBe("TypeMismatch");
    expect(outcome(groth16.verify(vk, proof, inputs.map(Number) as never))).toBe("TypeMismatch");
    expect(outcome(groth16.verify(vk, proof, 5 as never))).toBe("TypeMismatch");
    const { proxy, revoke } = Proxy.revocable(inputs, {});
    revoke();
    expect(outcome(groth16.verify(vk, proof, proxy))).toBe("TypeMismatch");
  });

  it("bytes are measured by their internal length; another realm's and shared bytes are bytes", () => {
    const { proof, inputs } = general(premium);
    class Liar extends Uint8Array {
      override get length(): number {
        return 256;
      }
    }
    expect(outcome(groth16.verify(vk, new Liar(proof.subarray(1)), inputs)), "255 bytes claiming 256").toBe("Malformed");
    expect(outcome(groth16.verify(vk, new Liar(proof), inputs))).toBe("verified");
    const foreign = runInNewContext(`Uint8Array.of(${[...proof].join(",")})`) as Uint8Array;
    expect(outcome(groth16.verify(vk, foreign, inputs))).toBe("verified");
    const shared = new Uint8Array(new SharedArrayBuffer(356));
    shared.set(bytes(premium.proofBytes));
    expect(outcome(sp1.verifyWith(vk, bytes(premium.programVKey), bytes(premium.publicValues), shared))).toBe("verified");
  });

  it("the proof is copied before the inputs are read: a getter changing it changes nothing", () => {
    const { proof, inputs } = general(premium);
    const mine = new Uint8Array(proof);
    const changing = new Proxy(inputs, {
      get: (t, k, r) => {
        if (k === "0") mine.fill(0);
        return Reflect.get(t, k, r) as unknown;
      },
    });
    expect(outcome(groth16.verify(vk, mine, changing))).toBe("verified");
    expect(mine.every((x) => x === 0)).toBe(true);
  });
});

describe("groth16.VerifyingKey", { timeout: 120_000 }, () => {
  it("finite points make a key; an identity anywhere in it is Malformed", () => {
    const key = expectOk(groth16.VerifyingKey.of(G1, G2, G2, G2, [G1, G1]), "a key of finite points");
    expect(key.publicInputs).toBe(1);
    expect(Object.isFrozen(key)).toBe(true);
    const cases: readonly (readonly [string, Uint8Array, Uint8Array, Uint8Array, Uint8Array, Uint8Array[]])[] = [
      ["alpha", ZERO64, G2, G2, G2, [G1, G1]],
      ["beta", G1, ZERO128, G2, G2, [G1, G1]],
      ["gamma", G1, G2, ZERO128, G2, [G1, G1]],
      ["delta", G1, G2, G2, ZERO128, [G1, G1]],
      ["IC_0", G1, G2, G2, G2, [ZERO64, G1]],
      ["an IC point", G1, G2, G2, G2, [G1, ZERO64]],
    ];
    for (const [role, a, b, g, d, ic] of cases) {
      expect(groth16.VerifyingKey.of(a, b, g, d, ic), role).toEqual({ ok: false, error: { kind: "Malformed", reason: `the key's ${role} is the identity` } });
    }
    // the key that would accept A = P, B = Q, C = P for every input: alpha, beta, gamma and IC all the identity
    expect(outcome(groth16.VerifyingKey.of(ZERO64, ZERO128, ZERO128, G2, [ZERO64, ZERO64]) as never)).toBe("Malformed");
  });

  it("IC holds IC_0, checked first; a wrong width or a non-canonical coordinate is Malformed, a point off the curve or outside the subgroup InvalidProof", () => {
    expect(groth16.VerifyingKey.of(ZERO64, G2, G2, G2, [])).toEqual({ ok: false, error: { kind: "Malformed", reason: "IC needs IC_0" } });
    expect(outcome(groth16.VerifyingKey.of(G1.subarray(1), G2, G2, G2, [G1]) as never)).toBe("Malformed");
    expect(outcome(groth16.VerifyingKey.of(G1, G2, G2, G2, [new Uint8Array(65)]) as never)).toBe("Malformed");
    expect(outcome(groth16.VerifyingKey.of(bytes(be32(P) + be32(2n)), G2, G2, G2, [G1]) as never)).toBe("Malformed");
    expect(outcome(groth16.VerifyingKey.of(bytes(be32(1n) + be32(3n)), G2, G2, G2, [G1]) as never)).toBe("InvalidProof");
    // a point on the twist outside the order-r subgroup (zk_v1.json's "B outside the subgroup" row's B)
    const outside = rows.find((c) => c.name === "B outside the subgroup");
    if (outside === undefined) throw new Error("the row");
    const b = bytes(outside.proofBytes).subarray(164, 292);
    expect(outcome(groth16.VerifyingKey.of(G1, b, G2, G2, [G1]) as never)).toBe("InvalidProof");
  });

  it("what is not bytes, or an array of bytes, is a TypeMismatch", () => {
    expect(outcome(groth16.VerifyingKey.of("00" as never, G2, G2, G2, [G1]) as never)).toBe("TypeMismatch");
    expect(outcome(groth16.VerifyingKey.of(G1, G2, G2, G2, G1 as never) as never)).toBe("TypeMismatch");
    expect(outcome(groth16.VerifyingKey.of(G1, G2, G2, G2, ["00"] as never) as never)).toBe("TypeMismatch");
    const { proxy, revoke } = Proxy.revocable([G1], {});
    revoke();
    expect(outcome(groth16.VerifyingKey.of(G1, G2, G2, G2, proxy) as never)).toBe("TypeMismatch");
  });
});

describe("sp1", { timeout: 120_000 }, () => {
  it("the widths, then the selector, the exit code and the VK root, before the proof", () => {
    const [key, values, framed] = [bytes(premium.programVKey), bytes(premium.publicValues), bytes(premium.proofBytes)];
    expect(reasonOf(sp1.verifyWith(vk, new Uint8Array(33), values, framed.subarray(1)))).toBe("programVKey must be 32 bytes, got 33");
    expect(reasonOf(sp1.verifyWith(vk, key, values, new Uint8Array(357)))).toBe("proofBytes must be 356 bytes, got 357");
    const noSelector = patched(framed, 0, new Uint8Array(4));
    expect(reasonOf(sp1.verifyWith(vk, key, values, patched(noSelector, 4, Uint8Array.of(1))))).toBe("wrong verifier selector");
    expect(reasonOf(sp1.verifyWith(vk, key, values, patched(patched(framed, 4, Uint8Array.of(1)), 36, new Uint8Array(32))))).toBe("non-zero exit code");
    expect(reasonOf(sp1.verifyWith(vk, key, values, patched(patched(framed, 36, new Uint8Array(32)), 100, ZERO64)))).toBe("wrong VK root");
    // a framing that passes, then the proof: a nonce not in Fr is the proof's InvalidProof
    expect(outcome(sp1.verifyWith(vk, key, values, patched(framed, 68, bytes(be32(R)))))).toBe("InvalidProof");
    // under a key of another number of inputs, SP1's five are the wrong number
    const other = expectOk(groth16.VerifyingKey.of(G1, G2, G2, G2, [G1, G1]), "a one-input key");
    expect(reasonOf(sp1.verifyWith(other, key, values, framed))).toBe("expected 1 public inputs, got 5");
  });

  it("what is not bytes, or a key, is a TypeMismatch", () => {
    const [key, values, framed] = [bytes(premium.programVKey), bytes(premium.publicValues), bytes(premium.proofBytes)];
    expect(outcome(sp1.verifyWith({} as never, key, values, framed))).toBe("TypeMismatch");
    expect(outcome(sp1.verifyWith(vk, "00" as never, values, framed))).toBe("TypeMismatch");
    expect(outcome(sp1.verifyWith(vk, key, [1] as never, framed))).toBe("TypeMismatch");
    expect(outcome(sp1.verifyWith(vk, key, values, null as never))).toBe("TypeMismatch");
    expect(outcome(sp1.verify(key, values, 5 as never))).toBe("TypeMismatch");
    expect(sp1.hashPublicValues("00" as never)).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
  });
});
