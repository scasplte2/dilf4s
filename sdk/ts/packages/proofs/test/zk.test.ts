// The BN254 group operations against zk_v1.json (zk.md section 1, EIP-196/197), as the Rust SDK's zk tests run them:
// every add, mul and pairing row gives the row's point, verdict or error kind. Then the cases the rows leave implicit:
// each coordinate's canonical check, the identities, scalars past r, and what is not bytes.
import { runInNewContext } from "node:vm";

import { models } from "@kd5ujc/dilf4s-core";
import { bn254 } from "@noble/curves/bn254.js";
import { describe, expect, it } from "vitest";

import { zk } from "../src/index.js";
import { bytes, hexOf, loadVectors } from "./vectors.js";

const vectors = loadVectors("zk_v1.json", models.zkVectors.ZkVectors);

type Got = { ok: true; value: Uint8Array | boolean } | { ok: false; error: { kind: string } };

const outcome = (r: Got): string => (r.ok ? (typeof r.value === "boolean" ? `value ${String(r.value)}` : `point ${hexOf(r.value)}`) : `error ${r.error.kind}`);
const want = (r: models.zkVectors.ZkPointResult | models.zkVectors.ZkBoolResult): string =>
  "error" in r ? `error ${r.error}` : "point" in r ? `point ${r.point}` : `value ${String(r.value)}`;

const P = bn254.fields.Fp.ORDER;
const R = bn254.fields.Fr.ORDER;
const be32 = (n: bigint): string => n.toString(16).padStart(64, "0");
const G1 = bytes(be32(1n) + be32(2n));
const ZERO64 = new Uint8Array(64);
const ZERO128 = new Uint8Array(128);

describe("zk_v1.json", { timeout: 120_000 }, () => {
  it("every add row", () => {
    expect(vectors.add.length).toBeGreaterThan(0);
    for (const c of vectors.add) expect(outcome(zk.add(bytes(c.a), bytes(c.b))), c.name).toBe(want(c.result));
  });

  it("every mul row", () => {
    expect(vectors.mul.length).toBeGreaterThan(0);
    for (const c of vectors.mul) expect(outcome(zk.mul(bytes(c.point), bytes(c.scalar))), c.name).toBe(want(c.result));
  });

  it("every pairing row", () => {
    expect(vectors.pairing.length).toBeGreaterThan(0);
    for (const c of vectors.pairing) {
      const pairs = c.pairs.map((p) => [bytes(p.g1), bytes(p.g2)] as const);
      expect(outcome(zk.pairing(pairs)), c.name).toBe(want(c.result));
    }
  });
});

describe("BN254 encodings", { timeout: 120_000 }, () => {
  it("a coordinate at p is malformed, in either word of G1 and any of G2's four", () => {
    expect(outcome(zk.add(bytes(be32(P) + be32(2n)), G1)), "G1 x").toBe("error Malformed");
    expect(outcome(zk.add(bytes(be32(1n) + be32(P)), G1)), "G1 y").toBe("error Malformed");
    for (let i = 0; i < 4; i++) {
      const g2 = new Uint8Array(128);
      g2.set(bytes(be32(P)), i * 32);
      expect(outcome(zk.pairing([[G1, g2]])), `G2 word ${i.toString()}`).toBe("error Malformed");
    }
  });

  it("the identity: (0, 0) adds as zero, multiplies to zero, and drops out of a pairing", () => {
    expect(outcome(zk.add(ZERO64, G1))).toBe(`point ${hexOf(G1)}`);
    expect(outcome(zk.add(ZERO64, ZERO64))).toBe(`point ${hexOf(ZERO64)}`);
    expect(outcome(zk.mul(ZERO64, bytes(be32(5n))))).toBe(`point ${hexOf(ZERO64)}`);
    expect(outcome(zk.mul(G1, bytes(be32(0n))))).toBe(`point ${hexOf(ZERO64)}`);
    expect(outcome(zk.pairing([[ZERO64, ZERO128]]))).toBe("value true");
    expect(outcome(zk.pairing([]))).toBe("value true");
  });

  it("a scalar is any 256-bit value, taken mod r", () => {
    const two = zk.mul(G1, bytes(be32(2n)));
    expect(outcome(zk.mul(G1, bytes(be32(R + 2n))))).toBe(outcome(two));
    expect(outcome(zk.mul(G1, bytes(be32(R))))).toBe(`point ${hexOf(ZERO64)}`);
    expect(outcome(zk.add(G1, G1))).toBe(outcome(two));
    const max = zk.mul(G1, bytes("f".repeat(64)));
    expect(outcome(max)).toBe(outcome(zk.mul(G1, bytes(be32((2n ** 256n - 1n) % R)))));
  });

  it("a point off the curve is InvalidPoint; widths other than the encoding's are Malformed", () => {
    expect(outcome(zk.add(bytes(be32(1n) + be32(3n)), G1))).toBe("error InvalidPoint");
    expect(outcome(zk.add(G1.subarray(1), G1))).toBe("error Malformed");
    expect(outcome(zk.mul(G1, new Uint8Array(33)))).toBe("error Malformed");
    expect(outcome(zk.pairing([[G1, new Uint8Array(129)]]))).toBe("error Malformed");
  });

  it("e(P, Q) e(-P, Q) = 1, and e(P, Q) alone is not", () => {
    const q = bn254.G2.Point.BASE.toAffine();
    const g2 = bytes(be32(q.x.c1) + be32(q.x.c0) + be32(q.y.c1) + be32(q.y.c0));
    const minus = bytes(be32(1n) + be32(P - 2n));
    expect(outcome(zk.pairing([[G1, g2], [minus, g2]]))).toBe("value true");
    expect(outcome(zk.pairing([[G1, g2]]))).toBe("value false");
  });

  it("bytes are measured by their internal length; another realm's and shared bytes are bytes", () => {
    class Liar extends Uint8Array {
      override get length(): number {
        return 64;
      }
    }
    expect(outcome(zk.add(new Liar(G1.subarray(1)), G1)), "63 bytes claiming 64").toBe("error Malformed");
    expect(outcome(zk.add(new Liar(G1), G1))).toBe(outcome(zk.mul(G1, bytes(be32(2n)))));
    const foreign = runInNewContext(`Uint8Array.of(${[...G1].join(",")})`) as Uint8Array;
    const shared = new Uint8Array(new SharedArrayBuffer(64));
    shared.set(G1);
    expect(outcome(zk.add(foreign, shared))).toBe(outcome(zk.mul(G1, bytes(be32(2n)))));
    expect(outcome(zk.pairing([[foreign, ZERO128]]))).toBe("value true");
  });

  it("an array or pair that is revoked, or revokes itself while it is read, is a TypeMismatch, never thrown on", () => {
    const revoked = (): never => {
      const { proxy, revoke } = Proxy.revocable([], {});
      revoke();
      return proxy as never;
    };
    const revoking = <T extends object>(target: T): T => {
      const { proxy, revoke } = Proxy.revocable(target, {
        get: (t, name) => {
          const v: unknown = Reflect.get(t, name);
          revoke();
          return v;
        },
      });
      return proxy;
    };
    expect(outcome(zk.pairing(revoked())), "a revoked list").toBe("error TypeMismatch");
    expect(outcome(zk.pairing([revoked()])), "a revoked pair").toBe("error TypeMismatch");
    expect(outcome(zk.pairing(revoking([[G1, ZERO128], [G1, ZERO128]]))), "a list revoked by its first read").toBe("error TypeMismatch");
    expect(outcome(zk.pairing([revoking([G1, ZERO128])])), "a pair revoked by its first read").toBe("error TypeMismatch");
    const { proxy, revoke } = Proxy.revocable(new Uint8Array(64), {});
    revoke();
    expect(outcome(zk.add(proxy, G1)), "a revoked Proxy of bytes").toBe("error TypeMismatch");
  });

  it("what is not bytes, or not an array of pairs, is a TypeMismatch", () => {
    expect(outcome(zk.add("00" as never, G1))).toBe("error TypeMismatch");
    expect(outcome(zk.mul(G1, 5 as never))).toBe("error TypeMismatch");
    expect(outcome(zk.pairing({} as never))).toBe("error TypeMismatch");
    expect(outcome(zk.pairing([[G1, "00"]] as never))).toBe("error TypeMismatch");
    expect(outcome(zk.pairing([G1] as never))).toBe("error TypeMismatch");
  });
});
