// GF(2^192) as Rust's `sigma/gf2_192.rs` tests it, plus an independent reference: carry-less multiplication of the two
// elements as `bigint` polynomials, reduced modulo the pentanomial.
import { describe, expect, it } from "vitest";

import { add, BYTES, evaluate, fromByte, fromBytes, mul, mulByte, toBytes, ZERO, type Gf2_192 } from "../src/internal/gf2_192.js";
import { bigToLE, leToBig } from "../src/internal/bytes.js";

const xPow = (n: number): Gf2_192 => Array.from({ length: n }).reduce<Gf2_192>((p) => mul(p, fromByte(2)), fromByte(1));
const bytes = (f: (i: number) => number): Uint8Array => Uint8Array.from({ length: BYTES }, (_, i) => f(i) & 0xff);
const element = (f: (i: number) => number): Gf2_192 => fromBytes(bytes(f));

/** Zero, one, `x^191`, the whole top byte, all ones, and a mixed element: the high ones carry into the reduction. */
const fixed = (): Gf2_192[] => [ZERO, fromByte(1), element((i) => (i === 23 ? 0x80 : 0)), element((i) => (i === 23 ? 0xff : 0)), element(() => 0xff), element((i) => i * 31 + 7)];

/** A deterministic stream of pseudo-random elements (xorshift64, as Rust's test), each from three 64-bit words. */
function pseudoRandom(seed: bigint, count: number): Gf2_192[] {
  const mask = (1n << 64n) - 1n;
  let state = seed;
  const word = (): bigint => {
    state ^= (state << 13n) & mask;
    state ^= state >> 7n;
    state ^= (state << 17n) & mask;
    return state;
  };
  return Array.from({ length: count }, () => fromBytes(bigToLE(word() | (word() << 64n) | (word() << 128n), BYTES)));
}

/** The reference: `a·b` as polynomials over GF(2), then reduced by `x^192 = x^7 + x^2 + x + 1`. */
function reference(a: Gf2_192, b: Gf2_192): Gf2_192 {
  const [x, y] = [leToBig(toBytes(a)), leToBig(toBytes(b))];
  let product = 0n;
  for (let i = 0n; i < 192n; i++) if ((y >> i) & 1n) product ^= x << i;
  for (let i = 382n; i >= 192n; i--) if ((product >> i) & 1n) product ^= (1n << i) ^ (0x87n << (i - 192n));
  return fromBytes(bigToLE(product, BYTES));
}

describe("GF(2^192)", { timeout: 60_000 }, () => {
  it("x^192 reduces by the pentanomial", () => {
    expect(xPow(192)).toEqual(fromByte(0x87));
    expect(toBytes(xPow(191))[23]).toBe(0x80);
  });

  it("bytes round-trip little-endian", () => {
    const b = bytes((i) => i * 7 + 1);
    const e = fromBytes(b);
    expect(toBytes(e)).toEqual(b);
    expect(e[0] & 0xff).toBe(1);
    expect(e.every((w) => Number.isInteger(w) && w >= 0 && w < 2 ** 32), "every word unsigned").toBe(true);
  });

  it("multiplication commutes, associates and distributes", () => {
    const a = element((i) => (i * 31) % 256);
    const b = element((i) => (i * 17 + 3) % 256);
    const c = element(() => 0xa5);
    expect(mul(a, b)).toEqual(mul(b, a));
    expect(mul(a, add(b, c))).toEqual(add(mul(a, b), mul(a, c)));
    expect(mul(mul(a, b), c)).toEqual(mul(a, mul(b, c)));
  });

  it("the general multiply equals carry-less multiplication reduced by the pentanomial", () => {
    const all = [...fixed(), ...pseudoRandom(0x243f_6a88_85a3_08d3n, 24)];
    for (const a of all) for (const b of all) expect(mul(a, b), `${toBytes(a).toString()} * ${toBytes(b).toString()}`).toEqual(reference(a, b));
  });

  it("multiplying by a byte equals the general multiply at every byte", () => {
    for (const a of [...fixed(), ...pseudoRandom(0x9e37_79b9_7f4a_7c15n, 32)]) {
      for (let b = 0; b <= 255; b++) expect(mulByte(a, b), `${toBytes(a).toString()} * ${String(b)}`).toEqual(mul(a, fromByte(b)));
    }
  });

  it("Horner by bytes equals Horner by the general multiply at every point", () => {
    for (let n = 0; n <= 8; n++) {
      const coefficients = pseudoRandom(0x2545_f491_4f6c_dd1dn ^ BigInt(n), n);
      for (let x = 0; x <= 255; x++) {
        const expected = coefficients.reduceRight((acc, c) => add(mul(acc, fromByte(x)), c), ZERO);
        expect(evaluate(coefficients, x), `degree ${String(n)} at ${String(x)}`).toEqual(expected);
      }
    }
  });

  it("Horner evaluates lowest coefficient first", () => {
    const c0 = element(() => 1);
    const c1 = element(() => 2);
    // Q(x) = c0 + c1·x; at 0 it is c0, at 1 it is c0 + c1
    expect(evaluate([c0, c1], 0)).toEqual(c0);
    expect(evaluate([c0, c1], 1)).toEqual(add(c0, c1));
    expect(evaluate([c0, c1], 3)).toEqual(add(c0, mul(c1, fromByte(3))));
  });
});
