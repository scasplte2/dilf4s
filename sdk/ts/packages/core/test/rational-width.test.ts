import { afterEach, describe, expect, it, vi } from "vitest";

import { bitLength, cbor, RationalValue, Value, type CanonicalError, type Result } from "../src/index.js";
import { gcdSteps } from "../src/value.js";
import { expectOk } from "./vectors.js";

// A rational's numerator and denominator are at most cbor.MAX_RATIONAL_BITS wide (canonical-encoding.md section 1), so
// decoding one costs at most one gcd of bounded operands: work linear in the input, counted, never timed.
describe("rational width", () => {
  const max = cbor.MAX_RATIONAL_BITS;
  const edge = 1n << BigInt(max);

  /** `(F(k + 1), F(k))` with `F(k + 1)` exactly `bits` wide: coprime, and the pair whose gcd takes the most steps. */
  const fibonacci = (bits: number): readonly [bigint, bigint] => {
    let [a, b] = [1n, 1n];
    while (bitLength(b) < bits) [a, b] = [b, a + b];
    return [b, a];
  };

  const rational = (n: bigint, d: bigint): Value => {
    const v = Value.number(n, d);
    if (v?.kind !== "rational") throw new Error(`${bitLength(n).toString()} / ${bitLength(d).toString()} bits is not a rational`);
    return v;
  };

  /** Tag 30 around `[n, d]`, whatever `n` and `d` are. */
  const rationalBytes = (n: bigint, d: bigint): Uint8Array =>
    Uint8Array.from([0xd8, 0x1e, 0x82, ...expectOk(cbor.encode(Value.int(n)), "n"), ...expectOk(cbor.encode(Value.int(d)), "d")]);

  const kind = (r: Result<Value, CanonicalError>): string => (r.ok ? "accepted" : r.error.kind);

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("is the width of 10^10000, the JSON digit and scale bound", () => {
    expect(bitLength(10n ** 10000n)).toBe(max);
  });

  it("k maximal rationals take exactly k gcds, each of operands within the bound", () => {
    // as wide as a rational's parts may be, and coprime (gcd(2^a - 1, 2^b - 1) = 2^gcd(a, b) - 1); counting the gcds
    // does not need the slowest pair, which the gcd's own step count covers
    const [n, d] = [edge - 1n, (edge >> 1n) - 1n];
    expect([bitLength(n), bitLength(d)]).toEqual([max, max - 1]);
    const k = 4;
    const maximal = expectOk(Value.array(Array.from({ length: k }, () => rational(n, d))), "array");
    const bytes = expectOk(cbor.encode(maximal), "encodes");
    expect(bytes.length).toBeGreaterThan(k * 8300);
    const of = vi.spyOn(RationalValue, "of");
    const decoded = expectOk(cbor.decode(bytes), "decodes");
    expect(Value.equals(decoded, maximal)).toBe(true);
    expect(of).toHaveBeenCalledTimes(k);
    expect(Math.max(...of.mock.calls.flat().map(bitLength))).toBe(max);
  });

  it("a gcd of the widest parts takes a bounded number of multi-precision steps", () => {
    const [n, d] = fibonacci(max);
    const [g, steps] = gcdSteps(n, d);
    expect(g).toBe(1n);
    // Lehmer's algorithm takes about 13 bits a step (2,487 here); Euclid's took 47,849 remainders
    expect(steps).toBeLessThanOrEqual(max / 10);
    expect(gcdSteps(edge - 1n, edge - 3n)[1]).toBeLessThanOrEqual(max / 10);
  });

  it("Lehmer's gcd is Euclid's, for either sign, zero and shared factors", () => {
    const euclid = (a: bigint, b: bigint): bigint => {
      let [x, y] = [a < 0n ? -a : a, b < 0n ? -b : b];
      while (y !== 0n) [x, y] = [y, x % y];
      return x;
    };
    let seed = 0x2545f4914f6cdd1dn;
    const random = (bits: number): bigint => {
      let v = 0n;
      for (let i = 0; i < bits; i += 32) {
        seed = (seed * 6364136223846793005n + 1442695040888963407n) & ((1n << 64n) - 1n);
        v = (v << 32n) | (seed >> 32n);
      }
      return v & ((1n << BigInt(bits)) - 1n);
    };
    const pairs: (readonly [bigint, bigint])[] = [
      [0n, 0n],
      [0n, 7n],
      [7n, 0n],
      [1n, 1n],
      [-6n, 4n],
      [1n << 100n, 1n << 99n],
      [(1n << 4000n) - 1n, (1n << 3999n) - 1n],
      [edge - 1n, 3n],
      fibonacci(2000),
    ];
    for (let i = 0; i < 400; i++) {
      const g = random(1 + Number(random(8) % 300n));
      const sign = random(1) === 0n ? 1n : -1n;
      pairs.push([sign * random(1 + Number(random(12) % 3000n)) * g, random(1 + Number(random(12) % 3000n)) * g]);
    }
    for (const [a, b] of pairs) expect(gcdSteps(a, b)[0]).toBe(euclid(a, b));
  });

  it("a part one bit too wide is refused before any gcd, even when the rational is not in lowest terms either", () => {
    const of = vi.spyOn(RationalValue, "of");
    for (const [n, d] of [
      [edge * 2n, edge + 2n],
      [2n, edge * 2n],
      [-edge - 1n, 2n],
      [1n, -edge * 2n],
    ] as const) {
      expect(cbor.decode(rationalBytes(n, d))).toMatchObject({ ok: false, error: { kind: "Unsupported", offset: 0 } });
    }
    expect(of).not.toHaveBeenCalled();
  });

  it("the bound is on Java's bitLength, for either sign", () => {
    for (const [n, d] of [
      [edge - 1n, 2n],
      [-edge, 3n],
      [2n, edge - 1n],
    ] as const) {
      const decoded = expectOk(cbor.decode(rationalBytes(n, d)), `${bitLength(n).toString()} / ${bitLength(d).toString()} bits`);
      expect(Value.equals(decoded, rational(n, d))).toBe(true);
    }
    for (const [n, d] of [
      [edge, 3n],
      [-edge - 1n, 2n],
      [3n, edge],
    ] as const) {
      expect(kind(cbor.decode(rationalBytes(n, d)))).toBe("Unsupported");
    }
  });

  it("a wide numerator is refused before the denominator is read", () => {
    const wide = Uint8Array.from([0xd8, 0x1e, 0x82, ...expectOk(cbor.encode(Value.int(edge)), "n"), 0x61, 0x61]);
    expect(kind(cbor.decode(wide))).toBe("Unsupported");
    expect(kind(cbor.decode(Uint8Array.from([0xd8, 0x1e, 0x82, 0x01, 0x61, 0x61])))).toBe("NonCanonical");
  });
});
