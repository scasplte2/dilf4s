import { afterEach, describe, expect, it, vi } from "vitest";

import { bignums } from "../src/cbor.js";
import { bitLength, cbor, codec, IntValue, json, MAX_INTEGER_BITS, MAX_RATIONAL_BITS, Value, type CanonicalError, type Result } from "../src/index.js";
import { expectOk, int, integerBytes } from "./vectors.js";

// An integer is at most MAX_INTEGER_BITS wide (canonical-encoding.md section 1), for either sign: the only constructor
// checks it, and a decoder refuses a wider bignum from its length, before it builds the integer. Work is counted, never
// timed.
describe("integer width", () => {
  const max = MAX_INTEGER_BITS;
  const edge = 1n << BigInt(max);
  const tooWide = { ok: false, error: { kind: "IntegerTooWide", limit: max } };
  const accepted = [edge - 1n, -edge];
  const refused = [edge, -edge - 1n];

  const kind = (r: Result<Value, CanonicalError>): string => (r.ok ? "accepted" : r.error.kind);

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("is a rational part's, the width of 10^10000", () => {
    expect(max).toBe(MAX_RATIONAL_BITS);
    expect(bitLength(10n ** 10000n)).toBe(max);
  });

  it("is checked by the only constructor, on Java bitLength, for either sign", () => {
    for (const n of accepted) expect(expectOk(IntValue.of(n), "within").bitLength).toBe(max);
    for (const n of refused) {
      expect(bitLength(n)).toBe(max + 1);
      expect(IntValue.of(n)).toEqual(tooWide);
      expect(Value.int(n)).toEqual(tooWide);
      expect(Value.number(n * 3n, 3n)).toEqual(tooWide);
    }
    expect(codec.intRange(null, null).encode(edge)).toEqual(tooWide);
    expect(codec.intRange(0n, 1n).encode(edge)).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
  });

  it("refuses a bignum one bit too wide, for either tag, before building its magnitude", () => {
    const magnitude = vi.spyOn(bignums, "magnitude");
    for (const n of [...refused, edge * 255n]) expect(cbor.decode(integerBytes(n))).toEqual(tooWide);
    expect(magnitude).not.toHaveBeenCalled();
  });

  it("builds exactly k magnitudes for k of the widest integers, each within the bound", () => {
    const values = [...accepted, ...accepted].map(int);
    const array = expectOk(Value.array(values), "an array");
    const bytes = expectOk(cbor.encode(array), "encodes");
    expect(accepted.map((n) => Array.from(integerBytes(n)))).toEqual(accepted.map((n) => Array.from(expectOk(cbor.encode(int(n)), "encodes"))));
    const magnitude = vi.spyOn(bignums, "magnitude");
    const decoded = expectOk(cbor.decode(bytes), "decodes");
    expect(Value.equals(decoded, array)).toBe(true);
    expect(magnitude).toHaveBeenCalledTimes(values.length);
    expect(Math.max(...magnitude.mock.calls.map(([b]) => b.length))).toBe(4153);
  });

  it("checks a bignum's form and length before its width", () => {
    const magnitude = vi.spyOn(bignums, "magnitude");
    // 4154 bytes, the first zero: non-canonical, though a length alone would call it too wide
    const leadingZero = Uint8Array.from([0xc2, 0x59, 0x10, 0x3a, 0x00, ...new Uint8Array(4153).fill(0xff)]);
    expect(kind(cbor.decode(leadingZero))).toBe("NonCanonical");
    // a byte string claimed 2^32 - 1 bytes long, with 4 present: truncated, not too wide
    const truncated = Uint8Array.from([0xc3, 0x5a, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
    expect(kind(cbor.decode(truncated))).toBe("UnexpectedEnd");
    // 4153 bytes, the first 0x0f: 33220 bits, -2^33220; the first 0x10: 33221
    const widest = Uint8Array.from([0xc3, 0x59, 0x10, 0x39, 0x0f, ...new Uint8Array(4152).fill(0xff)]);
    expect(Value.equals(expectOk(cbor.decode(widest), "decodes"), int(-edge))).toBe(true);
    const past = Uint8Array.from([0xc3, 0x59, 0x10, 0x39, 0x10, ...new Uint8Array(4152)]);
    expect(cbor.decode(past)).toEqual(tooWide);
    expect(magnitude).toHaveBeenCalledTimes(1);
  });

  it("refuses a rational part too wide as RationalTooWide, from its length", () => {
    const magnitude = vi.spyOn(bignums, "magnitude");
    const bytes = Uint8Array.from([0xd8, 0x1e, 0x82, ...integerBytes(edge), 0x03]);
    expect(cbor.decode(bytes)).toEqual({ ok: false, error: { kind: "RationalTooWide", limit: MAX_RATIONAL_BITS } });
    expect(magnitude).not.toHaveBeenCalled();
  });

  it("reads JSON integers up to the bound and refuses one past it, for either sign", () => {
    const ten = 10n ** 10000n;
    expect(Value.equals(expectOk(json.parse("1e10000"), "1e10000"), int(ten))).toBe(true);
    expect(Value.equals(expectOk(json.parse("-1e10000"), "-1e10000"), int(-ten))).toBe(true);
    expect(json.parse("2e10000")).toEqual(tooWide);
    expect(json.parse("-2e10000")).toEqual(tooWide);
    expect(json.parse(`[${"9".repeat(10000)}e10000]`)).toEqual(tooWide);
  });

  it("renders every integer of at most 10000 digits; 10^10000 up to the bound cannot be", () => {
    const nines = BigInt("9".repeat(10000));
    const overLong = { ok: false, error: { kind: "NotJsonRepresentable", what: "over-long number" } };
    expect(json.renderWithin(int(nines), json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: "9".repeat(10000) });
    expect(json.renderWithin(int(-nines), json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: `-${"9".repeat(10000)}` });
    for (const n of [nines + 1n, edge - 1n, -edge]) expect(json.renderWithin(int(n), json.MAX_RENDER_BYTES)).toEqual(overLong);
  });
});
