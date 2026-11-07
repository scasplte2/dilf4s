import { describe, expect, it } from "vitest";

import { json, RationalValue, Value, type Result } from "../src/index.js";
import { expectOk, int } from "./vectors.js";

// JSON numbers have at most 10,000 significant digits, counted on the text before any conversion, and render only
// within the same bounds, so that a rendering always reads back
describe("JSON number bounds", () => {
  const digits = (n: number): string => "9".repeat(n);

  it("10,000 digits read and write; leading zeros are not counted", () => {
    const ones = "1".repeat(10000);
    const v = expectOk(json.parse(ones), "parses");
    expect(json.renderWithin(v, json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: ones });
    const small = expectOk(json.parse(`0.${"0".repeat(10000)}2e1`), "parses");
    expect(json.renderWithin(small, json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: `0.${"0".repeat(9999)}2` });
    // within the digit and scale bounds, but past the data model's: 10,000 nines (33220 bits), 1/10^10000
    expect(json.parse(digits(10000))).toEqual({ ok: false, error: { kind: "IntegerTooWide", limit: 33219 } });
    expect(json.parse(`0.${"0".repeat(9999)}1`)).toEqual({ ok: false, error: { kind: "RationalTooWide", limit: 33219 } });
    expect(json.parse(`1e${"0".repeat(20000)}2`)).toEqual({ ok: true, value: int(100n) });
  });

  it("more digits are refused by their length", () => {
    expect(json.parse(digits(10001))).toEqual({ ok: false, error: { kind: "InvalidNumber", length: 10001, offset: 0, bound: { kind: "Digits", digits: 10001, limit: 10000 } } });
    expect(json.parse(`0.${"1".repeat(10001)}e1`)).toMatchObject({ ok: false, error: { kind: "InvalidNumber", bound: { kind: "Digits", digits: 10001 } } });
    expect(json.parse(`1e${"1".repeat(10001)}`)).toMatchObject({ ok: false, error: { kind: "InvalidNumber", bound: { kind: "Digits", digits: 10001 } } });
    // both past the bound: the significand's count, as every SDK reports it
    expect(json.parse(`${"1".repeat(10002)}e${"1".repeat(10001)}`)).toMatchObject({ ok: false, error: { kind: "InvalidNumber", bound: { kind: "Digits", digits: 10002 } } });
    expect(json.parse(`1e${"9".repeat(20)}`)).toMatchObject({ ok: false, error: { kind: "InvalidNumber", bound: { kind: "Scale", scale: -2147483648, limit: 10000 } } });
  });

  it("rendering refuses what would not read back", () => {
    // every integer renders: the widest, of either sign, has 10,000 digits
    for (const n of [(1n << 33219n) - 1n, -(1n << 33219n)]) {
      expect(json.renderWithin(int(n), json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: n.toString() });
      expect(n.toString().replace("-", "").length).toBe(10000);
    }
    // 1/5^14400 (33436 bits) is no value at all (MAX_RATIONAL_BITS); 1/5^14306 (33218 bits) is, at a scale past the bound
    expect(Value.number(1n, 5n ** 14400n)).toEqual({ ok: false, error: { kind: "RationalTooWide", limit: 33219 } });
    const wide = expectOk(Value.number(1n, 5n ** 14306n), "1/5^14306");
    expect(json.renderWithin(wide, json.MAX_RENDER_BYTES)).toEqual({ ok: false, error: { kind: "NotJsonRepresentable", what: "over-long number" } });
    const third = expectOk(Value.number(1n, 3n), "1/3");
    expect(json.renderWithin(third, json.MAX_RENDER_BYTES)).toEqual({ ok: false, error: { kind: "NotJsonRepresentable", what: "non-terminating rational" } });
  });

  it("a malformed number is described by its length and offset, never its text", () => {
    const nines = digits(10001);
    const cases: readonly (readonly [string, number])[] = [
      [`[0${nines}]`, 10002],
      [`[${nines}.]`, 10002],
      [`[1.${nines}e]`, 10004],
      [`[-${nines}e+]`, 10004],
    ];
    for (const [text, length] of cases) {
      const r = json.parse(text);
      expect(r).toEqual({ ok: false, error: { kind: "InvalidNumber", length, offset: 1 } });
      expect(JSON.stringify(r, (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v))).not.toContain("99999");
    }
  });

  it("a decimal is reduced by 2 and 5 alone, exactly as by its gcd with 10^scale", () => {
    const numerators = [0n, 1n, -1n, 2n, 5n, 10n, 16n, 25n, 125n, 640n, 3125n, -7812500n, 999n, 1000000n, 2048000000n, 5n ** 20000n, 3n * 2n ** 20000n];
    // the parts themselves (toEqual does not see private fields), or both refused as too wide (5^20000 over a small scale)
    const parts = (r: Result<Value, unknown>): readonly [string, bigint, bigint] | undefined => {
      const v = r.ok ? r.value : undefined;
      return v?.kind === "int" ? ["int", v.value, 1n] : v?.kind === "rational" ? ["rational", v.numerator, v.denominator] : undefined;
    };
    for (const n of numerators)
      for (const scale of [0, 1, 2, 3, 7, 8, 9, 64, 1000, 10000])
        expect(parts(RationalValue.decimal(n, scale))).toEqual(parts(RationalValue.number(n, 10n ** BigInt(scale))));
  });
});
