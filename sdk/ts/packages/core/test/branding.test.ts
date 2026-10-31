import { describe, expect, it } from "vitest";

import { RationalValue, type ArrayValue, type BoolValue, type IntValue, type NullValue, type Value } from "../src/index.js";

// Each `@ts-expect-error` is the test: typecheck fails if a structural look-alike were accepted as a canonical value,
// which would let invariants (reduced rationals, bounded depth) be bypassed without a cast.
describe("canonical values only come from their factories", () => {
  it("object literals are not values", () => {
    // @ts-expect-error a RationalValue is not structural
    const r: RationalValue = { kind: "rational", depth: 0, numerator: 2n, denominator: 2n };
    // @ts-expect-error nor is an ArrayValue
    const a: ArrayValue = { kind: "array", depth: 1, items: [] };
    // @ts-expect-error nor an IntValue
    const i: IntValue = { kind: "int", depth: 0, value: 1n };
    // @ts-expect-error nor a BoolValue
    const b: BoolValue = { kind: "bool", depth: 0, value: true };
    // @ts-expect-error nor a NullValue
    const n: NullValue = { kind: "null", depth: 0 };
    const forged: unknown[] = [r, a, i, b, n];
    expect(forged.some((x) => x instanceof RationalValue)).toBe(false);
  });

  it("factory values read as before", () => {
    const half: Value | undefined = RationalValue.number(2n, 4n);
    expect(half?.kind === "rational" && [half.numerator, half.denominator]).toEqual([1n, 2n]);
  });
});
