import { utf8Length } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { EXPONENT_CEILING, measure } from "../src/index.js";

const LIMIT = 2 ** 20;
const utf8 = (s: string): number => {
  const n = utf8Length(s);
  return n.ok ? n.value : -1;
};

describe("a text's measure", () => {
  it("is its UTF-8 length when no number has an exponent, an unpaired surrogate counting 3 as core counts it", () => {
    for (const t of ["", "null", '{"a": [1, 2.5, -3]}', '"héllo 😀"', '"\\ud800"', '"\ud800x"', '["e", "1e5", "\\"1e9\\""]', "[0.000001, 12345678901234567890]"]) expect(measure(t, LIMIT), t).toBe(utf8(t));
  });

  it("adds each number's exponent, read as JSON reads numbers, and never one inside a string", () => {
    expect(measure("8e9999", LIMIT)).toBe(6 + 9999);
    expect(measure("[1E+3, -2.5e-07, 0e0]", LIMIT)).toBe(21 + 3 + 7);
    expect(measure('["8e9999", "\\"", 1e2]', LIMIT)).toBe(21 + 2);
    expect(measure("1e0000000000000009", LIMIT), "leading zeros do not count").toBe(18 + 9);
    expect(measure("1e123456", LIMIT), "a six-digit exponent counts the ceiling").toBe(8 + EXPONENT_CEILING);
  });

  it("stops past the limit, and a text with more code units than the limit is past it at once", () => {
    expect(measure("x".repeat(11), 10)).toBe(11);
    expect(measure("[8e9999, 8e9999]", 100)).toBeGreaterThan(100);
    expect(measure("[8e9999, 8e9999]", 100)).toBeLessThan(2 * 10_000);
  });
});
