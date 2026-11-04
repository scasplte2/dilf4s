import { describe, expect, it } from "vitest";

import { codec, MAX_INTEGER_BITS } from "../src/index.js";
import { int } from "./vectors.js";

// a refused integer is named by its size: its digits would be work no caller pays for
describe("integer range refusals", () => {
  const huge = 1n << 1_000_000n;
  // the widest integer a value holds
  const widest = int((1n << BigInt(MAX_INTEGER_BITS)) - 1n);
  const uint = codec.intRange(0n, (1n << 64n) - 1n);

  it("decoding names the bit length", () => {
    expect(uint.decode(widest)).toEqual({ ok: false, error: { path: [], reason: "integer of 33220 bits outside 0..18446744073709551615" } });
  });

  it("encoding names the bit length, Java's for negatives", () => {
    expect(uint.encode(-(1n << 63n))).toMatchObject({ ok: false, error: { kind: "TypeMismatch", reason: "integer of 63 bits outside 0..18446744073709551615" } });
    expect(uint.encode(huge)).toMatchObject({ ok: false, error: { reason: "integer of 1000001 bits outside 0..18446744073709551615" } });
  });

  it("literals and enums name the bit length", () => {
    expect(codec.literalInt(7n).decode(widest)).toMatchObject({ ok: false, error: { reason: "expected 7, got an integer of 33220 bits" } });
    const children = codec.intEnum("TrieChildren", [["Left", 1n]] as const);
    expect(children.decode(widest)).toMatchObject({ ok: false, error: { reason: "an integer of 33220 bits is not a TrieChildren" } });
  });
});
