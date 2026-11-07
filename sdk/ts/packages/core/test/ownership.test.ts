import { Buffer } from "node:buffer";

import { describe, expect, it } from "vitest";

import { cbor, codec } from "../src/index.js";
import { bytesValue, expectOk, hexOf } from "./vectors.js";

// Buffer#slice and Buffer#subarray share memory, so a Buffer must be copied into a plain Uint8Array.
describe("byte strings own their bytes", () => {
  it("a BytesValue made from a Buffer does not follow the Buffer", () => {
    const source = Buffer.from([1, 2]);
    const v = bytesValue(source);
    source[0] = 9;
    expect(hexOf(v.bytes)).toBe("0102");
  });

  it("the bytes getter hands out a copy, even from a Buffer", () => {
    const v = bytesValue(Buffer.from([1, 2]));
    const out = v.bytes;
    out[0] = 9;
    expect(hexOf(v.bytes)).toBe("0102");
    expect(Buffer.isBuffer(out), "a plain Uint8Array").toBe(false);
  });

  it("a value decoded from a Buffer does not follow the Buffer", () => {
    const input = Buffer.from([0x42, 0x01, 0x02]);
    const v = expectOk(cbor.decode(input), "decodes");
    input[1] = 9;
    expect(hexOf(expectOk(cbor.encode(v), "encodes"))).toBe("420102");
  });

  it("a codec's encoded and decoded bytes are independent of the caller's", () => {
    const source = Buffer.from([1, 2]);
    const v = expectOk(codec.bytes.encode(source), "encodes");
    source[0] = 9;
    const decoded = expectOk(codec.bytes.decode(v), "decodes");
    decoded[1] = 9;
    expect(hexOf(expectOk(cbor.encode(v), "encodes"))).toBe("420102");
  });
});
