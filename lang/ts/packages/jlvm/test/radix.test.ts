import { describe, expect, it } from "vitest";
import { RadixIndex, radixKey } from "../src/radix.js";

const key = (kind: number, bytes: readonly number[]) => radixKey(kind, bytes.length, (i) => bytes[i] ?? 0);

describe("radix membership", () => {
  it("distinguishes kind tags, empty payloads and zeros with exact traversal counts", () => {
    const index = new RadixIndex();
    const a = key(0, []);
    const b = key(1, []);
    expect(index.add(a)).toEqual({ added: true, work: { bytes: 0, branches: 0 } });
    expect(index.add(a)).toEqual({ added: false, work: { bytes: 18, branches: 0 } });
    expect(index.add(b)).toEqual({ added: true, work: { bytes: 19, branches: 0 } });
    expect(index.contains(b)).toEqual({ found: true, work: { bytes: 19, branches: 1 } });
    expect(index.contains(key(0, [0])).found).toBe(false);
    expect(index.add(key(0, [0])).added).toBe(true);
    expect(index.contains(key(0, [0])).found).toBe(true);
  });

  it("stops short probes before traversing a long-key suffix", () => {
    const index = new RadixIndex();
    const prefix = Array<number>(4096).fill(0);
    for (let i = 0; i < 256; i++) index.add(key(4, [...prefix, i]));
    const short = key(4, []);
    const result = index.contains(short);
    expect(result.found).toBe(false);
    expect(result.work.branches).toBe(1);
    expect(result.work.bytes).toBeLessThanOrEqual(2 * short.length);
    expect(index.add(short).added).toBe(true);
    expect(index.contains(short).found).toBe(true);
  });

  it("preserves exact membership for distinct, duplicate and reversed insertions within linear work", () => {
    const inputs = Array.from({ length: 1024 }, (_, i) => key(i % 8, [0, Math.floor(i / 256), i % 256, 0]));
    for (const ordered of [inputs, [...inputs].reverse()]) {
      const index = new RadixIndex();
      for (const k of ordered) {
        const inserted = index.add(k);
        expect(inserted.added).toBe(true);
        expect(inserted.work.bytes).toBeLessThanOrEqual(18 * k.length + 1);
        expect(inserted.work.branches).toBeLessThanOrEqual(16 * k.length + 1);
      }
      for (const k of inputs) {
        const lookup = index.contains(k);
        expect(lookup.found).toBe(true);
        expect(lookup.work.bytes).toBeLessThanOrEqual(10 * k.length);
        expect(lookup.work.branches).toBeLessThanOrEqual(8 * k.length + 1);
        expect(index.add(k).added).toBe(false);
      }
      expect(index.contains(key(0, [0, 4, 0, 0])).found).toBe(false);
    }
  });

  it("encodes lengths above 32 bits without truncating their header", () => {
    const huge = radixKey(4, 2 ** 32 + 3, () => 0);
    expect(Array.from({ length: 9 }, (_, i) => huge.byteAt(i))).toEqual([0, 0, 0, 1, 0, 0, 0, 3, 4]);
    const index = new RadixIndex();
    index.add(huge);
    expect(index.contains(key(4, [0, 0, 0])).found).toBe(false);
    expect(index.add(key(4, [0, 0, 0])).added).toBe(true);
  });
});
