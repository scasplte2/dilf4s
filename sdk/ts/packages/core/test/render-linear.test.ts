import { describe, expect, it } from "vitest";

import { json, MAX_DEPTH, Value } from "../src/index.js";
import { expectOk, int } from "./vectors.js";

const text = (s: string): Value => expectOk(Value.text(s), "text");
const array = (items: readonly Value[]): Value => expectOk(Value.array(items), "array");
const map = (entries: readonly (readonly [string, Value])[]): Value => expectOk(Value.map(entries), "map");

/** What reached the sink, as counts alone: characters, pieces, and the longest piece. */
interface Count {
  readonly chars: number;
  readonly pieces: number;
  readonly longest: number;
}

const counting: json.TextSink<Count> = (c, piece) => ({ chars: c.chars + piece.length, pieces: c.pieces + 1, longest: Math.max(c.longest, piece.length) });

const counted = (v: Value): Count => expectOk(json.renderInto(v, json.MAX_RENDER_BYTES, { chars: 0, pieces: 0, longest: 0 }, counting), "renders");

const chain = Array.from({ length: MAX_DEPTH }).reduce<Value>((v) => array([v]), int(7n));
const bottom = 'a"b\n\u0001é😀';
const mapChain = Array.from({ length: MAX_DEPTH }, (_, i) => i).reduce<Value>((v, i) => map([[`k${i.toString()}`, v], ["z", int(BigInt(i))]]), text(bottom));
const wide = map(Array.from({ length: 20000 }, (_, i) => [`key${i.toString().padStart(5, "0")}`, i % 2 === 0 ? int(BigInt(i)) : text(`v\t${i.toString()}`)] as const));

// counted, not timed: a renderer that built each level's text from its children's would copy a character once per level
// above it, and would hand the sink a container's text whole
describe("JSON rendering is one pass", { timeout: 60_000 }, () => {
  it("every character of a depth-256 chain, a deep map chain and a wide map reaches the sink once", () => {
    for (const [name, v, longest] of [
      ["array chain", chain, 1],
      ["map chain", mapChain, JSON.stringify(bottom).length],
      ["wide map", wide, ',"key00000":'.length],
    ] as const) {
      const rendered = expectOk(json.renderWithin(v, json.MAX_RENDER_BYTES), name);
      const c = counted(v);
      expect(c.chars, `${name}: characters written`).toBe(rendered.length);
      expect(BigInt(c.pieces) <= 4n * v.weight, `${name}: ${c.pieces.toString()} pieces for weight ${v.weight.toString()}`).toBe(true);
      expect(c.longest, `${name}: the longest piece is a scalar, a key or punctuation`).toBe(longest);
    }
  });

  it("the chain is its 513 characters, in 513 pieces", () => {
    expect(counted(chain)).toEqual({ chars: 513, pieces: 513, longest: 1 });
    expect(json.renderWithin(chain, json.MAX_RENDER_BYTES)).toEqual({ ok: true, value: `${"[".repeat(256)}7${"]".repeat(256)}` });
  });
});
