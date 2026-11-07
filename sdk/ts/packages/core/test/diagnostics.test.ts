import { describe, expect, it } from "vitest";

import { codec, describeFailure, digestFromHex, DomainTag, json, MapValue, MAX_PREVIEW_CODE_POINTS, models, parseHashAlgorithm, Preview, Value } from "../src/index.js";
import { decodeFailure, at } from "../src/error.js";
import { expectOk, int, loadVectors } from "./vectors.js";

// Bounded diagnostics (docs/specs/conformance.md section 8): a refusal holds caller text only as a `Preview`, so however
// long the input, what an error holds and renders stays within a fixed bound.

// a preview renders as at most 32 code points, each escaped to at most 6 characters, quotes, a mark and a length
const PREVIEW_BOUND = 32 * 6 + 64;
const MESSAGE_BOUND = 2 * PREVIEW_BOUND + 256;
const HUGE = 1 << 20;

const text = (s: string): Value => expectOk(Value.text(s), "text");
const kept = (p: Preview): void => {
  expect(Array.from(p.head).length, "a preview keeps at most 32 code points").toBeLessThanOrEqual(MAX_PREVIEW_CODE_POINTS);
};
const bounded = (what: string, rendered: string): void => {
  expect(rendered.length, `${what} renders boundedly`).toBeLessThanOrEqual(MESSAGE_BOUND);
};
const rendered = (e: unknown): string => JSON.stringify(e, (_, v: unknown) => (v instanceof Preview ? v.toString() : v));

describe("a preview keeps the first 32 code points and the UTF-8 length, the same in every SDK", () => {
  // shared/vectors/diagnostics_v1.json (conformance.md section 8): each object repeats one member name, refused as
  // `DuplicateKey` with the preview given
  const vectors = loadVectors("diagnostics_v1.json", models.diagnosticVectors.DiagnosticVectors);
  it("holds every row of the spec's table", () => {
    expect(vectors.previews.length).toBeGreaterThanOrEqual(11);
  });
  it.each(vectors.previews.map((c) => [c.json, c] as const))("%s", (_, c) => {
    const r = json.parse(c.json);
    expect(r.ok).toBe(false);
    if (r.ok || r.error.kind !== "DuplicateKey") throw new Error("a duplicate key");
    expect(r.error.key.head).toBe(c.head);
    expect(BigInt(r.error.key.length)).toBe(c.length);
    expect(r.error.key.truncated).toBe(c.truncated);
  });
});

describe("bounded refusals", () => {
  it("a preview is built only by truncating: an object of its shape is not one", () => {
    const shaped = { head: "x".repeat(HUGE), length: HUGE, truncated: false, short: () => "", toString: () => "" };
    // @ts-expect-error the type is nominal: only Preview.of builds one
    const forged: Preview = shaped;
    expect(forged instanceof Preview).toBe(false);
  });

  it("a preview renders the head escaped, a mark when truncated, and the length", () => {
    expect(Preview.of("abc").toString()).toBe('"abc" (UTF-8 length 3)');
    expect(Preview.of("a".repeat(40)).toString()).toBe(`"${"a".repeat(32)}"… (UTF-8 length 40)`);
    expect(Preview.of('"\\\u0000').toString()).toBe('"\\"\\\\\\u0000" (UTF-8 length 3)');
    expect(describeFailure(at(at(decodeFailure("r"), "b".repeat(33)), "a"))).toBe(`a.${"b".repeat(32)}…: r`);
  });

  it("a repeated 1 MiB JSON member name or map key is a preview", () => {
    const key = "k".repeat(HUGE);
    const parsed = json.parse(`{"${key}":1,"${key}":2}`);
    const built = MapValue.of([
      [key, int(1n)],
      [key, int(2n)],
    ]);
    for (const r of [parsed, built]) {
      if (r.ok || r.error.kind !== "DuplicateKey") throw new Error("a duplicate key");
      kept(r.error.key);
      expect(r.error.key.length).toBe(HUGE);
      bounded("DuplicateKey", rendered(r.error));
    }
  });

  it("literal, pattern and alternative refusals preview a 1 MiB text", () => {
    const huge = text("x".repeat(HUGE));
    const failures = [codec.literalText("a").decode(huge), codec.matching("0|[1-9][0-9]*").decode(huge), codec.selectAlternative(huge, [], ["a"])];
    for (const f of failures) {
      expect(f.ok).toBe(false);
      if (!f.ok) bounded("refusal", describeFailure(f.error) + rendered(f.error));
    }
    const encoded = codec.oneOfText("x".repeat(HUGE), ["a"]);
    expect(encoded.ok).toBe(false);
    if (!encoded.ok) bounded("TypeMismatch", rendered(encoded.error));
  });

  it("unknown keys are counted and the first, in canonical order, previewed; never listed", () => {
    const small = expectOk(MapValue.of(["a", "zz", "é", "aaa", "b"].map((k) => [k, int(1n)] as const)), "a map");
    const f = codec.fields(small, ["a"]);
    expect(f.ok ? "accepted" : describeFailure(f.error)).toBe('<root>: 4 unknown keys, the first "b" (UTF-8 length 1)');
    const many = expectOk(MapValue.of(Array.from({ length: 4096 }, (_, i) => [`${"u".repeat(256)}${i.toString()}`, int(BigInt(i))] as const)), "a map");
    const g = codec.fields(many, ["a"]);
    expect(g.ok ? "accepted" : g.error.reason).toMatch(/^4096 unknown keys/);
    if (!g.ok) bounded("unknown keys", describeFailure(g.error));
    const none = codec.selectAlternative(many, [[["a"], ["a"]]], []);
    expect(none.ok ? "accepted" : describeFailure(none.error)).toBe("<root>: no alternative fits a map of 4096 keys");
  });

  it("a model refusing a 1 MiB unknown key renders boundedly", () => {
    const r = codec.fromJson(models.merkleVectors.MerkleVectors, `{"${"k".repeat(HUGE)}":1}`);
    expect(r.ok).toBe(false);
    if (!r.ok) bounded("ModelError", rendered(r.error));
  });

  it("a 1 MiB caller key on a failure's path is a preview", () => {
    const m = expectOk(MapValue.of([["k".repeat(HUGE), text("not an int")]]), "a map");
    const f = codec.textMap(codec.uint).decode(m);
    expect(f.ok).toBe(false);
    if (!f.ok) {
      f.error.path.forEach(kept);
      bounded("path", describeFailure(f.error));
    }
  });

  it("a weight budget of any size is refused as a 64-bit budget, its outcome unchanged", () => {
    const huge = 1n << 1_000_000n;
    const below = codec.decodeWithin(codec.any, Value.null, -huge);
    expect(below.ok ? "accepted" : describeFailure(below.error)).toBe("<root>: the value weighs over 0");
    expect(codec.decodeWithin(codec.any, Value.null, huge).ok, "a budget past MAX_WEIGHT admits every value").toBe(true);
  });

  it("a 1 MiB domain tag is a preview", () => {
    const r = DomainTag.of("X".repeat(HUGE));
    expect(r.ok).toBe(false);
    if (!r.ok && r.error.kind === "InvalidDomainTag") {
      kept(r.error.tag);
      bounded("InvalidDomainTag", rendered(r.error));
    }
  });

  it("a 1 MiB hash function name is a preview", () => {
    const r = parseHashAlgorithm("X".repeat(HUGE));
    expect(r.ok).toBe(false);
    if (!r.ok && r.error.kind === "UnknownAlgorithm") {
      kept(r.error.name);
      bounded("UnknownAlgorithm", rendered(r.error));
    }
  });

  it("a 1 MiB digest hex is refused by its width, holding sizes only", () => {
    const cases = ["a".repeat(HUGE), `0x${"a".repeat(HUGE)}`, "a".repeat(64) + "é".repeat(HUGE), "é".repeat(HUGE)];
    const refusals = cases.map((t) => digestFromHex("SHA-256", t));
    expect(refusals.map((r) => (r.ok ? "accepted" : r.error))).toEqual([
      { kind: "InvalidLength", algorithm: "SHA-256", expected: 32, actual: HUGE / 2 },
      { kind: "InvalidLength", algorithm: "SHA-256", expected: 32, actual: HUGE / 2 },
      { kind: "InvalidHex", offset: 64 },
      { kind: "InvalidHex", offset: 0 },
    ]);
    for (const r of refusals) bounded("digest hex", rendered(r));
    // past the one digit it reads beyond the width, the text is not read: its length refuses it, whatever follows
    const past = digestFromHex("SHA-256", "a".repeat(65) + "é".repeat(HUGE));
    expect(past.ok ? "accepted" : past.error.kind).toBe("InvalidLength");
    bounded("digest hex", rendered(past));
  });
});
