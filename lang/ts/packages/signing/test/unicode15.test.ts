/**
 * BIP-39 accepts only code points Unicode 15.0 assigns (signing.md section 2.1): the table is
 * `shared/unicode/assigned-15.0.txt`. The vectors pin the refusals a JSON text can carry; this pins the table and what
 * JSON cannot (a lone surrogate).
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, test, vi } from "vitest";

import { mnemonicToSeed, toEntropy, toMnemonic, WordList } from "../src/index.js";
import { ENGLISH } from "../src/wordlists/english.js";
import { ASSIGNED, assigned } from "../src/unicode15.js";
import { expectOk } from "./vectors.js";

const shared = fileURLToPath(new URL("../../../../../shared/unicode/assigned-15.0.txt", import.meta.url));

describe("Unicode 15.0", () => {
  test("the table is shared/unicode/assigned-15.0.txt", () => {
    const ranges = readFileSync(shared, "utf8")
      .split("\n")
      .filter((l) => !l.startsWith("#") && l.trim().length > 0)
      .map((l) => l.split("..").map((x) => Number.parseInt(x, 16)));
    expect(ranges.length, "707 ranges").toBe(707);
    expect(ASSIGNED.map((r) => [r[0], r[1]])).toEqual(ranges);
  });

  test("assigned in 15.0, and not", () => {
    for (const cp of [0x41, 0x1e030, 0x1d400, 0xfb01, 0xe000, 0x1f6dc, 0x10fffd]) expect(assigned(cp), `U+${cp.toString(16)}`).toBe(true);
    for (const cp of [0x1ccd6, 0xfdd0, 0xfffe, 0x10ffff, 0xd800, 0xdfff, -1, 0x110000]) expect(assigned(cp), `U+${cp.toString(16)}`).toBe(false);
  });

  test("a lone surrogate in a phrase, a passphrase or a word is refused", () => {
    const valid = [...Array<string>(11).fill("abandon"), "about"].join(" ");
    const kind = (r: { readonly ok: boolean; readonly error?: { readonly kind: string } }): string => (r.ok ? "accepted" : (r.error?.kind ?? "?"));
    expect(kind(toEntropy(`${valid}\uD800`))).toBe("InvalidMnemonic");
    const m = expectOk(toMnemonic(new Uint8Array(16)), "m");
    expect(kind(mnemonicToSeed(m, "dilf4s\uD800"))).toBe("InvalidPassphrase");
    expect(kind(WordList.of([`abandon\uD800`, ...ENGLISH.slice(1)]))).toBe("InvalidWordList");
  });

  test("a runtime whose NFKD predates Unicode 15.0 reads ASCII only, and derives no seed it would normalise differently", () => {
    // emulate one that normalises nothing, so U+1E030 (new in 15.0) stays itself; the only text such a runtime reads
    // is ASCII, which is its own NFKD
    const spy = vi.spyOn(String.prototype, "normalize").mockImplementation(function (this: string) {
      return this;
    });
    try {
      const kind = (r: { readonly ok: boolean; readonly error?: { readonly kind: string } }): string => (r.ok ? "accepted" : (r.error?.kind ?? "?"));
      const m = expectOk(toMnemonic(new Uint8Array(16)), "m");
      expect(kind(mnemonicToSeed(m, "dilf4s")), "an ASCII passphrase").toBe("accepted");
      expect(kind(mnemonicToSeed(m, "\u{1E030} dilf4s")), "a passphrase that would normalise differently").toBe("InvalidPassphrase");
      expect(kind(mnemonicToSeed(m, "caf\u00e9")), "any non-ASCII passphrase").toBe("InvalidPassphrase");
      expect(kind(toEntropy([...Array<string>(11).fill("abandon"), "about"].join(" "))), "an ASCII phrase").toBe("accepted");
      expect(kind(toEntropy(`${[...Array<string>(11).fill("abandon"), "about"].join(" ")}\u00a0`)), "a non-ASCII phrase").toBe("InvalidMnemonic");
      expect(kind(WordList.of([`caf\u00e9`, ...ENGLISH.slice(1)])), "a non-ASCII word").toBe("InvalidWordList");
    } finally {
      spy.mockRestore();
    }
  });
});
