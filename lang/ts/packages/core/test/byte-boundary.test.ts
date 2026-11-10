import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/**
 * The byte boundary's invariant (docs/specs/canonical-encoding.md section 1, "Caller bytes"), checked over the source:
 * the public-facing code of proofs, signing and jlvm, and core's hash entry points, take a caller's bytes through core's
 * `ownBytes` family alone. None of it may name the raw readers (`plainBytes`, `takeBytes`, `takeBytesWithin`) or read a
 * value's `.buffer` or `.byteOffset`: those belong to the boundary itself (`core/src/bytes.ts`). Every boundary call
 * states its field's bound, so none may use the unbounded forms (`ownAny`, `ownCopyAny`); a field with truly no bound
 * passes `UNBOUNDED` explicitly, and each such use is on the allowlist below, with its reason.
 */

/** Every explicit `UNBOUNDED` in the scanned code, by file: how many, and why the field has no bound. */
const UNBOUNDED_ALLOWED: Readonly<Record<string, { readonly uses: number; readonly reason: string }>> = {
  "core/src/hash.ts": { uses: 2, reason: "data to hash (digest, hashParts, update, TaggedHash.hash, expandMessageXmd's message) and a DST, which RFC 9380 hashes past 255 bytes" },
  "core/src/hex.ts": { uses: 1, reason: "toHex renders bytes of any length" },
  "core/src/utf8.ts": { uses: 1, reason: "decodeUtf8 decodes text of any length" },
  "core/src/commitment.ts": { uses: 1, reason: "a commitment prefix is the caller's own bytes, hashed before the value" },
  "proofs/src/check.ts": {
    uses: 1,
    reason: "bytesArg, for bytes with no bound of their own: an RFC 6962 leaf(data), a Merkle node's digests and a trie leaf's or branch's digests (hashed as given, as before), and bytesHash's byte strings",
  },
  "proofs/src/merkle.ts": { uses: 1, reason: "an InclusionProof's or ConsistencyProof's path keeps the digests it is given, sized when the proof is checked" },
  "signing/src/internal/check.ts": { uses: 1, reason: "a message to sign or verify has no length bound in any scheme's spec" },
  "signing/src/bls.ts": { uses: 1, reason: "BLS messages copied for aggregate checks have no length bound" },
  "signing/src/keys.ts": { uses: 1, reason: "Secret.of holds a secret of any length" },
  "signing/src/derivation.ts": { uses: 1, reason: "Icarus fromEntropy stretches entropy of any length with PBKDF2" },
};

const packages = fileURLToPath(new URL("../..", import.meta.url));

/** Every `.ts` file under `dir`, recursively. */
function sources(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((e) => (e.isDirectory() ? sources(join(dir, e.name)) : e.name.endsWith(".ts") ? [join(dir, e.name)] : []));
}

const scanned = (): string[] => [
  ...sources(join(packages, "proofs", "src")),
  ...sources(join(packages, "signing", "src")),
  // the JLVM takes no caller bytes of its own: a program's encoding goes to cbor.decode, and every value is core's
  ...sources(join(packages, "jlvm", "src")),
  ...["hash.ts", "hex.ts", "commitment.ts", "utf8.ts"].map((f) => join(packages, "core", "src", f)),
];

/** The code of `text` without its comments (a comment may name what the code must not use). */
const code = (text: string): string => text.replace(/\/\*[\s\S]*?\*\//g, "").replace(/\/\/[^\n]*/g, "");

/** Each use in `text` of a raw byte reader, an unbounded boundary form, or `.buffer` / `.byteOffset`: the invariant broken. */
export function violations(text: string): string[] {
  return [...code(text).matchAll(/\b(plainBytes|takeBytes|takeBytesWithin|ownAny|ownCopyAny)\b|\.(buffer|byteOffset)\b/g)].map((m) => m[0]);
}

/** How many times the code of `text` passes `UNBOUNDED` (an import of it aside). */
export function unboundedUses(text: string): number {
  return code(text)
    .split("\n")
    .filter((l) => !/^\s*import\b/.test(l) && !/^\s*\}\s*from\b/.test(l))
    .reduce((n, l) => n + (l.match(/\bUNBOUNDED\b/g)?.length ?? 0), 0);
}

describe("every caller byte argument passes through core's ownBytes boundary", () => {
  it("the scan finds a planted violation, and ignores one named only in a comment", () => {
    expect(violations('import { plainBytes } from "@kd5ujc/dilf4s-core/internal";\nconst v = plainBytes(x);')).toEqual(["plainBytes", "plainBytes"]);
    expect(violations("const o = view.byteOffset + caller.buffer.byteLength;")).toEqual([".byteOffset", ".buffer"]);
    expect(violations("// see core's takeBytes and ownAny\n/** plainBytes */ const ok = ownBytes(x, 32);")).toEqual([]);
    expect(violations("const view = ownAny(x);\nconst copy = ownCopyAny(y);"), "the unbounded forms").toEqual(["ownAny", "ownCopyAny"]);
    expect(unboundedUses('import { UNBOUNDED } from "x";\nconst a = ownBytes(x, UNBOUNDED); // UNBOUNDED\nconst b = ownCopy(y, UNBOUNDED);'), "a planted unbounded use").toBe(2);
  });

  it("no public-facing source in proofs, signing, jlvm or core's hash API reads caller bytes around it", () => {
    const files = scanned();
    expect(files.length, "the sources were found").toBeGreaterThan(30);
    const found = files.flatMap((f) => violations(readFileSync(f, "utf8")).map((v) => `${f.slice(packages.length)}: ${v}`));
    expect(found).toEqual([]);
  });

  it("every explicit UNBOUNDED is on the allowlist, with its reason, and no file uses more than it lists", () => {
    const uses = Object.fromEntries(scanned().map((f) => [f.slice(packages.length), unboundedUses(readFileSync(f, "utf8"))] as const).filter(([, n]) => n > 0));
    const allowed = Object.fromEntries(Object.entries(UNBOUNDED_ALLOWED).map(([f, a]) => [f, a.uses] as const));
    expect(uses, "explicit unbounded uses, by file").toEqual(allowed);
    for (const [f, a] of Object.entries(UNBOUNDED_ALLOWED)) expect(a.reason.length, `${f} states why`).toBeGreaterThan(10);
  });
});
