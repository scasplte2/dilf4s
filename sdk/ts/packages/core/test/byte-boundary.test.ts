import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/**
 * The byte boundary's invariant (docs/specs/canonical-encoding.md section 1, "Caller bytes"), checked over the source:
 * the public-facing code of proofs and signing, and core's hash entry points, take a caller's bytes through core's
 * `ownBytes` family alone. None of it may name the raw readers (`plainBytes`, `takeBytes`, `takeBytesWithin`) or read a
 * value's `.buffer` or `.byteOffset`: those belong to the boundary itself (`core/src/bytes.ts`).
 */

const packages = fileURLToPath(new URL("../..", import.meta.url));

/** Every `.ts` file under `dir`, recursively. */
function sources(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((e) => (e.isDirectory() ? sources(join(dir, e.name)) : e.name.endsWith(".ts") ? [join(dir, e.name)] : []));
}

const scanned = (): string[] => [
  ...sources(join(packages, "proofs", "src")),
  ...sources(join(packages, "signing", "src")),
  ...["hash.ts", "hex.ts", "commitment.ts", "utf8.ts"].map((f) => join(packages, "core", "src", f)),
];

/** The code of `text` without its comments (a comment may name what the code must not use). */
const code = (text: string): string => text.replace(/\/\*[\s\S]*?\*\//g, "").replace(/\/\/[^\n]*/g, "");

/** Each use in `text` of a raw byte reader, or of `.buffer` / `.byteOffset`: the boundary's invariant broken. */
export function violations(text: string): string[] {
  return [...code(text).matchAll(/\b(plainBytes|takeBytes|takeBytesWithin)\b|\.(buffer|byteOffset)\b/g)].map((m) => m[0]);
}

describe("every caller byte argument passes through core's ownBytes boundary", () => {
  it("the scan finds a planted violation, and ignores one named only in a comment", () => {
    expect(violations('import { plainBytes } from "@kd5ujc/dilf4s-core/internal";\nconst v = plainBytes(x);')).toEqual(["plainBytes", "plainBytes"]);
    expect(violations("const o = view.byteOffset + caller.buffer.byteLength;")).toEqual([".byteOffset", ".buffer"]);
    expect(violations("// see core's takeBytes\n/** plainBytes */ const ok = ownAny(x);")).toEqual([]);
  });

  it("no public-facing source in proofs, signing or core's hash API reads caller bytes around it", () => {
    const files = scanned();
    expect(files.length, "the sources were found").toBeGreaterThan(20);
    const found = files.flatMap((f) => violations(readFileSync(f, "utf8")).map((v) => `${f.slice(packages.length)}: ${v}`));
    expect(found).toEqual([]);
  });
});
