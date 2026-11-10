import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/**
 * No ambient access, checked over the source: no module but the bin (the stdio edge) names the process, a Node
 * module, the file system, the network, a clock, randomness or a way to load code, and each module imports only its
 * allowed modules. The tools are therefore functions of their arguments alone.
 */

const src = fileURLToPath(new URL("../src", import.meta.url));

/** The modules each source may import, besides its own package's files. */
const IMPORTS: Readonly<Record<string, readonly string[]>> = {
  "bin.ts": [],
  "stdio.ts": ["@modelcontextprotocol/sdk/shared/transport.js", "@modelcontextprotocol/sdk/types.js"],
  "admission.ts": ["@modelcontextprotocol/sdk/shared/transport.js", "@modelcontextprotocol/sdk/types.js"],
  "server.ts": ["@modelcontextprotocol/sdk/server/index.js", "@modelcontextprotocol/sdk/shared/transport.js", "@modelcontextprotocol/sdk/types.js"],
};
const LIBRARIES = ["@kd5ujc/dilf4s-core", "@kd5ujc/dilf4s-jlvm"];

/** The code of `text` without its comments (a comment may name what the code must not use). */
const code = (text: string): string => text.replace(/\/\*[\s\S]*?\*\//g, "").replace(/\/\/[^\n]*/g, "");

const AMBIENT = /\b(process|globalThis|window|require|fetch|XMLHttpRequest|WebSocket|Date|performance|setTimeout|setInterval|setImmediate|queueMicrotask|eval|Function|Deno|Bun|navigator)\b|\bcrypto\s*[.[]|Math\.random|\bimport\s*\(/g;

/** The code of `text` without its comments or its quoted strings (a string may hold such a word as data). */
const bare = (text: string): string => code(text).replace(/"(?:[^"\\\n]|\\.)*"|'(?:[^'\\\n]|\\.)*'/g, '""');

/** Each ambient name the code of `text` uses. */
export function ambient(text: string): string[] {
  return [...bare(text).matchAll(AMBIENT)].map((m) => m[0]);
}

/** The modules `text` imports. */
export function imports(text: string): string[] {
  return [...code(text).matchAll(/\bfrom\s+"([^"]+)"/g)].map((m) => m[1] ?? "");
}

describe("no tool has ambient access", () => {
  it("the scan finds planted uses, and ignores one named only in a comment", () => {
    expect(ambient('const t = Date.now();\nconst e = process.env["X"];\nawait fetch(u);\nconst m = await import("fs");\nconst r = crypto.getRandomValues(b);')).toEqual(["Date", "process", "fetch", "import(", "crypto."]);
    expect(ambient('// process.env and Date.now()\n/* fetch */ const x = "crypto";')).toEqual([]);
    expect(imports('import { readFileSync } from "node:fs";\nimport type { T } from "./t.js";')).toEqual(["node:fs", "./t.js"]);
  });

  it("only the bin names the process, and every module imports only what it may", () => {
    const files = readdirSync(src).filter((f) => f.endsWith(".ts"));
    expect(files.length, "the sources were found").toBe(11);
    const found = files.flatMap((f) => {
      const text = readFileSync(join(src, f), "utf8");
      const uses = f === "bin.ts" ? ambient(text).filter((u) => u !== "process") : ambient(text);
      const allowed = [...LIBRARIES, ...(IMPORTS[f] ?? [])];
      const modules = imports(text).filter((m) => !m.startsWith("./") && !allowed.includes(m));
      return [...uses.map((u) => `${f}: ${u}`), ...modules.map((m) => `${f}: imports ${m}`)];
    });
    expect(found).toEqual([]);
  });
});
