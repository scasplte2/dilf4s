import { readdirSync, readFileSync, realpathSync } from "node:fs";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import ts from "typescript";
import { describe, expect, it } from "vitest";

/**
 * The packages' boundary (docs/specs/canonical-encoding.md section 1): the SDK's guarantees hold through each package's
 * `exports` map alone, to a caller and to a sibling package. The map of each lists exactly the entry points below, each
 * compiled from `src/` to `dist/`, and the sources of every package (as its build compiles them) import a sibling only
 * through a listed entry point of one it declares in its `dependencies`, and its own files only within its `src/`.
 */

/** Each workspace package's entry points: `.`, and core's `./internal`, which exists for the sibling packages alone. */
const ENTRY_POINTS: Readonly<Record<string, readonly string[]>> = {
  core: [".", "./internal"],
  signing: ["."],
  proofs: ["."],
  jlvm: ["."],
};

const workspace = fileURLToPath(new URL("../../..", import.meta.url));
const packages = join(workspace, "packages");

interface Manifest {
  readonly name: string;
  readonly exports?: unknown;
  readonly dependencies?: Readonly<Record<string, string>>;
}

const readManifest = (dir: string): Manifest => JSON.parse(readFileSync(join(dir, "package.json"), "utf8")) as Manifest;

/** The package `dir` as its build (`tsconfig.build.json`) compiles it. */
function buildConfig(dir: string): ts.ParsedCommandLine {
  const path = join(dir, "tsconfig.build.json");
  const json: unknown = ts.readConfigFile(path, (p) => ts.sys.readFile(p)).config;
  return ts.parseJsonConfigFileContent(json, ts.sys, dir, undefined, path);
}

/**
 * Every file `build` compiles: its root files and, transitively, every file they import (one its config leaves out
 * included), but for the default library and what is found through `node_modules` (a sibling, a dependency).
 */
function compiled(build: ts.ParsedCommandLine): string[] {
  const program = ts.createProgram({ rootNames: build.fileNames, options: build.options, projectReferences: build.projectReferences });
  return program
    .getSourceFiles()
    .filter((f) => !program.isSourceFileDefaultLibrary(f) && !program.isSourceFileFromExternalLibrary(f))
    .map((f) => resolve(f.fileName));
}

/** Every TypeScript file under `dir`, recursively, a link included. */
function sources(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((e) => (e.isDirectory() ? sources(join(dir, e.name)) : /\.[cm]?tsx?$/.test(e.name) ? [join(dir, e.name)] : []));
}

interface Conditions {
  readonly source: string;
  readonly types: string;
  readonly import: string;
}

/**
 * What the entry point `.` (`index`) or `./x` (`x`) maps to. Node and TypeScript take the first condition that matches,
 * in the object's order, so the order is part of the map: the workspace's own `source` (which the tests and the
 * typecheck read) first, then the declarations, then the module.
 */
function conditions(entry: string): Conditions {
  const stem = entry === "." ? "index" : entry.slice("./".length);
  return { source: `./src/${stem}.ts`, types: `./dist/${stem}.d.ts`, import: `./dist/${stem}.js` };
}

/** How an `exports` field differs from the entry points `intended`, each mapped as `conditions` states. */
export function exportsProblems(exports: unknown, intended: readonly string[]): string[] {
  if (typeof exports !== "object" || exports === null || Array.isArray(exports)) return ["exports is no map of subpaths"];
  const map = exports as Readonly<Record<string, unknown>>;
  return [
    ...Object.keys(map)
      .filter((k) => !intended.includes(k))
      .map((k) => `${k}: no intended entry point`),
    ...intended.flatMap((k): string[] => {
      if (!Object.hasOwn(map, k)) return [`${k}: missing`];
      const got = map[k];
      const want = conditions(k);
      const same = typeof got === "object" && got !== null && JSON.stringify(Object.entries(got)) === JSON.stringify(Object.entries(want));
      return same ? [] : [`${k}: ${JSON.stringify(got)}, not ${JSON.stringify(want)} in that order`];
    }),
  ];
}

const literal = (e: ts.Node): string | undefined => (ts.isStringLiteralLike(e) ? e.text : undefined);

/**
 * Every module `text` names, from the syntax tree, so a comment or a string that only mentions one is not one: imports
 * and re-exports (type-only too), `import x = require(...)`, `import(...)` calls and types, `require(...)` calls,
 * `declare module` names and triple-slash references. A dynamic import or `require` whose argument is not a literal
 * names a module no scan can check, which is `undefined`.
 */
export function specifiers(fileName: string, text: string): (string | undefined)[] {
  const file = ts.createSourceFile(fileName, text, ts.ScriptTarget.Latest);
  const found: (string | undefined)[] = [...file.referencedFiles, ...file.typeReferenceDirectives].map((r) => r.fileName);
  const todo: ts.Node[] = [file];
  for (let n = todo.pop(); n !== undefined; n = todo.pop()) {
    if ((ts.isImportDeclaration(n) || ts.isExportDeclaration(n)) && n.moduleSpecifier !== undefined) found.push(literal(n.moduleSpecifier));
    else if (ts.isImportEqualsDeclaration(n) && ts.isExternalModuleReference(n.moduleReference)) found.push(literal(n.moduleReference.expression));
    else if (ts.isCallExpression(n) && (n.expression.kind === ts.SyntaxKind.ImportKeyword || (ts.isIdentifier(n.expression) && n.expression.text === "require")))
      found.push(n.arguments[0] === undefined ? undefined : literal(n.arguments[0]));
    else if (ts.isImportTypeNode(n)) found.push(ts.isLiteralTypeNode(n.argument) ? literal(n.argument.literal) : undefined);
    else if (ts.isModuleDeclaration(n) && ts.isStringLiteral(n.name)) found.push(n.name.text);
    // children pushed last first, so they are walked in source order
    const children: ts.Node[] = [];
    ts.forEachChild(n, (c) => {
      children.push(c);
    });
    todo.push(...children.reverse());
  }
  return found;
}

/** The package a bare specifier names: its scope and name, or its first segment. */
const packageOf = (spec: string): string =>
  spec
    .split("/")
    .slice(0, spec.startsWith("@") ? 2 : 1)
    .join("/");

/**
 * Why the module `spec`, which `file` of the package in `dir` (its manifest `self`) imports, crosses the boundary, or
 * `undefined` when it does not. `entries` maps each workspace package's name to the subpaths of its `exports` map.
 */
export function crossing(spec: string | undefined, file: string, dir: string, self: Manifest, entries: ReadonlyMap<string, readonly string[]>): string | undefined {
  if (spec === undefined) return "a module that is not a literal, which no scan can check";
  if (spec.startsWith("file:") || isAbsolute(spec)) return `${spec}: a file's path or URL, outside every exports map`;
  if (spec.startsWith(".")) return resolve(dirname(file), spec).startsWith(join(dir, "src") + sep) ? undefined : `${spec}: outside its package's src`;
  const name = packageOf(spec);
  if (name === self.name) return `${spec}: its own package, by name`;
  if (!Object.hasOwn(self.dependencies ?? {}, name)) return `${spec}: ${name} is not in its dependencies`;
  const listed = entries.get(name);
  if (listed === undefined) return name.startsWith("@kd5ujc/") ? `${spec}: no workspace package` : undefined;
  const subpath = `.${spec.slice(name.length)}`;
  return listed.includes(subpath) ? undefined : `${spec}: ${subpath} is not an entry point of ${name}`;
}

describe("the packages' exports boundary", { timeout: 60_000 }, () => {
  const manifests = Object.keys(ENTRY_POINTS).map((p) => [p, readManifest(join(packages, p))] as const);
  const entries = new Map(manifests.map(([, m]) => [m.name, typeof m.exports === "object" && m.exports !== null ? Object.keys(m.exports) : []] as const));

  it("the checks find planted violations, and ignore a deep import named only in a comment or a string", () => {
    const core = { ".": conditions("."), "./internal": conditions("./internal") };
    expect(exportsProblems(core, ENTRY_POINTS["core"] ?? []), "core's own map").toEqual([]);
    expect(exportsProblems({ ...core, "./bytes": conditions("./bytes") }, [".", "./internal"]), "an extra subpath").toEqual(["./bytes: no intended entry point"]);
    expect(exportsProblems({ ".": conditions("."), "./*": "./dist/*.js" }, ["."]), "a subpath pattern").toEqual(["./*: no intended entry point"]);
    expect(exportsProblems({ ".": conditions(".") }, [".", "./internal"]), "a missing entry point").toEqual(["./internal: missing"]);
    const { source, types, import: module } = conditions(".");
    const wrong = [{ types, source, import: module }, { source, types, import: module, require: "./dist/index.cjs" }, { source: "./src/bytes.ts", types, import: module }, { source, types, import: { default: module } }, "./dist/index.js"];
    for (const [i, e] of wrong.entries()) expect(exportsProblems({ ".": e }, ["."]), `a wrong entry, case ${i.toString()}`).toEqual([expect.stringMatching(/^\.: .* in that order$/)]);
    expect(exportsProblems("./dist/index.js", ["."]), "a bare string").toEqual(["exports is no map of subpaths"]);

    const dir = join(packages, "proofs");
    const file = join(dir, "src", "sub", "merkle.ts");
    const self: Manifest = { name: "@kd5ujc/dilf4s-proofs", dependencies: { "@kd5ujc/dilf4s-core": "0.1.0", "@kd5ujc/dilf4s-evil": "0.1.0", "@noble/hashes": "2.4.0" } };
    const text = [
      '/// <reference path="../../../core/src/value.ts" />',
      'import { ok } from "@kd5ujc/dilf4s-core";',
      'import { ownBytes } from "@kd5ujc/dilf4s-core/internal";',
      'import { local } from "../check.js";',
      'import { sha256 } from "@noble/hashes/sha2.js";',
      '// import { evil } from "@kd5ujc/dilf4s-core/src/bytes.js";',
      "/* export * from \"../../../core/src/bytes.js\"; */",
      "const s = \"import('@kd5ujc/dilf4s-core/dist/bytes.js')\";",
      'import { takeBytes } from "@kd5ujc/dilf4s-core/src/bytes.js";',
      'import type { Shape } from "@kd5ujc/dilf4s-core/dist/codec.js";',
      'export * from "../../../core/src/bytes.js";',
      'export { sign } from "@kd5ujc/dilf4s-signing";',
      'type T = import("@kd5ujc/dilf4s-core/internal/bytes").T;',
      "const m = await import(spec);",
      'const n = await import("file:///sdk/ts/packages/core/dist/bytes.js");',
      'import x = require("/sdk/ts/packages/core/dist/bytes.js");',
      'declare module "@kd5ujc/dilf4s-core/dist/value.js" {}',
      'import "@kd5ujc/dilf4s-proofs";',
      'import "@kd5ujc/dilf4s-evil";',
      'import { ed25519 } from "@noble/curves/ed25519.js";',
    ].join("\n");
    expect(specifiers(file, text).map((s) => crossing(s, file, dir, self, entries))).toEqual([
      "../../../core/src/value.ts: outside its package's src",
      undefined,
      undefined,
      undefined,
      undefined,
      "@kd5ujc/dilf4s-core/src/bytes.js: ./src/bytes.js is not an entry point of @kd5ujc/dilf4s-core",
      "@kd5ujc/dilf4s-core/dist/codec.js: ./dist/codec.js is not an entry point of @kd5ujc/dilf4s-core",
      "../../../core/src/bytes.js: outside its package's src",
      "@kd5ujc/dilf4s-signing: @kd5ujc/dilf4s-signing is not in its dependencies",
      "@kd5ujc/dilf4s-core/internal/bytes: ./internal/bytes is not an entry point of @kd5ujc/dilf4s-core",
      "a module that is not a literal, which no scan can check",
      "file:///sdk/ts/packages/core/dist/bytes.js: a file's path or URL, outside every exports map",
      "/sdk/ts/packages/core/dist/bytes.js: a file's path or URL, outside every exports map",
      "@kd5ujc/dilf4s-core/dist/value.js: ./dist/value.js is not an entry point of @kd5ujc/dilf4s-core",
      "@kd5ujc/dilf4s-proofs: its own package, by name",
      "@kd5ujc/dilf4s-evil: no workspace package",
      "@noble/curves/ed25519.js: @noble/curves is not in its dependencies",
    ]);
  });

  it("the workspace is exactly these packages", () => {
    const root = JSON.parse(readFileSync(join(workspace, "package.json"), "utf8")) as { readonly workspaces: readonly string[] };
    expect([...root.workspaces].sort()).toEqual(Object.keys(ENTRY_POINTS).map((p) => `packages/${p}`).sort());
  });

  it("each package's exports map lists exactly its entry points, and no other field names an entry", () => {
    for (const [p, m] of manifests) {
      expect(m.name, p).toBe(`@kd5ujc/dilf4s-${p}`);
      expect(exportsProblems(m.exports, ENTRY_POINTS[p] ?? []), p).toEqual([]);
      // with an exports map these are ignored by Node, but read by older resolvers, which would then reach any file
      expect(["main", "module", "types", "typings", "typesVersions", "browser"].filter((f) => f in m), p).toEqual([]);
    }
  });

  it("each entry point's source is a file the build compiles, and its types and module are what the build emits from it", () => {
    for (const [p] of manifests) {
      const dir = join(packages, p);
      const build = buildConfig(dir);
      expect(build.errors.map((e) => ts.flattenDiagnosticMessageText(e.messageText, "\n")), p).toEqual([]);
      for (const e of ENTRY_POINTS[p] ?? []) {
        const c = conditions(e);
        expect(build.fileNames.map((f) => resolve(f)), `${p} ${e}`).toContain(resolve(dir, c.source));
        const emitted = ts.getOutputFileNames(build, resolve(dir, c.source), false).map((f) => resolve(f));
        expect(emitted, `${p} ${e}`).toEqual(expect.arrayContaining([resolve(dir, c.types), resolve(dir, c.import)]));
      }
    }
  });

  it("a file the build's config leaves out, but a compiled source imports, is still scanned", () => {
    const build = buildConfig(join(packages, "proofs"));
    const check = join(packages, "proofs", "src", "check.ts");
    const without = { ...build, fileNames: build.fileNames.filter((f) => resolve(f) !== check) };
    expect(without.fileNames.length, "check.ts is left out of the roots").toBe(build.fileNames.length - 1);
    expect(compiled(without), "and compiled, as merkle.ts imports it").toContain(check);
  });

  /**
   * Each package's files: every one its build compiles and every source in its src, each with whether it is within its
   * src by its real path (so a link out of src is not).
   */
  const scan = (): { p: string; m: Manifest; dir: string; files: { file: string; within: boolean }[] }[] =>
    manifests.map(([p, m]) => {
      const dir = join(packages, p);
      const src = realpathSync(join(dir, "src"));
      const files = [...new Set([...compiled(buildConfig(dir)), ...sources(join(dir, "src"))])];
      return { p, m, dir, files: files.map((file) => ({ file, within: realpathSync(file).startsWith(src + sep) })) };
    });

  it("every file a package's build compiles, and every source in its src, is within its src", () => {
    const outside = scan().flatMap(({ p, files }) => files.filter((f) => !f.within).map((f) => `${p}: ${relative(packages, f.file)}`));
    expect(outside).toEqual([]);
  });

  it("every module a package's sources import is a declared dependency's entry point, or a file of its own src", () => {
    // a file outside its src is refused above, and the import that reached it here
    const scanned = scan().flatMap(({ m, dir, files }) => files.filter((f) => f.within).map(({ file }) => ({ dir, m, file, specs: specifiers(file, readFileSync(file, "utf8")) })));
    expect(scanned.length, "the sources were found").toBeGreaterThan(80);
    const siblings = scanned.flatMap((s) => s.specs).filter((s) => s?.startsWith("@kd5ujc/") === true);
    expect(siblings.length, "the sibling imports were found").toBeGreaterThan(50);
    expect([...new Set(siblings)], "both of core's entry points are imported").toEqual(expect.arrayContaining(["@kd5ujc/dilf4s-core", "@kd5ujc/dilf4s-core/internal"]));
    const found = scanned.flatMap((s) => s.specs.flatMap((spec) => ((c): string[] => (c === undefined ? [] : [`${relative(packages, s.file)}: ${c}`]))(crossing(spec, s.file, s.dir, s.m, entries))));
    expect(found).toEqual([]);
  });
});
