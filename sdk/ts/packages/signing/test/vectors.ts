import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { type codec, describeFailure, fromHex, json, type Result } from "@kd5ujc/dilf4s-core";
type Codec<T> = codec.Codec<T>;

/** The repository's shared/vectors and docs/test-vectors, read at test time. */
export const vectorsDir = fileURLToPath(new URL("../../../../../shared/vectors/", import.meta.url));
export const docsDir = fileURLToPath(new URL("../../../../../docs/test-vectors/", import.meta.url));
export const resourcesDir = fileURLToPath(new URL("../../../../../crypto/src/main/resources/", import.meta.url));

export const readDoc = (path: string): string => readFileSync(docsDir + path, "utf8");
export const readDocBytes = (path: string): Uint8Array => readFileSync(docsDir + path);
export const readResource = (path: string): string => readFileSync(resourcesDir + path, "utf8");

/** The value of a success; a failure fails the test with its error. */
export function expectOk<T, E>(r: Result<T, E>, what: string): T {
  if (!r.ok) throw new Error(`${what}: ${JSON.stringify(r.error, (_, x: unknown) => (typeof x === "bigint" ? x.toString() : x))}`);
  return r.value;
}

/** A vector file parsed with core's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, codec: Codec<T>): T {
  const value = expectOk(json.parse(readFileSync(vectorsDir + file, "utf8")), `${file} is canonical JSON`);
  const decoded = codec.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}

/** Bytes of hex text (an optional `0x` prefix is dropped). */
export function h(text: string): Uint8Array {
  return expectOk(fromHex(text.startsWith("0x") ? text.slice(2) : text), "hex");
}

export const utf8 = (s: string): Uint8Array => new TextEncoder().encode(s);

/** Tab-separated rows of a docs/test-vectors file. */
export const tsv = (path: string): string[][] =>
  readDoc(path)
    .split("\n")
    .filter((l) => l.length > 0)
    .map((l) => l.split("\t"));
