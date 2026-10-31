import { readFileSync, readdirSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { json, type Result } from "../src/index.js";
import type { Codec } from "../src/codec.js";
import { describeFailure } from "../src/error.js";

/** The repository's shared/vectors, read at test time. */
export const vectorsDir = fileURLToPath(new URL("../../../../../shared/vectors/", import.meta.url));

export const vectorFiles = (): string[] => readdirSync(vectorsDir).filter((f) => f.endsWith(".json")).sort();

export const readVectors = (file: string): string => readFileSync(vectorsDir + file, "utf8");

/** The value of a success; a failure fails the test with its error. */
export function expectOk<T, E>(r: Result<T, E>, what: string): T {
  if (!r.ok) throw new Error(`${what}: ${JSON.stringify(r.error, (_, x: unknown) => (typeof x === "bigint" ? x.toString() : x))}`);
  return r.value;
}

/** A vector file parsed with this SDK's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, codec: Codec<T>): T {
  const value = expectOk(json.parse(readVectors(file)), `${file} is canonical JSON`);
  const decoded = codec.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}
