import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { type BoolValue, type BytesValue, cbor, type codec, describeFailure, fromHex, type IntValue, json, type Result, Value, toHex } from "@kd5ujc/dilf4s-core";

type Codec<T> = codec.Codec<T>;

/** The repository root, for the shared vectors and the Scala reference files. */
export const repoDir = fileURLToPath(new URL("../../../../../", import.meta.url));

export const readRepoFile = (path: string): string => readFileSync(repoDir + path, "utf8");

/** The value of a success; a failure fails the test with its error. */
export function expectOk<T, E>(r: Result<T, E>, what: string): T {
  if (!r.ok) throw new Error(`${what}: ${JSON.stringify(r.error, (_, x: unknown) => (typeof x === "bigint" ? x.toString() : x))}`);
  return r.value;
}

/** The integer `n`, which a test knows is within the data model's bound (`MAX_INTEGER_BITS`). */
export const int = (n: bigint): IntValue => expectOk(Value.int(n), "an integer within MAX_INTEGER_BITS");

/** The byte string holding a copy of `b`, which a test knows is a `Uint8Array`. */
export const bytesValue = (b: Uint8Array): BytesValue => expectOk(Value.bytes(b), "a Uint8Array");

/** The boolean `b`, which a test knows is a boolean. */
export const boolValue = (b: boolean): BoolValue => expectOk(Value.bool(b), "a boolean");

/** The error of a failure; a success fails the test. */
export function expectErr<T, E>(r: Result<T, E>, what: string): E {
  if (r.ok) throw new Error(`${what}: expected a failure`);
  return r.error;
}

/** A vector file parsed with core's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, codec: Codec<T>): T {
  const value = expectOk(json.parse(readRepoFile(`shared/vectors/${file}`)), `${file} is canonical JSON`);
  const decoded = codec.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}

export const bytes = (hex: string): Uint8Array => expectOk(fromHex(hex), "hex");

/** The canonical value of hex CBOR. */
export const valueOf = (hex: string): Value => expectOk(cbor.decode(bytes(hex)), "canonical CBOR");

/** Lowercase hex of bytes, or of a success's bytes; a failure (or what `toHex` refuses) fails the test. */
export const hexOf = (b: Uint8Array | Result<Uint8Array, unknown>): string => expectOk(toHex(b instanceof Uint8Array ? b : expectOk(b, "bytes")), "hex");
