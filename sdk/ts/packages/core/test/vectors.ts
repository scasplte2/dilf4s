import { readFileSync, readdirSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { type BoolValue, type BytesValue, cbor, type IntValue, json, type Result, Value } from "../src/index.js";
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

/** The integer `n`, which a test knows is within the data model's bound (`MAX_INTEGER_BITS`). */
export const int = (n: bigint): IntValue => expectOk(Value.int(n), "an integer within MAX_INTEGER_BITS");

/** The byte string holding a copy of `b`, which a test knows is a `Uint8Array`. */
export const bytesValue = (b: Uint8Array): BytesValue => expectOk(Value.bytes(b), "a Uint8Array");

/** The boolean `b`, which a test knows is a boolean. */
export const boolValue = (b: boolean): BoolValue => expectOk(Value.bool(b), "a boolean");

/**
 * The canonical CBOR of the integer `n`, whatever its width: what {@link cbor.encode} writes when `n` is a value, and for
 * a bignum too wide to be one, the bytes a decoder must refuse. Its magnitude is at most 65535 bytes.
 */
export function integerBytes(n: bigint): Uint8Array {
  if (n >= -(1n << 64n) && n < 1n << 64n) return expectOk(cbor.encode(int(n)), "encodes");
  const magnitude = cbor.magnitudeBytes(n < 0n ? -1n - n : n);
  return Uint8Array.from([n < 0n ? 0xc3 : 0xc2, 0x59, magnitude.length >> 8, magnitude.length & 0xff, ...magnitude]);
}

/** A vector file parsed with this SDK's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, codec: Codec<T>): T {
  const value = expectOk(json.parse(readVectors(file)), `${file} is canonical JSON`);
  const decoded = codec.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}
