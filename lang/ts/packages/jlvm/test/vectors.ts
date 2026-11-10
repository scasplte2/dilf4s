import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { ArrayValue, cbor, type codec, describeFailure, fromHex, json, MapValue, type models, type Result, toHex, type Value } from "@kd5ujc/dilf4s-core";

type Codec<T> = codec.Codec<T>;

/** The repository root, for the shared vectors and the specification. */
export const repoDir = fileURLToPath(new URL("../../../../../", import.meta.url));

export const readRepoFile = (path: string): string => readFileSync(repoDir + path, "utf8");

/** The value of a success; a failure fails the test with its error. */
export function expectOk<T, E>(r: Result<T, E>, what: string): T {
  if (!r.ok) throw new Error(`${what}: ${JSON.stringify(r.error, (_, x: unknown) => (typeof x === "bigint" ? x.toString() : x))}`);
  return r.value;
}

/** The error of a failure; a success fails the test. */
export function expectErr<T, E>(r: Result<T, E>, what: string): E {
  if (r.ok) throw new Error(`${what}: expected a failure`);
  return r.error;
}

/** A vector file parsed with core's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, c: Codec<T>): T {
  const value = expectOk(json.parse(readRepoFile(`shared/vectors/${file}`)), `${file} is canonical JSON`);
  const decoded = c.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}

export const bytes = (text: string): Uint8Array => expectOk(fromHex(text), "hex");

/** The lowercase hex of bytes a test holds. */
export const hex = (b: Uint8Array): string => expectOk(toHex(b), "bytes");

/** The canonical value of hex CBOR. */
export const valueOf = (hex: string): Value => expectOk(cbor.decode(bytes(hex)), "canonical CBOR");

/** Every text `"$"` in `template` replaced by `inner`, which is shared, never copied. */
function substitute(template: Value, inner: Value): Value {
  switch (template.kind) {
    case "text":
      return template.value === "$" ? inner : template;
    case "array":
      return expectOk(ArrayValue.of(template.items.map((x) => substitute(x, inner))), "a template array");
    case "map":
      return expectOk(MapValue.of(template.entries().map(([k, x]) => [k, substitute(x, inner)] as const)), "a template map");
    case "null":
    case "bool":
    case "int":
    case "rational":
    case "bytes":
      return template;
  }
}

/**
 * A `built-value` binding (`jlvm-vectors.cddl`), built with sharing: from `leaf`, each step in order, so a value far
 * heavier than any text could carry is a few objects, and is never encoded or copied whole.
 */
export function built(b: models.jlvmVectors.BuiltValue): Value {
  let v = valueOf(b.leaf);
  for (const step of b.steps ?? []) {
    if ("copies" in step) {
      const n = Number(step.copies);
      v = expectOk(ArrayValue.of(Array.from({ length: n }, () => v)), "copies");
    } else {
      const t = valueOf(step.template);
      for (let i = 0n; i < step.times; i++) v = substitute(t, v);
    }
  }
  return v;
}
