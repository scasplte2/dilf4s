import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { ArrayValue, cbor, type codec, describeFailure, fromHex, json, MapValue, type models, type Result, type Value } from "@kd5ujc/dilf4s-core";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { InMemoryTransport } from "@modelcontextprotocol/sdk/inMemory.js";
import { expect } from "vitest";

import { createServer, type ServerOptions } from "../src/index.js";

/** The repository root, for the shared vectors and the specification. */
export const repoDir = fileURLToPath(new URL("../../../../../", import.meta.url));

export function expectOk<T, E>(r: Result<T, E>, what: string): T {
  if (!r.ok) throw new Error(`${what}: ${JSON.stringify(r.error, (_, x: unknown) => (typeof x === "bigint" ? x.toString() : x))}`);
  return r.value;
}

/** A vector file parsed with core's JSON parser and decoded into its generated type. */
export function loadVectors<T>(file: string, c: codec.Codec<T>): T {
  const value = expectOk(json.parse(readFileSync(`${repoDir}shared/vectors/${file}`, "utf8")), `${file} is canonical JSON`);
  const decoded = c.decode(value);
  if (!decoded.ok) throw new Error(`${file} does not fit its schema: ${describeFailure(decoded.error)}`);
  return decoded.value;
}

const valueOf = (hex: string): Value => expectOk(cbor.decode(expectOk(fromHex(hex), "hex")), "canonical CBOR");

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

/** A `built-value` binding (`jlvm-vectors.cddl`), built with sharing, as the jlvm package's conformance test builds it. */
export function built(b: models.jlvmVectors.BuiltValue): Value {
  let v = valueOf(b.leaf);
  for (const step of b.steps ?? []) {
    if ("copies" in step) v = expectOk(ArrayValue.of(Array.from({ length: Number(step.copies) }, () => v)), "copies");
    else {
      const t = valueOf(step.template);
      for (let i = 0n; i < step.times; i++) v = substitute(t, v);
    }
  }
  return v;
}

/** What a tool call returned: its structured content, as the client validated it against the tool's output schema. */
export interface Called {
  readonly structured: Readonly<Record<string, unknown>>;
  readonly isError: boolean;
  readonly text: string;
}

/** A client connected in memory to a new server, its tools listed first so that it validates every result. */
export async function connected(options?: ServerOptions): Promise<{ readonly call: (name: string, args?: unknown) => Promise<Called>; readonly close: () => Promise<void> }> {
  const made = createServer(options);
  if (!made.ok) throw new Error(made.error);
  const [clientSide, serverSide] = InMemoryTransport.createLinkedPair();
  await made.value.connect(serverSide);
  const client = new Client({ name: "dilf4s-mcp-test", version: "0.0.0" });
  await client.connect(clientSide);
  await client.listTools();
  const call = async (name: string, args?: unknown): Promise<Called> => {
    const r = await client.callTool({ name, arguments: args as Record<string, unknown> | undefined });
    const content = r.content as readonly { readonly type: string; readonly text?: string }[];
    const structured = r.structuredContent as Readonly<Record<string, unknown>>;
    const text = content[0]?.text ?? "";
    expect(content, "one text block").toHaveLength(1);
    expect(text, "the text block is the structured content").toBe(JSON.stringify(structured));
    return { structured, isError: r.isError === true, text };
  };
  return { call, close: () => client.close() };
}

/** A refusal's kind, or `undefined` for a success. */
export const refusalKind = (c: Called): string | undefined => (c.structured["ok"] === true ? undefined : (c.structured["refusal"] as { readonly kind: string }).kind);
