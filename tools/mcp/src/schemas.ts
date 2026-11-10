/**
 * The tools as an MCP client lists them: names, descriptions, and JSON Schemas of their arguments and results. Every
 * result, a refusal included, matches its tool's output schema; the in-memory tests check each one through the SDK's
 * client, which validates every structured result against the schema it listed.
 */
import { MAX_BINDINGS, MAX_BINDINGS_BYTES, MAX_OUTPUT_BYTES, MAX_TEXT_BYTES } from "./limits.js";
import type { ToolName } from "./tools.js";

type Schema = Readonly<Record<string, unknown>>;

const HEX = { type: "string", pattern: "^([0-9a-f]{2})*$" } as const;
const DIGEST = { type: "string", pattern: "^[0-9a-f]{64}$" } as const;
const COUNT = { type: "integer", minimum: 0 } as const;

const SURFACE = `JSON surface text (docs/specs/jlvm.md section 1): canonical JSON, with "0x" + lowercase hex strings as bytes; at most ${MAX_TEXT_BYTES.toString()} bytes, counting each number's exponent as that many digits.`;

const refusal: Schema = {
  type: "object",
  required: ["ok", "refusal"],
  properties: {
    ok: { const: false },
    refusal: {
      type: "object",
      required: ["kind", "message", "where"],
      properties: {
        kind: { enum: ["InvalidArguments", "UnknownTool", "InputTooLarge", "GasLimitOutOfRange", "Malformed", "Limit", "OutputTooLarge"] },
        message: { type: "string" },
        where: {
          type: "object",
          required: ["argument"],
          properties: { argument: { type: "string" }, binding: { type: "string" }, offset: COUNT },
          additionalProperties: false,
        },
      },
      additionalProperties: false,
    },
  },
  additionalProperties: false,
};

const oneOf = (success: Schema): Schema => ({ type: "object", anyOf: [success, refusal] });

const errorRecord: Schema = {
  type: "object",
  required: ["error"],
  properties: {
    error: { enum: ["Malformed", "Type", "Arity", "Arithmetic", "Domain", "Crypto", "Limit", "OutOfGas"] },
    op: { type: "string" },
    cause: { type: "object", minProperties: 1, maxProperties: 1, additionalProperties: { type: "string" } },
    message: { type: "string" },
    required: COUNT,
    available: COUNT,
  },
  additionalProperties: false,
};

const value: Schema = {
  type: "object",
  required: ["weight"],
  properties: {
    weight: COUNT,
    cbor: HEX,
    json: { type: "string" },
    omitted: { type: "object", properties: { cbor: { const: "TooLarge" }, json: { enum: ["NoSurfaceForm", "TooLarge"] } }, additionalProperties: false },
  },
  additionalProperties: false,
};

const statement: Schema = {
  type: "object",
  required: ["version", "language", "schedule", "program", "input", "bindings", "gasLimit", "gasUsed", "outcome"],
  properties: {
    version: { const: 1 },
    language: COUNT,
    schedule: COUNT,
    program: DIGEST,
    input: DIGEST,
    bindings: DIGEST,
    gasLimit: COUNT,
    gasUsed: COUNT,
    outcome: { anyOf: [{ type: "object", required: ["result"], properties: { result: DIGEST }, additionalProperties: false }, errorRecord] },
  },
  additionalProperties: false,
};

const program = { type: "string", description: `The program: ${SURFACE}` } as const;

/** A tool as `tools/list` gives it. */
export interface ToolDescriptor {
  readonly name: ToolName;
  readonly title: string;
  readonly description: string;
  readonly inputSchema: Schema;
  readonly outputSchema: Schema;
  readonly annotations: Readonly<Record<string, boolean | string>>;
}

const annotations = (title: string): Readonly<Record<string, boolean | string>> => ({ title, readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false });

/** The tools, in the order a client lists them. */
export const TOOLS: readonly ToolDescriptor[] = [
  {
    name: "jlvm_check",
    title: "Check a JLVM program",
    description:
      "Parses and compiles a JLVM program (dilf4s JLVM, language 1). Returns its identity (taggedHash SHA-256 \"dilf4s/jlvm/program/v1\" of its canonical CBOR), its canonical form as hex, and the canonical form's weight and depth; or the refusal: its kind, message and where (the argument, and the offset of a JSON error). Example: {\"program\": \"{\\\"+\\\": [1, {\\\"var\\\": \\\"x\\\"}]}\"}.",
    inputSchema: { type: "object", properties: { program }, required: ["program"], additionalProperties: false },
    outputSchema: oneOf({
      type: "object",
      required: ["ok", "id", "weight", "depth"],
      properties: { ok: { const: true }, id: DIGEST, canonical: HEX, omitted: { type: "object", properties: { canonical: { const: "TooLarge" } }, additionalProperties: false }, weight: COUNT, depth: COUNT },
      additionalProperties: false,
    }),
    annotations: annotations("Check a JLVM program"),
  },
  {
    name: "jlvm_evaluate",
    title: "Evaluate a JLVM program",
    description: `Evaluates a JLVM program on input data, with host bindings (read as {"var": "$name"}) and a gas limit (default 2^24, at most the server's cap). Deterministic: the result is a function of the arguments alone. Returns the outcome as the conformance vectors record it (a value as canonical CBOR hex and surface JSON, or an error's kind, op, cause and message), the gas used, the warnings, and the zk-jlvm statement (docs/specs/zk-jlvm.md): the program's identity, the input and bindings commitments, the outcome's digest, and its canonical CBOR, which a proved call's public values must equal. A text that does not parse is refused before anything runs. Example: {"program": "{\\"*\\": [{\\"var\\": \\"$rate\\"}, {\\"var\\": \\"amount\\"}]}", "data": "{\\"amount\\": 40}", "bindings": {"rate": "0.5"}, "gasLimit": 100000}.`,
    inputSchema: {
      type: "object",
      properties: {
        program,
        data: { type: "string", description: `The input data, read with {"var": "path"}: ${SURFACE} "null" unless given.` },
        bindings: {
          type: "object",
          description: `The host bindings: each name to its value's ${SURFACE} At most ${MAX_BINDINGS.toString()}, measuring at most ${MAX_BINDINGS_BYTES.toString()} bytes together, names included.`,
          additionalProperties: { type: "string" },
        },
        gasLimit: { type: "integer", minimum: 0, description: "The gas limit: 2^24 unless given; a limit above the server's cap (jlvm_ops gives it) is refused." },
      },
      required: ["program"],
      additionalProperties: false,
    },
    outputSchema: oneOf({
      type: "object",
      required: ["ok", "outcome", "gasUsed", "gasLimit", "warnings", "statement", "statementCbor"],
      properties: {
        ok: { const: true },
        outcome: { anyOf: [{ type: "object", required: ["value"], properties: { value }, additionalProperties: false }, { type: "object", required: ["error"], properties: { error: errorRecord }, additionalProperties: false }] },
        gasUsed: COUNT,
        gasLimit: COUNT,
        warnings: { type: "array", items: { type: "object", required: ["kind", "path", "pathBytes"], properties: { kind: { type: "string" }, path: { type: "string" }, pathBytes: COUNT }, additionalProperties: false } },
        statement,
        statementCbor: HEX,
      },
      additionalProperties: false,
    }),
    annotations: annotations("Evaluate a JLVM program"),
  },
  {
    name: "jlvm_ops",
    title: "List the JLVM operations",
    description: `The closed list of JLVM operations (language 1, gas schedule 1), or the one named (an alias too): each with its summary, aliases, class (form, strict or crypto), arity, argument kinds, an example program and data, and its gas cost; with the schedule's scalar charges, the default gas limit and this server's cap. Values and renderings in results are bounded at ${MAX_OUTPUT_BYTES.toString()} bytes each. Example: {"op": "merkle_verify"}.`,
    inputSchema: { type: "object", properties: { op: { type: "string", description: "One operation's name or alias; every operation unless given." } }, additionalProperties: false },
    outputSchema: oneOf({
      type: "object",
      required: ["ok", "language", "schedule", "gas", "ops"],
      properties: {
        ok: { const: true },
        language: COUNT,
        schedule: COUNT,
        gas: { type: "object", additionalProperties: COUNT },
        ops: {
          type: "array",
          items: {
            type: "object",
            required: ["op", "aliases", "class", "variadic", "summary", "arity", "params", "example", "cost"],
            properties: {
              op: { type: "string" },
              aliases: { type: "array", items: { type: "string" } },
              class: { enum: ["form", "strict", "crypto"] },
              variadic: { type: "boolean" },
              summary: { type: "string" },
              arity: { type: "object", required: ["min", "max"], properties: { min: COUNT, max: { anyOf: [COUNT, { type: "null" }] } }, additionalProperties: false },
              params: { type: "array", items: { type: "object", required: ["name", "kinds"] } },
              example: { type: "object", required: ["program", "data"], properties: { program: { type: "string" }, data: { type: "string" } }, additionalProperties: false },
              cost: { type: "object", required: ["base", "perUnit", "unit", "extra"] },
            },
            additionalProperties: false,
          },
        },
      },
      additionalProperties: false,
    }),
    annotations: annotations("List the JLVM operations"),
  },
];
