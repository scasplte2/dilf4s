/**
 * The tools, as pure functions of their arguments: each returns a structured result, a refusal included, and nothing
 * throws, reads a clock, the environment, a file or the network. The same arguments give the same result.
 *
 * Every argument is read and bounded before any is parsed (`limits.ts`, `measure.ts`), in a fixed order; then the
 * program, the data and the bindings (in code-point order of their names) are parsed in that order, the first
 * refusal winning, as the conformance vectors run them.
 */
import { cbor, DomainTag, digest, hashCommitment, json, models, Preview, taggedHash, toHex, Value, type Result } from "@kd5ujc/dilf4s-core";
import {
  Bindings,
  compile,
  DEFAULT_GAS_LIMIT,
  evaluate,
  LATEST_SCHEDULE,
  opNamed,
  parse,
  parseValue,
  siteOf,
  VERSION,
  type JlvmError,
  type Outcome,
  type Program,
} from "@kd5ujc/dilf4s-jlvm";

import { CATALOGUE, type Entry } from "./catalogue.js";
import { DEFAULT_GAS_CAP, MAX_BINDINGS, MAX_BINDINGS_BYTES, MAX_OUTPUT_BYTES, MAX_TEXT_BYTES } from "./limits.js";
import { measure } from "./measure.js";
import { renderSurface } from "./surface.js";

// ------------------------------------------------------------------ results

/** Why a call was refused. `InvalidArguments` and `UnknownTool` are the caller's misuse of the protocol; the others are ordinary outcomes. */
export type RefusalKind = "InvalidArguments" | "UnknownTool" | "InputTooLarge" | "GasLimitOutOfRange" | "Malformed" | "Limit" | "OutputTooLarge";

/** Where a refusal arose: the argument, a binding's name (previewed), and for a JSON error the offset in its text. */
export interface Where {
  readonly argument: string;
  readonly binding?: string;
  readonly offset?: number;
}

export interface Refusal {
  readonly kind: RefusalKind;
  readonly message: string;
  readonly where: Where;
}

export interface Refused {
  readonly ok: false;
  readonly refusal: Refusal;
}

/** A tool's result: its structured content, and whether it is the caller's misuse (MCP's `isError`). */
export interface ToolResult {
  readonly structured: Readonly<Record<string, unknown>>;
  readonly isError: boolean;
}

const refused = (kind: RefusalKind, message: string, where: Where): Refused => ({ ok: false, refusal: { kind, message, where } });

const MISUSE: ReadonlySet<RefusalKind> = new Set<RefusalKind>(["InvalidArguments", "UnknownTool"]);

const result = (r: Refused | (Readonly<Record<string, unknown>> & { readonly ok: true })): ToolResult =>
  r.ok ? { structured: r, isError: false } : { structured: { ok: false, refusal: r.refusal }, isError: MISUSE.has(r.refusal.kind) };

/** A caller's text, as a refusal may name it: at most a preview, so no refusal echoes unbounded input. */
const named = (s: string): string => {
  const p = Preview.of(s);
  return p.head.length < s.length ? `${p.head}…` : p.head;
};

/** An informative message, bounded: the JLVM's own are, and this keeps every one so. */
const MAX_MESSAGE = 1024;
const bounded = (m: string): string => (m.length > MAX_MESSAGE ? `${m.slice(0, MAX_MESSAGE)}…` : m);

const hex = (b: Uint8Array): string => {
  const h = toHex(b);
  return h.ok ? h.value : "";
};

// ------------------------------------------------------------------ arguments

type Args = Readonly<Record<string, unknown>>;

/** The arguments as an object holding only `allowed` fields: missing is none, anything else is refused. */
function argumentsOf(given: unknown, allowed: readonly string[]): Result<Args, Refused> {
  if (given === undefined) return { ok: true, value: {} };
  if (typeof given !== "object" || given === null || Array.isArray(given)) return { ok: false, error: refused("InvalidArguments", "the arguments are not an object", { argument: "arguments" }) };
  const unknown = Object.keys(given).find((k) => !allowed.includes(k));
  if (unknown !== undefined) return { ok: false, error: refused("InvalidArguments", `no argument is named ${named(unknown)}; the arguments are ${allowed.join(", ")}`, { argument: "arguments" }) };
  return { ok: true, value: given as Args };
}

/** An own field's value: never one an object inherits (`__proto__`, `constructor`). */
const field = (args: Args, name: string): unknown => (Object.hasOwn(args, name) ? Object.getOwnPropertyDescriptor(args, name)?.value : undefined);

/** A text argument: required unless `fallback` is given (a null given is wrong-typed, never the fallback). */
function text(args: Args, name: string, fallback?: string): Result<string, Refused> {
  const given = field(args, name);
  const v = given === undefined ? fallback : given;
  if (v === undefined) return { ok: false, error: refused("InvalidArguments", `${name} is required: JSON surface text`, { argument: name }) };
  if (typeof v !== "string") return { ok: false, error: refused("InvalidArguments", `${name} must be a string of JSON surface text`, { argument: name }) };
  return { ok: true, value: v };
}

function sized(name: string, t: string, binding?: string): Refused | undefined {
  const m = measure(t, MAX_TEXT_BYTES);
  if (m <= MAX_TEXT_BYTES) return undefined;
  return refused("InputTooLarge", `${binding === undefined ? name : `the binding ${named(binding)}`} measures more than ${MAX_TEXT_BYTES.toString()} bytes (its UTF-8 length plus its numbers' exponents)`, binding === undefined ? { argument: name } : { argument: name, binding: named(binding) });
}

/** The host bindings: an object of names to surface texts, in code-point order of the names, bounded together. */
function bindingsOf(args: Args): Result<readonly (readonly [string, string])[], Refused> {
  const v = field(args, "bindings");
  if (v === undefined) return { ok: true, value: [] };
  if (typeof v !== "object" || v === null || Array.isArray(v)) return { ok: false, error: refused("InvalidArguments", "bindings must be an object of names to JSON surface texts", { argument: "bindings" }) };
  const names = Object.keys(v);
  if (names.length > MAX_BINDINGS) return { ok: false, error: refused("InputTooLarge", `at most ${MAX_BINDINGS.toString()} bindings`, { argument: "bindings" }) };
  const entries: (readonly [string, string])[] = [];
  for (const k of names) {
    const t: unknown = Object.getOwnPropertyDescriptor(v, k)?.value;
    if (typeof t !== "string") return { ok: false, error: refused("InvalidArguments", "each binding's value must be a string of JSON surface text", { argument: "bindings", binding: named(k) }) };
    entries.push([k, t]);
  }
  return { ok: true, value: entries.sort(([a], [b]) => byCodePoint(a, b)) };
}

/** Code-point order, which is UTF-8 byte order: what the statement orders the bindings by. */
function byCodePoint(a: string, b: string): number {
  for (let i = 0; i < a.length && i < b.length; ) {
    const [x, y] = [a.codePointAt(i) ?? 0, b.codePointAt(i) ?? 0];
    if (x !== y) return x - y;
    i += x > 0xffff ? 2 : 1;
  }
  return a.length - b.length;
}

/** The bindings' measure, names included: each name's UTF-8 length and each text's measure, until past the cap. */
function bindingsSized(entries: readonly (readonly [string, string])[]): Refused | undefined {
  let total = 0;
  for (const [k, t] of entries) {
    const one = sized("bindings", t, k);
    if (one !== undefined) return one;
    total += measure(k, MAX_BINDINGS_BYTES) + measure(t, MAX_BINDINGS_BYTES);
    if (total > MAX_BINDINGS_BYTES) return refused("InputTooLarge", `the bindings measure more than ${MAX_BINDINGS_BYTES.toString()} bytes together`, { argument: "bindings" });
  }
  return undefined;
}

/** The gas limit: a whole number from 0 to the server's cap, the default limit (or the cap, if lower) unless given. */
function gasLimitOf(args: Args, cap: number): Result<number, Refused> {
  const v = field(args, "gasLimit");
  if (v === undefined) return { ok: true, value: Math.min(DEFAULT_GAS_LIMIT, cap) };
  if (typeof v !== "number" || !Number.isSafeInteger(v) || v < 0) return { ok: false, error: refused("InvalidArguments", "gasLimit must be a whole number from 0", { argument: "gasLimit" }) };
  if (v > cap) return { ok: false, error: refused("GasLimitOutOfRange", `gasLimit is above this server's cap, ${cap.toString()}`, { argument: "gasLimit" }) };
  return { ok: true, value: v };
}

// ------------------------------------------------------------------ parsing

/** A JLVM parse refusal, with the offset of the JSON error when the text is not canonical JSON. */
function parseRefusal(e: JlvmError, t: string, where: Where): Refused {
  const kind: RefusalKind = e.kind === "Limit" ? "Limit" : "Malformed";
  const message = e.kind === "OutOfGas" ? "out of gas" : bounded(e.message);
  // the failure path only, within the text cap: core's parser says where the JSON broke
  const j = json.parse(t);
  const offset = !j.ok && "offset" in j.error && typeof j.error.offset === "number" ? j.error.offset : undefined;
  return refused(kind, message, offset === undefined ? where : { ...where, offset });
}

function programOf(t: string): Result<Program, Refused> {
  const r = parse(t);
  if (r.ok) return r;
  return { ok: false, error: r.error.kind === "TypeMismatch" ? refused("InvalidArguments", "program must be a string", { argument: "program" }) : parseRefusal(r.error, t, { argument: "program" }) };
}

function valueOf(t: string, where: Where): Result<Value, Refused> {
  const r = parseValue(t);
  if (r.ok) return r;
  return { ok: false, error: r.error.kind === "TypeMismatch" ? refused("InvalidArguments", "a value must be a string", where) : parseRefusal(r.error, t, where) };
}

// ------------------------------------------------------------------ outputs

/** A value's renderings, each within {@link MAX_OUTPUT_BYTES}; one longer, or a JSON form the value lacks, is omitted and said to be. */
function rendered(v: Value): Readonly<Record<string, unknown>> {
  const c = cbor.encodeWithin(v, MAX_OUTPUT_BYTES);
  const j = renderSurface(v, MAX_OUTPUT_BYTES);
  const omitted = { ...(c.ok ? {} : { cbor: "TooLarge" }), ...(j.ok ? {} : { json: j.error }) };
  return {
    weight: Number(v.weight),
    ...(c.ok ? { cbor: hex(c.value) } : {}),
    ...(j.ok ? { json: j.value } : {}),
    ...(Object.keys(omitted).length === 0 ? {} : { omitted }),
  };
}

/** A family's name as the vectors key a cause: lower camel case (`PoseidonMerkle` is `poseidonMerkle`). */
const family = (kind: string): string => kind.charAt(0).toLowerCase() + kind.slice(1);

/** An error's normative record (`jlvm-vectors.cddl` `jlvm-error`): its kind, `op` and cause. */
function record(e: JlvmError): models.jlvmVectors.JlvmError {
  const op = siteOf(e);
  const cause = e.kind === "Crypto" && e.cause !== undefined ? { [family(e.cause.kind)]: e.cause.error.kind } : undefined;
  return { error: e.kind, ...(op === undefined || op === "<not a JLVM error>" ? {} : { op }), ...(cause === undefined ? {} : { cause }) } as models.jlvmVectors.JlvmError;
}

/** An error as the result shows it: its normative record, then its informative fields. */
function shownError(e: JlvmError): Readonly<Record<string, unknown>> {
  return { ...record(e), ...(e.kind === "OutOfGas" ? { required: e.required, available: e.available } : { message: bounded(e.message) }) };
}

// ------------------------------------------------------------------ the statement (docs/specs/zk-jlvm.md section 2)

const SHA = "SHA-256";
const TAGS = { input: "dilf4s/zk/jlvm/input/v1", bindings: "dilf4s/zk/jlvm/bindings/v1", binding: "dilf4s/zk/jlvm/binding/v1", result: "dilf4s/zk/jlvm/result/v1" } as const;

/** `taggedHash_SHA-256(tag, prefix || commit(v))`, the commitment streamed. */
function taggedCommitment(tag: string, v: Value, prefix: readonly Uint8Array[] = []): Uint8Array | undefined {
  const t = digest(SHA, new TextEncoder().encode(tag));
  if (!t.ok) return undefined;
  const h = hashCommitment(SHA, [t.value, t.value, ...prefix], v);
  return h.ok ? h.value : undefined;
}

function bindingsDigest(entries: readonly (readonly [string, Value])[]): Uint8Array | undefined {
  const each: Uint8Array[] = [];
  for (const [k, v] of entries) {
    const name = Value.text(k);
    const encoded = name.ok ? cbor.encode(name.value) : undefined;
    const e = encoded?.ok === true ? taggedCommitment(TAGS.binding, v, [encoded.value]) : undefined;
    if (e === undefined) return undefined;
    each.push(e);
  }
  const all = new Uint8Array(each.length * 32);
  each.forEach((e, i) => {
    all.set(e, i * 32);
  });
  const tag = DomainTag.of(TAGS.bindings);
  const h = tag.ok ? taggedHash(SHA, tag.value, all) : undefined;
  return h?.ok === true ? h.value : undefined;
}

/** The statement of an evaluation, in its canonical form and as hex digests, or `undefined` if a digest could not be taken. */
function statementOf(program: Program, data: Value, env: readonly (readonly [string, Value])[], gasLimit: number, o: Outcome): Readonly<Record<string, unknown>> | undefined {
  const id = program.id();
  const input = taggedCommitment(TAGS.input, data);
  const binds = bindingsDigest(env);
  const value = o.result.ok ? taggedCommitment(TAGS.result, o.result.value) : undefined;
  if (!id.ok || input === undefined || binds === undefined || (o.result.ok && value === undefined)) return undefined;
  const outcome: models.zkJlvm.ZkJlvmOutcome = o.result.ok ? { result: value ?? new Uint8Array(32) } : record(o.result.error);
  const s: models.zkJlvm.ZkJlvmStatement = {
    version: 1n,
    language: BigInt(VERSION),
    schedule: BigInt(LATEST_SCHEDULE.version),
    program: id.value,
    input,
    bindings: binds,
    gasLimit: BigInt(gasLimit),
    gasUsed: BigInt(o.gasUsed),
    outcome,
  };
  const encoded = models.zkJlvm.ZkJlvmStatement.encode(s);
  const bytes = encoded.ok ? cbor.encode(encoded.value) : undefined;
  if (bytes?.ok !== true) return undefined;
  return {
    statement: {
      version: 1,
      language: VERSION,
      schedule: LATEST_SCHEDULE.version,
      program: hex(id.value),
      input: hex(input),
      bindings: hex(binds),
      gasLimit,
      gasUsed: o.gasUsed,
      outcome: "result" in outcome ? { result: hex(outcome.result) } : outcome,
    },
    statementCbor: hex(bytes.value),
  };
}

// ------------------------------------------------------------------ the tools

/** `jlvm_check`: parse and compile a program; its canonical form, identity, size and depth, or the refusal. */
export function check(given: unknown): ToolResult {
  const args = argumentsOf(given, ["program"]);
  if (!args.ok) return result(args.error);
  const t = text(args.value, "program");
  if (!t.ok) return result(t.error);
  const big = sized("program", t.value);
  if (big !== undefined) return result(big);
  const program = programOf(t.value);
  if (!program.ok) return result(program.error);
  const compiled = compile(program.value);
  if (!compiled.ok) return result(compiled.error.kind === "TypeMismatch" ? refused("InvalidArguments", "not a program", { argument: "program" }) : parseRefusal(compiled.error, t.value, { argument: "program" }));
  const id = program.value.id();
  const canonical = program.value.canonical();
  const encoded = cbor.encodeWithin(canonical, MAX_OUTPUT_BYTES);
  if (!id.ok) return result(refused("Limit", "the program has no identity: its encoding is over the ceiling", { argument: "program" }));
  return result({
    ok: true,
    id: hex(id.value),
    ...(encoded.ok ? { canonical: hex(encoded.value) } : { omitted: { canonical: "TooLarge" } }),
    weight: Number(canonical.weight),
    depth: canonical.depth,
  });
}

/** `jlvm_evaluate`: run a program on data with host bindings and a gas limit; the outcome as the vectors record it, and its statement. */
export function evaluateCall(given: unknown, cap: number = DEFAULT_GAS_CAP): ToolResult {
  const args = argumentsOf(given, ["program", "data", "bindings", "gasLimit"]);
  if (!args.ok) return result(args.error);
  const pt = text(args.value, "program");
  if (!pt.ok) return result(pt.error);
  const dt = text(args.value, "data", "null");
  if (!dt.ok) return result(dt.error);
  const bt = bindingsOf(args.value);
  if (!bt.ok) return result(bt.error);
  const limit = gasLimitOf(args.value, cap);
  if (!limit.ok) return result(limit.error);
  const big = sized("program", pt.value) ?? sized("data", dt.value) ?? bindingsSized(bt.value);
  if (big !== undefined) return result(big);
  const program = programOf(pt.value);
  if (!program.ok) return result(program.error);
  const data = valueOf(dt.value, { argument: "data" });
  if (!data.ok) return result(data.error);
  const env: (readonly [string, Value])[] = [];
  for (const [k, t] of bt.value) {
    const v = valueOf(t, { argument: "bindings", binding: named(k) });
    if (!v.ok) return result(v.error);
    env.push([k, v.value]);
  }
  const bindings = Bindings.of(env);
  if (!bindings.ok) return result(refused("InvalidArguments", "each binding is named once", { argument: "bindings" }));
  const ran = evaluate(program.value, data.value, { env: bindings.value, gasLimit: limit.value });
  if (!ran.ok) return result(refused("InvalidArguments", bounded(ran.error.reason), { argument: "arguments" }));
  const o = ran.value;
  const statement = statementOf(program.value, data.value, env, limit.value, o);
  if (statement === undefined) return result(refused("OutputTooLarge", "a digest of the call could not be taken", { argument: "arguments" }));
  return result({
    ok: true,
    outcome: o.result.ok ? { value: rendered(o.result.value) } : { error: shownError(o.result.error) },
    gasUsed: o.gasUsed,
    gasLimit: limit.value,
    warnings: o.warnings.map((w) => ({ kind: w.kind, path: w.path.head, pathBytes: w.path.length })),
    ...statement,
  });
}

/** `jlvm_ops`: the closed list of operations, or the one named, with arities, argument kinds, examples and gas costs. */
export function ops(given: unknown, cap: number = DEFAULT_GAS_CAP): ToolResult {
  const args = argumentsOf(given, ["op"]);
  if (!args.ok) return result(args.error);
  const name = field(args.value, "op");
  if (name !== undefined && typeof name !== "string") return result(refused("InvalidArguments", "op must be a string: an operation's name or alias", { argument: "op" }));
  const wanted = name === undefined ? undefined : opNamed(name);
  if (name !== undefined && wanted === undefined) return result(refused("InvalidArguments", `${named(name)} is no operation of language version ${VERSION.toString()}`, { argument: "op" }));
  const s = LATEST_SCHEDULE;
  const list: readonly Entry[] = wanted === undefined ? CATALOGUE : CATALOGUE.filter((e) => e.op === wanted);
  return result({
    ok: true,
    language: VERSION,
    schedule: s.version,
    gas: { literal: s.literal, element: s.element, varAccess: s.varAccess, perPathSegment: s.perPathSegment, callbackRun: s.callbackRun, rendering: s.rendering, defaultLimit: Math.min(DEFAULT_GAS_LIMIT, cap), cap },
    ops: list,
  });
}

/** The tools by name. */
export const TOOL_NAMES = ["jlvm_check", "jlvm_evaluate", "jlvm_ops"] as const;
export type ToolName = (typeof TOOL_NAMES)[number];

/** Calls the tool `name` with `given`, under the gas cap `cap`; an unknown tool is a refusal. */
export function callTool(name: string, given: unknown, cap: number = DEFAULT_GAS_CAP): ToolResult {
  switch (name) {
    case "jlvm_check":
      return check(given);
    case "jlvm_evaluate":
      return evaluateCall(given, cap);
    case "jlvm_ops":
      return ops(given, cap);
    default:
      return result(refused("UnknownTool", `no tool is named ${named(name)}; the tools are ${TOOL_NAMES.join(", ")}`, { argument: "name" }));
  }
}
