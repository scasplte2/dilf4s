/**
 * The dilf4s JLVM: a strict, deterministic, gas-metered expression language for ledgers, written in JsonLogic syntax
 * (language version 1, gas schedule version 1). The normative specification is `docs/specs/jlvm.md`; the conformance
 * vectors `shared/vectors/jlvm_v1.json` are part of it, and the Scala implementation (`lang/scala/jlvm/`) is the reference.
 *
 * A result is a function of the program, the input data, the host bindings, the language version and the gas
 * schedule; an evaluation reports the gas it used whether it succeeds or fails.
 * - {@link parse} reads a program's JSON surface into a {@link Program}, bounded in size and depth, with exactly one
 *   canonical form; {@link fromCanonical} and {@link decode} read that form back. {@link compile} checks its special
 *   forms.
 * - {@link evaluate} runs it on an explicit continuation stack, charging every unit of work before doing it.
 * - The cryptographic and proof operations (section 6) run on `@kd5ujc/dilf4s-signing`, `@kd5ujc/dilf4s-proofs` and
 *   core's hashing.
 *
 * Library functions never throw: every failure is a typed result, and an argument of the wrong type (from JavaScript,
 * or a cast) is a `TypeMismatch`, checked before it is read.
 */
import { isValue, type MapValue, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { isRevokedProxy, readGuarded } from "@kd5ujc/dilf4s-core/internal";

import { decodeProgram, fromCanonicalValue, isProgram, Program } from "./ast.js";
import { Bindings, isBindings } from "./bindings.js";
import { limit as limitError, siteOf } from "./error.js";
import { ALL_OPS, CRYPTO_OPS, FORM_OPS, isCrypto, isForm, isStrict, isVariadic, opFromWire, opNamed } from "./op.js";
import type { JlvmError } from "./error.js";
import { DEFAULT_GAS_LIMIT, LATEST_SCHEDULE, MAX_GAS_LIMIT, SCHEDULE_V1, type GasSchedule } from "./gas.js";
import { run, type Warning } from "./machine.js";
import { compile as compileNode, type Node } from "./node.js";
import { renderProgram } from "./render.js";
import { parseData, parseProgram } from "./surface.js";

export { Bindings, isBindings } from "./bindings.js";
export { MAX_CANONICAL_WEIGHT, MAX_PROGRAM_DEPTH, MAX_PROGRAM_SIZE, MAX_SURFACE_DEPTH, PROGRAM_TAG, Program, VERSION } from "./ast.js";
export type { JlvmError, JlvmErrorKind, JlvmSite } from "./error.js";
export { NOT_A_JLVM_ERROR, siteOf } from "./error.js";
export { DEFAULT_GAS_LIMIT, GCD_LIMBS, LATEST_SCHEDULE, MAX_GAS_LIMIT, RENDER_LIMBS, SCHEDULE_V1, type GasSchedule, type Merging, type OpCost, type Rate, type Search, type SizeUnit } from "./gas.js";
export { MAX_WARNINGS, type Warning } from "./machine.js";
export { ALL_OPS, CRYPTO_OPS, FORM_OPS, isCrypto, isForm, isStrict, isVariadic, opFromWire, opNamed, type CryptoOp, type FormOp, type JlvmOp, type StrictOp, type VariadicOp } from "./op.js";
export { MAX_EXPONENT, MAX_INT_BYTES } from "./ops.js";
export { MAX_NUMBER_BITS, MAX_RESULT_WEIGHT } from "./values.js";

const notA = (what: string): { readonly ok: false; readonly error: TypeMismatch } => ({ ok: false, error: { kind: "TypeMismatch", reason: `expected ${what}` } });

/** Parses a program from its JSON surface text (jlvm.md section 1). Text that is not a string is a `TypeMismatch`. */
export function parse(text: string): Result<Program, JlvmError | TypeMismatch> {
  return typeof (text as unknown) === "string" ? parseProgram(text) : notA("program text");
}

/**
 * Parses input data or a host binding from its JSON surface text: canonical JSON, with `0x`-hex strings as bytes. Text
 * that is not a string is a `TypeMismatch`.
 */
export function parseValue(text: string): Result<Value, JlvmError | TypeMismatch> {
  return typeof (text as unknown) === "string" ? parseData(text) : notA("value text");
}

/**
 * A program's surface text (jlvm.md section 1): exact, so {@link parse} reads it back as the same program. A literal
 * holding `0x`-hex text, or a non-terminating rational, has no surface form and is `Malformed`. Something that is not
 * a {@link Program} is a `TypeMismatch`.
 */
export function render(program: Program): Result<string, JlvmError | TypeMismatch> {
  return isProgram(program) ? renderProgram(program) : notA("a Program");
}

/** The program a canonical form (`shared/schemas/jlvm.cddl`) denotes; something that is not a canonical value is a `TypeMismatch`. */
export function fromCanonical(value: Value): Result<Program, JlvmError | TypeMismatch> {
  return isValue(value) ? fromCanonicalValue(value) : notA("a canonical value");
}

/** The program a canonical CBOR encoding denotes; bytes that are not a `Uint8Array` are a `TypeMismatch`. */
export function decode(bytes: Uint8Array): Result<Program, JlvmError | TypeMismatch> {
  return decodeProgram(bytes);
}

/** The key {@link Compiled}'s constructor takes, private to this module. */
const COMPILED: unique symbol = Symbol("compiled");

// set once, by Compiled's static block
let makeCompiled!: (p: Program, r: Node) => Compiled;
let rootOf!: (c: Compiled) => Node;
let isCompiledOf!: (x: object) => boolean;

/** A program checked for evaluation: its special forms are well-formed. Made only by {@link compile}. */
export class Compiled {
  readonly #program: Program;
  readonly #root: Node;
  /** Whether {@link compile} made it, with its key: JavaScript can call a private constructor. */
  readonly #made: boolean;
  private constructor(key: symbol, program: Program, root: Node) {
    this.#made = key === COMPILED;
    this.#program = program;
    this.#root = root;
    Object.freeze(this);
  }

  static {
    makeCompiled = (p, r) => new Compiled(COMPILED, p, r);
    rootOf = (c) => c.#root;
    isCompiledOf = (x) => #made in x && x.#made;
  }

  /** The program. */
  get program(): Program {
    return this.#program;
  }
}

Object.freeze(Compiled);
Object.freeze(Compiled.prototype);

const isCompiled = (x: unknown): x is Compiled => typeof x === "object" && x !== null && isCompiledOf(x);

/**
 * Checks a program's special forms (`if`, `let`, the callbacks), its `let` names, and its spreads: these are
 * `Malformed`, found before any gas is used. Something that is not a {@link Program} is a `TypeMismatch`.
 */
export function compile(program: Program): Result<Compiled, JlvmError | TypeMismatch> {
  if (!isProgram(program)) return notA("a Program");
  const root = compileNode(program);
  return root.ok ? { ok: true, value: makeCompiled(program, root.value) } : root;
}

/** How to evaluate. */
export interface EvaluateOptions {
  /**
   * The host bindings, read as `$name` (e.g. `$ordinal`); `let` and callback bindings shadow them. {@link Bindings} can
   * bind a name to null, which then reads as null where an unbound name is `Malformed`; a core `MapValue` binds the
   * names it holds, none of them to null. Either way names are compared, never hashed.
   */
  readonly env?: Bindings | MapValue;
  /** The gas limit: a whole number, {@link DEFAULT_GAS_LIMIT} unless given. One over {@link MAX_GAS_LIMIT} is `Limit`, with no gas used. */
  readonly gasLimit?: number;
  /** The gas schedule: one of the published schedules, {@link LATEST_SCHEDULE} unless given. */
  readonly schedule?: GasSchedule;
}

/** The result of an evaluation: the gas it used, whether it succeeded or failed; the value or the error; and the warnings. Out of gas, the whole limit is used. */
export interface Outcome {
  /** The gas used. */
  readonly gasUsed: number;
  /** The value, or the error. */
  readonly result: Result<Value, JlvmError>;
  /** Lint warnings, distinct, at most {@link MAX_WARNINGS}. */
  readonly warnings: readonly Warning[];
}

const SCHEDULES: readonly GasSchedule[] = [SCHEDULE_V1];

/**
 * Evaluates `program` against `data`. A program that does not compile uses no gas. An argument of the wrong type (a
 * `program` that is neither a {@link Program} nor a {@link Compiled}, `data` or `env` that is not a canonical value,
 * a gas limit that is not a non-negative whole number, an unpublished schedule) is a `TypeMismatch`, before anything is
 * evaluated. A gas limit over {@link MAX_GAS_LIMIT} is an outcome, `Limit` with no gas used, found before the program
 * is compiled (jlvm.md section 7).
 */
export function evaluate(program: Program | Compiled, data: Value, options: EvaluateOptions = {}): Result<Outcome, TypeMismatch> {
  if (!isProgram(program) && !isCompiled(program)) return notA("a Program or a Compiled program");
  if (!isValue(data)) return notA("the data as a canonical value");
  const given: unknown = options;
  // an object, read once per field, each read guarded: a revoked Proxy (or one a getter revoked meanwhile) is refused
  if (typeof given !== "object" || given === null || isRevokedProxy(given)) return notA("evaluation options");
  const bindings: unknown = readGuarded(given, "env");
  const limit: unknown = readGuarded(given, "gasLimit");
  const chosen: unknown = readGuarded(given, "schedule");
  const gasLimit = limit === undefined ? DEFAULT_GAS_LIMIT : limit;
  const schedule = chosen === undefined ? LATEST_SCHEDULE : chosen;
  if (bindings !== undefined && !isBindings(bindings) && (!isValue(bindings) || bindings.kind !== "map")) return notA("the host bindings as Bindings or a canonical map");
  if (typeof gasLimit !== "number" || !Number.isInteger(gasLimit)) return notA("a gas limit as a whole number");
  const published = SCHEDULES.find((s) => s === schedule);
  if (published === undefined) return notA("a published gas schedule");
  // a limit outside [0, 2^53 - 1] is Limit, as in every language, checked before the program is compiled: a negative
  // one (a host's arithmetic gone below zero), or one over the maximum, named by its bit length
  if (gasLimit < 0) return { ok: true, value: { gasUsed: 0, result: { ok: false, error: limitError("a gas limit is negative") }, warnings: [] } };
  if (gasLimit > MAX_GAS_LIMIT) return { ok: true, value: { gasUsed: 0, result: { ok: false, error: limitError(`a gas limit of ${BigInt(gasLimit).toString(2).length.toString()} bits is over 2^53 - 1`) }, warnings: [] } };
  let root: Node;
  if (isCompiled(program)) root = rootOf(program);
  else {
    const compiled = compileNode(program);
    if (!compiled.ok) return { ok: true, value: { gasUsed: 0, result: compiled, warnings: [] } };
    root = compiled.value;
  }
  const ran = run(published, root, data, bindings, gasLimit);
  return { ok: true, value: { gasUsed: ran.used, result: ran.result, warnings: ran.warnings } };
}

// every export frozen, and everything it holds: its functions, their prototype objects, a class's methods, and the
// lists, tables and schedule (which freeze themselves where they are defined), so no caller can change what another
// evaluation reads, or hang state on the package. An explicit stack; the exports form a small tree of the package's
// own objects, and a core value they hold is frozen already
// every export that is an object or a function, listed here (a test checks that none is missed)
const freezing: unknown[] = [parse, parseValue, render, fromCanonical, decode, compile, evaluate, Compiled, Bindings, isBindings, Program, siteOf, SCHEDULE_V1, ALL_OPS, CRYPTO_OPS, FORM_OPS, isCrypto, isForm, isStrict, isVariadic, opFromWire, opNamed];
const frozenAlready = new WeakSet();
for (let x = freezing.pop(); x !== undefined; x = freezing.pop()) {
  if (typeof x !== "object" && typeof x !== "function") continue;
  if (x === null || frozenAlready.has(x)) continue;
  frozenAlready.add(x);
  Object.freeze(x);
  for (const k of Reflect.ownKeys(x)) {
    const v: unknown = Object.getOwnPropertyDescriptor(x, k)?.value;
    if ((typeof v === "object" && v !== null) || typeof v === "function") freezing.push(v);
  }
}
