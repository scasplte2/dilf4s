/**
 * Programs (`Ast.scala`, `Parser.scala`'s canonical form): the syntax tree is the canonical program of
 * `shared/schemas/jlvm.cddl` itself, the generated {@link Expr}, so encoding and decoding it are core's iterative
 * codec. {@link Program} bounds a tree's size and depth before anything else reads it, and checks that it has exactly
 * one surface and canonical form (jlvm.md section 1).
 */
import { cbor, DomainTag, describeFailure, models, NullValue, Preview, taggedHash, type CanonicalError, type HashAlgorithm, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { isAlgorithm } from "@kd5ujc/dilf4s-core/internal";

import { limit, malformed, type JlvmError } from "./error.js";
import { amount } from "./gas.js";
import { callName, isVariadic, opFromWire, type JlvmOp } from "./op.js";
import { compareCodePoints } from "./jvalue.js";

/** A program's expression: the canonical syntax tree (generated). */
export type Expr = models.jlvm.JlvmExpr;

/** The language version. */
export const VERSION = 1;

/** Expression nesting: an operation's arguments, an array's items and a map's fields are one level deeper. */
export const MAX_PROGRAM_DEPTH = models.limits.JLVM_MAX_PROGRAM_DEPTH;

/** JSON nesting of the rendered program, as the canonical JSON parser bounds it. */
export const MAX_SURFACE_DEPTH = models.limits.CANONICAL_MAX_DEPTH;

/**
 * The largest program, in size: one per expression, plus the weight of each literal and default, plus the UTF-16
 * length of each object key, binding name and path segment, plus one per segment.
 */
export const MAX_PROGRAM_SIZE = models.limits.JLVM_MAX_PROGRAM_SIZE;

/** The heaviest canonical form a program can have: 64 per unit of the largest program's size, over the 40 any needs. */
export const MAX_CANONICAL_WEIGHT = BigInt(models.limits.JLVM_MAX_CANONICAL_WEIGHT);

/** The tag of a program's identity. */
export const PROGRAM_TAG = "dilf4s/jlvm/program/v1";

/** A `var` path: `$name.path` reads a binding (host, `let` or callback); any other path reads the data. */
export type VarRef = { readonly bound: false; readonly path: readonly string[] } | { readonly bound: true; readonly name: string; readonly path: readonly string[] };

/** The `.`-separated segments of `s`, empty ones included. */
export const segments = (s: string): readonly string[] => s.split(".");

/** A `var` path from its text: `""` is the whole data; no segment is empty, and a `$` name is not. */
export function parseVar(s: string): Result<VarRef, JlvmError> {
  if (s.length === 0) return { ok: true, value: { bound: false, path: [] } };
  const segs = segments(s);
  if (segs.some((g) => g.length === 0)) return { ok: false, error: malformed(`empty segment in var path ${Preview.of(s).toString()}`) };
  const [head = "", ...rest] = segs;
  if (!head.startsWith("$")) return { ok: true, value: { bound: false, path: segs } };
  const name = head.slice(1);
  return name.length === 0 ? { ok: false, error: malformed(`empty binding name in ${Preview.of(s).toString()}`) } : { ok: true, value: { bound: true, name, path: rest } };
}

/** What an expression is, read off its fields. */
export type View =
  | { readonly t: "lit"; readonly value: Value | undefined }
  | { readonly t: "var"; readonly path: string; readonly default: Value | undefined }
  | { readonly t: "varAt"; readonly path: Expr; readonly default: Value | undefined }
  | { readonly t: "apply"; readonly op: JlvmOp; readonly args: readonly Expr[] }
  | { readonly t: "spread"; readonly op: JlvmOp; readonly arg: Expr }
  | { readonly t: "arr"; readonly items: readonly Expr[] }
  | { readonly t: "map"; readonly fields: ReadonlyMap<string, Expr> };

/** The alternative of `e`: a null literal (`{}`) is a literal of no value. */
export function view(e: Expr): View {
  if ("op" in e) return { t: "apply", op: e.op, args: e.args };
  if ("spread" in e) return { t: "spread", op: e.spread, arg: e.arg };
  if ("arr" in e) return { t: "arr", items: e.arr };
  if ("map" in e) return { t: "map", fields: e.map };
  if ("var" in e) return { t: "var", path: e.var, default: e.default };
  if ("path" in e) return { t: "varAt", path: e.path, default: e.default };
  if ("lit" in e) return { t: "lit", value: e.lit };
  return { t: "lit", value: undefined };
}

/** The null literal. */
export const NULL_EXPR: Expr = Object.freeze({});

/**
 * A map expression's fields, in code-point order of their distinct keys: a `ReadonlyMap` the codec encodes, found by
 * binary search. Keys are the program's own, so they are compared, never hashed.
 */
export class Fields implements ReadonlyMap<string, Expr> {
  readonly #entries: readonly (readonly [string, Expr])[];
  private constructor(entries: readonly (readonly [string, Expr])[]) {
    this.#entries = entries;
    Object.freeze(this);
  }
  /** The fields of `entries`, whose keys are distinct: sorted by code point. */
  static of(entries: readonly (readonly [string, Expr])[]): Fields {
    return new Fields([...entries].sort((a, b) => compareCodePoints(a[0], b[0])));
  }
  get size(): number {
    return this.#entries.length;
  }
  get(key: string): Expr | undefined {
    let [lo, hi] = [0, this.#entries.length];
    for (; lo < hi; ) {
      const mid = (lo + hi) >>> 1;
      const c = compareCodePoints(this.#entries[mid]?.[0] ?? "", key);
      if (c === 0) return this.#entries[mid]?.[1];
      if (c < 0) lo = mid + 1;
      else hi = mid;
    }
    return undefined;
  }
  has(key: string): boolean {
    return this.get(key) !== undefined;
  }
  forEach(f: (value: Expr, key: string, map: ReadonlyMap<string, Expr>) => void, thisArg?: unknown): void {
    for (const [k, v] of this.#entries) f.call(thisArg, v, k, this);
  }
  entries(): MapIterator<[string, Expr]> {
    return this.#entries.map(([k, v]): [string, Expr] => [k, v]).values();
  }
  keys(): MapIterator<string> {
    return this.#entries.map(([k]) => k).values();
  }
  values(): MapIterator<Expr> {
    return this.#entries.map(([, v]) => v).values();
  }
  [Symbol.iterator](): MapIterator<[string, Expr]> {
    return this.entries();
  }
}

/** A map expression's fields in code-point order of their keys, however the map orders them. */
export function sortedFields(fields: ReadonlyMap<string, Expr>): readonly (readonly [string, Expr])[] {
  return [...fields.entries()].sort((a, b) => compareCodePoints(a[0], b[0]));
}

/** The sub-expressions of `e`, fields included, in evaluation order. */
export function children(e: Expr): readonly Expr[] {
  const v = view(e);
  switch (v.t) {
    case "apply":
      return v.args;
    case "arr":
      return v.items;
    case "spread":
      return [v.arg];
    case "varAt":
      return [v.path];
    case "map":
      return sortedFields(v.fields).map(([, x]) => x);
    case "lit":
    case "var":
      return [];
  }
}

const defaultWeight = (d: Value | undefined): number => (d === undefined ? 0 : amount(d.weight));

const pathSize = (p: readonly string[]): number => p.reduce((n, s) => n + s.length + 1, 0);

/** One node's contribution to the size (`Program.size`); a malformed path counts as its text. */
function size(e: Expr): number {
  const v = view(e);
  switch (v.t) {
    case "lit":
      return (v.value === undefined ? 1 : amount(v.value.weight)) + 1;
    case "var": {
      const r = parseVar(v.path);
      if (!r.ok) return v.path.length + 2 + defaultWeight(v.default);
      return r.value.bound ? r.value.name.length + 2 + pathSize(r.value.path) + defaultWeight(v.default) : pathSize(r.value.path) + 1 + defaultWeight(v.default);
    }
    case "varAt":
      return defaultWeight(v.default) + 1;
    case "map": {
      let n = 1;
      for (const k of v.fields.keys()) n += 1 + k.length;
      return n;
    }
    case "arr":
    case "apply":
    case "spread":
      return 1;
  }
}

/**
 * The size and depth bounds, before anything else reads the tree: each node counts as it is reached, and the walk
 * stops as soon as a bound is exceeded. An explicit stack.
 */
function measure(root: Expr): JlvmError | undefined {
  const todo: (readonly [Expr, number])[] = [[root, 0]];
  let seen = 0;
  for (let top = todo.pop(); top !== undefined; top = todo.pop()) {
    const [e, d] = top;
    if (d > MAX_PROGRAM_DEPTH) return limit(`the program nests deeper than ${MAX_PROGRAM_DEPTH.toString()}`);
    seen = Math.min(seen + size(e), Number.MAX_SAFE_INTEGER);
    if (seen > MAX_PROGRAM_SIZE) return limit(`the program is larger than ${MAX_PROGRAM_SIZE.toString()}`);
    // a map's fields are read without sorting them: their order does not change the size or the depth
    const v = view(e);
    if (v.t === "map") for (const x of v.fields.values()) todo.push([x, d + 1]);
    else for (const x of children(e)) todo.push([x, d + 1]);
  }
  return undefined;
}

/**
 * The first `var` path that does not parse, as `Parser.fromWire` finds it: before the size and depth bounds. The tree
 * is bounded already, by the text it was parsed from or by the weight of the form it was decoded from. An explicit
 * stack.
 */
function paths(root: Expr): JlvmError | undefined {
  const todo: Expr[] = [root];
  for (let e = todo.pop(); e !== undefined; e = todo.pop()) {
    const v = view(e);
    if (v.t === "var") {
      const r = parseVar(v.path);
      if (!r.ok) return r.error;
    }
    if (v.t === "map") for (const x of v.fields.values()) todo.push(x);
    else for (const x of children(e)) todo.push(x);
  }
  return undefined;
}

/** The shapes no surface text can produce. Runs once {@link measure} has bounded the tree. */
function check(root: Expr): JlvmError | undefined {
  const todo: Expr[] = [root];
  for (let e = todo.pop(); e !== undefined; e = todo.pop()) {
    const v = view(e);
    // an operation this package knows, by its own list: whatever the decoder was given to accept, nothing else runs
    if ((v.t === "apply" || v.t === "spread") && opFromWire(v.op) === undefined) return malformed("not an operation of this language version");
    if (v.t === "spread" && !isVariadic(v.op)) return malformed(`${v.op} does not spread its argument`);
    if (v.t === "spread" && view(v.arg).t === "arr") return malformed(`a spread ${v.op} takes one expression, not an argument list`);
    if ((v.t === "var" || v.t === "varAt") && v.default?.kind === "null") return malformed("a var default is never null: the canonical form cannot tell it from no default");
    if (v.t === "lit" && v.value?.kind === "null") return malformed("the null literal is written as {}, never as a literal of null");
    const kids = children(e);
    for (let i = kids.length - 1; i >= 0; i--) {
      const k = kids[i];
      if (k !== undefined) todo.push(k);
    }
  }
  return undefined;
}

// ---- the depth of the rendered surface (Parser.surfaceDepth)

/** A literal value's nesting, counting only non-empty arrays and maps (as the canonical JSON parser counts a rendered value). */
function valueDepth(root: Value): number {
  let deepest = 0;
  const todo: (readonly [Value, number])[] = [[root, 0]];
  for (let top = todo.pop(); top !== undefined; top = todo.pop()) {
    const [x, above] = top;
    if (x.kind === "array" && x.length > 0) {
      deepest = Math.max(deepest, above + 1);
      for (const c of x.items) todo.push([c, above + 1]);
    } else if (x.kind === "map" && x.size > 0) {
      deepest = Math.max(deepest, above + 1);
      for (const [, c] of x.entries()) todo.push([c, above + 1]);
    }
  }
  return deepest;
}

/** A literal or default as the renderer writes it: quoted, `{"literal": v}`, when it is an array or a map. */
const literalDepth = (v: Value | undefined): number => (v !== undefined && (v.kind === "array" || v.kind === "map") ? valueDepth(v) + 1 : 0);

/** How deep the rendered surface nests a program's JSON (`Parser.surfaceStep`): a worklist of (expression, nesting of the JSON container holding it). */
function surfaceDepth(root: Expr): number {
  let deepest = 0;
  const todo: (readonly [Expr, number])[] = [[root, 0]];
  for (let top = todo.pop(); top !== undefined; top = todo.pop()) {
    const [e, l] = top;
    const v = view(e);
    let own: number;
    switch (v.t) {
      case "lit":
        own = l + literalDepth(v.value);
        break;
      case "var":
        own = v.default === undefined ? l + 1 : l + 2 + literalDepth(v.default);
        break;
      case "varAt": {
        const p = view(v.path);
        if (p.t === "lit" && v.default === undefined) own = l + 2 + (p.value === undefined ? 0 : valueDepth(p.value));
        else if (p.t === "lit") own = Math.max(l + 3 + (p.value === undefined ? 0 : valueDepth(p.value)), l + 2 + literalDepth(v.default));
        else if (p.t === "arr" && v.default === undefined) {
          todo.push([v.path, l + 2]);
          own = l + 2;
        } else if (v.default === undefined) {
          todo.push([v.path, l + 1]);
          own = l + 1;
        } else {
          todo.push([v.path, l + 2]);
          own = l + 2 + literalDepth(v.default);
        }
        break;
      }
      case "apply": {
        const [bindings, body] = v.args;
        const pairs = bindings === undefined ? undefined : view(bindings);
        if (v.op === "let" && v.args.length === 2 && pairs?.t === "arr" && body !== undefined) {
          todo.push([body, l + 2]);
          own = l + 2;
          for (const pair of pairs.items) {
            const nx = view(pair);
            const [name, x] = nx.t === "arr" ? nx.items : [];
            if (nx.t === "arr" && nx.items.length === 2 && name !== undefined && x !== undefined && isTextLiteral(name)) {
              todo.push([x, l + 4]);
              own = Math.max(own, l + 4);
            } else {
              todo.push([pair, l + 3]);
              own = Math.max(own, l + 3);
            }
          }
        } else {
          for (const a of v.args) todo.push([a, l + 2]);
          own = v.args.length === 0 ? l + 1 : l + 2;
        }
        break;
      }
      case "spread":
        todo.push([v.arg, l + 1]);
        own = l + 1;
        break;
      case "arr": {
        const [first, ...rest] = v.items;
        if (first === undefined) own = l;
        else if (isCallText(first)) {
          for (const x of rest) todo.push([x, l + 1]);
          own = l + 2;
        } else {
          for (const x of v.items) todo.push([x, l + 1]);
          own = l + 1;
        }
        break;
      }
      case "map": {
        const fs = [...v.fields.entries()];
        const [only] = fs;
        if (fs.length === 0) own = l;
        else if (fs.length === 1 && only !== undefined && !only[0].startsWith("_")) {
          todo.push([only[1], l + 2]);
          own = l + 2;
        } else {
          for (const [, x] of fs) todo.push([x, l + 1]);
          own = l + 1;
        }
        break;
      }
    }
    deepest = Math.max(deepest, own);
  }
  return deepest;
}

function isTextLiteral(e: Expr): boolean {
  const v = view(e);
  return v.t === "lit" && v.value?.kind === "text";
}

function isCallText(e: Expr): boolean {
  const v = view(e);
  return v.t === "lit" && v.value?.kind === "text" && callName(v.value.value);
}

// ---- programs

/** A depth bound is `Limit`, however it is reached; anything else malformed is `Malformed`. */
export function canonicalError(e: CanonicalError): JlvmError {
  return e.kind === "DepthExceeded" ? limit(`the program nests deeper than its canonical form allows (${e.limit.toString()} levels)`) : malformed(`not a canonical program: ${e.kind}`);
}

/** The key the constructor takes, private to this module: without it, what it builds is not a program. */
const KEY: unique symbol = Symbol("program");

// set once, by Program's static block: the only ways to read or make one from outside the class
let exprOf!: (p: Program) => Expr;
let isProgramOf!: (x: object) => boolean;
let make!: (expr: Expr, canonical: Value) => Program;

/**
 * A program: language version 1 and its expression, bounded in size and depth, with exactly one surface form and one
 * canonical form. Made only by this package (`parse`, `fromCanonical`, `decode`), each through {@link programOf}; its
 * tree is never handed out, so nothing changes it once it is checked.
 */
export class Program {
  readonly #expr: Expr;
  readonly #canonical: Value;
  /** Whether this package made it, with {@link KEY}: a constructor is private to TypeScript alone, and JavaScript can call it. */
  readonly #made: boolean;
  private constructor(key: symbol, expr: Expr, canonical: Value) {
    this.#made = key === KEY;
    this.#expr = this.#made ? expr : NULL_EXPR;
    this.#canonical = this.#made ? canonical : NullValue.instance;
    Object.freeze(this);
  }

  static {
    exprOf = (p) => p.#expr;
    isProgramOf = (x) => #made in x && x.#made;
    make = (expr, canonical) => new Program(KEY, expr, canonical);
  }

  /**
   * The canonical form (`shared/schemas/jlvm.cddl`), built and checked when the program was made. One built by calling
   * the constructor from JavaScript is no program: its form is null.
   */
  canonical(): Value {
    return this.#canonical;
  }

  /** The canonical CBOR encoding; one past `cbor.MAX_ENCODED_BYTES` is a `Limit`. */
  encode(): Result<Uint8Array, JlvmError | TypeMismatch> {
    if (!this.#made) return { ok: false, error: { kind: "TypeMismatch", reason: "expected a Program this package made" } };
    const e = cbor.encode(this.#canonical);
    return e.ok ? e : { ok: false, error: limit("the program's encoding is over the encoding ceiling") };
  }

  /**
   * The program's identity: `taggedHash_H("dilf4s/jlvm/program/v1", cbor(program))` with `hash`, SHA-256 unless given.
   * A `hash` that is not one of core's algorithms is a `TypeMismatch`.
   */
  id(hash: HashAlgorithm = "SHA-256"): Result<Uint8Array, JlvmError | TypeMismatch> {
    if (!isAlgorithm(hash)) return { ok: false, error: { kind: "TypeMismatch", reason: "expected a hash algorithm" } };
    const tag = DomainTag.of(PROGRAM_TAG);
    if (!tag.ok) return { ok: false, error: malformed("the program tag is not a domain tag") };
    const e = this.encode();
    if (!e.ok) return e;
    // the algorithm is checked above, so core's refusal of one is not reached; it would be the TypeMismatch above
    const h = taggedHash(hash, tag.value, e.value);
    return h.ok ? h : { ok: false, error: { kind: "TypeMismatch", reason: "expected a hash algorithm" } };
  }
}

/**
 * The program of `expr`: the size and depth bounds, then the shapes no surface text can produce, then the rendered
 * surface's depth and the canonical form (`Program.from`).
 */
export function programOf(expr: Expr): Result<Program, JlvmError> {
  // the normative order (jlvm.md section 1): `var` paths that do not parse, then the size and depth bounds, then the
  // shapes no surface text writes
  const bounded = paths(expr) ?? measure(expr) ?? check(expr);
  if (bounded !== undefined) return { ok: false, error: bounded };
  const surface = surfaceDepth(expr);
  if (surface > MAX_SURFACE_DEPTH) return { ok: false, error: limit(`the program's JSON surface nests ${surface.toString()} deep, deeper than ${MAX_SURFACE_DEPTH.toString()}`) };
  const c = models.jlvm.JlvmProgram.encode({ jlvm: 1n, expr });
  return c.ok ? { ok: true, value: make(expr, c.value) } : { ok: false, error: canonicalError(c.error) };
}

/** The program a canonical form denotes (`Parser.fromCanonical`): it must decode, then pass {@link programOf}. */
export function fromCanonicalValue(value: Value): Result<Program, JlvmError> {
  // the normative order (jlvm.md section 1, "Reading a canonical form"): the weight guard, in O(1) from the cached
  // weight, before the decoder expands a value's shared parts; then the decoder's shape errors; then programOf's
  if (value.weight > MAX_CANONICAL_WEIGHT) return { ok: false, error: limit(`the form weighs more than any program's, ${MAX_CANONICAL_WEIGHT.toString()}`) };
  const w = models.jlvm.JlvmProgram.decode(value);
  return w.ok ? programOf(w.value.expr) : { ok: false, error: malformed(`not a canonical program: ${describeFailure(w.error)}`) };
}

/** The program a canonical encoding denotes (`Parser.decode`). */
export function decodeProgram(bytes: Uint8Array): Result<Program, JlvmError | TypeMismatch> {
  const v = cbor.decode(bytes);
  if (!v.ok) return { ok: false, error: v.error.kind === "TypeMismatch" ? v.error : canonicalError(v.error) };
  return fromCanonicalValue(v.value);
}

// the class and its prototype frozen: no caller can replace a method every program shares
Object.freeze(Program);
Object.freeze(Program.prototype);

/** Whether `x` is a {@link Program} this package made: its private field, never its prototype. O(1). */
export function isProgram(x: unknown): x is Program {
  return typeof x === "object" && x !== null && isProgramOf(x);
}

/** The expression of a program this package made. */
export const programExpr = (p: Program): Expr => exprOf(p);
