/**
 * The JSON authoring surface (`Parser.scala`, `JsonSurface.scala`, jlvm.md section 1).
 *
 * Text is validated by core's strict canonical JSON parser first (duplicate keys, depth, exact numbers), then read
 * structurally with nulls and member order still visible, so a null field cannot turn a map expression into an
 * operation. Scalars and literal values are converted by the canonical parser itself, from their source text: where a
 * string or an integer is looked for, only a string's or a scalar's span is read, so no span is read once per level of
 * the containers around it ({@link Spans} counts what is read). Nothing recurses: the structure is scanned with an
 * explicit stack, and the program is built from it bottom up with another.
 */
import { ArrayValue, BytesValue, json, MapValue, Preview, Value, type CanonicalError, type Result } from "@kd5ujc/dilf4s-core";

import { canonicalError, Fields, NULL_EXPR, parseVar, programOf, type Expr, type Program } from "./ast.js";
import { malformed, type JlvmError } from "./error.js";
import { callName, isVariadic, opNamed, removed, type JlvmOp } from "./op.js";
import { hexBytes } from "./values.js";

/** A JSON node of text the strict parser has accepted, with its source span `[start, end)`. */
type Json =
  | { readonly kind: "null" | "scalar" | "str"; readonly start: number; readonly end: number }
  | { readonly kind: "arr"; readonly start: number; readonly end: number; readonly items: readonly Json[] }
  | { readonly kind: "obj"; readonly start: number; readonly end: number; readonly fields: readonly (readonly [string, Json])[] };

/** The spans read back through the strict parser, by total length (`Steps.spanned` in the reference). */
export class Spans {
  #read = 0;
  constructor(private readonly text: string) {}

  /** The UTF-16 units read back so far. */
  get read(): number {
    return this.#read;
  }

  value(j: Json): Result<Value, CanonicalError> {
    this.#read += j.end - j.start;
    return json.parse(this.text.slice(j.start, j.end));
  }

  /** The text of a JSON string; nothing is read for any other node. */
  str(j: Json): string | undefined {
    if (j.kind !== "str") return undefined;
    const v = this.value(j);
    return v.ok && v.value.kind === "text" ? v.value.value : undefined;
  }

  /** The integer a number denotes, as path text; nothing is read for any other node. */
  integer(j: Json): string | undefined {
    if (j.kind !== "scalar") return undefined;
    const v = this.value(j);
    return v.ok && v.value.kind === "int" ? v.value.value.toString() : undefined;
  }

  /** The canonical value of this JSON, with `0x`-hex strings as bytes (`JsonSurface.value`). */
  literal(j: Json): Out<Value> {
    const v = this.value(j);
    if (!v.ok) return bad(malformed(`not a value: ${v.error.kind}`));
    const b = fromCanonicalJson(v.value);
    return b.ok ? b : bad(malformed(`not a value: ${b.error.kind}`));
  }
}

type Out<T> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: JlvmError };
const good = <T>(value: T): Out<T> => ({ ok: true, value });
const bad = <T>(error: JlvmError): Out<T> => ({ ok: false, error });

const isSpace = (c: number): boolean => c === 0x20 || c === 0x09 || c === 0x0a || c === 0x0d;

type Open = { readonly arr: true; readonly start: number; readonly items: Json[] } | { readonly arr: false; readonly start: number; readonly fields: [string, Json][]; key: string | undefined };

/** The structure of `text`, which the strict parser has accepted: an explicit stack, no recursion. */
function tree(text: string, r: Spans): Json | undefined {
  const stack: Open[] = [];
  for (let i = 0; ; ) {
    for (let c = text.charCodeAt(i); isSpace(c) || c === 0x2c || c === 0x3a; c = text.charCodeAt(i)) i++;
    if (i >= text.length) return undefined;
    const start = i;
    const first = text.charCodeAt(i);
    let node: Json;
    if (first === 0x7b || first === 0x5b) {
      stack.push(first === 0x7b ? { arr: false, start, fields: [], key: undefined } : { arr: true, start, items: [] });
      i++;
      continue;
    }
    if (first === 0x7d || first === 0x5d) {
      i++;
      const open = stack.pop();
      if (open === undefined) return undefined;
      node = open.arr ? { kind: "arr", start: open.start, end: i, items: open.items } : { kind: "obj", start: open.start, end: i, fields: open.fields };
    } else if (first === 0x22) {
      i++;
      for (let c = text.charCodeAt(i); c !== 0x22; c = text.charCodeAt(i)) {
        if (Number.isNaN(c)) return undefined;
        i += c === 0x5c ? 2 : 1;
      }
      i++;
      const str: Json = { kind: "str", start, end: i };
      // in an object awaiting a key, a string is the key
      const top = stack.at(-1);
      if (top !== undefined && !top.arr && top.key === undefined) {
        const k = r.str(str);
        if (k === undefined) return undefined;
        top.key = k;
        continue;
      }
      node = str;
    } else {
      for (let c = text.charCodeAt(i); !Number.isNaN(c) && !isSpace(c) && c !== 0x2c && c !== 0x5d && c !== 0x7d; c = text.charCodeAt(i)) i++;
      node = { kind: first === 0x6e ? "null" : "scalar", start, end: i };
    }
    const top = stack.at(-1);
    if (top === undefined) return node;
    if (top.arr) top.items.push(node);
    else {
      if (top.key === undefined) return undefined;
      top.fields.push([top.key, node]);
      top.key = undefined;
    }
  }
}

/** Program or data text through the strict parser: a depth bound is `Limit` for a program and `Malformed` for data. */
function strict(text: string, depthIsLimit: boolean): Out<Value> {
  const v = json.parse(text);
  if (v.ok) return v;
  return bad(v.error.kind === "DepthExceeded" && depthIsLimit ? canonicalError(v.error) : malformed(`not canonical JSON: ${v.error.kind}`));
}

/** Parses a program from its JSON surface text (`Parser.parse`). */
export function parseProgram(text: string, spans: Spans = new Spans(text)): Result<Program, JlvmError> {
  const checked = strict(text, true);
  if (!checked.ok) return checked;
  const j = tree(text, spans);
  if (j === undefined) return bad(malformed("not JSON"));
  const expr = build(j, spans);
  return expr.ok ? programOf(expr.value) : expr;
}

/** Parses input data or a host binding from JSON surface text (`JsonSurface.parse`): the strict parser, then `0x`-hex strings as bytes. */
export function parseData(text: string): Result<Value, JlvmError> {
  const v = strict(text, false);
  if (!v.ok) return v;
  const b = fromCanonicalJson(v.value);
  return b.ok ? b : bad(malformed(`not a value: ${b.error.kind}`));
}

// ---- building the program, bottom up

/** How a JSON node is read: as an expression; as an array's elements, exempt from the call check; or as `let` bindings or one binding pair. */
type Ctx = "expr" | "elements" | "bindings" | "pair";

/** A node's children, each read in its context, and how their expressions make the node's. */
interface Plan {
  readonly kids: readonly (readonly [Json, Ctx])[];
  readonly combine: (xs: readonly Expr[]) => Out<Expr>;
}

type Planned = Out<Expr> | Plan;

const isPlan = (p: Planned): p is Plan => "kids" in p;

const lit = (v: Value): Expr => (v.kind === "null" ? NULL_EXPR : { lit: v });

/** The program `j` denotes: each node planned, its children built, then combined; an explicit stack, no recursion. */
function build(root: Json, r: Spans): Out<Expr> {
  const first = plan(root, "expr", r);
  if (!isPlan(first)) return first;
  const stack: { readonly plan: Plan; readonly out: Expr[] }[] = [{ plan: first, out: [] }];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const next = top.plan.kids[top.out.length];
    if (next !== undefined) {
      const p = plan(next[0], next[1], r);
      if (!isPlan(p)) {
        if (!p.ok) return p;
        top.out.push(p.value);
      } else stack.push({ plan: p, out: [] });
      continue;
    }
    stack.pop();
    const done = top.plan.combine(top.out);
    if (!done.ok) return done;
    const below = stack.at(-1);
    if (below === undefined) return done;
    below.out.push(done.value);
  }
  return bad(malformed("not JSON"));
}

const each = (items: readonly Json[], ctx: Ctx = "expr"): readonly (readonly [Json, Ctx])[] => items.map((j) => [j, ctx] as const);

function plan(j: Json, ctx: Ctx, r: Spans): Planned {
  switch (ctx) {
    case "elements":
      return j.kind === "arr" ? elements(j.items) : plan(j, "expr", r);
    case "pair":
      return j.kind === "arr" ? elements(j.items) : plan(j, "expr", r);
    case "bindings":
      // a let's [name, expression] pairs: a name is never code, so a pair is exempt from the array check
      return j.kind === "arr" ? { kids: each(j.items, "pair"), combine: (xs) => good({ arr: xs }) } : plan(j, "expr", r);
    case "expr":
      break;
  }
  switch (j.kind) {
    case "null":
      return good(NULL_EXPR);
    case "scalar":
    case "str": {
      const v = r.literal(j);
      return v.ok ? good(lit(v.value)) : v;
    }
    case "arr":
      return array(j.items, r);
    case "obj": {
      const [only] = j.fields;
      return j.fields.length === 1 && only !== undefined ? single(only[0], only[1], r) : mapExpr(j.fields);
    }
  }
}

const elements = (items: readonly Json[]): Plan => ({ kids: each(items), combine: (xs) => good({ arr: xs }) });

/** An array headed by a call's name is rejected, so text shaped like a call is never read as data by accident. */
function array(items: readonly Json[], r: Spans): Planned {
  const head = items[0];
  const name = head === undefined ? undefined : r.str(head);
  if (name === undefined || !callName(name)) return elements(items);
  const hint = removed(name);
  const call = hint === undefined ? `write the call as {"${name}": [...]}` : `"${name}" is not an operation: ${hint}`;
  return bad(malformed(`an array may not start with "${name}", which reads as a call: ${call}; quote data as {"literal": [...]}, or start it with {"literal": "${name}"}`));
}

/** A map expression: its fields' keys are distinct, as the strict parser checked. */
function mapExpr(fields: readonly (readonly [string, Json])[]): Plan {
  return { kids: each(fields.map(([, x]) => x)), combine: (xs) => good({ map: Fields.of(fields.map(([k], i) => [k, xs[i] ?? NULL_EXPR] as const)) }) };
}

function single(key: string, arg: Json, r: Spans): Planned {
  switch (key) {
    case "literal": {
      const v = r.literal(arg);
      return v.ok ? good(lit(v.value)) : v;
    }
    case "var":
      return variable(arg, r);
    case "object":
      return arg.kind === "obj" ? mapExpr(arg.fields) : bad(malformed('object takes an object: {"object": {key: expression, ...}}'));
  }
  if (key.startsWith("_")) return mapExpr([[key, arg]]);
  const hint = removed(key);
  if (hint !== undefined) return bad(malformed(`"${key}" is not an operation: ${hint}`));
  const op = opNamed(key);
  if (op === undefined) return bad(malformed(`unknown operation ${Preview.of(key).toString()}: write single-key data as {"object": {...}} or {"literal": {...}}`));
  return operation(op, arg);
}

function operation(op: JlvmOp, arg: Json): Plan {
  if (arg.kind === "arr") {
    const [bindings, body] = arg.items;
    if (op === "let" && arg.items.length === 2 && bindings !== undefined && body !== undefined) {
      return { kids: [[bindings, "bindings"], [body, "expr"]], combine: (xs) => good({ op, args: xs }) };
    }
    return { kids: each(arg.items), combine: (xs) => good({ op, args: xs }) };
  }
  return isVariadic(op)
    ? { kids: [[arg, "expr"]], combine: ([x]) => good({ spread: op, arg: x ?? NULL_EXPR }) }
    : { kids: [[arg, "expr"]], combine: (xs) => good({ op, args: xs }) };
}

/**
 * A path is text or an integer; an object or an array is an expression computing the path. Only a string's or a
 * number's span is read back, so a nested computed path is read once, not once per level.
 */
function path(j: Json, d: Value | undefined, r: Spans): Planned {
  const withDefault = d === undefined ? {} : { default: d };
  const t = r.str(j) ?? r.integer(j);
  if (t !== undefined) {
    const parsed = parseVar(t);
    return parsed.ok ? good({ var: t, ...withDefault }) : parsed;
  }
  switch (j.kind) {
    case "obj":
      return { kids: [[j, "expr"]], combine: ([p]) => good({ path: p ?? NULL_EXPR, ...withDefault }) };
    case "arr":
      // a literal array here is the path's segments, not code, so it is exempt from the array check
      return { kids: each(j.items), combine: (xs) => good({ path: { arr: xs }, ...withDefault }) };
    case "null":
    case "scalar":
    case "str":
      return bad(malformed("a var path is text, an integer, or an expression computing one"));
  }
}

function variable(arg: Json, r: Spans): Planned {
  if (arg.kind !== "arr") return path(arg, undefined, r);
  const [p, d] = arg.items;
  if (arg.items.length === 1 && p !== undefined) return path(p, undefined, r);
  if (arg.items.length === 2 && p !== undefined && d !== undefined) {
    const dv = defaultValue(d, r);
    // a null default is no default: the canonical form drops null
    return dv.ok ? path(p, dv.value.kind === "null" ? undefined : dv.value, r) : dv;
  }
  return bad(malformed("var takes a path, or [path, default]"));
}

/** A default is a literal value, never evaluated; an object or array default is quoted, `{"literal": ...}`. */
function defaultValue(j: Json, r: Spans): Out<Value> {
  const quote = 'a var default is a literal value: quote an object or array default with {"literal": ...}';
  if (j.kind === "obj") {
    const [only] = j.fields;
    return j.fields.length === 1 && only?.[0] === "literal" ? r.literal(only[1]) : bad(malformed(quote));
  }
  return j.kind === "arr" ? bad(malformed(quote)) : r.literal(j);
}

// ---- 0x strings as bytes

type Rebuilding = { readonly map: false; readonly items: readonly Value[]; readonly out: Value[] } | { readonly map: true; readonly entries: readonly (readonly [string, Value])[]; readonly out: Value[] };

/**
 * Strings in `0x`-hex form become bytes, everywhere in the value (`JsonSurface.fromCanonicalJson`): every leaf
 * through the conversion, the containers rebuilt, with an explicit stack and no recursion. Rebuilt containers hold
 * what the originals did, with text leaves now bytes, so they are as valid; a refusal would be the constructor's.
 */
export function fromCanonicalJson(root: Value): Result<Value, CanonicalError> {
  const leaf = (v: Value): Value => {
    if (v.kind !== "text") return v;
    const b = hexBytes(v.value);
    const bytes = b === undefined ? undefined : BytesValue.of(b);
    return bytes?.ok === true ? bytes.value : v;
  };
  const open = (v: Value): Rebuilding | undefined =>
    v.kind === "array" ? { map: false, items: v.items, out: [] } : v.kind === "map" ? { map: true, entries: v.entries(), out: [] } : undefined;
  const top0 = open(root);
  if (top0 === undefined) return { ok: true, value: leaf(root) };
  const stack: Rebuilding[] = [top0];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.out.length;
    const next = top.map ? top.entries[i]?.[1] : top.items[i];
    if (next !== undefined) {
      const o = open(next);
      if (o === undefined) top.out.push(leaf(next));
      else stack.push(o);
      continue;
    }
    stack.pop();
    const t = top;
    const built = t.map ? MapValue.of(t.entries.map(([k], j) => [k, t.out[j] ?? Value.null] as const)) : ArrayValue.of(t.out);
    if (!built.ok) return built;
    const below = stack.at(-1);
    if (below === undefined) return built;
    below.out.push(built.value);
  }
  return { ok: true, value: root };
}
