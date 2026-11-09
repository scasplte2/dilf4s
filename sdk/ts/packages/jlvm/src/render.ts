/**
 * Rendering a program's surface text (`Parser.render`, jlvm.md section 1): exact, so parsing the rendered text gives the
 * same program. A single-key map expression is written `{"object": ...}`, a literal array or map is quoted, and a
 * literal holding `0x`-hex text (which reads back as bytes) or a non-terminating rational has no surface form.
 *
 * The program is turned into a tree of JSON bottom up, with an explicit stack, then printed from a worklist: nothing
 * recurses, and no text is copied once per level.
 */
import { ArrayValue, json, MapValue, TextValue, Value, type Result } from "@kd5ujc/dilf4s-core";
import { toHexOf } from "@kd5ujc/dilf4s-core/internal";

import { programExpr, sortedFields, view, type Expr, type Program } from "./ast.js";
import { malformed, type JlvmError } from "./error.js";
import { callName } from "./op.js";
import { hexBytes } from "./values.js";

/** JSON being written: text already rendered, or a container of it (a map expression may hold null fields, which no value can). */
type Out = { readonly t: "raw"; readonly text: string } | { readonly t: "arr"; readonly items: readonly Out[] } | { readonly t: "obj"; readonly fields: readonly (readonly [string, Out])[] };

type Rendered<T> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: JlvmError };
const good = <T>(value: T): Rendered<T> => ({ ok: true, value });
const bad = <T>(error: JlvmError): Rendered<T> => ({ ok: false, error });

const raw = (text: string): Out => ({ t: "raw", text });
const arr = (items: readonly Out[]): Out => ({ t: "arr", items });

/** A string as JSON text. */
function quoted(s: string): Rendered<string> {
  const t = TextValue.of(s);
  if (!t.ok) return bad(malformed("text with an unpaired surrogate"));
  const r = json.renderWithin(t.value, json.MAX_RENDER_BYTES);
  return r.ok ? r : bad(malformed(`no JSON text: ${r.error.kind}`));
}

const member = (name: string, arg: Out): Rendered<Out> => {
  const k = quoted(name);
  return k.ok ? good({ t: "obj", fields: [[k.value, arg]] }) : k;
};

/** Whether a text value (not a map key) in `v` has the `0x`-hex form, which reads back as bytes. */
function hasHexText(root: Value): boolean {
  const todo: Value[] = [root];
  for (let v = todo.pop(); v !== undefined; v = todo.pop()) {
    if (v.kind === "text" && hexBytes(v.value) !== undefined) return true;
    if (v.kind === "array") for (const x of v.items) todo.push(x);
    else if (v.kind === "map") for (const [, x] of v.entries()) todo.push(x);
  }
  return false;
}

type Rebuilding = { readonly map: false; readonly items: readonly Value[]; readonly out: Value[] } | { readonly map: true; readonly entries: readonly (readonly [string, Value])[]; readonly out: Value[] };

/** Bytes become text in `0x`-hex form, everywhere in the value (`JsonSurface.toCanonicalJson`): an explicit stack. */
function toCanonicalJson(root: Value): Rendered<Value> {
  const leaf = (v: Value): Rendered<Value> => {
    if (v.kind !== "bytes") return good(v);
    const t = TextValue.of(`0x${toHexOf(v.bytes)}`);
    return t.ok ? good(t.value) : bad(malformed("bytes with no hex form"));
  };
  const open = (v: Value): Rebuilding | undefined => (v.kind === "array" ? { map: false, items: v.items, out: [] } : v.kind === "map" ? { map: true, entries: v.entries(), out: [] } : undefined);
  const first = open(root);
  if (first === undefined) return leaf(root);
  const stack: Rebuilding[] = [first];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.out.length;
    const next = top.map ? top.entries[i]?.[1] : top.items[i];
    if (next !== undefined) {
      const o = open(next);
      if (o !== undefined) {
        stack.push(o);
        continue;
      }
      const l = leaf(next);
      if (!l.ok) return l;
      top.out.push(l.value);
      continue;
    }
    stack.pop();
    const t = top;
    const built = t.map ? MapValue.of(t.entries.map(([k], j) => [k, t.out[j] ?? Value.null] as const)) : ArrayValue.of(t.out);
    if (!built.ok) return bad(malformed(`not a value: ${built.error.kind}`));
    const below = stack.at(-1);
    if (below === undefined) return good(built.value);
    below.out.push(built.value);
  }
  return good(root);
}

/**
 * A value as JSON surface text that reads back as the same value (`JsonSurface.render`): bytes in `0x`-hex form. Its
 * rendering is bounded by `json.MAX_RENDER_BYTES`, refused from the weight when the weight is over it; a
 * non-terminating rational has none; and text in `0x`-hex form (it would read back as bytes) has none either.
 */
export function renderValue(v: Value): Rendered<string> {
  if (v.weight > BigInt(json.MAX_RENDER_BYTES)) return bad(malformed("the value is too large to render"));
  if (hasHexText(v)) return bad(malformed("a value with 0x-hex text has no JSON surface form"));
  const c = toCanonicalJson(v);
  if (!c.ok) return c;
  const r = json.renderWithin(c.value, json.MAX_RENDER_BYTES);
  return r.ok ? r : bad(malformed(`no JSON surface form: ${r.error.kind}`));
}

const literal = (v: Value): Rendered<Out> => {
  const r = renderValue(v);
  return r.ok ? good(raw(r.value)) : r;
};

const quote = (v: Value): Rendered<Out> => {
  const l = literal(v);
  return l.ok ? member("literal", l.value) : l;
};

/** A default as written: an array or map is quoted. */
const defaultJson = (d: Value): Rendered<Out> => (d.kind === "array" || d.kind === "map" ? quote(d) : literal(d));

/** A node's children to render, and how their JSON makes the node's. */
interface Plan {
  readonly kids: readonly Expr[];
  readonly combine: (outs: readonly Out[]) => Rendered<Out>;
}

type Planned = Rendered<Out> | Plan;
const isPlan = (p: Planned): p is Plan => "kids" in p;

const NONE: Out = raw("null");

function plan(e: Expr): Planned {
  const v = view(e);
  switch (v.t) {
    case "lit":
      if (v.value === undefined) return good(raw("null"));
      return v.value.kind === "array" || v.value.kind === "map" ? quote(v.value) : literal(v.value);
    case "var": {
      const r = quoted(v.path);
      if (!r.ok) return r;
      if (v.default === undefined) return member("var", raw(r.value));
      const d = defaultJson(v.default);
      return d.ok ? member("var", arr([raw(r.value), d.value])) : d;
    }
    case "varAt": {
      // a literal path is quoted, or it would read as a static one; an object path with no default is written bare, so
      // a computed path adds one JSON level, as in the canonical form; a bare array would read as [path, default]
      const d = v.default === undefined ? undefined : defaultJson(v.default);
      if (d !== undefined && !d.ok) return d;
      const finish = (pj: Out): Rendered<Out> => (pj.t === "obj" && d === undefined ? member("var", pj) : member("var", arr(d === undefined ? [pj] : [pj, d.value])));
      const p = view(v.path);
      if (p.t === "lit") {
        // every literal path is quoted, the null literal too: a bare null reads back as no path
        const q = quote(p.value ?? Value.null);
        return q.ok ? finish(q.value) : q;
      }
      return { kids: [v.path], combine: ([pj = NONE]) => finish(pj) };
    }
    case "apply": {
      const [bindings, body] = v.args;
      const pairs = bindings === undefined ? undefined : view(bindings);
      if (v.op === "let" && v.args.length === 2 && pairs?.t === "arr" && body !== undefined) {
        // binding names are written plainly: the parser reads them back as names, whatever they spell
        // each pair as its name and expression, when it is [name, expression]; any other pair as itself
        const named = pairs.items.map((pair): readonly [Expr, Expr] | undefined => {
          const nx = view(pair);
          const [n, x] = nx.t === "arr" && nx.items.length === 2 ? nx.items : [];
          return n !== undefined && x !== undefined && isTextLiteral(n) ? [n, x] : undefined;
        });
        return {
          kids: [...pairs.items.flatMap((pair, i) => named[i] ?? [pair]), body],
          combine: (outs) => {
            let at = 0;
            const ps = named.map((pair) => {
              const o = pair !== undefined ? arr([outs[at] ?? NONE, outs[at + 1] ?? NONE]) : (outs[at] ?? NONE);
              at += pair !== undefined ? 2 : 1;
              return o;
            });
            return member("let", arr([arr(ps), outs[at] ?? NONE]));
          },
        };
      }
      const op = v.op;
      return { kids: v.args, combine: (outs) => member(op, arr(outs)) };
    }
    case "spread": {
      const op = v.op;
      return { kids: [v.arg], combine: ([a = NONE]) => member(op, a) };
    }
    case "arr": {
      // a leading text naming a call is quoted: unquoted, the array would not parse
      const [first] = v.items;
      const head = first === undefined ? undefined : view(first);
      if (head?.t === "lit" && head.value?.kind === "text" && callName(head.value.value)) {
        const q = quote(head.value);
        if (!q.ok) return q;
        const quotedHead = q.value;
        return { kids: v.items.slice(1), combine: (outs) => good(arr([quotedHead, ...outs])) };
      }
      return { kids: v.items, combine: (outs) => good(arr(outs)) };
    }
    case "map": {
      const fields = sortedFields(v.fields);
      return {
        kids: fields.map(([, x]) => x),
        combine: (outs) => {
          const named: (readonly [string, Out])[] = [];
          for (const [i, [k]] of fields.entries()) {
            const q = quoted(k);
            if (!q.ok) return q;
            named.push([q.value, outs[i] ?? NONE]);
          }
          const [only] = fields;
          const [onlyOut] = outs;
          if (fields.length === 1 && only !== undefined && onlyOut !== undefined && !only[0].startsWith("_")) {
            const inner = member(only[0], onlyOut);
            return inner.ok ? member("object", inner.value) : inner;
          }
          return good({ t: "obj", fields: named });
        },
      };
    }
  }
}

function isTextLiteral(e: Expr): boolean {
  const v = view(e);
  return v.t === "lit" && v.value?.kind === "text";
}

/** Compact text from a worklist: no recursion, and no text copied once per level. */
function print(root: Out): string {
  const pieces: string[] = [];
  const todo: (Out | string)[] = [root];
  for (let next = todo.pop(); next !== undefined; next = todo.pop()) {
    if (typeof next === "string") {
      pieces.push(next);
      continue;
    }
    switch (next.t) {
      case "raw":
        pieces.push(next.text);
        break;
      case "arr": {
        const parts: (Out | string)[] = ["["];
        next.items.forEach((x, i) => parts.push(...(i === 0 ? [x] : [",", x])));
        parts.push("]");
        for (let i = parts.length - 1; i >= 0; i--) todo.push(parts[i] ?? "");
        break;
      }
      case "obj": {
        const parts: (Out | string)[] = ["{"];
        next.fields.forEach(([k, x], i) => parts.push(...(i === 0 ? [k, ":", x] : [",", k, ":", x])));
        parts.push("}");
        for (let i = parts.length - 1; i >= 0; i--) todo.push(parts[i] ?? "");
        break;
      }
    }
  }
  return pieces.join("");
}

/** A program's surface text (`Parser.render`): exact, so {@link parse} reads it back as the same program. */
export function renderProgram(program: Program): Result<string, JlvmError> {
  const first = plan(programExpr(program));
  if (!isPlan(first)) return first.ok ? { ok: true, value: print(first.value) } : first;
  const stack: { readonly plan: Plan; readonly out: Out[] }[] = [{ plan: first, out: [] }];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const next = top.plan.kids[top.out.length];
    if (next !== undefined) {
      const p = plan(next);
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
    if (below === undefined) return { ok: true, value: print(done.value) };
    below.out.push(done.value);
  }
  return { ok: false, error: malformed("an empty program") };
}
