/**
 * The checked form of a program (`Node.scala`). Every special form's shape (`if`, `let`, the callbacks) is validated
 * once, by {@link compile}, so evaluation never meets a malformed form; what remains are value errors. Compilation
 * walks the program bottom up with an explicit stack, carrying the `let` names in scope as a persistent sorted set,
 * so it never recurses and never hashes a name the program chose.
 */
import { BoolValue, MapValue, NullValue, Preview, type Value } from "@kd5ujc/dilf4s-core";
import { compareCodePoints, MapShape } from "@kd5ujc/dilf4s-core/internal";

import { parseVar, programExpr, sortedFields, view, type Expr, type Program, type VarRef } from "./ast.js";
import { malformed } from "./error.js";
import { isCrypto, isStrict, isVariadic, type CryptoOp, type JlvmOp, type StrictOp, type VariadicOp } from "./op.js";
import { bad, good, kindName, segmentCount, type Out } from "./values.js";

/** `and`, `or`, `default`. */
export type Logical = "and" | "or" | "default";

/** `map`, `filter`, `all`, `none`, `find`, and `count` with a callback. */
export type Iteration = "map" | "filter" | "all" | "none" | "find" | "count";

/** A `let`'s bindings, in evaluation order, with their positions sorted by name: names are found by binary search, never hashed. */
export interface Bindings {
  readonly bindings: readonly (readonly [string, Node])[];
  /** The names' segment counts, summed once: what each evaluation charges per insertion. */
  readonly nameSegments: number;
  /** Positions into `bindings`, sorted by name. */
  readonly sorted: readonly number[];
}

/** The position of the binding named `name`, if this `let` binds it. */
export function position(b: Bindings, name: string): number | undefined {
  let [lo, hi] = [0, b.sorted.length];
  for (; lo < hi; ) {
    const mid = (lo + hi) >>> 1;
    const p = b.sorted[mid] ?? 0;
    const c = compareCodePoints(b.bindings[p]?.[0] ?? "", name);
    if (c === 0) return p;
    if (c < 0) lo = mid + 1;
    else hi = mid;
  }
  return undefined;
}

/** A node of the checked program. */
export type Node =
  | { readonly t: "const"; readonly value: Value }
  /** A static path, with its segment counts (a `$name` counts as a segment), computed once. */
  | { readonly t: "read"; readonly ref: VarRef; readonly text: string; readonly default: Value | undefined; readonly all: number; readonly first: number }
  /** `var` with a computed path. */
  | { readonly t: "readAt"; readonly path: Node; readonly default: Value | undefined }
  | { readonly t: "arrayOf"; readonly items: readonly Node[] }
  /** Field values in code-point order of their keys, the evaluation order; the keys' shape builds the map from them. */
  | { readonly t: "objectOf"; readonly shape: MapShape; readonly fields: readonly Node[] }
  | { readonly t: "strict"; readonly op: StrictOp; readonly args: readonly Node[] }
  /** A variadic strict operation on the elements of one array value. */
  | { readonly t: "spread"; readonly op: VariadicOp; readonly arg: Node }
  | { readonly t: "cond"; readonly branches: readonly (readonly [Node, Node])[]; readonly otherwise: Node }
  | { readonly t: "logic"; readonly kind: Logical; readonly args: readonly Node[] }
  | { readonly t: "bind"; readonly bindings: Bindings; readonly body: Node }
  | { readonly t: "each"; readonly kind: Iteration; readonly over: Node; readonly body: Node }
  /** `some`, with an optional quorum. */
  | { readonly t: "quorum"; readonly over: Node; readonly body: Node; readonly min: Node | undefined }
  /** `reduce`, with an optional initial accumulator. */
  | { readonly t: "fold"; readonly over: Node; readonly body: Node; readonly init: Node | undefined }
  | { readonly t: "lookup"; readonly op: "missing" | "missing_some"; readonly args: readonly Node[] }
  | { readonly t: "crypto"; readonly op: CryptoOp; readonly args: readonly Node[] };

/** The null literal, as a node. */
const NULL: Node = { t: "const", value: NullValue.instance };

/** The names a `let` may bind: non-empty, without `.` or a leading `$`. */
const bindable = (name: string): boolean => name.length > 0 && !name.includes(".") && !name.startsWith("$");

/**
 * The `let` names in scope: a persistent sorted set (core's map, each name bound to true), shared by every node under
 * the same scope.
 */
type Lets = MapValue;

const inScope = (lets: Lets, name: string): boolean => lets.has(name);

/** `lets` with `name` added: a name is program text, so well-formed, and true nests nothing, so it is never refused. */
const adding = (lets: Lets, name: string): Lets => {
  const added = lets.set(name, BoolValue.true);
  return added.ok ? added.value : lets;
};

/** A node's sub-expressions, each compiled in its scope, and how their nodes make the node's. */
interface Plan {
  readonly kids: readonly (readonly [Expr, Lets])[];
  readonly combine: (ns: readonly Node[]) => Out<Node>;
}

type Planned = Out<Node> | Plan;

const isPlan = (p: Planned): p is Plan => "kids" in p;

const all = (es: readonly Expr[], lets: Lets): readonly (readonly [Expr, Lets])[] => es.map((e) => [e, lets] as const);

/** Compiles a program: bottom up, with an explicit stack. The work is linear in the program's size, times the log of its scope. */
export function compile(program: Program): Out<Node> {
  const first = plan(programExpr(program), MapValue.empty);
  if (!isPlan(first)) return first;
  const stack: { readonly plan: Plan; readonly out: Node[] }[] = [{ plan: first, out: [] }];
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const next = top.plan.kids[top.out.length];
    if (next !== undefined) {
      const p = plan(next[0], next[1]);
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
  return bad(malformed("an empty program"));
}

const failed = (m: string): Out<Node> => bad(malformed(m));

function plan(e: Expr, lets: Lets): Planned {
  const v = view(e);
  switch (v.t) {
    case "lit":
      return good(v.value === undefined ? NULL : { t: "const", value: v.value });
    case "var": {
      const r = parseVar(v.path);
      if (!r.ok) return r;
      const ref = r.value;
      const head = ref.path[0];
      if (!ref.bound && head !== undefined && inScope(lets, head)) {
        return failed(
          `${Preview.of(v.path).toString()} starts with ${Preview.of(head).toString()}, which let binds: read the binding with a leading '$', the data as {"var": [[name, ...]]}, or a callback element's field as "$current." and the name`,
        );
      }
      const counted = (p: readonly string[]): number => p.reduce((n, s) => n + segmentCount(s), 0);
      const [count, first] = ref.bound ? [segmentCount(ref.name) + counted(ref.path), 0] : [counted(ref.path), head === undefined ? 0 : segmentCount(head)];
      return good({ t: "read", ref, text: v.path, default: v.default, all: count, first });
    }
    case "varAt": {
      const d = v.default;
      return { kids: [[v.path, lets]], combine: ([p]) => good({ t: "readAt", path: p ?? NULL, default: d }) };
    }
    case "spread": {
      const op = v.op;
      if (!isVariadic(op)) return failed(`${op} does not spread its argument`);
      return {
        kids: [[v.arg, lets]],
        combine: ([n = NULL]) => {
          const notArray = (k: string): Out<Node> => failed(`a spread ${op} takes an array, and its argument is always of kind ${k}`);
          if (n.t === "const" && n.value.kind !== "array") return notArray(kindName(n.value));
          if (n.t === "objectOf") return notArray("object");
          return good({ t: "spread", op, arg: n });
        },
      };
    }
    case "arr":
      return { kids: all(v.items, lets), combine: (ns) => good({ t: "arrayOf", items: ns }) };
    case "map": {
      const fields = sortedFields(v.fields);
      // the keys' canonical order, found once here: building the map at run time compares, measures and encodes no key.
      // A program's keys are distinct, well-formed text, so the shape is never refused
      const shape = MapShape.of(fields.map(([k]) => k));
      if (!shape.ok) return failed("an object literal's keys are distinct text");
      const s = shape.value;
      return { kids: all(fields.map(([, x]) => x), lets), combine: (ns) => good({ t: "objectOf", shape: s, fields: ns }) };
    }
    case "apply":
      return apply(v.op, v.args, lets);
  }
}

function apply(op: JlvmOp, args: readonly Expr[], lets: Lets): Planned {
  const each = (kind: Iteration): Planned =>
    args.length === 2 ? { kids: all(args, lets), combine: ([over = NULL, body = NULL]) => good({ t: "each", kind, over, body }) } : failed(`${op} takes [array, callback]`);
  if (isCrypto(op)) return { kids: all(args, lets), combine: (ns) => good({ t: "crypto", op, args: ns }) };
  // count is a callback when written with two arguments, decided by syntax
  if (op === "count") return args.length === 2 ? each("count") : { kids: all(args, lets), combine: (ns) => good({ t: "strict", op, args: ns }) };
  if (isStrict(op)) return { kids: all(args, lets), combine: (ns) => good({ t: "strict", op, args: ns }) };
  switch (op) {
    case "if":
      return {
        kids: all(args, lets),
        combine: (ns) => {
          const n = ns.length;
          const pairs = (k: number): (readonly [Node, Node])[] => Array.from({ length: k }, (_, i) => [ns[2 * i] ?? NULL, ns[2 * i + 1] ?? NULL] as const);
          if (n >= 2 && n % 2 === 0) return good({ t: "cond", branches: pairs(n / 2), otherwise: NULL });
          if (n >= 3) return good({ t: "cond", branches: pairs((n - 1) / 2), otherwise: ns[n - 1] ?? NULL });
          return failed("if takes [condition, then, (condition, then)..., (else)]");
        },
      };
    case "and":
    case "or":
    case "default": {
      const kind = op;
      return { kids: all(args, lets), combine: (ns) => good({ t: "logic", kind, args: ns }) };
    }
    case "let":
      return letForm(args, lets);
    case "map":
    case "filter":
    case "all":
    case "none":
    case "find":
      return each(op);
    case "some":
      if (args.length === 2 || args.length === 3) {
        return { kids: all(args, lets), combine: ([over = NULL, body = NULL, min]) => good({ t: "quorum", over, body, min }) };
      }
      return failed("some takes [array, callback] or [array, callback, minimum]");
    case "reduce":
      if (args.length === 2 || args.length === 3) {
        return { kids: all(args, lets), combine: ([over = NULL, body = NULL, init]) => good({ t: "fold", over, body, init }) };
      }
      return failed("reduce takes [array, callback] or [array, callback, initial]");
    case "missing":
    case "missing_some": {
      const lookup = op;
      return { kids: all(args, lets), combine: (ns) => good({ t: "lookup", op: lookup, args: ns }) };
    }
  }
}

/** A `let`'s `[name, expression]` pairs, or its object of bindings in code-point order of the names. */
function bindingPairs(first: Expr | undefined): Out<readonly (readonly [string, Expr])[]> {
  const f = first === undefined ? undefined : view(first);
  if (f?.t === "map") return good(sortedFields(f.fields));
  if (f?.t !== "arr") return bad(malformed("let takes [bindings, body]"));
  const read: (readonly [string, Expr])[] = [];
  for (const pair of f.items) {
    const p = view(pair);
    const [n, x] = p.t === "arr" ? p.items : [];
    const nv = n === undefined ? undefined : view(n);
    if (p.t !== "arr" || p.items.length !== 2 || x === undefined || nv?.t !== "lit" || nv.value?.kind !== "text") return bad(malformed("a let binding is [name, expression]"));
    read.push([nv.value.value, x]);
  }
  return good(read);
}

/** `[[[name, expr], ...], body]`, or `[{name: expr, ...}, body]` with the bindings in code-point order of the names. */
function letForm(args: readonly Expr[], lets: Lets): Planned {
  const [first, body] = args;
  const pairs = bindingPairs(first);
  if (args.length !== 2 || body === undefined) return failed("let takes [bindings, body]");
  if (!pairs.ok) return pairs;
  const bs = pairs.value;
  const unbindable = bs.find(([n]) => !bindable(n));
  if (unbindable !== undefined) return failed(`let cannot bind ${Preview.of(unbindable[0]).toString()}: names are non-empty, without '.' or a leading '$'`);
  const sorted = bs.map((_, i) => i).sort((a, b) => compareCodePoints(bs[a]?.[0] ?? "", bs[b]?.[0] ?? ""));
  if (sorted.some((p, i) => i > 0 && bs[p]?.[0] === bs[sorted[i - 1] ?? 0]?.[0])) return failed("let binds a name twice");
  // each binding sees the ones before it, and the body sees all of them
  const scopes: Lets[] = [lets];
  for (const [n] of bs) scopes.push(adding(scopes.at(-1) ?? lets, n));
  const kids: (readonly [Expr, Lets])[] = bs.map(([, x], i) => [x, scopes[i] ?? lets] as const);
  kids.push([body, scopes.at(-1) ?? lets]);
  const nameSegments = bs.reduce((n, [name]) => n + segmentCount(name), 0);
  return {
    kids,
    combine: (ns) => {
      const bindings = bs.map(([n], i) => [n, ns[i] ?? NULL] as const);
      return good({ t: "bind", bindings: { bindings, nameSegments, sorted }, body: ns[bs.length] ?? NULL });
    },
  };
}

