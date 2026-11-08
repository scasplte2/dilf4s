/**
 * The evaluation rules as small steps, run by an explicit continuation stack (`Machine.scala`: `Semantics` and the
 * `direct` driver; `sdk/rust/jlvm/src/machine.rs`).
 *
 * The reference writes each node's behaviour as `Step`s whose continuations are closures. Here, as in the Rust port,
 * every closure is a named variant: {@link Then} for the work a `charge` pays for, {@link Kont} for what `eval` does
 * with a value. Each variant cites the reference line it ports, so the order of charges (which decides the gas of an
 * out-of-gas run) can be read off side by side. The driver keeps the continuations in an array; nothing recurses on
 * the program or the data.
 */
import { models, Preview, type TextValue, type Value } from "@kd5ujc/dilf4s-core";

import { segments as splitPath, parseVar, type VarRef } from "./ast.js";
import { CRYPTO } from "./crypto.js";
import { arity, domain, limit, malformed, typeError, type EvalError } from "./error.js";
import { base, perUnit, pathText, plus, rendered as renderCharge, sized, sizedWithin, times, type GasSchedule } from "./gas.js";
import { Heap, JMap, type JValue } from "./jvalue.js";
import { position, type Bindings as LetBindings, type Iteration, type Logical, type Node } from "./node.js";
import type { CryptoOp, StrictOp, VariadicOp } from "./op.js";
import * as ops from "./ops.js";
import { array, at, bad, bool, count, FALSE, integer, kindName, mapResult, NULL, num, pathSegments, segmentCount, smallIntegerText, TRUE, truthy, type Num, type Out } from "./values.js";

/** A lint finding recorded during evaluation. It never changes a result or its gas. */
export interface Warning {
  /**
   * Inside a callback, a plain path resolved in an element, and an outer element or the data holds the same first
   * key: `$current` (or an explicit outer path) says which one is meant. The path is the caller's, so it is held as a
   * preview: two long paths that share their preview count as one warning.
   */
  readonly kind: "AmbiguousPath";
  readonly path: Preview;
}

/**
 * The host's bindings, as the machine reads them: the value bound to a name (null when bound to null), or `undefined`
 * when the name is unbound. `Bindings` and a core `MapValue` (which binds no name to null) both are.
 */
export interface HostEnv {
  get(name: string): Value | undefined;
}

/** At most this many distinct warnings are kept per evaluation. */
export const MAX_WARNINGS = models.limits.JLVM_MAX_WARNINGS;

// ------------------------------------------------------------------------------------------------ scope

/**
 * The `$` bindings: the host's, then one level per enclosing `let` and callback (innermost first). A walk is bounded
 * by lexical nesting, not by the number of bindings: a `let` is one level whose names are found by binary search, and
 * its values live in the machine's let stack.
 */
type Bindings =
  | { readonly t: "host"; readonly env: HostEnv | undefined }
  /** The first `count` bindings of a `let`, whose values are `slots[slot]`. */
  | { readonly t: "let"; readonly parent: Bindings; readonly node: LetBindings; readonly slot: number; readonly count: number }
  | { readonly t: "element"; readonly parent: Bindings; readonly current: JValue; readonly index: JValue }
  | { readonly t: "accumulating"; readonly parent: Bindings; readonly current: JValue; readonly index: JValue; readonly accumulator: JValue };

/** An enclosing callback's contribution to plain paths (`Machine.scala` `Frame`). */
type Frame = { readonly t: "element"; readonly value: JValue } | { readonly t: "accumulating"; readonly current: JValue; readonly accumulator: JValue };

interface Frames {
  readonly frame: Frame;
  readonly parent: Frames | undefined;
}

/** What `""` reads. */
const frameElement = (f: Frame): JValue => (f.t === "element" ? f.value : f.current);

/** The value a frame binds to `key`, if it binds it. */
function frameLookup(f: Frame, key: string): JValue | undefined {
  if (f.t === "element") return f.value.kind === "map" ? f.value.get(key) : undefined;
  return key === "current" ? f.current : key === "accumulator" ? f.accumulator : undefined;
}

/** What a node sees (`Machine.scala` `Scope`): the bindings and, inside callbacks, the enclosing frames. */
interface Scope {
  readonly bindings: Bindings;
  readonly frames: Frames | undefined;
  readonly frameCount: number;
}

const enter = (scope: Scope, bindings: Bindings, frame: Frame): Scope => ({
  bindings,
  frames: { frame, parent: scope.frames },
  frameCount: scope.frameCount + 1,
});

/**
 * A plain data path (`Scope.resolve`): `""` is the innermost element, or the data outside callbacks. Otherwise the
 * path is read from the innermost frame binding its first key, else from the data; it is ambiguous when an outer frame
 * or the data also binds that key.
 */
function resolve(data: JValue, scope: Scope, path: readonly string[]): readonly [JValue, boolean] {
  const [key, ...rest] = path;
  if (key === undefined) return [scope.frames === undefined ? data : frameElement(scope.frames.frame), false];
  for (let f = scope.frames; f !== undefined; f = f.parent) {
    const inner = frameLookup(f.frame, key);
    if (inner === undefined) continue;
    let ambiguous = at(data, [key]).kind !== "null";
    for (let g = f.parent; g !== undefined && !ambiguous; g = g.parent) ambiguous = frameLookup(g.frame, key) !== undefined;
    return [at(inner, rest), ambiguous];
  }
  return [at(data, path), false];
}

/** The binding `$name`, innermost first. */
function binding(slots: readonly (readonly JValue[])[], scope: Scope, name: string): JValue | undefined {
  for (let here: Bindings = scope.bindings; ; ) {
    switch (here.t) {
      case "host":
        return here.env?.get(name);
      case "let": {
        const p = position(here.node, name);
        const found = p !== undefined && p < here.count ? slots[here.slot]?.[p] : undefined;
        if (found !== undefined) return found;
        here = here.parent;
        break;
      }
      case "element":
        if (name === "current") return here.current;
        if (name === "index") return here.index;
        here = here.parent;
        break;
      case "accumulating":
        if (name === "current") return here.current;
        if (name === "index") return here.index;
        if (name === "accumulator") return here.accumulator;
        here = here.parent;
        break;
    }
  }
}

// ------------------------------------------------------------------------------------------------ steps

/** What `all` continues with once every argument is evaluated. */
type AllThen =
  /** `Machine.scala:145` */
  | { readonly t: "array" }
  /** `Machine.scala:150` */
  | { readonly t: "object"; readonly keys: readonly string[]; readonly keyBytes: readonly number[] }
  /** `Machine.scala:156` */
  | { readonly t: "strict"; readonly op: StrictOp }
  /** `Machine.scala:222-225` */
  | { readonly t: "lookup"; readonly op: "missing" | "missing_some" }
  /** `Machine.scala:233-234` */
  | { readonly t: "crypto"; readonly op: CryptoOp };

/** What a callback form does with its evaluated collection (`Machine.scala:186`, `:192`, `:210`). */
type CollThen =
  | { readonly t: "each"; readonly kind: Iteration; readonly body: Node; readonly scope: Scope }
  | { readonly t: "quorum"; readonly body: Node; readonly min: Node | undefined; readonly scope: Scope }
  | { readonly t: "fold"; readonly body: Node; readonly init: Node | undefined; readonly scope: Scope };

/** A callback's collection: the array's items, shared. */
type Elements = readonly JValue[];

/** The work a `charge` pays for: each is the reference's `() => Step` thunk at the cited line. */
type Then =
  /** `Machine.scala:113` */
  | { readonly t: "done"; readonly value: JValue }
  /** `Machine.scala:256`: reads a path already paid for. */
  | { readonly t: "lookup"; readonly ref: VarRef; readonly default: Value | undefined; readonly scope: Scope }
  /** `Machine.scala:126`: parses a computed text path, already paid for. */
  | { readonly t: "parsePath"; readonly text: string; readonly default: Value | undefined; readonly scope: Scope }
  /** `Machine.scala:138`: converts a computed array path's segments, already paid for. */
  | { readonly t: "segmentPath"; readonly segments: Elements; readonly default: Value | undefined; readonly scope: Scope }
  /** `Machine.scala:145`, `:150`, `:156`, `:222`, `:233`: evaluates the arguments. */
  | { readonly t: "all"; readonly args: readonly Node[]; readonly scope: Scope; readonly then: AllThen }
  /** `Machine.scala:156`, `:167`: the operation, after its size charge (a spread's, on its array's elements). */
  | { readonly t: "operate"; readonly op: StrictOp; readonly vs: Elements }
  /** `Machine.scala:163` */
  | { readonly t: "spreadArg"; readonly op: VariadicOp; readonly arg: Node; readonly scope: Scope }
  /** `Machine.scala:173` */
  | { readonly t: "cond"; readonly branches: readonly (readonly [Node, Node])[]; readonly otherwise: Node; readonly scope: Scope }
  /** `Machine.scala:335`: tests condition `j`, after its element charge. */
  | { readonly t: "condTest"; readonly branches: readonly (readonly [Node, Node])[]; readonly j: number; readonly otherwise: Node; readonly scope: Scope }
  /** `Machine.scala:175` */
  | { readonly t: "logic"; readonly kind: Logical; readonly args: readonly Node[]; readonly scope: Scope }
  /** `Machine.scala:182` */
  | { readonly t: "bind"; readonly node: LetBindings; readonly body: Node; readonly scope: Scope }
  /** `Machine.scala:186`, `:192`, `:210`: evaluates the collection. */
  | { readonly t: "collection"; readonly op: "map" | "filter" | "all" | "none" | "find" | "count" | "some" | "reduce"; readonly over: Node; readonly then: CollThen }
  /** `Machine.scala:375`: the collection's elements, after their charge. */
  | { readonly t: "proceed"; readonly then: CollThen; readonly vs: Elements }
  /** `Machine.scala:399`: one run of a `map`/`filter`/`all`/`none`/`find`/`count` body. */
  | { readonly t: "iterRun"; readonly it: Iterating }
  /** `Machine.scala:427` */
  | { readonly t: "quorumRun"; readonly q: Quorum }
  /** `Machine.scala:438` */
  | { readonly t: "foldRun"; readonly vs: Elements; readonly body: Node; readonly scope: Scope; readonly i: number; readonly acc: JValue }
  /** `Machine.scala:325-326`: one step of `+` or `*`, after its accumulator's charge. */
  | { readonly t: "accumulate"; readonly op: "+" | "*"; readonly rest: readonly Num[]; readonly i: number; readonly acc: Num }
  /** `Machine.scala:308`: renders `join`'s pieces, after the rendering charge. */
  | { readonly t: "joinPieces"; readonly vs: Elements }
  /** `Machine.scala:308`: builds a `join`, after its separators' charge. */
  | { readonly t: "joinBuild"; readonly j: ops.Joining }
  /** `Machine.scala:310` */
  | { readonly t: "cat"; readonly vs: Elements }
  /** `Machine.scala:225` */
  | { readonly t: "lookupRun"; readonly op: "missing" | "missing_some"; readonly vs: Elements; readonly scope: Scope }
  /** `Machine.scala:234` */
  | { readonly t: "cryptoRun"; readonly op: CryptoOp; readonly vs: Elements };

/** A `map`/`filter`/`all`/`none`/`find`/`count` in progress: the element `i`, what `map` and `filter` keep, and `count`'s count. */
interface Iterating {
  readonly kind: Iteration;
  readonly vs: Elements;
  readonly body: Node;
  readonly scope: Scope;
  readonly i: number;
  /** `map`'s results so far, owned by this run alone. */
  readonly acc: JValue[];
  /** `filter`'s kept positions so far, owned by this run alone. */
  readonly picked: number[];
  readonly n: number;
}

/** A `some` in progress: its quorum, the element `i`, and how many bodies were truthy. */
interface Quorum {
  readonly vs: Elements;
  readonly body: Node;
  readonly scope: Scope;
  readonly min: number;
  readonly i: number;
  readonly n: number;
}

/** What `eval` does with the value: each is the reference's `CanonicalValue => Step` at the cited line. */
type Kont =
  /** `eval(n, s, done(_))`: the value is the result. */
  | { readonly t: "return" }
  /** `Machine.scala:122-141` */
  | { readonly t: "readAt"; readonly default: Value | undefined; readonly scope: Scope }
  /** `Machine.scala:107` */
  | { readonly t: "all"; readonly args: readonly Node[]; readonly i: number; readonly acc: JValue[]; readonly scope: Scope; readonly then: AllThen }
  /** `Machine.scala:166-169` */
  | { readonly t: "spread"; readonly op: VariadicOp }
  /** `Machine.scala:335` */
  | { readonly t: "condTest"; readonly branches: readonly (readonly [Node, Node])[]; readonly j: number; readonly otherwise: Node; readonly scope: Scope }
  /** `Machine.scala:351-358` */
  | { readonly t: "logic"; readonly kind: Logical; readonly args: readonly Node[]; readonly i: number; readonly scope: Scope }
  /** `Machine.scala:363`: binding `i`'s value. */
  | { readonly t: "bindValue"; readonly node: LetBindings; readonly body: Node; readonly slot: number; readonly i: number; readonly outer: Scope }
  /** The `let` body's value: its bindings go out of scope. */
  | { readonly t: "bindDone" }
  /** `Machine.scala:374-378` */
  | { readonly t: "collection"; readonly op: "map" | "filter" | "all" | "none" | "find" | "count" | "some" | "reduce"; readonly then: CollThen }
  /** `Machine.scala:197-200` */
  | { readonly t: "quorumMin"; readonly vs: Elements; readonly body: Node; readonly scope: Scope }
  /** `Machine.scala:212` */
  | { readonly t: "foldInit"; readonly vs: Elements; readonly body: Node; readonly scope: Scope }
  /** `Machine.scala:403-414` */
  | { readonly t: "iterBody"; readonly it: Iterating }
  /** `Machine.scala:427` */
  | { readonly t: "quorumBody"; readonly q: Quorum }
  /** `Machine.scala:438` */
  | { readonly t: "foldBody"; readonly vs: Elements; readonly body: Node; readonly scope: Scope; readonly i: number };

/** One step of evaluation (`Machine.scala` `Step`). */
type Step =
  | { readonly t: "done"; readonly value: JValue }
  | { readonly t: "fail"; readonly error: EvalError }
  /** Pay, then do the work: nothing is done before it is paid for. */
  | { readonly t: "charge"; readonly gas: number; readonly then: Then }
  /** Evaluate the node, then continue with its value. */
  | { readonly t: "eval"; readonly node: Node; readonly scope: Scope; readonly k: Kont }
  /** Record a warning (free), then finish with the value. */
  | { readonly t: "note"; readonly warning: Warning; readonly value: JValue };

const done = (value: JValue): Step => ({ t: "done", value });
const fail = (error: EvalError): Step => ({ t: "fail", error });
const charge = (gas: number, then: Then): Step => ({ t: "charge", gas, then });
const evaluate = (node: Node, scope: Scope, k: Kont): Step => ({ t: "eval", node, scope, k });
const RETURN: Kont = { t: "return" };

function result(r: Out | { readonly ok: false; readonly error: EvalError }): Step {
  return r.ok ? done(r.value) : fail(r.error);
}

// ------------------------------------------------------------------------------------------------ the machine

/** What an evaluation ends with: the gas used, the value or error, and the warnings. */
export interface Ran {
  readonly used: number;
  readonly result: { readonly ok: true; readonly value: Value } | { readonly ok: false; readonly error: EvalError };
  readonly warnings: readonly Warning[];
  /** The steps the driver took: a deterministic count of the work done. */
  readonly steps: number;
}

/**
 * One evaluation's state: the inputs, the meter, and the values of the `let`s being evaluated (a stack: a `let`'s
 * bindings go out of scope when its body returns, and evaluation is depth-first).
 */
class Machine {
  readonly #slots: JValue[][] = [];
  readonly #warnings: Warning[] = [];
  readonly #seen: string[] = [];
  readonly #heap = new Heap();
  #used = 0;

  constructor(
    private readonly schedule: GasSchedule,
    private readonly data: Value,
    private readonly limit: number,
  ) {}

  /** The direct driver (`Machine.direct`): the step, an explicit stack of continuations, and the meter. */
  run(root: Node, env: HostEnv | undefined): Ran {
    const ks: Kont[] = [];
    let step = this.start(root, { bindings: { t: "host", env }, frames: undefined, frameCount: 0 });
    for (let steps = 1; ; steps++) {
      switch (step.t) {
        case "done": {
          const k = ks.pop();
          if (k !== undefined) {
            step = this.resume(k, step.value);
            break;
          }
          const v = this.#heap.value(step.value);
          return this.ran(v === undefined ? { ok: false, error: limit("the result nests too deep") } : { ok: true, value: v }, steps);
        }
        case "fail":
          return this.ran({ ok: false, error: step.error }, steps);
        case "charge": {
          const total = plus(this.#used, step.gas);
          if (total > this.limit) {
            this.#used = this.limit;
            return this.ran({ ok: false, error: { kind: "OutOfGas", required: total, available: this.limit } }, steps);
          }
          this.#used = total;
          step = this.after(step.then);
          break;
        }
        case "eval":
          if (step.k.t !== "return") ks.push(step.k);
          step = this.start(step.node, step.scope);
          break;
        case "note":
          this.warn(step.warning);
          step = done(step.value);
          break;
      }
    }
  }

  private ran(result: Ran["result"], steps: number): Ran {
    return { used: this.#used, result, warnings: [...this.#warnings], steps };
  }

  private warn(w: Warning): void {
    const key = w.path.toString();
    if (this.#warnings.length < MAX_WARNINGS && !this.#seen.includes(key)) {
      this.#warnings.push(w);
      this.#seen.push(key);
    }
  }

  /** The gas left: what a charge can still be paid from. */
  private left(): number {
    return Math.max(this.limit - this.#used, 0);
  }

  /** `Semantics.start` (`Machine.scala:111-237`). */
  private start(node: Node, scope: Scope): Step {
    const s = this.schedule;
    switch (node.t) {
      case "const":
        return charge(s.literal, { t: "done", value: node.value });
      case "read":
        return charge(plus(this.readCost(node.all), this.frameCost(scope, node.first)), { t: "lookup", ref: node.ref, default: node.default, scope });
      case "readAt":
        return evaluate(node.path, scope, { t: "readAt", default: node.default, scope });
      case "arrayOf":
        return charge(plus(s.literal, times(s.element, node.items.length)), { t: "all", args: node.items, scope, then: { t: "array" } });
      case "objectOf":
        return charge(plus(s.literal, times(s.element, node.fields.length)), { t: "all", args: node.fields, scope, then: { t: "object", keys: node.keys, keyBytes: node.keyBytes } });
      case "strict":
        return charge(base(s, node.op), { t: "all", args: node.args, scope, then: { t: "strict", op: node.op } });
      case "spread":
        return charge(base(s, node.op), { t: "spreadArg", op: node.op, arg: node.arg, scope });
      case "cond":
        return charge(base(s, "if"), { t: "cond", branches: node.branches, otherwise: node.otherwise, scope });
      case "logic":
        return charge(base(s, node.kind), { t: "logic", kind: node.kind, args: node.args, scope });
      // each binding is an insertion into the scope, paid like a field of an object literal, and like a path segment
      // once more per KeyBlock of its name
      case "bind":
        return charge(plus(base(s, "let"), times(s.element, node.bindings.nameSegments)), { t: "bind", node: node.bindings, body: node.body, scope });
      case "each":
        return charge(base(s, node.kind), { t: "collection", op: node.kind, over: node.over, then: { t: "each", kind: node.kind, body: node.body, scope } });
      case "quorum":
        return charge(base(s, "some"), { t: "collection", op: "some", over: node.over, then: { t: "quorum", body: node.body, min: node.min, scope } });
      case "fold":
        return charge(base(s, "reduce"), { t: "collection", op: "reduce", over: node.over, then: { t: "fold", body: node.body, init: node.init, scope } });
      case "lookup":
        return charge(base(s, node.op), { t: "all", args: node.args, scope, then: { t: "lookup", op: node.op } });
      case "crypto":
        return charge(base(s, node.op), { t: "all", args: node.args, scope, then: { t: "crypto", op: node.op } });
    }
  }

  /** The work after a charge. */
  private after(then: Then): Step {
    switch (then.t) {
      case "done":
        return done(then.value);
      case "lookup":
        return this.lookup(then.ref, then.default, then.scope);
      case "parsePath": {
        const r = parseVar(then.text);
        return r.ok ? this.read(r.value, then.default, then.scope) : fail(domain("var", r.error.kind === "Malformed" ? r.error.message : "a malformed path"));
      }
      case "segmentPath": {
        const p: string[] = [];
        for (const v of then.segments) {
          const seg = segment(v);
          if (!seg.ok) return fail(seg.error);
          p.push(seg.value);
        }
        return this.lookup({ bound: false, path: p }, then.default, then.scope);
      }
      case "all":
        return this.all(then.args, 0, [], then.scope, then.then);
      case "operate":
        return this.operate(then.op, then.vs);
      case "spreadArg":
        return evaluate(then.arg, then.scope, { t: "spread", op: then.op });
      case "cond":
        return this.cond(then.branches, 0, then.otherwise, then.scope);
      case "condTest": {
        const b = then.branches[then.j];
        return b === undefined ? evaluate(then.otherwise, then.scope, RETURN) : evaluate(b[0], then.scope, { t: "condTest", branches: then.branches, j: then.j, otherwise: then.otherwise, scope: then.scope });
      }
      case "logic":
        return this.logic(then.kind, then.args, 0, then.scope);
      case "bind": {
        const slot = this.#slots.length;
        this.#slots.push([]);
        return this.bind(then.node, then.body, slot, 0, then.scope);
      }
      case "collection":
        return evaluate(then.over, then.then.scope, { t: "collection", op: then.op, then: then.then });
      case "proceed":
        return this.proceed(then.then, then.vs);
      case "iterRun": {
        const it = then.it;
        const inner = element(it.scope, it.vs, it.i);
        return inner.ok ? evaluate(it.body, inner.value, { t: "iterBody", it }) : fail(inner.error);
      }
      case "quorumRun": {
        const q = then.q;
        const inner = element(q.scope, q.vs, q.i);
        return inner.ok ? evaluate(q.body, inner.value, { t: "quorumBody", q }) : fail(inner.error);
      }
      case "foldRun": {
        const inner = accumulating(then.scope, then.vs, then.i, then.acc);
        return inner.ok ? evaluate(then.body, inner.value, { t: "foldBody", vs: then.vs, body: then.body, scope: then.scope, i: then.i }) : fail(inner.error);
      }
      case "accumulate":
        return this.combine(then.op, then.rest, then.i, then.acc);
      case "joinPieces": {
        const j = ops.joining(then.vs);
        return j.ok ? charge(times(perUnit(this.schedule, "join"), ops.separatorBytes(j.value)), { t: "joinBuild", j: j.value }) : fail(j.error);
      }
      case "joinBuild":
        return result(ops.joined(then.j));
      case "cat":
        return result(ops.cat(then.vs));
      case "lookupRun":
        return result(lookups(then.op, then.vs, (p) => resolve(this.data, then.scope, p)[0].kind === "null"));
      case "cryptoRun":
        return result(CRYPTO[then.op](then.vs, this.#heap));
    }
  }

  /** What a continuation does with a value. */
  private resume(k: Kont, v: JValue): Step {
    switch (k.t) {
      case "return":
        return done(v);
      case "readAt": {
        if (v.kind === "text") return charge(pathText(this.schedule, v), { t: "parsePath", text: v.value, default: k.default, scope: k.scope });
        // each segment's count is O(1), and the segments are counted only as far as the gas left can pay for
        if (v.kind === "array") {
          const items = v.items;
          const [first] = items;
          let g = this.readCost(0);
          if (first !== undefined) {
            const left = this.left();
            g = plus(g, this.frameCost(k.scope, pathSegments(first)));
            for (const s of items) {
              g = plus(g, times(this.schedule.perPathSegment, pathSegments(s)));
              if (g > left) break;
            }
          }
          return charge(g, { t: "segmentPath", segments: items, default: k.default, scope: k.scope });
        }
        return fail(typeError("var", `a computed path is text or an array of segments, got ${kindName(v)}`));
      }
      case "all":
        k.acc.push(v);
        return this.all(k.args, k.i + 1, k.acc, k.scope, k.then);
      // the elements are sized where they are, and taken out of the array only once that is paid
      case "spread": {
        if (v.kind !== "array") return fail(typeError(k.op, `a spread argument is an array, got ${kindName(v)}`));
        // each element is an argument: it pays `element` first, so sizing a long spread is paid for
        const per = times(this.schedule.element, v.length);
        const left = this.left();
        const g = per > left ? per : plus(per, sizedWithin(this.schedule, k.op, v.items, Math.max(left - per, 0)));
        return charge(g, { t: "operate", op: k.op, vs: v.items });
      }
      case "condTest": {
        if (!truthy(v)) return this.cond(k.branches, k.j + 1, k.otherwise, k.scope);
        const b = k.branches[k.j];
        return evaluate(b === undefined ? k.otherwise : b[1], k.scope, RETURN);
      }
      case "logic": {
        const decisive = k.kind === "and" ? !truthy(v) : truthy(v);
        const last = k.i + 1 === k.args.length;
        return decisive || (last && k.kind !== "default") ? done(v) : this.logic(k.kind, k.args, k.i + 1, k.scope);
      }
      case "bindValue":
        this.#slots[k.slot]?.push(v);
        return this.bind(k.node, k.body, k.slot, k.i + 1, k.outer);
      case "bindDone":
        this.#slots.pop();
        return done(v);
      case "collection":
        if (v.kind === "array") return charge(times(perUnit(this.schedule, k.op), v.length), { t: "proceed", then: k.then, vs: v.items });
        if (v.kind === "null") return this.proceed(k.then, []);
        return fail(typeError(k.op, `expected an array, got ${kindName(v)}`));
      case "quorumMin": {
        const n = integer("some", v);
        if (!n.ok) return fail(n.error);
        const m = n.value.value;
        const min = m < 0n ? 0 : m > BigInt(Number.MAX_SAFE_INTEGER) ? Number.MAX_SAFE_INTEGER : Number(m);
        return this.quorum({ vs: k.vs, body: k.body, scope: k.scope, min, i: 0, n: 0 });
      }
      case "foldInit":
        return this.fold(k.vs, k.body, k.scope, 0, v);
      case "iterBody":
        return this.iterated(k.it, v);
      case "quorumBody": {
        const q = k.q;
        return this.quorum({ ...q, i: q.i + 1, n: truthy(v) ? q.n + 1 : q.n });
      }
      case "foldBody":
        return this.fold(k.vs, k.body, k.scope, k.i + 1, v);
    }
  }

  // ---- variables

  /** The access, and each segment. */
  private readCost(segments: number): number {
    return plus(this.schedule.varAccess, times(this.schedule.perPathSegment, segments));
  }

  /**
   * A plain path inside callbacks may consult every enclosing frame, and each compares its first key with the
   * frame's element: O(the key's length) where keys share a prefix. So each frame pays the first segment's count.
   */
  private frameCost(scope: Scope, firstSegment: number): number {
    return times(times(this.schedule.perPathSegment, scope.frameCount), firstSegment);
  }

  /** Charges for the access, each segment and each frame searched, then reads (`Machine.scala:249-257`). */
  private read(ref: VarRef, d: Value | undefined, scope: Scope): Step {
    const counted = (p: readonly string[]): number => p.reduce((n, s) => plus(n, segmentCount(s)), 0);
    const g = ref.bound
      ? this.readCost(plus(segmentCount(ref.name), counted(ref.path)))
      : plus(this.readCost(counted(ref.path)), this.frameCost(scope, ref.path[0] === undefined ? 0 : segmentCount(ref.path[0])));
    return charge(g, { t: "lookup", ref, default: d, scope });
  }

  /** Reads a path already paid for (`Machine.scala:260-272`). */
  private lookup(ref: VarRef, d: Value | undefined, scope: Scope): Step {
    const orDefault = (v: JValue): JValue => (v.kind === "null" ? (d ?? NULL) : v);
    if (!ref.bound) {
      const [v, ambiguous] = resolve(this.data, scope, ref.path);
      const value = orDefault(v);
      return ambiguous ? { t: "note", warning: { kind: "AmbiguousPath", path: Preview.of(ref.path.join(".")) }, value } : done(value);
    }
    const v = binding(this.#slots, scope, ref.name);
    if (v !== undefined) return done(orDefault(at(v, ref.path)));
    return d !== undefined ? done(d) : fail(malformed(`the binding ${Preview.of(ref.name).toString()} is not bound`));
  }

  // ---- strict operations

  /** `Machine.scala:105-109`: evaluates the arguments from `i` on, then continues with all their values. */
  private all(args: readonly Node[], i: number, acc: JValue[], scope: Scope, then: AllThen): Step {
    const n = args[i];
    return n === undefined ? this.allDone(then, acc, scope) : evaluate(n, scope, { t: "all", args, i, acc, scope, then });
  }

  private allDone(then: AllThen, vs: JValue[], scope: Scope): Step {
    switch (then.t) {
      case "array":
        return result(array("array", vs));
      case "object": {
        const m = JMap.sorted(
          then.keys.map((k, i) => [k, vs[i] ?? NULL] as const),
          then.keyBytes,
        );
        return result(mapResult("object", m));
      }
      case "strict":
        return charge(sized(this.schedule, then.op, vs), { t: "operate", op: then.op, vs });
      // each key is a path parsed at run time, and searches every enclosing frame: both paid before resolving
      case "lookup": {
        // each key's charge is O(1), and the keys are priced only as far as the gas left can pay for; a key's whole
        // text bounds its first segment, which every frame compares: priced without parsing it
        const left = this.left();
        let g = sized(this.schedule, then.op, vs);
        for (const k of lookupPaths(then.op, vs)) {
          g = plus(plus(g, pathText(this.schedule, k)), this.frameCost(scope, pathSegments(k)));
          if (g > left) break;
        }
        return charge(g, { t: "lookupRun", op: then.op, vs, scope });
      }
      case "crypto":
        return charge(sized(this.schedule, then.op, vs), { t: "cryptoRun", op: then.op, vs });
    }
  }

  /**
   * An operation after its size charge (`Machine.scala:292-312`): `+` and `*` fold in charged steps; `cat` and
   * `join` pay for rendering numbers before they render them, and `join` for its separators before building.
   */
  private operate(op: StrictOp, vs: Elements): Step {
    if (op === "+" || op === "*") {
      const rs = ops.operands(op, vs);
      if (!rs.ok) return fail(rs.error);
      const [first, ...rest] = rs.value;
      return first === undefined ? result(ops.identity(op)) : this.accumulate(op, rest, 0, first);
    }
    if (op === "join") return charge(renderCharge(this.schedule, ops.rendered(op, vs)), { t: "joinPieces", vs });
    if (op === "cat") return charge(renderCharge(this.schedule, ops.rendered(op, vs)), { t: "cat", vs });
    return result(ops.STRICT[op](vs, this.#heap));
  }

  /** `+` or `*`, left to right (`Machine.scala:322-327`): each step after the first pays the size charge of its accumulator before it runs. */
  private accumulate(op: "+" | "*", rest: readonly Num[], i: number, acc: Num): Step {
    if (i >= rest.length) return result(num(op, acc));
    if (i === 0) return this.combine(op, rest, i, acc);
    // the accumulator was checked against the number bound when it was made, so this is its value
    return charge(sized(this.schedule, op, [acc]), { t: "accumulate", op, rest, i, acc });
  }

  private combine(op: "+" | "*", rest: readonly Num[], i: number, acc: Num): Step {
    const x = rest[i];
    if (x === undefined) return result(num(op, acc));
    const r = ops.combine(op, acc, x);
    return r.ok ? this.accumulate(op, rest, i + 1, r.value) : fail(r.error);
  }

  // ---- lazy forms

  /** Each condition tested is charged one element (`Machine.scala:332-337`). */
  private cond(branches: readonly (readonly [Node, Node])[], j: number, otherwise: Node, scope: Scope): Step {
    return j < branches.length ? charge(this.schedule.element, { t: "condTest", branches, j, otherwise, scope }) : evaluate(otherwise, scope, RETURN);
  }

  /** `and` stops at the first falsy value, `or` and `default` at the first truthy one (`Machine.scala:340-359`). */
  private logic(kind: Logical, args: readonly Node[], i: number, scope: Scope): Step {
    const n = args[i];
    if (n === undefined) return done(kind === "and" ? TRUE : kind === "or" ? FALSE : NULL);
    return evaluate(n, scope, { t: "logic", kind, args, i, scope });
  }

  /** Binding `i`, seeing the ones before it; then the body, seeing all (`Machine.scala:361-365`). */
  private bind(node: LetBindings, body: Node, slot: number, i: number, outer: Scope): Step {
    const scope: Scope = { bindings: { t: "let", parent: outer.bindings, node, slot, count: i }, frames: outer.frames, frameCount: outer.frameCount };
    const b = node.bindings[i];
    return b === undefined ? evaluate(body, scope, { t: "bindDone" }) : evaluate(b[1], scope, { t: "bindValue", node, body, slot, i, outer });
  }

  // ---- callbacks

  /** The callback form, once its collection is evaluated and paid for. */
  private proceed(then: CollThen, vs: Elements): Step {
    switch (then.t) {
      case "each":
        return this.iterate({ kind: then.kind, vs, body: then.body, scope: then.scope, i: 0, acc: [], picked: [], n: 0 });
      case "quorum":
        return then.min === undefined ? this.quorum({ vs, body: then.body, scope: then.scope, min: 1, i: 0, n: 0 }) : evaluate(then.min, then.scope, { t: "quorumMin", vs, body: then.body, scope: then.scope });
      case "fold": {
        if (then.init !== undefined) return evaluate(then.init, then.scope, { t: "foldInit", vs, body: then.body, scope: then.scope });
        const [first] = vs;
        return first === undefined ? done(NULL) : this.fold(vs, then.body, then.scope, 1, first);
      }
    }
  }

  /** One body's value, `Machine.scala:403-414`: kept, counted, or deciding the result. */
  private iterated(it: Iterating, v: JValue): Step {
    const t = truthy(v);
    const next = { ...it, i: it.i + 1 };
    switch (it.kind) {
      case "map":
        it.acc.push(v);
        return this.iterate(next);
      case "filter":
        if (t) it.picked.push(it.i);
        return this.iterate(next);
      case "all":
        return t ? this.iterate(next) : done(FALSE);
      case "none":
        return t ? done(FALSE) : this.iterate(next);
      case "find":
        return t ? done(it.vs[it.i] ?? NULL) : this.iterate(next);
      case "count":
        return this.iterate(t ? { ...next, n: it.n + 1 } : next);
    }
  }

  private iterate(it: Iterating): Step {
    if (it.i < it.vs.length) return charge(this.schedule.callbackRun, { t: "iterRun", it });
    switch (it.kind) {
      case "map":
        return result(array("map", it.acc));
      case "filter":
        return result(array("filter", it.picked.map((j) => it.vs[j] ?? NULL)));
      case "all":
        return done(bool(it.vs.length > 0));
      case "none":
        return done(TRUE);
      case "find":
        return done(NULL);
      case "count":
        return result(count(it.n));
    }
  }

  /** Stops as soon as `min` elements satisfy the body (`Machine.scala:421-428`). */
  private quorum(q: Quorum): Step {
    if (q.n >= q.min) return done(TRUE);
    if (q.i >= q.vs.length) return done(FALSE);
    return charge(this.schedule.callbackRun, { t: "quorumRun", q });
  }

  /** `Machine.scala:433-439` */
  private fold(vs: Elements, body: Node, scope: Scope, i: number, acc: JValue): Step {
    return i < vs.length ? charge(this.schedule.callbackRun, { t: "foldRun", vs, body, scope, i, acc }) : done(acc);
  }
}

/** The element is bound as `$current` and is the innermost frame for plain paths (`Machine.scala:382-383`). */
function element(scope: Scope, vs: Elements, i: number): Out<Scope> {
  const current = vs[i] ?? NULL;
  const index = count(i);
  if (!index.ok) return index;
  return { ok: true, value: enter(scope, { t: "element", parent: scope.bindings, current, index: index.value }, { t: "element", value: current }) };
}

/** `Machine.scala:430-431` */
function accumulating(scope: Scope, vs: Elements, i: number, acc: JValue): Out<Scope> {
  const current = vs[i] ?? NULL;
  const index = count(i);
  if (!index.ok) return index;
  return {
    ok: true,
    value: enter(scope, { t: "accumulating", parent: scope.bindings, current, index: index.value, accumulator: acc }, { t: "accumulating", current, accumulator: acc }),
  };
}

/**
 * A segment of a computed data path (`Machine.scala:278-284`): text is an exact key; an integer is an index, or the
 * key that is its decimal form, bounded to 64 bits before it is rendered.
 */
function segment(v: JValue): Out<string> {
  if (v.kind === "text") return { ok: true, value: v.value };
  if (v.kind === "int") {
    const t = smallIntegerText(v.value);
    return t === undefined ? bad(domain("var", "an integer path segment must fit in 64 bits")) : { ok: true, value: t };
  }
  return bad(typeError("var", `a path segment is text or an integer, got ${kindName(v)}`));
}

// ---- lookups (Lookups.scala)

/** The values a lookup reads as paths, unchecked: what its charge is computed from, before any is parsed. */
function lookupPaths(op: "missing" | "missing_some", args: Elements): Elements {
  const [a, b] = args;
  if (op === "missing" && args.length === 1 && a?.kind === "array") return a.items;
  if (op === "missing_some" && args.length === 2 && b?.kind === "array") return b.items;
  return op === "missing" ? args : [];
}

/** `missing` and `missing_some`: which data paths are absent, as `absent` says. */
function lookups(op: "missing" | "missing_some", args: Elements, absent: (path: readonly string[]) => boolean): Out {
  // the keys are kept as the values they are: only the result is bounded, once it is chosen (an absent key is never
  // bounded on its own, as `missing_some` may discard it)
  const keys = (ks: Elements): Out<readonly TextValue[]> => {
    const [only] = ks;
    const list = ks.length === 1 && only?.kind === "array" ? only.items : ks;
    const out: TextValue[] = [];
    for (const k of list) {
      if (k.kind !== "text") return bad(typeError(op, `a key is a string path, got ${kindName(k)}`));
      out.push(k);
    }
    return { ok: true, value: out };
  };
  const missing = (ks: readonly TextValue[]): readonly JValue[] => ks.filter((k) => absent(k.value.length === 0 ? [] : splitPath(k.value)));
  if (op === "missing") {
    const ks = keys(args);
    if (!ks.ok) return ks;
    return array(op, missing(ks.value));
  }
  const [min, ks] = args;
  if (args.length !== 2 || min === undefined || ks?.kind !== "array") return bad(arity(op, "takes [minimum, [keys]]"));
  const n = integer(op, min);
  if (!n.ok) return n;
  const all = keys([ks]);
  if (!all.ok) return all;
  const gone = missing(all.value);
  const present = all.value.length - gone.length;
  return array(op, BigInt(present) >= n.value.value ? [] : gone);
}

/** Evaluates a compiled program (`Machine.run`): the gas used, the value or error, and the warnings. */
export function run(schedule: GasSchedule, root: Node, data: Value, env: HostEnv | undefined, gasLimit: number): Ran {
  return new Machine(schedule, data, gasLimit).run(root, env);
}
