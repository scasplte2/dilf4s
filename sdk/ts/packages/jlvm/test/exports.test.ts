/**
 * No export of this package is state a caller can change: every exported list, table and schedule is frozen, as are
 * the exported classes and their prototypes, and every lookup reads module-private data. Mutation attempts, strict and
 * sloppy, change no parse, canonical decoding, classification or evaluation; nor does changing the generated codec's
 * own list of operations, which is core's.
 */
import { models, Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import * as jlvm from "../src/index.js";
import { expectOk } from "./vectors.js";

/** Every object an export holds, the export included: what must be frozen. */
function reachable(root: object): object[] {
  const seen = new Set<object>();
  const todo: object[] = [root];
  for (let x = todo.pop(); x !== undefined; x = todo.pop()) {
    if (seen.has(x)) continue;
    seen.add(x);
    for (const k of Reflect.ownKeys(x)) {
      const d = Object.getOwnPropertyDescriptor(x, k);
      const v: unknown = d?.value;
      if ((typeof v === "object" && v !== null) || typeof v === "function") todo.push(v);
    }
  }
  return [...seen];
}

/** What the package does, observed: a parse, a canonical decoding, a classification and an evaluation. */
function behaviour(): string {
  const parsed = jlvm.parse('{"evil": [1]}');
  const form = expectOk(models.jlvm.JlvmProgram.encode({ jlvm: 1n, expr: { op: "+", args: [] } }), "a form");
  const evil = expectOk(Value.map([["jlvm", expectOk(Value.int(1n), "1")], ["expr", expectOk(Value.map([["op", expectOk(Value.text("evil"), "evil")], ["args", expectOk(Value.array([]), "[]")]]), "an apply")]]), "a form");
  const p = expectOk(jlvm.parse('{"+": [1, 2]}'), "a program");
  const o = expectOk(jlvm.evaluate(p, Value.null), "evaluates");
  return JSON.stringify({
    parsed: parsed.ok ? "parsed" : parsed.error.kind,
    decoded: jlvm.fromCanonical(form).ok,
    evil: ((r): string => (r.ok ? "decoded" : r.error.kind))(jlvm.fromCanonical(evil)),
    classes: ["+", "map", "sha256", "sigma_verify", "evil"].map((op) => [jlvm.isForm(op as never), jlvm.isStrict(op as never), jlvm.isCrypto(op as never), jlvm.opFromWire(op)]),
    all: jlvm.ALL_OPS.length,
    outcome: [o.gasUsed, o.result.ok ? o.result.value.kind : "error"],
  });
}

describe("the package's exports", () => {
  it("are frozen throughout: lists, tables, the schedule, the classes and their prototypes", () => {
    const exported: unknown[] = Object.values(jlvm);
    const objects = exported.flatMap((e): object[] => ((typeof e === "object" && e !== null) || typeof e === "function" ? reachable(e) : []));
    expect(objects.length).toBeGreaterThan(100);
    const mutable = objects.filter((o) => !Object.isFrozen(o));
    expect(mutable.map((o) => (Array.isArray(o) ? `array of ${o.length.toString()}` : `${Object.prototype.toString.call(o)} ${Reflect.ownKeys(o).map(String).slice(0, 4).join(",")} ${String((o as { constructor?: { name?: string } }).constructor?.name)}`))).toEqual([]);
    for (const c of [jlvm.Program, jlvm.Compiled, jlvm.Bindings]) {
      expect(Object.isFrozen(c), c.name).toBe(true);
      expect(Object.isFrozen(c.prototype), `${c.name}.prototype`).toBe(true);
    }
    // never the generated codec's own array
    const shape = models.jlvm.JlvmOp.shape;
    expect(shape.kind === "union" && shape.texts === (jlvm.ALL_OPS as readonly string[])).toBe(false);
  });

  it("cannot be changed by push, index assignment, length = 0 or defineProperty, strict or sloppy", () => {
    const before = behaviour();
    const lists: unknown[][] = [jlvm.ALL_OPS, jlvm.FORM_OPS, jlvm.CRYPTO_OPS, jlvm.SCHEDULE_V1.costs["+"].extra].map((l) => l as unknown[]);
    for (const l of lists) {
      expect(() => l.push("evil")).toThrow(TypeError);
      expect(() => {
        l[0] = "evil";
      }).toThrow(TypeError);
      expect(() => {
        l.length = 0;
      }).toThrow(TypeError);
      expect(() => Object.defineProperty(l, 0, { value: "evil" })).toThrow(TypeError);
      // eslint-disable-next-line @typescript-eslint/no-implied-eval -- sloppy-mode code, which only a Function body is
      (new Function("l", "l.push = 1; l[0] = 'evil'; l[l.length] = 'evil'; l.length = 0;") as (l: unknown) => void)(l);
    }
    for (const c of [jlvm.Program, jlvm.Compiled, jlvm.Bindings]) {
      expect(() => Object.defineProperty(c.prototype, "evil", { value: 1 })).toThrow(TypeError);
      // eslint-disable-next-line @typescript-eslint/no-implied-eval -- sloppy-mode code
      (new Function("c", "c.prototype.get = null; c.prototype.encode = null; c.none = null;") as (c: unknown) => void)(c);
    }
    expect(jlvm.Bindings.none.size).toBe(0);
    expect(behaviour()).toBe(before);
  });

  it("hold when the generated codec's own list of operations is changed: an unknown operation is still no program", () => {
    const shape = models.jlvm.JlvmOp.shape;
    if (shape.kind !== "union") throw new Error("a union");
    const texts = shape.texts as string[];
    const n = texts.length;
    const before = behaviour();
    // core's own state: frozen once the generator freezes it, and then nothing changes
    const changed = Reflect.set(texts, n, "evil");
    try {
      const after = JSON.parse(behaviour()) as { evil: string; all: number };
      expect(after.evil, "an unknown operation the codec now accepts is still refused, as Malformed").toBe("Malformed");
      expect(after.all, "the package's own list is a copy").toBe(jlvm.ALL_OPS.length);
    } finally {
      if (changed) Reflect.set(texts, "length", n);
    }
    expect(behaviour()).toBe(before);
  });
});

describe("the exported predicates", () => {
  it("hold for own wire names alone: an alias, an unknown name, a non-string or a Proxy is none, never read", () => {
    const revoked = Proxy.revocable({}, {});
    revoked.revoke();
    const reads: PropertyKey[] = [];
    const watching = new Proxy(new String("+"), { get: (t, k, r) => (reads.push(k), Reflect.get(t, k, r) as unknown) });
    const invalid: unknown[] = ["getKey", "size", "evil", "", "var", "noop", undefined, null, 5, 5n, true, {}, [], ["+"], Symbol("+"), new String("+"), revoked.proxy, watching];
    for (const [i, x] of invalid.entries()) {
      const v = x as never;
      expect([jlvm.isStrict(v), jlvm.isForm(v), jlvm.isCrypto(v), jlvm.isVariadic(v)], `case ${i.toString()}`).toEqual([false, false, false, false]);
      expect(jlvm.opFromWire(v), `case ${i.toString()}`).toBeUndefined();
      if (i >= 4) expect(jlvm.opNamed(v), `case ${i.toString()}`).toBeUndefined();
      expect(jlvm.isBindings(v), `case ${i.toString()}`).toBe(false);
    }
    expect(reads, "a Proxy is never read").toEqual([]);
    // the aliases name their operations, but are no wire name, so no class holds them
    expect(jlvm.opNamed("getKey")).toBe("get");
    // each own name is in exactly one class
    for (const op of jlvm.ALL_OPS) expect([jlvm.isStrict(op), jlvm.isForm(op), jlvm.isCrypto(op)].filter(Boolean), op).toHaveLength(1);
    expect([jlvm.isStrict("+"), jlvm.isForm("map"), jlvm.isCrypto("sigma_verify"), jlvm.isVariadic("cat"), jlvm.isStrict("count")]).toEqual([true, true, true, true, true]);
  });
});
