/**
 * The machine updates a core map without copying it: `set`, `unset` and `merge` over a host map of any size touch only
 * the entries they set or remove, as gas charges them (a search per entry), and the map's size, weight and depth stay
 * exact. Counted, never timed: the core map's entries are never listed by an update.
 */
import { ArrayValue, MapValue, models, Value, type Value as V } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { Bindings, Compiled, compile, evaluate, fromCanonical, parse, Program } from "../src/index.js";
import { Heap } from "../src/jvalue.js";
import { expectErr, expectOk } from "./vectors.js";

const int = (n: number): V => expectOk(Value.int(BigInt(n)), "an integer");
const text = (s: string): V => expectOk(Value.text(s), "text");
const arr = (vs: readonly V[]): V => expectOk(ArrayValue.of(vs), "an array");
const map = (es: readonly (readonly [string, V])[]): MapValue => expectOk(MapValue.of(es), "a map");

/**
 * Runs `f` counting how often a core map's entries are listed. Core's classes are frozen, so `MapValue.prototype` cannot
 * be patched; listing a map (`MapValue.entries`) maps its frozen array of entries (`{key, utf8, value}`), the only such
 * arrays there are, so `Array.prototype.map` over one counts as a listing.
 */
function listings<T>(f: () => T): readonly [T, number] {
  let calls = 0;
  const own = Object.getOwnPropertyDescriptor(Array.prototype, "map");
  const original = Array.prototype.map;
  const isEntries = (a: readonly unknown[]): boolean => {
    const first: unknown = a[0];
    return Object.isFrozen(a) && typeof first === "object" && first !== null && "utf8" in first && "key" in first && "value" in first;
  };
  Object.defineProperty(Array.prototype, "map", {
    configurable: true,
    writable: true,
    value: function (this: unknown[], ...args: Parameters<typeof original>) {
      if (isEntries(this)) calls++;
      return original.apply(this, args);
    },
  });
  try {
    return [f(), calls];
  } finally {
    if (own !== undefined) Object.defineProperty(Array.prototype, "map", own);
  }
}

describe("updating a core map", { timeout: 120_000 }, () => {
  const big = map(Array.from({ length: 100_000 }, (_, i) => [`k${i.toString()}`, int(i)] as const));
  const nested = map(Array.from({ length: 1000 }, (_, i) => [`k${i.toString()}`, arr([int(i)])] as const));
  const data = map([["m", big], ["n", nested], ["xs", arr(Array.from({ length: 500 }, (_, i) => int(i)))]]);

  it("lists none of its entries, for an absent key or a present one, flat or nested", () => {
    for (const p of [
      '{"!!": {"unset": [{"var": "m"}, "absent"]}}',
      '{"!!": {"unset": [{"var": "m"}, "k7"]}}',
      '{"!!": {"set": [{"var": "m"}, "k7", 1]}}',
      '{"has": [{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}, [1]]}, {"var": "m"}]}, "k3"]}',
      '{"has": [{"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "m"}]}, "k3"]}',
      '{"!!": {"merge": [{"var": "m"}, {"object": {"k1": 2, "new": 3}}]}}',
      '{"!!": {"set": [{"var": "n"}, "new", 1]}}',
    ]) {
      const program = expectOk(parse(p), p);
      const [o, calls] = listings(() => expectOk(evaluate(program, data), "evaluates"));
      expect(o.result.ok, p).toBe(true);
      expect(calls, `${p} lists the host map`).toBe(0);
    }
  });

  it("hides its deepest values with an exact depth, reading the host map's depths once, not once per update", () => {
    // every value of `n` is an array: hiding one may lower the map's depth, which the host map's count of values by
    // depth decides, found in one pass, once per evaluation, however many updates read it
    const p = expectOk(parse('{"length": {"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "n"}]}}'), "unsets");
    const [o, calls] = listings(() => expectOk(evaluate(p, data), "evaluates"));
    expect(o.result).toEqual({ ok: true, value: int(500) });
    expect(calls).toBe(1);
    // a host map one level within the limit, its deepest value hidden, nested twice: its exact depth lets the arrays be
    // built, from the host map's depths read once
    const deep = Array.from({ length: 254 }).reduce<V>((x) => arr([x]), int(0));
    const near = map([["a", deep], ["b", int(1)]]);
    const nested = expectOk(parse('{"!!": [[[{"unset": [{"var": "m"}, "a"]}], [{"unset": [{"var": "m"}, "a"]}]]]}'), "nests");
    const [n, reads] = listings(() => expectOk(evaluate(nested, map([["m", near]])), "evaluates"));
    expect(n.result.ok, JSON.stringify(n.result.ok ? null : n.result.error)).toBe(true);
    expect(reads).toBe(1);
    // and one that is truly too deep is still refused
    const tooDeep = expectOk(parse('[[{"unset": [{"var": "m"}, "b"]}]]'), "too deep");
    expect(expectOk(evaluate(tooDeep, map([["m", near]])), "evaluates").result).toMatchObject({ ok: false, error: { kind: "Limit" } });
  });

  it("does work linear in its updates around a hidden deep host value: n log n, never n^2", () => {
    // Codex round 10's case: a host map's deep value hidden, the map held in a seed, then n entries set beside it.
    // Every depth stays exact, so no update rescans the tree. Counted, never timed: each node the machine builds or
    // visits runs Math.max, so its calls bound the work
    const deep = Array.from({ length: 254 }).reduce<V>((x) => arr([x]), int(0));
    const host = map([["a", deep], ["b", int(1)]]);
    const p = expectOk(
      parse('{"length": {"reduce": [{"var": "$xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}, 1]}, {"object": {"seed": [{"unset": [{"var": ""}, "a"]}]}}]}}'),
      "the case",
    );
    const counted = (n: number): readonly [number, number] => {
      const env = expectOk(Bindings.of([["xs", arr(Array.from({ length: n }, (_, i) => int(i)))]]), "env");
      let calls = 0;
      const max = Math.max;
      Math.max = (...xs: number[]): number => {
        calls++;
        return max(...xs);
      };
      try {
        const o = expectOk(evaluate(p, host, { env }), "evaluates");
        expect(o.result).toEqual({ ok: true, value: int(n + 1) });
        return [calls, o.gasUsed];
      } finally {
        Math.max = max;
      }
    };
    const [[a, ga], [b, gb]] = [counted(2000), counted(4000)];
    // twice the updates, each O(log n): about twice the work and the gas, never four times
    expect(b / a, `${a.toString()} then ${b.toString()} calls`).toBeLessThan(2.4);
    expect(gb / ga).toBeLessThan(2.4);
    expect(b / gb, "work per unit of gas").toBeLessThan(2);
  });

  it("compacts a map once it hides more of its host map than shows through, so listing it is linear in what is left", () => {
    const heap = new Heap();
    const host = map(Array.from({ length: 10_001 }, (_, i) => [`k${i.toString()}`, int(i)] as const));
    let m = heap.own(host);
    const [, reads] = listings(() => {
      for (let i = 0; i < 10_000; i++) m = m.removed(`k${i.toString()}`);
    });
    expect(m.size).toBe(1);
    expect(Reflect.get(m, "base"), "no base once most of it is hidden").toBeUndefined();
    // the host map was listed once, to convert it, after half of it was hidden
    expect(reads).toBe(1);
    // every map derived from it is small: 100 fresh versions, each listed whole, list one or two entries each
    for (let i = 0; i < 100; i++) {
      const v = m.set("kept", int(i));
      expect(v?.entries().length).toBe(2);
      expect(Reflect.get(v ?? m, "base")).toBeUndefined();
    }
    // the conversion is the host map's, once: a second lineage from it replays its own steps only
    let other = heap.own(host);
    for (let i = 10_000; i > 0; i--) other = other.removed(`k${i.toString()}`);
    expect(other.size).toBe(1);
    expect(other.get("k0")).toEqual(int(0));
  });

  it("recounts a map whose host map's weight saturates, once an entry of it is hidden", () => {
    // [x, x] nested 63 deep: weight past 2^63, held in 63 arrays
    const huge = Array.from({ length: 63 }).reduce<V>((x) => arr([x, x]), int(1));
    const host = map([["huge", huge], ["small", int(1)]]);
    expect(host.weight).toBe(2n ** 63n - 1n);
    const set = expectOk(evaluate(expectOk(parse('{"set": [{"unset": [{"var": ""}, "huge"]}, "k", 1]}'), "p"), host), "evaluates");
    expect(set.result).toEqual({ ok: true, value: map([["k", int(1)], ["small", int(1)]]) });
    const length = expectOk(evaluate(expectOk(parse('{"length": {"unset": [{"var": ""}, "huge"]}}'), "p"), host), "evaluates");
    expect(length.result).toEqual({ ok: true, value: int(1) });
    expect(length.gasUsed).toBeLessThan(1000);
  });

  it("keeps the size, weight, depth and entries exact through any updates", () => {
    // a deterministic walk of sets and removals over a map whose values nest to different depths
    const deep = (d: number): V => Array.from({ length: d }).reduce<V>((x) => arr([x]), int(d));
    const base = map(Array.from({ length: 64 }, (_, i) => [`k${((i * 7) % 64).toString()}`, deep(i % 5)] as const));
    const heap = new Heap();
    let m = heap.own(base);
    let seed = 12345;
    const next = (n: number): number => {
      seed = (seed * 1103515245 + 12345) % 2 ** 31;
      return seed % n;
    };
    for (let step = 0; step < 2000; step++) {
      const key = `k${next(80).toString()}`;
      const op = next(3);
      const updated = op === 0 ? m.removed(key) : m.set(key, op === 1 ? deep(next(6)) : text("v".repeat(next(40))));
      if (updated === undefined) throw new Error("within the depth bound");
      m = updated;
      const core = heap.value(m);
      if (core?.kind !== "map") throw new Error("converts");
      expect(m.size, `step ${step.toString()}: size`).toBe(core.size);
      expect(m.weight, `step ${step.toString()}: weight`).toBe(core.weight);
      expect(m.depth, `step ${step.toString()}: depth`).toBe(core.depth);
      for (const [k, v] of core.entries()) expect(Value.equals(heap.value(m.get(k) ?? Value.null) ?? Value.null, v), k).toBe(true);
      expect(m.get("absent")).toBeUndefined();
    }
  });
});

describe("what a program cannot be is refused before it is read", () => {
  it("a canonical form built with shared parts is refused from its weight, before it is expanded", () => {
    // [x, x] nested 40 deep in canonical arrays: 2^40 nodes in 40 containers
    const form = (expr: unknown): V => expectOk(models.jlvm.JlvmProgram.encode({ jlvm: 1n, expr: expr as models.jlvm.JlvmExpr }), "a form");
    let x: V = expectOk(Value.map([]), "the null literal");
    for (let i = 0; i < 40; i++) x = expectOk(Value.map([["arr", arr([x, x])]]), "an array expression");
    const shared = expectOk(Value.map([["jlvm", int(1)], ["expr", x]]), "a program");
    expect(expectErr(fromCanonical(shared), "shared")).toMatchObject({ kind: "Limit" });
    // one just within the size bound still decodes
    expect(fromCanonical(form({ arr: Array.from({ length: 1000 }, () => ({})) })).ok).toBe(true);
  });

  it("a Program or Compiled built by calling the constructor from JavaScript is no program", () => {
    const P = Program as unknown as new (...args: unknown[]) => Program;
    const C = Compiled as unknown as new (...args: unknown[]) => Compiled;
    const real = expectOk(parse("1"), "a program");
    const fakes: unknown[] = [new P(), new P(Symbol("program"), {}, Value.null), new C(), new C(Symbol("compiled"), real, { t: "const", value: Value.null })];
    for (const f of fakes) {
      expect(expectErr(evaluate(f as Program, Value.null), "evaluate")).toMatchObject({ kind: "TypeMismatch" });
      expect(expectErr(compile(f as Program), "compile")).toMatchObject({ kind: "TypeMismatch" });
    }
    const p = fakes[0] as Program;
    expect(expectErr(p.encode(), "encode")).toMatchObject({ kind: "TypeMismatch" });
    expect(expectErr(p.id(), "id")).toMatchObject({ kind: "TypeMismatch" });
  });
});
