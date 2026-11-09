/**
 * The machine updates a host map in place of a copy: `set`, `unset` and `merge` over a host map of any size build only
 * the nodes on their search paths of core's persistent tree, as gas charges them (a search per entry), never list the
 * host map, and keep its size, weight and depth exact. Counted, never timed: the nodes a result does not share with its
 * host map, whether a map's entries were ever listed, and the nodes built.
 */
import { ArrayValue, MapValue, models, Value, type Value as V } from "@kd5ujc/dilf4s-core";
import { treeStats, unsharedNodes } from "@kd5ujc/dilf4s-core/internal";
import { describe, expect, it } from "vitest";

import { Bindings, Compiled, compile, evaluate, fromCanonical, parse, Program } from "../src/index.js";
import { expectErr, expectOk } from "./vectors.js";

const int = (n: number): V => expectOk(Value.int(BigInt(n)), "an integer");
const arr = (vs: readonly V[]): V => expectOk(ArrayValue.of(vs), "an array");
const map = (es: readonly (readonly [string, V])[]): MapValue => expectOk(MapValue.of(es), "a map");

/** A map built one `set` at a time, as an update makes one: its entries never listed, so a listing shows. */
function unlisted(es: readonly (readonly [string, V])[]): MapValue {
  const m = es.reduce((acc, [k, v]) => expectOk(acc.set(k, v), "set"), MapValue.empty);
  expect(treeStats(m)).toMatchObject({ listed: false, listedByCodePoint: false });
  return m;
}

/** The AVL bound on the height of a tree of `n` entries. */
const avlHeight = (n: number): number => Math.floor(1.4405 * Math.log2(n + 2) - 0.3277);

describe("updating a host map", { timeout: 120_000 }, () => {
  const big = unlisted(Array.from({ length: 100_000 }, (_, i) => [`k${i.toString()}`, int(i)] as const));
  const nested = unlisted(Array.from({ length: 1000 }, (_, i) => [`k${i.toString()}`, arr([int(i)])] as const));
  const data = map([["m", big], ["n", nested], ["xs", arr(Array.from({ length: 500 }, (_, i) => int(i)))]]);

  it("lists none of its entries and shares all its tree but the updates' paths, for an absent key or a present one, flat or nested", () => {
    const perUpdate = 2 * avlHeight(100_000 + 500) + 3;
    for (const [p, host, updates] of [
      ['{"unset": [{"var": "m"}, "absent"]}', big, 0],
      ['{"unset": [{"var": "m"}, "k7"]}', big, 1],
      ['{"set": [{"var": "m"}, "k7", 1]}', big, 1],
      ['{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}, [1]]}, {"var": "m"}]}', big, 500],
      ['{"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "m"}]}', big, 500],
      ['{"merge": [{"var": "m"}, {"object": {"k1": 2, "new": 3}}]}', big, 2],
      ['{"set": [{"var": "n"}, "new", 1]}', nested, 1],
    ] as const) {
      const program = expectOk(parse(p), p);
      const o = expectOk(evaluate(program, data), "evaluates");
      if (!o.result.ok || o.result.value.kind !== "map") throw new Error(`${p}: a map`);
      expect(unsharedNodes(host, o.result.value), `${p}: the nodes it built`).toBeLessThanOrEqual(updates * perUpdate);
      // a removal can take whole subtrees away, so only a set's own node is certain: at least one node either way
      if (updates > 0) expect(unsharedNodes(host, o.result.value), `${p}: the count is not vacuous`).toBeGreaterThan(0);
      expect(treeStats(host), `${p}: the host map is never listed`).toMatchObject({ listed: false, listedByCodePoint: false });
      expect(treeStats(o.result.value), `${p}: nor is the map it built`).toMatchObject({ listed: false, listedByCodePoint: false });
    }
  });

  it("hides its deepest values with an exact depth, which the tree keeps: no count of depths is read", () => {
    // every value of `n` is an array: removing them lowers the map's depth, which each node's deepest value gives,
    // with no listing of the host map at all (the overlay this replaces read its depths once per evaluation)
    const p = expectOk(parse('{"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "n"}]}'), "unsets");
    const o = expectOk(evaluate(p, data), "evaluates");
    if (!o.result.ok || o.result.value.kind !== "map") throw new Error("a map");
    expect([o.result.value.size, o.result.value.depth]).toEqual([500, 2]);
    expect(treeStats(nested).listed).toBe(false);
    // a host map one level within the limit, its deepest value hidden, nested twice: its exact depth lets the arrays be
    // built
    const deep = Array.from({ length: 254 }).reduce<V>((x) => arr([x]), int(0));
    const near = unlisted([["a", deep], ["b", int(1)]]);
    const twice = expectOk(parse('{"!!": [[[{"unset": [{"var": "m"}, "a"]}], [{"unset": [{"var": "m"}, "a"]}]]]}'), "nests");
    const n = expectOk(evaluate(twice, map([["m", near]])), "evaluates");
    expect(n.result.ok, JSON.stringify(n.result.ok ? null : n.result.error)).toBe(true);
    expect(treeStats(near).listed).toBe(false);
    // and one that is truly too deep is still refused
    const tooDeep = expectOk(parse('[[{"unset": [{"var": "m"}, "b"]}]]'), "too deep");
    expect(expectOk(evaluate(tooDeep, map([["m", near]])), "evaluates").result).toMatchObject({ ok: false, error: { kind: "Limit" } });
  });

  it("does work linear in its updates around a hidden deep host value: n log n, never n^2", () => {
    // Codex round 10's case: a host map's deep value hidden, the map held in a seed, then n entries set beside it.
    // Every depth stays exact, so no update rescans the tree. Counted, never timed: each node core's tree builds runs
    // Math.max, so its calls bound the work
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
    // the count is not vacuous: at least a node per update
    expect(a).toBeGreaterThan(2000);
    // twice the updates, each O(log n): about twice the work and the gas, never four times
    expect(b / a, `${a.toString()} then ${b.toString()} calls`).toBeLessThan(2.4);
    expect(gb / ga).toBeLessThan(2.4);
    expect(b / gb, "work per unit of gas").toBeLessThan(2);
  });

  it("removing most of a host map leaves a map of what is left, listed in time linear in it", () => {
    const host = unlisted(Array.from({ length: 2001 }, (_, i) => [`k${i.toString()}`, int(i)] as const));
    const env = expectOk(Bindings.of([["xs", arr(Array.from({ length: 2000 }, (_, i) => int(i)))]]), "env");
    const p = expectOk(parse('{"keys": {"reduce": [{"var": "$xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$current"}]}]}, {"var": ""}]}}'), "unsets");
    const o = expectOk(evaluate(p, host, { env }), "evaluates");
    expect(o.result).toEqual({ ok: true, value: arr([expectOk(Value.text("k2000"), "text")]) });
    // the host map is as it was, and was never listed: only the one-entry map that was left is
    expect(host.size).toBe(2001);
    expect(treeStats(host)).toMatchObject({ listed: false, listedByCodePoint: false });
  });

  it("keeps the size, weight, depth and entries exact through any updates a program makes", () => {
    // a deterministic walk of sets and removals, each by a program on the map the last one left, over a host map whose
    // values nest to different depths; each result is compared with the map built afresh from the same entries
    const deep = (d: number): V => Array.from({ length: d }).reduce<V>((x) => arr([x]), int(d));
    const model = new Map<string, V>(Array.from({ length: 64 }, (_, i) => [`k${((i * 7) % 64).toString()}`, deep(i % 5)] as const));
    let m: MapValue = map([...model]);
    let seed = 12345;
    const next = (n: number): number => {
      seed = (seed * 1103515245 + 12345) % 2 ** 31;
      return seed % n;
    };
    const set = expectOk(compile(expectOk(parse('{"set": [{"var": "m"}, {"var": "k"}, {"var": "v"}]}'), "set")), "compiles");
    const unset = expectOk(compile(expectOk(parse('{"unset": [{"var": "m"}, {"var": "k"}]}'), "unset")), "compiles");
    for (let step = 0; step < 400; step++) {
      const key = `k${next(80).toString()}`;
      const op = next(3);
      const v = op === 1 ? deep(next(6)) : expectOk(Value.text("v".repeat(next(40))), "text");
      const o = expectOk(evaluate(op === 0 ? unset : set, map([["m", m], ["k", expectOk(Value.text(key), "key")], ["v", v]])), "evaluates");
      if (!o.result.ok || o.result.value.kind !== "map") throw new Error(`step ${step.toString()}: a map`);
      m = o.result.value;
      if (op === 0) model.delete(key);
      else model.set(key, v);
      const ref = map([...model]);
      expect([m.size, m.depth, m.weight], `step ${step.toString()}`).toEqual([ref.size, ref.depth, ref.weight]);
      expect(Value.equals(m, ref), `step ${step.toString()}: equal`).toBe(true);
    }
  });

  it("keeps an exact weight once a heavy entry of a host map whose weight saturates is removed", () => {
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
