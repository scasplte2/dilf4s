/**
 * Evaluation work is linear in the gas charged, counted, never timed: the machine's steps against the gas of every
 * conformance case and of loops that grow, and the persistent map's work per update against its size.
 */
import { ArrayValue, MapValue, models, Value, type Value as V } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { Bindings, DEFAULT_GAS_LIMIT, parse, parseValue, SCHEDULE_V1 } from "../src/index.js";
import { JMap, type JValue } from "../src/jvalue.js";
import { run } from "../src/machine.js";
import { compile } from "../src/node.js";
import { built, expectOk, loadVectors } from "./vectors.js";

const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);

const int = (n: number): V => expectOk(Value.int(BigInt(n)), "an integer");

/** The machine's steps and gas for a program on data. */
function counted(program: string, data: V, gasLimit = DEFAULT_GAS_LIMIT, env?: MapValue): { readonly steps: number; readonly gas: number; readonly ok: boolean } {
  const root = expectOk(compile(expectOk(parse(program), program.slice(0, 60))), "compiles");
  const r = run(SCHEDULE_V1, root, data, env, gasLimit);
  return { steps: r.steps, gas: r.used, ok: r.result.ok };
}

/**
 * Every step either takes a charge or is an evaluation, a value returned, or a warning noted; each evaluation charges
 * before its node does anything, so a run's steps are bounded by a small multiple of its charges, and each charge is
 * at least 1. The bound: four steps per unit of gas, plus the few uncharged steps an evaluation starts and ends with.
 */
const STEPS_PER_GAS = 4;

describe("steps are linear in gas", { timeout: 600_000 }, () => {
  it("on every conformance case that runs", () => {
    let checked = 0;
    let worst = 0;
    for (const c of vectors.cases) {
      const p = parse(c.program);
      const d = parseValue(c.data);
      if (!p.ok || !d.ok) continue;
      const root = compile(p.value);
      if (!root.ok) continue;
      const env: [string, V][] = [];
      for (const [k, v] of c.env ?? []) env.push([k, expectOk(parseValue(v), "a binding")]);
      for (const [k, b] of c.built ?? []) env.push([k, built(b)]);
      const r = run(SCHEDULE_V1, root.value, d.value, expectOk(Bindings.of(env), "env"), Number(c.gasLimit ?? vectors.gasLimit));
      expect(r.used, `${c.name}: the vector's gas`).toBe(Number(c.gasUsed));
      expect(r.steps, `${c.name}: ${r.steps.toString()} steps for ${r.used.toString()} gas`).toBeLessThanOrEqual(STEPS_PER_GAS * r.used + 2);
      worst = Math.max(worst, r.steps / Math.max(r.used, 1));
      checked++;
    }
    expect(checked).toBeGreaterThan(2400);
    expect(worst).toBeLessThan(1);
  });

  it("for loops whose work grows, at twice and four times the size", () => {
    const programs = [
      '{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}, {"var": "$current"}]}, {"object": {}}]}',
      '{"map": [{"var": "xs"}, {"+": [{"var": "$current"}, {"var": "$index"}]}]}',
      '{"filter": [{"var": "xs"}, {"<": [{"var": "$index"}, 100]}]}',
      '{"reduce": [{"var": "xs"}, {"+": [{"var": "$accumulator"}, 1]}, 0]}',
      '{"all": [{"var": "xs"}, {"some": [[1, 2], {"==": [{"var": "$current"}, 2]}]}]}',
    ];
    for (const p of programs) {
      const at = (n: number) => counted(p, expectOk(MapValue.of([["xs", expectOk(ArrayValue.of(Array.from({ length: n }, (_, i) => int(i))), "xs")]]), "data"));
      const [a, b, c] = [at(1000), at(2000), at(4000)];
      for (const r of [a, b, c]) expect(r.ok, p).toBe(true);
      // steps per unit of gas stay put as the input grows
      const ratios = [a, b, c].map((r) => r.steps / r.gas);
      expect(Math.max(...ratios) / Math.min(...ratios), `${p}: ${ratios.join(", ")}`).toBeLessThan(1.1);
    }
  });
});

/** The nodes of `m`'s tree that `n` does not share. */
function unshared(m: JMap, n: JMap): number {
  const shared = new Set<object>();
  const walk = (root: unknown, visit: (t: object) => boolean): void => {
    const todo: unknown[] = [root];
    for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
      if (typeof t !== "object" || t === null || !visit(t)) continue;
      todo.push(Reflect.get(t, "left"), Reflect.get(t, "right"));
    }
  };
  walk(Reflect.get(m, "root"), (t) => {
    shared.add(t);
    return true;
  });
  let fresh = 0;
  walk(Reflect.get(n, "root"), (t) => {
    if (shared.has(t)) return false;
    fresh++;
    return true;
  });
  return fresh;
}

const heightOf = (m: JMap): number => {
  const root: unknown = Reflect.get(m, "root");
  return typeof root === "object" && root !== null ? Number(Reflect.get(root, "height")) : 0;
};

describe("the persistent map", () => {
  const sizes = [1 << 10, 1 << 14, 1 << 17];

  it("stays balanced: its height is within the AVL bound, 1.44 log2(n + 2)", () => {
    for (const n of sizes) {
      let m = JMap.empty;
      // keys inserted in order, the worst case for an unbalanced tree, then every other one removed
      for (let i = 0; i < n; i++) m = m.set(`k${i.toString().padStart(8, "0")}`, int(i)) ?? m;
      expect(m.size).toBe(n);
      expect(heightOf(m), `${n.toString()} entries`).toBeLessThanOrEqual(Math.ceil(1.44 * Math.log2(n + 2)));
      for (let i = 0; i < n; i += 2) m = m.removed(`k${i.toString().padStart(8, "0")}`);
      expect(m.size).toBe(n / 2);
      expect(heightOf(m), `${(n / 2).toString()} entries after removals`).toBeLessThanOrEqual(Math.ceil(1.44 * Math.log2(n / 2 + 2)));
    }
  });

  it("an update builds O(log n) nodes and shares every other one", () => {
    for (const n of sizes) {
      const entries = Array.from({ length: n }, (_, i) => [`k${i.toString().padStart(8, "0")}`, int(i)] as const);
      const m = JMap.sorted(entries);
      if (m === undefined) throw new Error("builds");
      const bound = 2 * Math.ceil(1.44 * Math.log2(n + 2)) + 2;
      for (const key of ["k00000000", `k${(n >> 1).toString().padStart(8, "0")}`, "new", "zzz"]) {
        const set = m.set(key, int(-1));
        if (set === undefined) throw new Error("sets");
        expect(unshared(m, set), `set ${key} in ${n.toString()}`).toBeLessThanOrEqual(bound);
        expect(unshared(m, m.removed(key)), `remove ${key} from ${n.toString()}`).toBeLessThanOrEqual(bound);
      }
    }
  });

  it("lists its entries in code-point order, which UTF-16 order is not", () => {
    // U+FF61 sorts after U+1F600 in UTF-16 units (0xFF61 > 0xD83D) and before it by code point
    const keys = ["\u{1F600}", "｡", "a", "", "é"];
    let m = JMap.empty;
    for (const k of keys) m = m.set(k, int(1)) ?? m;
    expect(m.entries().map(([k]) => k)).toEqual(["", "a", "é", "｡", "\u{1F600}"]);
    const v: JValue | undefined = m.get("｡");
    expect(v).toEqual(int(1));
  });
});
