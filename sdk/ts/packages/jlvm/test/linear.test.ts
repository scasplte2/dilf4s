/**
 * Evaluation work is linear in the gas charged, counted, never timed: the machine's steps against the gas of every
 * conformance case and of loops that grow, and an update's work on core's persistent map against the map's size.
 */
import { ArrayValue, MapValue, models, Value, type Value as V } from "@kd5ujc/dilf4s-core";
import { treeStats, unsharedNodes } from "@kd5ujc/dilf4s-core/internal";
import { describe, expect, it } from "vitest";

import { Bindings, DEFAULT_GAS_LIMIT, evaluate, parse, parseValue, SCHEDULE_V1 } from "../src/index.js";
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

/** The AVL bound on the height of a tree of `n` entries. */
const avlHeight = (n: number): number => Math.floor(1.4405 * Math.log2(n + 2) - 0.3277);

/** The map a program gives on `data`, within the gas limit. */
function mapOf(program: string, data: V, env?: MapValue): MapValue {
  const o = expectOk(evaluate(expectOk(parse(program), program.slice(0, 60)), data, { env, gasLimit: 1e12 }), "evaluates");
  if (!o.result.ok || o.result.value.kind !== "map") throw new Error(`${program}: ${JSON.stringify(o.result.ok ? o.result.value.kind : o.result.error)}`);
  return o.result.value;
}

describe("an update of core's persistent map, from a program", { timeout: 120_000 }, () => {
  const sizes = [1 << 10, 1 << 14, 1 << 17];
  const key = (i: number): string => `k${i.toString().padStart(8, "0")}`;

  it("builds O(log n) nodes of a host map's tree and shares every other one, however large the host map", () => {
    for (const n of sizes) {
      const host = expectOk(MapValue.of(Array.from({ length: n }, (_, i) => [key(i), int(i)] as const)), "a host map");
      // one update builds a node per level and a rotation's more
      const bound = 2 * avlHeight(n) + 3;
      for (const k of [key(0), key(n >> 1), "new", "zzz"]) {
        for (const p of [`{"set": [{"var": ""}, "${k}", -1]}`, `{"unset": [{"var": ""}, "${k}"]}`]) {
          const r = mapOf(p, host);
          expect(unsharedNodes(host, r), `${p} on ${n.toString()} entries`).toBeLessThanOrEqual(bound);
        }
        expect(unsharedNodes(host, mapOf(`{"set": [{"var": ""}, "${k}", -1]}`, host)), "the count is not vacuous").toBeGreaterThan(0);
      }
      // a loop of updates builds at most that many nodes per update: the host map is never copied
      const m = 64;
      const env = expectOk(MapValue.of([["xs", expectOk(ArrayValue.of(Array.from({ length: m }, (_, i) => int(i))), "xs")]]), "env");
      const looped = mapOf('{"reduce": [{"var": "$xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["n", {"var": "$current"}]}, 1]}, {"var": ""}]}', host, env);
      expect(looped.size).toBe(n + m);
      expect(unsharedNodes(host, looped), `${m.toString()} updates of ${n.toString()} entries`).toBeLessThanOrEqual(m * bound);
      expect(treeStats(looped).height).toBeLessThanOrEqual(avlHeight(n + m));
    }
  });

  it("lists its keys in code-point order, which UTF-16 order is not, whichever way the map was built", () => {
    // U+FF61 sorts after U+1F600 in UTF-16 units (0xFF61 > 0xD83D) and before it by code point; canonical order puts
    // the shorter UTF-8 first, so it differs from both
    const keys = ["\u{1F600}", "｡", "a", "", "é", "aa"];
    const listed = ["", "a", "aa", "é", "｡", "\u{1F600}"];
    const text = (s: string): string => JSON.stringify(s);
    const host = expectOk(MapValue.of(keys.map((k) => [k, int(1)] as const)), "a host map");
    const literal = `{"object": {${keys.map((k) => `${text(k)}: 1`).join(", ")}}}`;
    const updated = keys.reduce((m, k) => `{"set": [${m}, ${text(k)}, 1]}`, '{"object": {}}');
    for (const source of ['{"var": ""}', literal, updated]) {
      const o = expectOk(evaluate(expectOk(parse(`{"keys": [${source}]}`), source), host), "evaluates");
      expect(o.result.ok && o.result.value.kind === "array" && o.result.value.items.map((t) => (t.kind === "text" ? t.value : ""))).toEqual(listed);
    }
  });
});
