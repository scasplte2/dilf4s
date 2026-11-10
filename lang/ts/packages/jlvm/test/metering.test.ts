/**
 * Denial-of-service regressions, ported from the reference's `MeteringSuite` and Rust's `tests/metering.rs`: every
 * unit of work is charged before it is done, bounded by the gas limit, and nothing hashes or recurses on
 * attacker-chosen input. Timed comparisons are replaced by what they guard that is deterministic: results, error
 * kinds, gas gaps, and equal gas for colliding and ordinary inputs. Cases that would take hours if the guard failed
 * (a whole walk of `2^40` units, a copy of every entry per update) finish within the suite's timeout.
 */
import { ArrayValue, MapValue, Preview, Value, type Value as V } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { DEFAULT_GAS_LIMIT, evaluate, parse, parseValue, SCHEDULE_V1, type Outcome } from "../src/index.js";
import { expectOk } from "./vectors.js";

const S = SCHEDULE_V1;

const t = (s: string): V => expectOk(Value.text(s), "text");
const int = (n: number | bigint): V => expectOk(Value.int(BigInt(n)), "an integer");
const arr = (vs: readonly V[]): V => expectOk(ArrayValue.of(vs), "an array");
const obj = (kvs: readonly (readonly [string, V])[]): V => expectOk(MapValue.of(kvs), "a map");
const json = (text: string): V => expectOk(parseValue(text), text);
const repeat = <T>(x: T, n: number): T[] => Array.from({ length: n }, () => x);

function evalLimit(program: string, data: V, gasLimit: number, env?: MapValue): Outcome {
  const p = expectOk(parse(program), program.slice(0, 80));
  return expectOk(evaluate(p, data, env === undefined ? { gasLimit } : { gasLimit, env }), "evaluates");
}

const evaluated = (program: string, data: V = Value.null): Outcome => evalLimit(program, data, DEFAULT_GAS_LIMIT);
const kind = (o: Outcome): string => (o.result.ok ? "value" : o.result.error.kind);
const valueOf = (o: Outcome): V => expectOk(o.result, "a value");

/** `2^k` distinct strings of length `2k` with one Java hash code: every combination of "Aa" and "BB". */
const combinations = (k: number, a: string, b: string): string[] => Array.from({ length: k }).reduce<string[]>((acc) => acc.flatMap((s) => [s + a, s + b]), [""]);
const colliding = (k: number): string[] => combinations(k, "Aa", "BB");
const control = (k: number): string[] => combinations(k, "Aa", "Ac");

describe("metering", { timeout: 120_000 }, () => {
  it("production dedup keys preserve unsigned bytes, Unicode and first occurrences", () => {
    const values = [json('"0x00"'), json('"0x7f"'), json('"0x80"'), json('"0xff"'), t("\u0000"), t("\u00ff"), t("\u0100"), t("\u{10000}")];
    const data = obj([["xs", arr([...values, ...[...values].reverse()])], ["ys", arr(values)]]);
    expect(evaluated('{"unique":{"var":"xs"}}', data).result).toEqual({ ok: true, value: arr(values) });
    expect(evaluated('{"intersect":[{"var":"xs"},{"var":"ys"}]}', data).result).toEqual({ ok: true, value: json("true") });
    expect(evaluated('{"intersect":[["0x00"],["0x80"]]}').result).toEqual({ ok: true, value: json("false") });
    expect(evaluated('{"intersect":[["ÿ"],["Ā"]]}').result).toEqual({ ok: true, value: json("false") });
  });

  it("join charges its output before building it", () => {
    const p = '{"join": [{"var": "xs"}, {"var": "sep"}]}';
    const fits = evaluated(p, obj([["xs", arr(repeat(Value.null, 1000))], ["sep", t("x".repeat(1000))]]));
    const out = Number(valueOf(fits).weight);
    expect(fits.gasUsed).toBeGreaterThanOrEqual(S.costs.join.perUnit * out);
    const oversized = evaluated(p, obj([["xs", arr(repeat(Value.null, 5000))], ["sep", t("x".repeat(5000))]]));
    expect(kind(oversized), "25 MB of separators is Limit").toBe("Limit");
    expect(oversized.gasUsed, "rejected before its output is charged").toBeLessThan(S.costs.join.perUnit * 4999 * 5000);
  });

  it("cat and join charge for rendering numbers", () => {
    const fifth = expectOk(Value.number(1n, 5n ** 1764n), "1/5^1764");
    const half = expectOk(Value.number(1n, 1n << 4095n), "1/2^4095");
    for (const [p, v] of [
      ['{"cat": {"var": "xs"}}', fifth],
      ['{"cat": {"var": "xs"}}', half],
      ['{"join": [{"var": "xs"}, ""]}', fifth],
    ] as const) {
      const o = evaluated(p, obj([["xs", arr(repeat(v, 20))]]));
      expect(o.gasUsed, "at least 1 gas per output character").toBeGreaterThanOrEqual(Number(valueOf(o).weight));
    }
    const cat = '{"cat": {"var": "xs"}}';
    const ints = evaluated(cat, obj([["xs", arr(Array.from({ length: 1000 }, (_, i) => int(i * 1_000_003)))]]));
    const words = evaluated(cat, obj([["xs", arr(repeat(t("w"), 1000))]]));
    expect(ints.gasUsed, "1000 rendered integers cost more than 1000 letters").toBeGreaterThan(words.gasUsed);
  });

  it("+ and * bound every intermediate", () => {
    expect(kind(evaluated('{"*": {"var": "xs"}}', obj([["xs", arr([...repeat(int(2), 10000), int(0)])]]))), "spread").toBe("Limit");
    expect(kind(evaluated(`{"*": [${repeat("2", 5000).join(", ")}, 0]}`)), "strict").toBe("Limit");
    expect(valueOf(evaluated(`{"+": [${repeat("1", 5000).join(", ")}, 0]}`))).toEqual(int(5000));
  });

  it("each step pays for its accumulator", () => {
    const o = evaluated('{"*": {"var": "xs"}}', obj([["xs", arr(repeat(int(2), 4000))]]));
    expect(Value.equals(valueOf(o), int(1n << 4000n))).toBe(true);
    const accumulators = Array.from({ length: 3999 }, (_, i) => 1 + Math.floor((i + 1) / 64)).reduce((a, b) => a + b, 0);
    expect(o.gasUsed).toBeGreaterThanOrEqual(S.costs["*"].perUnit * accumulators);
  });

  it("unique and intersect cost the same for colliding strings", () => {
    const same = arr(colliding(13).map(t));
    const other = arr(control(13).map(t));
    const unique = '{"unique": {"var": "xs"}}';
    const intersect = '{"intersect": [{"var": "xs"}, {"var": "xs"}]}';
    const u = evaluated(unique, obj([["xs", same]]));
    expect(Value.equals(valueOf(u), same)).toBe(true);
    expect(u.gasUsed).toBe(evaluated(unique, obj([["xs", other]])).gasUsed);
    const i = evaluated(intersect, obj([["xs", same]]));
    expect(Value.equals(valueOf(i), json("true"))).toBe(true);
    expect(i.gasUsed).toBe(evaluated(intersect, obj([["xs", other]])).gasUsed);
  });

  it("unique keeps first occurrences, and intersect is a subset test", () => {
    const run = (p: string): V => valueOf(evaluated(p));
    const same = (a: V, b: string): boolean => Value.equals(a, json(b));
    expect(same(run('{"unique": [[3, "a", 1, 3, [1], {"literal": {"k": 1}}, "a", 2, [1], {"literal": {"k": 1}}, null, 2, null]]}'), '[3, "a", 1, [1], {"k": 1}, 2, null]')).toBe(true);
    expect(same(run('{"unique": [[1, 2.5, 1, 2.5]]}'), "[1, 2.5]")).toBe(true);
    expect(same(run('{"intersect": [[2, [1], "x"], [[1], 1, 2, "x"]]}'), "true")).toBe(true);
    expect(same(run('{"intersect": [[2, [1, 2]], [[1], 1, 2]]}'), "false")).toBe(true);
  });

  it("lookups pay for the path text before resolving it", () => {
    const long = t(repeat("a", 50001).join("."));
    for (const p of ['{"missing_some": [0, [{"var": "p"}]]}', '{"missing": [{"var": "p"}]}']) {
      const gap = evaluated(p, obj([["p", long]])).gasUsed - evaluated(p, obj([["p", t("a")]])).gasUsed;
      expect(gap, p).toBeGreaterThanOrEqual(Number(long.weight));
    }
  });

  it("lookups bound what they return, never a key they read: one absent key past the result bound", () => {
    // as Scala and Rust: missing_some whose minimum is met returns [], whatever the keys weigh; missing returns them
    const data = obj([["ks", arr([t("a".repeat(2 ** 24))])]]);
    expect(evalLimit('{"missing_some": [0, {"var": "ks"}]}', data, 1e9).result).toEqual({ ok: true, value: json("[]") });
    expect(kind(evalLimit('{"missing_some": [1, {"var": "ks"}]}', data, 1e9))).toBe("Limit");
    expect(kind(evalLimit('{"missing": {"var": "ks"}}', data, 1e9))).toBe("Limit");
  });

  it("inside callbacks, missing pays for every frame", () => {
    const data = obj([["ks", arr(Array.from({ length: 50 }, (_, i) => t(`k${i.toString()}`)))]]);
    const gas = (p: string): number => evaluated(p, data).gasUsed;
    const frames = gas('{"map": [[1], {"map": [[1], {"missing": {"var": [["ks"]]}}]}]}') - gas('{"map": [[1], {"missing": {"var": [["ks"]]}}]}');
    const varFrame = gas('{"map": [[1], {"map": [[1], {"var": "k0"}]}]}') - gas('{"map": [[1], {"var": "k0"}]}');
    expect(frames - varFrame).toBeGreaterThanOrEqual(50 * S.perPathSegment - S.perPathSegment);
  });

  it("each frame a read searches pays for its first key's length", () => {
    // every frame's element holds a 96-character key the query shares all but its last character with
    const data = obj([["xs", arr([obj([[`${"k".repeat(95)}b`, t("v")]])])]]);
    const nested = (frames: number, key: string): string => Array.from({ length: frames }).reduce<string>((inner) => `{"map": [{"var": "xs"}, ${inner}]}`, `{"var": "${key}"}`);
    // a 96-character key counts 4 segments where a 1-character key counts 1: 3 more, once itself and once per frame
    for (const frames of [1, 2, 3]) {
      const delta = evaluated(nested(frames, `${"k".repeat(95)}a`), data).gasUsed - evaluated(nested(frames, "a"), data).gasUsed;
      expect(delta, `${frames.toString()} frames`).toBe(3 * S.perPathSegment * (1 + frames));
    }
  });

  it("a computed path is charged by its size before it is parsed", () => {
    // a trailing '.' makes the path malformed: parsing it is the only way to find out
    const starved = evalLimit('{"var": {"var": "p"}}', obj([["p", t(`${repeat("a", 50001).join(".")}.`)]]), S.varAccess + S.perPathSegment + 15);
    expect(kind(starved)).toBe("OutOfGas");
  });

  it("integer path segments are bounded before they are rendered", () => {
    const p = '{"var": [["m", {"var": "i"}]]}';
    expect(valueOf(evaluated(p, obj([["m", obj([["-1", int(7)], ["12", int(8)]])], ["i", int(-1)]])))).toEqual(int(7));
    expect(valueOf(evaluated(p, obj([["m", obj([["12", int(8)]])], ["i", int(12)]])))).toEqual(int(8));
    const huge = evaluated(p, obj([["m", obj([["-1", int(7)]])], ["i", int((1n << 4095n) + 1n)]]));
    expect(huge.result).toMatchObject({ ok: false, error: { kind: "Domain", op: "var" } });
  });

  it("a computed array path pays for the length of its segments", () => {
    const p = '{"var": [[{"var": "k"}]]}';
    const gap = evaluated(p, obj([["k", t("k".repeat(50000))]])).gasUsed - evaluated(p, obj([["k", t("k")]])).gasUsed;
    expect(gap).toBeGreaterThanOrEqual(Math.floor(50000 / 32));
  });

  it("object literals evaluate with long keys", () => {
    const p = `{"count": [{"var": "xs"}, {"object": {"${"k".repeat(50000)}": 1}}]}`;
    expect(valueOf(evaluated(p, obj([["xs", arr(repeat(int(1), 1000))]])))).toEqual(int(1000));
  });

  const lets = (n: number, names: readonly string[]): string => `{"let": [[${names.slice(0, n).map((x) => `["${x}", 1]`).join(", ")}], {"var": "$${names[n - 1] ?? ""}"}]}`;

  it("let compiles thousands of bindings", () => {
    expect(valueOf(evaluated(lets(8000, Array.from({ length: 8000 }, (_, i) => `n${i.toString()}`))))).toEqual(int(1));
  });

  it("let and reads cost the same for colliding names", () => {
    const bound = evaluated(lets(1024, colliding(10)));
    expect(valueOf(bound)).toEqual(int(1));
    expect(bound.gasUsed).toBe(evaluated(lets(1024, control(10))).gasUsed);
  });

  it("a let pays for the length of its names", () => {
    const prefix = "n".repeat(100_000);
    const two = (p: string): string => `{"let": [[["${p}a", 1], ["${p}b", 2]], 3]}`;
    const gap = evaluated(two(prefix)).gasUsed - evaluated(two("")).gasUsed;
    expect(gap).toBeGreaterThanOrEqual(S.element * 2 * Math.floor((prefix.length + 1) / 32));
  });

  it("set, unset and merge update a large map in place", () => {
    // a host map of 100,000 entries updated 2,000 times: each update is O(log n) and shares the rest; rebuilding the
    // map per update would copy 200 million entries
    const m = obj(Array.from({ length: 100_000 }, (_, i) => [`k${i.toString()}`, int(i)] as const));
    const data = obj([["xs", arr(repeat(int(1), 2000))], ["m", m]]);
    const set = evaluated('{"length": {"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"cat": ["n", {"var": "$index"}]}, 1]}, {"var": "m"}]}}', data);
    expect(valueOf(set)).toEqual(int(102_000));
    const unset = evaluated('{"length": {"reduce": [{"var": "xs"}, {"unset": [{"var": "$accumulator"}, {"cat": ["k", {"var": "$index"}]}]}, {"var": "m"}]}}', data);
    expect(valueOf(unset)).toEqual(int(98_000));
    const merged = evaluated('{"length": {"merge": [{"var": "m"}, {"object": {"k1": 2, "new": 3}}]}}', data);
    expect(valueOf(merged)).toEqual(int(100_001));
  });

  it("get, has, set and unset pay for long keys", () => {
    const long = "k".repeat(1 << 20);
    const build = '{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"var": "k"}, {"var": "$index"}]}, {"object": {}}]}';
    const lookup = '{"count": [{"var": "xs"}, {"get": [{"var": "m"}, {"var": "k"}]}]}';
    const data = (k: string): V => obj([["xs", arr(repeat(int(1), 50))], ["k", t(k)], ["m", obj([[k, int(1)]])]]);
    expect(evaluated(build, data(long)).result.ok).toBe(true);
    expect(valueOf(evaluated(lookup, data(long)))).toEqual(int(50));
    // every search pays per SearchBlock (128 units) of its key at each of its levels: 2 for a map of one entry
    const perBlock = S.costs.get.search?.perBlock ?? 0;
    expect(perBlock).toBeGreaterThan(0);
    const gap = evaluated(lookup, data(long)).gasUsed - evaluated(lookup, data("k")).gasUsed;
    expect(gap).toBe(50 * perBlock * ((1 << 20) / 128) * 2);
  });

  it("programs are bounded in size before compilation", () => {
    const isLimit = (text: string): boolean => {
      const r = parse(text);
      return !r.ok && r.error.kind === "Limit";
    };
    const nulls = (n: number): string => `[${repeat("null", n).join(",")}]`;
    expect(isLimit(nulls(1 << 18)), "one past the bound").toBe(true);
    expect(parse(nulls((1 << 18) / 4)).ok, "within the bound").toBe(true);
    const x = "x".repeat((1 << 18) + 1);
    expect(isLimit(`{"var": "${x}"}`), "a path segment counts its length").toBe(true);
    expect(isLimit(`{"var": "$${x}"}`), "a binding name counts its length").toBe(true);
    expect(isLimit(`{"var": "${repeat("y".repeat(1000), 300).join(".")}"}`), "a long path is bounded").toBe(true);
  });

  it("deep programs and data run on the explicit stack", () => {
    // 127 nested operations: each takes two canonical levels
    const p = Array.from({ length: 127 }).reduce<string>((inner) => `{"+": [${inner}, 1]}`, "1");
    expect(valueOf(evaluated(p))).toEqual(int(128));
    const deeper = parse(`{"+": [${p}, 1]}`);
    expect(deeper.ok ? "parsed" : deeper.error.kind, "128 nested operations exceed the canonical depth").toBe("Limit");
    // data 255 arrays deep, read whole and compared
    const d = Array.from({ length: 255 }).reduce<string>((inner) => `[${inner}]`, "1");
    expect(valueOf(evaluated('{"==": [{"var": "d"}, {"var": "d"}]}', json(`{"d": ${d}}`)))).toEqual(json("true"));
  });

  it("ambiguous paths warn once and never change gas", () => {
    const data = json('{"a": 1, "xs": [{"a": 2}]}');
    const plain = evaluated('{"map": [{"var": "xs"}, {"var": "a"}]}', data);
    expect(Value.equals(valueOf(plain), json("[2]"))).toBe(true);
    expect(plain.warnings).toHaveLength(1);
    expect(evaluated('{"map": [{"var": "xs"}, {"var": "$current.a"}]}', data).warnings).toEqual([]);
  });

  it("host bindings are shadowed by let and callbacks", () => {
    const env = expectOk(MapValue.of([["ordinal", int(5)]]), "env");
    const o = evalLimit('{"let": [[["x", {"var": "$ordinal"}]], {"map": [[10], {"+": [{"var": "$x"}, {"var": "$current"}]}]}]}', Value.null, DEFAULT_GAS_LIMIT, env);
    expect(Value.equals(valueOf(o), json("[15]"))).toBe(true);
    expect(kind(evaluated('{"var": "$nope"}'))).toBe("Malformed");
  });

  it("lower and upper change only ASCII letters", () => {
    const data = obj([["s", t("AzΣσİßé́é\u{10D50}\u{1D400}[`@{")]]);
    expect(valueOf(evaluated('{"lower": {"var": "s"}}', data))).toEqual(t("azΣσİßé́é\u{10D50}\u{1D400}[`@{"));
    expect(valueOf(evaluated('{"upper": {"var": "s"}}', data))).toEqual(t("AZΣσİßé́É\u{10D50}\u{1D400}[`@{"));
  });

  it("trim removes exactly the White_Space code points", () => {
    // U+FEFF is not White_Space (JavaScript's trim removes it); U+0085 is (JavaScript's trim keeps it)
    const data = obj([["s", t("\u0085　 a﻿  \t")]]);
    expect(valueOf(evaluated('{"trim": {"var": "s"}}', data))).toEqual(t("a﻿"));
  });

  it("charges are computed in constant time per argument and only as far as payable", () => {
    const big = t("k".repeat(1 << 20));
    const data = obj([["p", arr(repeat(big, 1000))], ["m", obj([["k", int(1)]])], ["k", big]]);
    expect(kind(evalLimit('{"var": {"var": "p"}}', data, 20))).toBe("OutOfGas");
    // 100 of them at the default limit: each segment is a 1 MiB key, priced by its blocks
    expect(valueOf(evaluated('{"var": {"var": "p"}}', obj([["p", arr(repeat(t("k".repeat(1 << 20)), 100))]])))).toEqual(Value.null);
    for (const p of ['{"get": [{"var": "m"}, {"var": "k"}]}', '{"has": [{"var": "m"}, {"var": "k"}]}', '{"set": [{"var": "m"}, {"var": "k"}, 1]}', '{"unset": [{"var": "m"}, {"var": "k"}]}']) {
      expect(kind(evalLimit(p, data, 100)), p).toBe("OutOfGas");
    }
  });

  it("object literals with long common-prefix keys build without comparing them", () => {
    const prefix = "k".repeat(130_000);
    const o = evaluated(`{"map": [{"var": "xs"}, {"object": {"${prefix}a": 1, "${prefix}b": 2}}]}`, obj([["xs", arr(repeat(int(1), 60))]]));
    const maps = valueOf(o);
    expect(maps.kind === "array" ? maps.length : 0).toBe(60);
  });

  it("a starved charge copies nothing it has not paid for", () => {
    expect(kind(evalLimit('{"var": {"var": "p"}}', obj([["p", t("a".repeat(64 << 20))]]), 20))).toBe("OutOfGas");
    expect(kind(evalLimit('{"+": {"var": "xs"}}', obj([["xs", arr(repeat(int(1), 1_000_000))]]), 200))).toBe("OutOfGas");
    // the widest integers a value holds, far past the number bound: their bit lengths are cached
    const huge = int((1n << 33219n) - 1n);
    expect(kind(evaluated('{"abs": {"var": "n"}}', obj([["n", huge]])))).toBe("Limit");
    const negative = int(-(1n << 33219n));
    for (const p of ['{"abs": {"var": "n"}}', '{"pow": [{"var": "n"}, 2]}', '{"round": {"var": "n"}}']) expect(kind(evaluated(p, obj([["n", negative]]))), p).toBe("Limit");
  });

  it("a map to a large value shares it and stops at the weight bound", () => {
    const data = obj([["xs", arr(repeat(int(1), 10_000))], ["big", t("b".repeat(1 << 20))]]);
    const o = evaluated('{"map": [{"var": "xs"}, {"var": "big"}]}', data);
    expect(kind(o)).toBe("Limit");
    expect(o.gasUsed).toBeLessThan(2_000_000);
    const kept = evaluated('{"length": {"filter": [{"var": "xs"}, true]}}', obj([["xs", arr(repeat(t("b".repeat(1 << 20)), 15))]]));
    expect(valueOf(kept)).toEqual(int(15));
  });

  /** `[x, x]` nested `k` deep: `2^k` leaves in `k` allocations. */
  const doubling = (k: number): V => Array.from({ length: k }).reduce<V>((x) => arr([x, x]), t("leaf"));

  it("doubling data is charged by its logical weight before any walk", () => {
    for (const k of [40, 200]) {
      const d = doubling(k);
      expect(d.weight > 1n << 40n).toBe(true);
      const data = obj([["d", d], ["e", doubling(k)]]);
      for (const p of [
        '{"==": [{"var": "d"}, {"var": "e"}]}',
        '{"in": [1, {"var": "d"}]}',
        '{"unique": {"var": "d"}}',
        '{"intersect": [{"var": "d"}, {"var": "e"}]}',
        '{"flatten": {"var": "d"}}',
        '{"sha256": {"var": "d"}}',
        '{"cat": [{"var": "d"}, {"var": "e"}]}',
        '{"merge": [{"var": "d"}]}',
        '{"sigma_verify": [{"var": "d"}, "0x00", "0x00"]}',
      ]) {
        const o = evaluated(p, data);
        expect(kind(o), `k = ${k.toString()}: ${p}`).toBe("OutOfGas");
        expect(o.gasUsed).toBe(DEFAULT_GAS_LIMIT);
      }
      expect(kind(evaluated('[{"var": "d"}]', data)), "building a value from it is bounded at once").toBe("Limit");
      expect(valueOf(evaluated('{"var": "d"}', data))).toBe(d);
      expect(valueOf(evaluated('{"typeof": {"var": "d"}}', data))).toEqual(t("array"));
    }
  });

  it("a rational with no decimal form is named by its size, never its digits", () => {
    const third = expectOk(Value.number(10n ** 1000n + 1n, 3n), "a third");
    for (const p of ['{"cat": {"var": "xs"}}', '{"join": [{"var": "xs"}, ""]}']) {
      const o = evaluated(p, obj([["xs", arr([third])]]));
      const rendered = JSON.stringify(o.result.ok ? null : o.result.error);
      expect(rendered).not.toContain("0000000000");
      expect(rendered).toContain("a rational of 3322/2 bits");
    }
  });

  it("a long ambiguous path warns with a preview", () => {
    const k = "x".repeat(4096);
    const o = evaluated(`{"map": [{"var": "items"}, {"var": "${k}"}]}`, obj([[k, int(1)], ["items", arr([obj([[k, int(2)]])])]]));
    expect(o.warnings).toHaveLength(1);
    expect(o.warnings[0]?.path.toString()).toBe(Preview.of(k).toString());
  });

  it("a refusal previews a huge name or path, never echoes it", () => {
    const huge = "q".repeat(1 << 20);
    for (const program of [`{"${huge}": 1}`, `{"var": "${huge}..x"}`, `{"var": "$.${huge}"}`, `{"let": [[["${huge}.x", 1]], 1]}`]) {
      const r = parse(program);
      expect(r.ok).toBe(false);
      expect(JSON.stringify(r.ok ? null : r.error).length).toBeLessThanOrEqual(2048);
    }
  });
});
