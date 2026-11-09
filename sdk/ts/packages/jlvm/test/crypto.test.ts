/**
 * The section 6 operations on arguments the machine built: the vectors pass their arguments as data, literals and host
 * bindings (core's values), and only a few build one at run time. Here every `jlvm_v1.json` case that reaches an
 * operation taking a map or an array is run again with each of the operation's arguments rebuilt by the machine (a map
 * merged with itself, an array and each array or map in it rebuilt by `map` and `merge`), so its heads, modes, entries,
 * keys, pairs and proofs are the machine's own containers. The outcome (the value, or the error's kind, op and cause)
 * must be the vector's; the gas differs, since rebuilding costs.
 */
import { cbor, models } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { Bindings, evaluate, MAX_GAS_LIMIT, parse, parseValue, siteOf, type Outcome } from "../src/index.js";
import { built, expectOk, hex, loadVectors } from "./vectors.js";

type Case = models.jlvmVectors.JlvmCase;

const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);

/** The operations whose arguments are, or hold, maps and arrays. */
const CONTAINERS = ["merkle_verify", "merkle_consistency_verify", "trie_verify", "trie_prefix_verify", "trie_range_verify", "bls_aggregate_verify", "bn254_pairing", "pmt_verify", "poseidon", "ecvrf_verify", "sigma_verify"] as const;

type Json = null | boolean | number | string | Json[] | { [k: string]: Json };

const current: Json = { var: "$current" };
const is = (x: Json, kind: string): Json => ({ "==": [{ typeof: [x] }, kind] });

/** `x` rebuilt by the machine when it is a map (merged with itself) or an array (mapped), its elements likewise; else itself. */
function rebuilt(x: Json): Json {
  const element: Json = { if: [is(current, "array"), { merge: [current] }, is(current, "object"), { merge: [current, current] }, current] };
  return { if: [is(x, "object"), { merge: [x, x] }, is(x, "array"), { map: [x, element] }, x] };
}

/** The program with every argument of every container operation rebuilt (tests may recurse: the programs are small). */
function rewrite(x: Json): Json {
  if (Array.isArray(x)) return x.map(rewrite);
  if (x === null || typeof x !== "object") return x;
  const out: Record<string, Json> = {};
  for (const [k, v] of Object.entries(x)) {
    const inner = rewrite(v);
    out[k] = (CONTAINERS as readonly string[]).includes(k) && Array.isArray(inner) ? inner.map(rebuilt) : inner;
  }
  return out;
}

const cause = (e: { kind: string; cause?: { kind: string; error: { kind: string } } }): string => (e.cause === undefined ? "none" : `${e.cause.kind.toLowerCase()}:${e.cause.error.kind}`);

function outcome(o: Outcome): string {
  if (o.result.ok) return `value ${hex(expectOk(cbor.encode(o.result.value), "encodes"))}`;
  const e = o.result.error;
  return `error ${e.kind} op ${siteOf(e) ?? "none"} cause ${cause(e)}`;
}

function expected(c: Case): string {
  const o = c.outcome;
  if ("cbor" in o) return `value ${o.cbor}`;
  const [only] = Object.entries(o.cause ?? {});
  return `error ${o.error} op ${o.op ?? "none"} cause ${only === undefined ? "none" : `${only[0].toLowerCase()}:${String(only[1])}`}`;
}

function run(c: Case, program: string): Outcome | undefined {
  const p = parse(program);
  const d = parseValue(c.data);
  if (!p.ok || !d.ok) return undefined;
  const env: [string, models.jlvmVectors.BuiltValue | string][] = [...(c.env ?? []), ...(c.built ?? [])];
  const pairs = env.map(([k, v]) => [k, typeof v === "string" ? expectOk(parseValue(v), "a binding") : built(v)] as const);
  return expectOk(evaluate(p.value, d.value, { env: expectOk(Bindings.of(pairs), "env"), gasLimit: MAX_GAS_LIMIT }), "evaluates");
}

describe("section 6 operations on containers the machine built", { timeout: 600_000 }, () => {
  it("give every reaching case's outcome", () => {
    let checked = 0;
    const failures: string[] = [];
    for (const c of vectors.cases) {
      const o = c.outcome;
      // a case decided by its gas limit, or a limit the rebuilding can reach first, says nothing here
      if ("error" in o && (o.error === "OutOfGas" || o.error === "Limit" || o.error === "Malformed")) continue;
      if (c.gasLimit !== undefined || !CONTAINERS.some((op) => c.program.includes(`"${op}"`))) continue;
      // JSON.parse reads numbers as doubles: a program with a long integer is not round-tripped here
      if (/\d{16,}/.test(c.program)) continue;
      const original = run(c, c.program);
      if (original === undefined || outcome(original) !== expected(c)) continue;
      const again = run(c, JSON.stringify(rewrite(JSON.parse(c.program) as Json)));
      if (again === undefined) continue;
      if (outcome(again) !== expected(c)) failures.push(`${c.name}: want ${expected(c)}, got ${outcome(again)}`);
      checked++;
    }
    expect(failures).toEqual([]);
    expect(checked, "cases rerun on machine-built arguments").toBeGreaterThan(200);
  });
});
