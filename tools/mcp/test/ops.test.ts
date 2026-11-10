/**
 * The catalogue `jlvm_ops` gives, against the engine: exactly the closed op list, in its order; the schedule's own
 * costs; every example runs; and every arity outside an operation's range is refused as the engine refuses it, every
 * arity inside it is not.
 */
import { ALL_OPS, FORM_OPS, SCHEDULE_V1, type JlvmOp } from "@kd5ujc/dilf4s-jlvm";
import { describe, expect, it } from "vitest";

import { CATALOGUE, evaluateCall, type Entry } from "../src/index.js";

interface Shown {
  readonly ok: boolean;
  readonly outcome?: { readonly value?: unknown; readonly error?: { readonly error: string; readonly op?: string } };
}

const run = (program: string, data = "null"): Shown => evaluateCall({ program, data }).structured as unknown as Shown;

/** A failure's kind and site, or `value`. */
function verdict(s: Shown): string {
  if (!s.ok) return "refused";
  const e = s.outcome?.error;
  return e === undefined ? "value" : `${e.error}${e.op === undefined ? "" : ` ${e.op}`}`;
}

/** The example's argument list, as JSON values written back as text. */
function exampleArgs(e: Entry): readonly string[] {
  const parsed = JSON.parse(e.example.program) as Readonly<Record<string, unknown>>;
  const args = parsed[e.op];
  return Array.isArray(args) ? args.map((a) => JSON.stringify(a)) : [];
}

const FORMS: ReadonlySet<string> = new Set<string>(FORM_OPS);

/**
 * The operations that refuse too many arguments otherwise than with `Arity`: a Merkle verifier reads a fourth as its
 * profile (`Domain` unless it is the one "rfc6962"), and bn254_pairing counts its pairs (`Crypto` past 64).
 */
const TOO_MANY: Readonly<Partial<Record<JlvmOp, string>>> = { merkle_verify: "Domain", merkle_consistency_verify: "Domain", bn254_pairing: "Crypto" };

describe("the operation catalogue", { timeout: 120_000 }, () => {
  it("is exactly the closed op list, in the schema's order, with the schedule's costs", () => {
    expect(CATALOGUE.map((e) => e.op)).toEqual([...ALL_OPS]);
    expect(CATALOGUE).toHaveLength(93);
    for (const e of CATALOGUE) {
      const c = SCHEDULE_V1.costs[e.op];
      expect([e.cost.base, e.cost.perUnit, e.cost.unit, e.cost.extra.length], e.op).toEqual([c.base, c.perUnit, c.unit, c.extra.length]);
    }
  });

  it("every example parses, compiles and runs without a Type, Arity or Malformed error", () => {
    const bad = CATALOGUE.flatMap((e) => {
      const v = verdict(run(e.example.program, e.example.data));
      return v === "refused" || /^(Type|Arity|Malformed)\b/.test(v) ? [`${e.op}: ${v}`] : [];
    });
    expect(bad).toEqual([]);
  });

  it("every example's head is its operation, written as an argument list", () => {
    for (const e of CATALOGUE) expect(Object.keys(JSON.parse(e.example.program) as object), e.op).toEqual([e.op]);
  });

  it("each arity outside the range is refused as the engine refuses it, and each inside is not", () => {
    const wrong: string[] = [];
    for (const e of CATALOGUE) {
      const base = exampleArgs(e);
      const top = (e.arity.max ?? e.arity.min + 2) + 1;
      for (let n = 0; n <= top; n++) {
        const args = Array.from({ length: n }, (_, i) => base[Math.min(i, base.length - 1)] ?? "null");
        const v = verdict(run(`{"${e.op}": [${args.join(", ")}]}`, e.example.data));
        const inside = n >= e.arity.min && (e.arity.max === null || n <= e.arity.max);
        // forms are checked when the program compiles (Malformed); missing_some, a lookup, and every other operation when it runs (Arity)
        const refusal = n > (e.arity.max ?? n) && e.op in TOO_MANY ? `${TOO_MANY[e.op] ?? ""} ${e.op}` : FORMS.has(e.op) && e.op !== "missing_some" ? "Malformed" : `Arity ${e.op}`;
        if (inside ? v === refusal : v !== refusal) wrong.push(`${e.op} with ${n.toString()}: ${v}`);
      }
    }
    expect(wrong).toEqual([]);
  });

  it("aliases name their operations, and only the variadic ones are marked so", () => {
    const aliased = Object.fromEntries(CATALOGUE.filter((e) => e.aliases.length > 0).map((e) => [e.op, e.aliases]));
    expect(aliased).toEqual({ get: ["getKey"], set: ["setKey"], unset: ["deleteKey"], length: ["size"] });
    expect(CATALOGUE.filter((e) => e.variadic).map((e) => e.op)).toEqual(["+", "*", "min", "max", "merge", "cat"] satisfies JlvmOp[]);
  });
});
