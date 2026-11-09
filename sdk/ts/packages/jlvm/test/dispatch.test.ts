/**
 * Every operation has exactly one handler (Scala's `OpDispatchSuite`, Rust's `every_operation_has_one_handler`), and the
 * gas table of `docs/specs/jlvm.md` section 7 is this package's schedule, row for row (Rust's `gas_table.rs`): the
 * reference generates the table from its own schedule, so the implementations and the spec agree on every operation,
 * not only on those the vectors reach.
 */
import { models, Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { CRYPTO } from "../src/crypto.js";
import { ALL_OPS, CRYPTO_OPS, evaluate, FORM_OPS, isCrypto, isForm, isStrict, NOT_A_JLVM_ERROR, opFromWire, opNamed, parse, SCHEDULE_V1, siteOf, type JlvmOp, type OpCost, type Rate, type SizeUnit } from "../src/index.js";
import { Heap } from "../src/jvalue.js";
import { STRICT } from "../src/ops.js";
import { readRepoFile } from "./vectors.js";

describe("dispatch", () => {
  it("the operations are exactly the generated JlvmOp's, each a form, strict or cryptographic, never two", () => {
    const shape = models.jlvm.JlvmOp.shape;
    expect(shape.kind).toBe("union");
    expect(ALL_OPS).toEqual(shape.kind === "union" ? shape.texts : []);
    // the schedule is a record over the generated type, so its keys are every operation the compiler knows
    expect([...ALL_OPS].sort()).toEqual(Object.keys(SCHEDULE_V1.costs).sort());
    expect(new Set(ALL_OPS).size).toBe(ALL_OPS.length);
    for (const op of ALL_OPS) {
      const classes = [isForm(op), isStrict(op), isCrypto(op)].filter(Boolean).length;
      expect(classes, `${op} is in ${classes.toString()} classes`).toBe(1);
      expect(opFromWire(op), `${op} is its own wire name`).toBe(op);
      expect(opNamed(op)).toBe(op);
    }
    for (const op of [...FORM_OPS, ...CRYPTO_OPS]) expect(ALL_OPS, op).toContain(op);
  });

  it("every strict operation has exactly one strict handler, and every cryptographic one exactly one cryptographic handler", () => {
    const strict = ALL_OPS.filter(isStrict);
    expect(Object.keys(STRICT).sort()).toEqual([...strict].sort());
    expect(Object.keys(CRYPTO).sort()).toEqual([...CRYPTO_OPS].sort());
    const heap = new Heap();
    for (const op of strict) {
      // with no arguments, a handler refuses or answers: it never falls through to another operation's
      const r = STRICT[op]([], heap);
      if (!r.ok && "op" in r.error) expect(r.error.op, op).toBe(op);
    }
    for (const op of CRYPTO_OPS) {
      const r = CRYPTO[op]([], heap);
      // with no arguments, every cryptographic handler runs and refuses the arity at its own operation (no pairs is a
      // pairing's true): none is a stub, and the handler type admits no outcome but a value or a JLVM error
      if (!r.ok) expect({ kind: r.error.kind, op: siteOf(r.error) }, op).toEqual({ kind: "Arity", op });
      else expect(op, "only bn254_pairing answers no arguments").toBe("bn254_pairing");
    }
  });

  it("the aliases name their operations, and are not wire names", () => {
    for (const [alias, own] of [
      ["getKey", "get"],
      ["setKey", "set"],
      ["deleteKey", "unset"],
      ["size", "length"],
    ] as const) {
      expect(opNamed(alias)).toBe(own);
      expect(opFromWire(alias)).toBeUndefined();
    }
  });
});

const BEGIN = "<!-- gas-table: generated from GasSchedule.V1 by GasTableGen; do not edit -->";
const END = "<!-- gas-table: end -->";

const unit = (u: SizeUnit): string => (u === "None" ? "none" : `\`${u}\``);
const rate = (r: Rate): string => `${r.perUnit.toString()} per ${unit(r.unit)}`;

function row(op: JlvmOp, c: OpCost): string {
  const others: string[] = [];
  if (c.byArgument !== undefined) others.push(`argument ${c.byArgument[0].toString()}: ${rate(c.byArgument[1])}`);
  for (const r of c.extra) others.push(`plus ${rate(r)}`);
  if (c.search !== undefined) others.push(`search: ${c.search.perLevel.toString()} per level, ${c.search.perBlock.toString()} per level and key block`);
  if (c.merging !== undefined) others.push(`merging: ${c.merging.perLevel.toString()} per level and entry, ${c.merging.perBlock.toString()} per level and weight block`);
  const per = c.unit === "None" && c.perUnit === 0 ? "" : rate({ perUnit: c.perUnit, unit: c.unit });
  return `| \`${op}\` | ${c.base.toString()} | ${per} | ${others.join("; ")} |`;
}

function render(): string {
  const s = SCHEDULE_V1;
  const scalars = (
    [
      ["literal", s.literal],
      ["element", s.element],
      ["varAccess", s.varAccess],
      ["perPathSegment", s.perPathSegment],
      ["callbackRun", s.callbackRun],
      ["rendering", s.rendering],
    ] as const
  )
    .map(([k, g]) => `\`${k}\` ${g.toString()}`)
    .join(", ");
  return [
    BEGIN,
    "",
    `Schedule v${s.version.toString()}. Scalar charges: ${scalars}.`,
    "",
    "| Operation | Base | Per unit | Other rates |",
    "|---|---|---|---|",
    ...ALL_OPS.map((op) => row(op, s.costs[op])),
    "",
    END,
  ].join("\n");
}

describe("the gas table", () => {
  it("is this schedule, row for row", () => {
    const spec = readRepoFile("docs/specs/jlvm.md");
    const [b, e] = [spec.indexOf(BEGIN), spec.indexOf(END)];
    expect(b >= 0 && e > b, "jlvm.md has the gas-table markers").toBe(true);
    expect(spec.slice(b, e + END.length)).toBe(render());
  });
});

describe("the published schedule", () => {
  const run = (limit: number): string => {
    const p = parse('{"+": [1, 2]}');
    if (!p.ok) throw new Error("parses");
    const o = evaluate(p.value, Value.null, { gasLimit: limit });
    if (!o.ok) throw new Error("evaluates");
    return o.value.result.ok ? `value at ${o.value.gasUsed.toString()}` : o.value.result.error.kind;
  };

  it("is frozen throughout, so no assignment, strict or sloppy, changes an outcome", () => {
    const before = run(120);
    expect(before).toBe("OutOfGas");
    const cost = SCHEDULE_V1.costs["+"];
    // strict code (a module's): every assignment throws, and nothing changes
    expect(() => {
      (cost as { base: number }).base = 0;
    }).toThrow(TypeError);
    expect(() => {
      (cost.extra as Rate[]).push({ perUnit: 0, unit: "None" });
    }).toThrow(TypeError);
    expect(() => {
      (cost.extra[0] as { perUnit: number }).perUnit = 0;
    }).toThrow(TypeError);
    expect(() => {
      (SCHEDULE_V1 as { literal: number }).literal = 0;
    }).toThrow(TypeError);
    // sloppy code: every assignment is silently ignored
    // eslint-disable-next-line @typescript-eslint/no-implied-eval -- sloppy-mode code, which only a Function body is
    const sloppy = new Function(
      "s",
      "s.costs['+'].base = 0; s.costs['+'].extra[0].perUnit = 0; s.costs.get.search.perLevel = 0; s.costs.merge.merging.perLevel = 0; s.costs.schnorr_verify.byArgument[1].perUnit = 0; s.literal = 0;",
    ) as (s: unknown) => void;
    sloppy(SCHEDULE_V1);
    expect(SCHEDULE_V1.costs["+"].base).toBe(40);
    expect(SCHEDULE_V1.costs.get.search?.perLevel).toBe(3);
    expect(SCHEDULE_V1.costs.merge.merging?.perLevel).toBe(12);
    expect(SCHEDULE_V1.costs.schnorr_verify.byArgument?.[1].perUnit).toBe(1);
    expect(run(120)).toBe(before);
    expect(Object.isFrozen(SCHEDULE_V1.costs) && Object.isFrozen(SCHEDULE_V1.costs["+"].extra)).toBe(true);
  });

  it("is the only schedule evaluate takes: one a caller builds or copies, however alike, is refused", () => {
    const p = parse("1");
    if (!p.ok) throw new Error("parses");
    for (const schedule of [{ ...SCHEDULE_V1 }, { ...SCHEDULE_V1, costs: { ...SCHEDULE_V1.costs } }, Object.create(SCHEDULE_V1) as typeof SCHEDULE_V1]) {
      expect(evaluate(p.value, Value.null, { schedule })).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
    }
    expect(evaluate(p.value, Value.null, { schedule: SCHEDULE_V1 }).ok).toBe(true);
  });
});

describe("siteOf", () => {
  it("names an error's site, and refuses what is not a JLVM error, guarded, never thrown on", () => {
    expect(siteOf({ kind: "Type", op: "+", message: "" })).toBe("+");
    expect(siteOf({ kind: "Domain", op: "var", message: "" })).toBe("var");
    expect(siteOf({ kind: "Limit", message: "" })).toBeUndefined();
    const revoked = Proxy.revocable({ kind: "Type", op: "+" }, {});
    revoked.revoke();
    const selfRevoking = Proxy.revocable(
      { kind: "Type", op: "+" },
      {
        get: (t, k, r) => {
          if (k === "kind") selfRevoking.revoke();
          return Reflect.get(t, k, r) as unknown;
        },
      },
    );
    for (const x of [revoked.proxy, selfRevoking.proxy, null, 5, "Type", { kind: "Nope" }, { kind: "Type", op: "getKey" }, { kind: "Type" }, { kind: "Arity", op: 5 }]) {
      expect(siteOf(x as never)).toBe(NOT_A_JLVM_ERROR);
    }
    // each field is read once
    const reads: PropertyKey[] = [];
    const counting = new Proxy({ kind: "Crypto", op: "sha256" }, { get: (t, k, r) => (reads.push(k), Reflect.get(t, k, r) as unknown) });
    expect(siteOf(counting as never)).toBe("sha256");
    expect(reads).toEqual(["kind", "op"]);
  });
});
