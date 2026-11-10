/**
 * Every textual conformance vector in `shared/vectors/jlvm_v1.json`, called through `jlvm_evaluate` by an MCP client
 * in memory, each giving the
 * vector's value (canonical CBOR, and its surface JSON exactly when the vector records one) or error (kind, `op`,
 * cause), its gas and its warnings. A text that does not parse is a refusal, which the vectors record as that error
 * with no gas. Every proved call of `shared/vectors/zk_jlvm_v1.json` gives the file's statement, whose canonical CBOR is
 * the proof's public values.
 *
 * The cases whose bindings are built with shared parts (`built`) are heavier than any text the server accepts: each is
 * checked to be so.
 */
import { json, models } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { GAS_CAP_CEILING, MAX_TEXT_BYTES, measure } from "../src/index.js";
import { built, connected, loadVectors, type Called } from "./support.js";

type Case = models.jlvmVectors.JlvmCase;

const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);
const zk = loadVectors("zk_jlvm_v1.json", models.zkJlvmVectors.ZkJlvmVectors);

const textual = vectors.cases.filter((c) => c.built === undefined);

const family = (name: string): string => name.toLowerCase();

function expectedCause(c: models.jlvmVectors.JlvmCause | undefined): string {
  if (c === undefined) return "none";
  const entries: readonly (readonly [string, unknown])[] = Object.entries(c);
  const [only] = entries;
  return entries.length === 1 && only !== undefined ? `${family(only[0])}:${String(only[1])}` : "malformed cause";
}

/** A case's outcome, rendered for comparison: the value's CBOR and JSON, or the error's kind, `op` and cause; its gas; its warnings. */
function expected(c: Case): string {
  const o = c.outcome;
  const outcome = "cbor" in o ? `value ${o.cbor} json ${o.json ?? "<none>"}` : `error ${o.error} op ${o.op ?? "none"} cause ${expectedCause(o.cause)}`;
  return `${outcome} gas ${c.gasUsed.toString()} warnings ${c.warnings.toString()}`;
}

/** Whether every text of a case is within the server's measure: its UTF-8 length plus its numbers' exponents. */
function fits(c: Case): boolean {
  return [c.program, c.data, ...(c.env ?? new Map<string, string>()).values()].every((t) => measure(t, MAX_TEXT_BYTES) <= MAX_TEXT_BYTES);
}

/**
 * What the server answers for a case: a text past its measure, or a limit past its highest cap, never reaches the
 * JLVM. A text past the measure is `InputTooLarge`; a limit past 2^53 is not a whole number JSON carries exactly, so it
 * is `InvalidArguments`; one past the cap is `GasLimitOutOfRange` (server.test.ts).
 */
function expectedOf(c: Case, limit: bigint): string {
  const refused = (kind: string): string => `error ${kind} op none cause none gas 0 warnings 0`;
  // a text past the server's measure is refused before anything is read
  if (!fits(c)) return refused("InputTooLarge");
  if (limit > BigInt(Number.MAX_SAFE_INTEGER)) return refused("InvalidArguments");
  if (limit > BigInt(GAS_CAP_CEILING)) return refused("GasLimitOutOfRange");
  return expected(c);
}

interface Shown {
  readonly error?: string;
  readonly op?: string;
  readonly cause?: Readonly<Record<string, string>>;
}

function actual(r: Called): string {
  const s = r.structured;
  if (s["ok"] !== true) {
    const refusal = s["refusal"] as { readonly kind: string };
    return `error ${refusal.kind} op none cause none gas 0 warnings 0`;
  }
  const outcome = s["outcome"] as { readonly value?: { readonly cbor?: string; readonly json?: string; readonly omitted?: { readonly json?: string } }; readonly error?: Shown };
  const tail = ` gas ${String(s["gasUsed"])} warnings ${String((s["warnings"] as readonly unknown[]).length)}`;
  if (outcome.value !== undefined) {
    const j = outcome.value.json ?? (outcome.value.omitted?.json === "NoSurfaceForm" ? "<none>" : `<omitted ${String(outcome.value.omitted?.json)}>`);
    return `value ${outcome.value.cbor ?? "<omitted>"} json ${j}${tail}`;
  }
  const e = outcome.error ?? {};
  const [cause] = Object.entries(e.cause ?? {});
  return `error ${e.error ?? "?"} op ${e.op ?? "none"} cause ${cause === undefined ? "none" : `${family(cause[0])}:${cause[1]}`}${tail}`;
}

const argsOf = (c: { readonly program: string; readonly data: string; readonly env?: ReadonlyMap<string, string>; readonly gasLimit?: bigint }, defaultLimit: bigint): Readonly<Record<string, unknown>> => ({
  program: c.program,
  data: c.data,
  bindings: Object.fromEntries(c.env ?? []),
  gasLimit: Number(c.gasLimit ?? defaultLimit),
});

describe("jlvm_v1.json through jlvm_evaluate", { timeout: 600_000 }, () => {
  it("the file holds exactly the textual and built cases this test covers", () => {
    expect(vectors.cases.length, "cases").toBe(3363);
    expect(textual.length, "textual cases").toBe(3343);
    expect(vectors.cases.filter((c) => c.built !== undefined).length, "built cases").toBe(20);
  });

  it("every text of every case a text carries is within the server's caps, but the rows past the exponent budget", () => {
    // their exponents pass core's 2^24 budget, so they pass the server's measure (which counts exponents) by far
    expect(textual.filter((c) => !fits(c)).map((c) => c.name)).toEqual(["dilf4s JSON exponent budget: a program past it is refused", "dilf4s JSON exponent budget: a binding past it is refused"]);
  });

  it("each textual case gives the vector's outcome, gas and warnings, its value's JSON included", async () => {
    // the highest cap, so the cases with a limit past the default run at their own
    const { call, close } = await connected({ gasCap: GAS_CAP_CEILING });
    const failures: string[] = [];
    for (const c of textual) {
      const got = actual(await call("jlvm_evaluate", argsOf(c, vectors.gasLimit)));
      const want = expectedOf(c, c.gasLimit ?? vectors.gasLimit);
      if (got !== want) failures.push(`${c.name}: want ${want.slice(0, 300)}, got ${got.slice(0, 300)}`);
    }
    await close();
    expect(failures).toEqual([]);
  });

  it("each case whose bindings are built with shared parts has a binding no text within the cap can carry", () => {
    const heavy = vectors.cases.filter((c) => c.built !== undefined);
    expect(heavy.length, "the built cases this test checks").toBe(20);
    for (const c of heavy) {
      const renderings = [...(c.built ?? new Map<string, models.jlvmVectors.BuiltValue>()).values()].map((b) => json.renderWithin(built(b), MAX_TEXT_BYTES));
      expect(
        renderings.some((r) => !r.ok && r.error.kind === "TooLarge"),
        `${c.name}: a binding's JSON is longer than ${MAX_TEXT_BYTES.toString()} bytes`,
      ).toBe(true);
    }
  });
});

describe("zk_jlvm_v1.json through jlvm_evaluate", { timeout: 120_000 }, () => {
  it("each call's statement is the file's, and its canonical CBOR is the proof's public values", async () => {
    const { call, close } = await connected();
    expect(zk.calls.length).toBe(8);
    for (const c of zk.calls) {
      const r = await call("jlvm_evaluate", argsOf(c, 0n));
      expect(r.structured["ok"], c.name).toBe(true);
      const s = r.structured["statement"] as Readonly<Record<string, unknown>>;
      expect({ program: s["program"], input: s["input"], bindings: s["bindings"], gasUsed: s["gasUsed"] }, `${c.name}: the digests`).toEqual({
        program: c.statement.program,
        input: c.statement.input,
        bindings: c.statement.bindings,
        gasUsed: Number(c.statement.gasUsed),
      });
      expect(r.structured["statementCbor"], `${c.name}: the public values`).toBe(c.publicValues);
    }
    await close();
  });
});
