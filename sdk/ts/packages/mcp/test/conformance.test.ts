/**
 * The conformance vectors through the tool: a deterministic sample of `shared/vectors/jlvm_v1.json` (every seventh case
 * that a text can carry, in file order) called through `jlvm_evaluate` by an MCP client in memory, each giving the
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
const STRIDE = 7;
const sample = textual.filter((_, i) => i % STRIDE === 0);

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

/**
 * What the server answers for a case: a limit past its highest cap never reaches the JLVM. One past 2^53 is not a whole
 * number JSON carries exactly, so it is `InvalidArguments`; one past the cap is `GasLimitOutOfRange` (server.test.ts).
 */
function expectedOf(c: Case, limit: bigint): string {
  const refused = (kind: string): string => `error ${kind} op none cause none gas 0 warnings 0`;
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
  it("the file holds the cases this test samples", () => {
    // counted from the file, not pinned: rows other PRs add to jlvm_v1.json must not break this test (#139 added 7
    // while this one was open); the sample is every STRIDE-th case a text carries, and those are all but the built ones
    expect(vectors.cases.length, "cases").toBeGreaterThan(3000);
    expect(textual.length + vectors.cases.filter((c) => c.built !== undefined).length, "every case is textual or built").toBe(vectors.cases.length);
    expect(sample.length, "the sample").toBe(Math.ceil(textual.length / STRIDE));
  });

  it("every text of every case a text carries is within the server's caps", () => {
    const over = textual.flatMap((c) => [c.program, c.data, ...[...(c.env ?? new Map<string, string>()).values()]].filter((t) => measure(t, MAX_TEXT_BYTES) > MAX_TEXT_BYTES).map(() => c.name));
    expect(over).toEqual([]);
  });

  it("each sampled case gives the vector's outcome, gas and warnings, its value's JSON included", async () => {
    // the highest cap, so the cases with a limit past the default run at their own
    const { call, close } = await connected({ gasCap: GAS_CAP_CEILING });
    const failures: string[] = [];
    for (const c of sample) {
      const got = actual(await call("jlvm_evaluate", argsOf(c, vectors.gasLimit)));
      const want = expectedOf(c, c.gasLimit ?? vectors.gasLimit);
      if (got !== want) failures.push(`${c.name}: want ${want.slice(0, 300)}, got ${got.slice(0, 300)}`);
    }
    await close();
    expect(failures).toEqual([]);
  });

  it("each case whose bindings are built with shared parts has a binding no text within the cap can carry", () => {
    const heavy = vectors.cases.filter((c) => c.built !== undefined);
    expect(heavy.length, "the built cases this test checks").toBeGreaterThanOrEqual(20);
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
    expect(zk.calls.length).toBeGreaterThan(0);
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
