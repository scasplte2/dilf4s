/**
 * The conformance vectors `shared/vectors/jlvm_v1.json`, run as the reference's `JlvmVectors.run` runs them: parse the
 * program, the data and the host bindings (the first failure is the outcome, with no gas), add the bindings built with
 * shared parts, then evaluate with the case's gas limit (the file's when absent) and the latest schedule. Every case
 * must give the same value (canonical CBOR) or error (its kind, `op` and `cause`), the same gas and the same number of
 * warnings. Every case runs: none is skipped.
 *
 * `JLVM_CASE=<substring>` runs only the matching cases and prints each outcome.
 */
import { cbor, models, type Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { Bindings, evaluate, fromCanonical, opNamed, parse, parseValue, siteOf, type JlvmError, type Outcome } from "../src/index.js";
import { built, expectOk, hex, loadVectors } from "./vectors.js";

type Case = models.jlvmVectors.JlvmCase;

const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);

const failed = (e: JlvmError): Outcome => ({ gasUsed: 0, result: { ok: false, error: e }, warnings: [] });

/** `JlvmVectors.run`: the first parse failure (program, then data, then bindings) is the outcome, with no gas. */
export function run(c: Case, defaultLimit: bigint): Outcome {
  const program = parse(c.program);
  if (!program.ok) return program.error.kind === "TypeMismatch" ? failed({ kind: "Malformed", message: "not text" }) : failed(program.error);
  const data = parseValue(c.data);
  if (!data.ok) return data.error.kind === "TypeMismatch" ? failed({ kind: "Malformed", message: "not text" }) : failed(data.error);
  const env: [string, Value][] = [];
  for (const [k, v] of c.env ?? []) {
    const x = parseValue(v);
    if (!x.ok) return x.error.kind === "TypeMismatch" ? failed({ kind: "Malformed", message: "not text" }) : failed(x.error);
    env.push([k, x.value]);
  }
  for (const [k, b] of c.built ?? []) env.push([k, built(b)]);
  const bindings = expectOk(Bindings.of(env), "the host bindings");
  return expectOk(evaluate(program.value, data.value, { env: bindings, gasLimit: Number(c.gasLimit ?? defaultLimit) }), "evaluates");
}

/** A cause family's name, as the vectors key it (`poseidonMerkle`) or the error names it (`PoseidonMerkle`), compared alike. */
const family = (name: string): string => name.toLowerCase();

/** A Crypto error's cause as the vectors record it: its family and that family's kind. */
function cause(e: JlvmError): string {
  if (e.kind !== "Crypto" || e.cause === undefined) return "none";
  return `${family(e.cause.kind)}:${e.cause.error.kind}`;
}

/** A recorded cause: its one key, the family, and that family's kind. */
function expectedCause(c: models.jlvmVectors.JlvmCause | undefined): string {
  if (c === undefined) return "none";
  const entries: readonly (readonly [string, unknown])[] = Object.entries(c);
  const [only] = entries;
  return entries.length === 1 && only !== undefined ? `${family(only[0])}:${String(only[1])}` : "malformed cause";
}

/** What a case's outcome must be, rendered for comparison: a value, or an error's kind, normative `op` and cause. */
function expected(c: Case): string {
  const o = c.outcome;
  return "cbor" in o ? `value ${o.cbor}` : `error ${o.error} op ${o.op ?? "none"} cause ${expectedCause(o.cause)}`;
}

function actual(o: Outcome): string {
  if (o.result.ok) return `value ${hex(expectOk(cbor.encode(o.result.value), "encodes"))}`;
  const e = o.result.error;
  return `error ${e.kind} op ${siteOf(e) ?? "none"} cause ${cause(e)}`;
}

describe("jlvm_v1.json", { timeout: 600_000 }, () => {
  it("every case gives the vector's outcome, gas and warnings", () => {
    const filter = process.env["JLVM_CASE"];
    let passed = 0;
    const failures: string[] = [];
    for (const c of vectors.cases) {
      if (filter !== undefined && !c.name.includes(filter)) continue;
      const o = run(c, vectors.gasLimit);
      if (filter !== undefined) console.log(`${c.name}: ${actual(o)} gas ${o.gasUsed.toString()}`);
      const [want, got] = [expected(c), actual(o)];
      if (want !== got) failures.push(`outcome ${c.name}: want ${want}, got ${got} (${JSON.stringify(o.result.ok ? null : o.result.error)})`);
      else if (BigInt(o.gasUsed) !== c.gasUsed) failures.push(`gas ${c.name}: want ${c.gasUsed.toString()}, got ${o.gasUsed.toString()} (${got})`);
      else if (BigInt(o.warnings.length) !== c.warnings) failures.push(`warnings ${c.name}: want ${c.warnings.toString()}, got ${o.warnings.length.toString()}`);
      else passed++;
    }
    console.log(`jlvm_v1.json: ${passed.toString()} of ${vectors.cases.length.toString()} cases pass`);
    for (const f of failures.slice(0, 60)) console.log(`  ${f}`);
    expect(failures, `${failures.length.toString()} cases differ`).toEqual([]);
    if (filter === undefined) expect(passed).toBe(vectors.cases.length);
  });

  it("every canonical form is read as its row says, its refusals in the normative order (jlvm.md section 1)", () => {
    expect(vectors.decodings.length).toBeGreaterThanOrEqual(6);
    for (const row of vectors.decodings) {
      const r = fromCanonical(built(row.form));
      const got = r.ok ? `cbor ${hex(expectOk(r.value.encode(), "encodes"))}` : `error ${r.error.kind}`;
      const want = "cbor" in row.outcome ? `cbor ${row.outcome.cbor}` : `error ${row.outcome.error}`;
      expect(got, row.name).toBe(want);
    }
  });

  it("every alias parses to the program its own name writes, with the row's canonical form and identity", () => {
    expect(vectors.aliases).toHaveLength(4);
    for (const row of vectors.aliases) {
      const alias = expectOk(parse(row.program), row.alias);
      const own = expectOk(parse(row.own), row.op);
      const encoded = hex(expectOk(alias.encode(), "encodes"));
      expect(encoded, `${row.alias}: its canonical form`).toBe(row.cbor);
      expect(hex(expectOk(own.encode(), "encodes")), `${row.alias} parses as ${row.op}`).toBe(encoded);
      expect(hex(expectOk(alias.id(), "an identity")), `${row.alias}: its identity`).toBe(row.id);
      expect(opNamed(row.alias), `${row.alias} names ${row.op}`).toBe(row.op);
      expect(encoded.includes(hex(new TextEncoder().encode(row.alias))), `${row.alias}: the canonical form never names the alias`).toBe(false);
    }
  });

  it("every error kind of the schema has a row, and each row's op is present exactly for the kinds that carry one", () => {
    const errors = vectors.cases.flatMap((c) => ("error" in c.outcome ? [[c.name, c.outcome] as const] : []));
    const kinds = models.jlvmErrors.JlvmErrorKind.shape;
    const all = kinds.kind === "union" ? kinds.texts : [];
    for (const k of all) expect(errors.some(([, e]) => e.error === k), `a row with error ${k}`).toBe(true);
    const carries = (k: string): boolean => ["Type", "Arity", "Arithmetic", "Domain", "Crypto"].includes(k);
    for (const [name, e] of errors) {
      expect(e.op !== undefined, `${name}: ${e.error} with op ${String(e.op)}`).toBe(carries(e.error));
      expect(e.cause === undefined || e.error === "Crypto", `${name}: a cause on ${e.error}`).toBe(true);
    }
    expect(errors.some(([, e]) => e.op === "var"), "a row with op var").toBe(true);
  });
});
