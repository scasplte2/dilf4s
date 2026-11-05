import { describe, expect, it } from "vitest";

import { models } from "../src/index.js";
import { loadVectors } from "./vectors.js";

type JlvmErrorKind = models.jlvmVectors.JlvmErrorKind;

/**
 * Whether each kind of the schema's `jlvm-error-kind` carries a normative `op` (jlvm.md section 3). A `Record` keyed by
 * the generated union, so a new kind does not compile until it is listed here.
 */
const hasSite: Readonly<Record<JlvmErrorKind, boolean>> = {
  Malformed: false,
  Type: true,
  Arity: true,
  Arithmetic: true,
  Domain: true,
  Crypto: true,
  Limit: false,
  OutOfGas: false,
};

/**
 * The coverage gate (conformance.md section 6) for the JLVM's errors. This SDK has no JLVM evaluator, so it checks the
 * schema's side: every kind has a row of jlvm_v1.json, and each row's `op` is present exactly for the kinds that carry
 * one, with a row for the `var` site.
 */
describe("every JLVM error kind has a vector row", { timeout: 60_000 }, () => {
  const vectors = loadVectors("jlvm_v1.json", models.jlvmVectors.JlvmVectors);
  const errors = vectors.cases.flatMap((c) => ("error" in c.outcome ? [{ name: c.name, error: c.outcome }] : []));

  it("every kind the schema names has a row", () => {
    const pinned = new Set<string>(errors.map((e) => e.error.error));
    expect(Object.keys(hasSite).filter((k) => !pinned.has(k))).toEqual([]);
  });

  it("a row's op is present exactly for the kinds that carry one", () => {
    expect(errors.filter((e) => hasSite[e.error.error] !== (e.error.op !== undefined)).map((e) => e.name)).toEqual([]);
    expect(errors.filter((e) => e.error.op === "var").length).toBeGreaterThan(0);
  });
});
