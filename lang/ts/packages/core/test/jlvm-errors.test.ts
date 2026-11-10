import { describe, expect, it } from "vitest";

import { codec, DomainTag, fromHex, models, taggedHash } from "../src/index.js";
import { expectOk, hexOf, loadVectors } from "./vectors.js";

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

  it("a row's cause is present only for Crypto, and some Crypto rows record one", () => {
    expect(errors.filter((e) => e.error.cause !== undefined && e.error.error !== "Crypto").map((e) => e.name)).toEqual([]);
    expect(errors.filter((e) => e.error.cause !== undefined).length).toBeGreaterThan(0);
  });

  /**
   * This SDK has no parser, so it checks each alias row's canonical side (jlvm.md section 1): the recorded form decodes
   * as a program applying the row's own name, never the alias, and its identity is the tagged hash of that form.
   */
  it("each alias row's canonical form names the operation, and its identity is that form's", () => {
    const tag = expectOk(DomainTag.of("dilf4s/jlvm/program/v1"), "the program tag");
    expect(vectors.aliases.map((a) => a.alias).sort()).toEqual(["deleteKey", "getKey", "setKey", "size"]);
    for (const a of vectors.aliases) {
      const form = expectOk(fromHex(a.cbor), "hex");
      const program = expectOk(codec.fromCbor(models.jlvm.JlvmProgram, form), a.alias);
      expect("op" in program.expr ? program.expr.op : undefined, a.alias).toBe(a.op);
      expect(hexOf(taggedHash("SHA-256", tag, form)), a.alias).toBe(a.id);
      expect(a.program.includes(`"${a.alias}"`) && a.own.includes(`"${a.op}"`), a.alias).toBe(true);
    }
  });
});
