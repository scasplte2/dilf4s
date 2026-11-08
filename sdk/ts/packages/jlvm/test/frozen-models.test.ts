// Nothing here imports core or the JLVM statically: each test loads them itself, in a fresh module registry, so the
// first test changes core's generated models before the JLVM has read them, and the second after.
import type { Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it, vi } from "vitest";

/** Every change a caller could try on the generated models the JLVM reads: core freezes them, so each is refused. */
async function tamper(): Promise<readonly boolean[]> {
  const core = await import("@kd5ujc/dilf4s-core");
  const op = core.models.jlvm.JlvmOp.shape;
  const program = core.models.jlvm.JlvmProgram.shape;
  const texts: unknown = op.kind === "union" ? op.texts : [];
  const field: unknown = program.kind === "record" ? program.fields[0] : {};
  const n = Array.isArray(texts) ? texts.length : 0;
  return [
    Reflect.set(texts as object, n, "evil"),
    Reflect.set(texts as object, 0, "evil"),
    Reflect.set(texts as object, "length", 0),
    Reflect.defineProperty(texts as object, n, { value: "evil" }),
    Reflect.set(field as object, "codec", core.codec.int),
    Reflect.defineProperty(field as object, "codec", { value: core.codec.int }),
  ];
}

/** What the JLVM does with an unknown operation, a version-2 program and a version-1 one, and its own op list. */
async function observe(): Promise<unknown> {
  const core = await import("@kd5ujc/dilf4s-core");
  const jlvm = await import("../src/index.js");
  const ok = <T>(r: { readonly ok: true; readonly value: T } | { readonly ok: false }): T => {
    if (!r.ok) throw new Error("expected a success");
    return r.value;
  };
  const form = (version: bigint, op: string): Value =>
    ok(core.Value.map([["jlvm", ok(core.Value.int(version))], ["expr", ok(core.Value.map([["op", ok(core.Value.text(op))], ["args", ok(core.Value.array([]))]]))]]));
  const kind = (r: { readonly ok: boolean; readonly error?: { readonly kind: string } }): string => (r.ok ? "program" : (r.error?.kind ?? "?"));
  const p = ok(jlvm.parse('{"+": [1, 2]}'));
  const o = ok(jlvm.evaluate(p, core.Value.null));
  return {
    unknownOp: kind(jlvm.fromCanonical(form(1n, "evil"))),
    version2: kind(jlvm.fromCanonical(form(2n, "+"))),
    version1: kind(jlvm.fromCanonical(form(1n, "+"))),
    parsedUnknown: kind(jlvm.parse('{"evil": [1]}')),
    ops: jlvm.ALL_OPS.length,
    knowsEvil: jlvm.opFromWire("evil"),
    evaluated: [o.gasUsed, o.result.ok],
  };
}

const expected = { unknownOp: "Malformed", version2: "Malformed", version1: "program", parsedUnknown: "Malformed", ops: 93, knowsEvil: undefined, evaluated: [148, true] };

describe("core's generated models, changed before or after the JLVM loads, change nothing it does", () => {
  it("changed before the JLVM is imported", async () => {
    vi.resetModules();
    expect(await tamper(), "every change refused").toEqual([false, false, false, false, false, false]);
    expect(await observe()).toEqual(expected);
  });

  it("changed after the JLVM is imported", async () => {
    vi.resetModules();
    const before = await observe();
    expect(before).toEqual(expected);
    expect(await tamper(), "every change refused").toEqual([false, false, false, false, false, false]);
    expect(await observe()).toEqual(before);
  });
});
