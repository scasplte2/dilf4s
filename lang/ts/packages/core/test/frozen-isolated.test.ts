// Nothing here imports the models statically: the first module to load them is the test itself, which tries to
// change them at once, before any other module (core's entry point, as a consumer such as an evaluator imports it)
// has read them.
import { runInNewContext } from "node:vm";
import { describe, expect, it, vi } from "vitest";

import type { CodecShape } from "../src/codec.js";

describe("the models are immutable from the moment their module has loaded", () => {
  it("changes tried before any other module imports them change nothing", async () => {
    vi.resetModules();
    const jlvm = await import("../src/generated/jlvm.js");
    const op = jlvm.JlvmOp.shape as CodecShape & { readonly kind: "union" };
    const program = jlvm.JlvmProgram.shape as CodecShape & { readonly kind: "record" };
    const intCodec = (await import("../src/codec.js")).int;
    const tries = {
      push: runInNewContext("(function () { try { t.push('evil'); return 'changed'; } catch (e) { return e.name; } })()", { t: op.texts }) as unknown,
      sloppyIndex: runInNewContext("t[0] = 'evil'; t[0]", { t: op.texts }) as unknown,
      strictIndex: runInNewContext("'use strict'; (function () { try { t[0] = 'evil'; return 'changed'; } catch (e) { return e.name; } })()", { t: op.texts }) as unknown,
      fieldCodec: Reflect.set(program.fields[0] ?? {}, "codec", intCodec),
      defineCodec: Reflect.defineProperty(program.fields[0] ?? {}, "codec", { value: intCodec }),
    };
    expect(tries).toEqual({ push: "TypeError", sloppyIndex: "if", strictIndex: "TypeError", fieldCodec: false, defineCodec: false });

    // a consumer that loads afterwards gets the same, unchanged models
    const core = await import("../src/index.js");
    expect(core.models.jlvm.JlvmOp, "one module instance").toBe(jlvm.JlvmOp);
    expect(core.codec.fromJson(core.models.jlvm.JlvmProgram, '{"jlvm":1,"expr":{"op":"evil","args":[]}}').ok, "an unknown operation").toBe(false);
    expect(core.codec.fromJson(core.models.jlvm.JlvmProgram, '{"jlvm":2,"expr":{}}').ok, "a version-2 program").toBe(false);
    expect(core.codec.fromJson(core.models.jlvm.JlvmProgram, '{"jlvm":1,"expr":{"op":"if","args":[]}}').ok, "a version-1 program").toBe(true);
  });
});
