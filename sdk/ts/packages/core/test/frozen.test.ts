import { runInNewContext } from "node:vm";
import { describe, expect, it } from "vitest";

import { codec, models, Value } from "../src/index.js";
import type { Codec, CodecShape } from "../src/codec.js";
import { expectOk, int } from "./vectors.js";

const { jlvm, jlvmErrors } = models;
const { alternative, intEnum, record, required, text, uint, union } = codec;

const textValue = (s: string): Value => expectOk(Value.text(s), "text");

/** `x` as a `T`, past the types, as a JavaScript caller passes it. */
const bad = (x: unknown): never => x as never;

/**
 * Every object reachable from `roots` through own data properties, deferred references resolved: an iterative walk
 * with a visited set, so a cycle through a reference ends. Functions are not entered (a leaf's code is its own).
 */
function reachable(roots: readonly unknown[]): readonly object[] {
  const seen = new Set<object>();
  const todo = [...roots];
  for (let x = todo.pop(); todo.length > 0 || x !== undefined; x = todo.pop()) {
    if (typeof x !== "object" || x === null || seen.has(x)) continue;
    seen.add(x);
    for (const key of Reflect.ownKeys(x)) {
      const d = Object.getOwnPropertyDescriptor(x, key);
      if (d !== undefined && "value" in d) todo.push(d.value);
    }
    const shape: unknown = Reflect.get(x, "shape");
    if (typeof shape === "object" && shape !== null && Reflect.get(shape, "kind") === "defer") todo.push((shape as CodecShape & { kind: "defer" }).resolve());
  }
  return [...seen];
}

describe("every generated model and core codec is deeply frozen when its module has loaded", () => {
  const generated = Object.values(models).flatMap((ns) => Object.values(ns) as unknown[]);
  const objects = reachable([...generated, codec.any, codec.bool, codec.int, codec.uint, codec.nint, codec.bytes, codec.text]);

  it("the walk reaches every generated constant's codec, its shape and their parts", () => {
    expect(generated.filter((x) => typeof x === "object").length, "the generated codecs").toBeGreaterThan(250);
    expect(objects.length, "codecs, shapes, field lists, fields, alternatives and name arrays").toBeGreaterThan(2000);
  });

  it("each object reached is frozen", () => {
    expect(objects.filter((o) => !Object.isFrozen(o))).toEqual([]);
  });

  it("each generated constant is a frozen codec or a primitive", () => {
    const loose = generated.filter((x) => !(typeof x === "number" || typeof x === "bigint" || (typeof x === "object" && x !== null && Object.isFrozen(x) && "shape" in x)));
    expect(loose).toEqual([]);
  });
});

type Mode = "strict" | "sloppy";

/** The outcome of `body` (an expression over `t`, `k` and `x`) run as script in `mode`: `ok:` its value, or `throw:` the error's name. */
function attempt(mode: Mode, body: string, t: object, k: PropertyKey, x: unknown): string {
  const script = `${mode === "strict" ? '"use strict";' : ""}(function () { try { return "ok:" + String(${body}); } catch (e) { return "throw:" + e.name; } })()`;
  return runInNewContext(script, { t, k, x }) as string;
}

/**
 * Every way a caller might change `t[k]` to `x`, and what each gives in each mode. A frozen object refuses each one:
 * strict code gets a TypeError for an assignment, sloppy code has it ignored, and `Reflect` answers false in both.
 */
const writes: readonly { readonly name: string; readonly body: string; readonly strict: string; readonly sloppy: string }[] = [
  { name: "assignment", body: "(t[k] = x, 'done')", strict: "throw:TypeError", sloppy: "ok:done" },
  { name: "defineProperty", body: "Object.defineProperty(t, k, { value: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
  { name: "Reflect.set", body: "Reflect.set(t, k, x)", strict: "ok:false", sloppy: "ok:false" },
  { name: "Reflect.defineProperty", body: "Reflect.defineProperty(t, k, { value: x })", strict: "ok:false", sloppy: "ok:false" },
  { name: "delete", body: "delete t[k]", strict: "throw:TypeError", sloppy: "ok:false" },
  { name: "setPrototypeOf", body: "Object.setPrototypeOf(t, { [k]: x }) && 'done'", strict: "throw:TypeError", sloppy: "throw:TypeError" },
];

/** And for an array: growing or emptying it. */
const arrayWrites: readonly { readonly name: string; readonly body: string; readonly strict: string; readonly sloppy: string }[] = [
  { name: "push", body: "t.push(x)", strict: "throw:TypeError", sloppy: "throw:TypeError" },
  { name: "length = 0", body: "(t.length = 0, 'done')", strict: "throw:TypeError", sloppy: "ok:done" },
  { name: "splice", body: "t.splice(0, 1, x).length", strict: "throw:TypeError", sloppy: "throw:TypeError" },
  { name: "unshift", body: "t.unshift(x)", strict: "throw:TypeError", sloppy: "throw:TypeError" },
];

/** Tries each write on each target, in both modes, then checks each refused as it should and `probe` gives what it gave before. */
function cannotChange(targets: readonly { readonly what: string; readonly t: object; readonly k: PropertyKey; readonly x: unknown }[], probe: () => unknown): void {
  const before = probe();
  for (const mode of ["strict", "sloppy"] as const) {
    for (const { what, t, k, x } of targets) {
      for (const w of [...writes, ...(Array.isArray(t) ? arrayWrites : [])]) {
        expect(attempt(mode, w.body, t, k, x), `${w.name} on ${what}, ${mode}`).toBe(w[mode]);
      }
    }
  }
  expect(probe(), "what the codecs accept and produce").toEqual(before);
}

const shapeOf = <K extends CodecShape["kind"]>(c: Codec<unknown>, kind: K): CodecShape & { readonly kind: K } => {
  expect(c.shape.kind).toBe(kind);
  return c.shape as CodecShape & { readonly kind: K };
};

describe("no write changes what a generated codec accepts or produces", () => {
  it("the JLVM operation choice (texts)", () => {
    const op = shapeOf(jlvm.JlvmOp, "union");
    cannotChange(
      [
        { what: "its texts", t: op.texts, k: 0, x: "evil" },
        { what: "its shape's texts", t: op, k: "texts", x: ["evil"] },
        { what: "the codec's shape", t: jlvm.JlvmOp, k: "shape", x: codec.text.shape },
        { what: "the codec's decode", t: jlvm.JlvmOp, k: "decode", x: (v: Value) => codec.text.decode(v) },
      ],
      () => [jlvm.JlvmOp.decode(textValue("evil")), jlvm.JlvmOp.decode(textValue("if")), jlvm.JlvmOp.encode("evil" as models.jlvm.JlvmOp), jlvm.JlvmOp.encode("if")],
    );
    expect(jlvm.JlvmOp.decode(textValue("evil")).ok, "an unknown operation").toBe(false);
  });

  it("the JLVM program record (fields and their codecs)", () => {
    const program = shapeOf(jlvm.JlvmProgram, "record");
    const version = program.fields[0];
    expect(version?.key).toBe("jlvm");
    cannotChange(
      [
        { what: "its version field's codec", t: version ?? {}, k: "codec", x: codec.int },
        { what: "its version field's key", t: version ?? {}, k: "key", x: "other" },
        { what: "its fields", t: program.fields, k: 0, x: codec.required("jlvm", codec.int) },
        { what: "its allowed keys", t: program.allowed, k: 0, x: "other" },
        { what: "its shape's fields", t: program, k: "fields", x: [] },
      ],
      () => [
        codec.fromJson(jlvm.JlvmProgram, '{"jlvm":2,"expr":{}}'),
        codec.fromJson(jlvm.JlvmProgram, '{"jlvm":1,"expr":{}}'),
        jlvm.JlvmProgram.encode({ jlvm: bad(2n), expr: {} }),
        jlvm.JlvmProgram.encode({ jlvm: 1n, expr: {} }),
      ],
    );
    expect(codec.fromJson(jlvm.JlvmProgram, '{"jlvm":2,"expr":{}}').ok, "a version-2 program").toBe(false);
  });

  it("a deferred reference (its resolution)", () => {
    const apply = shapeOf(jlvm.JlvmApply, "record");
    const ref = apply.fields[0]?.codec ?? codec.any;
    const deferred = shapeOf(ref, "defer");
    const expr = shapeOf(jlvm.JlvmExpr, "union");
    expect(deferred.resolve(), "the reference resolves to the operation choice").toBe(jlvm.JlvmOp);
    cannotChange(
      [
        { what: "its resolution", t: deferred, k: "resolve", x: () => codec.text },
        { what: "the reference's shape", t: ref, k: "shape", x: codec.text.shape },
        { what: "what it resolves to", t: deferred.resolve(), k: "shape", x: codec.text.shape },
        { what: "the expression choice's alternatives", t: expr.alternatives, k: 0, x: codec.alternative(codec.any, [[], []], [[], []]) },
        { what: "an alternative's codec", t: expr.alternatives[4] ?? {}, k: "codec", x: codec.any },
        { what: "an alternative's wire keys", t: expr.alternatives[4]?.keys[1] ?? [], k: 0, x: "evil" },
      ],
      () => [
        codec.fromJson(jlvm.JlvmApply, '{"op":"evil","args":[]}'),
        codec.fromJson(jlvm.JlvmApply, '{"op":"+","args":[{"op":"if","args":[]}]}'),
        codec.fromJson(jlvm.JlvmApply, '{"op":"+","args":[{"op":"if","args":[],"evil":1}]}'),
      ],
    );
    expect(deferred.resolve(), "still the operation choice").toBe(jlvm.JlvmOp);
  });

  it("an error-kind list", () => {
    const kinds = shapeOf(jlvmErrors.JlvmErrorKind, "union");
    cannotChange([{ what: "its texts", t: kinds.texts, k: 0, x: "Evil" }], () => [
      jlvmErrors.JlvmErrorKind.decode(textValue("Evil")),
      jlvmErrors.JlvmErrorKind.decode(textValue("OutOfGas")),
      jlvmErrors.JlvmErrorKind.encode("Evil" as models.jlvmErrors.JlvmErrorKind),
    ]);
    expect(jlvmErrors.JlvmErrorKind.decode(textValue("Evil")).ok).toBe(false);
  });
});

describe("a combinator keeps a copy of what it is given, so changing the original changes nothing", () => {
  it("a union's texts", () => {
    const texts: string[] = ["a", "b"];
    const U = union<"a" | "b">("U", [], texts as ("a" | "b")[]);
    texts.push("evil");
    texts[0] = "z";
    expect(U.decode(textValue("evil")).ok, "a text added later").toBe(false);
    expect(U.decode(textValue("a")), "a text replaced later").toEqual({ ok: true, value: "a" });
    expect(U.encode("evil" as "a")).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
    expect(U.shape).toMatchObject({ texts: ["a", "b"] });
  });

  it("an alternative's names and keys, and the alternatives list", () => {
    const names: string[] = ["x"];
    const A = record<{ readonly x: bigint }>({ x: required("x", uint) });
    const made = alternative(A, [names, names], [names, names]);
    const literal = { codec: A, required: ["x"], allowed: ["x"], keys: [["x"], ["x"]] as [string[], string[]] };
    const alternatives = [made, literal];
    const U = union<{ readonly x: bigint }>("U", alternatives, []);
    names.push("y");
    names[0] = "z";
    literal.keys[0].push("y");
    literal.keys[1][0] = "z";
    alternatives.length = 0;
    expect(Object.isFrozen(made) && Object.isFrozen(made.required) && Object.isFrozen(made.keys) && Object.isFrozen(made.keys[0]), "the alternative itself").toBe(true);
    expect(codec.fromJson(U, '{"x":1}'), "the original keys").toEqual({ ok: true, value: { x: 1n } });
    expect(codec.fromJson(U, '{"z":1}').ok, "a key replaced later").toBe(false);
    expect(codec.fromJson(U, '{"x":1,"y":2}').ok, "a key added later").toBe(false);
    expect(U.encode({ x: 1n }).ok, "the original names").toBe(true);
    expect(U.encode({ z: 1n } as unknown as { x: bigint }).ok, "a name replaced later").toBe(false);
  });

  it("an enumeration's cases", () => {
    const cases: [string, bigint][] = [["a", 1n]];
    const E = intEnum("E", cases);
    (cases[0] ?? ["", 0n])[1] = 2n;
    cases.push(["b", 3n]);
    expect(E.decode(int(1n)), "the original case").toEqual({ ok: true, value: "a" });
    expect(E.decode(int(2n)).ok, "a wire value replaced later").toBe(false);
    expect(E.decode(int(3n)).ok, "a case added later").toBe(false);
    expect(E.encode("b").ok, "a label added later").toBe(false);
  });

  it("a record's fields", () => {
    const fields = { x: required("x", uint) };
    const R = record<{ readonly x: bigint }>(fields);
    Object.assign(fields, { x: required("x", text) });
    expect(Object.isFrozen(required("x", uint)), "a field").toBe(true);
    expect(codec.fromJson(R, '{"x":1}')).toEqual({ ok: true, value: { x: 1n } });
    expect(codec.fromJson(R, '{"x":"a"}').ok, "a field's codec replaced later").toBe(false);
  });
});

describe("schema arguments that are not of their types make a codec that refuses everything", () => {
  const refuses = (c: Codec<unknown>, reason: string): void => {
    expect(c.encode("a"), "encoding").toEqual({ ok: false, error: { kind: "TypeMismatch", reason } });
    expect(c.decode(textValue("a")), "decoding").toEqual({ ok: false, error: { path: [], reason } });
  };


  it("a union's name, alternatives and texts", () => {
    refuses(union(bad(1), [], ["a"]), "the schema's name is not a string");
    refuses(union("U", bad(null), ["a"]), "the schema's names are not arrays of strings");
    refuses(union("U", [], bad(["a", 1])), "the schema's names are not arrays of strings");
    refuses(union("U", [bad({ codec: text, required: ["a"], allowed: ["a"], keys: [["a"], [1]] })], []), "the schema's names are not arrays of strings");
    refuses(union("U", [alternative(text, bad([["a"], null]), [["a"], ["a"]])], []), "the schema's names are not arrays of strings");
    const revoked = Proxy.revocable([], {});
    revoked.revoke();
    refuses(union("U", bad(revoked.proxy), []), "the schema's names are not arrays of strings");
  });

  it("a record's fields", () => {
    refuses(record(bad(null)), "the schema's fields are not an object of record fields");
    refuses(record(bad({ x: { key: 1, codec: text, optional: false } })), "the schema's fields are not an object of record fields");
    refuses(record(bad({ x: { key: "x", codec: text, optional: "no" } })), "the schema's fields are not an object of record fields");
  });

  it("an enumeration's name and cases", () => {
    refuses(intEnum(bad(Symbol("E")), [["a", 1n]]), "the schema's name is not a string");
    refuses(intEnum("E", bad([["a", 1]])), "the schema's enumeration cases are not [label, bigint] pairs");
    refuses(intEnum("E", bad({ length: 1, 0: ["a", 1n] })), "the schema's enumeration cases are not [label, bigint] pairs");
  });
});
