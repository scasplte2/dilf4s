import { readdirSync } from "node:fs";
import { runInNewContext } from "node:vm";
import { describe, expect, it } from "vitest";

import { codec, hashCommitment, json, models, Preview, Value, valueCommitmentWithin, type Result } from "../src/index.js";
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

/** The realm's own prototypes (`Object.prototype`, `Map.prototype`, the iterators'...), where a walk of core's objects stops. */
const intrinsics: ReadonlySet<unknown> = new Set([
  Object.getPrototypeOf([].values()),
  Object.getPrototypeOf(Object.getPrototypeOf([].values())),
  ...Object.getOwnPropertyNames(globalThis).flatMap((name): unknown[] => {
    const g: unknown = Reflect.get(globalThis, name);
    const p: unknown = typeof g === "function" ? Reflect.get(g, "prototype") : undefined;
    return typeof p === "object" && p !== null ? [p, Object.getPrototypeOf(p) as unknown] : [];
  }),
]);

/** Whether `f` is a class: its `prototype` is fixed, as a class's is and a plain function's is not. */
const isClass = (f: unknown): boolean => typeof f === "function" && Object.getOwnPropertyDescriptor(f, "prototype")?.writable === false;

/**
 * Every object and class reachable from `roots` through own properties (values and accessors), prototypes and a
 * class's superclass, up to the realm's own: iterative, with a visited set. A plain function is not entered.
 */
function reachableWithClasses(roots: readonly unknown[]): readonly object[] {
  const seen = new Set<object>();
  const todo = [...roots];
  while (todo.length > 0) {
    const x = todo.pop();
    const entered = (typeof x === "object" && x !== null) || isClass(x);
    if (!entered || seen.has(x as object) || intrinsics.has(x)) continue;
    const o = x as object;
    seen.add(o);
    todo.push(Object.getPrototypeOf(o));
    for (const key of Reflect.ownKeys(o)) {
      const d = Object.getOwnPropertyDescriptor(o, key);
      if (d !== undefined) todo.push(d.value, Reflect.get(d, "get"), Reflect.get(d, "set"));
    }
  }
  return [...seen];
}

describe("core's classes, their prototypes and its exported objects are frozen", () => {
  it("every object and class reachable from any of core's modules, as shipped (entry points or not)", async () => {
    // each module's exports, as a caller importing the shipped file by its URL gets them
    const files = readdirSync(new URL("../src/", import.meta.url)).filter((f) => f.endsWith(".ts"));
    expect(files, "core's modules").toContain("cbor.ts");
    const modules = await Promise.all(files.map(async (f) => (await import(/* @vite-ignore */ new URL(`../src/${f}`, import.meta.url).href)) as Record<string, unknown>));
    const decoded = expectOk(codec.fromJson(codec.textMap(codec.uint), '{"a":1}'), "a text map");
    const roots = [...modules.flatMap((m) => Object.values(m)), decoded, expectOk(Value.text("x"), "text")];
    const reached = reachableWithClasses(roots);
    expect(reached.filter(isClass).length, "core's classes").toBeGreaterThanOrEqual(12);
    // a module namespace (cbor, codec, models...) is not frozen, but no code can write to it
    const namespace = (o: object): boolean => Object.prototype.toString.call(o) === "[object Module]";
    expect(reached.filter((o) => !Object.isFrozen(o) && !namespace(o)).map((o) => (isClass(o) ? `class ${(o as { name: string }).name}` : Object.prototype.toString.call(o)))).toEqual([]);
    expect(Object.isFrozen(Object.getPrototypeOf(decoded)), "a decoded text map's private class").toBe(true);
  });

  it("replacing a value class's factory or accessor changes nothing a codec accepts or produces", async () => {
    const { IntValue } = await import("../src/index.js");
    const probe = (): unknown => [jlvm.JlvmProgram.encode({ jlvm: 1n, expr: {} }), codec.fromJson(jlvm.JlvmProgram, '{"jlvm":2,"expr":{}}')];
    const before = probe();
    for (const mode of ["strict", "sloppy"] as const) {
      for (const w of writes) {
        expect(attempt(mode, w.body, IntValue, "of", () => IntValue.of(2n)), `${w.name} on IntValue.of, ${mode}`).toBe(w[mode]);
        expect(attempt(mode, w.body, IntValue.prototype, "value", 1n), `${w.name} on IntValue.prototype.value, ${mode}`).toBe(w[mode]);
        expect(attempt(mode, w.body, Value, "int", () => IntValue.of(2n)), `${w.name} on Value.int, ${mode}`).toBe(w[mode]);
      }
    }
    expect(probe()).toEqual(before);
  });
});

describe("schema arguments of the wrong type for a leaf or literal make a codec that refuses everything", () => {
  const refuses = (c: Codec<unknown>, reason: string): void => {
    expect(c.encode("a"), "encoding").toEqual({ ok: false, error: { kind: "TypeMismatch", reason } });
    expect(c.decode(textValue("a")), "decoding").toEqual({ ok: false, error: { path: [], reason } });
  };

  it("literals, patterns, leaves and references", () => {
    const circular: Record<string, unknown> = {};
    const literal = codec.literalText(bad(circular));
    circular["self"] = circular;
    refuses(literal, "the schema's literal is not a string");
    refuses(codec.literalText(bad(1n)), "the schema's literal is not a string");
    refuses(codec.literalInt(bad(null)), "the schema's literal is not a bigint");
    refuses(codec.literalInt(bad("1")), "the schema's literal is not a bigint");
    refuses(codec.matching(bad("[a-z]+")), "the schema's pattern is not one of the shared patterns");
    refuses(codec.matching(bad({ toString: () => "([0-9a-f]{2})*" })), "the schema's pattern is not one of the shared patterns");
    refuses(codec.leaf(bad(1), bad(2)), "the schema's functions are not functions");
    refuses(codec.defer(bad(null)), "the schema's functions are not functions");
    const revoked = Proxy.revocable(() => codec.text, {});
    revoked.revoke();
    refuses(codec.defer(bad(revoked.proxy)), "the schema's functions are not functions");
  });

  it("a part that is not a codec is refused where it is used, never called", () => {
    const notACodec = "the schema's codec is not a codec";
    const items = codec.vector<unknown>(bad(undefined), 0, null);
    expect(items.encode([1])).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: notACodec } });
    const decoded = codec.fromJson(items, "[1]");
    expect(decoded.ok ? undefined : decoded.error.kind === "Decode" ? decoded.error.error.reason : undefined).toBe(notACodec);
    const field = codec.record<{ readonly x: number }>({ x: required("x", bad({ encode: 1, decode: null })) });
    expect(field.encode({ x: 1 })).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: notACodec } });
    expect(codec.defer(() => bad(42)).decode(int(1n))).toEqual({ ok: false, error: { path: [], reason: notACodec } });
  });
});

describe("a refusal one caller is given cannot change what the next caller is given", () => {
  /** `n` arrays around null, built one level at a time: past the depth limit, the shared DepthExceeded. */
  const nested = (n: number): Result<unknown, unknown> =>
    Array.from({ length: n }).reduce<Result<Value, unknown>>((inner) => (inner.ok ? Value.array([inner.value]) : inner), { ok: true, value: Value.null });
  const probes: readonly (readonly [string, () => Result<unknown, unknown>])[] = [
    ["a bool codec's refusal", () => codec.bool.encode(bad(null))],
    ["a generated record's refusal of a bool field", () => models.jlvmVectors.JlvmVectors.encode(bad({ cases: null }))],
    ["an integer too wide", () => Value.int(1n << 40_000n)],
    ["a rational too wide", () => Value.number(1n << 40_000n, 3n)],
    ["not a bigint", () => Value.int(bad(1))],
    ["not an array", () => Value.array(bad(1))],
    ["not entries", () => Value.map(bad(1))],
    ["the depth limit", () => nested(300)],
    ["a non-terminating decimal", () => json.decimal(bad(expectOk(Value.number(1n, 3n), "1/3")))],
    ["not a value to commit", () => valueCommitmentWithin(bad(1), 10)],
    ["not a byte limit", () => valueCommitmentWithin(Value.null, bad("10"))],
    ["not an algorithm", () => hashCommitment(bad("MD5"), [], Value.null)],
    ["not a prefix", () => hashCommitment("SHA-256", bad(1), Value.null)],
  ];

  for (const [what, probe] of probes) {
    it(what, () => {
      const first = probe();
      const again = probe();
      expect(first.ok, "a refusal").toBe(false);
      const error: unknown = first.ok ? undefined : first.error;
      if (typeof error !== "object" || error === null) return;
      const shared = !again.ok && again.error === error;
      expect(!shared || Object.isFrozen(error), "an error given to every caller is frozen").toBe(true);
      const before = structuredClone(error);
      for (const mode of ["strict", "sloppy"] as const) {
        for (const w of writes) attempt(mode, w.body, error, "kind", "Poisoned");
        attempt(mode, "(t.reason = x, t.limit = x, 'done')", error, "reason", "poisoned");
      }
      expect(again.ok ? undefined : again.error, "what the next caller is given").toEqual(before);
      const third = probe();
      expect(third.ok ? undefined : third.error).toEqual(before);
    });
  }
});

describe("a caller's function or codec revoked after a codec is built is refused, never called", () => {
  const revoked = "the schema's function is a revoked Proxy";
  const notACodec = "the schema's codec is not a codec";
  const textEncode = (v: string): ReturnType<typeof codec.text.encode> => codec.text.encode(v);
  const textDecode = (v: Value): ReturnType<typeof codec.text.decode> => codec.text.decode(v);

  it("a leaf's functions", () => {
    const encode = Proxy.revocable(textEncode, {});
    const decode = Proxy.revocable(textDecode, {});
    const L = codec.leaf(encode.proxy, decode.proxy);
    expect(L.encode("a").ok, "before").toBe(true);
    encode.revoke();
    decode.revoke();
    expect(L.encode("a")).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: revoked } });
    expect(L.decode(textValue("a"))).toEqual({ ok: false, error: { path: [], reason: revoked } });
  });

  it("a reference's function, before and after it first resolves", () => {
    const early = Proxy.revocable(() => codec.text, {});
    const unresolved = codec.defer(early.proxy);
    early.revoke();
    expect(unresolved.encode("a")).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: revoked } });
    expect(unresolved.decode(textValue("a"))).toEqual({ ok: false, error: { path: [], reason: revoked } });
    const late = Proxy.revocable(() => codec.text, {});
    const resolved = codec.defer(late.proxy);
    expect(resolved.encode("a").ok, "resolved before the revocation").toBe(true);
    late.revoke();
    expect(resolved.encode("a").ok, "its target is kept").toBe(true);
    expect(resolved.decode(textValue("a"))).toEqual({ ok: true, value: "a" });
  });

  it("a hand-made codec, or its methods", () => {
    const whole = Proxy.revocable({ encode: textEncode, decode: textDecode, shape: codec.text.shape }, {});
    const method = Proxy.revocable(textEncode, {});
    const viaWhole = codec.vector(whole.proxy, 0, null);
    const viaMethod = codec.vector({ encode: method.proxy, decode: textDecode, shape: codec.text.shape }, 0, null);
    expect([viaWhole.encode(["a"]).ok, viaMethod.encode(["a"]).ok], "before").toEqual([true, true]);
    whole.revoke();
    method.revoke();
    expect(viaWhole.encode(["a"])).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: notACodec } });
    const decoded = viaWhole.decode(expectOk(Value.array([textValue("a")]), "an array"));
    expect(decoded.ok ? undefined : decoded.error.reason).toBe(notACodec);
    expect(viaMethod.encode(["a"])).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: notACodec } });
  });
});

describe("a reference resolves once, for good; a hand-made codec's methods are called as they are", () => {
  it("a resolution that returns nothing is kept, and refused, however the function's answer changes", () => {
    const target: { codec?: Codec<string> } = {};
    let calls = 0;
    const ref = codec.defer(() => {
      calls++;
      return bad(target.codec);
    });
    expect(ref.decode(textValue("a"))).toEqual({ ok: false, error: { path: [], reason: "the schema's codec is not a codec" } });
    Object.assign(target, { codec: codec.text });
    expect(ref.decode(textValue("a")).ok, "the first answer is kept").toBe(false);
    expect(ref.encode("a").ok).toBe(false);
    expect(calls, "the function is called once").toBe(1);
  });

  it("a resolution that throws resolves nothing, so the next use resolves", () => {
    const early: { readonly codec?: Codec<string> } = {};
    const ref = codec.defer(() => early.codec ?? bad((null as unknown as { x: never }).x));
    expect(() => ref.encode("a"), "the caller's own TypeError").toThrow(TypeError);
    Object.assign(early, { codec: codec.text });
    expect(ref.encode("a").ok).toBe(true);
  });

  it("a method whose own call property is replaced is still called", () => {
    const encode = (v: string): ReturnType<typeof codec.text.encode> => codec.text.encode(v);
    const decode = (v: Value): ReturnType<typeof codec.text.decode> => codec.text.decode(v);
    Object.assign(encode, { call: null, apply: null });
    Object.assign(decode, { call: null, apply: null });
    const items = codec.vector({ encode, decode, shape: codec.text.shape }, 0, null);
    const v = expectOk(items.encode(["a"]), "encodes");
    expect(items.decode(v)).toEqual({ ok: true, value: ["a"] });
  });
});

describe("a hand-made codec or leaf that gives what is not a Result is refused, never thrown on", () => {
  const notAResult = "a hand-made codec gave what is not a Result";
  const revoked = Proxy.revocable({ ok: true, value: Value.null }, {});
  revoked.revoke();
  const given: readonly (readonly [string, unknown])[] = [
    ["undefined", undefined],
    ["a number", 42],
    ['{ ok: "yes" }', { ok: "yes", value: Value.null }],
    ["{ ok: 1 }", { ok: 1, value: Value.null }],
    ["a revoked Proxy", revoked.proxy],
  ];
  const encodeRefusal = { ok: false, error: { kind: "TypeMismatch", reason: notAResult } };
  const decodeRefusal = { ok: false, error: { path: [], reason: notAResult } };
  const nestedDecodeRefusal = { ok: false, error: { path: [Preview.of("[0]")], reason: notAResult } };

  for (const [what, r] of given) {
    it(`${what}, from a leaf, a codec made otherwise, nested or at the root`, () => {
      const L = codec.leaf<string>(() => bad(r), () => bad(r));
      const H: Codec<string> = { encode: () => bad(r), decode: () => bad(r), shape: codec.text.shape };
      expect(L.encode("a"), "the leaf's own encode").toEqual(encodeRefusal);
      expect(L.decode(textValue("a")), "the leaf's own decode").toEqual(decodeRefusal);
      for (const [name, c] of [["a leaf", L], ["a codec made otherwise", H]] as const) {
        const items = codec.vector(c, 0, null);
        expect(items.encode(["a"]), `${name} in a vector, encoding`).toEqual(encodeRefusal);
        expect(items.decode(expectOk(Value.array([textValue("a")]), "an array")), `${name} in a vector, decoding`).toEqual(nestedDecodeRefusal);
        expect(codec.toCbor(c, "a"), `toCbor with ${name}`).toEqual(encodeRefusal);
        expect(codec.fromJson(c, '"a"'), `fromJson with ${name}`).toEqual({ ok: false, error: { kind: "Decode", error: decodeRefusal.error } });
        expect(codec.fromCbor(c, Uint8Array.of(0x61, 0x61)), `fromCbor with ${name}`).toEqual({ ok: false, error: { kind: "Decode", error: decodeRefusal.error } });
        expect(codec.decodeWithin(c, textValue("a"), 10n), `decodeWithin with ${name}`).toEqual(decodeRefusal);
      }
    });
  }

  it("a decode refusal that is not a decode failure, nested where its path is read", () => {
    const failures: readonly unknown[] = [null, 1, { reason: "r" }, { path: 1, reason: "r" }, { path: [], reason: 1 }, { path: ["x"], reason: "r" }, { path: { length: 1, 0: Preview.of("x") }, reason: "r" }];
    const array = expectOk(Value.array([textValue("a")]), "an array");
    for (const error of failures) {
      const items = codec.vector(codec.leaf<string>((s) => codec.text.encode(s), () => bad({ ok: false, error })), 0, null);
      expect(items.decode(array), JSON.stringify(error)).toEqual(nestedDecodeRefusal);
    }
  });

  it("an encode refusal that is no refusal (no string kind), and one that is, passed on as given", () => {
    for (const error of [null, 1, "TypeMismatch", {}, { kind: 1 }]) {
      const items = codec.vector(codec.leaf<string>(() => bad({ ok: false, error }), (v) => codec.text.decode(v)), 0, null);
      expect(items.encode(["a"]), JSON.stringify(error)).toEqual(encodeRefusal);
    }
    const own = { kind: "TypeMismatch", reason: "mine" };
    const L = codec.leaf<string>(() => bad({ ok: false, error: own }), (v) => codec.text.decode(v));
    const given = codec.vector(L, 0, null).encode(["a"]);
    expect(given.ok ? undefined : given.error, "the leaf's own refusal").toBe(own);
  });

  it("a Result is read once, its tag then its value or error, and passed on as given", () => {
    let reads = 0;
    const counted = (o: object): object =>
      new Proxy(o, {
        get: (t, k) => {
          reads++;
          return Reflect.get(t, k) as unknown;
        },
      });
    const L = codec.leaf<string>(
      (s) => bad(counted({ ok: true, value: expectOk(Value.text(s), "text") })),
      () => bad(counted({ ok: false, error: { path: [Preview.of("k")], reason: "mine" } })),
    );
    const items = codec.vector(L, 0, null);
    expect(items.encode(["a"])).toEqual({ ok: true, value: expectOk(Value.array([textValue("a")]), "an array") });
    expect(reads, "ok, then value").toBe(2);
    expect(items.decode(expectOk(Value.array([textValue("a")]), "an array"))).toEqual({ ok: false, error: { path: [Preview.of("[0]"), Preview.of("k")], reason: "mine" } });
    expect(reads, "ok, then error").toBe(4);
  });
});

describe("a reference that uses itself while it first resolves", () => {
  it("keeps the answer its inner resolution settled, for that use and every later one", () => {
    let calls = 0;
    const seen: unknown[] = [];
    const ref: Codec<unknown> = codec.defer((): Codec<unknown> => {
      calls++;
      if (calls === 1) {
        seen.push(ref.decode(int(3n)));
        return codec.text;
      }
      return codec.uint;
    });
    seen.push(ref.decode(int(3n)));
    seen.push(ref.decode(int(3n)));
    expect(seen, "the inner use, then two later ones").toEqual([{ ok: true, value: 3n }, { ok: true, value: 3n }, { ok: true, value: 3n }]);
    expect((ref.shape as CodecShape & { readonly kind: "defer" }).resolve(), "the inner answer").toBe(codec.uint);
    expect(calls).toBe(2);
  });
});
