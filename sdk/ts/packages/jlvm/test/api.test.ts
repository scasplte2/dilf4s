/**
 * The public API: typed refusals for arguments of the wrong type (`TypeMismatch`, never a throw), the canonical form
 * and the program's identity, and the surface parser's guarantees (`sdk/rust/jlvm/src/surface.rs` tests).
 */
import { cbor, models, Value } from "@kd5ujc/dilf4s-core";
import { describe, expect, it } from "vitest";

import { Bindings, compile, decode, evaluate, fromCanonical, parse, parseValue, SCHEDULE_V1, type Program } from "../src/index.js";
import { Spans, parseProgram } from "../src/surface.js";
import { expectErr, expectOk, hex } from "./vectors.js";

const program = (text: string): Program => expectOk(parse(text), text.slice(0, 60));
const data = expectOk(parseValue('{"x": 2}'), "data");

describe("arguments of the wrong type are TypeMismatch", () => {
  const typeMismatch = { kind: "TypeMismatch" };

  it("for every entry point", () => {
    const anything: unknown[] = [undefined, null, 1, 1n, "x", {}, [], () => 1, Symbol("s"), new Proxy({}, {})];
    for (const x of anything) {
      const v = x as never;
      if (typeof x !== "string") {
        expect(expectErr(parse(v), "parse")).toMatchObject(typeMismatch);
        expect(expectErr(parseValue(v), "parseValue")).toMatchObject(typeMismatch);
      }
      expect(expectErr(fromCanonical(v), "fromCanonical")).toMatchObject(typeMismatch);
      expect(expectErr(decode(v), "decode")).toMatchObject(typeMismatch);
      expect(expectErr(compile(v), "compile")).toMatchObject(typeMismatch);
      expect(expectErr(evaluate(v, data), "evaluate's program")).toMatchObject(typeMismatch);
      expect(expectErr(evaluate(program("1"), v), "evaluate's data")).toMatchObject(typeMismatch);
      if (x !== undefined) expect(expectErr(evaluate(program("1"), data, { env: v }), "evaluate's env")).toMatchObject(typeMismatch);
    }
  });

  it("for evaluation options: a gas limit out of range or not whole, an unpublished schedule, options that are no object", () => {
    const p = program("1");
    for (const gasLimit of [-1, 1.5, Number.NaN, Number.POSITIVE_INFINITY, 2 ** 53, "5", 5n]) {
      expect(expectErr(evaluate(p, data, { gasLimit: gasLimit as never }), `gas limit ${String(gasLimit)}`)).toMatchObject(typeMismatch);
    }
    expect(expectErr(evaluate(p, data, { schedule: { ...SCHEDULE_V1 } }), "a copied schedule")).toMatchObject(typeMismatch);
    expect(expectErr(evaluate(p, data, null as never), "null options")).toMatchObject(typeMismatch);
    // a revoked Proxy as the options, or options a getter revokes while they are read, are refused, never thrown on
    const revoked = Proxy.revocable({}, {});
    revoked.revoke();
    expect(expectErr(evaluate(p, data, revoked.proxy as never), "revoked options")).toMatchObject(typeMismatch);
    const selfRevoking = Proxy.revocable({ gasLimit: 100 }, {
      get: (t, k, r) => {
        if (k === "env") selfRevoking.revoke();
        return Reflect.get(t, k, r) as unknown;
      },
    });
    expect(expectErr(evaluate(p, data, selfRevoking.proxy), "options revoked by their own getter")).toMatchObject(typeMismatch);
    // each field is read once
    const reads: PropertyKey[] = [];
    const counting = new Proxy({ gasLimit: 100 }, { get: (t, k, r) => (reads.push(k), Reflect.get(t, k, r) as unknown) });
    expect(expectOk(evaluate(p, data, counting), "counted").gasUsed).toBe(10);
    expect(reads).toEqual(["env", "gasLimit", "schedule"]);
    expect(expectErr(evaluate(p, data, { env: Value.null as never }), "a non-map env")).toMatchObject(typeMismatch);
    expect(expectOk(evaluate(p, data, { gasLimit: Number.MAX_SAFE_INTEGER }), "the largest limit").gasUsed).toBe(10);
    expect(expectOk(evaluate(p, data, { gasLimit: 0 }), "no gas").result).toMatchObject({ ok: false, error: { kind: "OutOfGas", available: 0 } });
  });

  it("a program's identity refuses an algorithm that is not core's", () => {
    expect(expectErr(program("1").id("MD5" as never), "MD5")).toMatchObject(typeMismatch);
  });
});

describe("host bindings", () => {
  const run = (text: string, env?: Bindings): unknown => {
    const o = expectOk(evaluate(program(text), Value.null, env === undefined ? {} : { env }), text);
    return o.result.ok ? o.result.value : o.result.error.kind;
  };
  const five = expectOk(Value.int(5n), "5");
  const x = expectOk(Bindings.of([["x", Value.null]]), "x bound to null");

  it("bind a name to null, distinct from an unbound name, as Scala and Rust do", () => {
    expect(run('{"var": "$x"}', x)).toBe(Value.null);
    expect(run('{"var": "$x"}')).toBe("Malformed");
    expect(run('{"var": "$x"}', Bindings.none)).toBe("Malformed");
    // a default replaces a null read, bound or not
    expect(run('{"var": ["$x", 5]}', x)).toEqual(five);
    expect(run('{"var": ["$x", 5]}')).toEqual(five);
    expect(run('{"var": "$x.a"}', x)).toBe(Value.null);
  });

  it("refuse what is not a [name, value] entry, a name bound twice, and bindings JavaScript built", () => {
    const malformed: unknown[] = [{ [Symbol.iterator]: null }, { [Symbol.iterator]: () => 5 }, { [Symbol.iterator]: () => ({ next: 1 }) }, { [Symbol.iterator]: () => ({ next: () => 5 }) }];
    for (const [i, entries] of [[["x"]], [[1, Value.null]], [["x", null]], [["x", Value.null], ["x", five]], 5, null, "xy", ...malformed].entries()) {
      expect(expectErr(Bindings.of(entries as never), `case ${i.toString()}`), `case ${i.toString()}`).toMatchObject({ kind: "TypeMismatch" });
    }
    // done is read by truthiness, as for...of reads it: a finished result stops the walk and its value is never read
    const steps = (done: unknown): Iterable<readonly [string, Value]> => {
      let i = 0;
      const results = [{ done: false, value: ["a", five] }, { done, value: ["b", five] }, { done: true, value: undefined }];
      return { [Symbol.iterator]: () => ({ next: () => results[i++] }) } as unknown as Iterable<readonly [string, Value]>;
    };
    for (const [n, done] of [1, "yes", {}, [], -1, true].entries()) {
      const b = expectOk(Bindings.of(steps(done)), `truthy done ${n.toString()}`);
      expect(b.size, `truthy done ${n.toString()} finishes`).toBe(1);
      expect(b.get("b"), `truthy done ${n.toString()}: its value is not consumed`).toBeUndefined();
    }
    for (const done of [0, "", null, undefined, Number.NaN, false]) expect(expectOk(Bindings.of(steps(done)), `done ${String(done)}`).size, `done ${String(done)} goes on`).toBe(2);
    // a function is an object to the protocol, as for...of reads it
    const fn = Object.assign(() => 0, { [Symbol.iterator]: () => [["f", five]][Symbol.iterator]() });
    expect(expectOk(Bindings.of(fn as unknown as Iterable<readonly [string, Value]>), "a function iterable").get("f")).toEqual(five);
    // a revoked Proxy, as the entries, as an entry, or revoked by a getter while it is read, is refused, never thrown on
    const revoked = (): object => {
      const r = Proxy.revocable({}, {});
      r.revoke();
      return r.proxy;
    };
    const revokedArray = (): object => {
      const r = Proxy.revocable([], {});
      r.revoke();
      return r.proxy;
    };
    for (const [n, entries] of [revoked(), revokedArray(), [revokedArray()], [revoked()]].entries()) {
      expect(expectErr(Bindings.of(entries as never), `revoked ${n.toString()}`)).toMatchObject({ kind: "TypeMismatch" });
    }
    const selfRevoking = Proxy.revocable(["x", Value.null], {
      get: (t, k, r) => {
        if (k === "0") selfRevoking.revoke();
        return Reflect.get(t, k, r) as unknown;
      },
    });
    expect(expectErr(Bindings.of([selfRevoking.proxy] as never), "an entry revoked by its own trap")).toMatchObject({ kind: "TypeMismatch" });
    const B = Bindings as unknown as new (...args: unknown[]) => Bindings;
    expect(expectErr(evaluate(program("1"), data, { env: new B(Symbol("bindings"), [["x", Value.null]]) }), "a fake")).toMatchObject({ kind: "TypeMismatch" });
    expect(x.size).toBe(1);
    expect(x.get("y")).toBeUndefined();
  });
});

describe("programs", () => {
  it("evaluate alike whether compiled first or not, and a program that does not compile uses no gas", () => {
    const p = program('{"+": [1, {"var": "x"}]}');
    const a = expectOk(evaluate(p, data), "evaluates");
    const b = expectOk(evaluate(expectOk(compile(p), "compiles"), data), "evaluates compiled");
    expect(a).toEqual(b);
    expect(a.result).toEqual({ ok: true, value: expectOk(Value.int(3n), "3") });
    const bad = expectOk(evaluate(program('{"if": [true]}'), data), "evaluates");
    expect(bad).toMatchObject({ gasUsed: 0, result: { ok: false, error: { kind: "Malformed" } } });
  });

  it("round-trip through their canonical form and its encoding, with one identity", () => {
    for (const text of ['{"map": [{"var": "xs"}, {"+": [{"var": "$current"}, 1]}]}', '{"let": [{"b": 1, "a": 2}, {"var": "$a"}]}', '{"+": {"var": "xs"}}', '{"object": {"if": 1}}', '{"var": [["a.b", 0]]}', '[{"literal": "+"}, 1]', "null", '{"var": ["a", {"literal": [1]}]}']) {
      const p = program(text);
      const bytes = expectOk(p.encode(), "encodes");
      const back = expectOk(decode(bytes), "decodes");
      expect(hex(expectOk(back.encode(), "re-encodes")), text).toBe(hex(bytes));
      expect(hex(expectOk(back.id(), "an id")), text).toBe(hex(expectOk(p.id(), "an id")));
      expect(hex(expectOk(expectOk(fromCanonical(p.canonical()), "from its form").encode(), "encodes"))).toBe(hex(bytes));
    }
  });

  it("a canonical form no surface text writes is refused", () => {
    const form = (expr: unknown): Value => expectOk(models.jlvm.JlvmProgram.encode({ jlvm: 1n, expr: expr as models.jlvm.JlvmExpr }), "a form");
    const lit = (v: Value): unknown => ({ lit: v });
    const one = expectOk(Value.int(1n), "1");
    for (const [what, expr] of [
      ["a spread of a non-variadic operation", { spread: "-", arg: { var: "xs" } }],
      ["a spread of an argument list", { spread: "+", arg: { arr: [lit(one)] } }],
      ["a var path with an empty segment", { var: "a..b" }],
      ["an empty binding name", { var: "$" }],
    ] as const) {
      expect(expectErr(fromCanonical(form(expr)), what), what).toMatchObject({ kind: "Malformed" });
    }
    expect(expectErr(decode(Uint8Array.of(0xa0)), "an empty map is no program")).toMatchObject({ kind: "Malformed" });
    expect(expectErr(decode(Uint8Array.of(0xff)), "not CBOR")).toMatchObject({ kind: "Malformed" });
    expect(cbor.decode(Uint8Array.of(0xa0)).ok).toBe(true);
  });
});

describe("the surface parser", () => {
  it("keeps null fields, so a null field cannot turn a map expression into an operation", () => {
    const o = expectOk(evaluate(program('{"+": [1, 2], "x": null}'), data), "evaluates");
    expect(o.result.ok && o.result.value.kind === "map" && o.result.value.size).toBe(1);
  });

  it("refuses numbers past 10,000 digits, as literals and as paths", () => {
    const most = "1".repeat(10_000);
    expect(parse(`{"var": ${most}}`).ok, "a 10,000-digit path is read").toBe(true);
    expect(parse(`{"+": [${"9".repeat(10_000)}, 1]}`).ok, "10,000 nines are past 33219 bits").toBe(false);
    expect(parse(`{"+": [${most}, 1]}`).ok, "a 10,000-digit literal").toBe(true);
    expect(parse(`{"var": ${most}9}`).ok, "a 10,001-digit path is refused").toBe(false);
    expect(parse(`{"+": [${most}9, 1]}`).ok, "and a 10,001-digit literal").toBe(false);
    expect(parse(`{"var": 1e${"1".repeat(10_001)}}`).ok, "and a 10,001-digit exponent").toBe(false);
  });

  it("reads each span back once, not once per level: at most twice the text however deep it nests", () => {
    const texts = [
      `${"[".repeat(100)}1${"]".repeat(100)}`,
      `${'["x", '.repeat(100)}"a"${"]".repeat(100)}`,
      `${'{"var": '.repeat(120)}"a"${"}".repeat(120)}`,
      `${'{"var": ['.repeat(40)}"a"${", 1]}".repeat(40)}`,
    ];
    for (const text of texts) {
      const spans = new Spans(text);
      expect(parseProgram(text, spans).ok, text.slice(0, 40)).toBe(true);
      expect(spans.read, `${spans.read.toString()} units read back for ${text.length.toString()}`).toBeLessThanOrEqual(2 * text.length);
    }
  });

  it("reads 0x-hex strings as bytes, in programs and in data, and other strings as text", () => {
    const v = expectOk(parseValue('["0x00ff", "0x0", "0xAB", "0x", "x"]'), "data");
    expect(v.kind === "array" ? v.items.map((x) => x.kind) : []).toEqual(["bytes", "text", "text", "bytes", "text"]);
    const o = expectOk(evaluate(program('{"length": "0x00ff"}'), data), "evaluates");
    expect(o.result).toEqual({ ok: true, value: expectOk(Value.int(2n), "2") });
  });

  it("names the operation to use for a removed one, and refuses an array headed by a call's name", () => {
    for (const text of ['{"noop": []}', '{"mpt_verify": []}', '["+", 1, 2]', '["var", "a"]', '["noop"]']) expect(expectErr(parse(text), text)).toMatchObject({ kind: "Malformed" });
    expect(parse('[{"literal": "+"}, 1, 2]').ok).toBe(true);
    expect(parse('{"literal": ["+", 1, 2]}').ok).toBe(true);
  });

  it("refuses text too deep for the canonical parser as Limit for a program and Malformed for data", () => {
    const deep = `${"[".repeat(300)}${"]".repeat(300)}`;
    expect(expectErr(parse(deep), "a program")).toMatchObject({ kind: "Limit" });
    expect(expectErr(parseValue(deep), "data")).toMatchObject({ kind: "Malformed" });
  });
});
