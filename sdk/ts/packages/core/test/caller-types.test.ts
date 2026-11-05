import { runInNewContext } from "node:vm";

import { describe, expect, it } from "vitest";

import { codec, IntValue, json, Value } from "../src/index.js";
import { expectOk, int } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;
const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });

describe("what a caller passes past the types is refused, never thrown on or misread", () => {
  it("bytes from another realm, and a Buffer, are bytes; an object claiming to be one is not", () => {
    const foreign = runInNewContext("new Uint8Array([1, 2])") as Uint8Array;
    expect(foreign instanceof Uint8Array, "another realm's array is no instance of this realm's").toBe(false);
    const v = expectOk(codec.bytes.encode(foreign), "another realm's Uint8Array");
    expect(v.kind === "bytes" && [...v.bytes]).toEqual([1, 2]);
    expect(expectOk(codec.bytes.encode(Buffer.from([3])), "a Buffer").kind).toBe("bytes");
    const claims = { [Symbol.toStringTag]: "Uint8Array", length: 2, 0: 1, 1: 2 };
    expect(codec.bytes.encode(bad(claims))).toEqual(mismatch("expected a Uint8Array"));
    expect(codec.bytes.encode(bad(new DataView(new ArrayBuffer(2))))).toEqual(mismatch("expected a Uint8Array"));
    expect(codec.bytes.encode(bad(new Uint16Array(2)))).toEqual(mismatch("expected a Uint8Array"));
    const record = codec.record<{ readonly a: bigint }>({ a: codec.required("a", codec.uint) });
    const cbor = expectOk(codec.toCbor(record, { a: 1n }), "encodes");
    const foreignCbor = runInNewContext(`new Uint8Array([${cbor.join(",")}])`) as Uint8Array;
    expect(expectOk(codec.fromCbor(record, foreignCbor), "another realm's CBOR")).toEqual({ a: 1n });
    for (const x of [null, "a1", [161, 97, 97, 1]]) {
      expect(codec.fromCbor(record, bad(x)), JSON.stringify(x)).toEqual({ ok: false, error: { kind: "Canonical", error: { kind: "TypeMismatch", reason: "expected a Uint8Array" } } });
    }
  });

  it("an object borrowing a value's prototype, or a Proxy of a value, is not a value, and no trap runs", () => {
    const borrowed: unknown = Object.create(IntValue.prototype);
    expect(codec.any.encode(bad(borrowed))).toEqual(mismatch("expected a canonical value"));
    expect(json.renderWithin(bad(borrowed), 100)).toEqual(mismatch("expected a canonical value"));
    const traps: string[] = [];
    const logAll = new Proxy(
      {},
      {
        get: (_t, name) => () => {
          traps.push(String(name));
          return undefined;
        },
      },
    );
    const proxy: unknown = new Proxy(int(5n), logAll);
    expect(codec.any.encode(bad(proxy))).toEqual(mismatch("expected a canonical value"));
    expect(codec.uint.decode(bad(proxy))).toEqual({ ok: false, error: { path: [], reason: "expected a canonical value" } });
    expect(traps).toEqual([]);
    // a real value still is one
    const real = expectOk(codec.any.encode(int(5n)), "a value");
    expect(real.kind === "int" && real.value).toBe(5n);
  });

  it("text and JSON entry points refuse what is not text, and a scale bound that is not a safe integer from 0", () => {
    expect(Value.text(bad(5))).toEqual(mismatch("expected a string"));
    for (const x of [null, 5, undefined]) expect(json.parse(bad(x)), String(x)).toEqual(mismatch("the JSON text is not a string"));
    for (const scale of [Number.NaN, -1, 1.5, Number.POSITIVE_INFINITY]) {
      expect(json.parse("1.5", scale), String(scale)).toEqual(mismatch("the scale bound is not a safe integer from 0"));
    }
    expect(expectOk(json.parse("1.5", 1), "within the bound").kind).toBe("rational");
    const record = codec.record<{ readonly a: bigint }>({ a: codec.required("a", codec.uint) });
    expect(codec.fromJson(record, bad(5))).toEqual({ ok: false, error: { kind: "Canonical", error: { kind: "TypeMismatch", reason: "the JSON text is not a string" } } });
  });
});
