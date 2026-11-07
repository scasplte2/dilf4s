import { runInNewContext } from "node:vm";

import { describe, expect, it } from "vitest";

import { bitLength, BoolValue, BytesValue, cbor, codec, decodeUtf8, describeFailure, equals, hashCommitment, inspect, IntValue, json, MapValue, Preview, RationalValue, TextValue, utf8Length, Value, valueCommitment, valueCommitmentWithin } from "../src/index.js";
import { bytesView, gcdSteps, NOT_A_VALUE } from "../src/value.js";
import { expectOk, int } from "./vectors.js";

/** What a JavaScript caller, or a cast, can pass past the types. */
const bad = (x: unknown): never => x as never;
const mismatch = (reason: string): unknown => ({ ok: false, error: { kind: "TypeMismatch", reason } });
/** A test label for anything a caller may pass: an object is not read, so a Proxy's traps do not run for it. */
const label = (x: unknown): string => (typeof x === "object" && x !== null ? `object ${Array.isArray(x) ? "array" : "non-array"}` : typeof x === "symbol" ? x.toString() : `${typeof x} ${String(x)}`);

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

  it("a rational made from parts that are not bigints is refused: never thrown on, converted or looped on", { timeout: 5_000 }, () => {
    const valueOf = { valueOf: () => 3n };
    for (const [n, d] of [
      [1, 2],
      ["1", "3"],
      [true, 2n],
      [2 ** 53 + 1, 1n],
      [1.5, 2n],
      [1n, 0],
      [valueOf, 2n],
      [1n, valueOf],
    ] as const) {
      expect(Value.number(bad(n), bad(d)), `${String(n)}/${String(d)}`).toEqual(mismatch("expected a bigint"));
      expect(RationalValue.number(bad(n), bad(d)), `${String(n)}/${String(d)}`).toEqual(mismatch("expected a bigint"));
    }
    expect(Value.number(1n, 0n)).toEqual({ ok: false, error: { kind: "DivisionByZero" } });
    expect(RationalValue.of(bad(1), bad(2))).toBeUndefined();
    expect(RationalValue.of(1n, bad(2))).toBeUndefined();
    // gcdSteps alone reads its operands as bigints, so no input can loop
    expect(gcdSteps(bad(6), bad(4))[0]).toBe(2n);
    expect(() => gcdSteps(bad(1.5), 2n)).toThrow(RangeError);
  });

  it("a decimal far past the width is refused from its scale, before any power of five is built", { timeout: 5_000 }, () => {
    expect(RationalValue.decimal(1n, Number.MAX_SAFE_INTEGER)).toEqual({ ok: false, error: { kind: "RationalTooWide", limit: 33219 } });
    expect(RationalValue.decimal(1n, 33219 + 2)).toEqual({ ok: false, error: { kind: "RationalTooWide", limit: 33219 } });
    // within reach of the check, the walk decides: 2^33218 / 10^33219 is 1 / (2 5^33219), too wide by its fives
    expect(RationalValue.decimal(1n << 33218n, 33219).ok).toBe(false);
    expect(expectOk(RationalValue.decimal(5n, 1), "0.5").kind).toBe("rational");
  });

  it("a decimal's scale with no integer value, or a part of the wrong type, is refused, a negative scale multiplies, and nothing throws", { timeout: 5_000 }, () => {
    const intTooWide = { ok: false, error: { kind: "IntegerTooWide", limit: 33219 } };
    for (const scale of [Number.NaN, 0.5, -0.5, Number.POSITIVE_INFINITY, Number.NEGATIVE_INFINITY, 1n, "1", null]) {
      expect(RationalValue.decimal(5n, bad(scale)), String(scale)).toEqual(mismatch("the scale is not an integer"));
    }
    for (const unscaled of [5, "5", null, 5.5]) expect(RationalValue.decimal(bad(unscaled), 1), String(unscaled)).toEqual(mismatch("expected a bigint"));
    const fifty = expectOk(RationalValue.decimal(5n, -1), "5 * 10");
    expect(fifty.kind === "int" && fifty.value).toBe(50n);
    expect(RationalValue.decimal(5n, -10_000)).toEqual(intTooWide);
    expect(RationalValue.decimal(5n, -Number.MAX_SAFE_INTEGER)).toEqual(intTooWide);
    const zero = expectOk(RationalValue.decimal(0n, -1), "0");
    expect(zero.kind === "int" && zero.value).toBe(0n);
  });

  it("a value built by calling a constructor from outside is not a value", () => {
    const fakeInt: unknown = Reflect.construct(IntValue as unknown as new (...args: unknown[]) => object, [Symbol("canonical value"), 5, 3]);
    const fakeMap: unknown = Reflect.construct(MapValue as unknown as new (...args: unknown[]) => object, [Symbol("canonical value"), [], 1, 1n]);
    for (const fake of [fakeInt, fakeMap]) {
      expect(codec.any.encode(bad(fake))).toEqual(mismatch("expected a canonical value"));
      expect(codec.uint.decode(bad(fake))).toEqual({ ok: false, error: { path: [], reason: "expected a canonical value" } });
      expect(json.renderWithin(bad(fake), 100)).toEqual(mismatch("expected a canonical value"));
    }
  });

  it("text and JSON entry points refuse what is not text, and a scale bound that is not an integer from 0 to the spec's", { timeout: 5_000 }, () => {
    expect(Value.text(bad(5))).toEqual(mismatch("expected a string"));
    for (const x of [null, 5, undefined]) expect(json.parse(bad(x)), String(x)).toEqual(mismatch("the JSON text is not a string"));
    for (const scale of [Number.NaN, -1, 1.5, Number.POSITIVE_INFINITY, json.DEFAULT_MAX_SCALE + 1, Number.MAX_SAFE_INTEGER]) {
      expect(json.parse("1.5", scale), String(scale)).toEqual(mismatch(`the scale bound is not an integer from 0 to ${json.DEFAULT_MAX_SCALE.toString()}`));
    }
    expect(expectOk(json.parse("1.5", 1), "within the bound").kind).toBe("rational");
    expect(json.parse("1.5", json.DEFAULT_MAX_SCALE).ok, "the spec's bound itself is a bound").toBe(true);
    const record = codec.record<{ readonly a: bigint }>({ a: codec.required("a", codec.uint) });
    expect(codec.fromJson(record, bad(5))).toEqual({ ok: false, error: { kind: "Canonical", error: { kind: "TypeMismatch", reason: "the JSON text is not a string" } } });
  });
});

describe("the constructors refuse a value of the wrong type, never reading it as another type", () => {
  it("an integer is a bigint: a number, however integral, is refused, not kept as a number", () => {
    for (const x of [5, 5.5, 2 ** 53, "5", true, null, undefined, {}, { valueOf: () => 5n }]) {
      expect(Value.int(bad(x)), label(x)).toEqual(mismatch("expected a bigint"));
      expect(IntValue.of(bad(x)), label(x)).toEqual(mismatch("expected a bigint"));
    }
    const five = expectOk(Value.int(5n), "5n");
    expect(typeof five.value).toBe("bigint");
  });

  it("a boolean is a boolean: a truthy or falsy value is refused, not read as a condition", () => {
    for (const x of [1, 0, "true", "", null, undefined, {}, new Boolean(true)]) {
      expect(Value.bool(bad(x)), label(x)).toEqual(mismatch("expected a boolean"));
      expect(BoolValue.of(bad(x)), label(x)).toEqual(mismatch("expected a boolean"));
    }
    expect(expectOk(Value.bool(false), "false")).toBe(BoolValue.false);
    expect(expectOk(BoolValue.of(true), "true")).toBe(BoolValue.true);
  });

  it("bytes are a Uint8Array, from any realm: a string, a number or an array-like is refused, not copied as zeros or nothing", () => {
    const claims = { [Symbol.toStringTag]: "Uint8Array", length: 2, 0: 1, 1: 2 };
    for (const x of ["ab", 5, [1, 2], { length: 2 }, claims, null, undefined, new Uint16Array(2), new DataView(new ArrayBuffer(2)), new ArrayBuffer(2)]) {
      expect(Value.bytes(bad(x)), label(x)).toEqual(mismatch("expected a Uint8Array"));
      expect(BytesValue.of(bad(x)), label(x)).toEqual(mismatch("expected a Uint8Array"));
    }
    const foreign = runInNewContext("new Uint8Array([1, 2])") as Uint8Array;
    expect([...expectOk(Value.bytes(foreign), "another realm's Uint8Array").bytes]).toEqual([1, 2]);
    expect([...expectOk(BytesValue.of(Buffer.from([3])), "a Buffer").bytes]).toEqual([3]);
  });

  it("text is a string, through TextValue.of as through Value.text", () => {
    for (const x of [5, null, undefined, {}, ["a"], Symbol("a")]) {
      expect(Value.text(bad(x)), label(x)).toEqual(mismatch("expected a string"));
      expect(TextValue.of(bad(x)), label(x)).toEqual(mismatch("expected a string"));
    }
    expect(TextValue.of("\uD800")).toEqual({ ok: false, error: { kind: "UnpairedSurrogate" } });
  });
});

describe("a Proxy of an array reporting a length no array has is refused, never looped over", () => {
  it("Value.array, a codec's vector, describeFailure and hashCommitment's prefix", { timeout: 5_000 }, () => {
    const one = int(1n);
    for (const length of [Number.POSITIVE_INFINITY, 2 ** 53, 1.5, -1]) {
      const proxy = (item: unknown): never =>
        bad(new Proxy([], { get: (t, name) => (name === "length" ? length : typeof name === "string" && /^[0-9]+$/.test(name) ? item : (Reflect.get(t, name) as unknown)) }));
      expect(Value.array(proxy(one)), String(length)).toEqual(mismatch("expected an array"));
      expect(codec.vector(codec.uint, 0, null).encode(proxy(1n)), String(length)).toEqual(mismatch("expected an array"));
      expect(describeFailure(bad({ path: proxy(Preview.of("a")), reason: "r" })), String(length)).toBe("<not a decode failure>");
      expect(hashCommitment("SHA-256", proxy(Uint8Array.of(1)), one), String(length)).toEqual(mismatch("expected an array of Uint8Arrays"));
      // a Proxy of an array walks as an array: its length is read each step, and refused when no array has it
      const entries = new Proxy([["a", one]], { get: (t, name) => (name === "length" ? length : (Reflect.get(t, name) as unknown)) });
      expect(Value.map(bad(entries)), String(length)).toEqual(mismatch("expected an iterable of [key, value] entries"));
    }
  });
});

describe("an array's items and a map's entries are checked in the pass that reads them", () => {
  const one = int(1n);

  it("an array is an array, and each item a value, found at its index", () => {
    for (const x of ["ab", 5, null, undefined, { length: 1, 0: one }, new Set([one])]) expect(Value.array(bad(x)), label(x)).toEqual(mismatch("expected an array"));
    expect(Value.array(bad([one, 5]))).toEqual(mismatch("expected a canonical value at index 1"));
    const holey: unknown[] = [one];
    holey[2] = one;
    expect(Value.array(bad(holey))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(Value.array(bad([Object.create(IntValue.prototype)]))).toEqual(mismatch("expected a canonical value at index 0"));
    const traps: string[] = [];
    const proxy: unknown = new Proxy(one, { get: (_t, name) => traps.push(String(name)) });
    expect(Value.array(bad([one, proxy]))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(traps, "a Proxy of a value is not one, and none of its traps runs").toEqual([]);
    const ok = expectOk(Value.array([one, one]), "two values");
    expect(ok.items).toEqual([one, one]);
  });

  it("an array is read by index, its length once and each item once, never through its iterator, up to the first refused item", () => {
    const reads: string[] = [];
    const items = [one, one, 5, one];
    const watched = new Proxy(items, {
      get: (t, name, receiver) => {
        reads.push(String(name));
        return Reflect.get(t, name, receiver) as unknown;
      },
    });
    expect(Value.array(bad(watched))).toEqual(mismatch("expected a canonical value at index 2"));
    expect(reads).toEqual(["length", "0", "1", "2"]);
    reads.length = 0;
    items[2] = one;
    expect(expectOk(Value.array(bad(watched)), "four values").length).toBe(4);
    expect(reads, "n + 1 reads for n items").toEqual(["length", "0", "1", "2", "3"]);
    const notALength = new Proxy([one], { get: (t, name) => (name === "length" ? Symbol("length") : (Reflect.get(t, name) as unknown)) });
    expect(Value.array(bad(notALength))).toEqual(mismatch("expected an array"));
  });

  it("a map's entries are an iterable of [string, value] arrays, each checked as it is read", () => {
    for (const x of [5, "ab", null, undefined]) expect(Value.map(bad(x)), label(x)).toEqual(mismatch("expected an iterable of [key, value] entries"));
    // an array is read through its own iterator when it has one, as for...of reads it
    const own = Object.assign([["a", one]], { [Symbol.iterator]: () => [["b", one]][Symbol.iterator]() });
    expect(expectOk(Value.map(bad(own)), "an array with its own iterator").keys()).toEqual(["b"]);
    // a function with an iterator is iterable, as for...of and Iterable<T> accept it
    const fn = Object.assign(() => undefined, { [Symbol.iterator]: () => [["a", one]][Symbol.iterator]() });
    expect(expectOk(Value.map(bad(fn)), "an iterable function").keys()).toEqual(["a"]);
    const notOpened = "expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)";
    for (const x of [{}, { [Symbol.iterator]: 5 }, { [Symbol.iterator]: () => 5 }, { [Symbol.iterator]: () => ({ next: 5 }) }]) {
      expect(Value.map(bad(x))).toEqual(mismatch(notOpened));
    }
    const notAResult = { [Symbol.iterator]: () => ({ next: () => 7 }) };
    expect(Value.map(bad(notAResult))).toEqual(mismatch("an iterator's next() gave a result that is not an object"));
    expect(Value.map(bad([["a", one], 5]))).toEqual(mismatch("expected a [key, value] array at index 1"));
    expect(Value.map(bad([["a", one], new Set(["b", one])]))).toEqual(mismatch("expected a [key, value] array at index 1"));
    expect(Value.map(bad([[1, one]]))).toEqual(mismatch("expected a string key at index 0"));
    expect(Value.map(bad([["a", 1n]]))).toEqual(mismatch("expected a canonical value at index 0"));
    expect(Value.map(bad([["a", undefined]]))).toEqual(mismatch("expected a canonical value at index 0"));
    const m = expectOk(Value.map(new Map<string, Value>([["a", one], ["b", Value.null]])), "a Map");
    expect(m.keys(), "a null value is dropped").toEqual(["a"]);
  });

  it("when caller code throws reading an entry and return() throws too, return's error surfaces, unlike for...of", () => {
    const closed: string[] = [];
    const it = {
      next: () => ({
        done: false,
        value: Object.defineProperty([], 0, {
          get: () => {
            throw new Error("caller code threw");
          },
        }),
      }),
      return: () => {
        closed.push("return");
        throw new Error("return threw");
      },
    };
    expect(() => Value.map(bad({ [Symbol.iterator]: () => it }))).toThrow("return threw");
    expect(closed).toEqual(["return"]);
  });

  it("a map's iterator is closed when an entry is refused or caller code throws reading one", () => {
    const log: string[] = [];
    function* entries(xs: readonly unknown[]): Generator {
      try {
        yield* xs;
      } finally {
        log.push("closed");
      }
    }
    expect(Value.map(bad(entries([["a", one], ["b", 5]])))).toEqual(mismatch("expected a canonical value at index 1"));
    expect(log, "refused entry").toEqual(["closed"]);
    log.length = 0;
    expect(Value.map(bad(entries([["a", one], ["a", one]])))).toEqual({ ok: false, error: { kind: "DuplicateKey", key: expect.anything() as unknown } });
    expect(log, "duplicate key").toEqual(["closed"]);
    log.length = 0;
    const throwing = Object.defineProperty([], 0, {
      get: () => {
        throw new Error("caller code threw");
      },
    });
    expect(() => Value.map(bad(entries([throwing])))).toThrow("caller code threw");
    expect(log, "caller code throwing while an entry is read").toEqual(["closed"]);
    log.length = 0;
    expect(expectOk(Value.map(bad(entries([["a", one]]))), "ends").size).toBe(1);
    expect(log, "ended normally: the generator's own finally").toEqual(["closed"]);
    // next() throwing, or a result that is not an object, does not close (for...of does not)
    const closes: string[] = [];
    const throwingNext = { [Symbol.iterator]: () => ({ next: () => { throw new Error("next threw"); }, return: () => closes.push("return") }) };
    expect(() => Value.map(bad(throwingNext))).toThrow("next threw");
    const sevens = { [Symbol.iterator]: () => ({ next: () => 7, return: () => closes.push("return") }) };
    expect(Value.map(bad(sevens))).toEqual(mismatch("an iterator's next() gave a result that is not an object"));
    expect(closes).toEqual([]);
  });
});

describe("bytes are read through the array's own slots, never its overridable properties", () => {
  /** A subclass and an instance whose `length` lies, as any caller can make one. */
  class Liar extends Uint8Array {}
  Object.defineProperty(Liar.prototype, "length", { get: () => 32 });
  const liars = (xs: readonly number[]): readonly Uint8Array[] => [new Liar(xs), Object.defineProperty(Uint8Array.from(xs), "length", { get: () => 32 })];

  it("a lying length is not believed by the constructors, the codecs or the decoder", () => {
    for (const [i, liar] of liars([1, 2]).entries()) {
      expect(liar.length, `liar ${i.toString()}`).toBe(32);
      const v = expectOk(Value.bytes(liar), "a lying length");
      expect([...v.bytes], `liar ${i.toString()}`).toEqual([1, 2]);
      expect(v.length).toBe(2);
      expect(codec.sizedBytes(32, 32).encode(liar), `liar ${i.toString()}`).toEqual(mismatch("byte string of 2 bytes outside the schema size"));
    }
    const record = codec.record<{ readonly a: Uint8Array }>({ a: codec.required("a", codec.sizedBytes(1, 1)) });
    for (const [i, liar] of liars([0x41, 0x07]).entries()) {
      const d = expectOk(cbor.decode(liar), `liar ${i.toString()}`);
      expect(d.kind === "bytes" && [...d.bytes]).toEqual([7]);
    }
    // {"a": h'07'}: read to its true end, never past it, and nothing after it believed
    for (const [i, liar] of liars([0xa1, 0x61, 0x61, 0x41, 0x07]).entries()) {
      expect([...expectOk(codec.fromCbor(record, liar), `liar ${i.toString()}`).a]).toEqual([7]);
    }
  });

  it("a detached array is read as empty, never thrown on", () => {
    const buffer = new ArrayBuffer(2);
    const detached = new Uint8Array(buffer);
    structuredClone(buffer, { transfer: [buffer] });
    expect(expectOk(Value.bytes(detached), "detached").length).toBe(0);
    expect(cbor.decode(detached)).toEqual({ ok: false, error: { kind: "UnexpectedEnd", offset: 0 } });
  });

  it("cbor.decode refuses what is not bytes, and a depth limit that is not a number", () => {
    for (const x of ["ab", null, undefined, [0x01], { length: 1, 0: 1 }]) expect(cbor.decode(bad(x)), label(x)).toEqual(mismatch("expected a Uint8Array"));
    for (const depth of ["1", Number.NaN, 1n, null]) expect(cbor.decode(Uint8Array.of(0x80), bad(depth)), label(depth)).toEqual(mismatch("the depth limit is not a number"));
    expect(expectOk(cbor.decode(Uint8Array.of(0x80), Number.POSITIVE_INFINITY), "an unbounded limit is the largest").kind).toBe("array");
  });
});

describe("a decimal is of a rational this package made", () => {
  it("json.decimal refuses what is not a RationalValue, never reading a numerator and denominator from it", () => {
    const quarter = expectOk(Value.number(1n, 4n), "1/4");
    expect(json.decimal(bad(quarter))).toEqual({ ok: true, value: "0.25" });
    const third = expectOk(Value.number(1n, 3n), "1/3");
    expect(json.decimal(bad(third))).toEqual({ ok: false, error: { kind: "NotJsonRepresentable", what: "non-terminating rational" } });
    const lookalike = { kind: "rational", numerator: 1n, denominator: 4n };
    for (const x of [5, null, undefined, "0.25", lookalike, Object.create(RationalValue.prototype), int(1n)]) {
      expect(json.decimal(bad(x)), label(x)).toEqual(mismatch("expected a RationalValue"));
    }
  });
});

describe("the encoding and commitment entry points refuse what is not a value, or a limit that is not a number", () => {
  const v = int(5n);
  const b = expectOk(Value.bytes(Uint8Array.of(1, 2)), "bytes");
  const notAValue = mismatch("expected a canonical value");
  const notALimit = mismatch("the byte limit is not a number");

  it("a value that is not one is refused before it is read, by every entry point that encodes", () => {
    const traps: string[] = [];
    const proxy: unknown = new Proxy(v, { get: (_t, name) => traps.push(String(name)) });
    const fake: unknown = Reflect.construct(IntValue as unknown as new (...args: unknown[]) => object, [Symbol("canonical value"), 5, 3]);
    for (const x of [5, 5n, null, undefined, "a", { kind: "int", value: 5n, weight: 1n, depth: 0 }, Object.create(IntValue.prototype), proxy, fake]) {
      expect(cbor.encode(bad(x)), label(x)).toEqual(notAValue);
      expect(cbor.encodeWithin(bad(x), 100), label(x)).toEqual(notAValue);
      expect(cbor.encodedLengthWithin(bad(x), 100), label(x)).toEqual(notAValue);
      expect(valueCommitment(bad(x)), label(x)).toEqual(notAValue);
      expect(valueCommitmentWithin(bad(x), 100), label(x)).toEqual(notAValue);
      expect(hashCommitment("SHA-256", [], bad(x)), label(x)).toEqual(notAValue);
    }
    expect(traps, "a Proxy of a value is not one, and none of its traps runs").toEqual([]);
  });

  it("a byte limit that is not a number is refused, for a byte string's commitment too; an odd number is still a limit", () => {
    for (const limit of ["100", 100n, null, undefined, {}, [100]]) {
      expect(cbor.encodeWithin(v, bad(limit)), label(limit)).toEqual(notALimit);
      expect(cbor.encodedLengthWithin(v, bad(limit)), label(limit)).toEqual(notALimit);
      expect(valueCommitmentWithin(v, bad(limit)), label(limit)).toEqual(notALimit);
      expect(valueCommitmentWithin(b, bad(limit)), label(limit)).toEqual(notALimit);
    }
    expect(cbor.encodeWithin(v, Number.NaN)).toEqual({ ok: false, error: { kind: "TooLarge", limit: 0 } });
    expect(expectOk(cbor.encodedLengthWithin(v, Number.POSITIVE_INFINITY), "the largest limit")).toBe(1);
  });

  it("hashCommitment refuses an algorithm or a prefix of the wrong type before hashing, and reads prefix parts by their own length", () => {
    for (const algorithm of ["MD5", "sha-256", undefined, null, 5]) {
      expect(hashCommitment(bad(algorithm), [], v), label(algorithm)).toEqual(mismatch("expected a HashAlgorithm"));
    }
    for (const prefix of [null, undefined, "ab", Uint8Array.of(1), [5], [Uint8Array.of(1), "ab"], [new Uint16Array(1)], new Set([Uint8Array.of(1)])]) {
      expect(hashCommitment("SHA-256", bad(prefix), v), label(prefix)).toEqual(mismatch("expected an array of Uint8Arrays"));
    }
    const liar = Object.defineProperty(Uint8Array.of(1, 2), "length", { get: () => 32 });
    const honest = expectOk(hashCommitment("SHA-256", [Uint8Array.of(1, 2)], v), "honest prefix");
    expect(expectOk(hashCommitment("SHA-256", [liar], v), "a lying prefix part")).toEqual(honest);
    const foreign = runInNewContext("[new Uint8Array([1, 2])]") as Uint8Array[];
    expect(expectOk(hashCommitment("SHA-256", foreign, v), "another realm's array and bytes")).toEqual(honest);
  });
});

describe("the public byte helpers check what they are given", () => {
  it("decodeUtf8 reads a Uint8Array by its own length, and is undefined for anything else", () => {
    for (const x of [5, "ab", null, [0x61], { length: 1, 0: 0x61 }]) expect(decodeUtf8(bad(x)), label(x)).toBeUndefined();
    const longer = Object.defineProperty(Uint8Array.of(0x61, 0x62), "length", { get: () => 32 });
    const shorter = Object.defineProperty(Uint8Array.of(0x61, 0x62), "length", { get: () => 1 });
    expect(decodeUtf8(longer)).toBe("ab");
    expect(decodeUtf8(shorter)).toBe("ab");
  });

  it("magnitudeBytes refuses what is not a non-negative bigint, never reading a negative one as zero", () => {
    for (const x of [5, "5", null, -1n, -(2n ** 70n)]) expect(cbor.magnitudeBytes(bad(x)), label(x)).toEqual(mismatch("expected a non-negative bigint"));
    expect([...expectOk(cbor.magnitudeBytes(0n), "zero")]).toEqual([]);
    expect([...expectOk(cbor.magnitudeBytes(256n), "256")]).toEqual([1, 0]);
  });
});

describe("equality and inspection of what is not a value", () => {
  const traps: string[] = [];
  const proxy: unknown = new Proxy(int(5n), { get: (_t, name) => traps.push(String(name)) });
  const notValues: readonly unknown[] = [5, 5n, null, undefined, "a", { kind: "int", value: 5n, weight: 1n, depth: 0 }, Object.create(IntValue.prototype), proxy];

  it("a non-value equals nothing, itself included, and nothing of it is read", () => {
    for (const x of notValues) {
      expect(Value.equals(bad(x), int(5n)), label(x)).toBe(false);
      expect(Value.equals(int(5n), bad(x)), label(x)).toBe(false);
      expect(equals(bad(x), bad(x)), `${label(x)} with itself`).toBe(false);
    }
    expect(traps).toEqual([]);
    expect(equals(int(5n), int(5n))).toBe(true);
  });

  it("a non-value is inspected as a fixed placeholder, never read or stringified", () => {
    const loud = { toString: () => traps.push("toString"), [Symbol.toPrimitive]: () => traps.push("toPrimitive") };
    for (const x of [...notValues, loud]) expect(inspect(bad(x)), label(x)).toBe(NOT_A_VALUE);
    expect(NOT_A_VALUE).toBe("<not a canonical value>");
    expect(traps).toEqual([]);
    expect(inspect(int(5n))).toBe("5");
  });
});

describe("cbor.decode reads its input in place, or copies it once when it may change", () => {
  const encoded = [0x82, 0x42, 0x01, 0x02, 0x61, 0x61]; // [h'0102', "a"]

  it("a decoded byte string holds no view of the input", () => {
    const input = Uint8Array.from(encoded);
    const v = expectOk(cbor.decode(input), "decodes");
    input.fill(0);
    const first = v.kind === "array" ? v.items[0] : undefined;
    expect(first?.kind === "bytes" && [...first.bytes]).toEqual([1, 2]);
  });

  it("a SharedArrayBuffer's or another realm's bytes decode as the same value", () => {
    const shared = new Uint8Array(new SharedArrayBuffer(encoded.length));
    shared.set(encoded);
    const foreign = runInNewContext(`new Uint8Array([${encoded.join(",")}])`) as Uint8Array;
    const expected = expectOk(cbor.decode(Uint8Array.from(encoded)), "local");
    expect(Value.equals(expectOk(cbor.decode(shared), "shared"), expected)).toBe(true);
    expect(Value.equals(expectOk(cbor.decode(foreign), "another realm"), expected)).toBe(true);
    // a view into a larger buffer is read from its own offset and length
    const larger = new Uint8Array(16).fill(0xff);
    larger.set(encoded, 5);
    expect(Value.equals(expectOk(cbor.decode(larger.subarray(5, 5 + encoded.length)), "a view"), expected)).toBe(true);
  });
});

describe("the remaining exports check what they are given", () => {
  it("a map lookup by a key that is not text finds nothing, and reads nothing", () => {
    const m = expectOk(Value.map([["5", int(5n)]]), "a map");
    const read: string[] = [];
    const iterable = { [Symbol.iterator]: () => read.push("iterator"), toString: () => read.push("toString") };
    for (const k of [5, null, undefined, iterable, ["5"]]) expect(m.get(bad(k)), label(k)).toBeUndefined();
    expect(read).toEqual([]);
    expect(m.get("5")).toBe(m.entries()[0]?.[1]);
  });

  it("bitLength and utf8Length refuse what is not a bigint or a string, never converting it", () => {
    const converted: string[] = [];
    const valueOf = { valueOf: () => converted.push("valueOf") };
    for (const x of [5, 5.5, "5", null, valueOf]) expect(bitLength(bad(x)), label(x)).toEqual(mismatch("expected a bigint"));
    for (const x of [5, null, ["a"], { [Symbol.iterator]: () => converted.push("iterator") }]) expect(utf8Length(bad(x)), label(x)).toEqual(mismatch("expected a string"));
    expect(converted).toEqual([]);
    expect(bitLength(255n)).toEqual({ ok: true, value: 8 });
    expect(utf8Length("é")).toEqual({ ok: true, value: 2 });
  });

  it("describeFailure gives a fixed text for what is not a decode failure, reading its path by index", () => {
    for (const x of [5, null, "f", {}, { path: "a", reason: "r" }, { path: [], reason: 5 }, { path: ["a"], reason: "r" }, { path: [{ short: () => "x" }], reason: "r" }]) {
      expect(describeFailure(bad(x)), label(x)).toBe("<not a decode failure>");
    }
    expect(describeFailure({ path: [], reason: "r" })).toBe("<root>: r");
    expect(describeFailure({ path: [Preview.of("a"), Preview.of("b")], reason: "r" })).toBe("a.b: r");
  });

  it("Preview.of gives a fixed preview for what is not text, never reading it", () => {
    const read: string[] = [];
    for (const x of [5, null, undefined, { length: 3, codePointAt: () => read.push("codePointAt") }]) {
      const p = Preview.of(bad(x));
      expect([p.head, p.length, p.truncated], label(x)).toEqual(["<not text>", 0, false]);
    }
    expect(read).toEqual([]);
    expect(Preview.of("<not text>").length, "the text itself has a length").toBe(10);
  });

  it("json.renderInto refuses a sink that is not a function before writing", () => {
    for (const sink of [null, 5, {}, "f"]) expect(json.renderInto(int(1n), 100, [], bad(sink)), label(sink)).toEqual(mismatch("the sink is not a function"));
  });

  it("a hashCommitment prefix part is copied when it is checked, so a later change cannot reach the hash", () => {
    const [first, second] = [Uint8Array.of(1, 2), Uint8Array.of(3, 4)];
    const reads: string[] = [];
    // reading the second part changes the first, after the first was checked
    const watched = new Proxy([first, second], {
      get: (t, name) => {
        if (name === "0" || name === "1") reads.push(name);
        if (name === "1") first.fill(9);
        return Reflect.get(t, name) as unknown;
      },
    });
    const honest = expectOk(hashCommitment("SHA-256", [Uint8Array.of(1, 2), Uint8Array.of(3, 4)], int(1n)), "the bytes as checked");
    const changed = expectOk(hashCommitment("SHA-256", [Uint8Array.of(9, 9), Uint8Array.of(3, 4)], int(1n)), "the bytes as changed");
    expect(expectOk(hashCommitment("SHA-256", watched, int(1n)), "watched"), "the first part as it was when checked").toEqual(honest);
    expect(honest).not.toEqual(changed);
    expect([...first], "the caller's array did change").toEqual([9, 9]);
    expect(reads, "each part read once").toEqual(["0", "1"]);
  });
});

describe("the Value.map fast path observes what for...of observes", () => {
  const one = int(1n);

  it("a Proxy of an entry array sees the same traps as for...of: the iterator method, then length and each index", () => {
    const traps = (log: string[]): (readonly [string, Value])[] =>
      new Proxy(
        [
          ["a", one] as const,
          ["b", one] as const,
        ],
        {
          get: (t, name, receiver) => {
            log.push(String(name));
            return Reflect.get(t, name, receiver) as unknown;
          },
        },
      );
    const byForOf: string[] = [];
    for (const entry of traps(byForOf)) expect(entry).toBeDefined();
    const byMap: string[] = [];
    expect(expectOk(Value.map(traps(byMap)), "a Proxy of entries").size).toBe(2);
    expect(byMap).toEqual(["Symbol(Symbol.iterator)", "length", "0", "length", "1", "length"]);
    expect(byMap).toEqual(byForOf);
  });

  it("falls back to the iterator when the array iterators' next has been replaced", () => {
    const arrayIterator = Object.getPrototypeOf([][Symbol.iterator]()) as { next: (this: Iterator<unknown>) => IteratorResult<unknown> };
    const original = arrayIterator.next;
    const steps: string[] = [];
    const entry = ["a", one] as const;
    // other arrays are iterated too (inside Uint8Array.from, say): only a step that yields this entry is noted
    arrayIterator.next = function (this: Iterator<unknown>) {
      const r = original.call(this);
      if (r.value === entry) steps.push("entry");
      return r;
    };
    const entries: (readonly [string, Value])[] = [entry];
    const run = (): unknown => {
      steps.length = 0;
      try {
        return Value.map(entries);
      } finally {
        arrayIterator.next = original;
      }
    };
    const patched = run();
    const patchedSteps = [...steps];
    const restored = run();
    expect(patched).toEqual(restored);
    expect(patchedSteps, "the patched next walked the entries").toEqual(["entry"]);
    expect(steps, "restored, the fast path reads by index").toEqual([]);
  });
});

describe("a byte string decoded from shared memory shares none of it", () => {
  it("writing the SharedArrayBuffer after decoding changes nothing in the value", () => {
    const encoded = [0x43, 0x01, 0x02, 0x03]; // h'010203'
    const shared = new Uint8Array(new SharedArrayBuffer(encoded.length));
    shared.set(encoded);
    const v = expectOk(cbor.decode(shared), "decodes");
    shared.fill(0xff);
    expect(v.kind === "bytes" && [...v.bytes]).toEqual([1, 2, 3]);
    const view = v.kind === "bytes" ? bytesView(v) : new Uint8Array(0);
    expect(Object.getPrototypeOf(view.buffer), "its own ArrayBuffer").toBe(ArrayBuffer.prototype);
  });
});

describe("forged previews and schema arguments of the wrong type", () => {
  it("a Preview built by calling the constructor from outside is not one, so describeFailure never reads its head", () => {
    const read: string[] = [];
    const head = { [Symbol.iterator]: () => read.push("iterator"), toString: () => read.push("toString") };
    for (const args of [[head, 0, 0], [Symbol("preview"), head, 0, 0], [Symbol("preview"), "x".repeat(1 << 20), 0, 0]]) {
      const forged: unknown = Reflect.construct(Preview as unknown as new (...a: unknown[]) => object, args);
      expect(describeFailure(bad({ path: [forged], reason: "r" }))).toBe("<not a decode failure>");
    }
    expect(read).toEqual([]);
    expect(describeFailure({ path: [Preview.of("a")], reason: "r" })).toBe("a: r");
  });

  it("fields, selectAlternative and hasFields refuse schema names that are not arrays of strings", () => {
    const m = expectOk(Value.map([["a", int(1n)]]), "a map");
    const names = { ok: false, error: { path: [], reason: "the schema's names are not arrays of strings" } };
    for (const x of [5, null, "a", [5], { length: 1, 0: "a" }]) {
      expect(codec.fields(m, bad(x)), label(x)).toEqual(names);
      expect(codec.selectAlternative(m, bad(x), []), label(x)).toEqual(names);
      expect(codec.selectAlternative(m, [], bad(x)), label(x)).toEqual(names);
      expect(codec.selectAlternative(m, bad([[x, ["a"]]]), []), label(x)).toEqual(names);
      expect(codec.hasFields({ a: 1 }, bad(x), ["a"], ["a"]), label(x)).toBe(false);
    }
    expect(expectOk(codec.fields(m, ["a"]), "allowed")).toBe(m);
    expect(codec.selectAlternative(m, [[["a"], ["a"]]], [])).toEqual({ ok: true, value: 0 });
    expect(codec.hasFields({ a: 1 }, ["a"], ["a"], ["a"])).toBe(true);
  });

  it("a codec built from bounds of the wrong type refuses every value, both ways", () => {
    const sizes = "the schema's size bounds are not numbers";
    for (const [lo, hi] of [["1", null], [Number.NaN, null], [0, "5"], [0, Number.NaN], [0n, null]] as const) {
      const v = codec.vector(codec.uint, bad(lo), bad(hi));
      expect(v.encode([1n]), `${String(lo)}..${String(hi)}`).toEqual(mismatch(sizes));
      expect(v.decode(expectOk(Value.array([int(1n)]), "array"))).toEqual({ ok: false, error: { path: [], reason: sizes } });
      expect(codec.sizedBytes(bad(lo), bad(hi)).encode(Uint8Array.of(1))).toEqual(mismatch(sizes));
      expect(codec.sizedText(bad(lo), bad(hi)).encode("a")).toEqual(mismatch(sizes));
    }
    const ints = "the schema's integer bounds are not bigints";
    for (const [lo, hi] of [[0, 5n], [0n, 5], ["0", null]] as const) {
      expect(codec.intRange(bad(lo), bad(hi)).encode(1n)).toEqual(mismatch(ints));
      expect(codec.intRange(bad(lo), bad(hi)).decode(int(1n))).toEqual({ ok: false, error: { path: [], reason: ints } });
    }
    expect(expectOk(codec.intRange(0n, null).encode(1n), "valid bounds").kind).toBe("int");
  });
});

describe("a revoked Proxy is refused as a TypeMismatch, never thrown on", () => {
  /** An array Proxy, revoked: every question the engine is asked about it throws, Array.isArray included. */
  const revoked = (): never => {
    const { proxy, revoke } = Proxy.revocable([], {});
    revoke();
    return bad(proxy);
  };

  it("at the array entry points, and where an object is read", () => {
    expect(hashCommitment("SHA-256", revoked(), int(1n))).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(Value.array(revoked())).toEqual(mismatch("expected an array"));
    expect(Value.map(revoked())).toEqual(mismatch("expected an iterable of [key, value] entries"));
    expect(Value.map(bad([revoked()]))).toEqual(mismatch("expected a [key, value] array at index 0"));
    expect(codec.vector(codec.uint, 0, null).encode(revoked())).toEqual(mismatch("expected an array"));
    expect(describeFailure(revoked())).toBe("<not a decode failure>");
    expect(describeFailure(bad({ path: revoked(), reason: "r" }))).toBe("<not a decode failure>");
    const record = codec.record<{ readonly a: bigint }>({ a: codec.required("a", codec.uint) });
    expect(record.encode(revoked())).toEqual(mismatch("expected a record"));
  });
});

/** A Proxy of `target` that revokes itself as its first read returns: every later read of it would throw. */
function revokingAfterFirstRead<T extends object>(target: T): T {
  const { proxy, revoke } = Proxy.revocable(target, {
    get: (t, name) => {
      const v: unknown = Reflect.get(t, name);
      revoke();
      return v;
    },
  });
  return proxy;
}

describe("an object or array a getter revokes while it is read is refused as a TypeMismatch, never thrown on", () => {
  const notAResult = mismatch("an iterator's next() gave a result that is not an object");
  const resultRevoking = (value: unknown): never => bad({ [Symbol.iterator]: () => ({ next: () => revokingAfterFirstRead({ done: false, value }) }) });

  it("arrays, maps and their entries, and an iterator's result revoked by its own done getter", () => {
    expect(hashCommitment("SHA-256", bad(revokingAfterFirstRead([Uint8Array.of(1), Uint8Array.of(2)])), int(1n))).toEqual(mismatch("expected an array of Uint8Arrays"));
    expect(Value.array(revokingAfterFirstRead([int(1n), int(2n)]))).toEqual(mismatch("expected a canonical value at index 0"));
    expect(Value.map(bad(revokingAfterFirstRead([["a", int(1n)]])))).toMatchObject({ ok: false, error: { kind: "TypeMismatch" } });
    expect(Value.map(bad([revokingAfterFirstRead(["a", int(1n)])]))).toEqual(mismatch("expected a canonical value at index 0"));
    expect(Value.map(resultRevoking(["a", int(1n)]))).toEqual(notAResult);
  });

  it("the encoder's arrays, records, map entries and iterator results; failures and schema names", () => {
    expect(codec.vector(codec.uint, 0, null).encode(revokingAfterFirstRead([1n, 2n]))).toEqual(mismatch("expected an array"));
    const record = codec.record<{ readonly a: bigint; readonly b: bigint }>({ a: codec.required("a", codec.uint), b: codec.required("b", codec.uint) });
    expect(record.encode(revokingAfterFirstRead({ a: 1n, b: 2n }))).toEqual(mismatch("expected a record"));
    const map = codec.textMap(codec.uint);
    expect(map.encode(bad([revokingAfterFirstRead(["a", 1n])]))).toEqual(mismatch("expected a map entry (a [key, value] array)"));
    expect(map.encode(resultRevoking(["a", 1n]))).toEqual(notAResult);
    expect(codec.hasFields(revokingAfterFirstRead({ a: 1n, b: 2n }), ["a", "b"], ["a", "b"], ["a", "b"])).toBe(false);
    expect(describeFailure(bad(revokingAfterFirstRead({ path: [], reason: "r" })))).toBe("<not a decode failure>");
    expect(describeFailure(bad({ path: revokingAfterFirstRead([Preview.of("a"), Preview.of("b")]), reason: "r" }))).toBe("<not a decode failure>");
    const names = codec.selectAlternative(int(1n), bad(revokingAfterFirstRead([[["a"], ["a"]]])), []);
    expect(names.ok ? "selected" : names.error.reason).toBe("the schema's names are not arrays of strings");
  });
});
