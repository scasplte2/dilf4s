import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { codec, describeFailure, json, MAX_DEPTH, Value, type Result } from "../src/index.js";
import type { Codec } from "../src/codec.js";
import { expectOk, int } from "./vectors.js";

const { defer, leaf, optional, record, required, textMap, uint, union, alternative, vector } = codec;

/** The native stack's depth here: every frame, with the trace limit lifted for these tests. */
const stackDepth = (): number => (new Error().stack ?? "").split("\n").length;

/** A leaf that writes and reads an integer, noting the native stack's depth each time it runs. */
function probe(seen: number[]): Codec<bigint> {
  return leaf(
    (n) => {
      seen.push(stackDepth());
      return Value.int(n);
    },
    (v) => {
      seen.push(stackDepth());
      return v.kind === "int" ? codec.ok(v.value) : codec.decodeFailure("expected int");
    },
  );
}

/** `n` arrays around `x`: the codec nests a vector per level, `n` deep. */
type Nested = bigint | readonly Nested[];

function nestedVectors(n: number, inner: Codec<bigint>): Codec<Nested> {
  return Array.from({ length: n }).reduce<Codec<Nested>>((c) => vector(c, 1, 1), inner);
}

const nestedValue = (n: number, x: bigint): Nested => Array.from({ length: n }).reduce<Nested>((v) => [v], x);

/** A recursive schema through a record, a vector, a text map and references. */
interface Node {
  readonly items?: readonly Node[];
  readonly fields?: ReadonlyMap<string, Node>;
  readonly leaf?: bigint;
}

function nodeCodec(inner: Codec<bigint>): Codec<Node> {
  const node: Codec<Node> = record<Node>({
    items: optional("i", vector(defer(() => node), 0, null)),
    fields: optional("f", textMap(defer(() => node))),
    leaf: optional("l", inner),
  });
  return node;
}

/** `n` nodes, alternately through an array and a map, ending in a leaf: depth 2n - 1. */
const chain = (n: number): Node =>
  Array.from({ length: n - 1 }, (_, i) => i).reduce<Node>((inner, i) => (i % 2 === 0 ? { items: [inner] } : { fields: new Map([["k", inner]]) }), { leaf: 7n });

describe("codecs walk iteratively", { timeout: 60_000 }, () => {
  const limit = Error.stackTraceLimit;
  beforeAll(() => {
    Error.stackTraceLimit = Infinity;
  });
  afterAll(() => {
    Error.stackTraceLimit = limit;
  });

  it("a leaf runs at the same native depth under one vector or under 256 (the depth bound)", () => {
    const shallow: number[] = [];
    const deep: number[] = [];
    const one = nestedVectors(1, probe(shallow));
    const bound = nestedVectors(MAX_DEPTH, probe(deep));
    const encoded = expectOk(bound.encode(nestedValue(MAX_DEPTH, 5n)), "256 arrays encode");
    expect(encoded.depth).toBe(MAX_DEPTH);
    expect(expectOk(bound.decode(encoded), "and decode")).toEqual(nestedValue(MAX_DEPTH, 5n));
    expect(expectOk(one.decode(expectOk(one.encode(nestedValue(1, 5n)), "one array")), "decodes")).toEqual([5n]);
    expect(deep).toHaveLength(2);
    expect(deep, "encode, then decode, at the depth of one level").toEqual(shallow);
    // one level more is refused before it is written
    expect(nestedVectors(MAX_DEPTH + 1, uint).encode(nestedValue(MAX_DEPTH + 1, 5n))).toEqual({ ok: false, error: { kind: "DepthExceeded", limit: MAX_DEPTH } });
  });

  it("a recursive schema through records, vectors, text maps and references runs at one native depth", () => {
    const shallow: number[] = [];
    const deep: number[] = [];
    const n = MAX_DEPTH / 2; // 128 nodes: depth 255, the deepest a node chain reaches
    const node = nodeCodec(probe(deep));
    const value = expectOk(node.encode(chain(n)), "128 nodes encode");
    expect(value.depth).toBe(2 * n - 1);
    // decoded maps are the codec's own ReadonlyMap: compared by what they encode to
    const again = expectOk(nodeCodec(uint).encode(expectOk(node.decode(value), "and decode")), "re-encodes");
    expect(Value.equals(again, value)).toBe(true);
    const one = nodeCodec(probe(shallow));
    expect(expectOk(one.decode(expectOk(one.encode(chain(1)), "one node")), "decodes")).toEqual(chain(1));
    expect(deep).toHaveLength(2);
    expect(deep).toEqual(shallow);
  });

  it("a vector and a text map of 100,000 parts encode and decode", () => {
    const n = 100_000;
    const xs = Array.from({ length: n }, (_, i) => BigInt(i));
    const many = vector(uint, 0, null);
    const v = expectOk(many.encode(xs), "a long array");
    expect(v.kind === "array" && v.items.length).toBe(n);
    expect(expectOk(many.decode(v), "decodes")).toEqual(xs);
    const keyed = textMap(uint);
    const m = new Map(xs.map((x) => [`k${x.toString()}`, x] as const));
    const back = expectOk(keyed.decode(expectOk(keyed.encode(m), "a wide map")), "decodes");
    expect(back.size).toBe(n);
    expect(back.get("k99999")).toBe(99999n);
    expect([...back.keys()].slice(0, 3)).toEqual(["k0", "k1", "k2"]);
  });
});

describe("what the walks read and refuse", () => {
  interface Inner {
    readonly x: bigint;
    readonly y?: bigint;
  }
  const Inner: Codec<Inner> = record<Inner>({ x: required("x", uint), y: optional("why", uint) });
  interface Outer {
    readonly list: readonly Inner[];
    readonly byName: ReadonlyMap<string, Inner>;
  }
  const Outer: Codec<Outer> = record<Outer>({ list: required("list", vector(Inner, 0, 2)), byName: required("by-name", textMap(Inner)) });

  const decoded = (r: Result<unknown, Parameters<typeof describeFailure>[0]>): string => (r.ok ? "decoded" : describeFailure(r.error));
  const map = (entries: readonly (readonly [string, Value])[]): Value => expectOk(Value.map(entries), "map");
  const array = (items: readonly Value[]): Value => expectOk(Value.array(items), "array");
  const inner = (x: Value): Value => map([["x", x]]);

  it("a refusal names the path to it, outermost first", () => {
    const bad = map([
      ["list", array([inner(int(1n)), inner(Value.bool(true))])],
      ["by-name", map([])],
    ]);
    expect(decoded(Outer.decode(bad))).toBe("list.[1].x: expected int, got bool");
    const badKey = map([
      ["list", array([])],
      ["by-name", map([["n", map([["x", int(1n)], ["why", int(-1n)]])]])],
    ]);
    expect(decoded(Outer.decode(badKey))).toBe("by-name.n.why: integer of 0 bits outside 0..18446744073709551615");
  });

  it("a record refuses unknown keys first, then a missing field, at itself", () => {
    expect(decoded(Inner.decode(map([["z", int(1n)]])))).toBe('<root>: 1 unknown key, the first "z" (UTF-8 length 1)');
    expect(decoded(Inner.decode(map([["why", int(1n)]])))).toBe('<root>: missing required key "x"');
    const nested = map([
      ["list", array([map([])])],
      ["by-name", map([])],
    ]);
    expect(decoded(Outer.decode(nested))).toBe('list.[0]: missing required key "x"');
  });

  it("an absent optional field reads as undefined and is not written", () => {
    const r = expectOk(Inner.decode(inner(int(3n))), "decodes");
    expect(r).toEqual({ x: 3n, y: undefined });
    expect(Object.keys(r)).toEqual(["x", "y"]);
    const back = expectOk(Inner.encode(r), "encodes");
    expect(back.kind === "map" && back.keys()).toEqual(["x"]);
  });

  it("encoding refuses the first field in schema order, and a vector's size before its parts", () => {
    expect(Inner.encode({ x: -1n, y: -2n })).toMatchObject({ ok: false, error: { kind: "TypeMismatch", reason: "integer of 0 bits outside 0..18446744073709551615" } });
    expect(Outer.encode({ list: [{ x: 1n }, { x: 2n }, { x: -1n }], byName: new Map() })).toMatchObject({ ok: false, error: { reason: "array of 3 elements outside the schema size" } });
  });

  it("a union tells its alternatives apart, and refuses what fits none without throwing", () => {
    interface A {
      readonly a: bigint;
    }
    interface B {
      readonly b: bigint;
    }
    type U = A | B | "none";
    const A: Codec<A> = record<A>({ a: required("a", uint) });
    const B: Codec<B> = record<B>({ b: required("b", uint) });
    const U: Codec<U> = union<U>("U", [alternative(defer(() => A), [["a"], ["a"]], [["a"], ["a"]]), alternative(defer(() => B), [["b"], ["b"]], [["b"], ["b"]])], ["none"]);
    const NoText: Codec<A | B> = union<A | B>("NoText", [alternative(A, [["a"], ["a"]], [["a"], ["a"]]), alternative(B, [["b"], ["b"]], [["b"], ["b"]])], []);
    expect(expectOk(U.decode(expectOk(U.encode({ b: 2n }), "b")), "decodes")).toEqual({ b: 2n });
    expect(expectOk(U.decode(expectOk(U.encode("none"), "text")), "decodes")).toBe("none");
    expect(U.encode({ a: 1n, b: 2n })).toMatchObject({ ok: false, error: { reason: "no alternative of U fits" } });
    // a text where no text alternative exists (outside the type): a refusal, never a TypeError
    expect(NoText.encode("a" as unknown as A)).toMatchObject({ ok: false, error: { reason: "no alternative of NoText fits" } });
    expect(decoded(U.decode(Value.bool(true)))).toBe("<root>: expected a map or one of the text alternatives");
  });

  it("a union reads the fields of every alternative in UTF-16 order, once per alternative it tries", () => {
    interface Z {
      readonly z: bigint;
    }
    interface A {
      readonly a?: bigint;
      readonly b: bigint;
    }
    const Z: Codec<Z> = record<Z>({ z: required("z", uint) });
    const A: Codec<A> = record<A>({ a: optional("a", uint), b: required("b", uint) });
    const U: Codec<Z | A> = union<Z | A>("U", [alternative(Z, [["z"], ["z"]], [["z"], ["z"]]), alternative(A, [["b"], ["a", "b"]], [["b"], ["a", "b"]])], []);
    const read: string[] = [];
    const value = {
      get z(): undefined {
        read.push("z");
        return undefined;
      },
      get b(): bigint {
        read.push("b");
        return 2n;
      },
      get a(): undefined {
        read.push("a");
        return undefined;
      },
    } as unknown as A;
    expect(expectOk(U.encode(value), "encodes").kind).toBe("map");
    // a probe in sorted order for each alternative tried (Z, then A); then the record reads its fields in schema order
    expect(read).toEqual(["a", "b", "z", "a", "b", "z", "a", "b"]);
  });

  it("a map is read entry by entry from its iterator, each entry's key and value as it is read", () => {
    const shared: [string, bigint] = ["", 0n];
    const reused: ReadonlyMap<string, bigint> = {
      *[Symbol.iterator]() {
        for (const [k, v] of [["a", 1n], ["b", 2n]] as const) {
          shared[0] = k;
          shared[1] = v;
          yield shared;
        }
      },
    } as unknown as ReadonlyMap<string, bigint>;
    const v = expectOk(textMap(uint).encode(reused), "an iterator that reuses its entry");
    expect(v.kind === "map" && v.entries()).toEqual([
      ["a", int(1n)],
      ["b", int(2n)],
    ]);
    // a refused entry stops the reading: nothing after it is read
    let after = false;
    const stops: ReadonlyMap<string, bigint> = {
      *[Symbol.iterator]() {
        yield ["a", -1n];
        after = true;
        yield ["b", 1n];
      },
    } as unknown as ReadonlyMap<string, bigint>;
    expect(textMap(uint).encode(stops).ok).toBe(false);
    expect(after).toBe(false);
  });

  it("a codec made by hand keeps its own checks wherever it is composed", () => {
    // a codec spread from uint, its methods replaced to refuse zero: its shape is still uint's
    const nonZero: Codec<bigint> = {
      ...uint,
      encode: (n, depth) => (n === 0n ? codec.typeMismatch("zero") : uint.encode(n, depth)),
      decode: (v) => (v.kind === "int" && v.value === 0n ? codec.decodeFailure("zero") : uint.decode(v)),
    };
    const many = vector(nonZero, 0, null);
    expect(many.encode([1n, 0n])).toMatchObject({ ok: false, error: { reason: "zero" } });
    expect(decoded(many.decode(array([int(1n), int(0n)])))).toBe("[1]: zero");
    const inRecord = record<{ readonly n: bigint }>({ n: required("n", nonZero) });
    expect(inRecord.encode({ n: 0n })).toMatchObject({ ok: false, error: { reason: "zero" } });
    expect(decoded(defer(() => nonZero).decode(int(0n)))).toBe("<root>: zero");
  });

  it("an iterator's next is read once, when the container is opened", () => {
    const producer = new Map([
      ["a", 1n],
      ["b", 2n],
    ])[Symbol.iterator]();
    let reads = 0;
    const iterator = {
      get next() {
        reads++;
        return reads === 1 ? producer.next.bind(producer) : () => ({ done: true, value: undefined });
      },
    };
    const m = { [Symbol.iterator]: () => iterator } as unknown as ReadonlyMap<string, bigint>;
    const v = expectOk(textMap(uint).encode(m), "encodes");
    expect(v.kind === "map" && v.keys()).toEqual(["a", "b"]);
    expect(reads).toBe(1);
  });

  it("a refusal closes the iterators of the containers still open, innermost first", () => {
    const closed: string[] = [];
    function* entries(name: string, items: readonly (readonly [string, unknown])[]): Generator<readonly [string, unknown]> {
      try {
        yield* items;
      } finally {
        closed.push(name);
      }
    }
    const inner = { [Symbol.iterator]: () => entries("inner", [["x", -1n]]) } as unknown as ReadonlyMap<string, bigint>;
    const outer = { [Symbol.iterator]: () => entries("outer", [["m", inner]]) } as unknown as ReadonlyMap<string, ReadonlyMap<string, bigint>>;
    expect(textMap(textMap(uint)).encode(outer).ok).toBe(false);
    expect(closed).toEqual(["inner", "outer"]);
  });

  it("a chain of 10,000 references is followed once per reference, each way", () => {
    let resolved = 0;
    const chain = Array.from({ length: 10_000 }).reduce<Codec<bigint>>(
      (inner) =>
        defer(() => {
          resolved++;
          return inner;
        }),
      uint,
    );
    const v = expectOk(chain.encode(5n), "encodes");
    expect(resolved).toBe(10_000);
    expect(expectOk(chain.decode(v), "decodes")).toBe(5n);
    expect(resolved).toBe(20_000);
  });

  it("a reference to itself, through no container, is refused instead of looping", () => {
    const loop: Codec<bigint> = defer(() => loop);
    expect(loop.encode(1n)).toMatchObject({ ok: false, error: { kind: "TypeMismatch", reason: "the codec refers to itself without writing a container" } });
    expect(decoded(loop.decode(int(1n)))).toBe("<root>: the codec refers to itself without reading a container");
  });

  it("the compiler checks a record's fields against its type", () => {
    // @ts-expect-error a field's codec reads its property's type
    record<{ readonly a: string }>({ a: required("a", uint) });
    // @ts-expect-error every property has a field
    record<{ readonly a: string; readonly b: string }>({ a: required("a", codec.text) });
    // @ts-expect-error an optional property has an optional field
    record<{ readonly a?: string }>({ a: required("a", codec.text) });
    // @ts-expect-error a field's type is exactly its property's: a narrower codec does not write a wider type
    record<{ readonly a: string }>({ a: required("a", codec.literalText("x")) });
    expect(true).toBe(true);
  });

  it("an explicit wider type argument is unchecked and acts as a cast (Codec is bivariant, deliberately)", () => {
    // this compiles: if Codec's variance changes, this line fails to typecheck, and the change is a deliberate one
    const cast = record<{ readonly a: string }>({ a: required<string>("a", codec.literalText("x")) });
    const v = expectOk(cast.encode({ a: "y" }), "the literal codec writes its literal, whatever the property holds");
    expect(expectOk(json.renderWithin(v, json.MAX_RENDER_BYTES), "renders")).toBe('{"a":"x"}');
  });

  it("when caller code throws mid-walk, every iterator still open is closed, innermost first, and the error propagates", () => {
    const closed: string[] = [];
    function* items(name: string, xs: readonly unknown[]): Generator {
      try {
        yield* xs;
      } finally {
        closed.push(name);
      }
    }
    // a hand-made codec that throws on its second element, inside a map inside a vector
    const boom: Codec<bigint> = {
      ...uint,
      encode: (n, depth) => {
        if (n === 2n) throw new Error("caller code threw");
        return uint.encode(n, depth);
      },
    };
    const inner = { [Symbol.iterator]: () => items("inner", [["a", 1n], ["b", 2n], ["c", 3n]]) } as unknown as ReadonlyMap<string, bigint>;
    // an array read through its own iterator, which notes when it is closed
    const outer: ReadonlyMap<string, bigint>[] = [inner, inner];
    Object.defineProperty(outer, Symbol.iterator, { value: () => items("outer", [inner, inner]) });
    const nested = vector(textMap(boom), 0, null);
    expect(() => nested.encode(outer)).toThrow("caller code threw");
    expect(closed).toEqual(["inner", "outer"]);
  });
});

/** A hand-written iterator over `xs` that logs `return` calls, and can throw from `next` or `return`. */
function logged(name: string, xs: readonly unknown[], log: string[], opts: { readonly nextThrowsAt?: number; readonly returnThrows?: string; readonly done?: unknown } = {}): Iterator<unknown> {
  let i = 0;
  return {
    next() {
      if (opts.nextThrowsAt === i) throw new Error(`next-${name}`);
      const v = xs[i];
      i++;
      return i <= xs.length ? { done: false, value: v } : ({ done: opts.done ?? true, value: undefined } as IteratorResult<unknown>);
    },
    return() {
      log.push(`return-${name}`);
      if (opts.returnThrows !== undefined) throw new Error(opts.returnThrows);
      return { done: true, value: undefined };
    },
  };
}

/** An array read through the iterator `it` makes. */
function through<T>(it: () => Iterator<unknown>, xs: readonly T[]): T[] {
  const a = [...xs];
  Object.defineProperty(a, Symbol.iterator, { value: it });
  return a;
}

/** `uint`, made by hand, throwing on 2. */
const throwsOnTwo: Codec<bigint> = {
  ...codec.uint,
  encode: (n, depth) => {
    if (n === 2n) throw new Error("E1");
    return codec.uint.encode(n, depth);
  },
};

describe("closing iterators as for...of does", () => {
  const nested = (element: Codec<bigint>): Codec<readonly (readonly bigint[])[]> => vector(vector(element, 0, null), 0, null);

  it("normal completion closes nothing; a finished sibling is not closed; nothing is closed twice", () => {
    const log: string[] = [];
    const a = through(() => logged("a", [1n], log), [1n]);
    const b = through(() => logged("b", [1n, 3n], log), [1n, 3n]);
    expect(nested(uint).encode(through(() => logged("outer", [a, b], log), [a, b])).ok).toBe(true);
    expect(log).toEqual([]);
    const thrown: string[] = [];
    const a2 = through(() => logged("a", [1n], thrown), [1n]);
    const b2 = through(() => logged("b", [1n, 2n], thrown), [1n, 2n]);
    expect(() => nested(throwsOnTwo).encode(through(() => logged("outer", [a2, b2], thrown), [a2, b2]))).toThrow("E1");
    expect(thrown).toEqual(["return-b", "return-outer"]);
    const refused: string[] = [];
    const a3 = through(() => logged("a", [1n], refused), [1n]);
    const b3 = through(() => logged("b", [1n, -1n], refused), [1n, -1n]);
    expect(nested(uint).encode(through(() => logged("outer", [a3, b3], refused), [a3, b3])).ok).toBe(false);
    expect(refused).toEqual(["return-b", "return-outer"]);
  });

  it("a throwing return() still lets the outer iterators close, and its error is the one that surfaces", () => {
    const log: string[] = [];
    const inner = through(() => logged("inner", [1n, 2n], log, { returnThrows: "E2" }), [1n, 2n]);
    expect(() => nested(throwsOnTwo).encode(through(() => logged("outer", [inner], log), [inner]))).toThrow("E2");
    expect(log).toEqual(["return-inner", "return-outer"]);
    const both: string[] = [];
    const inner2 = through(() => logged("inner", [1n, -1n], both, { returnThrows: "E2" }), [1n, -1n]);
    expect(() => nested(uint).encode(through(() => logged("outer", [inner2], both, { returnThrows: "E3" }), [inner2]))).toThrow("E3");
    expect(both).toEqual(["return-inner", "return-outer"]);
  });

  it("an iterator whose next() threw is not closed; the ones around it are", () => {
    const log: string[] = [];
    const inner = through(() => logged("inner", [1n, 3n], log, { nextThrowsAt: 1 }), [1n, 3n]);
    expect(() => nested(uint).encode(through(() => logged("outer", [inner], log), [inner]))).toThrow("next-inner");
    expect(log).toEqual(["return-outer"]);
  });

  it("a map entry that cannot be destructured closes its iterator", () => {
    const log: string[] = [];
    const m = { [Symbol.iterator]: () => logged("map", [5], log) } as unknown as ReadonlyMap<string, bigint>;
    expect(() => textMap(uint).encode(m)).toThrow(TypeError);
    expect(log).toEqual(["return-map"]);
  });

  it("any truthy done ends the walk", () => {
    const log: string[] = [];
    const v = expectOk(vector(uint, 0, null).encode(through(() => logged("a", [1n, 3n], log, { done: "yes" }), [1n, 3n])), "encodes");
    expect(v.kind === "array" && v.items.length).toBe(2);
    expect(log).toEqual([]);
  });

  it("a depth that is not a safe integer from 0 is refused before anything is read, never a RangeError", () => {
    type R = readonly R[];
    const R: Codec<R> = vector(defer(() => R), 0, null);
    const deep = Array.from({ length: 200_000 }).reduce<R>((v) => [v], []);
    for (const depth of [-200_010, -1, 0.5, Number.NaN, Number.NEGATIVE_INFINITY, Number.POSITIVE_INFINITY, 2 ** 53]) {
      expect(R.encode(deep, depth), String(depth)).toEqual({ ok: false, error: { kind: "TypeMismatch", reason: "the depth is not a safe integer from 0" } });
    }
    expect(R.encode(deep)).toEqual({ ok: false, error: { kind: "DepthExceeded", limit: MAX_DEPTH } });
    expect(R.encode(deep, MAX_DEPTH)).toEqual({ ok: false, error: { kind: "DepthExceeded", limit: MAX_DEPTH } });
    // nothing of the value is touched: not its iterator, nor a record's getters
    const touched: string[] = [];
    const watched = through(() => {
      touched.push("iterator");
      return logged("watched", [], touched);
    }, []);
    const record1 = record<{ readonly a: bigint }>({ a: required("a", uint) });
    const getter = {
      get a(): bigint {
        touched.push("getter");
        return 1n;
      },
    };
    for (const depth of [-1, Number.NaN]) {
      expect(vector(uint, 0, null).encode(watched, depth).ok).toBe(false);
      expect(record1.encode(getter, depth).ok).toBe(false);
    }
    expect(touched).toEqual([]);
  });

  it("a value's getter that throws on a result is not followed by return(), as for...of does not close for it", () => {
    const log: string[] = [];
    const it: Iterator<unknown> = {
      next: () =>
        ({
          done: false,
          get value(): unknown {
            throw new Error("value getter");
          },
        }) as IteratorResult<unknown>,
      return: () => {
        log.push("return");
        return { done: true, value: undefined };
      },
    };
    expect(() => vector(uint, 0, null).encode(through(() => it, [1n]))).toThrow("value getter");
    expect(log).toEqual([]);
    const map = { [Symbol.iterator]: () => it } as unknown as ReadonlyMap<string, bigint>;
    expect(() => textMap(uint).encode(map)).toThrow("value getter");
    expect(log).toEqual([]);
  });

  it("an iterator result that is not an object is refused, that iterator left open and the ones around it closed", () => {
    for (const result of [7, null, undefined, "x"]) {
      const log: string[] = [];
      const bad: Iterator<unknown> = {
        next: () => result as unknown as IteratorResult<unknown>,
        return: () => {
          log.push("return-bad");
          return { done: true, value: undefined };
        },
      };
      const inner = through(() => bad, [1n]);
      const outer = through(() => logged("outer", [inner], log), [inner]);
      expect(nested(uint).encode(outer), String(result)).toEqual({
        ok: false,
        error: { kind: "TypeMismatch", reason: "an iterator's next() gave a result that is not an object" },
      });
      expect(log).toEqual(["return-outer"]);
      const map = { [Symbol.iterator]: () => bad } as unknown as ReadonlyMap<string, bigint>;
      expect(textMap(uint).encode(map).ok).toBe(false);
      expect(log).toEqual(["return-outer"]);
    }
  });
});
