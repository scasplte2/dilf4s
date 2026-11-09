/**
 * Nothing recurses on the program or the data (the `codec-walk.test.ts` pattern): parsing, compiling, encoding,
 * decoding and evaluating a program at the depth limit run at the native stack depth of a program one level deep.
 *
 * The probe is an engine method the package calls at the bottom of a deep program: `String.prototype.split`, which
 * reads a `var` path's segments, a static one while the program is parsed, measured, checked and compiled, and a
 * computed one while it is evaluated. It notes the native stack's depth each time; a walk that recursed would call it
 * deeper, one or more frames per level. (Core's classes are frozen, so their methods cannot be probed.)
 */
import { cbor, type Value } from "@kd5ujc/dilf4s-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { compile, decode, evaluate, fromCanonical, parse, parseValue } from "../src/index.js";
import { expectOk } from "./vectors.js";

const stackDepth = (): number => (new Error().stack ?? "").split("\n").length;

/** Runs `f` with `String.prototype.split(".")` noting the stack depth at each call, while parsing and while evaluating. */
function probed<T>(f: (evaluating: () => void) => T): { readonly value: T; readonly splits: number[]; readonly gets: number[] } {
  const splits: number[] = [];
  const gets: number[] = [];
  let into = splits;
  const split = Object.getOwnPropertyDescriptor(String.prototype, "split");
  // eslint-disable-next-line @typescript-eslint/unbound-method -- called with its receiver, through apply
  const originalSplit = String.prototype.split;
  Object.defineProperty(String.prototype, "split", {
    configurable: true,
    writable: true,
    value: function (this: string, ...args: Parameters<typeof originalSplit>) {
      if ((args[0] as unknown) === ".") into.push(stackDepth());
      return originalSplit.apply(this, args);
    },
  });
  try {
    return {
      value: f(() => {
        into = gets;
      }),
      splits,
      gets,
    };
  } finally {
    if (split !== undefined) Object.defineProperty(String.prototype, "split", split);
  }
}

/** A static path, read while compiling, beside a computed one, read while evaluating: true. */
const INNER = '{"==": [{"var": "x.y"}, {"var": {"cat": ["x", ".y"]}}]}';

/** `n` nested operations of one argument each around a data read: `{"!!": [{"!!": [... {"var": "x.y"}]}]}`. */
const nestedOps = (n: number): string => Array.from({ length: n }).reduce<string>((inner) => `{"!!": [${inner}]}`, INNER);

/** `n` nested arrays around a data read. */
const nestedArrays = (n: number): string => Array.from({ length: n }).reduce<string>((inner) => `[${inner}]`, INNER);

/** `n` nested `map` callbacks, each over a one-element array, around a data read. */
const nestedMaps = (n: number): string => Array.from({ length: n }).reduce<string>((inner) => `{"map": [[1], ${inner}]}`, INNER);

/** `n` nested `let`s, each binding a name, around a data read. */
const nestedLets = (n: number): string => Array.from({ length: n }).reduce<string>((inner, _, i) => `{"let": [[["n${i.toString()}", 1]], ${inner}]}`, INNER);

/** The deepest nesting of `make` that parses: the depth bound, reached. */
function deepest(make: (n: number) => string): number {
  let n = 1;
  for (; parse(make(n + 1)).ok; n++);
  return n;
}

describe("the walks are iterative", { timeout: 120_000 }, () => {
  const limit = Error.stackTraceLimit;
  beforeAll(() => {
    Error.stackTraceLimit = Infinity;
  });
  afterAll(() => {
    Error.stackTraceLimit = limit;
  });

  const data = expectOk(parseValue('{"x": {"y": true}}'), "data");

  for (const [name, make] of [
    ["operations", nestedOps],
    ["arrays", nestedArrays],
    ["map callbacks", nestedMaps],
    ["lets", nestedLets],
  ] as const) {
    it(`${name} at the depth limit parse, compile, encode, decode and evaluate at the native depth of one level`, () => {
      const n = deepest(make);
      expect(n, `${name} nest at least 60 deep`).toBeGreaterThanOrEqual(60);
      const runs = [1, n].map((k) =>
        probed((evaluating) => {
          const program = expectOk(parse(make(k)), `${name} ${k.toString()}`);
          const compiled = expectOk(compile(program), "compiles");
          const bytes = expectOk(program.encode(), "encodes");
          const decoded = expectOk(decode(bytes), "decodes");
          expect(expectOk(fromCanonical(program.canonical()), "from its canonical form").canonical()).toEqual(decoded.canonical());
          evaluating();
          return expectOk(evaluate(compiled, data), "evaluates");
        }),
      );
      const [shallow, deep] = runs;
      if (shallow === undefined || deep === undefined) throw new Error("two runs");
      expect(deep.value.result.ok, `${name} ${n.toString()} evaluates: ${JSON.stringify(deep.value.result.ok ? null : deep.value.result.error)}`).toBe(true);
      expect(deep.splits.length).toBeGreaterThan(0);
      expect(deep.gets.length).toBeGreaterThan(0);
      expect(new Set(deep.splits), "every path read at one native depth, however deep the program").toEqual(new Set(shallow.splits));
      expect(new Set(deep.gets), "every path read while evaluating at one native depth, however deep the program").toEqual(new Set(shallow.gets));
    });
  }

  it("one level past the limit is Limit, from the parser, before anything is evaluated", () => {
    for (const make of [nestedOps, nestedArrays, nestedMaps, nestedLets]) {
      const r = parse(make(deepest(make) + 1));
      expect(r.ok ? "parsed" : r.error.kind).toBe("Limit");
    }
  });

  it("data 254 arrays deep (256 levels with its two maps) is read whole and compared, without overflowing the stack", () => {
    const deepData = (k: number): Value => expectOk(parseValue(`{"x": {"y": ${"[".repeat(k)}1${"]".repeat(k)}}}`), "data");
    const program = expectOk(parse('{"==": [{"var": "x.y"}, {"var": "x.y"}]}'), "program");
    // core's equality walks it (core's own walk-depth tests cover that it does so iteratively): here, that it runs
    for (const k of [1, 254]) expect(expectOk(evaluate(program, deepData(k)), "evaluates").result.ok, `${k.toString()} deep`).toBe(true);
    // and the canonical encoding of a value the machine built that deep is iterative too
    const built = expectOk(evaluate(expectOk(parse(INNER), "p"), deepData(254)), "evaluates");
    expect(built.result.ok && cbor.encode(built.result.value).ok).toBe(true);
  });
});
