/**
 * Host bindings (jlvm.md section 4.1): names a program reads as `$name`, each bound to a canonical value, null
 * included. A name bound to null is bound: it reads as null, where an unbound name is `Malformed`. That is why they are
 * not a canonical map, which holds no null.
 */
import { isValue, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { close, isArrayGuarded, isArrayLength, isResult, NOT_A_RESULT, NOT_ITERABLE, openWith, readGuarded, readStep, walksAsArray } from "@kd5ujc/dilf4s-core/internal";

import { compareCodePoints } from "./jvalue.js";

/** The key the constructor takes, private to this module: without it, what it builds is no bindings. */
const KEY: unique symbol = Symbol("bindings");

const notA = (what: string): { readonly ok: false; readonly error: TypeMismatch } => ({ ok: false, error: { kind: "TypeMismatch", reason: `expected ${what}` } });

/** Keeps the entry read, a `[name, value]` array (its two items read once each, guarded), or says why it is refused. */
function entry(kept: (readonly [string, Value])[], e: unknown): string | undefined {
  if (!isArrayGuarded(e)) return "a [name, value] array";
  // guarded: the name's read (a Proxy's trap) may have revoked the entry; neither check takes REVOKED
  const name: unknown = readGuarded(e, 0);
  const value: unknown = readGuarded(e, 1);
  if (typeof name !== "string") return "a string name";
  if (!isValue(value)) return "a canonical value for a binding";
  kept.push([name, value]);
  return undefined;
}

/** The bindings of `kept`, sorted by code point, unless a name is bound twice. */
function sorted(kept: (readonly [string, Value])[]): Result<Bindings, TypeMismatch> {
  kept.sort((a, b) => compareCodePoints(a[0], b[0]));
  for (let i = 1; i < kept.length; i++) if (kept[i]?.[0] === kept[i - 1]?.[0]) return notA("each name bound once");
  return { ok: true, value: make(kept) };
}

// set once, by the static block
let isBindingsOf!: (x: object) => boolean;
let make!: (entries: readonly (readonly [string, Value])[]) => Bindings;

/**
 * Names bound to values, null included, sorted by code point and found by binary search: names are compared, never
 * hashed. Made only by {@link Bindings.of}.
 */
export class Bindings {
  readonly #entries: readonly (readonly [string, Value])[];
  /** Whether {@link Bindings.of} made it, with its key: JavaScript can call a private constructor. */
  readonly #made: boolean;
  private constructor(key: symbol, entries: readonly (readonly [string, Value])[]) {
    this.#made = key === KEY;
    this.#entries = this.#made ? entries : [];
    Object.freeze(this);
  }

  static {
    isBindingsOf = (x) => #made in x && x.#made;
    make = (entries) => new Bindings(KEY, entries);
  }

  /** No bindings. */
  static readonly none: Bindings = new Bindings(KEY, []);

  /**
   * The bindings of `entries`, read once, in order, as `for...of` reads them: each a `[name, value]` array whose name
   * is a string and whose value a canonical value (a null value too), and no name twice. Anything else is a
   * `TypeMismatch`, as is an iteration protocol of the wrong shape (where `for...of` throws a `TypeError`) and a
   * revoked Proxy anywhere in it. Read with core's iterator helpers, as `MapValue.of` reads its entries; an exception
   * raised by the caller's own code (an iterator, a getter, a trap) propagates, the iterator closed first.
   */
  static of(entries: Iterable<readonly [string, Value]>): Result<Bindings, TypeMismatch> {
    const given: unknown = entries;
    // an object, or a function, as for...of accepts either; a revoked Proxy is neither to read
    if (!isResult(given)) return notA("an iterable of [name, value] entries");
    const method: unknown = Reflect.get(given, Symbol.iterator);
    const kept: (readonly [string, Value])[] = [];
    // an array the intrinsic iterator would walk is read as it would, by index, with nothing to close
    if (walksAsArray(given, method)) {
      for (let i = 0; ; i++) {
        // the length each step, as the array iterator reads it, each read guarded: one may have revoked the array
        const length: unknown = readGuarded(given, "length");
        if (!isArrayLength(length)) return notA("an iterable of [name, value] entries");
        if (i >= length) break;
        const refused = entry(kept, readGuarded(given, i));
        if (refused !== undefined) return notA(refused);
      }
      return sorted(kept);
    }
    const read = openWith<unknown>(given, method);
    if (read === undefined) return notA(NOT_ITERABLE);
    // as for...of: the iterator is closed for a refusal, or a throw, while an entry is read, never for one from its own
    // next() or from reading its result
    let reading = false;
    try {
      for (;;) {
        const r = readStep(read.next());
        if (r === undefined) return notA(NOT_A_RESULT);
        if (r.done) return sorted(kept);
        reading = true;
        const refused = entry(kept, r.value);
        if (refused !== undefined) return notA(refused);
        reading = false;
      }
    } finally {
      if (reading) close(read);
    }
  }

  /** The value bound to `name` (null if bound to null), or `undefined` if unbound: O(log n) comparisons. */
  get(name: string): Value | undefined {
    // a name that is not a string (from a JavaScript caller) names no binding
    if (typeof (name as unknown) !== "string") return undefined;
    let [lo, hi] = [0, this.#entries.length];
    for (; lo < hi; ) {
      const mid = (lo + hi) >>> 1;
      const e = this.#entries[mid];
      if (e === undefined) return undefined;
      const c = compareCodePoints(e[0], name);
      if (c === 0) return e[1];
      if (c < 0) lo = mid + 1;
      else hi = mid;
    }
    return undefined;
  }

  /** The number of names bound. */
  get size(): number {
    return this.#entries.length;
  }
}

// the class and its prototype frozen: no caller can replace `get`, which the machine calls, or `none`
Object.freeze(Bindings);
Object.freeze(Bindings.prototype);

/** Whether `x` is {@link Bindings} this package made: its private field, never its prototype. O(1). */
export function isBindings(x: unknown): x is Bindings {
  return typeof x === "object" && x !== null && isBindingsOf(x);
}
