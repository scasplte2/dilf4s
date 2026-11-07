/**
 * Checks of what a caller passes past the types (from JavaScript, or a cast), made once at each public entry point, in
 * O(1) for each argument, before it is read: an argument of the wrong type is core's `TypeMismatch`, never a throw or an
 * argument read as another type. Past the check the verifiers work on plain, owned data. Internal: not exported from
 * the package.
 */
import { err, isValue, ok, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { close, isAlgorithm, isArrayLength, isResult, NOT_A_RESULT, NOT_ITERABLE, openWith, plainBytes, typeMismatch, walksAsArray } from "@kd5ujc/dilf4s-core/internal";
import type { HashAlgorithm } from "@kd5ujc/dilf4s-core";

/**
 * The token this package's constructors take, private to the package. A constructor is private to TypeScript alone, so
 * JavaScript can call one (`new`, `Reflect.construct`); without the token it reads nothing it is given and stamps no
 * brand (core's `mint`), so no check here accepts what it builds.
 */
export const TOKEN: unique symbol = Symbol("dilf4s-proofs");
export type Token = typeof TOKEN;

export const NOT_BYTES = typeMismatch("expected a Uint8Array");
export const NOT_A_VALUE = typeMismatch("expected a canonical value");
export const NOT_A_BIGINT = typeMismatch("expected a bigint");
export const NOT_A_NUMBER = typeMismatch("expected a number");
export const NOT_A_STRING = typeMismatch("expected a string");
export const NOT_AN_ALGORITHM = typeMismatch("expected a HashAlgorithm");

/** The refusal of an argument that is not `what` (`a TrieProof`, `an array`, ...). */
export const expected = (what: string): TypeMismatch => typeMismatch(`expected ${what}`);

/** The refusal of an item of a caller's array that is not `what`, at index `i`. */
export const expectedAt = (what: string, i: number): TypeMismatch => typeMismatch(`expected ${what} at index ${i.toString()}`);

/** `x` as a plain view of its own memory (see core's `plainBytes`: its own `length` is not believed), or a refusal. */
export const bytesArg = (x: unknown): Result<Uint8Array, TypeMismatch> => {
  const view = plainBytes(x);
  return view === undefined ? err(NOT_BYTES) : ok(view);
};

/** `x` if it is a canonical value made by core (`isValue`, one private-brand check: no Proxy trap runs). */
export const valueArg = (x: unknown): Result<Value, TypeMismatch> => (isValue(x) ? ok(x) : err(NOT_A_VALUE));

/** `x` if it is a `bigint`, never converted. */
export const bigintArg = (x: unknown): Result<bigint, TypeMismatch> => (typeof x === "bigint" ? ok(x) : err(NOT_A_BIGINT));

/** `x` if it is one of core's `HASH_ALGORITHMS`, compared with each name, never converted. */
export const algorithmArg = (x: unknown): Result<HashAlgorithm, TypeMismatch> => (isAlgorithm(x) ? ok(x) : err(NOT_AN_ALGORITHM));

/** Whether `x` may have its properties read: an object (a Proxy's `get` traps are the caller's own code). */
export const isObject = (x: unknown): x is object => (typeof x === "object" && x !== null) || typeof x === "function";

/**
 * A caller's array, read by index: `Array.isArray` first (no trap runs on a Proxy for it), then its length once and
 * each item once, each given to `item`, whose first refusal is the result. `what` names the array in a refusal.
 */
export function readArray<T, E>(x: unknown, what: string, item: (v: unknown, i: number) => Result<T, E>): Result<T[], E | TypeMismatch> {
  if (!Array.isArray(x)) return err(expected(what));
  const length: unknown = (x as readonly unknown[]).length;
  if (!isArrayLength(length)) return err(expected(what));
  const out: T[] = [];
  for (let i = 0; i < length; i++) {
    const r = item((x as readonly unknown[])[i], i);
    if (!r.ok) return r;
    out.push(r.value);
  }
  return ok(out);
}

/**
 * A caller's pair `[a, b]`, if it is an array, for its reader to read by index, in order, once each: an item that is
 * bytes is copied before the next is read, since reading it (a getter, a Proxy's trap) may run caller code.
 */
export function readPair(x: unknown): readonly unknown[] | undefined {
  return Array.isArray(x) ? (x as readonly unknown[]) : undefined;
}

/**
 * A caller's iterable, read once as `for...of` reads it (an array the intrinsic iterator walks is read by index), each
 * item folded into the state by `step`; the iterator is closed when `step` refuses an item or caller code throws while
 * one is read, as leaving `for...of` early closes it. What `for...of` throws a `TypeError` for (no iterator, a `next` or
 * a result not of the protocol's type) is a `TypeMismatch` instead.
 */
export function foldIterable<S, E>(x: unknown, init: S, step: (s: S, v: unknown, i: number) => Result<S, E>): Result<S, E | TypeMismatch> {
  if (!isResult(x)) return err(typeMismatch(NOT_ITERABLE));
  const method: unknown = Reflect.get(x, Symbol.iterator);
  let state = init;
  if (walksAsArray(x, method)) {
    // a Proxy of an array walks as one, and may report any length: only one an array can have is read up to
    const length: unknown = x.length;
    if (!isArrayLength(length)) return err(typeMismatch(NOT_ITERABLE));
    for (let i = 0; i < length; i++) {
      const r = step(state, x[i], i);
      if (!r.ok) return r;
      state = r.value;
    }
    return ok(state);
  }
  const read = openWith<unknown>(x, method);
  if (read === undefined) return err(typeMismatch(NOT_ITERABLE));
  let reading = false;
  try {
    for (let i = 0; ; i++) {
      const r: unknown = read.next();
      if (!isResult(r)) return err(typeMismatch(NOT_A_RESULT));
      if (r.done) return ok(state);
      const v: unknown = r.value;
      reading = true;
      const next = step(state, v, i);
      if (!next.ok) return next;
      state = next.value;
      reading = false;
    }
  } finally {
    if (reading) close(read);
  }
}

/** A caller's iterable, read as {@link foldIterable} reads it, each item checked by `item` and kept in order. */
export function readIterable<T, E>(x: unknown, item: (v: unknown, i: number) => Result<T, E>): Result<T[], E | TypeMismatch> {
  const out: T[] = [];
  const r = foldIterable(x, out, (o, v, i) => {
    const checked = item(v, i);
    if (!checked.ok) return checked;
    o.push(checked.value);
    return ok(o);
  });
  return r;
}
