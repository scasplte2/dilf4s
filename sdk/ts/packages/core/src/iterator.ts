/**
 * Reading a caller's iterable as `for...of` does, where what `for...of` throws a `TypeError` for (an iterator method,
 * iterator, `next` or result not of the protocol's type) is found instead, to be refused as a typed error: library code
 * does not throw. Caller code it runs (a getter, an iterator's methods) may throw. Internal to this package.
 */

import { isArrayGuarded, isRevokedProxy, readGuarded, REVOKED } from "./guard.js";

/** The greatest length an array can have: `2^32 - 1`. */
const MAX_ARRAY_LENGTH = 2 ** 32 - 1;

/**
 * Whether `n`, read once from what a caller passes as an array, is a length an array can have: an integer from 0 to
 * `2^32 - 1`. A real array's always is; a Proxy of one can report anything (`Infinity`, a fraction, a symbol), and a
 * loop up to such a length would never end, or would compare a symbol and throw.
 */
export const isArrayLength = (n: unknown): n is number => typeof n === "number" && Number.isInteger(n) && n >= 0 && n <= MAX_ARRAY_LENGTH;

/** An iterator and its `next`, read once when it is opened, as `for...of` reads it. */
export interface Reader<T> {
  readonly iterator: Iterator<T>;
  /** The iterator's `next`, bound: its result is the caller's, so it is read as `unknown` (see {@link isResult}). */
  readonly next: () => unknown;
}

/** The refusal of an iterable whose iterator `for...of` could not open. */
export const NOT_ITERABLE = "expected an iterable (its Symbol.iterator, iterator or next is not of the protocol's type)";

/** The refusal of an iterator result that is not an object, which `for...of` throws a `TypeError` for. */
export const NOT_A_RESULT = "an iterator's next() gave a result that is not an object";

/** Whether `r` may be read as an iterator or an iterator result: an object, a function included, as `for...of` requires. */
export function isResult(r: unknown): r is { readonly done?: unknown; readonly value?: unknown } {
  // a revoked Proxy is an object every read of which throws: it is no object to read
  return ((typeof r === "object" && r !== null) || typeof r === "function") && !isRevokedProxy(r);
}

const NO_ARGUMENTS: readonly unknown[] = Object.freeze([]);

/**
 * `iterable`'s iterator, opened as `for...of` opens it (its `Symbol.iterator` method read and called once, then the
 * iterator's `next` read once), or `undefined` where `for...of` would throw a `TypeError` before reading any item: no
 * iterator method, an iterator that is not an object, or a `next` that is not a function.
 */
export function open<T>(iterable: object): Reader<T> | undefined {
  return openWith(iterable, Reflect.get(iterable, Symbol.iterator));
}

/** {@link open}, given the iterator method already read from `iterable` (once, as `for...of` reads it). */
export function openWith<T>(iterable: object, method: unknown): Reader<T> | undefined {
  // a revoked Proxy of a function is a function no code can call, and an iterable revoked while its method was read
  // (by its own trap) is one no iterator can read: each refused as for...of's TypeError would be
  if (typeof method !== "function" || isRevokedProxy(method) || isRevokedProxy(iterable)) return undefined;
  const iterator: unknown = Reflect.apply(method, iterable, NO_ARGUMENTS);
  if (!isResult(iterator)) return undefined;
  const next: unknown = Reflect.get(iterator, "next");
  if (typeof next !== "function" || isRevokedProxy(next)) return undefined;
  // each step one Reflect.apply: never the function's own `call` or `bind` (which may be the caller's), and never
  // Function.prototype.bind, which reads the function's length and name (a Proxy's traps, which may revoke it). A
  // `next` revoked since (by caller code) gives no result, which a reader refuses as any result not an object
  return { iterator: iterator as Iterator<T>, next: (): unknown => (isRevokedProxy(next) ? undefined : (Reflect.apply(next, iterator, NO_ARGUMENTS) as unknown)) };
}

/** An iterator result, read as `for...of` reads it: `done` (any truthy value ends it), then `value` while not done. */
export type Step<T> = { readonly done: true } | { readonly done: false; readonly value: T };

const DONE: Step<never> = Object.freeze({ done: true });

/**
 * `r`, an iterator's result, read as `for...of` reads it (`done` once, then `value` once unless done), or `undefined`
 * where `for...of` would throw a `TypeError`: a result that is not an object, or one its own `done` getter revoked (a
 * Proxy whose trap revokes it), whose `value` no code can read.
 */
export function readStep(r: unknown): Step<unknown> | undefined {
  if (!isResult(r)) return undefined;
  const done: unknown = Reflect.get(r, "done");
  if (done) return DONE;
  const value: unknown = readGuarded(r, "value");
  return value === REVOKED ? undefined : { done: false, value };
}

/** The intrinsic array iterator: `Array.prototype[Symbol.iterator]`, and its iterators' `next`, as this module loaded. */
const ARRAY_VALUES: unknown = Reflect.get(Array.prototype, Symbol.iterator);
const ARRAY_ITERATOR: object = Object.getPrototypeOf((ARRAY_VALUES as () => object).call([])) as object;
const ARRAY_NEXT: unknown = Reflect.get(ARRAY_ITERATOR, "next");

/**
 * Whether `for...of` over `iterable`, whose iterator method is `method` (already read), walks it with the intrinsic
 * array iterator: an array whose method is `Array.prototype[Symbol.iterator]`, with the array iterators' `next`
 * unchanged. Such a walk reads the length and then the item at each index in turn, and closes nothing (the array
 * iterator has no `return`), so reading the array by index, the length each step, observes exactly what it would.
 *
 * It trusts the intrinsics as they were when this module loaded: `Array.prototype[Symbol.iterator]` and the array
 * iterators' `next` are compared with the functions captured then. Code that replaced either before this module loaded
 * is taken as the intrinsic (and its replacement then bypassed); a replacement made after is seen, and the walk goes
 * through the iterator. The array iterator's internal steps (reading `length`, then the index) are the language's own.
 */
export function walksAsArray(iterable: object, method: unknown): iterable is readonly unknown[] {
  return method === ARRAY_VALUES && isArrayGuarded(iterable) && Reflect.get(ARRAY_ITERATOR, "next") === ARRAY_NEXT;
}

/**
 * Closes `r`'s iterator, as leaving a `for...of` early does: its `return` is called if it is a function. Where
 * `for...of` would throw a `TypeError` (a `return` that is neither a function nor absent, or a result that is not an
 * object), this does nothing instead; an error `return` itself throws propagates.
 */
export function close(r: Reader<unknown>): void {
  // an iterator, or its `return`, revoked by caller code meanwhile has nothing to close
  if (isRevokedProxy(r.iterator)) return;
  const ret: unknown = Reflect.get(r.iterator, "return");
  if (typeof ret === "function" && !isRevokedProxy(ret)) Reflect.apply(ret, r.iterator, NO_ARGUMENTS);
}
