/**
 * Reading a caller's iterable as `for...of` does, where what `for...of` throws a `TypeError` for (an iterator method,
 * iterator, `next` or result not of the protocol's type) is found instead, to be refused as a typed error: library code
 * does not throw. Caller code it runs (a getter, an iterator's methods) may throw. Internal to this package.
 */

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
  return (typeof r === "object" && r !== null) || typeof r === "function";
}

const NO_ARGUMENTS: readonly unknown[] = Object.freeze([]);

/**
 * `iterable`'s iterator, opened as `for...of` opens it (its `Symbol.iterator` method read and called once, then the
 * iterator's `next` read once), or `undefined` where `for...of` would throw a `TypeError` before reading any item: no
 * iterator method, an iterator that is not an object, or a `next` that is not a function.
 */
export function open<T>(iterable: object): Reader<T> | undefined {
  const method: unknown = Reflect.get(iterable, Symbol.iterator);
  if (typeof method !== "function") return undefined;
  const iterator: unknown = Reflect.apply(method, iterable, NO_ARGUMENTS);
  if (!isResult(iterator)) return undefined;
  const next: unknown = Reflect.get(iterator, "next");
  // called through Reflect.apply, never the function's own `call` or `bind`, which may be the caller's
  return typeof next === "function" ? { iterator: iterator as Iterator<T>, next: () => Reflect.apply(next, iterator, NO_ARGUMENTS) as unknown } : undefined;
}

/**
 * Closes `r`'s iterator, as leaving a `for...of` early does: its `return` is called if it is a function. Where
 * `for...of` would throw a `TypeError` (a `return` that is neither a function nor absent, or a result that is not an
 * object), this does nothing instead; an error `return` itself throws propagates.
 */
export function close(r: Reader<unknown>): void {
  const ret: unknown = Reflect.get(r.iterator, "return");
  if (typeof ret === "function") Reflect.apply(ret, r.iterator, NO_ARGUMENTS);
}
