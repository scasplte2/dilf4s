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

/** `Function.prototype.bind`, read once when this module loads: a function's own `bind` may be the caller's. */
const bind = Reflect.get(Function.prototype, "bind") as (this: unknown, self: unknown) => () => unknown;

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
  if (typeof method !== "function") return undefined;
  const iterator: unknown = Reflect.apply(method, iterable, NO_ARGUMENTS);
  if (!isResult(iterator)) return undefined;
  const next: unknown = Reflect.get(iterator, "next");
  // bound once, through Function.prototype's own bind (never the function's own `bind` or `call`, which may be the
  // caller's), so each step is one direct call
  return typeof next === "function" ? { iterator: iterator as Iterator<T>, next: Reflect.apply(bind, next, [iterator]) } : undefined;
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
  return method === ARRAY_VALUES && Array.isArray(iterable) && Reflect.get(ARRAY_ITERATOR, "next") === ARRAY_NEXT;
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
