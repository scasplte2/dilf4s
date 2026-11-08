/**
 * Checks of what a caller passes past the types (from JavaScript, or a cast), made once at each public entry point, in
 * O(1) for each argument, before it is read: an argument of the wrong type is core's `TypeMismatch`, never a throw or an
 * argument read as another type. Past the check the verifiers work on plain, owned data. An exception from caller code
 * (a getter, a Proxy's trap or handler, an iterator's methods) propagates unchanged: `docs/specs/canonical-encoding.md` §1, "Caller code".
 * Internal: not exported from the package.
 */
import { err, isValue, ok, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { close, isAlgorithm, isArrayGuarded, isArrayLength, isResult, isRevokedProxy, NOT_A_RESULT, NOT_ITERABLE, openWith, ownBytes, readGuarded, UNBOUNDED, readStep, REVOKED, typeMismatch, walksAsArray } from "@kd5ujc/dilf4s-core/internal";
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

/**
 * `x` through core's byte boundary within `max` bytes (`ownBytes`: measured by its internal length, a view of stable
 * memory or else one copy), or a refusal; past `max`, uncopied. For bytes read before any caller code runs again;
 * bytes kept, or in use while caller code runs, take `ownCopy`.
 */
export const bytesArg = (x: unknown): Result<Uint8Array, TypeMismatch> => {
  // bytes with no bound of their own (data to hash, to commit): the boundary refuses only what is not bytes
  const view = ownBytes(x, UNBOUNDED);
  return view === undefined || typeof view === "number" ? err(NOT_BYTES) : ok(view);
};

let oversizedOf!: (length: number) => Oversized;

/**
 * Bytes a caller passed that are longer than their field's bound: their length alone, never their bytes (they are
 * never copied). A class of its own, with a private brand, so the compiler keeps it apart from bytes: nothing that
 * hashes, compares, encodes or returns bytes takes one, and the only way from a {@link Measured} value to bytes is a
 * check that narrows it ({@link isMeasuredBytes}, {@link isSized}), made at the stage the specs pin for its size.
 */
export class Oversized {
  readonly #oversized = true;
  private constructor(readonly length: number) {
    Object.freeze(this);
  }

  /** Whether `x` is one: one private-brand check. */
  static is(x: unknown): x is Oversized {
    return typeof x === "object" && x !== null && #oversized in x;
  }

  static {
    oversizedOf = (length) => new Oversized(length);
  }
}
Object.freeze(Oversized);
Object.freeze(Oversized.prototype);

/**
 * A caller's bytes as the boundary measured them against a field's bound: the owned bytes, or an {@link Oversized}
 * for ones longer (never copied). The boundary refuses nothing past the bound: the oversized value flows on to the
 * check that refused such bytes before (a digest's size, a key's mode, a leaf's weight), at the stage the specs pin.
 * Both have a `length`, the bytes' own.
 */
export type Measured = Uint8Array | Oversized;

/** What core's boundary gives (bytes, or the length of bytes past the bound) as a {@link Measured} value. */
export const measured = (b: Uint8Array | number): Measured => (typeof b === "number" ? oversizedOf(b) : b);

/** Whether measured bytes are bytes (within their bound): the narrowing every use of them as bytes takes. */
export const isMeasuredBytes = (m: Measured): m is Uint8Array => !Oversized.is(m);

/** Whether measured bytes are bytes of exactly `size` (a digest of its profile's size). */
export const isSized = (m: Measured, size: number): m is Uint8Array => !Oversized.is(m) && m.length === size;

/** `x` if it is a canonical value made by core (`isValue`, one private-brand check: no Proxy trap runs). */
export const valueArg = (x: unknown): Result<Value, TypeMismatch> => (isValue(x) ? ok(x) : err(NOT_A_VALUE));

/** `x` if it is a `bigint`, never converted. */
export const bigintArg = (x: unknown): Result<bigint, TypeMismatch> => (typeof x === "bigint" ? ok(x) : err(NOT_A_BIGINT));

/** `x` if it is one of core's `HASH_ALGORITHMS`, compared with each name, never converted. */
export const algorithmArg = (x: unknown): Result<HashAlgorithm, TypeMismatch> => (isAlgorithm(x) ? ok(x) : err(NOT_AN_ALGORITHM));

/** Whether `x` may have its properties read: an object (a Proxy's `get` traps are the caller's own code). */
export const isObject = (x: unknown): x is object => ((typeof x === "object" && x !== null) || typeof x === "function") && !isRevokedProxy(x);

/**
 * A caller's array, read by index: `isArrayGuarded` first (no trap runs on a Proxy for it, and a revoked Proxy is refused), then its length once and
 * each item once, each given to `item`, whose first refusal is the result. `what` names the array in a refusal.
 * `bound`, when given, refuses the length (read once) before any item is read: an array with a size limit is never
 * walked past it.
 */
export function readArray<T, E>(x: unknown, what: string, item: (v: unknown, i: number) => Result<T, E>, bound?: (length: number) => E | undefined): Result<T[], E | TypeMismatch> {
  if (!isArrayGuarded(x)) return err(expected(what));
  const length: unknown = (x as readonly unknown[]).length;
  if (!isArrayLength(length)) return err(expected(what));
  const outside = bound?.(length);
  if (outside !== undefined) return err(outside);
  const out: T[] = [];
  for (let i = 0; i < length; i++) {
    // guarded: an earlier read (the length's, an item's: a Proxy's trap) may have revoked the array
    const v: unknown = readGuarded(x, i);
    if (v === REVOKED) return err(expected(what));
    const r = item(v, i);
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
  return isArrayGuarded(x) ? x : undefined;
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
    // each read guarded: the iterator method's read, or an item's, may have revoked the array (a Proxy's trap)
    const length: unknown = readGuarded(x, "length");
    if (!isArrayLength(length)) return err(typeMismatch(NOT_ITERABLE));
    for (let i = 0; i < length; i++) {
      const v: unknown = readGuarded(x, i);
      if (v === REVOKED) return err(typeMismatch(NOT_ITERABLE));
      const r = step(state, v, i);
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
      const r = readStep(read.next());
      if (r === undefined) return err(typeMismatch(NOT_A_RESULT));
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
