/**
 * Checks of what a caller passes past the types (from JavaScript, or a cast), made once at each public entry point, in
 * O(1) for each argument, before it is read: an argument of the wrong type is core's `TypeMismatch`, never a throw, an
 * argument read as another type, or (the reverted attempt's second regression) bytes read as empty. Past the check the
 * schemes work on plain, owned data.
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { copyBytes, isLocalBuffer, plainBytes, typeMismatch } from "@kd5ujc/dilf4s-core/internal";

export const NOT_BYTES = typeMismatch("expected a Uint8Array");
export const NOT_A_NUMBER = typeMismatch("expected a number");
export const NOT_A_STRING = typeMismatch("expected a string");

/** The refusal of an argument that is not `what`. */
export const expected = (what: string): TypeMismatch => typeMismatch(`expected ${what}`);

/** The refusal of an item of a caller's array that is not `what`, at index `i`. */
export const expectedAt = (what: string, i: number): TypeMismatch => typeMismatch(`expected ${what} at index ${i.toString()}`);

/** Whether `x` may have its properties read: an object (a Proxy's `get` traps are the caller's own code). */
export const isObject = (x: unknown): x is object => (typeof x === "object" && x !== null) || typeof x === "function";

/**
 * Bytes to verify: a plain view of the caller's memory (its own `length` not believed), read once by the check that
 * uses it, or `undefined` if they are not a `Uint8Array`.
 */
export const viewOf = (x: unknown): Uint8Array | undefined => plainBytes(x);

/**
 * Bytes to sign: a plain view when only this thread can change them, otherwise (a `SharedArrayBuffer`, or another
 * realm's memory) a copy taken once. Signing reads a message more than once (Ed25519 hashes it for the nonce and again
 * for the challenge, BIP-340 likewise): another thread rewriting it between the reads would give two signatures with one
 * nonce over two messages, which reveals the key.
 */
export function signedOf(x: unknown): Uint8Array | undefined {
  const view = plainBytes(x);
  return view === undefined || isLocalBuffer(view) ? view : copyBytes(view);
}

/** {@link viewOf} as a `Result`. */
export const bytesArg = (x: unknown): Result<Uint8Array, TypeMismatch> => {
  const view = plainBytes(x);
  return view === undefined ? err(NOT_BYTES) : ok(view);
};

/**
 * A caller's array, read by index: `Array.isArray` first (no trap runs on a Proxy for it), then its length once and
 * each item once, each given to `item`, whose first refusal is the result.
 */
export function readArray<T, E>(x: unknown, what: string, item: (v: unknown, i: number) => Result<T, E>): Result<T[], E | TypeMismatch> {
  if (!Array.isArray(x)) return err(expected(what));
  const length: unknown = (x as readonly unknown[]).length;
  if (typeof length !== "number") return err(expected(what));
  const out: T[] = [];
  for (let i = 0; i < length; i++) {
    const r = item((x as readonly unknown[])[i], i);
    if (!r.ok) return r;
    out.push(r.value);
  }
  return ok(out);
}

/** A verdict without an allocation: the two successes of a boolean check, shared and frozen. */
export const TRUE: Result<boolean, never> = Object.freeze({ ok: true, value: true });
export const FALSE: Result<boolean, never> = Object.freeze({ ok: true, value: false });
export const verdict = (b: boolean): Result<boolean, never> => (b ? TRUE : FALSE);
