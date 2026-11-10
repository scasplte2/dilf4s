/**
 * Checks of what a caller passes past the types (from JavaScript, or a cast), made once at each public entry point, in
 * O(1) for each argument, before it is read: an argument of the wrong type is core's `TypeMismatch`, never a throw, an
 * argument read as another type, or (the reverted attempt's second regression) bytes read as empty. Past the check the
 * schemes work on plain, owned data. An exception from caller code (a getter, a Proxy's trap or handler, a source of
 * randomness) propagates unchanged: `docs/specs/canonical-encoding.md` §1, "Caller code".
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { isArrayGuarded, isArrayLength, isRevokedProxy, ownBytes, readGuarded, REVOKED, typeMismatch, UNBOUNDED } from "@kd5ujc/dilf4s-core/internal";

/** A message, through core's boundary: a message to sign or verify has no length bound in any scheme's spec. */
function messageBytes(x: unknown): Uint8Array | undefined {
  const b = ownBytes(x, UNBOUNDED);
  return typeof b === "number" ? undefined : b;
}

export const NOT_BYTES = typeMismatch("expected a Uint8Array");
export const NOT_A_NUMBER = typeMismatch("expected a number");
export const NOT_A_STRING = typeMismatch("expected a string");

/** The refusal of an argument that is not `what`. */
export const expected = (what: string): TypeMismatch => typeMismatch(`expected ${what}`);

/** The refusal of an item of a caller's array that is not `what`, at index `i`. */
export const expectedAt = (what: string, i: number): TypeMismatch => typeMismatch(`expected ${what} at index ${i.toString()}`);

/** Whether `x` may have its properties read: an object (a Proxy's `get` traps are the caller's own code). */
export const isObject = (x: unknown): x is object => ((typeof x === "object" && x !== null) || typeof x === "function") && !isRevokedProxy(x);

/**
 * Bytes to verify, through core's byte boundary (`ownBytes`, unbounded): measured by their internal length, then a view of stable
 * memory (this realm's own unshared, non-resizable `ArrayBuffer`) or else one copy, so a check that reads them more than
 * once reads one snapshot; `undefined` if they are not a `Uint8Array`.
 */
export const viewOf = (x: unknown): Uint8Array | undefined => messageBytes(x);

/**
 * Bytes to sign, through core's byte boundary (`ownBytes`, unbounded). Signing reads a message more than once (Ed25519 hashes it
 * for the nonce and again for the challenge, BIP-340 likewise): another thread rewriting it between the reads would
 * give two signatures with one nonce over two messages, which reveals the key. So memory another thread can write (a
 * `SharedArrayBuffer`, told apart by the engine's brand check, never by a prototype), a resizable buffer or another
 * realm's is copied once; this realm's own unshared, fixed `ArrayBuffer`, which nothing changes while the signer runs
 * (it calls no caller code), is read in place.
 */
export function signedOf(x: unknown): Uint8Array | undefined {
  return messageBytes(x);
}

/** {@link viewOf} as a `Result`. */
export const bytesArg = (x: unknown): Result<Uint8Array, TypeMismatch> => {
  const view = messageBytes(x);
  return view === undefined ? err(NOT_BYTES) : ok(view);
};

/**
 * A caller's array, read by index: `isArrayGuarded` first (no trap runs on a Proxy for it, and a revoked Proxy is refused), then its length once and
 * each item once, each given to `item`, whose first refusal is the result.
 */
export function readArray<T, E>(x: unknown, what: string, item: (v: unknown, i: number) => Result<T, E>): Result<T[], E | TypeMismatch> {
  if (!isArrayGuarded(x)) return err(expected(what));
  const length: unknown = (x as readonly unknown[]).length;
  if (!isArrayLength(length)) return err(expected(what));
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

/** A verdict without an allocation: the two successes of a boolean check, shared and frozen. */
export const TRUE: Result<boolean, never> = Object.freeze({ ok: true, value: true });
export const FALSE: Result<boolean, never> = Object.freeze({ ok: true, value: false });
/** The shared result of a boolean verdict. */
export const verdict = (b: boolean): Result<boolean, never> => (b ? TRUE : FALSE);
