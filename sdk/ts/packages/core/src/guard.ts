/**
 * The SDK's sanctioned catches in its own code (signing's `internal/attempt.ts` is the other, around noble), each
 * around one intrinsic call that runs no caller code: asking whether a caller's object is an array, and whether a
 * buffer is an unshared `ArrayBuffer` ({@link isArrayBufferBranded}). `Array.isArray` throws a `TypeError` for a revoked Proxy (or a Proxy whose
 * target is one), and no other question can be put to such an object without the engine throwing too: reading a
 * property, its prototype or its keys all throw. `Array.isArray` runs no caller code (a Proxy's traps are not called
 * for it), so the catch below can only ever see the engine's own refusal, never a caller's exception, and nothing else
 * is ever placed inside it. An exception from caller code (a getter, a trap, a Proxy whose handler is unusable)
 * propagates, by the rule of `docs/specs/canonical-encoding.md` §1, "Caller code". Internal to this SDK, through
 * `@kd5ujc/dilf4s-core/internal`.
 */

/**
 * `Array.isArray(x)`, with a revoked Proxy (which no code can use) answered `false` instead of the engine's
 * `TypeError`: so every array entry point refuses it as the `TypeMismatch` it gives a non-array. O(1), no caller code
 * runs, nothing is allocated.
 */
export function isArrayGuarded(x: unknown): x is unknown[] {
  try {
    return Array.isArray(x);
  } catch {
    return false;
  }
}

/**
 * Whether `x` is a revoked Proxy (or a Proxy whose target, at any depth, is one): an object whose every property read
 * throws. A field reader asks this before its first read, so such an object is refused as a `TypeMismatch` instead of
 * throwing from the engine. O(1), no caller code runs.
 */
export function isRevokedProxy(x: unknown): boolean {
  if ((typeof x !== "object" || x === null) && typeof x !== "function") return false;
  try {
    Array.isArray(x);
    return false;
  } catch {
    return true;
  }
}

/**
 * What {@link readGuarded} gives for a read of an object that is now a revoked Proxy: a symbol of this module's own,
 * which no check of a caller's value accepts as a type the SDK takes. A reader that takes any value (a version, a
 * decoded value, a field handed to a codec) compares with it before keeping what it read.
 */
export const REVOKED: unique symbol = Symbol("dilf4s: a revoked Proxy");

/**
 * `o[key]`, read once (`Reflect.get`), or {@link REVOKED} when `o` is now a revoked Proxy: caller code an earlier read
 * ran (a getter, a Proxy's trap) may have revoked `o`, and the engine would throw a `TypeError` for the read. Every
 * read of a caller's object after its first check goes through this, so such an object is refused as a
 * `TypeMismatch`, never a throw. O(1): the guard's one `Array.isArray`, then the read, the only caller code it runs.
 */
export function readGuarded(o: object, key: PropertyKey): unknown {
  return isRevokedProxy(o) ? REVOKED : Reflect.get(o, key);
}

const NO_ARGUMENTS: readonly unknown[] = Object.freeze([]);

/** `ArrayBuffer.prototype`'s own `byteLength` getter, read once when this module loads. */
const arrayBufferByteLength: unknown = Reflect.get(Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "byteLength") ?? {}, "get");

/**
 * Whether `x` is an `ArrayBuffer`, never a `SharedArrayBuffer`, by the engine's own brand check, which a prototype
 * cannot fake: `ArrayBuffer.prototype`'s `byteLength` getter throws a `TypeError` for anything else, a
 * `SharedArrayBuffer` given `ArrayBuffer.prototype` included. The getter runs no caller code (it reads internal slots),
 * so the catch below sees only that refusal: the guard's second engine-only catch, nothing else placed inside it.
 * O(1), nothing allocated (its argument list is one frozen array).
 */
export function isArrayBufferBranded(x: unknown): boolean {
  if (typeof arrayBufferByteLength !== "function" || typeof x !== "object" || x === null) return false;
  try {
    Reflect.apply(arrayBufferByteLength, x, NO_ARGUMENTS);
    return true;
  } catch {
    return false;
  }
}
