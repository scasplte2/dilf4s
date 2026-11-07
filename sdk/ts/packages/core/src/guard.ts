/**
 * The SDK's one sanctioned catch in its own code (signing's `internal/attempt.ts` is the other, around noble): asking
 * whether a caller's object is an array. `Array.isArray` throws a `TypeError` for a revoked Proxy (or a Proxy whose
 * target is one), and no other question can be put to such an object without the engine throwing too: reading a
 * property, its prototype or its keys all throw. `Array.isArray` runs no caller code (a Proxy's traps are not called
 * for it), so the catch below can only ever see the engine's own refusal, never a caller's exception, and nothing else
 * is ever placed inside it. Internal to this SDK, through `@kd5ujc/dilf4s-core/internal`.
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
