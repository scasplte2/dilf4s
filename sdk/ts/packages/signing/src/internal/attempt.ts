/**
 * The one boundary where a third-party call may throw. noble signals malformed input (a point that does not decode, a
 * scalar out of range) by throwing; this turns such a throw into `undefined`, as Scala's `Either.catchNonFatal` and
 * the backends' `call(...)` do. It wraps only noble calls on untrusted bytes; everything else validates first, so no
 * library function throws. One of the SDK's two sanctioned catches (the ESLint config allows a catch in this file and
 * in core's `guard.ts`, nowhere else); it stays apart from core's guard, which catches only `Array.isArray`'s refusal
 * of a revoked Proxy and never runs other code, where this wraps a library's whole computation.
 */
export function attempt<T>(f: () => T): T | undefined {
  try {
    return f();
  } catch {
    return undefined;
  }
}
