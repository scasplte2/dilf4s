/**
 * The one boundary where a third-party call may throw. noble signals malformed input (a point that does not decode, a
 * scalar out of range) by throwing; this turns such a throw into `undefined`, as Scala's `Either.catchNonFatal` and
 * the backends' `call(...)` do. It wraps only noble calls on untrusted bytes; everything else validates first, so no
 * library function throws.
 */
export function attempt<T>(f: () => T): T | undefined {
  // eslint-disable-next-line no-restricted-syntax -- the single sanctioned boundary: noble throws on adversarial bytes
  try {
    return f();
  } catch {
    return undefined;
  }
}
