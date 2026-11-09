package xyz.kd5ujc.dilf4s.internal;

import xyz.kd5ujc.core.canonical.CanonicalValue;

/**
 * One function per kind of canonical value, which {@code Values.visit} picks by an exhaustive match. Internal: not part
 * of the API, and it names the reference's types.
 *
 * @param <R> what each returns
 */
public interface ValueCases<R> {

  /** The null value. */
  R nul();

  /** A boolean. */
  R bool(CanonicalValue.CBool b);

  /** An integer. */
  R integer(CanonicalValue.CInt i);

  /** A rational that is not an integer. */
  R rational(CanonicalValue.CRational r);

  /** Text. */
  R text(CanonicalValue.CText t);

  /** A byte string. */
  R bytes(CanonicalValue.CBytes b);

  /** An array. */
  R array(CanonicalValue.CArray a);

  /** A map. */
  R map(CanonicalValue.CMap m);
}
