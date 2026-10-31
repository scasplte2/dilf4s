/** A success holding `value`. */
export interface Ok<T> {
  readonly ok: true;
  readonly value: T;
}

/** A failure holding `error`. */
export interface Err<E> {
  readonly ok: false;
  readonly error: E;
}

/** The outcome of every fallible function in this library: library functions never throw. */
export type Result<T, E> = Ok<T> | Err<E>;

export function ok<T>(value: T): Ok<T> {
  return { ok: true, value };
}

export function err<E>(error: E): Err<E> {
  return { ok: false, error };
}

/** Applies `f` to a success; a failure passes through. */
export function map<T, U, E>(r: Result<T, E>, f: (t: T) => U): Result<U, E> {
  return r.ok ? ok(f(r.value)) : r;
}

/** Chains a fallible step after a success; a failure passes through. */
export function andThen<T, U, E>(r: Result<T, E>, f: (t: T) => Result<U, E>): Result<U, E> {
  return r.ok ? f(r.value) : r;
}

/** Every success in order, or the first failure. */
export function all<T, E>(results: Iterable<Result<T, E>>): Result<readonly T[], E> {
  const out: T[] = [];
  for (const r of results) {
    if (!r.ok) return r;
    out.push(r.value);
  }
  return ok(out);
}
