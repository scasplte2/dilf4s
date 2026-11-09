/**
 * Exact rational arithmetic for the operations (`numerics` `Ratio.scala`, `lang/rust/jlvm/src/ratio.rs`), on core's
 * numbers: an integer, or a reduced rational with a positive denominator. Every result is built by core's
 * `RationalValue.number`, which reduces it (an integer result skips the gcd); operands are within the JLVM's number
 * bound (4096 bits) before any arithmetic, so no intermediate is anywhere near the data model's bound, whose refusal
 * would be the operation's `Limit`. Every division is by a value already shown to be non-zero.
 */
import { IntValue, RationalValue } from "@kd5ujc/dilf4s-core";

import { arithmetic } from "./error.js";
import type { JlvmOp } from "./op.js";
import { bad, good, parts, tooLarge, type Num, type Out } from "./values.js";

/** The number `n / d` (`d > 0`), reduced; within the data model's bound, as every result here is. */
function number(op: JlvmOp, n: bigint, d: bigint): Out<Num> {
  const r = d === 1n ? IntValue.of(n) : RationalValue.number(n, d);
  return r.ok ? good(r.value) : bad(tooLarge(op));
}

const integer = (op: JlvmOp, n: bigint): Out<Num> => number(op, n, 1n);

export function add(op: JlvmOp, a: Num, b: Num): Out<Num> {
  if (a.kind === "int" && b.kind === "int") return integer(op, a.value + b.value);
  const [an, ad] = parts(a);
  const [bn, bd] = parts(b);
  return number(op, an * bd + bn * ad, ad * bd);
}

export function negate(op: JlvmOp, a: Num): Out<Num> {
  const [n, d] = parts(a);
  return number(op, -n, d);
}

export function subtract(op: JlvmOp, a: Num, b: Num): Out<Num> {
  if (a.kind === "int" && b.kind === "int") return integer(op, a.value - b.value);
  const [an, ad] = parts(a);
  const [bn, bd] = parts(b);
  return number(op, an * bd - bn * ad, ad * bd);
}

export function multiply(op: JlvmOp, a: Num, b: Num): Out<Num> {
  if (a.kind === "int" && b.kind === "int") return integer(op, a.value * b.value);
  const [an, ad] = parts(a);
  const [bn, bd] = parts(b);
  return number(op, an * bn, ad * bd);
}

const isZero = (x: Num): boolean => x.kind === "int" && x.value === 0n;

const divisionByZero = (op: JlvmOp) => bad<Num>(arithmetic(op, "division by zero"));

/** `a / b`; division by zero is an `Arithmetic` error. */
export function divide(op: JlvmOp, a: Num, b: Num): Out<Num> {
  if (isZero(b)) return divisionByZero(op);
  const [an, ad] = parts(a);
  const [bn, bd] = parts(b);
  // the sign moves to the numerator, so the denominator stays positive
  return bn < 0n ? number(op, -an * bd, ad * -bn) : number(op, an * bd, ad * bn);
}

/** Round toward zero. */
export function truncate(a: Num): bigint {
  const [n, d] = parts(a);
  return n / d;
}

export function floor(a: Num): bigint {
  const [n, d] = parts(a);
  const q = n / d;
  return n < 0n && q * d !== n ? q - 1n : q;
}

export function ceil(a: Num): bigint {
  const [n, d] = parts(a);
  const q = n / d;
  return n > 0n && q * d !== n ? q + 1n : q;
}

/** Half away from zero. */
export function roundHalfUp(a: Num): bigint {
  const [n, d] = parts(a);
  const magnitude = n < 0n ? -n : n;
  const q = (magnitude * 2n + d) / (d * 2n);
  return n < 0n ? -q : n > 0n ? q : 0n;
}

/** Half to even. */
export function roundHalfEven(a: Num): bigint {
  const [n, d] = parts(a);
  const lo = floor(a);
  const twice = (n - lo * d) * 2n;
  if (twice < d) return lo;
  if (twice === d && lo % 2n === 0n) return lo;
  return lo + 1n;
}

/** The truncated remainder `a - b * truncate(a / b)`; division by zero is an `Arithmetic` error. */
export function remainder(op: JlvmOp, a: Num, b: Num): Out<Num> {
  if (isZero(b)) return divisionByZero(op);
  if (a.kind === "int" && b.kind === "int") return integer(op, a.value % b.value);
  const q = divide(op, a, b);
  if (!q.ok) return q;
  const t = integer(op, truncate(q.value));
  if (!t.ok) return t;
  const m = multiply(op, b, t.value);
  return m.ok ? subtract(op, a, m.value) : m;
}

/** The absolute value. */
export function absolute(op: JlvmOp, a: Num): Out<Num> {
  const [n, d] = parts(a);
  return n < 0n ? number(op, -n, d) : good(a);
}

/** An integer power; a negative exponent inverts, so zero to a negative power is division by zero. */
export function power(op: JlvmOp, a: Num, exponent: number): Out<Num> {
  const [n, d] = parts(a);
  const e = BigInt(Math.abs(exponent));
  if (exponent >= 0) return number(op, n ** e, d ** e);
  if (n === 0n) return divisionByZero(op);
  // the inverse d/n, its sign on the numerator
  return n < 0n ? number(op, (-d) ** e, (-n) ** e) : number(op, d ** e, n ** e);
}
