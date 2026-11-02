/** Byte budgets for the walks that expand a value (internal to this package). */
import type { TooLarge } from "./error.js";

/**
 * `maxBytes` as a limit: a whole number of bytes from 0 to `Number.MAX_SAFE_INTEGER`. A fraction rounds down, a larger
 * limit (or `Infinity`) is the largest, and `NaN` or a negative limit is 0, so no limit compares false and runs unbounded.
 */
export const byteLimit = (maxBytes: number): number => (Number.isNaN(maxBytes) || maxBytes <= 0 ? 0 : Math.min(Math.floor(maxBytes), Number.MAX_SAFE_INTEGER));

/** The refusal of a length over `limit`: it carries the limit only (docs/specs/conformance.md), never a measured size. */
export const tooLarge = (limit: number): TooLarge => ({ kind: "TooLarge", limit });

/** A count of bytes against a limit; it stops at the first take that would pass the limit. */
export class Meter {
  #used = 0;

  constructor(readonly limit: number) {}

  get used(): number {
    return this.#used;
  }

  /** Whether `weight` (a lower bound on what is still to come) fits what is left. */
  admits(weight: bigint): boolean {
    return weight <= BigInt(this.limit - this.#used);
  }

  /**
   * Takes `n` more bytes (a whole number), if they fit. The sum is exact whenever it is at most the limit: both terms
   * are safe integers and the limit is below 2^53, so a sum that rounds is past the limit anyway.
   */
  take(n: number): boolean {
    const fits = this.#used + n <= this.limit;
    if (fits) this.#used += n;
    return fits;
  }

  /** The refusal, after {@link admits} or {@link take} said no. */
  refusal(): TooLarge {
    return tooLarge(this.limit);
  }
}
