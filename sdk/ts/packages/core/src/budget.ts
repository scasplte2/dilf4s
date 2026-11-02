/** Byte budgets for the walks that expand a value (internal to this package). */
import type { TooLarge } from "./error.js";
import { MAX_WEIGHT } from "./value.js";

/**
 * `maxBytes` as a limit: a whole number of bytes from 0 to `Number.MAX_SAFE_INTEGER`. A fraction rounds down, a larger
 * limit (or `Infinity`) is the largest, and `NaN` or a negative limit is 0, so no limit compares false and runs unbounded.
 */
export const byteLimit = (maxBytes: number): number => (Number.isNaN(maxBytes) || maxBytes <= 0 ? 0 : Math.min(Math.floor(maxBytes), Number.MAX_SAFE_INTEGER));

/** The refusal of a length of at least `atLeast` bytes; from `MAX_WEIGHT` on, a saturated weight, it is uncountable. */
export const tooLarge = (limit: number, atLeast: bigint): TooLarge => ({ kind: "TooLarge", limit, atLeast: atLeast >= MAX_WEIGHT ? "uncountable" : atLeast });

/** A count of bytes against a limit; it stops at the first take that would pass the limit and remembers where. */
export class Meter {
  #used = 0;
  #over = 0n;

  constructor(readonly limit: number) {}

  get used(): number {
    return this.#used;
  }

  /** Whether `weight` (a lower bound on what is still to come) fits what is left; if not, the refusal is recorded. */
  admits(weight: bigint): boolean {
    const fits = weight <= BigInt(this.limit - this.#used);
    if (!fits) this.#over = BigInt(this.#used) + weight;
    return fits;
  }

  /**
   * Takes `n` more bytes (a whole number), if they fit. The sum is exact whenever it is at most the limit: both terms
   * are safe integers and the limit is below 2^53, so a sum that rounds is past the limit anyway.
   */
  take(n: number): boolean {
    const fits = this.#used + n <= this.limit;
    if (fits) this.#used += n;
    else this.#over = BigInt(this.#used) + BigInt(n);
    return fits;
  }

  /** The refusal, after {@link admits} or {@link take} said no. */
  refusal(): TooLarge {
    return tooLarge(this.limit, this.#over);
  }
}
