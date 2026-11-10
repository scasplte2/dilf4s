/**
 * Ordered lookups for caller-chosen text keys: they are compared, never hashed. A `Map` or `Set` of strings is not
 * safe for them: V8 hashes an integer-like string ("0", "65536", ...) by its value, with no seed, so keys a multiple of
 * the table size apart all share one bucket and n insertions cost O(n^2).
 */

/** A set of strings in UTF-16 code unit order: sorted runs of distinct power-of-two sizes, merged like a binary counter. */
export class KeySet {
  readonly #runs: string[][] = [];

  /** Whether `key` is present: a binary search of each run, O(log^2 n) comparisons. */
  has(key: string): boolean {
    return this.#runs.some((run) => found(run, key));
  }

  /** Adds `key`, or returns false (and changes nothing) if it is present: amortised O(log^2 n) comparisons. */
  add(key: string): boolean {
    if (this.has(key)) return false;
    let run = [key];
    for (let last = this.#runs.at(-1); last?.length === run.length; last = this.#runs.at(-1)) {
      this.#runs.pop();
      run = merge(last, run);
    }
    this.#runs.push(run);
    return true;
  }
}
Object.freeze(KeySet);
Object.freeze(KeySet.prototype);

function found(run: readonly string[], key: string): boolean {
  let lo = 0;
  let hi = run.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    const k = run[mid] ?? "";
    if (k === key) return true;
    if (k < key) lo = mid + 1;
    else hi = mid;
  }
  return false;
}

function merge(a: readonly string[], b: readonly string[]): string[] {
  const out: string[] = [];
  let i = 0;
  let j = 0;
  while (i < a.length && j < b.length) {
    const x = a[i] ?? "";
    const y = b[j] ?? "";
    if (x < y) {
      out.push(x);
      i++;
    } else {
      out.push(y);
      j++;
    }
  }
  return out.concat(a.slice(i), b.slice(j));
}

/**
 * The index of the item whose key's UTF-8 is `utf8` in `items`, sorted by `order` on their keys' UTF-8, or -1: a binary
 * search, O(log n) comparisons.
 */
export function search<A>(items: readonly A[], utf8Of: (a: A) => Uint8Array, utf8: Uint8Array, order: (a: Uint8Array, b: Uint8Array) => number): number {
  let lo = 0;
  let hi = items.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    const item = items[mid];
    if (item === undefined) return -1;
    const c = order(utf8Of(item), utf8);
    if (c === 0) return mid;
    if (c < 0) lo = mid + 1;
    else hi = mid;
  }
  return -1;
}
