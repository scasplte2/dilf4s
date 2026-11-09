/**
 * A canonical map's entries as a persistent AVL tree in canonical key order (shorter UTF-8 first, then bytewise: the
 * order of the encoded keys, RFC 8949 section 4.2.1), as Rust's `Map` keeps them (`sdk/rust/core/src/value/map.rs`).
 *
 * Every node is immutable and shared between the maps that hold it. An update copies the path from the root to the key
 * it changes, rebalancing on the way back, so it builds O(log n) nodes and shares every other subtree with the map it
 * came from. Each node caches its subtree's height, its deepest value and its exact weight, so a map's depth and weight
 * stay exact and O(1) through any sequence of updates, a saturated weight included. A node holds its entry rather than
 * being it: a map built from entries in order (decoded, or from `MapValue.of`) keeps them as they were made, so a walk
 * of all of them reads them in the order they lie in memory.
 *
 * Nothing here recurses. Every walk keeps an explicit path or stack of at most the tree's height, and an AVL tree of n
 * entries is lower than `1.4405 log2(n + 2)`: under 77 for any n an engine can hold (2^53), and 92 for any n below
 * 2^64.
 *
 * The nodes and entries are this package's alone: nothing writes one once it is built.
 */
import { compareBytes } from "./utf8.js";
import type { Value } from "./value.js";

/** A map entry: its key, the key's UTF-8, and its value. */
export interface Entry {
  readonly key: string;
  readonly utf8: Uint8Array;
  readonly value: Value;
}

/**
 * An exact sum of weights: a number while it is a safe integer, a bigint from there on. Only a value built from shared
 * parts weighs 2^53 or more, so the bigint is rare, and the sum is exact either way: a map's weight saturates at
 * `MAX_WEIGHT` only when it is read, so removing a heavy entry restores the exact weight of what is left.
 */
export type Sum = number | bigint;

/** One entry, its subtrees, and what they cache. */
export interface Node {
  readonly entry: Entry;
  readonly left: Node | undefined;
  readonly right: Node | undefined;
  /** 1 for a leaf. */
  readonly height: number;
  /** The depth of the deepest value in this subtree. */
  readonly deepest: number;
  /** The sum over this subtree of each entry's `1 + key UTF-8 length + value weight`, exactly. */
  readonly weight: Sum;
}

/**
 * The order of keys in an encoded map: their encodings compared bytewise (RFC 8949 section 4.2.1). For text keys that
 * is shorter UTF-8 first, then bytewise, because the head encodes the length in increasing order.
 */
export function canonicalKeyOrder(a: Uint8Array, b: Uint8Array): number {
  return a.length !== b.length ? a.length - b.length : compareBytes(a, b);
}

const SAFE = BigInt(Number.MAX_SAFE_INTEGER);

/** `a + b`, exactly. A double sum of two safe integers past the safe range rounds to at least 2^53, so the test is exact. */
const plus = (a: Sum, b: Sum): Sum => (typeof a === "number" && typeof b === "number" && a + b <= Number.MAX_SAFE_INTEGER ? a + b : BigInt(a) + BigInt(b));

/** An entry's share of its map's weight: one node, the key's UTF-8 bytes, and the value's weight. */
const share = (bytes: number, weight: bigint): Sum => (weight <= SAFE ? plus(1 + bytes, Number(weight)) : 1n + BigInt(bytes) + weight);

const heightOf = (t: Node | undefined): number => t?.height ?? 0;

/** The node of `entry` over `left` and `right`, which are already balanced against each other. */
function node(entry: Entry, left: Node | undefined, right: Node | undefined): Node {
  return {
    entry,
    left,
    right,
    height: Math.max(heightOf(left), heightOf(right)) + 1,
    deepest: Math.max(entry.value.depth, left?.deepest ?? 0, right?.deepest ?? 0),
    weight: plus(plus(left?.weight ?? 0, right?.weight ?? 0), share(entry.utf8.length, entry.value.weight)),
  };
}

/**
 * `e` over `left` and `right`, whose heights differ by at most 2, rebalanced with one or two rotations. A child as high
 * inside as outside (possible after a removal) takes the single rotation.
 */
function balanced(e: Entry, left: Node | undefined, right: Node | undefined): Node {
  const [hl, hr] = [heightOf(left), heightOf(right)];
  if (hl > hr + 1 && left !== undefined) {
    const inner = left.right;
    if (heightOf(left.left) >= heightOf(inner)) return node(left.entry, left.left, node(e, inner, right));
    if (inner !== undefined) return node(inner.entry, node(left.entry, left.left, inner.left), node(e, inner.right, right));
  } else if (hr > hl + 1 && right !== undefined) {
    const inner = right.left;
    if (heightOf(right.right) >= heightOf(inner)) return node(right.entry, node(e, left, inner), right.right);
    if (inner !== undefined) return node(inner.entry, node(e, left, inner.left), node(right.entry, inner.right, right.right));
  }
  return node(e, left, right);
}

/** A step of a search path: the node, and whether the search went left from it. */
type Turn = readonly [Node, boolean];

/** The path to `utf8`'s node, or to where it would go, and that node if present. O(log n) comparisons. */
function search(root: Node | undefined, utf8: Uint8Array): readonly [Turn[], Node | undefined] {
  const path: Turn[] = [];
  for (let t = root; t !== undefined; ) {
    const c = canonicalKeyOrder(utf8, t.entry.utf8);
    if (c === 0) return [path, t];
    path.push([t, c < 0]);
    t = c < 0 ? t.left : t.right;
  }
  return [path, undefined];
}

/** The path above `child` rebuilt bottom up, each node rebalanced: one node per level, every other subtree shared. */
function rebuilt(path: readonly Turn[], child: Node | undefined): Node | undefined {
  let t = child;
  for (let i = path.length - 1; i >= 0; i--) {
    const turn = path[i];
    if (turn === undefined) continue;
    const [p, left] = turn;
    t = left ? balanced(p.entry, t, p.right) : balanced(p.entry, p.left, t);
  }
  return t;
}

/** The entry at `utf8`, if present: O(log n) comparisons. */
export function found(root: Node | undefined, utf8: Uint8Array): Entry | undefined {
  for (let t = root; t !== undefined; ) {
    const c = canonicalKeyOrder(utf8, t.entry.utf8);
    if (c === 0) return t.entry;
    t = c < 0 ? t.left : t.right;
  }
  return undefined;
}

/** The tree with `e` set, in place of the entry at its key if there is one, and whether the key is new: O(log n). */
export function inserted(root: Node | undefined, e: Entry): readonly [Node, boolean] {
  const [path, at] = search(root, e.utf8);
  const leaf = node(e, at?.left, at?.right);
  return [rebuilt(path, leaf) ?? leaf, at === undefined];
}

/** The tree without its least entry, and that entry: the path down the left spine, rebuilt. */
function withoutLeast(t: Node): readonly [Node | undefined, Entry] {
  const path: Turn[] = [];
  let least = t;
  for (let l = t.left; l !== undefined; l = l.left) {
    path.push([least, true]);
    least = l;
  }
  return [rebuilt(path, least.right), least.entry];
}

/** What a removal finds when the key is absent: nothing to rebuild, so the map is its own result. */
export const ABSENT: unique symbol = Symbol("absent");

/** The tree without `utf8`'s entry, or {@link ABSENT} when it holds none: O(log n), one node per level. */
export function removed(root: Node | undefined, utf8: Uint8Array): Node | undefined | typeof ABSENT {
  const [path, at] = search(root, utf8);
  if (at === undefined) return ABSENT;
  if (at.left === undefined || at.right === undefined) return rebuilt(path, at.left ?? at.right);
  const [rest, least] = withoutLeast(at.right);
  return rebuilt(path, balanced(least, at.left, rest));
}

/**
 * The balanced tree of `entries`, which are unique and in canonical key order: O(n), built bottom up (each range once
 * its halves are) without comparing a key. Its heights differ by at most one.
 */
export function built(entries: readonly Entry[]): Node | undefined {
  const done: (Node | undefined)[] = [];
  // [lo, hi, whether both halves are built]: a post-order walk with an explicit stack of O(log n) ranges
  const todo: [number, number, boolean][] = [[0, entries.length, false]];
  for (let top = todo.pop(); top !== undefined; top = todo.pop()) {
    const [lo, hi, halves] = top;
    if (lo >= hi) {
      done.push(undefined);
      continue;
    }
    const mid = lo + ((hi - lo) >>> 1);
    if (!halves) {
      todo.push([lo, hi, true], [mid + 1, hi, false], [lo, mid, false]);
      continue;
    }
    const right = done.pop();
    const left = done.pop();
    const e = entries[mid];
    if (e !== undefined) done.push(node(e, left, right));
  }
  return done.pop();
}

/** `take` of each node, in canonical key order: an in-order walk with an explicit stack of at most the height. O(n). */
function inOrder<T>(root: Node | undefined, take: (t: Node) => T): T[] {
  const out: T[] = [];
  const stack: Node[] = [];
  for (let t = root; t !== undefined || stack.length > 0; ) {
    if (t !== undefined) {
      stack.push(t);
      t = t.left;
      continue;
    }
    const top = stack.pop();
    if (top === undefined) break;
    out.push(take(top));
    t = top.right;
  }
  return out;
}

/**
 * The tree's entries in canonical key order, one at each call, then `undefined`: a walk that stops where its reader
 * does, with an explicit stack of at most the height. Each call is amortised O(1); `k` calls cost O(k + height).
 */
export function cursor(root: Node | undefined): () => Entry | undefined {
  const stack: Node[] = [];
  for (let t = root; t !== undefined; t = t.left) stack.push(t);
  return () => {
    const top = stack.pop();
    if (top === undefined) return undefined;
    for (let t = top.right; t !== undefined; t = t.left) stack.push(t);
    return top.entry;
  };
}

/** The tree's nodes in canonical key order. O(n). */
export const nodes = (root: Node | undefined): Node[] => inOrder(root, (t) => t);

/** The tree's entries in canonical key order. O(n). */
export const listed = (root: Node | undefined): Entry[] => inOrder(root, (t) => t.entry);

// ------------------------------------------------------------------------------------------------ code-point order

/** The length of the common prefix of `a` and `b`, which agree on their first `from` bytes. */
function common(a: Uint8Array, b: Uint8Array, from: number): number {
  const n = Math.min(a.length, b.length);
  let i = from;
  for (; i < n && a[i] === b[i]; i++);
  return i;
}

/** Entries in byte order of their keys' UTF-8, and each one's common prefix with the one before it (0 for the first). */
interface Run<E extends Entry> {
  readonly items: readonly E[];
  readonly lcp: readonly number[];
}

/**
 * Two runs merged into one, in byte order, by their common prefixes (Ng and Kakehi's LCP merge, "Merging String
 * Sequences by Longest Common Prefixes", IPSJ Digital Courier 4, 2008). With `o` the last entry output, `ha` and `hb`
 * are each head's common prefix with `o`. The head sharing more with `o` comes first, read without a byte: both follow
 * `o`, and the other differs from `o` earlier, upward. Only when they share as much is a byte read, from that common
 * length on, and the one not taken then shares exactly the bytes read with the new `o`. So a byte that matches raises
 * an entry's common prefix with the entry before it, which never falls from one merge to the next, and is read once per
 * entry over every merge; a byte that differs is read once per entry output.
 */
function merged<E extends Entry>(a: Run<E>, b: Run<E>): Run<E> {
  const items: E[] = [];
  const lcp: number[] = [];
  let [i, j, ha, hb] = [0, 0, 0, 0];
  for (let x = a.items[0], y = b.items[0]; x !== undefined && y !== undefined; ) {
    let first = ha > hb;
    const h = Math.max(ha, hb);
    if (ha === hb) {
      const l = common(x.utf8, y.utf8, h);
      // keys of different lengths, never equal: a prefix sorts first
      first = l === x.utf8.length || (l < y.utf8.length && (x.utf8[l] ?? 0) < (y.utf8[l] ?? 0));
      if (first) hb = l;
      else ha = l;
    }
    if (first) {
      items.push(x);
      lcp.push(h);
      x = a.items[++i];
      ha = a.lcp[i] ?? 0;
    } else {
      items.push(y);
      lcp.push(h);
      y = b.items[++j];
      hb = b.lcp[j] ?? 0;
    }
  }
  // the rest of one run follows: its head shares its h with the last entry output, each after it its own lcp
  const tail = (rest: Run<E>, from: number, h: number): Run<E> => {
    for (let k = from; k < rest.items.length; k++) {
      const e = rest.items[k];
      if (e === undefined) continue;
      items.push(e);
      lcp.push(k === from ? h : (rest.lcp[k] ?? 0));
    }
    return { items, lcp };
  };
  return i < a.items.length ? tail(a, i, ha) : tail(b, j, hb);
}

/**
 * `canonical`, which is in canonical key order, rearranged into code-point order of the keys: the order of their UTF-8
 * bytes alone (jlvm.md section 5, `keys`, `values` and `entries`; and the JSON rendering's). Keys of one UTF-8 length
 * are in that order already, so the runs of each length are merged, pairwise in rounds, by {@link merged}.
 *
 * Linear in the map's weight `W`, whatever the keys. With `n` keys of `S` UTF-8 bytes in all, in `k` runs, the rounds
 * output `n ceil(log2 k)` entries, each after at most one pair of bytes that differ; and every pair that matches raises
 * one entry's common prefix with its predecessor, which no round lowers, so each key's bytes match at most once over all
 * rounds (`S`). Finding the runs' own prefixes reads at most `S + n` pairs more. `n` distinct byte strings have
 * `n log2 n < 8 S + 2.45 n` (fewer than `2 * 256^L` strings are `L` bytes or shorter), and every entry adds at least
 * `2 +` its key's length to `W` (`W > S + 2 n`), so the pairs read are at most `2 S + n (log2 n + 2) < 10 W`, and so
 * are the entries moved. Keys of one length (the first and last are as long: the lengths only grow) are already in
 * code-point order, and `canonical` itself is the answer, in O(1).
 */
export function codePointOrder<E extends Entry>(canonical: readonly E[]): readonly E[] {
  const [first, last] = [canonical[0], canonical.at(-1)];
  if (first === undefined || last === undefined || first.utf8.length === last.utf8.length) return canonical;
  let runs: Run<E>[] = [];
  for (let i = 0; i < canonical.length; ) {
    const length = canonical[i]?.utf8.length ?? 0;
    const items: E[] = [];
    const lcp: number[] = [];
    for (let e = canonical[i]; e?.utf8.length === length; e = canonical[++i]) {
      const before = items.at(-1);
      lcp.push(before === undefined ? 0 : common(before.utf8, e.utf8, 0));
      items.push(e);
    }
    runs.push({ items, lcp });
  }
  if (runs.length <= 1) return canonical;
  for (; runs.length > 1; ) {
    const next: Run<E>[] = [];
    for (let r = 0; r < runs.length; r += 2) {
      const [a, b] = [runs[r], runs[r + 1]];
      if (a !== undefined) next.push(b === undefined ? a : merged(a, b));
    }
    runs = next;
  }
  return runs[0]?.items ?? canonical;
}
