/**
 * Prefix and range walks over a verified partial trie (docs/specs/trie.md section 4), shared by the listings and by
 * `trie_prefix_verify` / `trie_range_verify`, which compare a claimed listing entry by entry as the walk reaches each
 * key, without building it. One path buffer serves the whole walk, and each frame carries where its path stands against
 * each bound (`B.Side`), so every node compares only the bits it adds: the work is linear in the proof's size plus the
 * bounds' and the claim's, however long the keys. Each walk counts its work in a `Work` tally for tests to bound.
 *
 * Internal: not exported from the package.
 */
import * as B from "./bits.js";
import { bytesEqual, copyBytes } from "./bytes.js";

/** A partial trie's node, as the walks read it (trie.ts's proof nodes). */
export type WalkNode =
  | { readonly kind: "stub" }
  | { readonly kind: "leaf"; readonly segment: B.Bits; readonly value: Uint8Array }
  | { readonly kind: "branch"; readonly segment: B.Bits; readonly value: Uint8Array | undefined; readonly left: WalkNode | undefined; readonly right: WalkNode | undefined };

/** An entry: a whole-byte key and its value hash. */
export interface KeyedHash {
  readonly key: Uint8Array;
  readonly valueHash: Uint8Array;
}

/** Work done, in steps: bits compared or written, and nodes visited. */
export interface Work {
  steps: number;
}

/** Key bounds `[from, until)`; `until` undefined is unbounded. */
interface Bounds {
  readonly from: B.Bits;
  readonly until: B.Bits | undefined;
}

const ALL: Bounds = { from: B.EMPTY_BITS, until: undefined };
const ZERO: B.Bits = { bytes: Uint8Array.of(0x00), length: 1 };
const ONE: B.Bits = { bytes: Uint8Array.of(0x80), length: 1 };

/** A path that grows and shrinks at its end: bits at and past `length` are zero. */
class Path {
  #bytes = new Uint8Array(64);
  #length = 0;

  get length(): number {
    return this.#length;
  }

  /** The path's bits, valid until it next changes. */
  get bits(): B.Bits {
    return { bytes: this.#bytes, length: this.#length };
  }

  push(s: B.Bits, work: Work): void {
    const need = (this.#length + s.length + 7) >> 3;
    if (need > this.#bytes.length) {
      const grown = new Uint8Array(Math.max(need, this.#bytes.length * 2));
      grown.set(this.#bytes);
      this.#bytes = grown;
    }
    for (let i = 0; i < s.length; i++) if (B.bit(s, i)) this.#bytes[(this.#length + i) >> 3] = (this.#bytes[(this.#length + i) >> 3] ?? 0) | (0x80 >> ((this.#length + i) & 7));
    this.#length += s.length;
    work.steps += s.length;
  }

  /** Back to `n` bits, clearing those cut so the padding stays zero. */
  truncate(n: number, work: Work): void {
    for (let i = n; i < this.#length; i++) this.#bytes[i >> 3] = (this.#bytes[i >> 3] ?? 0) & ~(0x80 >> (i & 7));
    work.steps += this.#length - n;
    this.#length = n;
  }
}
Object.freeze(Path);
Object.freeze(Path.prototype);

/** Where a path of `depth` bits stands against `bounds`, after `s`. */
function extendBoth(from: B.Side, until: B.Side, depth: number, s: B.Bits, bounds: Bounds, work: Work): readonly [B.Side, B.Side] {
  const f = B.extendSide(from, depth, s, bounds.from, work);
  const u = bounds.until === undefined ? until : B.extendSide(until, depth, s, bounds.until, work);
  return [f, u];
}

/** Whether the path is a key in the bounds: `from <= path < until`. */
const holds = (from: B.Side, until: B.Side, depth: number, bounds: Bounds): boolean =>
  !B.before(from, depth, bounds.from) && (bounds.until === undefined || B.before(until, depth, bounds.until));

/** Whether keys starting with the path can lie in the bounds: not wholly below `from`, and not at or above `until`. */
const meets = (from: B.Side, until: B.Side, depth: number, bounds: Bounds): boolean =>
  from !== "below" && (bounds.until === undefined || B.before(until, depth, bounds.until));

interface Frame {
  readonly node: WalkNode;
  /** The path's length above the node, the bit leading to it, and the sides at that length (before the bit). */
  readonly len: number;
  readonly bit: B.Bits | undefined;
  readonly from: B.Side;
  readonly until: B.Side;
}

/**
 * Visits every entry of the subtree at `start`, whose path is `at`, that `bounds` holds, in key order (a branch's own
 * entry, then its left subtree, then its right); `false` if a stub meets the bounds. `visit` sees the path as it
 * stands, which the walk then changes.
 */
function walk(start: WalkNode, at: B.Bits, bounds: Bounds, visit: (path: B.Bits, valueHash: Uint8Array) => void, work: Work): boolean {
  const path = new Path();
  path.push(at, work);
  const [f0, u0] = extendBoth("along", "along", 0, at, bounds, work);
  const todo: Frame[] = [{ node: start, len: at.length, bit: undefined, from: f0, until: u0 }];
  for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
    work.steps += 1;
    const { node, len } = t;
    path.truncate(len, work);
    let [from, until] = [t.from, t.until];
    if (t.bit !== undefined) {
      [from, until] = extendBoth(from, until, len, t.bit, bounds, work);
      path.push(t.bit, work);
    }
    if (node.kind === "stub") return false;
    [from, until] = extendBoth(from, until, path.length, node.segment, bounds, work);
    path.push(node.segment, work);
    const here = path.length;
    if (node.kind === "leaf") {
      if (holds(from, until, here, bounds)) visit(path.bits, node.value);
      continue;
    }
    for (const [child, bit] of [
      [node.right, ONE],
      [node.left, ZERO],
    ] as const) {
      if (child === undefined) continue;
      const [f, u] = extendBoth(from, until, here, bit, bounds, work);
      if (meets(f, u, here + 1, bounds)) todo.push({ node: child, len: here, bit, from, until });
    }
    if (node.value !== undefined && holds(from, until, here, bounds)) visit(path.bits, node.value);
  }
  return true;
}

/** The node whose subtree holds exactly the keys starting with `p`, and its path (`p`'s first bits); or why none. */
type Located = { readonly kind: "at"; readonly node: WalkNode; readonly path: B.Bits } | { readonly kind: "nowhere" } | { readonly kind: "stub" };

function locate(top: WalkNode | undefined, p: B.Bits, work: Work): Located {
  let node = top;
  let at = 0;
  for (;;) {
    if (node === undefined) return { kind: "nowhere" };
    if (node.kind === "stub") return { kind: "stub" };
    const remaining = p.length - at;
    const same = B.commonPrefixFrom(node.segment, 0, p, at);
    work.steps += same + 1;
    if (same === remaining) return { kind: "at", node, path: B.slice(p, 0, at) };
    if (node.kind === "leaf" || same < node.segment.length) return { kind: "nowhere" };
    at += node.segment.length;
    node = B.bit(p, at) ? node.right : node.left;
    at += 1;
  }
}

/** A walk that matches a claimed listing as it goes: `undefined` if a stub meets the query. */
function matching(run: (visit: (path: B.Bits, valueHash: Uint8Array) => void) => boolean, claimed: readonly KeyedHash[], work: Work): boolean | undefined {
  // the entries reached so far, and whether each matched the claim's entry in its place
  const m = { next: 0, equal: true };
  const covered = run((path, valueHash) => {
    const c = claimed.at(m.next);
    m.next += 1;
    work.steps += 1;
    if (!m.equal) return;
    m.equal = c !== undefined && c.key.length * 8 === path.length && bytesEqual(c.valueHash, valueHash) && bytesPrefixEqual(path.bytes, c.key, work);
  });
  return covered ? m.equal && m.next === claimed.length : undefined;
}

/** Whether `a` starts with the bytes of `key`, comparing them once each. */
function bytesPrefixEqual(a: Uint8Array, key: Uint8Array, work: Work): boolean {
  let i = 0;
  while (i < key.length && a[i] === key[i]) i++;
  work.steps += 8 * i + 1;
  return i === key.length;
}

const toEntry = (out: KeyedHash[]) => (path: B.Bits, valueHash: Uint8Array) => {
  out.push({ key: B.toBytes(path), valueHash: copyBytes(valueHash) });
};

/** Every entry under `prefix`, in key order; `undefined` if a stub lies on the way or under it. */
export function listPrefix(top: WalkNode | undefined, prefix: Uint8Array, work: Work = { steps: 0 }): KeyedHash[] | undefined {
  const l = locate(top, B.bitsOf(prefix), work);
  if (l.kind !== "at") return l.kind === "nowhere" ? [] : undefined;
  const out: KeyedHash[] = [];
  return walk(l.node, l.path, ALL, toEntry(out), work) ? out : undefined;
}

/** Every entry with `from <= key < until`, in key order; `undefined` if a stub meets the range. */
export function listRange(top: WalkNode | undefined, from: Uint8Array, until: Uint8Array | undefined, work: Work = { steps: 0 }): KeyedHash[] | undefined {
  const bounds: Bounds = { from: B.bitsOf(from), until: until === undefined ? undefined : B.bitsOf(until) };
  if (top === undefined || !meets("along", "along", 0, bounds)) return [];
  const out: KeyedHash[] = [];
  return walk(top, B.EMPTY_BITS, bounds, toEntry(out), work) ? out : undefined;
}

/** Whether `claimed`, in key order, is exactly the listing under `prefix`; `undefined` if a stub lies under it. */
export function listsPrefix(top: WalkNode | undefined, prefix: Uint8Array, claimed: readonly KeyedHash[], work: Work = { steps: 0 }): boolean | undefined {
  const l = locate(top, B.bitsOf(prefix), work);
  if (l.kind !== "at") return l.kind === "nowhere" ? claimed.length === 0 : undefined;
  return matching((visit) => walk(l.node, l.path, ALL, visit, work), claimed, work);
}

/** Whether `claimed`, in key order, is exactly the listing of `[from, until)`; `undefined` if a stub meets the range. */
export function listsRange(top: WalkNode | undefined, from: Uint8Array, until: Uint8Array | undefined, claimed: readonly KeyedHash[], work: Work = { steps: 0 }): boolean | undefined {
  const bounds: Bounds = { from: B.bitsOf(from), until: until === undefined ? undefined : B.bitsOf(until) };
  if (top === undefined || !meets("along", "along", 0, bounds)) return claimed.length === 0;
  return matching((visit) => walk(top, B.EMPTY_BITS, bounds, visit, work), claimed, work);
}
