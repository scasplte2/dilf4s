/**
 * A replica's trie: an in-memory binary Patricia trie (docs/specs/trie.md) that keeps a payload with every value hash
 * and caches every node's hash, so it is both the replica's ordered map and its root.
 *
 * It is persistent: an update copies the nodes on the changed key's path and shares every other subtree, so it hashes
 * at most `depth + 2` nodes (the path, a lowered or merged node, and a new leaf), however many entries the trie holds,
 * and the trie it was applied to is unchanged. Keys are only ever walked bit by bit: no `Map` or `Set` sees one, so
 * keys chosen to collide in a hash table cost nothing extra. Every walk uses an explicit stack.
 */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";

import * as B from "./bits.js";
import { compareBytes, copyBytes } from "./bytes.js";
import type { KeyMode, TrieError, TrieHashing } from "./trie.js";

/** A value: the caller's payload and the value hash the trie commits. */
interface Slot<P> {
  readonly payload: P;
  readonly hash: Uint8Array;
}

/** A leaf has a value and no children; a branch keeps the invariant (two children, or one child and a value). */
interface Node<P> {
  readonly segment: B.Bits;
  readonly value: Slot<P> | undefined;
  readonly left: Node<P> | undefined;
  readonly right: Node<P> | undefined;
  readonly hash: Uint8Array;
}

/** A node with its hash; `value` must be present when there are no children. */
function node<P>(h: TrieHashing, segment: B.Bits, value: Slot<P> | undefined, left: Node<P> | undefined, right: Node<P> | undefined): Node<P> {
  const hash = value !== undefined && left === undefined && right === undefined ? h.leafBits(segment, value.hash) : h.branchBits(segment, value?.hash, left?.hash, right?.hash);
  return { segment, value, left, right, hash };
}

const child = <P>(n: Node<P>, bit: boolean): Node<P> | undefined => (bit ? n.right : n.left);

/**
 * The node for these fields under the branch invariant: nothing for no value and no children, a leaf for a value alone,
 * and a lone child merged into its parent's segment (one more hash: the child's own children are kept).
 */
function normalize<P>(h: TrieHashing, segment: B.Bits, value: Slot<P> | undefined, left: Node<P> | undefined, right: Node<P> | undefined): Node<P> | undefined {
  if (value === undefined && (left === undefined || right === undefined)) {
    const only = left ?? right;
    return only === undefined ? undefined : node(h, B.concat([B.append(segment, only === right), only.segment]), only.value, only.left, only.right);
  }
  return node(h, segment, value, left, right);
}

/** `n` with its `bit` child replaced. */
const withChild = <P>(h: TrieHashing, n: Node<P>, bit: boolean, c: Node<P> | undefined): Node<P> | undefined =>
  bit ? normalize(h, n.segment, n.value, n.left, c) : normalize(h, n.segment, n.value, c, n.right);

/** The ancestors re-hashed bottom-up around a replaced subtree, each normalised. */
function rebuild<P>(h: TrieHashing, frames: readonly (readonly [Node<P>, boolean])[], replacement: Node<P> | undefined): Node<P> | undefined {
  let top = replacement;
  for (let i = frames.length - 1; i >= 0; i--) {
    const f = frames[i];
    if (f !== undefined) top = withChild(h, f[0], f[1], top);
  }
  return top;
}

type BuildTask<P> = { readonly lo: number; readonly hi: number; readonly offset: number } | { readonly segment: B.Bits; readonly value: Slot<P> | undefined; readonly left: boolean; readonly right: boolean };

/** A persistent trie of payloads in one key mode. */
export class TrieMap<P> {
  private constructor(
    readonly hashing: TrieHashing,
    readonly mode: KeyMode,
    private readonly top: Node<P> | undefined,
    /** The number of entries. */
    readonly size: number,
  ) {
    Object.freeze(this);
  }

  static empty<P>(hashing: TrieHashing, mode: KeyMode): TrieMap<P> {
    return new TrieMap<P>(hashing, mode, undefined, 0);
  }

  /** The root (a copy). */
  get root(): Uint8Array {
    return this.top === undefined ? this.hashing.empty : copyBytes(this.top.hash);
  }

  /** The payload at `key`, if any: one walk down its path. */
  get(key: Uint8Array): P | undefined {
    if (!this.mode.checkKey(key).ok) return undefined;
    const path = B.bitsOf(key);
    let at = this.top;
    let offset = 0;
    while (at !== undefined) {
      if (!B.startsWithAt(path, offset, at.segment)) return undefined;
      const pos = offset + at.segment.length;
      if (pos === path.length) return at.value?.payload;
      at = child(at, B.bit(path, pos));
      offset = pos + 1;
    }
    return undefined;
  }

  /** The trie with `key` holding `payload` committed as `hash`; this trie is unchanged. */
  set(key: Uint8Array, payload: P, hash: Uint8Array): Result<TrieMap<P>, TrieError> {
    const checked = this.mode.checkKey(key);
    if (!checked.ok) return checked;
    const h = this.hashing;
    const path = B.bitsOf(copyBytes(key));
    const slot: Slot<P> = { payload, hash: copyBytes(hash) };
    const leaf = (from: number): Node<P> => node(h, B.slice(path, from, path.length), slot, undefined, undefined);
    const frames: (readonly [Node<P>, boolean])[] = [];
    let at = this.top;
    let offset = 0;
    for (;;) {
      if (at === undefined) return ok(this.next(rebuild(h, frames, leaf(offset)), true));
      const seg = at.segment.length;
      const common = B.commonPrefixFrom(path, offset, at.segment, 0);
      const pos = offset + common;
      if (common < seg) {
        // the key leaves this node's segment: split it, the node keeping the rest below the split
        const oldBit = B.bit(at.segment, common);
        const rest = node(h, B.slice(at.segment, common + 1, seg), at.value, at.left, at.right);
        const shared = B.slice(at.segment, 0, common);
        const split =
          pos === path.length
            ? node(h, shared, slot, oldBit ? undefined : rest, oldBit ? rest : undefined)
            : B.bit(path, pos)
              ? node(h, shared, undefined, rest, leaf(pos + 1))
              : node(h, shared, undefined, leaf(pos + 1), rest);
        return ok(this.next(rebuild(h, frames, split), true));
      }
      if (pos === path.length) return ok(this.next(rebuild(h, frames, node(h, at.segment, slot, at.left, at.right)), at.value === undefined));
      const bit = B.bit(path, pos);
      const c = child(at, bit);
      if (c === undefined) return ok(this.next(rebuild(h, frames, withChild(h, at, bit, leaf(pos + 1))), true));
      frames.push([at, bit]);
      at = c;
      offset = pos + 1;
    }
  }

  /** The trie without `key` (this trie itself if the key is absent); this trie is unchanged. */
  delete(key: Uint8Array): Result<TrieMap<P>, TrieError> {
    const checked = this.mode.checkKey(key);
    if (!checked.ok) return checked;
    const h = this.hashing;
    const path = B.bitsOf(key);
    const frames: (readonly [Node<P>, boolean])[] = [];
    let at = this.top;
    let offset = 0;
    while (at !== undefined) {
      if (!B.startsWithAt(path, offset, at.segment)) break;
      const pos = offset + at.segment.length;
      if (pos === path.length) {
        if (at.value === undefined) break;
        const top = rebuild(h, frames, normalize(h, at.segment, undefined, at.left, at.right));
        return ok(new TrieMap(h, this.mode, top, this.size - 1));
      }
      const bit = B.bit(path, pos);
      frames.push([at, bit]);
      at = child(at, bit);
      offset = pos + 1;
    }
    return ok(this);
  }

  private next(top: Node<P> | undefined, added: boolean): TrieMap<P> {
    return new TrieMap(this.hashing, this.mode, top, added ? this.size + 1 : this.size);
  }

  /** Every payload, in key order (a key before its extensions, then bit 0 before bit 1). */
  values(): P[] {
    const out: P[] = [];
    const todo: Node<P>[] = this.top === undefined ? [] : [this.top];
    for (let n = todo.pop(); n !== undefined; n = todo.pop()) {
      if (n.value !== undefined) out.push(n.value.payload);
      if (n.right !== undefined) todo.push(n.right);
      if (n.left !== undefined) todo.push(n.left);
    }
    return out;
  }

  /** The trie of `entries`, whose keys must strictly increase: each node hashed once, bottom-up with an explicit stack. */
  static fromSorted<P>(hashing: TrieHashing, mode: KeyMode, entries: Iterable<readonly [Uint8Array, P, Uint8Array]>): Result<TrieMap<P>, TrieError> {
    const keys: B.Bits[] = [];
    const slots: Slot<P>[] = [];
    let previous: Uint8Array | undefined;
    for (const [key, payload, hash] of entries) {
      const checked = mode.checkKey(key);
      if (!checked.ok) return checked;
      // for byte strings, bitwise order with a prefix first is unsigned lexicographic byte order
      if (previous !== undefined && compareBytes(previous, key) >= 0) return err({ kind: "InvalidKey", reason: "keys must strictly increase" });
      previous = copyBytes(key);
      keys.push(B.bitsOf(previous));
      slots.push({ payload, hash: copyBytes(hash) });
    }
    if (keys.length === 0) return ok(TrieMap.empty(hashing, mode));
    const keyAt = (i: number): B.Bits => keys[i] ?? B.EMPTY_BITS;
    const tasks: BuildTask<P>[] = [{ lo: 0, hi: keys.length, offset: 0 }];
    const done: Node<P>[] = [];
    for (let t = tasks.pop(); t !== undefined; t = tasks.pop()) {
      if ("segment" in t) {
        const right = t.right ? done.pop() : undefined;
        const left = t.left ? done.pop() : undefined;
        done.push(node(hashing, t.segment, t.value, left, right));
        continue;
      }
      const first = keyAt(t.lo);
      if (t.hi - t.lo === 1) {
        done.push(node(hashing, B.drop(first, t.offset), slots[t.lo], undefined, undefined));
        continue;
      }
      // sorted keys share what the first and last share; a key ending there sorts first, then bit 0, then bit 1
      const pos = t.offset + B.commonPrefixFrom(first, t.offset, keyAt(t.hi - 1), t.offset);
      const here = first.length === pos;
      const restStart = here ? t.lo + 1 : t.lo;
      let split = restStart;
      while (split < t.hi && !B.bit(keyAt(split), pos)) split++;
      const [left, right] = [split > restStart, t.hi > split];
      tasks.push({ segment: B.slice(first, t.offset, pos), value: here ? slots[t.lo] : undefined, left, right });
      if (right) tasks.push({ lo: split, hi: t.hi, offset: pos + 1 });
      if (left) tasks.push({ lo: restStart, hi: split, offset: pos + 1 });
    }
    const top = done.pop();
    return top === undefined ? err({ kind: "InvalidNode", reason: "no root" }) : ok(new TrieMap(hashing, mode, top, keys.length));
  }
}
