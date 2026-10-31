/**
 * The binary Patricia Merkle trie (docs/specs/trie.md): key modes, node hashing, partial-trie proofs decoded from their
 * flat pre-order records, verification against a root, and what a verified proof answers (lookups, prefix and range
 * listings). Also the JLVM's `trie_verify`, `trie_prefix_verify` and `trie_range_verify` (jlvm.md section 6), and the
 * root of a trie built from its entries.
 *
 * Every walk uses an explicit stack, never recursion: a proof's depth is bounded only by the mode (up to 524280 bits).
 */
import { describeFailure, err, models, ok, outputSize, toHex, valueCommitment, type HashAlgorithm, type Result, type Value } from "@kd5ujc/dilf4s-core";

import * as B from "./bits.js";
import { bytesEqual, compareBytes, ConstantTag, copyBytes, concatBytes } from "./bytes.js";

/**
 * Why a trie operation failed, by Scala's `TrieError` names, plus `NotCovered`: the JLVM's "not covered" (a proof that
 * does not reach the queried key, or reaches a stub inside the queried prefix or range), which is an error, never false.
 */
export type TrieError =
  | { readonly kind: "InvalidKey"; readonly reason: string }
  | { readonly kind: "InvalidNode"; readonly reason: string }
  | { readonly kind: "InvalidProof"; readonly reason: string }
  | { readonly kind: "RootMismatch"; readonly expected: string; readonly computed: string }
  | { readonly kind: "Encoding"; readonly reason: string }
  | { readonly kind: "NotCovered"; readonly what: string };

const encoding = (reason: string): Result<never, TrieError> => err({ kind: "Encoding", reason });
const invalidProof = (reason: string): Result<never, TrieError> => err({ kind: "InvalidProof", reason });

// ------------------------------------------------------------------ key modes (section 1)

export const MAX_FIXED_BYTES = 1024;
export const MAX_VARIABLE_BYTES = 65535;

/** How keys are validated: `fixed(n)`, exactly `n` bytes, or `variable(max)`, at most `max` bytes. */
export class KeyMode {
  private constructor(
    readonly kind: "fixed" | "variable",
    /** The key length (fixed) or the longest key (variable), in bytes. */
    readonly bytes: number,
  ) {
    Object.freeze(this);
  }

  static fixed(bytes: number): Result<KeyMode, TrieError> {
    return Number.isInteger(bytes) && bytes >= 1 && bytes <= MAX_FIXED_BYTES
      ? ok(new KeyMode("fixed", bytes))
      : err({ kind: "InvalidKey", reason: `fixed key length must be 1..${MAX_FIXED_BYTES.toString()}` });
  }

  static variable(maxBytes: number): Result<KeyMode, TrieError> {
    return Number.isInteger(maxBytes) && maxBytes >= 0 && maxBytes <= MAX_VARIABLE_BYTES
      ? ok(new KeyMode("variable", maxBytes))
      : err({ kind: "InvalidKey", reason: `variable key limit must be 0..${MAX_VARIABLE_BYTES.toString()}` });
  }

  /** The longest possible path, in bits. */
  get maxBits(): number {
    return this.bytes * 8;
  }

  get allowsBranchValues(): boolean {
    return this.kind === "variable";
  }

  /** `undefined` if `key` is valid in this mode, else why not. */
  checkKey(key: Uint8Array): Result<undefined, TrieError> {
    if (this.kind === "fixed")
      return key.length === this.bytes ? ok(undefined) : err({ kind: "InvalidKey", reason: `fixed(${this.bytes.toString()}) mode needs ${this.bytes.toString()}-byte keys, got ${key.length.toString()}` });
    return key.length <= this.bytes ? ok(undefined) : err({ kind: "InvalidKey", reason: `variable(${this.bytes.toString()}) mode allows at most ${this.bytes.toString()} bytes, got ${key.length.toString()}` });
  }

  toString(): string {
    return `${this.kind}(${this.bytes.toString()})`;
  }
}

// ------------------------------------------------------------------ segments and hashing (sections 2, 3)

let segmentBitsOf!: (s: Segment) => B.Bits;

/** A node's segment: `length` bits, MSB-first in `bytes`, padded with zero bits to a whole byte. */
export class Segment {
  readonly #bits: B.Bits;
  private constructor(bits: B.Bits) {
    this.#bits = bits;
    Object.freeze(this);
  }

  /** The segment of the wire form `{n, b}`: `len(b) == ceil(n / 8)` and zero padding bits. */
  static of(n: bigint, b: Uint8Array): Result<Segment, TrieError> {
    const bits = segmentBits(n, b);
    return bits.ok ? ok(new Segment(bits.value)) : bits;
  }

  get length(): number {
    return this.#bits.length;
  }

  /** The padded bytes (a copy). */
  get bytes(): Uint8Array {
    return copyBytes(this.#bits.bytes);
  }

  static {
    segmentBitsOf = (s) => s.#bits;
  }
}

function segmentBits(n: bigint, b: Uint8Array): Result<B.Bits, TrieError> {
  const len = BigInt(b.length);
  if (n > len * 8n || len !== (n + 7n) / 8n) return encoding(`segment of ${n.toString()} bits in ${b.length.toString()} bytes`);
  const bits = Number(n);
  const rem = bits % 8;
  const last = b[b.length - 1] ?? 0;
  if (rem !== 0 && (last & (0xff >> rem)) !== 0) return encoding("segment padding bits must be zero");
  return ok({ bytes: copyBytes(b), length: bits });
}

export const VALUE_TAG = "dilf4s/trie/value/v1";
export const LEAF_TAG = "dilf4s/trie/leaf/v1";
export const BRANCH_TAG = "dilf4s/trie/branch/v1";
export const EMPTY_TAG = "dilf4s/trie/empty/v1";

// canonical CBOR of the node commitments: maps with fixed one-letter keys, written directly
function cborHead(major: number, n: number): Uint8Array {
  const m = major << 5;
  if (n < 24) return Uint8Array.of(m | n);
  if (n < 0x100) return Uint8Array.of(m | 24, n);
  if (n < 0x10000) return Uint8Array.of(m | 25, n >> 8, n & 0xff);
  if (n < 0x100000000) return Uint8Array.of(m | 26, (n >>> 24) & 0xff, (n >>> 16) & 0xff, (n >>> 8) & 0xff, n & 0xff);
  const hi = Math.floor(n / 0x100000000);
  return Uint8Array.of(m | 27, (hi >>> 24) & 0xff, (hi >>> 16) & 0xff, (hi >>> 8) & 0xff, hi & 0xff, (n >>> 24) & 0xff, (n >>> 16) & 0xff, (n >>> 8) & 0xff, n & 0xff);
}

const cborKey = (k: string): Uint8Array => Uint8Array.of(0x61, k.charCodeAt(0));
const cborBytes = (b: Uint8Array): Uint8Array => concatBytes([cborHead(2, b.length), b]);

/** `{"b": bytes, "n": bits}`: canonical key order is "b" then "n". */
function cborSegment(s: B.Bits): Uint8Array {
  return concatBytes([Uint8Array.of(0xa2), cborKey("b"), cborBytes(s.bytes.subarray(0, (s.length + 7) >> 3)), cborKey("n"), cborHead(0, s.length)]);
}

/** Hashing and node commitments over one hash algorithm (trie.md section 3). */
export class TrieHashing {
  /** The digest size in bytes. */
  readonly size: number;
  readonly #value: ConstantTag;
  readonly #leaf: ConstantTag;
  readonly #branch: ConstantTag;
  readonly #empty: Uint8Array;
  private constructor(readonly algorithm: HashAlgorithm) {
    this.size = outputSize(algorithm);
    this.#value = new ConstantTag(algorithm, VALUE_TAG);
    this.#leaf = new ConstantTag(algorithm, LEAF_TAG);
    this.#branch = new ConstantTag(algorithm, BRANCH_TAG);
    this.#empty = new ConstantTag(algorithm, EMPTY_TAG).hash(new Uint8Array(0));
    Object.freeze(this);
  }

  static of(algorithm: HashAlgorithm): TrieHashing {
    return new TrieHashing(algorithm);
  }

  /** The root of the empty trie. */
  get empty(): Uint8Array {
    return copyBytes(this.#empty);
  }

  /** The hash a trie commits for a value: `valueHash(commit(v))`, as a JLVM program claims it. */
  valueHash(value: Value): Uint8Array {
    return this.#value.hash(valueCommitment(value));
  }

  /** A leaf's hash. */
  leafHash(segment: Segment, valueHash: Uint8Array): Uint8Array {
    return this.leafBits(segmentBitsOf(segment), valueHash);
  }

  /** A branch's hash; absent fields are omitted. */
  branchHash(segment: Segment, valueHash: Uint8Array | undefined, left: Uint8Array | undefined, right: Uint8Array | undefined): Uint8Array {
    return this.branchBits(segmentBitsOf(segment), valueHash, left, right);
  }

  /** @internal */
  leafBits(segment: B.Bits, valueHash: Uint8Array): Uint8Array {
    // {"s": segment, "v": valueHash}
    return this.#leaf.hash(concatBytes([Uint8Array.of(0xa2), cborKey("s"), cborSegment(segment), cborKey("v"), cborBytes(valueHash)]));
  }

  /** @internal */
  branchBits(segment: B.Bits, valueHash: Uint8Array | undefined, left: Uint8Array | undefined, right: Uint8Array | undefined): Uint8Array {
    // canonical key order: "l", "r", "s", "v"
    const parts: Uint8Array[] = [];
    if (left !== undefined) parts.push(cborKey("l"), cborBytes(left));
    if (right !== undefined) parts.push(cborKey("r"), cborBytes(right));
    parts.push(cborKey("s"), cborSegment(segment));
    if (valueHash !== undefined) parts.push(cborKey("v"), cborBytes(valueHash));
    const count = 1 + (left === undefined ? 0 : 1) + (right === undefined ? 0 : 1) + (valueHash === undefined ? 0 : 1);
    return this.#branch.hash(concatBytes([cborHead(5, count), ...parts]));
  }
}

// ------------------------------------------------------------------ partial tries (section 4)

/** A node of a partial trie. */
type ProofNode =
  | { readonly kind: "stub"; readonly hash: Uint8Array }
  | { readonly kind: "leaf"; readonly segment: B.Bits; readonly value: Uint8Array }
  | { readonly kind: "branch"; readonly segment: B.Bits; readonly value: Uint8Array | undefined; readonly left: ProofNode | undefined; readonly right: ProofNode | undefined };

type Branch = Extract<ProofNode, { kind: "branch" }>;

const validBranch = (value: Uint8Array | undefined, children: number): boolean => children === 2 || (children === 1 && value !== undefined);

/** A branch record waiting for its children while decoding. */
interface Open {
  readonly segment: B.Bits;
  readonly value: Uint8Array | undefined;
  readonly wantLeft: boolean;
  readonly wantRight: boolean;
  readonly left: ProofNode | undefined;
}

let topOf!: (p: TrieProof) => ProofNode | undefined;

/** A decoded, well-formed partial trie (not yet checked against a root). An empty proof is the proof of the empty trie. */
export class TrieProof {
  readonly #top: ProofNode | undefined;
  private constructor(top: ProofNode | undefined) {
    this.#top = top;
    Object.freeze(this);
  }

  /** Whether this is the proof of the empty trie. */
  get isEmpty(): boolean {
    return this.#top === undefined;
  }

  static {
    topOf = (p) => p.#top;
  }

  /** A proof from its canonical value: the flat pre-order array of `trie-proof` records (trie.md section 5). */
  static decode(hashing: TrieHashing, value: Value): Result<TrieProof, TrieError> {
    const w = models.trie.TrieProof.decode(value);
    return w.ok ? TrieProof.fromRecords(hashing, w.value) : encoding(describeFailure(w.error));
  }

  /**
   * A proof from its typed records, rebuilt with an explicit stack: trailing records, missing children, a mask
   * inconsistent with the branch invariant, bad segments and digests of the wrong size are `Encoding` errors.
   */
  static fromRecords(hashing: TrieHashing, records: models.trie.TrieProof): Result<TrieProof, TrieError> {
    if (records.length === 0) return ok(new TrieProof(undefined));
    const stack: Open[] = [];
    for (const [i, r] of records.entries()) {
      const rec = record(hashing, r);
      if (!rec.ok) return rec;
      if ("wantLeft" in rec.value) {
        stack.push(rec.value);
        continue;
      }
      // give the finished node to the innermost open branch, closing branches that become complete
      let node: ProofNode = rec.value;
      for (;;) {
        const o = stack.pop();
        if (o === undefined) return i === records.length - 1 ? ok(new TrieProof(node)) : encoding("trailing proof records");
        if (o.wantLeft && o.left === undefined) {
          if (o.wantRight) {
            stack.push({ ...o, left: node });
            break;
          }
          node = { kind: "branch", segment: o.segment, value: o.value, left: node, right: undefined };
        } else node = { kind: "branch", segment: o.segment, value: o.value, left: o.left, right: node };
      }
    }
    return encoding("proof ended with unfinished branches");
  }

  /** The canonical value: pre-order records, each branch followed by its present children, left first. */
  encode(): Result<Value, TrieError> {
    const out: models.trie.TrieRecord[] = [];
    const todo: ProofNode[] = this.#top === undefined ? [] : [this.#top];
    for (let n = todo.pop(); n !== undefined; n = todo.pop()) {
      switch (n.kind) {
        case "stub":
          out.push({ h: n.hash });
          break;
        case "leaf":
          out.push({ s: wireSegment(n.segment), v: n.value });
          break;
        case "branch": {
          const c = n.left !== undefined && n.right !== undefined ? "both" : n.left !== undefined ? "left" : "right";
          out.push(n.value === undefined ? { s: wireSegment(n.segment), c } : { s: wireSegment(n.segment), v: n.value, c });
          if (n.right !== undefined) todo.push(n.right);
          if (n.left !== undefined) todo.push(n.left);
        }
      }
    }
    const v = models.trie.TrieProof.encode(out);
    return v.ok ? v : encoding(v.error.kind);
  }
}

const wireSegment = (s: B.Bits): models.trie.TrieSegment => ({ n: BigInt(s.length), b: s.bytes.slice(0, (s.length + 7) >> 3) });

function digestOf(hashing: TrieHashing, b: Uint8Array): Result<Uint8Array, TrieError> {
  return b.length === hashing.size ? ok(copyBytes(b)) : encoding(`a digest is ${hashing.size.toString()} bytes, got ${b.length.toString()}`);
}

/** One typed record: a finished node, or an open branch awaiting children. */
function record(hashing: TrieHashing, r: models.trie.TrieRecord): Result<ProofNode | Open, TrieError> {
  if ("h" in r) {
    const h = digestOf(hashing, r.h);
    return h.ok ? ok({ kind: "stub", hash: h.value }) : h;
  }
  const segment = segmentBits(r.s.n, r.s.b);
  if (!segment.ok) return segment;
  if (!("c" in r)) {
    const v = digestOf(hashing, r.v);
    return v.ok ? ok({ kind: "leaf", segment: segment.value, value: v.value }) : v;
  }
  const value = r.v === undefined ? ok(undefined) : digestOf(hashing, r.v);
  if (!value.ok) return value;
  const [wantLeft, wantRight] = r.c === "left" ? [true, false] : r.c === "right" ? [false, true] : [true, true];
  if (!validBranch(value.value, (wantLeft ? 1 : 0) + (wantRight ? 1 : 0))) return encoding("branch needs two children, or one child and a value");
  return ok({ segment: segment.value, value: value.value, wantLeft, wantRight, left: undefined });
}

// ------------------------------------------------------------------ verification

/** A pending step of the post-order hash: visit a node at a bit depth, or combine a branch's children. */
type HashTask = { readonly visit: ProofNode; readonly depth: number } | { readonly combine: Branch };

/** The partial trie's hash, enforcing the branch invariant, the mode, and the key-length bound (section 4, check 2). */
function hashProof(hashing: TrieHashing, mode: KeyMode, top: ProofNode): Result<Uint8Array, TrieError> {
  const tasks: HashTask[] = [{ visit: top, depth: 0 }];
  const done: Uint8Array[] = [];
  const pop = (present: boolean): Uint8Array | undefined => (present ? done.pop() : undefined);
  for (let t = tasks.pop(); t !== undefined; t = tasks.pop()) {
    if ("combine" in t) {
      const b = t.combine;
      const right = pop(b.right !== undefined);
      const left = pop(b.left !== undefined);
      done.push(hashing.branchBits(b.segment, b.value, left, right));
      continue;
    }
    const node = t.visit;
    switch (node.kind) {
      case "stub":
        done.push(node.hash);
        break;
      case "leaf": {
        const bits = t.depth + node.segment.length;
        const lengthOk = mode.kind === "fixed" ? bits === mode.maxBits : bits <= mode.maxBits && bits % 8 === 0;
        if (!lengthOk) return invalidProof(`leaf ends at bit ${bits.toString()}, invalid for ${mode.toString()}`);
        done.push(hashing.leafBits(node.segment, node.value));
        break;
      }
      case "branch": {
        const here = t.depth + node.segment.length;
        const children = (node.left === undefined ? 0 : 1) + (node.right === undefined ? 0 : 1);
        if (!validBranch(node.value, children)) return invalidProof("branch needs two children, or one child and a value");
        if (node.value !== undefined && (!mode.allowsBranchValues || here % 8 !== 0 || here > mode.maxBits))
          return invalidProof(`branch value at bit ${here.toString()} is invalid for ${mode.toString()}`);
        if (here + 1 > mode.maxBits) return invalidProof(`branch at bit ${here.toString()} exceeds ${mode.toString()}`);
        tasks.push({ combine: node });
        if (node.right !== undefined) tasks.push({ visit: node.right, depth: here + 1 });
        if (node.left !== undefined) tasks.push({ visit: node.left, depth: here + 1 });
      }
    }
  }
  const root = done.pop();
  return root === undefined ? invalidProof("no root") : ok(root);
}

/** {@link verify} with a root mismatch as the error `RootMismatch` (as Scala's `TrieProofs.verify`), for composed checks. */
export function check(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof, TrieError> {
  const top = topOf(proof);
  const computed = top === undefined ? ok(hashing.empty) : hashProof(hashing, mode, top);
  if (!computed.ok) return computed;
  return bytesEqual(computed.value, root) ? ok(createVerified(mode, top)) : err({ kind: "RootMismatch", expected: toHex(root), computed: toHex(computed.value) });
}

/**
 * Checks a proof against a trusted root (section 4): the verified proof; `null` when it is a well-formed proof of
 * another trie (a root mismatch, the JLVM's `false`); an error when it breaks the structure or the mode.
 */
export function verify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof | null, TrieError> {
  const r = check(hashing, mode, root, proof);
  return !r.ok && r.error.kind === "RootMismatch" ? ok(null) : r;
}

/** What a lookup proves: the key's value hash, its absence, or nothing (the proof does not reach it). */
export type Lookup = { readonly kind: "present"; readonly valueHash: Uint8Array } | { readonly kind: "absent" } | { readonly kind: "notCovered" };

/** An entry of a listing: a key and its value hash. */
export interface TrieEntry {
  readonly key: Uint8Array;
  readonly valueHash: Uint8Array;
}

/** A complete listing in key order, or nothing (a stub lies inside the query). */
export type Listing = { readonly kind: "listed"; readonly entries: readonly TrieEntry[] } | { readonly kind: "notCovered" };

const ABSENT: Lookup = { kind: "absent" };
const NOT_COVERED: Lookup & Listing = { kind: "notCovered" };
const listed = (entries: readonly TrieEntry[]): Listing => ({ kind: "listed", entries });
const entryOf = (key: B.Bits, valueHash: Uint8Array): TrieEntry => ({ key: B.toBytes(key), valueHash: copyBytes(valueHash) });

let createVerified!: (mode: KeyMode, top: ProofNode | undefined) => VerifiedTrieProof;

/** What a partial trie proves once its root has been checked: only {@link verify} makes one. */
export class VerifiedTrieProof {
  readonly #top: ProofNode | undefined;
  private constructor(
    readonly mode: KeyMode,
    top: ProofNode | undefined,
  ) {
    this.#top = top;
    Object.freeze(this);
  }

  static {
    createVerified = (mode, top) => new VerifiedTrieProof(mode, top);
  }

  /** The value hash at `key`, its absence, or not covered; a key invalid for the mode is `InvalidKey`. */
  lookup(key: Uint8Array): Result<Lookup, TrieError> {
    const valid = this.mode.checkKey(key);
    return valid.ok ? ok(this.lookupPath(B.bitsOf(key))) : valid;
  }

  private lookupPath(path: B.Bits): Lookup {
    let node = this.#top;
    let at = 0;
    while (node !== undefined) {
      switch (node.kind) {
        case "stub":
          return NOT_COVERED;
        case "leaf":
          return node.segment.length === path.length - at && B.startsWithAt(path, at, node.segment) ? { kind: "present", valueHash: copyBytes(node.value) } : ABSENT;
        case "branch": {
          if (!B.startsWithAt(path, at, node.segment)) return ABSENT;
          at += node.segment.length;
          if (at === path.length) return node.value === undefined ? ABSENT : { kind: "present", valueHash: copyBytes(node.value) };
          const next: ProofNode | undefined = B.bit(path, at) ? node.right : node.left;
          if (next === undefined) return ABSENT;
          node = next;
          at += 1;
        }
      }
    }
    return ABSENT;
  }

  /** Every entry whose key starts with `prefix`, in key order, if the proof covers the whole subtree. */
  prefix(prefix: Uint8Array): Listing {
    const p = B.bitsOf(prefix);
    // walk to the node whose subtree holds exactly the keys starting with the prefix
    let node = this.#top;
    let consumed: B.Bits = B.EMPTY_BITS;
    let at = 0;
    for (;;) {
      if (node === undefined) return listed([]);
      if (node.kind === "stub") return NOT_COVERED;
      const remaining = B.drop(p, at);
      if (B.startsWith(node.segment, remaining)) return gather(node, consumed);
      if (node.kind === "leaf" || !B.startsWith(remaining, node.segment)) return listed([]);
      at += node.segment.length;
      const b = B.bit(p, at);
      consumed = B.append(B.concat([consumed, node.segment]), b);
      node = b ? node.right : node.left;
      at += 1;
    }
  }

  /**
   * Every entry with `from <= key < until` (bitwise order, a prefix first; no `until` is unbounded), if no stub meets
   * the range. A covered listing is complete, including an empty one.
   */
  range(from: Uint8Array, until?: Uint8Array): Listing {
    const lo = B.bitsOf(from);
    const hi = until === undefined ? undefined : B.bitsOf(until);
    if (this.#top === undefined || !B.intersects(B.EMPTY_BITS, lo, hi)) return listed([]);
    const out: TrieEntry[] = [];
    const todo: (readonly [ProofNode, B.Bits])[] = [[this.#top, B.EMPTY_BITS]];
    for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
      const [node, at] = t;
      if (node.kind === "stub") return NOT_COVERED;
      const here = B.concat([at, node.segment]);
      if (node.kind === "leaf") {
        if (B.within(here, lo, hi)) out.push(entryOf(here, node.value));
        continue;
      }
      if (node.value !== undefined && B.within(here, lo, hi)) out.push(entryOf(here, node.value));
      const right = B.append(here, true);
      const left = B.append(here, false);
      if (node.right !== undefined && B.intersects(right, lo, hi)) todo.push([node.right, right]);
      if (node.left !== undefined && B.intersects(left, lo, hi)) todo.push([node.left, left]);
    }
    return listed(out);
  }
}

/** Every entry of a subtree whose path so far is `at`, depth first in key order; a stub anywhere is not covered. */
function gather(top: ProofNode, at: B.Bits): Listing {
  const out: TrieEntry[] = [];
  const todo: (readonly [ProofNode, B.Bits])[] = [[top, at]];
  for (let t = todo.pop(); t !== undefined; t = todo.pop()) {
    const [node, path] = t;
    if (node.kind === "stub") return NOT_COVERED;
    const here = B.concat([path, node.segment]);
    if (node.kind === "leaf") {
      out.push(entryOf(here, node.value));
      continue;
    }
    if (node.value !== undefined) out.push(entryOf(here, node.value));
    if (node.right !== undefined) todo.push([node.right, B.append(here, true)]);
    if (node.left !== undefined) todo.push([node.left, B.append(here, false)]);
  }
  return listed(out);
}

// ------------------------------------------------------------------ JLVM operations (jlvm.md section 6)

/** A claimed entry: a key and its value (never null), committed by `commit(v)`. */
export type ClaimedEntry = readonly [Uint8Array, Value];

function verified(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: Value): Result<VerifiedTrieProof | null, TrieError> {
  const r = digestOf(hashing, root);
  if (!r.ok) return r;
  const p = TrieProof.decode(hashing, proof);
  return p.ok ? verify(hashing, mode, r.value, p.value) : p;
}

const isAbsence = (v: Value | null): boolean => v === null || v.kind === "null";

/**
 * `trie_verify [mode, root, key, value, proof]`: whether `key` holds `value` (a null value claims absence). A
 * well-formed proof of another trie is `false`; malformed input, and a proof that does not reach the key, are errors.
 */
export function trieVerify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, key: Uint8Array, value: Value | null, proof: Value): Result<boolean, TrieError> {
  const k = mode.checkKey(key);
  if (!k.ok) return k;
  const v = verified(hashing, mode, root, proof);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  const l = v.value.lookup(key);
  if (!l.ok) return l;
  switch (l.value.kind) {
    case "present":
      return ok(value !== null && !isAbsence(value) && bytesEqual(hashing.valueHash(value), l.value.valueHash));
    case "absent":
      return ok(isAbsence(value));
    case "notCovered":
      return err({ kind: "NotCovered", what: "the proof does not reach this key" });
  }
}

function claimedEntries(hashing: TrieHashing, mode: KeyMode, entries: readonly ClaimedEntry[]): Result<readonly TrieEntry[], TrieError> {
  const out: TrieEntry[] = [];
  for (const [key, value] of entries) {
    if (value.kind === "null") return encoding("an entry is [key, value], with a value");
    const k = mode.checkKey(key);
    if (!k.ok) return k;
    out.push({ key, valueHash: hashing.valueHash(value) });
  }
  return ok(out);
}

function sameListing(claimed: readonly TrieEntry[], proven: Listing): Result<boolean, TrieError> {
  if (proven.kind === "notCovered") return err({ kind: "NotCovered", what: "the proof does not cover the whole query" });
  const e = proven.entries;
  return ok(e.length === claimed.length && e.every((x, i) => bytesEqual(x.key, claimed[i]?.key ?? new Uint8Array(0)) && bytesEqual(x.valueHash, claimed[i]?.valueHash ?? new Uint8Array(0))));
}

/** `trie_prefix_verify [mode, root, prefix, entries, proof]`: whether `entries`, in key order, are exactly the trie's under `prefix`. */
export function triePrefixVerify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, prefix: Uint8Array, entries: readonly ClaimedEntry[], proof: Value): Result<boolean, TrieError> {
  const claimed = claimedEntries(hashing, mode, entries);
  if (!claimed.ok) return claimed;
  const v = verified(hashing, mode, root, proof);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  return sameListing(claimed.value, v.value.prefix(prefix));
}

/** `trie_range_verify [mode, root, from, until, entries, proof]`: `until` null is unbounded. */
export function trieRangeVerify(
  hashing: TrieHashing,
  mode: KeyMode,
  root: Uint8Array,
  from: Uint8Array,
  until: Uint8Array | null,
  entries: readonly ClaimedEntry[],
  proof: Value,
): Result<boolean, TrieError> {
  const claimed = claimedEntries(hashing, mode, entries);
  if (!claimed.ok) return claimed;
  const v = verified(hashing, mode, root, proof);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  return sameListing(claimed.value, v.value.range(from, until ?? undefined));
}

// ------------------------------------------------------------------ building

type BuildTask = { readonly lo: number; readonly hi: number; readonly offset: number } | { readonly segment: B.Bits; readonly value: Uint8Array | undefined; readonly left: boolean; readonly right: boolean };

/**
 * The root of the trie holding `entries` (later duplicates win): the same root as inserting them in any order, since
 * the shape is a function of the key set. Built bottom-up over the sorted keys with an explicit stack.
 */
export function trieRoot(hashing: TrieHashing, mode: KeyMode, entries: Iterable<readonly [Uint8Array, Uint8Array]>): Result<Uint8Array, TrieError> {
  const given: (readonly [Uint8Array, Uint8Array])[] = [];
  for (const [key, valueHash] of entries) {
    const k = mode.checkKey(key);
    if (!k.ok) return k;
    const v = digestOf(hashing, valueHash);
    if (!v.ok) return v;
    given.push([copyBytes(key), v.value]);
  }
  // for byte strings, bitwise order with a prefix first is unsigned lexicographic byte order; the sort is stable, so
  // the last of a run of equal keys is the latest
  given.sort((a, b) => compareBytes(a[0], b[0]));
  const sorted = given.filter((e, i) => {
    const next = given[i + 1];
    return next === undefined || !bytesEqual(next[0], e[0]);
  });
  if (sorted.length === 0) return ok(hashing.empty);
  const keys = sorted.map(([k]) => B.bitsOf(k));
  const values = sorted.map(([, v]) => v);
  const tasks: BuildTask[] = [{ lo: 0, hi: sorted.length, offset: 0 }];
  const done: Uint8Array[] = [];
  const keyAt = (i: number): B.Bits => keys[i] ?? B.EMPTY_BITS;
  for (let t = tasks.pop(); t !== undefined; t = tasks.pop()) {
    if ("segment" in t) {
      const right = t.right ? done.pop() : undefined;
      const left = t.left ? done.pop() : undefined;
      done.push(hashing.branchBits(t.segment, t.value, left, right));
      continue;
    }
    const first = keyAt(t.lo);
    if (t.hi - t.lo === 1) {
      done.push(hashing.leafBits(B.drop(first, t.offset), values[t.lo] ?? new Uint8Array(0)));
      continue;
    }
    // sorted keys share what the first and last share; a key ending there sorts first, then bit 0, then bit 1
    const pos = t.offset + B.commonPrefixFrom(first, t.offset, keyAt(t.hi - 1), t.offset);
    const here = first.length === pos;
    const restStart = here ? t.lo + 1 : t.lo;
    let split = restStart;
    while (split < t.hi && !B.bit(keyAt(split), pos)) split++;
    const [left, right] = [split > restStart, t.hi > split];
    tasks.push({ segment: B.slice(first, t.offset, pos), value: here ? values[t.lo] : undefined, left, right });
    if (right) tasks.push({ lo: split, hi: t.hi, offset: pos + 1 });
    if (left) tasks.push({ lo: restStart, hi: split, offset: pos + 1 });
  }
  const root = done.pop();
  return root === undefined ? err({ kind: "InvalidNode", reason: "no root" }) : ok(root);
}
