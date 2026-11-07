/**
 * The binary Patricia Merkle trie (docs/specs/trie.md): key modes, node hashing, partial-trie proofs decoded from their
 * flat pre-order records, verification against a root, and what a verified proof answers (lookups, prefix and range
 * listings). Also the JLVM's `trie_verify`, `trie_prefix_verify` and `trie_range_verify` (jlvm.md section 6), and the
 * root of a trie built from its entries.
 *
 * Every walk uses an explicit stack, never recursion: a proof's depth is bounded only by the mode (up to 524280 bits).
 *
 * Every exported function and method checks its arguments once before it reads them, in O(1) each (an array: once per
 * item): an argument of the wrong type is core's `TypeMismatch` (check.ts). A caller's key or bound is read through
 * one plain view of its own memory, used for the check and the walk alike, so its own `length` is never believed.
 */
import { BYTES_TAG, describeFailure, err, isValue, models, ok, type Err, type HashAlgorithm, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { bytesLength, MAX_OUTPUT_SIZE, mint, outputSizeOf, plainBytes, readGuarded, takeBytes, takeBytesWithin, toHexOf } from "@kd5ujc/dilf4s-core/internal";

import * as B from "./bits.js";
import * as L from "./listing.js";
import { bytesEqual, compareBytes, ConstantTag, copyBytes, concatBytes } from "./bytes.js";
import { algorithmArg, bigintArg, bytesArg, expected, expectedAt, isObject, NOT_A_NUMBER, NOT_A_VALUE, NOT_BYTES, readArray, readIterable, readPair, TOKEN, valueArg, type Token } from "./check.js";
import { bitsOf, decodeInput, MAX_PROOF_WEIGHT } from "./input.js";
import { TRIE } from "./trie-internal.js";

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
  | { readonly kind: "NotCovered"; readonly what: string }
  /** Over `limit` in weight (`MAX_PROOF_WEIGHT`, or this SDK's commitment ceiling): claims, or a proof to emit. */
  | { readonly kind: "TooLarge"; readonly limit: number };

/** A trie refusal, or an argument of the wrong type: what the exported functions here may refuse with. */
type Refusal = TrieError | TypeMismatch;

/** Claims together within `MAX_PROOF_WEIGHT`, from their weights, before any is hashed. */
const claimsWithin = (values: readonly Value[]): Result<undefined, TrieError> =>
  values.reduce((t, v) => t + v.weight, 0n) <= MAX_PROOF_WEIGHT ? ok(undefined) : err({ kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) });

const encoding = (reason: string): Result<never, TrieError> => err({ kind: "Encoding", reason });
const invalidProof = (reason: string): Result<never, TrieError> => err({ kind: "InvalidProof", reason });

// ------------------------------------------------------------------ key modes (section 1)

export const MAX_FIXED_BYTES = models.limits.TRIE_MAX_FIXED_KEY_BYTES;
export const MAX_VARIABLE_BYTES = models.limits.TRIE_MAX_VARIABLE_KEY_BYTES;

const MODE = mint();
const SEGMENT = mint();
const HASHING = mint();
const PROOF = mint();
const VERIFIED = mint();
/** The refusal of a `VerifiedTrieProof` method called on what `verify` did not make (a constructor called from outside). */
const NOT_VERIFIED = expected("a VerifiedTrieProof");

let isMode!: (x: unknown) => x is KeyMode;

/** `x` if it is a {@link KeyMode} made here (a private-brand check: no Proxy trap or caller code runs). */
const modeArg = (x: unknown): Result<KeyMode, TypeMismatch> => (isMode(x) ? ok(x) : err(expected("a KeyMode")));

/** How keys are validated: `fixed(n)`, exactly `n` bytes, or `variable(max)`, at most `max` bytes. */
export class KeyMode {
  private constructor(
    token: Token,
    readonly kind: "fixed" | "variable",
    /** The key length (fixed) or the longest key (variable), in bytes. */
    readonly bytes: number,
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) [this.kind, this.bytes] = ["fixed", 0];
    if (token === TOKEN) MODE.stamp(this);
    Object.freeze(this);
  }

  static {
    isMode = (x): x is KeyMode => MODE.has(x);
    TRIE.defaultMode = new KeyMode(TOKEN, "fixed", 1);
  }

  /** Keys of exactly `bytes` bytes, `1..MAX_FIXED_BYTES` (`InvalidKey`); a length that is not a number is a `TypeMismatch`. */
  static fixed(bytes: number): Result<KeyMode, Refusal> {
    if (typeof (bytes as unknown) !== "number") return err(NOT_A_NUMBER);
    return Number.isInteger(bytes) && bytes >= 1 && bytes <= MAX_FIXED_BYTES
      ? ok(new KeyMode(TOKEN, "fixed", bytes))
      : err({ kind: "InvalidKey", reason: `fixed key length must be 1..${MAX_FIXED_BYTES.toString()}` });
  }

  /** Keys of at most `maxBytes` bytes, `0..MAX_VARIABLE_BYTES` (`InvalidKey`); a limit that is not a number is a `TypeMismatch`. */
  static variable(maxBytes: number): Result<KeyMode, Refusal> {
    if (typeof (maxBytes as unknown) !== "number") return err(NOT_A_NUMBER);
    return Number.isInteger(maxBytes) && maxBytes >= 0 && maxBytes <= MAX_VARIABLE_BYTES
      ? ok(new KeyMode(TOKEN, "variable", maxBytes))
      : err({ kind: "InvalidKey", reason: `variable key limit must be 0..${MAX_VARIABLE_BYTES.toString()}` });
  }

  /** The longest possible path, in bits. */
  get maxBits(): number {
    return this.bytes * 8;
  }

  get allowsBranchValues(): boolean {
    return this.kind === "variable";
  }

  /**
   * `undefined` if `key` is valid in this mode, else why not (`InvalidKey`), by the key's internal length; what is not a
   * `Uint8Array` is a `TypeMismatch`.
   */
  checkKey(key: Uint8Array): Result<undefined, Refusal> {
    if (!isMode(this)) return err(expected("a KeyMode"));
    const n = bytesLength(key);
    return n === undefined ? err(NOT_BYTES) : checkKeyLength(this, n);
  }

  toString(): string {
    return `${this.kind}(${this.bytes.toString()})`;
  }
}

const VALID: Result<undefined, never> = Object.freeze(ok(undefined));

/** {@link KeyMode.checkKey} of a key of `n` bytes (its internal length). */
function checkKeyLength(mode: KeyMode, n: number): Result<undefined, TrieError> {
  if (mode.kind === "fixed") return n === mode.bytes ? VALID : err({ kind: "InvalidKey", reason: `fixed(${mode.bytes.toString()}) mode needs ${mode.bytes.toString()}-byte keys, got ${n.toString()}` });
  return n <= mode.bytes ? VALID : err({ kind: "InvalidKey", reason: `variable(${mode.bytes.toString()}) mode allows at most ${mode.bytes.toString()} bytes, got ${n.toString()}` });
}

/** {@link KeyMode.checkKey} of a plain key. */
const checkKey = (mode: KeyMode, key: Uint8Array): Result<undefined, TrieError> => checkKeyLength(mode, key.length);

// ------------------------------------------------------------------ segments and hashing (sections 2, 3)

let segmentBitsOf!: (s: Segment) => B.Bits;
let isSegment!: (x: unknown) => x is Segment;

/** A node's segment: `length` bits, MSB-first in `bytes`, padded with zero bits to a whole byte. */
export class Segment {
  readonly #bits: B.Bits;
  private constructor(token: Token, bits: B.Bits) {
    this.#bits = bits;
    if (token === TOKEN) SEGMENT.stamp(this);
    Object.freeze(this);
  }

  /**
   * The segment of the wire form `{n, b}`: `len(b) == ceil(n / 8)` and zero padding bits (`Encoding`). A count that is
   * not a `bigint`, or bytes that are not a `Uint8Array`, are a `TypeMismatch`; the bytes are copied.
   */
  static of(n: bigint, b: Uint8Array): Result<Segment, Refusal> {
    const count = bigintArg(n);
    if (!count.ok) return count;
    // copied before its padding is checked: the bytes validated are the bytes the segment keeps
    const bytes = takeBytes(b);
    if (bytes === undefined) return err(NOT_BYTES);
    const bits = segmentBits(count.value, bytes);
    return bits.ok ? ok(new Segment(TOKEN, bits.value)) : bits;
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
    isSegment = (x): x is Segment => SEGMENT.has(x);
  }
}

function segmentBits(n: bigint, b: Uint8Array): Result<B.Bits, TrieError> {
  const len = BigInt(b.length);
  // bigint division truncates toward zero, so a negative length would pass the byte count: refuse it first
  if (n < 0n || n > len * 8n || len !== (n + 7n) / 8n) {
    // a count too wide for 64 bits is named by its size: its digits are superlinear and unpaid
    const count = bitsOf(n) <= 64 ? `${n.toString()} bits` : `a ${bitsOf(n).toString()}-bit count of bits`;
    return encoding(`segment of ${count} in ${b.length.toString()} bytes`);
  }
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

let isHashing!: (x: unknown) => x is TrieHashing;

/** `x` if it is a {@link TrieHashing} made here (a private-brand check: no Proxy trap or caller code runs). */
const hashingArg = (x: unknown): Result<TrieHashing, TypeMismatch> => (isHashing(x) ? ok(x) : err(expected("a TrieHashing")));

/** An optional digest: `undefined`, or a `Uint8Array` (a plain view). */
function optionalBytes(x: unknown): Result<Uint8Array | undefined, TypeMismatch> {
  return x === undefined ? ok(undefined) : bytesArg(x);
}

/** Hashing and node commitments over one hash algorithm (trie.md section 3). */
export class TrieHashing {
  /** The hash algorithm. */
  readonly algorithm: HashAlgorithm;
  /** The digest size in bytes. */
  readonly size: number;
  readonly #value: ConstantTag;
  readonly #leaf: ConstantTag;
  readonly #branch: ConstantTag;
  readonly #empty: Uint8Array;
  private constructor(token: Token, algorithm: HashAlgorithm) {
    // without the token, nothing of the caller's is read or kept: its algorithm is neither hashed with nor reported,
    // and every method refuses the unbranded receiver
    const a: HashAlgorithm = token === TOKEN ? algorithm : "SHA-256";
    this.algorithm = a;
    this.size = outputSizeOf(a);
    this.#value = new ConstantTag(a, VALUE_TAG);
    this.#leaf = new ConstantTag(a, LEAF_TAG);
    this.#branch = new ConstantTag(a, BRANCH_TAG);
    this.#empty = new ConstantTag(a, EMPTY_TAG).hash(new Uint8Array(0));
    if (token === TOKEN) HASHING.stamp(this);
    Object.freeze(this);
  }

  static {
    isHashing = (x): x is TrieHashing => HASHING.has(x);
    TRIE.valueHash = (h, value) => {
      const c = h.#value.commitment(value);
      if (c.ok) return c;
      // the value is checked first, so the commitment can only be too large
      return err(c.error.kind === "TooLarge" ? { kind: "TooLarge", limit: c.error.limit } : { kind: "Encoding", reason: c.error.reason });
    };
    TRIE.bytesHash = (h, b) => h.#value.hashParts([Uint8Array.of(BYTES_TAG), b]);
    TRIE.leafBits = (h, segment, valueHash) =>
      // {"s": segment, "v": valueHash}
      h.#leaf.hash(concatBytes([Uint8Array.of(0xa2), cborKey("s"), cborSegment(segment), cborKey("v"), cborBytes(valueHash)]));
    TRIE.branchBits = (h, segment, valueHash, left, right) => {
      // canonical key order: "l", "r", "s", "v"
      const parts: Uint8Array[] = [];
      if (left !== undefined) parts.push(cborKey("l"), cborBytes(left));
      if (right !== undefined) parts.push(cborKey("r"), cborBytes(right));
      parts.push(cborKey("s"), cborSegment(segment));
      if (valueHash !== undefined) parts.push(cborKey("v"), cborBytes(valueHash));
      const count = 1 + (left === undefined ? 0 : 1) + (right === undefined ? 0 : 1) + (valueHash === undefined ? 0 : 1);
      return h.#branch.hash(concatBytes([cborHead(5, count), ...parts]));
    };
    TRIE.rootText = rootText;
    TRIE.checkKey = checkKey;
    TRIE.defaultHashing = new TrieHashing(TOKEN, "SHA-256");
  }

  /** The hashing over `algorithm`; one that is not a `HashAlgorithm` (from a JavaScript caller) is a `TypeMismatch`. */
  static of(algorithm: HashAlgorithm): Result<TrieHashing, TypeMismatch> {
    const a = algorithmArg(algorithm);
    return a.ok ? ok(new TrieHashing(TOKEN, a.value)) : a;
  }

  /** The root of the empty trie (for a receiver not made here, the SHA-256 one its reported algorithm names). */
  get empty(): Uint8Array {
    return copyBytes(this.#empty);
  }

  /**
   * The hash a trie commits for a value: `valueHash(commit(v))`, as a JLVM program claims it. O(min(weight, the
   * ceiling)), as `valueCommitment`: a value whose commitment is over `cbor.MAX_ENCODED_BYTES` (2^31 - 9, this SDK's
   * ceiling; Scala and Rust hash without one) is `TooLarge`, and what is not a canonical value is a `TypeMismatch`. The
   * commitment is streamed into the hash, never built.
   */
  valueHash(value: Value): Result<Uint8Array, Refusal> {
    if (!HASHING.has(this)) return err(NOT_A_HASHING);
    const v = valueArg(value);
    return v.ok ? TRIE.valueHash(this, v.value) : v;
  }

  /**
   * The hash a trie commits for a byte string, `valueHash(commit(b))`: linear in `b`, which the caller holds. What is
   * not a `Uint8Array` is a `TypeMismatch`.
   */
  bytesHash(b: Uint8Array): Result<Uint8Array, TypeMismatch> {
    if (!HASHING.has(this)) return err(NOT_A_HASHING);
    const bytes = bytesArg(b);
    return bytes.ok ? ok(TRIE.bytesHash(this, bytes.value)) : bytes;
  }

  /** A leaf's hash; a segment not made by `Segment.of`, or a digest that is not a `Uint8Array`, is a `TypeMismatch`. */
  leafHash(segment: Segment, valueHash: Uint8Array): Result<Uint8Array, TypeMismatch> {
    if (!HASHING.has(this)) return err(NOT_A_HASHING);
    if (!isSegment(segment)) return err(expected("a Segment"));
    const v = bytesArg(valueHash);
    return v.ok ? ok(TRIE.leafBits(this, segmentBitsOf(segment), v.value)) : v;
  }

  /** A branch's hash; absent fields are omitted. Arguments are checked as {@link leafHash} checks them. */
  branchHash(segment: Segment, valueHash: Uint8Array | undefined, left: Uint8Array | undefined, right: Uint8Array | undefined): Result<Uint8Array, TypeMismatch> {
    if (!HASHING.has(this)) return err(NOT_A_HASHING);
    if (!isSegment(segment)) return err(expected("a Segment"));
    const v = optionalBytes(valueHash);
    if (!v.ok) return v;
    const l = optionalBytes(left);
    if (!l.ok) return l;
    const r = optionalBytes(right);
    return r.ok ? ok(TRIE.branchBits(this, segmentBitsOf(segment), v.value, l.value, r.value)) : r;
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
let isProof!: (x: unknown) => x is TrieProof;
let createProof!: (top: ProofNode | undefined) => TrieProof;


/** A decoded, well-formed partial trie (not yet checked against a root). An empty proof is the proof of the empty trie. */
export class TrieProof {
  readonly #top: ProofNode | undefined;
  private constructor(token: Token, top: ProofNode | undefined) {
    this.#top = token === TOKEN ? top : undefined;
    if (token === TOKEN) PROOF.stamp(this);
    Object.freeze(this);
  }

  /** Whether this is the proof of the empty trie. */
  get isEmpty(): boolean {
    return this.#top === undefined;
  }

  static {
    topOf = (p) => p.#top;
    isProof = (x): x is TrieProof => PROOF.has(x);
    createProof = (top) => new TrieProof(TOKEN, top);
  }

  /**
   * A proof from its canonical value: the flat pre-order array of `trie-proof` records (trie.md section 5). A value
   * heavier than `MAX_PROOF_WEIGHT` is `Encoding`, refused before it is decoded. What is not a `TrieHashing` or a
   * canonical value is a `TypeMismatch`.
   */
  static decode(hashing: TrieHashing, value: Value): Result<TrieProof, Refusal> {
    const h = hashingArg(hashing);
    if (!h.ok) return h;
    const v = valueArg(value);
    return v.ok ? decodeProof(h.value, v.value) : v;
  }

  /**
   * A proof from its typed records, rebuilt with an explicit stack: trailing records, missing children, a mask
   * inconsistent with the branch invariant, bad segments and digests of the wrong size are `Encoding` errors. The
   * records are the caller's: an array read by index, each record's fields read once (a field of the wrong type is a
   * `TypeMismatch`), its bytes read through plain views and copied as the proof keeps them.
   */
  static fromRecords(hashing: TrieHashing, records: models.trie.TrieProof): Result<TrieProof, Refusal> {
    const h = hashingArg(hashing);
    if (!h.ok) return h;
    const rs = readArray(records, "an array of trie records", recordArg);
    return rs.ok ? fromRecords(h.value, rs.value) : rs;
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
    if (!v.ok) return encoding(v.error.kind);
    // a proof every verifier would refuse is never emitted
    return v.value.weight <= MAX_PROOF_WEIGHT ? v : err({ kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) });
  }
}

function decodeProof(h: TrieHashing, value: Value): Result<TrieProof, TrieError> {
  const w = decodeInput(models.trie.TrieProof, value);
  return w.ok ? fromRecords(h, w.value) : encoding(describeFailure(w.error));
}

/** {@link TrieProof.fromRecords} of checked records: plain arrays, copied where the proof keeps them. */
function fromRecords(hashing: TrieHashing, records: models.trie.TrieProof): Result<TrieProof, TrieError> {
  if (records.length === 0) return ok(createProof(undefined));
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
      if (o === undefined) return i === records.length - 1 ? ok(createProof(node)) : encoding("trailing proof records");
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

/**
 * A caller's trie record, its fields read once: a stub `{h}`, a leaf `{s, v}` or a branch `{s, v?, c}`. Each byte field
 * is copied as it is read, before the next getter runs: caller code a later read runs cannot change or detach it.
 */
function recordArg(r: unknown, i: number): Result<models.trie.TrieRecord, TypeMismatch> {
  const bad = err(expectedAt("a trie record of its types", i));
  if (!isObject(r)) return bad;
  // each read guarded: a getter run by an earlier read may have revoked the record; no field's check takes REVOKED
  const h: unknown = readGuarded(r, "h");
  if (h !== undefined) {
    const hb = takeBytes(h);
    return hb === undefined ? bad : ok({ h: hb });
  }
  const s: unknown = readGuarded(r, "s");
  if (!isObject(s)) return bad;
  const n: unknown = readGuarded(s, "n");
  const b = takeBytes(readGuarded(s, "b"));
  if (typeof n !== "bigint" || b === undefined) return bad;
  const v: unknown = readGuarded(r, "v");
  const vb = v === undefined ? undefined : takeBytes(v);
  if (v !== undefined && vb === undefined) return bad;
  const c: unknown = readGuarded(r, "c");
  if (c === undefined) return vb === undefined ? bad : ok({ s: { n, b }, v: vb });
  if (c !== "left" && c !== "right" && c !== "both") return bad;
  return ok(vb === undefined ? { s: { n, b }, c } : { s: { n, b }, v: vb, c });
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
      done.push(TRIE.branchBits(hashing, b.segment, b.value, left, right));
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
        done.push(TRIE.leafBits(hashing, node.segment, node.value));
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

/**
 * A trusted root as a `RootMismatch` names it: its hex when it has the digest's `width`, otherwise its length, so a
 * caller's root of any length is never hex-encoded whole (docs/specs/conformance.md section 8).
 */
function rootText(root: Uint8Array, width: number): string {
  return root.length === width ? toHexOf(root) : `a root of ${root.length.toString()} bytes`;
}

const NOT_A_HASHING = expected("a TrieHashing");
const NOT_A_MODE = expected("a KeyMode");
const NOT_A_PROOF = expected("a TrieProof");

/**
 * {@link verify} with a root mismatch as the error `RootMismatch` (as Scala's `TrieProofs.verify`), for composed checks.
 * What is not a `TrieHashing`, a `KeyMode`, a `Uint8Array` root or a `TrieProof` is a `TypeMismatch`.
 */
export function check(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof, Refusal> {
  // each check allocates nothing but the root's plain view; a refusal is a shared constant
  if (!isHashing(hashing)) return err(NOT_A_HASHING);
  if (!isMode(mode)) return err(NOT_A_MODE);
  const r = plainBytes(root);
  if (r === undefined) return err(NOT_BYTES);
  return isProof(proof) ? checkOf(hashing, mode, r, proof) : err(NOT_A_PROOF);
}

function checkOf(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof, TrieError> {
  const top = topOf(proof);
  const computed = top === undefined ? ok(hashing.empty) : hashProof(hashing, mode, top);
  if (!computed.ok) return computed;
  return bytesEqual(computed.value, root) ? ok(createVerified(mode, top)) : err({ kind: "RootMismatch", expected: rootText(root, computed.value.length), computed: toHexOf(computed.value) });
}

/**
 * Checks a proof against a trusted root (section 4): the verified proof; `null` when it is a well-formed proof of
 * another trie (a root mismatch, the JLVM's `false`); an error when it breaks the structure or the mode. Arguments are
 * checked as {@link check} checks them.
 */
export function verify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: TrieProof): Result<VerifiedTrieProof | null, Refusal> {
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

// shared answers, frozen: one a caller changed would answer every later lookup
const ABSENT: Lookup = Object.freeze({ kind: "absent" });
const NOT_COVERED: Lookup & Listing = Object.freeze({ kind: "notCovered" });
const listed = (entries: readonly TrieEntry[]): Listing => ({ kind: "listed", entries });

let createVerified!: (mode: KeyMode, top: ProofNode | undefined) => VerifiedTrieProof;
/** A lookup of `key`, whose internal length is `n`, read by index only. */
let lookupOf!: (v: VerifiedTrieProof, key: Uint8Array, n: number) => Result<Lookup, TrieError>;
let verifiedTop!: (v: VerifiedTrieProof) => ProofNode | undefined;

/** An optional bound: `undefined`, or a `Uint8Array` (a plain view). */
const boundArg = optionalBytes;

/**
 * A query's prefix or bound, copied (`takeBytes`) before its claim is read: caller code a claim's getter (or a Proxy's
 * trap) runs cannot change the query the claim is compared with. A prefix or bound is a key's length at most.
 */
function queryArg(x: unknown): Result<Uint8Array, TypeMismatch> {
  const own = takeBytes(x);
  return own === undefined ? err(NOT_BYTES) : ok(own);
}

/** An optional query bound, `undefined` or copied as {@link queryArg} copies one. */
function optionalQueryArg(x: unknown): Result<Uint8Array | undefined, TypeMismatch> {
  return x === undefined ? ok(undefined) : queryArg(x);
}

/** A caller's claimed listing: an array of `{key, valueHash}` objects, each field read once, as plain views. */
const claimedArg = (claimed: unknown): Result<TrieEntry[], TypeMismatch> =>
  readArray(claimed, "an array of TrieEntry", (e, i) => {
    if (!isObject(e)) return err(expectedAt("a TrieEntry", i));
    // copied as read: caller code run by a later read (a getter, a Proxy's trap) cannot change an earlier entry
    const key = takeBytes(readGuarded(e, "key"));
    const valueHash = takeBytes(readGuarded(e, "valueHash"));
    return key === undefined || valueHash === undefined ? err(expectedAt("a TrieEntry of Uint8Arrays", i)) : ok({ key, valueHash });
  });

/** What a partial trie proves once its root has been checked: only {@link verify} makes one. */
export class VerifiedTrieProof {
  readonly #top: ProofNode | undefined;
  private constructor(
    token: Token,
    readonly mode: KeyMode,
    top: ProofNode | undefined,
  ) {
    // without the token the caller's tree is never kept, and no method answers for the instance
    this.#top = token === TOKEN ? top : undefined;
    if (token === TOKEN) VERIFIED.stamp(this);
    Object.freeze(this);
  }

  static {
    createVerified = (mode, top) => new VerifiedTrieProof(TOKEN, mode, top);
    verifiedTop = (v) => v.#top;
    lookupOf = (v, key, n) => {
      const valid = checkKeyLength(v.mode, n);
      // the walk reads the key by index only, up to its internal length: no view of it is needed
      return valid.ok ? ok(lookupPath(v.#top, { bytes: key, length: n * 8 })) : valid;
    };
    TRIE.lookup = (v, key) => lookupOf(v, key, key.length);
    TRIE.prefix = (v, prefix) => listingOf(L.listPrefix(v.#top, prefix));
    TRIE.range = (v, from, until) => listingOf(L.listRange(v.#top, from, until));
  }

  /**
   * The value hash at `key`, its absence, or not covered; a key invalid for the mode is `InvalidKey`, and what is not a
   * `Uint8Array` is a `TypeMismatch`. The key's internal length (never its `length` property) decides both its
   * validity and the walk, which reads it by index only: O(1) to check, and nothing allocated.
   */
  lookup(key: Uint8Array): Result<Lookup, Refusal> {
    if (!VERIFIED.has(this)) return err(NOT_VERIFIED);
    const n = bytesLength(key);
    return n === undefined ? err(NOT_BYTES) : lookupOf(this, key, n);
  }

  /** Every entry whose key starts with `prefix`, in key order, if the proof covers the whole subtree. */
  prefix(prefix: Uint8Array): Result<Listing, TypeMismatch> {
    if (!VERIFIED.has(this)) return err(NOT_VERIFIED);
    const p = bytesArg(prefix);
    return p.ok ? ok(TRIE.prefix(this, p.value)) : p;
  }

  /**
   * Every entry with `from <= key < until` (bitwise order, a prefix first; no `until` is unbounded), if no stub meets
   * the range. A covered listing is complete, including an empty one. Bounds that are not `Uint8Array`s (an absent
   * `until` aside) are a `TypeMismatch`.
   */
  range(from: Uint8Array, until?: Uint8Array): Result<Listing, TypeMismatch> {
    if (!VERIFIED.has(this)) return err(NOT_VERIFIED);
    const f = bytesArg(from);
    if (!f.ok) return f;
    const u = boundArg(until);
    return u.ok ? ok(TRIE.range(this, f.value, u.value)) : u;
  }

  /**
   * Whether `claimed`, in key order, is exactly the listing under `prefix`, compared entry by entry as the walk reaches
   * each key, without building the listing; `NotCovered` if a stub lies under the prefix. A claim that is not an array
   * of `TrieEntry` objects of `Uint8Array`s (each read once, by index) is a `TypeMismatch`.
   */
  listsPrefix(prefix: Uint8Array, claimed: readonly TrieEntry[]): Result<boolean, Refusal> {
    if (!VERIFIED.has(this)) return err(NOT_VERIFIED);
    // the prefix copied first: reading the claim may run caller code, which cannot then change the query
    const p = queryArg(prefix);
    if (!p.ok) return p;
    const c = claimedArg(claimed);
    return c.ok ? verdictOf(L.listsPrefix(this.#top, p.value, c.value)) : c;
  }

  /** Whether `claimed`, in key order, is exactly the listing of `[from, until)`, as {@link listsPrefix} compares it. */
  listsRange(from: Uint8Array, until: Uint8Array | undefined, claimed: readonly TrieEntry[]): Result<boolean, Refusal> {
    if (!VERIFIED.has(this)) return err(NOT_VERIFIED);
    // the bounds copied first: reading the claim may run caller code, which cannot then change the query
    const f = queryArg(from);
    if (!f.ok) return f;
    const u = optionalQueryArg(until);
    if (!u.ok) return u;
    const c = claimedArg(claimed);
    return c.ok ? verdictOf(L.listsRange(this.#top, f.value, u.value, c.value)) : c;
  }
}

/** The lookup of a plain path, already checked against the mode. */
function lookupPath(top: ProofNode | undefined, path: B.Bits): Lookup {
  let node = top;
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

const listingOf = (entries: readonly TrieEntry[] | undefined): Listing => (entries === undefined ? NOT_COVERED : listed(entries));

const verdictOf = (v: boolean | undefined): Result<boolean, TrieError> =>
  v === undefined ? err({ kind: "NotCovered", what: "the proof does not cover the whole query" }) : ok(v);

// ------------------------------------------------------------------ JLVM operations (jlvm.md section 6)

/** A claimed entry: a key and its value (never null), committed by `commit(v)`. */
export type ClaimedEntry = readonly [Uint8Array, Value];

/**
 * The refusal of the profile and proof every JLVM operation starts from (a hashing, a mode and a proof value), or
 * `undefined` when all three are of their types: nothing is allocated for a check that passes.
 */
function refuseOp(hashing: unknown, mode: unknown, proof: unknown): Err<TypeMismatch> | undefined {
  if (!isHashing(hashing)) return err(NOT_A_HASHING);
  if (!isMode(mode)) return err(NOT_A_MODE);
  return isValue(proof) ? undefined : err(NOT_A_VALUE);
}

function verified(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, proof: Value): Result<VerifiedTrieProof | null, TrieError> {
  const r = digestOf(hashing, root);
  if (!r.ok) return r;
  const p = decodeProof(hashing, proof);
  if (!p.ok) return p;
  const v = checkOf(hashing, mode, r.value, p.value);
  return !v.ok && v.error.kind === "RootMismatch" ? ok(null) : v;
}

/** A claimed value: `null` (absence), or a canonical value; anything else is a `TypeMismatch`. */
const claimArg = (value: unknown): Result<Value | null, TypeMismatch> => (value === null ? ok(null) : valueArg(value));

const isAbsence = (v: Value | null): boolean => v === null || v.kind === "null";

/**
 * `trie_verify [mode, root, key, value, proof]`: whether `key` holds `value` (a null value claims absence). A
 * well-formed proof of another trie is `false`; malformed input, and a proof that does not reach the key, are errors.
 * Every argument's type is checked first: the key is read by its internal length, and a value that is neither `null`
 * nor a canonical value (an object posing as a null value included) is a `TypeMismatch`, never a claim of absence.
 */
export function trieVerify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, key: Uint8Array, value: Value | null, proof: Value): Result<boolean, Refusal> {
  const refused = refuseOp(hashing, mode, proof);
  if (refused !== undefined) return refused;
  const h = hashing;
  const m = mode;
  const p = proof;
  // copied: caller code that later reads run (an entry's getter, a Proxy's trap) cannot change the trusted root
  const r = takeBytesWithin(root, MAX_OUTPUT_SIZE);
  if (r === undefined) return err(NOT_BYTES);
  const n = bytesLength(key);
  if (n === undefined) return err(NOT_BYTES);
  const claim = claimArg(value);
  if (!claim.ok) return claim;
  const valid = checkKeyLength(m, n);
  if (!valid.ok) return valid;
  const c = claimsWithin(claim.value === null || claim.value.kind === "null" ? [] : [claim.value]);
  if (!c.ok) return c;
  const v = verified(h, m, r, p);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  const l = lookupOf(v.value, key, n);
  if (!l.ok) return l;
  switch (l.value.kind) {
    case "present": {
      if (claim.value === null || isAbsence(claim.value)) return ok(false);
      const vh = TRIE.valueHash(h, claim.value);
      return vh.ok ? ok(bytesEqual(vh.value, l.value.valueHash)) : vh;
    }
    case "absent":
      return ok(isAbsence(claim.value));
    case "notCovered":
      return err({ kind: "NotCovered", what: "the proof does not reach this key" });
  }
}

/** A caller's claimed entries: an array of `[key, value]` arrays, read by index, each key copied before its value is read. */
const entriesArg = (entries: unknown): Result<(readonly [Uint8Array, Value])[], TypeMismatch> =>
  readArray(entries, "an array of [key, value] entries", (e, i) => {
    const pair = readPair(e);
    if (pair === undefined) return err(expectedAt("a [key, value] array", i));
    const key = takeBytes(readGuarded(pair, 0));
    if (key === undefined) return err(expectedAt("a Uint8Array key", i));
    const value = valueArg(readGuarded(pair, 1));
    return value.ok ? ok([key, value.value] as const) : err(expectedAt("a canonical value", i));
  });

function claimedEntries(hashing: TrieHashing, mode: KeyMode, entries: readonly (readonly [Uint8Array, Value])[]): Result<readonly TrieEntry[], TrieError> {
  for (const [key, value] of entries) {
    if (value.kind === "null") return encoding("an entry is [key, value], with a value");
    const k = checkKey(mode, key);
    if (!k.ok) return k;
  }
  const c = claimsWithin(entries.map(([, value]) => value));
  if (!c.ok) return c;
  const out: TrieEntry[] = [];
  for (const [key, value] of entries) {
    const h = TRIE.valueHash(hashing, value);
    if (!h.ok) return h;
    out.push({ key, valueHash: h.value });
  }
  return ok(out);
}

/**
 * `trie_prefix_verify [mode, root, prefix, entries, proof]`: whether `entries`, in key order, are exactly the trie's
 * under `prefix`. Every argument's type is checked first; the entries are read by index, once each.
 */
export function triePrefixVerify(hashing: TrieHashing, mode: KeyMode, root: Uint8Array, prefix: Uint8Array, entries: readonly ClaimedEntry[], proof: Value): Result<boolean, Refusal> {
  const refused = refuseOp(hashing, mode, proof);
  if (refused !== undefined) return refused;
  const h = hashing;
  const m = mode;
  const p = proof;
  // copied: caller code that later reads run (an entry's getter, a Proxy's trap) cannot change the trusted root
  const r = takeBytesWithin(root, MAX_OUTPUT_SIZE);
  if (r === undefined) return err(NOT_BYTES);
  // the prefix copied before the entries are read: caller code reading them runs cannot then change the query
  const pre = queryArg(prefix);
  if (!pre.ok) return pre;
  const given = entriesArg(entries);
  if (!given.ok) return given;
  const claimed = claimedEntries(h, m, given.value);
  if (!claimed.ok) return claimed;
  const v = verified(h, m, r, p);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  return verdictOf(L.listsPrefix(verifiedTop(v.value), pre.value, claimed.value));
}

/** `trie_range_verify [mode, root, from, until, entries, proof]`: `until` null is unbounded. Checked as {@link triePrefixVerify}. */
export function trieRangeVerify(
  hashing: TrieHashing,
  mode: KeyMode,
  root: Uint8Array,
  from: Uint8Array,
  until: Uint8Array | null,
  entries: readonly ClaimedEntry[],
  proof: Value,
): Result<boolean, Refusal> {
  const refused = refuseOp(hashing, mode, proof);
  if (refused !== undefined) return refused;
  const h = hashing;
  const m = mode;
  const p = proof;
  // copied: caller code that later reads run (an entry's getter, a Proxy's trap) cannot change the trusted root
  const r = takeBytesWithin(root, MAX_OUTPUT_SIZE);
  if (r === undefined) return err(NOT_BYTES);
  // the bounds copied before the entries are read, as triePrefixVerify copies its prefix
  const f = queryArg(from);
  if (!f.ok) return f;
  const u = until === null ? ok(undefined) : queryArg(until);
  if (!u.ok) return u;
  const given = entriesArg(entries);
  if (!given.ok) return given;
  const claimed = claimedEntries(h, m, given.value);
  if (!claimed.ok) return claimed;
  const v = verified(h, m, r, p);
  if (!v.ok || v.value === null) return v.ok ? ok(false) : v;
  return verdictOf(L.listsRange(verifiedTop(v.value), f.value, u.value, claimed.value));
}

// ------------------------------------------------------------------ building

type BuildTask = { readonly lo: number; readonly hi: number; readonly offset: number } | { readonly segment: B.Bits; readonly value: Uint8Array | undefined; readonly left: boolean; readonly right: boolean };

/**
 * The root of the trie holding `entries` (later duplicates win): the same root as inserting them in any order, since
 * the shape is a function of the key set. Built bottom-up over the sorted keys with an explicit stack. `entries` is
 * read once, as `for...of` reads it (its iterator closed on a refusal); each entry must be a `[key, valueHash]` array of
 * `Uint8Array`s (read by index, each copied), and anything else is a `TypeMismatch`.
 */
export function trieRoot(hashing: TrieHashing, mode: KeyMode, entries: Iterable<readonly [Uint8Array, Uint8Array]>): Result<Uint8Array, Refusal> {
  const h = hashingArg(hashing);
  if (!h.ok) return h;
  const m = modeArg(mode);
  if (!m.ok) return m;
  const given = readIterable(entries, (e, i) => {
    const pair = readPair(e);
    const key = pair === undefined ? undefined : takeBytes(readGuarded(pair, 0));
    const valueHash = pair === undefined ? undefined : takeBytes(readGuarded(pair, 1));
    // both copied as read: an iterator may hand out one buffer again with new contents
    return key === undefined || valueHash === undefined ? err(expectedAt("a [key, valueHash] array of Uint8Arrays", i)) : ok([key, valueHash] as const);
  });
  return given.ok ? trieRootOf(h.value, m.value, given.value) : given;
}

/** {@link trieRoot} of plain entries, their keys owned. */
function trieRootOf(hashing: TrieHashing, mode: KeyMode, entries: readonly (readonly [Uint8Array, Uint8Array])[]): Result<Uint8Array, TrieError> {
  const given: (readonly [Uint8Array, Uint8Array])[] = [];
  for (const [key, valueHash] of entries) {
    const k = checkKey(mode, key);
    if (!k.ok) return k;
    const v = digestOf(hashing, valueHash);
    if (!v.ok) return v;
    given.push([key, v.value]);
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
      done.push(TRIE.branchBits(hashing, t.segment, t.value, left, right));
      continue;
    }
    const first = keyAt(t.lo);
    if (t.hi - t.lo === 1) {
      done.push(TRIE.leafBits(hashing, B.drop(first, t.offset), values[t.lo] ?? new Uint8Array(0)));
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

TRIE.fromRecords = fromRecords;
TRIE.isProof = isProof;
TRIE.check = checkOf;
TRIE.trieRoot = trieRootOf;
