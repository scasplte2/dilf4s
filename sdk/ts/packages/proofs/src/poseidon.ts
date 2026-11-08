/**
 * Poseidon over the BN254 scalar field (docs/specs/zk.md section 3), byte-compatible with circomlib / circomlibjs, and
 * the sparse Poseidon Merkle tree of section 4. Evaluated in the optimised form of the checked-in tables, with plain
 * `bigint` arithmetic mod `r`.
 *
 * **Public data only**: the arithmetic is variable time. Never hash secrets here.
 */
import { describeFailure, err, models, ok, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";

import { isArrayGuarded, isArrayLength, mint, readGuarded } from "@kd5ujc/dilf4s-core/internal";

import { bigintArg, expected, expectedAt, NOT_A_NUMBER, readIterable, readPair, TOKEN, valueArg, type Token } from "./check.js";

import { bitsOf, decodeInput } from "./input.js";
import { FULL_ROUNDS, WIDTH2, WIDTH3, WIDTH4, WIDTH5, type PoseidonParams } from "./poseidon-constants.js";

/** The BN254 scalar field modulus `r`. */
export const FR_MODULUS = 21888242871839275222246405745257275088548364400416034343698204186575808495617n;

/** Why hashing or a field element was refused: generated from `shared/schemas/proofs-errors.cddl`. */
export type PoseidonError = models.proofsErrors.PoseidonError;

const FR = mint();
const MERKLE_PROOF = mint();

let isFr!: (x: unknown) => x is Fr;
let reduceOf!: (value: bigint) => Fr;

const NOT_AN_FR = expected("an Fr");

/** An element of the BN254 scalar field, `0 <= value < r`, valid by construction. */
export class Fr {
  readonly #value: bigint;
  private constructor(token: Token, value: bigint) {
    this.#value = token === TOKEN ? value : 0n;
    if (token === TOKEN) FR.stamp(this);
    Object.freeze(this);
  }

  static {
    isFr = (x): x is Fr => FR.has(x);
    reduceOf = (value) => {
      const m = value % FR_MODULUS;
      return new Fr(TOKEN, m < 0n ? m + FR_MODULUS : m);
    };
  }

  static readonly zero: Fr = new Fr(TOKEN, 0n);

  /** The element `value`, if canonical: a value `>= r` is malformed, never reduced. What is not a `bigint` is a `TypeMismatch`. */
  static of(value: bigint): Result<Fr, PoseidonError | TypeMismatch> {
    const v = bigintArg(value);
    if (!v.ok) return v;
    return v.value >= 0n && v.value < FR_MODULUS ? ok(new Fr(TOKEN, v.value)) : err({ kind: "NonCanonical", bits: bitsOf(v.value) });
  }

  /** `value mod r`; what is not a `bigint` is a `TypeMismatch`. */
  static reduce(value: bigint): Result<Fr, TypeMismatch> {
    const v = bigintArg(value);
    return v.ok ? ok(reduceOf(v.value)) : v;
  }

  get value(): bigint {
    return this.#value;
  }

  /** Whether `other` is the same element; something that is not an `Fr` equals nothing (it is never read). */
  equals(other: Fr): boolean {
    return isFr(other) && this.#value === other.#value;
  }

  toString(): string {
    return this.#value.toString();
  }
}

export const MAX_INPUTS = models.limits.POSEIDON_MAX_INPUTS;

const add = (a: bigint, b: bigint): bigint => (a + b) % FR_MODULUS;
const mul = (a: bigint, b: bigint): bigint => (a * b) % FR_MODULUS;
const pow5 = (a: bigint): bigint => {
  const a2 = mul(a, a);
  return mul(mul(a2, a2), a);
};
const at = (xs: readonly bigint[], i: number): bigint => xs[i] ?? 0n;

const dot = (row: readonly bigint[], s: readonly bigint[]): bigint => {
  let acc = 0n;
  for (let j = 0; j < row.length; j++) acc += at(row, j) * at(s, j);
  return acc % FR_MODULUS;
};

const HALF = FULL_ROUNDS / 2;

/** S-box every element, add the constants from `from`, and multiply by `m`. */
function fullRound(f: PoseidonParams, m: readonly (readonly bigint[])[], s: readonly bigint[], from: number): bigint[] {
  const boxed = s.map((x, i) => add(pow5(x), at(f.constants, from + i)));
  return m.map((row) => dot(row, boxed));
}

/** The permutation of `[0, inputs..]` (Scala's `Poseidon.permute`), returning element 0. */
function permute(f: PoseidonParams, inputs: readonly Fr[]): Fr {
  const t = f.t;
  let s = Array.from({ length: t }, (_, i) => add(i === 0 ? 0n : (inputs[i - 1]?.value ?? 0n), at(f.constants, i)));
  for (let r = 1; r < HALF; r++) s = fullRound(f, f.mds, s, r * t);
  s = fullRound(f, f.preSparse, s, HALF * t);
  for (let r = 0; r < f.partialRounds; r++) {
    const x0 = add(pow5(at(s, 0)), at(f.constants, (HALF + 1) * t + r));
    const from = (2 * t - 1) * r;
    let s0 = x0 * at(f.sparse, from);
    for (let j = 1; j < t; j++) s0 += at(s, j) * at(f.sparse, from + j);
    s = s.map((x, k) => (k === 0 ? s0 % FR_MODULUS : add(x, mul(x0, at(f.sparse, from + t + k - 1)))));
  }
  const tail = (HALF + 1) * t + f.partialRounds;
  for (let r = 0; r < HALF - 1; r++) s = fullRound(f, f.mds, s, tail + r * t);
  // the last round adds no constant, and only element 0 of its output is read
  return reduceOf(dot(f.mds[0] ?? [], s.map(pow5)));
}

const BY_INPUTS: readonly PoseidonParams[] = [WIDTH2, WIDTH3, WIDTH4, WIDTH5];

/**
 * A caller's array of `Fr`s: `Array.isArray`, its length read once and checked against `max` before any item is read
 * (`tooMany` then), each item read once, by index.
 */
function frsArg<E>(x: unknown, what: string, max: number, tooMany: (count: number) => E): Result<Fr[], E | TypeMismatch> {
  if (!isArrayGuarded(x)) return err(expected(what));
  const count: unknown = (x as readonly unknown[]).length;
  if (!isArrayLength(count)) return err(expected(what));
  if (count > max) return err(tooMany(count));
  const out: Fr[] = [];
  for (let i = 0; i < count; i++) {
    // guarded: an earlier read (a Proxy's trap) may have revoked the array; REVOKED is no Fr
    const f: unknown = readGuarded(x, i);
    if (!isFr(f)) return err(expectedAt("an Fr", i));
    out.push(f);
  }
  return ok(out);
}

/**
 * `poseidon([x1..xn])` for 1 to 4 inputs: width `n + 1`, state `[0, x1, ..]`, output `state[0]`. Inputs that are not an
 * array of `Fr`s are a `TypeMismatch`; more than four are refused by their count before any is read.
 */
export function hash(inputs: readonly Fr[]): Result<Fr, PoseidonError | TypeMismatch> {
  const xs = frsArg<PoseidonError>(inputs, "an array of Fr", MAX_INPUTS, (count) => ({ kind: "InputCount", count, max: MAX_INPUTS }));
  if (!xs.ok) return xs;
  const params = xs.value.length >= 1 ? BY_INPUTS[xs.value.length - 1] : undefined;
  return params === undefined ? err({ kind: "InputCount", count: xs.value.length, max: MAX_INPUTS }) : ok(permute(params, xs.value));
}

/** Two-to-one compression for Merkle nodes, unchecked. */
const compressOf = (left: Fr, right: Fr): Fr => permute(WIDTH3, [left, right]);

/** Two-to-one compression for Merkle nodes: `poseidon([l, r])`. What is not an `Fr` is a `TypeMismatch`. */
export function compress(left: Fr, right: Fr): Result<Fr, TypeMismatch> {
  return isFr(left) && isFr(right) ? ok(compressOf(left, right)) : err(NOT_AN_FR);
}

// ------------------------------------------------------------------ the Merkle tree (section 4)

/**
 * Why a Poseidon Merkle proof or tree was refused: generated from `shared/schemas/proofs-errors.cddl`. `Position`'s
 * `depth` is normative; a caller's position can be any size, so it is held by its bit length, never itself
 * (conformance.md section 8). `OccupiedPosition` is the reference tree's insertion refusal, which this SDK does not have.
 */
export type PoseidonMerkleError = models.proofsErrors.PoseidonMerkleError;

export const MAX_DEPTH = models.limits.POSEIDON_MAX_DEPTH;

let isMerkleProof!: (x: unknown) => x is PoseidonMerkleProof;
let siblingsOf!: (p: PoseidonMerkleProof) => readonly Fr[];
let createMerkleProof!: (position: bigint, siblings: readonly Fr[]) => PoseidonMerkleProof;

/** A path of siblings, root-first, with `0 <= position < 2^depth` (depth = the number of siblings). */
export class PoseidonMerkleProof {
  readonly #siblings: readonly Fr[];
  private constructor(
    token: Token,
    readonly position: bigint,
    siblings: readonly Fr[],
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) this.position = 0n;
    this.#siblings = Object.freeze(token === TOKEN ? [...siblings] : []);
    if (token === TOKEN) MERKLE_PROOF.stamp(this);
    Object.freeze(this);
  }

  static {
    isMerkleProof = (x): x is PoseidonMerkleProof => MERKLE_PROOF.has(x);
    siblingsOf = (p) => p.#siblings;
    createMerkleProof = (position, siblings) => new PoseidonMerkleProof(TOKEN, position, siblings);
  }

  get siblings(): readonly Fr[] {
    return this.#siblings;
  }

  get depth(): number {
    return this.#siblings.length;
  }

  /**
   * The depth is checked first, so the capacity `2^depth` it is compared with never exceeds `2^MAX_DEPTH`. A position
   * that is not a `bigint`, or siblings that are not an array of `Fr`s (its length read once and bounded by `MAX_DEPTH`
   * before any is read, each read once), are a `TypeMismatch`.
   */
  static of(position: bigint, siblings: readonly Fr[]): Result<PoseidonMerkleProof, PoseidonMerkleError | TypeMismatch> {
    const p = bigintArg(position);
    if (!p.ok) return p;
    const xs = frsArg<PoseidonMerkleError>(siblings, "an array of Fr", MAX_DEPTH, (depth) => ({ kind: "Position", positionBits: bitsOf(p.value), depth }));
    return xs.ok ? merkleProofOf(p.value, xs.value) : xs;
  }

  /**
   * A proof from its canonical value (`poseidon-merkle-proof`): field elements below `r`, `position < 2^depth`. A value
   * heavier than `MAX_PROOF_WEIGHT` is `Encoding`, refused before it is decoded.
   */
  static decode(value: Value): Result<PoseidonMerkleProof, PoseidonMerkleError | TypeMismatch> {
    const v = valueArg(value);
    if (!v.ok) return v;
    const w = decodeInput(models.poseidon.PoseidonMerkleProof, v.value);
    if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
    const siblings: Fr[] = [];
    for (const [i, x] of w.value.siblings.entries()) {
      const f = Fr.of(x);
      // a caller's number by its position and bit length, never its digits (conformance.md section 2)
      if (!f.ok) return err({ kind: "Encoding", reason: `sibling ${i.toString()} is not below r (${bitsOf(x).toString()} bits)` });
      siblings.push(f.value);
    }
    return merkleProofOf(w.value.position, siblings);
  }

  /** The canonical value. */
  encode(): Result<Value, PoseidonMerkleError> {
    const v = models.poseidon.PoseidonMerkleProof.encode({ position: this.position, siblings: this.#siblings.map((s) => s.value) });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

/** {@link PoseidonMerkleProof.of} of a checked position and siblings. */
const merkleProofOf = (position: bigint, siblings: readonly Fr[]): Result<PoseidonMerkleProof, PoseidonMerkleError> => {
  const depth = siblings.length;
  return depth <= MAX_DEPTH && position >= 0n && position < 1n << BigInt(depth) ? ok(createMerkleProof(position, siblings)) : err({ kind: "Position", positionBits: bitsOf(position), depth });
};

/** {@link computeRoot} of checked arguments. */
function rootOf(leaf: Fr, proof: PoseidonMerkleProof): Fr {
  let cur = leaf;
  const siblings = siblingsOf(proof);
  for (let level = 0; level < siblings.length; level++) {
    const sib = siblings[siblings.length - 1 - level] ?? Fr.zero;
    cur = ((proof.position >> BigInt(level)) & 1n) === 1n ? compressOf(sib, cur) : compressOf(cur, sib);
  }
  return cur;
}

const NOT_A_PROOF = expected("a PoseidonMerkleProof");

/**
 * The root a leaf and its path fold to: at level `i` from the leaf, bit `i` of the position marks a right child. A leaf
 * that is not an `Fr`, or a proof that is not a `PoseidonMerkleProof`, is a `TypeMismatch`.
 */
export function computeRoot(leaf: Fr, proof: PoseidonMerkleProof): Result<Fr, TypeMismatch> {
  if (!isFr(leaf)) return err(NOT_AN_FR);
  return isMerkleProof(proof) ? ok(rootOf(leaf, proof)) : err(NOT_A_PROOF);
}

/** Whether `leaf` folds along `proof` to `root`. What is not an `Fr` or a `PoseidonMerkleProof` is a `TypeMismatch`, never `false`. */
export function verifyInclusion(leaf: Fr, proof: PoseidonMerkleProof, root: Fr): Result<boolean, TypeMismatch> {
  if (!isFr(root)) return err(NOT_AN_FR);
  const r = computeRoot(leaf, proof);
  return r.ok ? ok(r.value.equals(root)) : r;
}

/** An absence proof is an inclusion proof of 0; arguments are checked as {@link verifyInclusion} checks them. */
export function verifyAbsence(proof: PoseidonMerkleProof, root: Fr): Result<boolean, TypeMismatch> {
  return verifyInclusion(Fr.zero, proof, root);
}

/**
 * `zero_0 = 0`, `zero_{i+1} = poseidon(zero_i, zero_i)`, for `i <= depth`: a depth outside `1..MAX_DEPTH` is `Depth`,
 * and one that is not a number is a `TypeMismatch`.
 */
export function zeroHashes(depth: number): Result<readonly Fr[], PoseidonMerkleError | TypeMismatch> {
  if (typeof (depth as unknown) !== "number") return err(NOT_A_NUMBER);
  if (!Number.isInteger(depth) || depth < 1 || depth > MAX_DEPTH) return err({ kind: "Depth", depth });
  const out = [Fr.zero];
  for (let i = 0; i < depth; i++) {
    const prev = out[i] ?? Fr.zero;
    out.push(compressOf(prev, prev));
  }
  return ok(out);
}

type Placed = readonly [position: bigint, node: Fr];

const byPosition = (a: Placed, b: Placed): number => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0);

/**
 * The root of the sparse tree of `depth` holding `leaves`, `(position, leaf)` entries applied in order: a later entry for
 * a position replaces an earlier one, and a leaf of 0 is empty. Positions are the caller's and V8 hashes a bigint by its
 * low 64 bits, so they are only ever compared: one stable sort, then each level pairs neighbours in one pass. O(n log n)
 * comparisons and O(n · depth) compressions. A `Map` is an iterable of entries, so it may be passed as before.
 * `leaves` is read once, as `for...of` reads it (its iterator closed on a refusal); an entry that is not a
 * `[bigint, Fr]` array (read by index) is a `TypeMismatch`, as is a depth that is not a number.
 */
export function treeRoot(depth: number, leaves: Iterable<Placed>): Result<Fr, PoseidonMerkleError | TypeMismatch> {
  const zeros = zeroHashes(depth);
  if (!zeros.ok) return zeros;
  const capacity = 1n << BigInt(depth);
  // each entry is copied as it is read: an iterable may yield one tuple again with new contents
  const read = readIterable<Placed, PoseidonMerkleError | TypeMismatch>(leaves, (e, i) => {
    const pair = readPair(e);
    if (pair === undefined) return err(expectedAt("a [position, leaf] array", i));
    const p: unknown = readGuarded(pair, 0);
    const leaf: unknown = readGuarded(pair, 1);
    if (typeof p !== "bigint" || !isFr(leaf)) return err(expectedAt("a [bigint, Fr] array", i));
    return p < 0n || p >= capacity ? err({ kind: "Position", positionBits: bitsOf(p), depth }) : ok([p, leaf] as const);
  });
  if (!read.ok) return read;
  const entries = read.value;
  // the sort is stable, so the last of a position's run of entries is its latest
  entries.sort(byPosition);
  let level: Placed[] = entries.filter((e, i) => entries[i + 1]?.[0] !== e[0] && !e[1].equals(Fr.zero));
  for (let i = 0; i < depth; i++) {
    const zero = zeros.value[i] ?? Fr.zero;
    const up: Placed[] = [];
    for (let j = 0; j < level.length; j++) {
      const [p, node] = level[j] ?? [0n, zero];
      const right = level[j + 1];
      if ((p & 1n) === 1n) up.push([p >> 1n, compressOf(zero, node)]);
      else if (right?.[0] === (p | 1n)) {
        up.push([p >> 1n, compressOf(node, right[1])]);
        j++;
      } else up.push([p >> 1n, compressOf(node, zero)]);
    }
    level = up;
  }
  return ok(level[0]?.[1] ?? zeros.value[depth] ?? Fr.zero);
}
