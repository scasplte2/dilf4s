/**
 * Poseidon over the BN254 scalar field (docs/specs/zk.md section 3), byte-compatible with circomlib / circomlibjs, and
 * the sparse Poseidon Merkle tree of section 4. Evaluated in the optimised form of the checked-in tables, with plain
 * `bigint` arithmetic mod `r`.
 *
 * **Public data only**: the arithmetic is variable time. Never hash secrets here.
 */
import { describeFailure, err, models, ok, type Result, type Value } from "@kd5ujc/dilf4s-core";

import { decodeInput } from "./input.js";
import { FULL_ROUNDS, WIDTH2, WIDTH3, WIDTH4, WIDTH5, type PoseidonParams } from "./poseidon-constants.js";

/** The BN254 scalar field modulus `r`. */
export const FR_MODULUS = 21888242871839275222246405745257275088548364400416034343698204186575808495617n;

export type PoseidonError = { readonly kind: "InputCount"; readonly count: number; readonly max: number } | { readonly kind: "NonCanonical"; readonly value: bigint };

/** An element of the BN254 scalar field, `0 <= value < r`, valid by construction. */
export class Fr {
  readonly #value: bigint;
  private constructor(value: bigint) {
    this.#value = value;
    Object.freeze(this);
  }

  static readonly zero: Fr = new Fr(0n);

  /** The element `value`, if canonical: a value `>= r` is malformed, never reduced. */
  static of(value: bigint): Result<Fr, PoseidonError> {
    return value >= 0n && value < FR_MODULUS ? ok(new Fr(value)) : err({ kind: "NonCanonical", value });
  }

  /** `value mod r`. */
  static reduce(value: bigint): Fr {
    const m = value % FR_MODULUS;
    return new Fr(m < 0n ? m + FR_MODULUS : m);
  }

  get value(): bigint {
    return this.#value;
  }

  equals(other: Fr): boolean {
    return this.#value === other.#value;
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
  return Fr.reduce(dot(f.mds[0] ?? [], s.map(pow5)));
}

const BY_INPUTS: readonly PoseidonParams[] = [WIDTH2, WIDTH3, WIDTH4, WIDTH5];

/** `poseidon([x1..xn])` for 1 to 4 inputs: width `n + 1`, state `[0, x1, ..]`, output `state[0]`. */
export function hash(inputs: readonly Fr[]): Result<Fr, PoseidonError> {
  const params = inputs.length >= 1 ? BY_INPUTS[inputs.length - 1] : undefined;
  return params === undefined ? err({ kind: "InputCount", count: inputs.length, max: MAX_INPUTS }) : ok(permute(params, inputs));
}

/** Two-to-one compression for Merkle nodes: `poseidon([l, r])`. */
export function compress(left: Fr, right: Fr): Fr {
  return permute(WIDTH3, [left, right]);
}

// ------------------------------------------------------------------ the Merkle tree (section 4)

export type PoseidonMerkleError =
  /** `depth` (the number of siblings, or the tree's depth) is normative; the position echoes the caller's input. */
  | { readonly kind: "Position"; readonly position: bigint; readonly depth: number }
  | { readonly kind: "Depth"; readonly depth: number }
  | { readonly kind: "Encoding"; readonly reason: string };

export const MAX_DEPTH = models.limits.POSEIDON_MAX_DEPTH;

/** A path of siblings, root-first, with `0 <= position < 2^depth` (depth = the number of siblings). */
export class PoseidonMerkleProof {
  readonly #siblings: readonly Fr[];
  private constructor(
    readonly position: bigint,
    siblings: readonly Fr[],
  ) {
    this.#siblings = Object.freeze([...siblings]);
    Object.freeze(this);
  }

  get siblings(): readonly Fr[] {
    return this.#siblings;
  }

  get depth(): number {
    return this.#siblings.length;
  }

  /** The depth is checked first, so the capacity `2^depth` it is compared with never exceeds `2^MAX_DEPTH`. */
  static of(position: bigint, siblings: readonly Fr[]): Result<PoseidonMerkleProof, PoseidonMerkleError> {
    const depth = siblings.length;
    return depth <= MAX_DEPTH && position >= 0n && position < 1n << BigInt(depth) ? ok(new PoseidonMerkleProof(position, siblings)) : err({ kind: "Position", position, depth });
  }

  /**
   * A proof from its canonical value (`poseidon-merkle-proof`): field elements below `r`, `position < 2^depth`. A value
   * heavier than `MAX_PROOF_WEIGHT` is `Encoding`, refused before it is decoded.
   */
  static decode(value: Value): Result<PoseidonMerkleProof, PoseidonMerkleError> {
    const w = decodeInput(models.poseidon.PoseidonMerkleProof, value);
    if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
    const siblings: Fr[] = [];
    for (const x of w.value.siblings) {
      const f = Fr.of(x);
      if (!f.ok) return err({ kind: "Encoding", reason: `sibling ${x.toString()} is not below r` });
      siblings.push(f.value);
    }
    return PoseidonMerkleProof.of(w.value.position, siblings);
  }

  /** The canonical value. */
  encode(): Result<Value, PoseidonMerkleError> {
    const v = models.poseidon.PoseidonMerkleProof.encode({ position: this.position, siblings: this.#siblings.map((s) => s.value) });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

/** The root a leaf and its path fold to: at level `i` from the leaf, bit `i` of the position marks a right child. */
export function computeRoot(leaf: Fr, proof: PoseidonMerkleProof): Fr {
  let cur = leaf;
  const siblings = proof.siblings;
  for (let level = 0; level < siblings.length; level++) {
    const sib = siblings[siblings.length - 1 - level] ?? Fr.zero;
    cur = ((proof.position >> BigInt(level)) & 1n) === 1n ? compress(sib, cur) : compress(cur, sib);
  }
  return cur;
}

export function verifyInclusion(leaf: Fr, proof: PoseidonMerkleProof, root: Fr): boolean {
  return computeRoot(leaf, proof).equals(root);
}

/** An absence proof is an inclusion proof of 0. */
export function verifyAbsence(proof: PoseidonMerkleProof, root: Fr): boolean {
  return verifyInclusion(Fr.zero, proof, root);
}

/** `zero_0 = 0`, `zero_{i+1} = poseidon(zero_i, zero_i)`, for `i <= depth`. */
export function zeroHashes(depth: number): Result<readonly Fr[], PoseidonMerkleError> {
  if (!Number.isInteger(depth) || depth < 1 || depth > MAX_DEPTH) return err({ kind: "Depth", depth });
  const out = [Fr.zero];
  for (let i = 0; i < depth; i++) {
    const prev = out[i] ?? Fr.zero;
    out.push(compress(prev, prev));
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
 */
export function treeRoot(depth: number, leaves: Iterable<Placed>): Result<Fr, PoseidonMerkleError> {
  const zeros = zeroHashes(depth);
  if (!zeros.ok) return zeros;
  const capacity = 1n << BigInt(depth);
  const entries: Placed[] = [];
  // each entry is copied as it is read: an iterable may yield one tuple again with new contents
  for (const [p, leaf] of leaves) {
    if (p < 0n || p >= capacity) return err({ kind: "Position", position: p, depth });
    entries.push([p, leaf]);
  }
  // the sort is stable, so the last of a position's run of entries is its latest
  entries.sort(byPosition);
  let level: Placed[] = entries.filter((e, i) => entries[i + 1]?.[0] !== e[0] && !e[1].equals(Fr.zero));
  for (let i = 0; i < depth; i++) {
    const zero = zeros.value[i] ?? Fr.zero;
    const up: Placed[] = [];
    for (let j = 0; j < level.length; j++) {
      const [p, node] = level[j] ?? [0n, zero];
      const right = level[j + 1];
      if ((p & 1n) === 1n) up.push([p >> 1n, compress(zero, node)]);
      else if (right?.[0] === (p | 1n)) {
        up.push([p >> 1n, compress(node, right[1])]);
        j++;
      } else up.push([p >> 1n, compress(node, zero)]);
    }
    level = up;
  }
  return ok(level[0]?.[1] ?? zeros.value[depth] ?? Fr.zero);
}
