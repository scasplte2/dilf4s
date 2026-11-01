/**
 * The dilf4s Merkle tree (docs/specs/merkle.md): RFC 9162 inclusion and consistency verification over the tagged
 * profile (any core hash algorithm) or the RFC 6962 profile, proof decoding from canonical values, and the JLVM's
 * `merkle_verify` / `merkle_consistency_verify` over a trusted head `{root, size}` (jlvm.md section 6).
 */
import { bitLength, describeFailure, digest, err, hashParts, models, ok, outputSize, toHex, valueCommitment, type HashAlgorithm, type Result, type Value } from "@kd5ujc/dilf4s-core";

import { bytesEqual, ConstantTag, copyBytes } from "./bytes.js";

/** Why a Merkle proof could not be checked, by Scala's `MerkleError` names. */
export type MerkleError =
  | { readonly kind: "IndexOutOfRange"; readonly index: bigint; readonly size: bigint }
  | { readonly kind: "InvalidSizes"; readonly oldSize: bigint; readonly newSize: bigint }
  | { readonly kind: "InvalidProof"; readonly reason: string }
  | { readonly kind: "RootMismatch"; readonly expected: string; readonly computed: string }
  | { readonly kind: "Encoding"; readonly reason: string };

/** The largest tree size or index: the schema's `tree-index`. */
export const MAX_TREE_INDEX = 2n ** 63n - 1n;

export const LEAF_TAG = "dilf4s/merkle/leaf/v1";
export const NODE_TAG = "dilf4s/merkle/node/v1";
export const EMPTY_TAG = "dilf4s/merkle/empty/v1";

/**
 * The hash functions of a Merkle profile (merkle.md section 2). `L` is what a leaf is given as: a canonical value for the
 * tagged profile (committed by `commit(v)`), raw bytes for RFC 6962. The tagged profile has no raw-bytes leaf, so a
 * proof of the bytes `0x6161` never verifies the text `"a"`; an already-computed leaf hash is the low-level input.
 */
export interface MerkleHashing<L> {
  readonly profile: "tagged" | "rfc6962";
  readonly algorithm: HashAlgorithm;
  /** The digest size in bytes. */
  readonly size: number;
  /** The root of the empty tree. */
  readonly empty: Uint8Array;
  leaf(input: L): Uint8Array;
  node(left: Uint8Array, right: Uint8Array): Uint8Array;
}

/** The dilf4s profile: tagged hashes per role, leaves committed by `commit(v)`. */
export class TaggedMerkleHashing implements MerkleHashing<Value> {
  readonly profile = "tagged";
  readonly size: number;
  readonly #leaf: ConstantTag;
  readonly #node: ConstantTag;
  readonly #empty: Uint8Array;
  private constructor(readonly algorithm: HashAlgorithm) {
    this.size = outputSize(algorithm);
    this.#leaf = new ConstantTag(algorithm, LEAF_TAG);
    this.#node = new ConstantTag(algorithm, NODE_TAG);
    this.#empty = new ConstantTag(algorithm, EMPTY_TAG).hash(new Uint8Array(0));
    Object.freeze(this);
  }

  static of(algorithm: HashAlgorithm): TaggedMerkleHashing {
    return new TaggedMerkleHashing(algorithm);
  }

  get empty(): Uint8Array {
    return copyBytes(this.#empty);
  }

  /** A value leaf's hash: the leaf hash of `commit(value)` (`MerkleTree.leaf`). */
  leaf(value: Value): Uint8Array {
    return this.#leaf.hash(valueCommitment(value));
  }

  node(left: Uint8Array, right: Uint8Array): Uint8Array {
    return this.#node.hashParts([left, right]);
  }
}

/** RFC 6962's one-byte prefixes, for external logs: leaves are raw bytes. */
export class Rfc6962MerkleHashing implements MerkleHashing<Uint8Array> {
  readonly profile = "rfc6962";
  readonly size: number;
  private constructor(readonly algorithm: HashAlgorithm) {
    this.size = outputSize(algorithm);
    Object.freeze(this);
  }

  static of(algorithm: HashAlgorithm): Rfc6962MerkleHashing {
    return new Rfc6962MerkleHashing(algorithm);
  }

  get empty(): Uint8Array {
    return digest(this.algorithm, new Uint8Array(0));
  }

  leaf(data: Uint8Array): Uint8Array {
    return hashParts(this.algorithm, [Uint8Array.of(0), data]);
  }

  node(left: Uint8Array, right: Uint8Array): Uint8Array {
    return hashParts(this.algorithm, [Uint8Array.of(1), left, right]);
  }
}

const withinIndex = (n: bigint): boolean => n >= 0n && n <= MAX_TREE_INDEX;

/** An audit path for leaf `index` of a tree of `size` leaves, `0 <= index < size`. */
export class InclusionProof {
  readonly #path: readonly Uint8Array[];
  private constructor(
    readonly index: bigint,
    readonly size: bigint,
    path: readonly Uint8Array[],
  ) {
    this.#path = Object.freeze(path.map(copyBytes));
    Object.freeze(this);
  }

  /** The path's digests, leaf side first (copies). */
  get path(): readonly Uint8Array[] {
    return this.#path.map(copyBytes);
  }

  static of(index: bigint, size: bigint, path: readonly Uint8Array[]): Result<InclusionProof, MerkleError> {
    return index >= 0n && index < size && withinIndex(size) ? ok(new InclusionProof(index, size, path)) : err({ kind: "IndexOutOfRange", index, size });
  }

  /** A proof from its canonical value (`merkle-inclusion`), its digests sized for `hashing`. */
  static decode(hashing: MerkleHashing<unknown>, value: Value): Result<InclusionProof, MerkleError> {
    const w = models.merkle.MerkleInclusion.decode(value);
    if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
    const sized = digests(hashing, w.value.path);
    return sized.ok ? InclusionProof.of(w.value.index, w.value.size, w.value.path) : sized;
  }

  /** The canonical value (`merkle-inclusion`). */
  encode(): Result<Value, MerkleError> {
    const v = models.merkle.MerkleInclusion.encode({ index: this.index, size: this.size, path: this.#path });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

/** A consistency path between tree sizes `0 <= oldSize <= newSize`. */
export class ConsistencyProof {
  readonly #path: readonly Uint8Array[];
  private constructor(
    readonly oldSize: bigint,
    readonly newSize: bigint,
    path: readonly Uint8Array[],
  ) {
    this.#path = Object.freeze(path.map(copyBytes));
    Object.freeze(this);
  }

  get path(): readonly Uint8Array[] {
    return this.#path.map(copyBytes);
  }

  /** Sizes out of order, or outside the tree-index range, are `InvalidSizes`, as in Scala and the verifier. */
  static of(oldSize: bigint, newSize: bigint, path: readonly Uint8Array[]): Result<ConsistencyProof, MerkleError> {
    return oldSize >= 0n && oldSize <= newSize && withinIndex(newSize)
      ? ok(new ConsistencyProof(oldSize, newSize, path))
      : err({ kind: "InvalidSizes", oldSize, newSize });
  }

  /** A proof from its canonical value (`merkle-consistency`), its digests sized for `hashing`. */
  static decode(hashing: MerkleHashing<unknown>, value: Value): Result<ConsistencyProof, MerkleError> {
    const w = models.merkle.MerkleConsistency.decode(value);
    if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
    const sized = digests(hashing, w.value.path);
    return sized.ok ? ConsistencyProof.of(w.value.old, w.value.new, w.value.path) : sized;
  }

  encode(): Result<Value, MerkleError> {
    const v = models.merkle.MerkleConsistency.encode({ old: this.oldSize, new: this.newSize, path: this.#path });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

function sized(hashing: MerkleHashing<unknown>, what: string, d: Uint8Array): Result<undefined, MerkleError> {
  return d.length === hashing.size ? ok(undefined) : err({ kind: "Encoding", reason: `${what} is ${d.length.toString()} bytes, not ${hashing.size.toString()}` });
}

function digests(hashing: MerkleHashing<unknown>, path: readonly Uint8Array[]): Result<undefined, MerkleError> {
  for (const [i, d] of path.entries()) {
    const s = sized(hashing, `path[${i.toString()}]`, d);
    if (!s.ok) return s;
  }
  return ok(undefined);
}

const invalid = (reason: string): Result<never, MerkleError> => err({ kind: "InvalidProof", reason });
const mismatch = (expected: Uint8Array, computed: Uint8Array): Result<never, MerkleError> => err({ kind: "RootMismatch", expected: toHex(expected), computed: toHex(computed) });

/** Right-shifts both until `fn` is odd or zero. */
function shiftUntilOddOrZero(fn: bigint, sn: bigint): readonly [bigint, bigint] {
  let [f, s] = [fn, sn];
  while ((f & 1n) === 0n && f !== 0n) [f, s] = [f >> 1n, s >> 1n];
  return [f, s];
}

/**
 * RFC 9162 section 2.1.3.2: whether `leafHash` is leaf `proof.index` of the tree of `proof.size` leaves with `root`. A
 * well-formed proof that fails is `InvalidProof` or `RootMismatch`; digests of the wrong size are `Encoding`.
 */
export function checkInclusion(hashing: MerkleHashing<unknown>, root: Uint8Array, leafHash: Uint8Array, proof: InclusionProof): Result<undefined, MerkleError> {
  const lengths = [sized(hashing, "root", root), sized(hashing, "leaf hash", leafHash), digests(hashing, proof.path)].find((r) => !r.ok);
  if (lengths !== undefined) return lengths;
  if (proof.index < 0n || proof.index >= proof.size) return err({ kind: "IndexOutOfRange", index: proof.index, size: proof.size });
  let fn = proof.index;
  let sn = proof.size - 1n;
  let r = leafHash;
  for (const p of proof.path) {
    if (sn === 0n) return invalid("path is too long for the tree size");
    if ((fn & 1n) === 1n || fn === sn) {
      [fn, sn] = (fn & 1n) === 0n ? shiftUntilOddOrZero(fn, sn) : [fn, sn];
      r = hashing.node(p, r);
    } else r = hashing.node(r, p);
    [fn, sn] = [fn >> 1n, sn >> 1n];
  }
  if (sn !== 0n) return invalid("path is too short for the tree size");
  return bytesEqual(r, root) ? ok(undefined) : mismatch(root, r);
}

/** RFC 9162 section 2.1.4.2: whether the tree of `oldRoot` is a prefix of the tree of `newRoot`. */
export function checkConsistency(hashing: MerkleHashing<unknown>, oldRoot: Uint8Array, newRoot: Uint8Array, proof: ConsistencyProof): Result<undefined, MerkleError> {
  const given = proof.path;
  const lengths = [sized(hashing, "old root", oldRoot), sized(hashing, "new root", newRoot), digests(hashing, given)].find((r) => !r.ok);
  if (lengths !== undefined) return lengths;
  const [m, n] = [proof.oldSize, proof.newSize];
  if (m < 0n || m > n) return err({ kind: "InvalidSizes", oldSize: m, newSize: n });
  if (m === 0n) return given.length === 0 && bytesEqual(oldRoot, hashing.empty) ? ok(undefined) : invalid("size 0 needs an empty proof and the empty root");
  if (m === n) return given.length === 0 && bytesEqual(oldRoot, newRoot) ? ok(undefined) : invalid("equal sizes need an empty proof and equal roots");
  const path = (m & (m - 1n)) === 0n ? [oldRoot, ...given] : given;
  const [first, ...rest] = path;
  if (first === undefined) return invalid("empty consistency path");
  let [fn, sn] = [m - 1n, n - 1n];
  while ((fn & 1n) === 1n) [fn, sn] = [fn >> 1n, sn >> 1n];
  let fr = first;
  let sr = first;
  for (const c of rest) {
    if (sn === 0n) return invalid("path is too long for the sizes");
    if ((fn & 1n) === 1n || fn === sn) {
      [fn, sn] = (fn & 1n) === 0n ? shiftUntilOddOrZero(fn, sn) : [fn, sn];
      fr = hashing.node(c, fr);
      sr = hashing.node(c, sr);
    } else sr = hashing.node(sr, c);
    [fn, sn] = [fn >> 1n, sn >> 1n];
  }
  if (sn !== 0n) return invalid("path is too short for the sizes");
  if (!bytesEqual(fr, oldRoot)) return mismatch(oldRoot, fr);
  return bytesEqual(sr, newRoot) ? ok(undefined) : mismatch(newRoot, sr);
}

/** A failed check that is a well-formed proof of something else is `false`; the other failures are errors. */
function outcome(r: Result<undefined, MerkleError>): Result<boolean, MerkleError> {
  if (r.ok) return ok(true);
  return r.error.kind === "InvalidProof" || r.error.kind === "RootMismatch" ? ok(false) : r;
}

/** {@link checkInclusion} as a verdict: `InvalidProof` and `RootMismatch` are `false`. */
export function verifyInclusion(hashing: MerkleHashing<unknown>, root: Uint8Array, leafHash: Uint8Array, proof: InclusionProof): Result<boolean, MerkleError> {
  return outcome(checkInclusion(hashing, root, leafHash, proof));
}

/** {@link checkConsistency} as a verdict: `InvalidProof` and `RootMismatch` are `false`. */
export function verifyConsistency(hashing: MerkleHashing<unknown>, oldRoot: Uint8Array, newRoot: Uint8Array, proof: ConsistencyProof): Result<boolean, MerkleError> {
  return outcome(checkConsistency(hashing, oldRoot, newRoot, proof));
}

/** A trusted tree head: a root and the size of the tree it is the root of, taken together. */
export interface MerkleHead {
  readonly root: Uint8Array;
  readonly size: bigint;
}

function checkHead(hashing: MerkleHashing<unknown>, what: string, head: MerkleHead): Result<undefined, MerkleError> {
  if (!withinIndex(head.size)) return err({ kind: "Encoding", reason: `${what}.size of ${bitLength(head.size).toString()} bits is not a tree size` });
  return sized(hashing, `${what}.root`, head.root);
}

/**
 * The JLVM's `merkle_verify [head, leaf, proof]`: whether `leaf` is in the tree of the trusted `head`, with the proof as
 * its canonical value. The leaf is a value for the tagged profile and raw bytes for RFC 6962. A proof whose size is not
 * the head's is `false`: a smaller tree's root can satisfy a proof that claims a larger size (merkle.md section 3).
 */
export function merkleVerify<L>(hashing: MerkleHashing<L>, head: MerkleHead, leaf: L, proof: Value): Result<boolean, MerkleError> {
  const h = checkHead(hashing, "head", head);
  if (!h.ok) return h;
  const p = InclusionProof.decode(hashing, proof);
  if (!p.ok) return p;
  return p.value.size !== head.size ? ok(false) : verifyInclusion(hashing, head.root, hashing.leaf(leaf), p.value);
}

/** The JLVM's `merkle_consistency_verify [oldHead, newHead, proof]`: sizes that differ from the heads' are `false`. */
export function merkleConsistencyVerify(hashing: MerkleHashing<unknown>, oldHead: MerkleHead, newHead: MerkleHead, proof: Value): Result<boolean, MerkleError> {
  const o = checkHead(hashing, "oldHead", oldHead);
  if (!o.ok) return o;
  const n = checkHead(hashing, "newHead", newHead);
  if (!n.ok) return n;
  const p = ConsistencyProof.decode(hashing, proof);
  if (!p.ok) return p;
  if (p.value.oldSize !== oldHead.size || p.value.newSize !== newHead.size) return ok(false);
  return verifyConsistency(hashing, oldHead.root, newHead.root, p.value);
}

/** The root of the tree over `leafHashes` (merkle.md section 1): pair level by level, promoting an unpaired last node. */
export function merkleRoot(hashing: MerkleHashing<unknown>, leafHashes: readonly Uint8Array[]): Result<Uint8Array, MerkleError> {
  const lengths = digests(hashing, leafHashes);
  if (!lengths.ok) return lengths;
  let level = leafHashes.map(copyBytes);
  if (level.length === 0) return ok(hashing.empty);
  while (level.length > 1) {
    const next: Uint8Array[] = [];
    for (let i = 0; i < level.length; i += 2) {
      const [l, r] = [level[i], level[i + 1]];
      if (l !== undefined) next.push(r === undefined ? l : hashing.node(l, r));
    }
    level = next;
  }
  return ok(level[0] ?? hashing.empty);
}
