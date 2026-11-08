/**
 * The dilf4s Merkle tree (docs/specs/merkle.md): RFC 9162 inclusion and consistency verification over the tagged
 * profile (any core hash algorithm) or the RFC 6962 profile, proof decoding from canonical values, and the JLVM's
 * `merkle_verify` / `merkle_consistency_verify` over a trusted head `{root, size}` (jlvm.md section 6).
 *
 * Every exported function checks its arguments once, in O(1) each (an array of digests: once per digest), before it
 * reads them: an argument of the wrong type is core's `TypeMismatch` (check.ts). Caller bytes are read through a plain
 * view of their own memory, and what a proof keeps is copied; the walks run on that plain, owned data.
 */
import { describeFailure, err, models, ok, type HashAlgorithm, type Result, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { bytesLength, digestOf, hashPartsOf, MAX_OUTPUT_SIZE, mint, outputSizeOf, ownBytes, ownCopy, readGuarded, toHexOf, UNBOUNDED } from "@kd5ujc/dilf4s-core/internal";

import { bytesEqual, ConstantTag, copyBytes } from "./bytes.js";
import { algorithmArg, bigintArg, bytesArg, expected, expectedAt, isObject, NOT_A_BIGINT, NOT_BYTES, readArray, TOKEN, valueArg, type Token } from "./check.js";
import { bitsOf, decodeInput, MAX_PROOF_WEIGHT } from "./input.js";

declare const INT64: unique symbol;

/**
 * A refusal's size or index: 64-bit, as Scala's `Long`. The brand's symbol is not exported, so only this module's
 * saturating `int64` makes one, and a caller's wider value is never kept whole (docs/specs/conformance.md section 8).
 */
export type Int64 = bigint & { readonly [INT64]: true };

/** Why a Merkle proof could not be checked, by Scala's `MerkleError` names. */
export type MerkleError =
  | { readonly kind: "IndexOutOfRange"; readonly index: Int64; readonly size: Int64 }
  | { readonly kind: "InvalidSizes"; readonly oldSize: Int64; readonly newSize: Int64 }
  | { readonly kind: "InvalidProof"; readonly reason: string }
  | { readonly kind: "RootMismatch"; readonly expected: string; readonly computed: string }
  | { readonly kind: "Encoding"; readonly reason: string }
  /** A claimed leaf over `limit` (`MAX_PROOF_WEIGHT`, or this SDK's commitment ceiling), refused before it is hashed. */
  | { readonly kind: "TooLarge"; readonly limit: number };

/** A Merkle refusal, or an argument of the wrong type: what every exported function here may refuse with. */
type Refusal = MerkleError | TypeMismatch;

/** The largest tree size or index: the schema's `tree-index`. */
export const MAX_TREE_INDEX = models.limits.MERKLE_MAX_TREE_INDEX;

export const LEAF_TAG = "dilf4s/merkle/leaf/v1";
export const NODE_TAG = "dilf4s/merkle/node/v1";
export const EMPTY_TAG = "dilf4s/merkle/empty/v1";

/**
 * The hash functions of a Merkle profile (merkle.md section 2). `L` is what a leaf is given as: a canonical value for the
 * tagged profile (committed by `commit(v)`), raw bytes for RFC 6962. The tagged profile has no raw-bytes leaf, so a
 * proof of the bytes `0x6161` never verifies the text `"a"`; an already-computed leaf hash is the low-level input. The
 * two profiles are this module's classes, and every function here takes only them: another object is a
 * `TypeMismatch`, so no caller code computes a node or a size a proof is checked with.
 */
export interface MerkleHashing<L> {
  readonly profile: "tagged" | "rfc6962";
  readonly algorithm: HashAlgorithm;
  /** The digest size in bytes. */
  readonly size: number;
  /** The root of the empty tree (a copy). */
  readonly empty: Uint8Array;
  /** A leaf's hash; a tagged leaf whose commitment is over `cbor.MAX_ENCODED_BYTES` is `TooLarge`. */
  leaf(input: L): Result<Uint8Array, Refusal>;
  /** A leaf's weight as a claim (a value's weight; raw bytes weigh one more than their length). */
  leafWeight(input: L): Result<bigint, TypeMismatch>;
  /** The hash of a node over two digests. */
  node(left: Uint8Array, right: Uint8Array): Result<Uint8Array, TypeMismatch>;
}

/** One of this module's profiles: the only hashing a function here computes with. */
type Profile = TaggedMerkleHashing | Rfc6962MerkleHashing;

const TAGGED = mint();
const RFC = mint();
const INCLUSION = mint();
const CONSISTENCY = mint();

let isTagged!: (x: unknown) => x is TaggedMerkleHashing;
let taggedNode!: (h: TaggedMerkleHashing, left: Uint8Array, right: Uint8Array) => Uint8Array;
let taggedLeaf!: (h: TaggedMerkleHashing, value: Value) => Result<Uint8Array, MerkleError>;
let taggedEmpty!: (h: TaggedMerkleHashing) => Uint8Array;
let isRfc!: (x: unknown) => x is Rfc6962MerkleHashing;

const NOT_A_PROFILE = expected("a TaggedMerkleHashing or an Rfc6962MerkleHashing");

/** `x` if it is one of this module's profiles: a private-brand check, so no Proxy trap or caller code runs. */
const profileArg = (x: unknown): Result<Profile, TypeMismatch> => (isTagged(x) || isRfc(x) ? ok(x) : err(NOT_A_PROFILE));

/** A node's hash under a profile, unchecked. */
const nodeOf = (h: Profile, left: Uint8Array, right: Uint8Array): Uint8Array =>
  h.profile === "tagged" ? taggedNode(h, left, right) : hashPartsOf(h.algorithm, [Uint8Array.of(1), left, right]);

/** The empty tree's root under a profile, owned by the caller of this function. */
const emptyOf = (h: Profile): Uint8Array => (h.profile === "tagged" ? taggedEmpty(h) : digestOf(h.algorithm, new Uint8Array(0)));

/** A leaf as its profile takes it: a canonical value (tagged), or raw bytes (a plain view), and its weight. */
interface Leaf {
  readonly input: Value | Uint8Array;
  readonly weight: bigint;
}

/** The longest raw leaf a verification takes: its weight (its length plus one) at most `MAX_PROOF_WEIGHT`. */
const MAX_RAW_LEAF = Number(MAX_PROOF_WEIGHT) - 1;

function leafArg(h: Profile, input: unknown): Result<Leaf, Refusal> {
  if (h.profile === "tagged") {
    const v = valueArg(input);
    return v.ok ? ok({ input: v.value, weight: v.value.weight }) : v;
  }
  // within the weight bound: a longer leaf is refused by its length as the weight check words it, uncopied
  const b = ownBytes(input, MAX_RAW_LEAF);
  if (b === undefined) return err(NOT_BYTES);
  return typeof b === "number" ? err({ kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) }) : ok({ input: b, weight: BigInt(b.length) + 1n });
}

/** A checked leaf's hash. */
const leafOf = (h: Profile, leaf: Leaf): Result<Uint8Array, MerkleError> =>
  h.profile === "tagged" ? taggedLeaf(h, leaf.input as Value) : ok(hashPartsOf(h.algorithm, [Uint8Array.of(0), leaf.input as Uint8Array]));

/** Two digests checked as bytes, and hashed as a node. */
function checkedNode(h: Profile, left: unknown, right: unknown): Result<Uint8Array, TypeMismatch> {
  // digests, within the profile's size: a longer one is refused uncopied
  const l = ownBytes(left, h.size);
  const r = ownBytes(right, h.size);
  if (l === undefined || r === undefined) return err(NOT_BYTES);
  return typeof l === "number" || typeof r === "number" ? err(expected(`digests of at most ${h.size.toString()} bytes`)) : ok(nodeOf(h, l, r));
}

/** The dilf4s profile: tagged hashes per role, leaves committed by `commit(v)`. */
export class TaggedMerkleHashing implements MerkleHashing<Value> {
  readonly profile = "tagged";
  readonly algorithm: HashAlgorithm;
  readonly size: number;
  readonly #leaf: ConstantTag;
  readonly #node: ConstantTag;
  readonly #empty: Uint8Array;
  private constructor(token: Token, algorithm: HashAlgorithm) {
    // without the token, nothing of the caller's is read or kept: its algorithm is neither hashed with nor reported,
    // and every method refuses the unbranded receiver
    const a: HashAlgorithm = token === TOKEN ? algorithm : "SHA-256";
    this.algorithm = a;
    this.size = outputSizeOf(a);
    this.#leaf = new ConstantTag(a, LEAF_TAG);
    this.#node = new ConstantTag(a, NODE_TAG);
    this.#empty = new ConstantTag(a, EMPTY_TAG).hash(new Uint8Array(0));
    if (token === TOKEN) TAGGED.stamp(this);
    Object.freeze(this);
  }

  static {
    isTagged = (x): x is TaggedMerkleHashing => TAGGED.has(x);
    taggedNode = (h, left, right) => h.#node.hashParts([left, right]);
    taggedLeaf = (h, value) => {
      const c = h.#leaf.commitment(value);
      if (c.ok) return c;
      // the value is checked first, so the commitment can only be too large
      return err(c.error.kind === "TooLarge" ? { kind: "TooLarge", limit: c.error.limit } : { kind: "Encoding", reason: c.error.reason });
    };
    taggedEmpty = (h) => copyBytes(h.#empty);
  }

  /** The profile over `algorithm`; one that is not a `HashAlgorithm` (from a JavaScript caller) is a `TypeMismatch`. */
  static of(algorithm: HashAlgorithm): Result<TaggedMerkleHashing, TypeMismatch> {
    const a = algorithmArg(algorithm);
    return a.ok ? ok(new TaggedMerkleHashing(TOKEN, a.value)) : a;
  }

  get empty(): Uint8Array {
    return copyBytes(this.#empty);
  }

  /**
   * A value leaf's hash: the leaf hash of `commit(value)` (`MerkleTree.leaf`). O(min(weight, the ceiling)), as
   * `valueCommitment`: a commitment over `cbor.MAX_ENCODED_BYTES` (2^31 - 9, this SDK's ceiling; Scala and Rust hash
   * without one) is `TooLarge`, and what is not a canonical value is a `TypeMismatch`. The commitment is streamed into
   * the hash, never built.
   */
  leaf(value: Value): Result<Uint8Array, Refusal> {
    if (!isTagged(this)) return err(NOT_A_PROFILE);
    const v = valueArg(value);
    return v.ok ? taggedLeaf(this, v.value) : v;
  }

  /** A value's weight; what is not a canonical value is a `TypeMismatch`. */
  leafWeight(value: Value): Result<bigint, TypeMismatch> {
    if (!isTagged(this)) return err(NOT_A_PROFILE);
    const v = valueArg(value);
    return v.ok ? ok(v.value.weight) : v;
  }

  /** A node's hash; digests that are not `Uint8Array`s are a `TypeMismatch`. */
  node(left: Uint8Array, right: Uint8Array): Result<Uint8Array, TypeMismatch> {
    return isTagged(this) ? checkedNode(this, left, right) : err(NOT_A_PROFILE);
  }
}

/** RFC 6962's one-byte prefixes, for external logs: leaves are raw bytes. */
export class Rfc6962MerkleHashing implements MerkleHashing<Uint8Array> {
  readonly profile = "rfc6962";
  readonly algorithm: HashAlgorithm;
  readonly size: number;
  private constructor(token: Token, algorithm: HashAlgorithm) {
    // without the token, nothing of the caller's is kept: its algorithm is neither hashed with nor reported, and every
    // method refuses the unbranded receiver
    const a: HashAlgorithm = token === TOKEN ? algorithm : "SHA-256";
    this.algorithm = a;
    this.size = outputSizeOf(a);
    if (token === TOKEN) RFC.stamp(this);
    Object.freeze(this);
  }

  static {
    isRfc = (x): x is Rfc6962MerkleHashing => RFC.has(x);
  }

  /** The profile over `algorithm`; one that is not a `HashAlgorithm` (from a JavaScript caller) is a `TypeMismatch`. */
  static of(algorithm: HashAlgorithm): Result<Rfc6962MerkleHashing, TypeMismatch> {
    const a = algorithmArg(algorithm);
    return a.ok ? ok(new Rfc6962MerkleHashing(TOKEN, a.value)) : a;
  }

  get empty(): Uint8Array {
    return digestOf(this.algorithm, new Uint8Array(0));
  }

  /** A raw leaf's hash, `H(0x00 || data)`; what is not a `Uint8Array` is a `TypeMismatch`. */
  leaf(data: Uint8Array): Result<Uint8Array, Refusal> {
    if (!isRfc(this)) return err(NOT_A_PROFILE);
    // a raw leaf of any length is hashed
    const b = bytesArg(data, UNBOUNDED);
    return b.ok ? ok(hashPartsOf(this.algorithm, [Uint8Array.of(0), b.value])) : b;
  }

  /** Raw bytes weigh one more than their length (their own, never their `length` property's). */
  leafWeight(data: Uint8Array): Result<bigint, TypeMismatch> {
    if (!isRfc(this)) return err(NOT_A_PROFILE);
    // measured only, by its internal length: nothing of it is read
    const n = bytesLength(data);
    return n === undefined ? err(NOT_BYTES) : ok(BigInt(n) + 1n);
  }

  /** A node's hash, `H(0x01 || left || right)`; digests that are not `Uint8Array`s are a `TypeMismatch`. */
  node(left: Uint8Array, right: Uint8Array): Result<Uint8Array, TypeMismatch> {
    return isRfc(this) ? checkedNode(this, left, right) : err(NOT_A_PROFILE);
  }
}

const withinIndex = (n: bigint): boolean => n >= 0n && n <= MAX_TREE_INDEX;

const INT64_MAX = (1n << 63n) - 1n;
// the one way to an Int64: a wider caller value is saturated
const int64 = (n: bigint): Int64 => (n > INT64_MAX ? INT64_MAX : n < -INT64_MAX - 1n ? -INT64_MAX - 1n : n) as Int64;

/**
 * A caller's path: an array of `Uint8Array`s, read by index, each copied from its own memory. With `width` (the
 * profile's digest size, for a caller that checks every digest's size before using any), a digest of another size is
 * kept as its plain view, never copied, and refused by that check: an oversized one costs O(1), not a copy.
 */
const pathArg = (path: unknown, width?: number): Result<Uint8Array[], TypeMismatch | MerkleError> =>
  readArray<Uint8Array, TypeMismatch | MerkleError>(path, "an array of Uint8Arrays", (d, i) => {
    if (width === undefined) {
      // a proof's digest, sized when the proof is checked: none is longer than the longest digest, so a longer one is
      // refused here, uncopied
      const own = ownCopy(d, MAX_OUTPUT_SIZE);
      if (typeof own === "number") return err(expectedAt(`a digest of at most ${MAX_OUTPUT_SIZE.toString()} bytes`, i));
      return own === undefined ? err(expectedAt("a Uint8Array", i)) : ok(own);
    }
    // measured before it is copied: a digest longer than the profile's is refused here, uncopied, as `digests` words it
    const own = ownCopy(d, width);
    if (own === undefined) return err(expectedAt("a Uint8Array", i));
    return typeof own === "number" ? err({ kind: "Encoding", reason: `path[${i.toString()}] is ${own.toString()} bytes, not ${width.toString()}` }) : ok(own);
  });

let isInclusion!: (x: unknown) => x is InclusionProof;
let createInclusion!: (index: bigint, size: bigint, path: readonly Uint8Array[]) => InclusionProof;
let inclusionPath!: (p: InclusionProof) => readonly Uint8Array[];

/** An audit path for leaf `index` of a tree of `size` leaves, `0 <= index < size`. */
export class InclusionProof {
  readonly #path: readonly Uint8Array[];
  private constructor(
    token: Token,
    readonly index: bigint,
    readonly size: bigint,
    path: readonly Uint8Array[],
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) [this.index, this.size] = [0n, 0n];
    this.#path = token === TOKEN ? Object.freeze(path) : [];
    if (token === TOKEN) INCLUSION.stamp(this);
    Object.freeze(this);
  }

  static {
    isInclusion = (x): x is InclusionProof => INCLUSION.has(x);
    createInclusion = (index, size, path) => new InclusionProof(TOKEN, index, size, path);
    inclusionPath = (p) => p.#path;
  }

  /** The path's digests, leaf side first (copies). */
  get path(): readonly Uint8Array[] {
    return this.#path.map(copyBytes);
  }

  /**
   * The proof of leaf `index` in a tree of `size`; indices out of order or range are `IndexOutOfRange`. An index or size
   * that is not a `bigint`, or a path that is not an array of `Uint8Array`s, is a `TypeMismatch`; the path's digests
   * are copied from their own memory, and sized when the proof is checked.
   */
  static of(index: bigint, size: bigint, path: readonly Uint8Array[]): Result<InclusionProof, Refusal> {
    const i = bigintArg(index);
    if (!i.ok) return i;
    const s = bigintArg(size);
    if (!s.ok) return s;
    const p = pathArg(path);
    return p.ok ? inclusionOf(i.value, s.value, p.value) : p;
  }

  /**
   * A proof from its canonical value (`merkle-inclusion`), its digests sized for `hashing`. A value heavier than
   * `MAX_PROOF_WEIGHT` is `Encoding`, refused before it is decoded; what is not a profile or a value is a `TypeMismatch`.
   */
  static decode(hashing: MerkleHashing<unknown>, value: Value): Result<InclusionProof, Refusal> {
    const h = profileArg(hashing);
    if (!h.ok) return h;
    const v = valueArg(value);
    return v.ok ? decodeInclusion(h.value, v.value) : v;
  }

  /** The canonical value (`merkle-inclusion`). */
  encode(): Result<Value, MerkleError> {
    const v = models.merkle.MerkleInclusion.encode({ index: this.index, size: this.size, path: this.#path });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

/** {@link InclusionProof.of} over checked arguments, the path owned (kept as it is). */
const inclusionOf = (index: bigint, size: bigint, path: readonly Uint8Array[]): Result<InclusionProof, MerkleError> =>
  index >= 0n && index < size && withinIndex(size)
    ? ok(createInclusion(index, size, path))
    : err({ kind: "IndexOutOfRange", index: int64(index), size: int64(size) });

function decodeInclusion(h: Profile, value: Value): Result<InclusionProof, MerkleError> {
  const w = decodeInput(models.merkle.MerkleInclusion, value);
  if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
  const sized = digests(h, w.value.path);
  return sized.ok ? inclusionOf(w.value.index, w.value.size, w.value.path.map(copyBytes)) : sized;
}

let isConsistency!: (x: unknown) => x is ConsistencyProof;
let createConsistency!: (oldSize: bigint, newSize: bigint, path: readonly Uint8Array[]) => ConsistencyProof;
let consistencyPath!: (p: ConsistencyProof) => readonly Uint8Array[];

/** A consistency path between tree sizes `0 <= oldSize <= newSize`. */
export class ConsistencyProof {
  readonly #path: readonly Uint8Array[];
  private constructor(
    token: Token,
    readonly oldSize: bigint,
    readonly newSize: bigint,
    path: readonly Uint8Array[],
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== TOKEN) [this.oldSize, this.newSize] = [0n, 0n];
    this.#path = token === TOKEN ? Object.freeze(path) : [];
    if (token === TOKEN) CONSISTENCY.stamp(this);
    Object.freeze(this);
  }

  static {
    isConsistency = (x): x is ConsistencyProof => CONSISTENCY.has(x);
    createConsistency = (oldSize, newSize, path) => new ConsistencyProof(TOKEN, oldSize, newSize, path);
    consistencyPath = (p) => p.#path;
  }

  get path(): readonly Uint8Array[] {
    return this.#path.map(copyBytes);
  }

  /**
   * Sizes out of order, or outside the tree-index range, are `InvalidSizes`, as in Scala and the verifier. Sizes that
   * are not `bigint`s, or a path that is not an array of `Uint8Array`s, are a `TypeMismatch`; the path is copied.
   */
  static of(oldSize: bigint, newSize: bigint, path: readonly Uint8Array[]): Result<ConsistencyProof, Refusal> {
    const o = bigintArg(oldSize);
    if (!o.ok) return o;
    const n = bigintArg(newSize);
    if (!n.ok) return n;
    const p = pathArg(path);
    return p.ok ? consistencyOf(o.value, n.value, p.value) : p;
  }

  /**
   * A proof from its canonical value (`merkle-consistency`), its digests sized for `hashing`. A value heavier than
   * `MAX_PROOF_WEIGHT` is `Encoding`, refused before it is decoded; what is not a profile or a value is a `TypeMismatch`.
   */
  static decode(hashing: MerkleHashing<unknown>, value: Value): Result<ConsistencyProof, Refusal> {
    const h = profileArg(hashing);
    if (!h.ok) return h;
    const v = valueArg(value);
    return v.ok ? decodeConsistency(h.value, v.value) : v;
  }

  encode(): Result<Value, MerkleError> {
    const v = models.merkle.MerkleConsistency.encode({ old: this.oldSize, new: this.newSize, path: this.#path });
    return v.ok ? v : err({ kind: "Encoding", reason: v.error.kind });
  }
}

/** {@link ConsistencyProof.of} over checked arguments, the path owned (kept as it is). */
const consistencyOf = (oldSize: bigint, newSize: bigint, path: readonly Uint8Array[]): Result<ConsistencyProof, MerkleError> =>
  oldSize >= 0n && oldSize <= newSize && withinIndex(newSize)
    ? ok(createConsistency(oldSize, newSize, path))
    : err({ kind: "InvalidSizes", oldSize: int64(oldSize), newSize: int64(newSize) });

function decodeConsistency(h: Profile, value: Value): Result<ConsistencyProof, MerkleError> {
  const w = decodeInput(models.merkle.MerkleConsistency, value);
  if (!w.ok) return err({ kind: "Encoding", reason: describeFailure(w.error) });
  const sized = digests(h, w.value.path);
  return sized.ok ? consistencyOf(w.value.old, w.value.new, w.value.path.map(copyBytes)) : sized;
}

/** The refusal of a digest longer than the profile's, measured at the boundary and never copied: as {@link sized} words it. */
const longer = (what: string, length: number, size: number): Result<never, MerkleError> => err({ kind: "Encoding", reason: `${what} is ${length.toString()} bytes, not ${size.toString()}` });

function sized(h: Profile, what: string, d: Uint8Array): Result<undefined, MerkleError> {
  return d.length === h.size ? ok(undefined) : err({ kind: "Encoding", reason: `${what} is ${d.length.toString()} bytes, not ${h.size.toString()}` });
}

function digests(h: Profile, path: readonly Uint8Array[]): Result<undefined, MerkleError> {
  for (const [i, d] of path.entries()) {
    const s = sized(h, `path[${i.toString()}]`, d);
    if (!s.ok) return s;
  }
  return ok(undefined);
}

const invalid = (reason: string): Result<never, MerkleError> => err({ kind: "InvalidProof", reason });
const mismatch = (expected: Uint8Array, computed: Uint8Array): Result<never, MerkleError> => err({ kind: "RootMismatch", expected: toHexOf(expected), computed: toHexOf(computed) });

/** Right-shifts both until `fn` is odd or zero. */
function shiftUntilOddOrZero(fn: bigint, sn: bigint): readonly [bigint, bigint] {
  let [f, s] = [fn, sn];
  while ((f & 1n) === 0n && f !== 0n) [f, s] = [f >> 1n, s >> 1n];
  return [f, s];
}

const NOT_AN_INCLUSION = expected("an InclusionProof");
const NOT_A_CONSISTENCY = expected("a ConsistencyProof");

/**
 * RFC 9162 section 2.1.3.2: whether `leafHash` is leaf `proof.index` of the tree of `proof.size` leaves with `root`. A
 * well-formed proof that fails is `InvalidProof` or `RootMismatch`; digests of the wrong size are `Encoding`. What is
 * not a profile, a `Uint8Array` or an `InclusionProof` is a `TypeMismatch`.
 */
export function checkInclusion(hashing: MerkleHashing<unknown>, root: Uint8Array, leafHash: Uint8Array, proof: InclusionProof): Result<undefined, Refusal> {
  // each check allocates nothing but the two digests' plain views; a refusal is a shared constant
  if (!isTagged(hashing) && !isRfc(hashing)) return err(NOT_A_PROFILE);
  // within the profile's digest size: a longer root or leaf hash is refused as the size check words it, uncopied
  const r = ownBytes(root, hashing.size);
  const l = ownBytes(leafHash, hashing.size);
  if (r === undefined || l === undefined) return err(NOT_BYTES);
  if (typeof r === "number") return longer("root", r, hashing.size);
  if (typeof l === "number") return longer("leaf hash", l, hashing.size);
  return isInclusion(proof) ? inclusionCheck(hashing, r, l, proof) : err(NOT_AN_INCLUSION);
}

function inclusionCheck(h: Profile, root: Uint8Array, leafHash: Uint8Array, proof: InclusionProof): Result<undefined, MerkleError> {
  const path = inclusionPath(proof);
  const lengths = [sized(h, "root", root), sized(h, "leaf hash", leafHash), digests(h, path)].find((r) => !r.ok);
  if (lengths !== undefined) return lengths;
  if (proof.index < 0n || proof.index >= proof.size) return err({ kind: "IndexOutOfRange", index: int64(proof.index), size: int64(proof.size) });
  let fn = proof.index;
  let sn = proof.size - 1n;
  let r = leafHash;
  for (const p of path) {
    if (sn === 0n) return invalid("path is too long for the tree size");
    if ((fn & 1n) === 1n || fn === sn) {
      [fn, sn] = (fn & 1n) === 0n ? shiftUntilOddOrZero(fn, sn) : [fn, sn];
      r = nodeOf(h, p, r);
    } else r = nodeOf(h, r, p);
    [fn, sn] = [fn >> 1n, sn >> 1n];
  }
  if (sn !== 0n) return invalid("path is too short for the tree size");
  return bytesEqual(r, root) ? ok(undefined) : mismatch(root, r);
}

/**
 * RFC 9162 section 2.1.4.2: whether the tree of `oldRoot` is a prefix of the tree of `newRoot`. What is not a profile,
 * a `Uint8Array` or a `ConsistencyProof` is a `TypeMismatch`.
 */
export function checkConsistency(hashing: MerkleHashing<unknown>, oldRoot: Uint8Array, newRoot: Uint8Array, proof: ConsistencyProof): Result<undefined, Refusal> {
  if (!isTagged(hashing) && !isRfc(hashing)) return err(NOT_A_PROFILE);
  const o = ownBytes(oldRoot, hashing.size);
  const n = ownBytes(newRoot, hashing.size);
  if (o === undefined || n === undefined) return err(NOT_BYTES);
  if (typeof o === "number") return longer("old root", o, hashing.size);
  if (typeof n === "number") return longer("new root", n, hashing.size);
  return isConsistency(proof) ? consistencyCheck(hashing, o, n, proof) : err(NOT_A_CONSISTENCY);
}

function consistencyCheck(h: Profile, oldRoot: Uint8Array, newRoot: Uint8Array, proof: ConsistencyProof): Result<undefined, MerkleError> {
  const given = consistencyPath(proof);
  const lengths = [sized(h, "old root", oldRoot), sized(h, "new root", newRoot), digests(h, given)].find((r) => !r.ok);
  if (lengths !== undefined) return lengths;
  const [m, n] = [proof.oldSize, proof.newSize];
  if (m < 0n || m > n) return err({ kind: "InvalidSizes", oldSize: int64(m), newSize: int64(n) });
  if (m === 0n) return given.length === 0 && bytesEqual(oldRoot, emptyOf(h)) ? ok(undefined) : invalid("size 0 needs an empty proof and the empty root");
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
      fr = nodeOf(h, c, fr);
      sr = nodeOf(h, c, sr);
    } else sr = nodeOf(h, sr, c);
    [fn, sn] = [fn >> 1n, sn >> 1n];
  }
  if (sn !== 0n) return invalid("path is too short for the sizes");
  if (!bytesEqual(fr, oldRoot)) return mismatch(oldRoot, fr);
  return bytesEqual(sr, newRoot) ? ok(undefined) : mismatch(newRoot, sr);
}

/** A failed check that is a well-formed proof of something else is `false`; the other failures are errors. */
function outcome(r: Result<undefined, Refusal>): Result<boolean, Refusal> {
  if (r.ok) return ok(true);
  return r.error.kind === "InvalidProof" || r.error.kind === "RootMismatch" ? ok(false) : r;
}

/** {@link checkInclusion} as a verdict: `InvalidProof` and `RootMismatch` are `false`. */
export function verifyInclusion(hashing: MerkleHashing<unknown>, root: Uint8Array, leafHash: Uint8Array, proof: InclusionProof): Result<boolean, Refusal> {
  return outcome(checkInclusion(hashing, root, leafHash, proof));
}

/** {@link checkConsistency} as a verdict: `InvalidProof` and `RootMismatch` are `false`. */
export function verifyConsistency(hashing: MerkleHashing<unknown>, oldRoot: Uint8Array, newRoot: Uint8Array, proof: ConsistencyProof): Result<boolean, Refusal> {
  return outcome(checkConsistency(hashing, oldRoot, newRoot, proof));
}

/** A trusted tree head: a root and the size of the tree it is the root of, taken together. */
export interface MerkleHead {
  readonly root: Uint8Array;
  readonly size: bigint;
}

/**
 * A caller's head, its fields read once each: an object whose `root` is a `Uint8Array` (copied) and whose `size`
 * is a `bigint`, or a `TypeMismatch`.
 */
function headArg(h: Profile, head: unknown, what: string): Result<MerkleHead, TypeMismatch | MerkleError> {
  if (!isObject(head)) return err(expected(`${what} to be a MerkleHead`));
  // copied as read: the size's getter, or the other head's, may run caller code that changes or detaches it; one
  // longer than the profile's digest is refused uncopied, as checkHead words it
  const root = ownCopy(Reflect.get(head, "root"), h.size);
  if (root === undefined) return err(expected(`${what}.root to be a Uint8Array`));
  if (typeof root === "number") return err({ kind: "Encoding", reason: `${what}.root is ${root.toString()} bytes, not ${h.size.toString()}` });
  // guarded: the root's getter may have revoked the head; REVOKED is no bigint
  const size: unknown = readGuarded(head, "size");
  return typeof size === "bigint" ? ok({ root, size }) : err(NOT_A_BIGINT);
}

function checkHead(h: Profile, what: string, head: MerkleHead): Result<undefined, MerkleError> {
  if (!withinIndex(head.size)) return err({ kind: "Encoding", reason: `${what}.size of ${bitsOf(head.size).toString()} bits is not a tree size` });
  return sized(h, `${what}.root`, head.root);
}

/**
 * The JLVM's `merkle_verify [head, leaf, proof]`: whether `leaf` is in the tree of the trusted `head`, with the proof as
 * its canonical value. The leaf is a value for the tagged profile and raw bytes for RFC 6962. A proof whose size is not
 * the head's is `false`: a smaller tree's root can satisfy a proof that claims a larger size (merkle.md section 3).
 * Every argument's type is checked first (the head's fields read once each), then the head, then the leaf's weight.
 */
export function merkleVerify<L>(hashing: MerkleHashing<L>, head: MerkleHead, leaf: L, proof: Value): Result<boolean, Refusal> {
  const h = profileArg(hashing);
  if (!h.ok) return h;
  const hd = headArg(h.value, head, "head");
  if (!hd.ok) return hd;
  const l = leafArg(h.value, leaf);
  if (!l.ok) return l;
  const v = valueArg(proof);
  if (!v.ok) return v;
  const c = checkHead(h.value, "head", hd.value);
  if (!c.ok) return c;
  // the leaf is a claim: bounded by weight before it is hashed and before the proof is decoded
  if (l.value.weight > MAX_PROOF_WEIGHT) return err({ kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) });
  const p = decodeInclusion(h.value, v.value);
  if (!p.ok) return p;
  if (p.value.size !== hd.value.size) return ok(false);
  const lh = leafOf(h.value, l.value);
  return lh.ok ? outcome(inclusionCheck(h.value, hd.value.root, lh.value, p.value)) : lh;
}

/**
 * The JLVM's `merkle_consistency_verify [oldHead, newHead, proof]`: sizes that differ from the heads' are `false`.
 * Every argument's type is checked first, each head's fields read once.
 */
export function merkleConsistencyVerify(hashing: MerkleHashing<unknown>, oldHead: MerkleHead, newHead: MerkleHead, proof: Value): Result<boolean, Refusal> {
  const h = profileArg(hashing);
  if (!h.ok) return h;
  const oh = headArg(h.value, oldHead, "oldHead");
  if (!oh.ok) return oh;
  const nh = headArg(h.value, newHead, "newHead");
  if (!nh.ok) return nh;
  const v = valueArg(proof);
  if (!v.ok) return v;
  const o = checkHead(h.value, "oldHead", oh.value);
  if (!o.ok) return o;
  const n = checkHead(h.value, "newHead", nh.value);
  if (!n.ok) return n;
  const p = decodeConsistency(h.value, v.value);
  if (!p.ok) return p;
  if (p.value.oldSize !== oh.value.size || p.value.newSize !== nh.value.size) return ok(false);
  return outcome(consistencyCheck(h.value, oh.value.root, nh.value.root, p.value));
}

/**
 * The root of the tree over `leafHashes` (merkle.md section 1): pair level by level, promoting an unpaired last node.
 * What is not a profile, or not an array of `Uint8Array`s (read by index, each copied), is a `TypeMismatch`.
 */
export function merkleRoot(hashing: MerkleHashing<unknown>, leafHashes: readonly Uint8Array[]): Result<Uint8Array, Refusal> {
  const h = profileArg(hashing);
  if (!h.ok) return h;
  // each leaf hash owned if it has the digest's size; one of another size is refused below, uncopied
  const given = pathArg(leafHashes, h.value.size);
  if (!given.ok) return given;
  const lengths = digests(h.value, given.value);
  if (!lengths.ok) return lengths;
  let level = given.value;
  if (level.length === 0) return ok(emptyOf(h.value));
  while (level.length > 1) {
    const next: Uint8Array[] = [];
    for (let i = 0; i < level.length; i += 2) {
      const [l, r] = [level[i], level[i + 1]];
      if (l !== undefined) next.push(r === undefined ? l : nodeOf(h.value, l, r));
    }
    level = next;
  }
  return ok(level[0] ?? emptyOf(h.value));
}
