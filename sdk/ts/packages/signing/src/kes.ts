/**
 * Key-evolving signatures (`kes.md`): the dilf4s sum and product compositions over a hash `H`, and Cardano's SumKES.
 * Keys are immutable values: evolving returns a new key. Their bytes never print.
 */
import { err, ok, outputSize, type HashAlgorithm, type Result } from "@kd5ujc/dilf4s-core";
import { ed25519 as noble } from "@noble/curves/ed25519.js";
import { blake2b } from "@noble/hashes/blake2.js";

import { verifyLibsodiumRule, verifyZip215 } from "./ed25519.js";
import type { KesError } from "./errors.js";
import { INSPECT } from "./keys.js";
import { INTERNAL, type Internal } from "./internal/token.js";
import { concat, copy, equal, isZero } from "./internal/bytes.js";
import { literalTaggedHash, TAGS } from "./internal/tags.js";

const ZERO32 = new Uint8Array(32);
const leafVk = (seed: Uint8Array): Uint8Array => noble.getPublicKey(seed);
const leafSign = (seed: Uint8Array, message: Uint8Array): Uint8Array => noble.sign(message, seed);

const malformed = (what: string, reason: string): KesError => ({ kind: "Malformed", what, reason });
const inconsistent = (reason: string): KesError => ({ kind: "InconsistentKey", reason });
const sizeError = (what: string, expected: number, got: number): KesError =>
  malformed(what, `expected ${String(expected)} bytes, got ${String(got)}`);
const outOfRange = (period: number, total: number): KesError => ({ kind: "PeriodOutOfRange", period, total });
const inRange = (period: number, total: number): boolean => Number.isInteger(period) && period >= 0 && period < total;

/** A secret KES key at a known period; `bytes` is its canonical encoding (a copy). */
abstract class KesKey {
  readonly #bytes: Uint8Array;
  protected constructor(
    bytes: Uint8Array,
    readonly period: number,
  ) {
    this.#bytes = copy(bytes);
  }

  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  abstract toString(): string;

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

// ------------------------------------------------------------------ the MMM sum tree over H

/** A sum-tree key: a leaf (its seed absent once erased), or a node with its active subtree, pending right seed and both roots. */
type Tree =
  | { readonly leaf: true; readonly seed: Uint8Array | undefined; readonly vk: Uint8Array }
  | { readonly leaf: false; readonly active: Tree; readonly rightSeed: Uint8Array | undefined; readonly left: Uint8Array; readonly right: Uint8Array };

const eraseActiveLeaf = (t: Tree): Tree =>
  t.leaf ? { leaf: true, seed: undefined, vk: t.vk } : { leaf: false, active: eraseActiveLeaf(t.active), rightSeed: t.rightSeed, left: t.left, right: t.right };

class MerkleKesOps {
  readonly n: number;
  readonly #seedHash: (m: Uint8Array) => Uint8Array;
  readonly #leafHash: (m: Uint8Array) => Uint8Array;
  readonly #nodeHash: (m: Uint8Array) => Uint8Array;
  readonly #productHash: (m: Uint8Array) => Uint8Array;

  constructor(hash: HashAlgorithm) {
    this.n = outputSize(hash);
    this.#seedHash = literalTaggedHash(hash, TAGS.kesSeed);
    this.#leafHash = literalTaggedHash(hash, TAGS.kesLeaf);
    this.#nodeHash = literalTaggedHash(hash, TAGS.kesNode);
    this.#productHash = literalTaggedHash(hash, TAGS.kesProduct);
  }

  /** The first 32 bytes of the tagged digests of `00 || seed` and `01 || seed`. */
  expand(seed: Uint8Array): readonly [Uint8Array, Uint8Array] {
    return [this.#seedHash(concat(Uint8Array.of(0), seed)).slice(0, 32), this.#seedHash(concat(Uint8Array.of(1), seed)).slice(0, 32)];
  }

  leafRoot(vk: Uint8Array): Uint8Array {
    return this.#leafHash(vk);
  }

  node(l: Uint8Array, r: Uint8Array): Uint8Array {
    return this.#nodeHash(concat(l, r));
  }

  /** The message a super tree signs to certify a sub-tree root. */
  productMessage(subRoot: Uint8Array): Uint8Array {
    return this.#productHash(subRoot);
  }

  root(t: Tree): Uint8Array {
    return t.leaf ? this.leafRoot(t.vk) : this.node(t.left, t.right);
  }

  gen(seed: Uint8Array, d: number): Tree {
    if (d === 0) return { leaf: true, seed, vk: leafVk(seed) };
    const [r0, r1] = this.expand(seed);
    const sub = this.gen(r0, d - 1);
    return { leaf: false, active: sub, rightSeed: r1, left: this.root(sub), right: this.root(this.gen(r1, d - 1)) };
  }

  /** `vk || sig || witnesses (bottom-up)`, or undefined once the active leaf is erased. */
  sign(t: Tree, d: number, period: number, message: Uint8Array): Uint8Array | undefined {
    if (t.leaf) return t.seed === undefined ? undefined : concat(t.vk, leafSign(t.seed, message));
    const half = 2 ** (d - 1);
    const inner = this.sign(t.active, d - 1, period % half, message);
    return inner === undefined ? undefined : concat(inner, period < half ? t.right : t.left);
  }

  /** The tree for `period + 1`; activating a right half regenerates it from its seed and checks its stored root. */
  evolve(t: Tree, d: number, period: number): Result<Tree, KesError> {
    if (t.leaf) return err({ kind: "Exhausted", period });
    const half = 2 ** (d - 1);
    if (period + 1 < half) {
      const a = this.evolve(t.active, d - 1, period);
      return a.ok ? ok({ ...t, active: a.value }) : a;
    }
    if (period + 1 === half) {
      const sub = t.rightSeed === undefined ? undefined : this.gen(t.rightSeed, d - 1);
      return sub !== undefined && equal(this.root(sub), t.right)
        ? ok({ leaf: false, active: sub, rightSeed: undefined, left: t.left, right: t.right })
        : err(inconsistent(`the right seed at height ${String(d)} does not match its root`));
    }
    const a = this.evolve(t.active, d - 1, period - half);
    return a.ok ? ok({ ...t, active: a.value, rightSeed: undefined }) : a;
  }

  signatureLength(d: number): number {
    return 96 + this.n * d;
  }

  verify(rootDigest: Uint8Array, d: number, period: number, message: Uint8Array, sig: Uint8Array): boolean {
    if (sig.length !== this.signatureLength(d) || !inRange(period, 2 ** d)) return false;
    const vk = sig.subarray(0, 32);
    let acc = this.leafRoot(vk);
    for (let level = 0; level < d; level++) {
      const w = sig.subarray(96 + this.n * level, 96 + this.n * (level + 1));
      acc = Math.floor(period / 2 ** level) % 2 === 0 ? this.node(acc, w) : this.node(w, acc);
    }
    return equal(acc, rootDigest) && verifyZip215(vk, message, sig.subarray(32, 96));
  }

  /** Leaf: `seed (zeros once erased) || vk`; node: `active || rightSeed (zeros once used) || left || right`. */
  encode(t: Tree): Uint8Array {
    return t.leaf ? concat(t.seed ?? ZERO32, t.vk) : concat(this.encode(t.active), t.rightSeed ?? ZERO32, t.left, t.right);
  }

  encodedLength(d: number): number {
    return 64 + (32 + 2 * this.n) * d;
  }

  /** A tree of height `d` at `period`: right seeds are zero exactly when used, and every active root is checked. */
  decode(b: Uint8Array, d: number, period: number, leafErased: boolean): Result<Tree, KesError> {
    if (d === 0) {
      const seed = b.slice(0, 32);
      const vk = b.slice(32, 64);
      const zero = isZero(seed);
      if (leafErased && zero) return ok({ leaf: true, seed: undefined, vk });
      // an active leaf is checked against its key, never by its bytes: an all-zero seed is a valid Ed25519 seed
      if (!leafErased && equal(leafVk(seed), vk)) return ok({ leaf: true, seed, vk });
      return err(inconsistent("leaf seed and verifying key disagree"));
    }
    const half = 2 ** (d - 1);
    const at = this.encodedLength(d - 1);
    const seed = b.slice(at, at + 32);
    const left = b.slice(at + 32, at + 32 + this.n);
    const right = b.slice(at + 32 + this.n, at + 32 + 2 * this.n);
    const active = this.decode(b.subarray(0, at), d - 1, period % half, leafErased);
    if (!active.ok) return active;
    if ((period < half) !== !isZero(seed))
      return err(inconsistent(`right seed state does not match period ${String(period)} at height ${String(d)}`));
    if (!equal(this.root(active.value), period < half ? left : right)) return err(inconsistent(`active subtree root mismatch at height ${String(d)}`));
    return ok({ leaf: false, active: active.value, rightSeed: period < half ? seed : undefined, left, right });
  }
}

function hashCheck(hash: HashAlgorithm): Result<HashAlgorithm, KesError> {
  return outputSize(hash) >= 32 ? ok(hash) : err(malformed("KES hash", `${hash} has an output below 32 bytes`));
}

function evolveTo<K extends KesKey>(key: K, target: number, total: number, evolve: (k: K) => Result<K, KesError>): Result<K, KesError> {
  if (!Number.isInteger(target) || target < key.period || target >= total) return err(outOfRange(target, total));
  let k = key;
  while (k.period < target) {
    const next = evolve(k);
    if (!next.ok) return next;
    k = next.value;
  }
  return ok(k);
}

/** A `KesSum` signing key at a known period. */
export class KesSumKey extends KesKey {
  readonly #ops: MerkleKesOps;
  readonly #tree: Tree;
  constructor(
    _token: Internal,
    readonly scheme: KesSum,
    ops: MerkleKesOps,
    tree: Tree,
    period: number,
  ) {
    super(ops.encode(tree), period);
    this.#ops = ops;
    this.#tree = tree;
    Object.freeze(this);
  }

  /** The root: fixed for the key's lifetime. */
  get verifyingKey(): Uint8Array {
    return this.#ops.root(this.#tree);
  }

  sign(message: Uint8Array): Result<Uint8Array, KesError> {
    const s = this.#ops.sign(this.#tree, this.scheme.height, this.period, message);
    return s === undefined ? err(inconsistent("the active leaf is erased")) : ok(s);
  }

  /** The key for the next period. */
  evolve(): Result<KesSumKey, KesError> {
    const t = this.#ops.evolve(this.#tree, this.scheme.height, this.period);
    if (!t.ok) return t.error.kind === "Exhausted" ? err({ kind: "Exhausted", period: this.period }) : t;
    return ok(new KesSumKey(INTERNAL, this.scheme, this.#ops, t.value, this.period + 1));
  }

  /** The key at `target` (at or after this key's period), evolving one period at a time. */
  evolveTo(target: number): Result<KesSumKey, KesError> {
    return evolveTo<KesSumKey>(this, target, this.scheme.totalPeriods, (k) => k.evolve());
  }

  toString(): string {
    return `KesSumKey(${this.scheme.hash}, height=${String(this.scheme.height)}, period=${String(this.period)}, <redacted>)`;
  }
}

/** The dilf4s sum composition over `H`: `2^height` periods, signatures `vk || Ed25519 sig || Merkle path`. */
export class KesSum {
  readonly #ops: MerkleKesOps;
  readonly totalPeriods: number;
  readonly signatureLength: number;
  readonly keyLength: number;

  private constructor(
    readonly hash: HashAlgorithm,
    readonly height: number,
  ) {
    this.#ops = new MerkleKesOps(hash);
    this.totalPeriods = 2 ** height;
    this.signatureLength = this.#ops.signatureLength(height);
    this.keyLength = this.#ops.encodedLength(height);
    Object.freeze(this);
  }

  /** A hash with at least 32 bytes of output, and a height of 0 to 30. */
  static of(hash: HashAlgorithm, height: number): Result<KesSum, KesError> {
    const h = hashCheck(hash);
    if (!h.ok) return h;
    return Number.isInteger(height) && height >= 0 && height <= 30
      ? ok(new KesSum(hash, height))
      : err(malformed("KES height", `expected 0..30, got ${String(height)}`));
  }

  /** The period-0 key of a 32-byte seed. */
  generate(seed: Uint8Array): Result<KesSumKey, KesError> {
    if (seed.length !== 32) return err(sizeError("KES seed", 32, seed.length));
    return ok(new KesSumKey(INTERNAL, this, this.#ops, this.#ops.gen(copy(seed), this.height), 0));
  }

  /** Loads an encoded key at the period the caller knows it to be at. */
  load(input: Uint8Array, period: number): Result<KesSumKey, KesError> {
    // decoding retains slices of the input; a private copy keeps the caller's buffer (a Buffer slices by view) apart
    const raw = copy(input);
    if (raw.length !== this.keyLength) return err(sizeError("KES key", this.keyLength, raw.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    const t = this.#ops.decode(raw, this.height, period, false);
    return t.ok ? ok(new KesSumKey(INTERNAL, this, this.#ops, t.value, period)) : t;
  }

  /** The scheme-level forms (as Scala and the Rust SDK); each delegates to the key, which carries its own scheme. */
  verifyingKey(key: KesSumKey): Uint8Array {
    return key.verifyingKey;
  }

  sign(key: KesSumKey, message: Uint8Array): Result<Uint8Array, KesError> {
    return key.sign(message);
  }

  evolve(key: KesSumKey): Result<KesSumKey, KesError> {
    return key.evolve();
  }

  evolveTo(key: KesSumKey, target: number): Result<KesSumKey, KesError> {
    return key.evolveTo(target);
  }

  /** Malformed sizes and out-of-range periods are errors; a well-formed signature verifies or not. */
  verify(vk: Uint8Array, period: number, message: Uint8Array, signature: Uint8Array): Result<boolean, KesError> {
    if (vk.length !== this.#ops.n) return err(sizeError("KES verifying key", this.#ops.n, vk.length));
    if (signature.length !== this.signatureLength) return err(sizeError("KES signature", this.signatureLength, signature.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    return ok(this.#ops.verify(vk, this.height, period, message, signature));
  }
}

// ------------------------------------------------------------------ product composition

interface ProductParts {
  readonly superTree: Tree;
  readonly subTree: Tree;
  readonly nextSeed: Uint8Array;
  readonly superSig: Uint8Array;
}

/** Certifies a fresh sub tree from a chain seed with the super tree at `superPeriod`, then erases the super leaf. */
function certify(scheme: KesProduct, ops: MerkleKesOps, superTree: Tree, superPeriod: number, chainSeed: Uint8Array, period: number): Result<KesProductKey, KesError> {
  const [subSeed, nextSeed] = ops.expand(chainSeed);
  const subTree = ops.gen(subSeed, scheme.subHeight);
  const superSig = ops.sign(superTree, scheme.superHeight, superPeriod, ops.productMessage(ops.root(subTree)));
  return superSig === undefined
    ? err(inconsistent("the super leaf is erased"))
    : ok(new KesProductKey(INTERNAL, scheme, ops, { superTree: eraseActiveLeaf(superTree), subTree, nextSeed, superSig }, period));
}

/** A `KesProduct` signing key: super tree (active leaf erased), sub tree, next chain seed, and the super signature. */
export class KesProductKey extends KesKey {
  readonly #ops: MerkleKesOps;
  readonly #parts: ProductParts;
  constructor(
    _token: Internal,
    readonly scheme: KesProduct,
    ops: MerkleKesOps,
    parts: ProductParts,
    period: number,
  ) {
    super(concat(ops.encode(parts.superTree), ops.encode(parts.subTree), parts.nextSeed, parts.superSig), period);
    this.#ops = ops;
    this.#parts = parts;
    Object.freeze(this);
  }

  /** The super tree's root: fixed for the key's lifetime. */
  get verifyingKey(): Uint8Array {
    return this.#ops.root(this.#parts.superTree);
  }

  /** `superSig || subRoot || subSig`. */
  sign(message: Uint8Array): Result<Uint8Array, KesError> {
    const { subTree, superSig } = this.#parts;
    const sub = this.#ops.sign(subTree, this.scheme.subHeight, this.period % 2 ** this.scheme.subHeight, message);
    return sub === undefined ? err(inconsistent("the sub leaf is erased")) : ok(concat(superSig, this.#ops.root(subTree), sub));
  }

  /** Within a sub tree, evolve it; at its end, evolve the super tree and certify the next sub tree. */
  evolve(): Result<KesProductKey, KesError> {
    const { superTree, subTree } = this.#parts;
    const subPeriods = 2 ** this.scheme.subHeight;
    const tSup = Math.floor(this.period / subPeriods);
    const tSub = this.period % subPeriods;
    if (tSub + 1 < subPeriods) {
      const s = this.#ops.evolve(subTree, this.scheme.subHeight, tSub);
      return s.ok ? ok(new KesProductKey(INTERNAL, this.scheme, this.#ops, { ...this.#parts, subTree: s.value }, this.period + 1)) : s;
    }
    const sup = this.#ops.evolve(superTree, this.scheme.superHeight, tSup);
    if (!sup.ok) return sup.error.kind === "Exhausted" ? err({ kind: "Exhausted", period: this.period }) : sup;
    return certify(this.scheme, this.#ops, sup.value, tSup + 1, this.#parts.nextSeed, this.period + 1);
  }

  evolveTo(target: number): Result<KesProductKey, KesError> {
    return evolveTo<KesProductKey>(this, target, this.scheme.totalPeriods, (k) => k.evolve());
  }

  toString(): string {
    const { hash, superHeight, subHeight } = this.scheme;
    return `KesProductKey(${hash}, heights=(${String(superHeight)},${String(subHeight)}), period=${String(this.period)}, <redacted>)`;
  }
}

/** The dilf4s product composition: a super sum tree certifies `2^superHeight` successive sub sum trees. */
export class KesProduct {
  readonly #ops: MerkleKesOps;
  readonly totalPeriods: number;
  readonly signatureLength: number;
  readonly keyLength: number;

  private constructor(
    readonly hash: HashAlgorithm,
    readonly superHeight: number,
    readonly subHeight: number,
  ) {
    this.#ops = new MerkleKesOps(hash);
    this.totalPeriods = 2 ** (superHeight + subHeight);
    this.signatureLength = this.#ops.signatureLength(superHeight) + this.#ops.n + this.#ops.signatureLength(subHeight);
    this.keyLength = this.#ops.encodedLength(superHeight) + this.#ops.encodedLength(subHeight) + 32 + this.#ops.signatureLength(superHeight);
    Object.freeze(this);
  }

  /** Non-negative heights summing to at most 30. */
  static of(hash: HashAlgorithm, superHeight: number, subHeight: number): Result<KesProduct, KesError> {
    const h = hashCheck(hash);
    if (!h.ok) return h;
    const valid = Number.isInteger(superHeight) && Number.isInteger(subHeight) && superHeight >= 0 && subHeight >= 0 && superHeight + subHeight <= 30;
    return valid
      ? ok(new KesProduct(hash, superHeight, subHeight))
      : err(malformed("KES heights", `expected non-negative heights summing to at most 30, got (${String(superHeight)}, ${String(subHeight)})`));
  }

  generate(seed: Uint8Array): Result<KesProductKey, KesError> {
    if (seed.length !== 32) return err(sizeError("KES seed", 32, seed.length));
    const [superSeed, chainSeed] = this.#ops.expand(seed);
    return certify(this, this.#ops, this.#ops.gen(superSeed, this.superHeight), 0, chainSeed, 0);
  }

  /** Loads an encoded key at `period`, checking both trees and that the super signature certifies the sub tree. */
  load(input: Uint8Array, period: number): Result<KesProductKey, KesError> {
    const raw = copy(input);
    if (raw.length !== this.keyLength) return err(sizeError("KES key", this.keyLength, raw.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    const ops = this.#ops;
    const subPeriods = 2 ** this.subHeight;
    const supLen = ops.encodedLength(this.superHeight);
    const subLen = ops.encodedLength(this.subHeight);
    const tSup = Math.floor(period / subPeriods);
    const sup = ops.decode(raw.subarray(0, supLen), this.superHeight, tSup, true);
    if (!sup.ok) return sup;
    const sub = ops.decode(raw.subarray(supLen, supLen + subLen), this.subHeight, period % subPeriods, false);
    if (!sub.ok) return sub;
    const nextSeed = raw.slice(supLen + subLen, supLen + subLen + 32);
    const superSig = raw.slice(supLen + subLen + 32);
    return ops.verify(ops.root(sup.value), this.superHeight, tSup, ops.productMessage(ops.root(sub.value)), superSig)
      ? ok(new KesProductKey(INTERNAL, this, ops, { superTree: sup.value, subTree: sub.value, nextSeed, superSig }, period))
      : err(inconsistent("the super signature does not certify the sub tree"));
  }

  /** The scheme-level forms (as Scala and the Rust SDK); each delegates to the key, which carries its own scheme. */
  verifyingKey(key: KesProductKey): Uint8Array {
    return key.verifyingKey;
  }

  sign(key: KesProductKey, message: Uint8Array): Result<Uint8Array, KesError> {
    return key.sign(message);
  }

  evolve(key: KesProductKey): Result<KesProductKey, KesError> {
    return key.evolve();
  }

  evolveTo(key: KesProductKey, target: number): Result<KesProductKey, KesError> {
    return key.evolveTo(target);
  }

  /** `superSig` over `productHash(subRoot)` at `t_sup` under `vk`, then `subSig` over the message at `t_sub` under `subRoot`. */
  verify(vk: Uint8Array, period: number, message: Uint8Array, signature: Uint8Array): Result<boolean, KesError> {
    const ops = this.#ops;
    if (vk.length !== ops.n) return err(sizeError("KES verifying key", ops.n, vk.length));
    if (signature.length !== this.signatureLength) return err(sizeError("KES signature", this.signatureLength, signature.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    const subPeriods = 2 ** this.subHeight;
    const supLen = ops.signatureLength(this.superHeight);
    const subRoot = signature.subarray(supLen, supLen + ops.n);
    return ok(
      ops.verify(vk, this.superHeight, Math.floor(period / subPeriods), ops.productMessage(subRoot), signature.subarray(0, supLen)) &&
        ops.verify(subRoot, this.subHeight, period % subPeriods, message, signature.subarray(supLen + ops.n)),
    );
  }
}

// ------------------------------------------------------------------ Cardano SumKES (Blake2b-256 over Ed25519, depths 0..7)

const blake256 = (b: Uint8Array): Uint8Array => blake2b(b, { dkLen: 32 });
const skLen = (d: number): number => 32 + 96 * d;

function cardanoVk(sk: Uint8Array, d: number): Uint8Array {
  return d === 0 ? leafVk(sk) : blake256(sk.subarray(skLen(d - 1) + 32, skLen(d)));
}

function cardanoGen(seed: Uint8Array, d: number): Uint8Array {
  if (d === 0) return seed;
  const r0 = blake256(concat(Uint8Array.of(1), seed));
  const r1 = blake256(concat(Uint8Array.of(2), seed));
  const sk0 = cardanoGen(r0, d - 1);
  return concat(sk0, r1, cardanoVk(sk0, d - 1), cardanoVk(cardanoGen(r1, d - 1), d - 1));
}

function cardanoSign(sk: Uint8Array, d: number, t: number, message: Uint8Array): Uint8Array {
  if (d === 0) return leafSign(sk, message);
  const half = 2 ** (d - 1);
  return concat(cardanoSign(sk.subarray(0, skLen(d - 1)), d - 1, t < half ? t : t - half, message), sk.subarray(skLen(d - 1) + 32, skLen(d)));
}

/** `updateKES`: undefined once the last period is reached; an error if the key is inconsistent. */
function cardanoUpdate(sk: Uint8Array, d: number, t: number): Result<Uint8Array | undefined, KesError> {
  if (d === 0) return ok(undefined);
  const half = 2 ** (d - 1);
  const r1 = sk.subarray(skLen(d - 1), skLen(d - 1) + 32);
  const vks = sk.subarray(skLen(d - 1) + 32, skLen(d));
  if (t + 1 === half) {
    // activating the right half regenerates it from r1, checked against the stored vk1, and zeroes r1
    const right = cardanoGen(r1, d - 1);
    return equal(cardanoVk(right, d - 1), vks.subarray(32)) ? ok(concat(right, ZERO32, vks)) : err(inconsistent(`r1 at depth ${String(d)} does not match vk1`));
  }
  const next = cardanoUpdate(sk.subarray(0, skLen(d - 1)), d - 1, t + 1 < half ? t : t - half);
  if (!next.ok || next.value === undefined) return next;
  return ok(concat(next.value, r1, vks));
}

/** Load-time consistency at period `t`: `r1` is zero exactly once used, and each active subtree's vk is the stored one. */
function cardanoCheck(sk: Uint8Array, d: number, t: number): Result<true, KesError> {
  if (d === 0) return ok(true);
  const half = 2 ** (d - 1);
  const inner = sk.subarray(0, skLen(d - 1));
  const r1 = sk.subarray(skLen(d - 1), skLen(d - 1) + 32);
  const vk0 = sk.subarray(skLen(d - 1) + 32, skLen(d - 1) + 64);
  const vk1 = sk.subarray(skLen(d - 1) + 64, skLen(d));
  if ((t >= half) !== isZero(r1)) return err(inconsistent(`r1 state does not match period ${String(t)} at depth ${String(d)}`));
  if (!equal(cardanoVk(inner, d - 1), t < half ? vk0 : vk1)) return err(inconsistent(`active subtree does not match its vk at depth ${String(d)}`));
  return cardanoCheck(inner, d - 1, t % half);
}

function cardanoVerify(vk: Uint8Array, d: number, t: number, message: Uint8Array, sig: Uint8Array): boolean {
  if (d === 0) return verifyLibsodiumRule(vk, message, sig);
  const inner = sig.subarray(0, sig.length - 64);
  const vk0 = sig.subarray(sig.length - 64, sig.length - 32);
  const vk1 = sig.subarray(sig.length - 32);
  const half = 2 ** (d - 1);
  return equal(blake256(concat(vk0, vk1)), vk) && (t < half ? cardanoVerify(vk0, d - 1, t, message, inner) : cardanoVerify(vk1, d - 1, t - half, message, inner));
}

/** A `CardanoSumKes` signing key: cardano-base's raw layout `sk || r1 || vk0 || vk1`, recursively. */
export class CardanoSumKesKey extends KesKey {
  constructor(
    _token: Internal,
    readonly scheme: CardanoSumKes,
    tree: Uint8Array,
    period: number,
  ) {
    super(tree, period);
    Object.freeze(this);
  }

  get verifyingKey(): Uint8Array {
    return cardanoVk(this.bytes, this.scheme.depth);
  }

  sign(message: Uint8Array): Uint8Array {
    return cardanoSign(this.bytes, this.scheme.depth, this.period, message);
  }

  /** The key for the next period (`updateKES`); the last period cannot evolve. */
  evolve(): Result<CardanoSumKesKey, KesError> {
    const next = cardanoUpdate(this.bytes, this.scheme.depth, this.period);
    if (!next.ok) return next;
    return next.value === undefined ? err({ kind: "Exhausted", period: this.period }) : ok(new CardanoSumKesKey(INTERNAL, this.scheme, next.value, this.period + 1));
  }

  evolveTo(target: number): Result<CardanoSumKesKey, KesError> {
    return evolveTo<CardanoSumKesKey>(this, target, this.scheme.totalPeriods, (k) => k.evolve());
  }

  toString(): string {
    return `CardanoSumKesKey(depth=${String(this.scheme.depth)}, period=${String(this.period)}, <redacted>)`;
  }
}

/**
 * cardano-base `Cardano.Crypto.KES.Sum`, `SumKES Blake2b_256` over `SingleKES Ed25519DSIGN`, depths 0 to 7, byte for
 * byte; `CardanoSumKes.sum6` is the Praos hot key. Leaves verify under libsodium's Ed25519 rule, as cardano-node does.
 */
export class CardanoSumKes {
  readonly totalPeriods: number;
  readonly signingKeyLength: number;
  readonly signatureLength: number;

  private constructor(readonly depth: number) {
    this.totalPeriods = 2 ** depth;
    this.signingKeyLength = skLen(depth);
    this.signatureLength = 64 + 64 * depth;
    Object.freeze(this);
  }

  /** `SumKES` of depth 0 to 7 (`Sum0KES` to `Sum7KES`). */
  static of(depth: number): Result<CardanoSumKes, KesError> {
    return Number.isInteger(depth) && depth >= 0 && depth <= 7 ? ok(new CardanoSumKes(depth)) : err(malformed("KES depth", `expected 0..7, got ${String(depth)}`));
  }

  /** `Sum6KES`: 64 periods, 448-byte signatures, 608-byte keys. */
  static readonly sum6: CardanoSumKes = new CardanoSumKes(6);

  /** The period-0 key for a 32-byte seed (`genKeyKES`). */
  generate(seed: Uint8Array): Result<CardanoSumKesKey, KesError> {
    return seed.length === 32 ? ok(new CardanoSumKesKey(INTERNAL, this, cardanoGen(copy(seed), this.depth), 0)) : err(sizeError("KES seed", 32, seed.length));
  }

  /** Loads cardano's raw signing key (the `kes.skey` payload) at the period the caller knows it to be at. */
  load(input: Uint8Array, period: number): Result<CardanoSumKesKey, KesError> {
    // the key keeps the encoding itself, so it must own it
    const raw = copy(input);
    if (raw.length !== this.signingKeyLength) return err(sizeError("KES signing key", this.signingKeyLength, raw.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    const c = cardanoCheck(raw, this.depth, period);
    return c.ok ? ok(new CardanoSumKesKey(INTERNAL, this, raw, period)) : c;
  }

  /** The scheme-level forms (as Scala and the Rust SDK); each delegates to the key, which carries its own scheme. */
  verifyingKey(key: CardanoSumKesKey): Uint8Array {
    return key.verifyingKey;
  }

  sign(key: CardanoSumKesKey, message: Uint8Array): Uint8Array {
    return key.sign(message);
  }

  evolve(key: CardanoSumKesKey): Result<CardanoSumKesKey, KesError> {
    return key.evolve();
  }

  evolveTo(key: CardanoSumKesKey, target: number): Result<CardanoSumKesKey, KesError> {
    return key.evolveTo(target);
  }

  /** `verifyKES` of `signature` under root key `vk` at relative `period`. */
  verify(vk: Uint8Array, period: number, message: Uint8Array, signature: Uint8Array): Result<boolean, KesError> {
    if (vk.length !== 32) return err(sizeError("KES verifying key", 32, vk.length));
    if (signature.length !== this.signatureLength) return err(sizeError("KES signature", this.signatureLength, signature.length));
    if (!inRange(period, this.totalPeriods)) return err(outOfRange(period, this.totalPeriods));
    return ok(cardanoVerify(vk, this.depth, period, message, signature));
  }
}
