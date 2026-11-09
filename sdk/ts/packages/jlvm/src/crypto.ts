/**
 * The cryptographic and proof operations (jlvm.md section 6; `CryptoOps.scala`, `ZkOps.scala`, `ProofOps.scala`): the
 * JLVM's own argument checks, then the `signing` and `proofs` packages and core's hashing for the cryptography. Every
 * handler runs after its operation's base, its arguments and its size charge (section 7), so a handler only decides the
 * outcome: the gas is already right, refusals included. {@link CRYPTO}'s type is a record over every
 * {@link CryptoOp}, so a missing or extra handler does not compile.
 *
 * The JLVM checks what needs no cryptographic library: arity, kinds, fixed widths, canonical BN254 scalars, Merkle tree
 * heads, trie key modes and key lengths, the Merkle profile, and the claims' weight. The packages decode keys,
 * signatures and proofs, and verify. The checks interleave as the reference's do, so the first refusal is the same:
 * where the reference decodes something before checking a later argument's kind (`sigma_verify`'s proposition,
 * `merkle_verify`'s proof before its RFC 6962 leaf), so does this. A refusal that a family's library made is a `Crypto` error whose `cause` is that family's typed error
 * (jlvm.md section 3); the operations' own shape checks carry no cause, as the reference's do.
 *
 * Arguments are values the machine owns, so no caller bytes cross here: a byte string is read through core's
 * `bytesView`, uncopied, and measured before the packages copy it, so a fixed-width argument charged per argument (an
 * `ecrecover` signature, a `bls_aggregate_verify` key) is never copied past its width.
 */
import { BytesValue, MapValue, digestFromBytes, models, type TypeMismatch, type Value } from "@kd5ujc/dilf4s-core";
import { bytesView } from "@kd5ujc/dilf4s-core/internal";
import { MAX_PROOF_WEIGHT, merkle, poseidon, trie, zk } from "@kd5ujc/dilf4s-proofs";
import { bip340, blsPop, ecrecover, ecvrfTai, ed25519, Message32, secp256k1Ecdsa, sigma, type VerifyingKey } from "@kd5ujc/dilf4s-signing";

import { arity, domain, typeError, type JlvmError } from "./error.js";
import type { AnyMap, Heap, JValue } from "./jvalue.js";
import type { CryptoOp } from "./op.js";
import { bad, bool, bounded, bytes as bytesResult, FALSE, good, kindName, TRUE, type Out } from "./values.js";

/** A cryptographic operation's handler: its evaluated arguments, and the evaluation's heap. */
export type CryptoHandler = (args: readonly JValue[], heap: Heap) => Out;

type Cause = models.jlvmErrors.JlvmCryptoCause;
type Family = Cause["kind"];
/** The typed error of family `F`. */
type ErrorOf<F extends Family> = Extract<Cause, { readonly kind: F }>["error"];

/** The most pairs `bn254_pairing` accepts. */
const MAX_PAIRS = models.limits.JLVM_MAX_PAIRS;
/** The most siblings `pmt_verify` accepts: the deepest Poseidon Merkle tree. */
const MAX_SIBLINGS = poseidon.MAX_DEPTH;
const FIELD_BYTES = 32;
const G1_BYTES = 64;
const G2_BYTES = 128;
const VRF_KEY_BYTES = 32;
const VRF_PROOF_BYTES = 80;

// ------------------------------------------------------------------ refusals

/** A `Crypto` error the operation's own checks make: no cause. */
const crypto = (op: CryptoOp, message: string): JlvmError => ({ kind: "Crypto", op, message });

/** A refusal's message: its kind, and its reason where it has one. Not normative; never the caller's input. */
function describe(e: { readonly kind: string }): string {
  const reason: unknown = "reason" in e ? e.reason : undefined;
  return typeof reason === "string" ? `${e.kind}: ${reason}` : e.kind;
}

/**
 * A family's refusal as the `Crypto` error that holds it, its cause the family and the refusal. A `TypeMismatch` (a
 * package refusing an argument's JavaScript type) cannot happen, since every argument passed here is of its type; it
 * would be a `Crypto` error with no cause, never a verdict.
 */
function refused<F extends Family>(op: CryptoOp, family: F, e: ErrorOf<F> | TypeMismatch): JlvmError {
  if (e.kind === "TypeMismatch") return crypto(op, `${op}: ${e.reason}`);
  const cause = { kind: family, error: e } as Cause;
  return { kind: "Crypto", op, message: describe(e), cause };
}

/** A package's result, its refusal as {@link refused}. */
function from<T, F extends Family>(op: CryptoOp, family: F, r: { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: ErrorOf<F> | TypeMismatch }): Out<T> {
  return r.ok ? good(r.value) : bad(refused(op, family, r.error));
}

const arityError = (op: CryptoOp, message: string, args: readonly JValue[]): Out<never> => bad(arity(op, `${message}, got ${args.length.toString()} arguments`));

// ------------------------------------------------------------------ argument decoding

/** Bytes, read in place; another kind is `Type`, named by `role` when given. */
function raw(op: CryptoOp, v: JValue, role?: string): Out<Uint8Array> {
  if (v.kind === "bytes") return good(bytesView(v));
  return bad(typeError(op, `${role === undefined ? "" : `${role}: `}expected bytes, got ${kindName(v)}`));
}

/** Bytes of exactly `width`, measured before they are read: the wrong kind is `Type`, the wrong width `Crypto`. */
function sized(op: CryptoOp, role: string, v: JValue, width: number): Out<Uint8Array> {
  const b = raw(op, v, role);
  if (!b.ok || b.value.length === width) return b;
  return bad(crypto(op, `${role}: expected ${width.toString()} bytes, got ${b.value.length.toString()}`));
}

/** 32 bytes, big-endian, below the BN254 scalar field modulus `r`: else `Crypto`, caused by Poseidon's `NonCanonical`. */
function fr(op: CryptoOp, role: string, v: JValue): Out<poseidon.Fr> {
  const b = sized(op, role, v, FIELD_BYTES);
  if (!b.ok) return b;
  const x = poseidon.Fr.of(beToBig(b.value));
  if (x.ok) return x;
  const error = x.error;
  if (error.kind === "TypeMismatch") return bad(refused(op, "Poseidon", error));
  return bad({ kind: "Crypto", op, message: `${role}: not a canonical BN254 scalar field element (must be < r)`, cause: { kind: "Poseidon", error } });
}

function beToBig(b: Uint8Array): bigint {
  let n = 0n;
  for (const x of b) n = (n << 8n) | BigInt(x);
  return n;
}

function integer(op: CryptoOp, role: string, v: JValue): Out<bigint> {
  return v.kind === "int" ? good(v.value) : bad(typeError(op, `${role}: expected an integer, got ${kindName(v)}`));
}

function array(op: CryptoOp, role: string, v: JValue): Out<readonly JValue[]> {
  return v.kind === "array" ? good(v.items) : bad(typeError(op, `${role}: expected an array, got ${kindName(v)}`));
}

/** `v` as a core value: a container the machine built is converted once, by the heap (its weight paid for it). */
function value(op: CryptoOp, heap: Heap, v: JValue): Out<Value> {
  const x = heap.value(v);
  return x === undefined ? bad(crypto(op, "a value the machine could not convert")) : good(x);
}

/** `[key, message, signature]`, all bytes. */
function triple(op: CryptoOp, args: readonly JValue[]): Out<readonly [Uint8Array, Uint8Array, Uint8Array]> {
  const [k, m, s] = args;
  if (args.length !== 3 || k === undefined || m === undefined || s === undefined) return arityError(op, "takes [key, message, signature]", args);
  const key = raw(op, k);
  if (!key.ok) return key;
  const message = raw(op, m);
  if (!message.ok) return message;
  const signature = raw(op, s);
  return signature.ok ? good([key.value, message.value, signature.value] as const) : signature;
}

// ------------------------------------------------------------------ signatures

type BytesVerify = (key: Uint8Array, message: Uint8Array, signature: Uint8Array) => { readonly ok: true; readonly value: boolean } | { readonly ok: false; readonly error: ErrorOf<"Signing"> | TypeMismatch };

/** `[key, message, signature]`: the scheme decodes the key, then the signature, then verifies. */
const signature =
  (op: CryptoOp, verify: BytesVerify): CryptoHandler =>
  (args) => {
    const t = triple(op, args);
    if (!t.ok) return t;
    const r = from(op, "Signing", verify(...t.value));
    return r.ok ? good(bool(r.value)) : r;
  };

/** Standard BIP-340: the key, then the message (exactly 32 bytes), then the signature are decoded, then verified. */
const bip340Verify: CryptoHandler = (args) => {
  const op = "bip340_verify";
  const t = triple(op, args);
  if (!t.ok) return t;
  const [k, m, s] = t.value;
  const key = from(op, "Signing", bip340.verifyingKey(k));
  if (!key.ok) return key;
  const message = from(op, "Signing", Message32.of(m));
  if (!message.ok) return message;
  const sig = from(op, "Signing", bip340.signature(s));
  if (!sig.ok) return sig;
  const r = from(op, "Signing", bip340.verify(key.value, message.value, sig.value));
  return r.ok ? good(bool(r.value)) : r;
};

/**
 * `[digest32, signature65]`: the signer's 20-byte Ethereum address. A signature that recovers no key is an error with no
 * cause, never a value: the zero address, or a null, would equal an unset owner.
 */
const ecrecoverOp: CryptoHandler = (args) => {
  const op = "ecrecover";
  const [d, s] = args;
  if (args.length !== 2 || d === undefined || s === undefined) return arityError(op, "takes [digest, signature]", args);
  const digest = raw(op, d);
  if (!digest.ok) return digest;
  const sig = raw(op, s);
  if (!sig.ok) return sig;
  const address = from(op, "Signing", ecrecover(digest.value, sig.value));
  if (!address.ok) return address;
  return address.value === undefined
    ? bad(crypto(op, "the signature recovers no key (r or s out of range, high s, or r not an x-coordinate)"))
    : bytesResult(op, address.value);
};

/**
 * `[[key, ...], message, signature]`: one message signed by every key (fast aggregate verification). The keys must carry
 * proofs of possession, checked by whoever admitted them. Every argument's kind first, then each key decoded in
 * order, then the signature.
 */
const blsAggregate: CryptoHandler = (args) => {
  const op = "bls_aggregate_verify";
  const [ks, m, s] = args;
  if (args.length !== 3 || ks === undefined || m === undefined || s === undefined) return arityError(op, "takes [[key, ...], message, signature]", args);
  if (ks.kind !== "array") return bad(typeError(op, `expected an array of keys, got ${kindName(ks)}`));
  const keyBytes: Uint8Array[] = [];
  for (const k of ks.items) {
    const b = raw(op, k);
    if (!b.ok) return b;
    keyBytes.push(b.value);
  }
  const message = raw(op, m);
  if (!message.ok) return message;
  const sigBytes = raw(op, s);
  if (!sigBytes.ok) return sigBytes;
  const keys: VerifyingKey<"BLS12-381-PoP">[] = [];
  for (const k of keyBytes) {
    const key = from(op, "Signing", blsPop.verifyingKey(k));
    if (!key.ok) return key;
    keys.push(key.value);
  }
  const sig = from(op, "Signing", blsPop.signature(sigBytes.value));
  if (!sig.ok) return sig;
  const r = from(op, "Signing", blsPop.fastAggregateVerify(keys, message.value, sig.value));
  return r.ok ? good(bool(r.value)) : r;
};

// ------------------------------------------------------------------ sigma protocols (sigma.md)

/**
 * `sigma_verify [proposition, proof, message]`: the proposition decoded first (its refusal is `Sigma`'s, whatever the
 * later arguments' kinds), then the proof's and the message's kinds, then the proof checked.
 */
const sigmaVerify: CryptoHandler = (args, heap) => {
  const op = "sigma_verify";
  const [p, pf, m] = args;
  if (args.length !== 3 || p === undefined || pf === undefined || m === undefined) return arityError(op, "takes [proposition, proof, message]", args);
  const given = value(op, heap, p);
  if (!given.ok) return given;
  const proposition = from(op, "Sigma", sigma.Proposition.fromValue(given.value));
  if (!proposition.ok) return proposition;
  const proof = raw(op, pf);
  if (!proof.ok) return proof;
  const message = raw(op, m);
  if (!message.ok) return message;
  const r = from(op, "Sigma", sigma.verify(proposition.value, proof.value, message.value));
  return r.ok ? good(bool(r.value)) : r;
};

/** `schnorr_verify [key33, message, proof56]`: a one-leaf `dlog` sigma proof (Ergo's P2PK signature), the key decoded first. */
const schnorrVerify: CryptoHandler = (args) => {
  const op = "schnorr_verify";
  const t = triple(op, args);
  if (!t.ok) return t;
  const r = from(op, "Sigma", sigma.verifyDlogBytes(...t.value));
  return r.ok ? good(bool(r.value)) : r;
};

// ------------------------------------------------------------------ Poseidon, Groth16, VRF and BN254

/** `poseidon [x1, .., xn]` or `[[x1, .., xn]]`, `1 <= n <= 4`: 32 bytes. */
const poseidonOp: CryptoHandler = (args) => {
  const op = "poseidon";
  const [first] = args;
  const inputs = args.length === 1 && first?.kind === "array" && first.items.length > 0 ? first.items : args;
  if (inputs.length === 0 || inputs.length > poseidon.MAX_INPUTS) {
    return bad(arity(op, `takes 1 to ${poseidon.MAX_INPUTS.toString()} field elements, got ${inputs.length.toString()}`));
  }
  const xs: poseidon.Fr[] = [];
  for (const [i, v] of inputs.entries()) {
    const x = fr(op, `input[${i.toString()}]`, v);
    if (!x.ok) return x;
    xs.push(x.value);
  }
  const digest = from(op, "Poseidon", poseidon.hash(xs));
  return digest.ok ? bytesResult(op, be32(digest.value.value)) : digest;
};

function be32(n: bigint): Uint8Array {
  const out = new Uint8Array(32);
  let x = n;
  for (let i = 31; i >= 0; i--) {
    out[i] = Number(x & 0xffn);
    x >>= 8n;
  }
  return out;
}

/** `pmt_verify [root, leaf, index, [siblings, root-first]]`: an index out of range for the depth is `PoseidonMerkle`'s refusal. */
const pmtVerify: CryptoHandler = (args) => {
  const op = "pmt_verify";
  const [rootV, leafV, indexV, siblingsV] = args;
  if (args.length !== 4 || rootV === undefined || leafV === undefined || indexV === undefined || siblingsV === undefined) {
    return arityError(op, "takes [root, leaf, index, siblings]", args);
  }
  const root = fr(op, "root", rootV);
  if (!root.ok) return root;
  const leaf = fr(op, "leaf", leafV);
  if (!leaf.ok) return leaf;
  const index = integer(op, "index", indexV);
  if (!index.ok) return index;
  const given = array(op, "siblings", siblingsV);
  if (!given.ok) return given;
  if (given.value.length > MAX_SIBLINGS) return bad(crypto(op, `at most ${MAX_SIBLINGS.toString()} siblings, got ${given.value.length.toString()}`));
  const siblings: poseidon.Fr[] = [];
  for (const [i, v] of given.value.entries()) {
    const x = fr(op, `sibling[${i.toString()}]`, v);
    if (!x.ok) return x;
    siblings.push(x.value);
  }
  // the index is charged as one element, whatever its size: the refusal carries its bit length, never its digits
  const proof = poseidon.PoseidonMerkleProof.of(index.value, siblings);
  if (!proof.ok) {
    const error = proof.error;
    if (error.kind === "TypeMismatch") return bad(refused(op, "PoseidonMerkle", error));
    return bad({ kind: "Crypto", op, message: `index is out of range for depth ${siblings.length.toString()}`, cause: { kind: "PoseidonMerkle", error } });
  }
  const r = from(op, "PoseidonMerkle", poseidon.verifyInclusion(leaf.value, proof.value, root.value));
  return r.ok ? good(bool(r.value)) : r;
};

/** SP1's Groth16 verifying key (circuit v6.1.0), decoded and checked once, as the Rust backend holds it. */
const SP1_KEY = zk.groth16.sp1.verifyingKey();

/**
 * `groth16_verify [vkey32, publicValues, proof]` (an SP1 Groth16 proof, zk.md section 2): a malformed key, framing or
 * proof is `Groth16`'s refusal; a proof that is rejected, invalid or fails the pairing is false.
 */
const groth16Verify: CryptoHandler = (args) => {
  const op = "groth16_verify";
  const [k, pv, pf] = args;
  if (args.length !== 3 || k === undefined || pv === undefined || pf === undefined) return arityError(op, "takes [vkey, publicValues, proof]", args);
  const vkey = sized(op, "vkey", k, FIELD_BYTES);
  if (!vkey.ok) return vkey;
  const publicValues = raw(op, pv, "publicValues");
  if (!publicValues.ok) return publicValues;
  const proof = raw(op, pf, "proof");
  if (!proof.ok) return proof;
  if (!SP1_KEY.ok) return bad(crypto(op, "SP1's Groth16 verifying key is unavailable"));
  const r = zk.groth16.sp1.verifyWith(SP1_KEY.value, vkey.value, publicValues.value, proof.value);
  if (r.ok) return good(TRUE);
  return r.error.kind === "Malformed" || r.error.kind === "TypeMismatch" ? bad(refused(op, "Groth16", r.error)) : good(FALSE);
};

/** `ecvrf_verify [pk32, alpha, proof80]` (ECVRF-EDWARDS25519-SHA512-TAI): `{valid: true, beta}` or `{valid: false}`. */
const ecvrfVerify: CryptoHandler = (args) => {
  const op = "ecvrf_verify";
  const [pkV, alphaV, proofV] = args;
  if (args.length !== 3 || pkV === undefined || alphaV === undefined || proofV === undefined) return arityError(op, "takes [pk, alpha, proof]", args);
  const pk = sized(op, "pk", pkV, VRF_KEY_BYTES);
  if (!pk.ok) return pk;
  const alpha = raw(op, alphaV, "alpha");
  if (!alpha.ok) return alpha;
  const proof = sized(op, "proof", proofV, VRF_PROOF_BYTES);
  if (!proof.ok) return proof;
  const r = ecvrfTai.verifyBytes(pk.value, alpha.value, proof.value);
  // a malformed proof or key is an error; every other refusal is a well-formed proof that does not verify
  if (!r.ok && (r.error.kind === "Malformed" || r.error.kind === "TypeMismatch")) return bad(refused(op, "Vrf", r.error));
  const beta = r.ok ? BytesValue.of(r.value.bytes) : undefined;
  const entries: (readonly [string, Value])[] = beta?.ok === true ? [["valid", TRUE], ["beta", beta.value]] : [["valid", FALSE]];
  const m = MapValue.of(entries);
  return m.ok ? bounded(op, m.value) : bad(crypto(op, "the result does not encode"));
};

/** `bn254_add [a64, b64]` (EIP-196): 64 bytes. */
const bn254Add: CryptoHandler = (args) => {
  const op = "bn254_add";
  const [aV, bV] = args;
  if (args.length !== 2 || aV === undefined || bV === undefined) return arityError(op, "takes [a, b]", args);
  const a = sized(op, "a", aV, G1_BYTES);
  if (!a.ok) return a;
  const b = sized(op, "b", bV, G1_BYTES);
  if (!b.ok) return b;
  const sum = from(op, "Zk", zk.add(a.value, b.value));
  return sum.ok ? bytesResult(op, sum.value) : sum;
};

/** `bn254_mul [point64, scalar32]` (EIP-196): any 256-bit scalar, reduced mod r; 64 bytes. */
const bn254Mul: CryptoHandler = (args) => {
  const op = "bn254_mul";
  const [pV, sV] = args;
  if (args.length !== 2 || pV === undefined || sV === undefined) return arityError(op, "takes [point, scalar]", args);
  const p = sized(op, "point", pV, G1_BYTES);
  if (!p.ok) return p;
  const s = sized(op, "scalar", sV, FIELD_BYTES);
  if (!s.ok) return s;
  const q = from(op, "Zk", zk.mul(p.value, s.value));
  return q.ok ? bytesResult(op, q.value) : q;
};

/**
 * `bn254_pairing [[g1, g2], ..]` or `[[[g1, g2], ..]]` (EIP-197; no pairs is true): a single argument whose elements
 * are all arrays is the list of pairs (a pair holds bytes, never arrays), so a single empty array is no pairs.
 */
const bn254Pairing: CryptoHandler = (args) => {
  const op = "bn254_pairing";
  const [first] = args;
  const pairs = args.length === 1 && first?.kind === "array" && first.items.every((v) => v.kind === "array") ? first.items : args;
  if (pairs.length > MAX_PAIRS) return bad(crypto(op, `at most ${MAX_PAIRS.toString()} pairs, got ${pairs.length.toString()}`));
  const points: (readonly [Uint8Array, Uint8Array])[] = [];
  for (const [i, v] of pairs.entries()) {
    const at = `pair[${i.toString()}]`;
    if (v.kind !== "array") return bad(typeError(op, `${at}: expected an array, got ${kindName(v)}`));
    const [a, b] = v.items;
    if (v.items.length !== 2 || a === undefined || b === undefined) return bad(crypto(op, `${at}: expected [g1, g2], got ${v.items.length.toString()} elements`));
    const g1 = sized(op, `${at}.g1`, a, G1_BYTES);
    if (!g1.ok) return g1;
    const g2 = sized(op, `${at}.g2`, b, G2_BYTES);
    if (!g2.ok) return g2;
    points.push([g1.value, g2.value]);
  }
  const r = from(op, "Zk", zk.pairing(points));
  return r.ok ? good(bool(r.value)) : r;
};

// ------------------------------------------------------------------ authenticated structures (merkle.md, trie.md)

const SHA256 = "SHA-256";
const TAGGED = merkle.TaggedMerkleHashing.of(SHA256);
const RFC6962 = merkle.Rfc6962MerkleHashing.of(SHA256);
const TRIE_HASHING = trie.TrieHashing.of(SHA256);

/** Claims (a leaf, a value, a listing's values) together at most `MAX_PROOF_WEIGHT`, from their weights: else the structure's `TooLarge`. */
function claims(values: readonly JValue[]): boolean {
  let total = 0n;
  for (const v of values) total += v.weight;
  return total <= MAX_PROOF_WEIGHT;
}

const merkleTooLarge = (op: CryptoOp): JlvmError => ({
  kind: "Crypto",
  op,
  message: `the claims weigh over ${MAX_PROOF_WEIGHT.toString()}`,
  cause: { kind: "Merkle", error: { kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) } },
});

const trieTooLarge = (op: CryptoOp): JlvmError => ({
  kind: "Crypto",
  op,
  message: `the claims weigh over ${MAX_PROOF_WEIGHT.toString()}`,
  cause: { kind: "Trie", error: { kind: "TooLarge", limit: Number(MAX_PROOF_WEIGHT) } },
});

/** A SHA-256 digest: bytes (`Type`) of 32 bytes (`Crypto`, caused by the hash family's `InvalidLength`). */
function digest(op: CryptoOp, what: string, v: JValue): Out<Uint8Array> {
  if (v.kind !== "bytes") return bad(typeError(op, `${what} is bytes, got ${kindName(v)}`));
  const b = bytesView(v);
  const d = digestFromBytes(SHA256, b);
  if (d.ok) return good(b);
  const error = d.error;
  if (error.kind === "TypeMismatch") return bad(refused(op, "Hash", error));
  return bad({ kind: "Crypto", op, message: `${what}: ${describe(error)}`, cause: { kind: "Hash", error } });
}

/** The entry at `key` of a map the machine holds. */
const entry = (m: AnyMap, key: string): JValue | undefined => m.get(key);

/** A trusted tree head `{root, size}`: anything else, or a size that is no tree size, is `Crypto` with no cause. */
function head(op: CryptoOp, what: string, v: JValue): Out<merkle.MerkleHead> {
  const shape = `${what} is a tree head {root, size}`;
  if (v.kind !== "map" || v.size !== 2) return bad(crypto(op, shape));
  const [rootV, sizeV] = [entry(v, "root"), entry(v, "size")];
  if (rootV === undefined || sizeV === undefined) return bad(crypto(op, shape));
  const root = digest(op, `${what}.root`, rootV);
  if (!root.ok) return root;
  return sizeV.kind === "int" && sizeV.value >= 0n && sizeV.value < 1n << 63n ? good({ root: root.value, size: sizeV.value }) : bad(crypto(op, `${what}.size is a tree size`));
}

type Profile = "tagged" | "rfc6962";

/** The optional last argument: none, or `"rfc6962"`; anything else is `Domain`. */
function profile(op: CryptoOp, rest: readonly JValue[]): Out<Profile> {
  const [p] = rest;
  if (rest.length === 0) return good("tagged");
  return rest.length === 1 && p?.kind === "text" && p.value === "rfc6962" ? good("rfc6962") : bad(domain(op, 'the only optional profile is "rfc6962"'));
}

/** A Merkle verdict: the verifiers make `InvalidProof` and `RootMismatch` false; any other refusal is an error. */
function merkleVerdict(op: CryptoOp, r: { readonly ok: true; readonly value: boolean } | { readonly ok: false; readonly error: ErrorOf<"Merkle"> | TypeMismatch }): Out {
  return r.ok ? good(bool(r.value)) : bad(refused(op, "Merkle", r.error));
}

/**
 * `merkle_verify [head, leaf, proof]` or with `"rfc6962"`: the head, the profile, the leaf's weight, the proof's
 * decoding, then an RFC 6962 leaf's kind, in that order. A proof of another size than the trusted head's is false.
 */
const merkleVerify: CryptoHandler = (args, heap) => {
  const op = "merkle_verify";
  const [h, leaf, proofV] = args;
  if (h === undefined || leaf === undefined || proofV === undefined) return bad(arity(op, "takes [head, leaf, proof] or [head, leaf, proof, profile]"));
  const hd = head(op, "head", h);
  if (!hd.ok) return hd;
  const p = profile(op, args.slice(3));
  if (!p.ok) return p;
  if (!claims([leaf])) return bad(merkleTooLarge(op));
  if (!TAGGED.ok || !RFC6962.ok) return bad(crypto(op, "SHA-256 Merkle hashing is unavailable"));
  const hashing = p.value === "tagged" ? TAGGED.value : RFC6962.value;
  const proofValue = value(op, heap, proofV);
  if (!proofValue.ok) return proofValue;
  const proof = from(op, "Merkle", merkle.InclusionProof.decode(hashing, proofValue.value));
  if (!proof.ok) return proof;
  if (p.value === "rfc6962" && leaf.kind !== "bytes") return bad(typeError(op, `an rfc6962 leaf is bytes, got ${kindName(leaf)}`));
  // the size comes from the trusted head: a smaller tree's root can satisfy a proof claiming a larger size
  if (proof.value.size !== hd.value.size) return good(FALSE);
  const leafHash = leafHashOf(op, heap, p.value, leaf);
  if (!leafHash.ok) return leafHash;
  return merkleVerdict(op, merkle.verifyInclusion(hashing, hd.value.root, leafHash.value, proof.value));
};

/** A leaf's hash: the tagged profile commits the value (`commit(v)`), RFC 6962 hashes its raw bytes. */
function leafHashOf(op: CryptoOp, heap: Heap, p: Profile, leaf: JValue): Out<Uint8Array> {
  if (!TAGGED.ok || !RFC6962.ok) return bad(crypto(op, "SHA-256 Merkle hashing is unavailable"));
  if (p === "rfc6962") return leaf.kind === "bytes" ? from(op, "Merkle", RFC6962.value.leaf(bytesView(leaf))) : bad(typeError(op, "an rfc6962 leaf is bytes"));
  const v = value(op, heap, leaf);
  return v.ok ? from(op, "Merkle", TAGGED.value.leaf(v.value)) : v;
}

/** `merkle_consistency_verify [oldHead, newHead, proof]` or with `"rfc6962"`: sizes other than the heads' are false. */
const merkleConsistencyVerify: CryptoHandler = (args, heap) => {
  const op = "merkle_consistency_verify";
  const [o, n, proofV] = args;
  if (o === undefined || n === undefined || proofV === undefined) return bad(arity(op, "takes [oldHead, newHead, proof] or [oldHead, newHead, proof, profile]"));
  const oldHead = head(op, "oldHead", o);
  if (!oldHead.ok) return oldHead;
  const newHead = head(op, "newHead", n);
  if (!newHead.ok) return newHead;
  const p = profile(op, args.slice(3));
  if (!p.ok) return p;
  if (!TAGGED.ok || !RFC6962.ok) return bad(crypto(op, "SHA-256 Merkle hashing is unavailable"));
  const hashing = p.value === "tagged" ? TAGGED.value : RFC6962.value;
  const proofValue = value(op, heap, proofV);
  if (!proofValue.ok) return proofValue;
  const proof = from(op, "Merkle", merkle.ConsistencyProof.decode(hashing, proofValue.value));
  if (!proof.ok) return proof;
  if (proof.value.oldSize !== oldHead.value.size || proof.value.newSize !== newHead.value.size) return good(FALSE);
  return merkleVerdict(op, merkle.verifyConsistency(hashing, oldHead.value.root, newHead.value.root, proof.value));
};

const MODE_SHAPE = 'a mode is {"fixed": n} or {"variable": max}';
const INT32_MIN = -(2n ** 31n);
const INT32_MAX = 2n ** 31n - 1n;

/** `{"fixed": n}` or `{"variable": max}`: anything else, or a bound out of range, is `Domain`; not a map is `Type`. */
function mode(op: CryptoOp, v: JValue): Out<trie.KeyMode> {
  if (v.kind !== "map") return bad(typeError(op, `a mode is an object, got ${kindName(v)}`));
  const [only] = v.size === 1 ? v.entries() : [];
  const n = only?.[1];
  if (only === undefined || n?.kind !== "int" || n.value < INT32_MIN || n.value > INT32_MAX) return bad(domain(op, MODE_SHAPE));
  const made = only[0] === "fixed" ? trie.KeyMode.fixed(Number(n.value)) : only[0] === "variable" ? trie.KeyMode.variable(Number(n.value)) : undefined;
  if (made === undefined) return bad(domain(op, MODE_SHAPE));
  return made.ok ? made : bad(domain(op, describe(made.error)));
}

/** A key: bytes (`Type`) of a length the mode admits (`Crypto`, caused by the trie's `InvalidKey`). */
function key(op: CryptoOp, m: trie.KeyMode, v: JValue): Out<Uint8Array> {
  if (v.kind !== "bytes") return bad(typeError(op, `a key is bytes, got ${kindName(v)}`));
  const b = bytesView(v);
  const checked = from(op, "Trie", m.checkKey(b));
  return checked.ok ? good(b) : checked;
}

/** A trie verdict: a proof of another root is false (the package's own mapping); any other refusal is an error. */
function trieVerdict(op: CryptoOp, r: { readonly ok: true; readonly value: boolean } | { readonly ok: false; readonly error: ErrorOf<"Trie"> | TypeMismatch }): Out {
  return r.ok ? good(bool(r.value)) : bad(refused(op, "Trie", r.error));
}

/** `trie_verify [mode, root, key, value, proof]`: the mode, the key, the claim's weight, the root, then the proof. */
const trieVerify: CryptoHandler = (args, heap) => {
  const op = "trie_verify";
  const [md, rootV, keyV, claimed, proofV] = args;
  if (args.length !== 5 || md === undefined || rootV === undefined || keyV === undefined || claimed === undefined || proofV === undefined) {
    return bad(arity(op, "takes [mode, root, key, value, proof]"));
  }
  const m = mode(op, md);
  if (!m.ok) return m;
  const k = key(op, m.value, keyV);
  if (!k.ok) return k;
  const absent = claimed.kind === "null";
  if (!claims(absent ? [] : [claimed])) return bad(trieTooLarge(op));
  const root = digest(op, "root", rootV);
  if (!root.ok) return root;
  if (!TRIE_HASHING.ok) return bad(crypto(op, "SHA-256 trie hashing is unavailable"));
  const claim = absent ? good(null) : value(op, heap, claimed);
  if (!claim.ok) return claim;
  const proof = value(op, heap, proofV);
  if (!proof.ok) return proof;
  return trieVerdict(op, trie.trieVerify(TRIE_HASHING.value, m.value, root.value, k.value, claim.value, proof.value));
};

const ENTRY_SHAPE = "an entry is [key, value], with a value";

/** Claimed entries: `[key, value]` pairs with a value, in order, each key valid for the mode; then their weight. */
function entries(op: CryptoOp, heap: Heap, m: trie.KeyMode, v: JValue): Out<readonly trie.ClaimedEntry[]> {
  if (v.kind !== "array") return bad(typeError(op, `entries is an array, got ${kindName(v)}`));
  const read: (readonly [Uint8Array, JValue])[] = [];
  for (const e of v.items) {
    const [k, x] = e.kind === "array" && e.items.length === 2 ? e.items : [];
    if (k === undefined || x === undefined || x.kind === "null") return bad(typeError(op, ENTRY_SHAPE));
    const kb = key(op, m, k);
    if (!kb.ok) return kb;
    read.push([kb.value, x]);
  }
  if (!claims(read.map(([, x]) => x))) return bad(trieTooLarge(op));
  const out: trie.ClaimedEntry[] = [];
  for (const [k, x] of read) {
    const c = value(op, heap, x);
    if (!c.ok) return c;
    out.push([k, c.value]);
  }
  return good(out);
}

/** Bytes named `what` for a query bound: another kind is `Type`. */
const query = (op: CryptoOp, what: string, v: JValue): Out<Uint8Array> => (v.kind === "bytes" ? good(bytesView(v)) : bad(typeError(op, `${what} is bytes, got ${kindName(v)}`)));

/** `trie_prefix_verify [mode, root, prefix, entries, proof]`: the mode, the prefix, the entries, the root, then the proof. */
const triePrefixVerify: CryptoHandler = (args, heap) => {
  const op = "trie_prefix_verify";
  const [md, rootV, prefixV, es, proofV] = args;
  if (args.length !== 5 || md === undefined || rootV === undefined || prefixV === undefined || es === undefined || proofV === undefined) {
    return bad(arity(op, "takes [mode, root, prefix, entries, proof]"));
  }
  const m = mode(op, md);
  if (!m.ok) return m;
  const prefix = query(op, "a prefix", prefixV);
  if (!prefix.ok) return prefix;
  const claimed = entries(op, heap, m.value, es);
  if (!claimed.ok) return claimed;
  const root = digest(op, "root", rootV);
  if (!root.ok) return root;
  if (!TRIE_HASHING.ok) return bad(crypto(op, "SHA-256 trie hashing is unavailable"));
  const proof = value(op, heap, proofV);
  if (!proof.ok) return proof;
  return trieVerdict(op, trie.triePrefixVerify(TRIE_HASHING.value, m.value, root.value, prefix.value, claimed.value, proof.value));
};

/** `trie_range_verify [mode, root, from, until, entries, proof]` (`until` null is unbounded): checked as `trie_prefix_verify`. */
const trieRangeVerify: CryptoHandler = (args, heap) => {
  const op = "trie_range_verify";
  const [md, rootV, fromV, untilV, es, proofV] = args;
  if (args.length !== 6 || md === undefined || rootV === undefined || fromV === undefined || untilV === undefined || es === undefined || proofV === undefined) {
    return bad(arity(op, "takes [mode, root, from, until, entries, proof]"));
  }
  const m = mode(op, md);
  if (!m.ok) return m;
  const lo = query(op, "from", fromV);
  if (!lo.ok) return lo;
  const hi = untilV.kind === "null" ? good(null) : query(op, "until", untilV);
  if (!hi.ok) return hi;
  const claimed = entries(op, heap, m.value, es);
  if (!claimed.ok) return claimed;
  const root = digest(op, "root", rootV);
  if (!root.ok) return root;
  if (!TRIE_HASHING.ok) return bad(crypto(op, "SHA-256 trie hashing is unavailable"));
  const proof = value(op, heap, proofV);
  if (!proof.ok) return proof;
  return trieVerdict(op, trie.trieRangeVerify(TRIE_HASHING.value, m.value, root.value, lo.value, hi.value, claimed.value, proof.value));
};

// ------------------------------------------------------------------ the table

/** The cryptographic operations, one handler each. */
export const CRYPTO: Readonly<Record<CryptoOp, CryptoHandler>> = Object.freeze({
  ed25519_verify: signature("ed25519_verify", ed25519.verifyBytes),
  secp256k1_verify: signature("secp256k1_verify", secp256k1Ecdsa.verifyBytes),
  bip340_verify: bip340Verify,
  ecrecover: ecrecoverOp,
  bls_verify: signature("bls_verify", blsPop.verifyBytes),
  bls_aggregate_verify: blsAggregate,
  poseidon: poseidonOp,
  pmt_verify: pmtVerify,
  groth16_verify: groth16Verify,
  ecvrf_verify: ecvrfVerify,
  bn254_add: bn254Add,
  bn254_mul: bn254Mul,
  bn254_pairing: bn254Pairing,
  schnorr_verify: schnorrVerify,
  sigma_verify: sigmaVerify,
  merkle_verify: merkleVerify,
  merkle_consistency_verify: merkleConsistencyVerify,
  trie_verify: trieVerify,
  trie_prefix_verify: triePrefixVerify,
  trie_range_verify: trieRangeVerify,
});
