/**
 * BLS12-381, Ethereum consensus profile (`signing.md` 1.6): minimal-pubkey-size (keys in G1, 48 bytes; signatures in
 * G2, 96 bytes), ciphersuite `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`, over noble's long-signature variant.
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { bls12_381 } from "@noble/curves/bls12-381.js";
import { expand, extract } from "@noble/hashes/hkdf.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { utf8ToBytes } from "@noble/hashes/utils.js";

import { invalidEncoding, invalidKey, lengthError, type CryptoError } from "./errors.js";
import { signatureBytes, signingKeyBytes, verifyingKeyBytes } from "./keys.js";
import { expectedAt, NOT_A_NUMBER, NOT_BYTES, readArray, verdict, viewOf } from "./internal/check.js";
import type { KeyPair, Signature, SigningKey, VerifyingKey } from "./keys.js";
import { isArrayGuarded, readGuarded, takeBytes } from "@kd5ujc/dilf4s-core/internal";

import { attempt } from "./internal/attempt.js";
import { beToBig, bigToBE, concat, u32be } from "./internal/bytes.js";
import {
  checkedPublicKey,
  checkedSign,
  checkedVerify,
  signatureOf,
  signingKeyOf,
  sized,
  verifiedMessage,
  verifyBytesWith,
  verifyingKeyOf,
  type CryptoRefusal,
  type SchemeCodec,
  type Verifier,
} from "./scheme.js";

const NAME = "BLS12-381-PoP";
export const SIGNATURE_DST = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_";
export const POP_DST = "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_";
const SIGNATURE_DST_BYTES = utf8ToBytes(SIGNATURE_DST);
const POP_DST_BYTES = utf8ToBytes(POP_DST);

const sigs = bls12_381.longSignatures;
const G1 = bls12_381.G1.Point;
const G2 = bls12_381.G2.Point;
type G1Point = ReturnType<typeof G1.fromAffine>;
type G2Point = ReturnType<typeof G2.fromAffine>;

/** The order `r` of G1, G2 and the scalar field. */
export const R: bigint = bls12_381.fields.Fr.ORDER;

/** The compressed identity of G2: flags `c0`, then zeros. */
const G2_INFINITY = concat(Uint8Array.of(0xc0), new Uint8Array(95));

/** KeyValidate: a compressed G1 point in the subgroup, not the identity. */
function g1(bytes: Uint8Array): Result<G1Point, CryptoError> {
  if (bytes.length !== 48) return err(lengthError("BLS public key", 48, bytes.length));
  const p = attempt(() => G1.fromBytes(bytes));
  if (p === undefined) return err(invalidKey("BLS public key does not decode"));
  return !p.is0() && attempt(() => p.isTorsionFree()) === true ? ok(p) : err(invalidKey("BLS public key is the identity or outside G1"));
}

/** A compressed G2 point in the subgroup; the identity decodes (and never verifies). */
function g2(bytes: Uint8Array): Result<G2Point, CryptoError> {
  if (bytes.length !== 96) return err(lengthError("BLS signature", 96, bytes.length));
  const p = attempt(() => sigs.Signature.fromBytes(bytes));
  if (p === undefined) return err(invalidEncoding("BLS signature", "does not decode"));
  return p.is0() || attempt(() => p.isTorsionFree()) === true ? ok(p) : err(invalidEncoding("BLS signature", "outside G2"));
}

const encodeG2 = (p: G2Point): Uint8Array => (p.is0() ? G2_INFINITY.slice() : sigs.Signature.toBytes(p));

const codec: SchemeCodec<typeof NAME> = {
  name: NAME,
  signingKey: (bytes) => {
    const b = sized("BLS signing key", 32, bytes);
    if (!b.ok) return b;
    const x = beToBig(b.value);
    return x > 0n && x < R ? ok(signingKeyOf(NAME, b.value)) : err(invalidKey("BLS secret key must be a 32-byte scalar in [1, r)"));
  },
  verifyingKey: (bytes) => {
    const b = sized("BLS public key", 48, bytes);
    if (!b.ok) return b;
    const p = g1(b.value);
    return p.ok ? ok(verifyingKeyOf(NAME, b.value)) : p;
  },
  signature: (bytes) => {
    const b = sized("BLS signature", 96, bytes);
    if (!b.ok) return b;
    const p = g2(b.value);
    return p.ok ? ok(signatureOf(NAME, b.value)) : p;
  },
  publicKey: checkedPublicKey(NAME, (sk) => publicOf(sk)),
};

/** The compressed public key of a secret key's bytes. */
const publicOf = (sk: Uint8Array): Uint8Array => G1.BASE.multiply(beToBig(sk)).toBytes(true);

/** A signature's bytes by the secret `sk` over `message` under `dst`, all owned and checked. */
const signRaw = (sk: Uint8Array, message: Uint8Array, dst: Uint8Array): Uint8Array => encodeG2(sigs.sign(sigs.hash(message, dst), sk));

function verifyWith(key: Uint8Array, message: Uint8Array, signature: Uint8Array, dst: Uint8Array): boolean {
  const pk = g1(key);
  const sig = g2(signature);
  if (!pk.ok || !sig.ok || sig.value.is0()) return false;
  // noble's verify returns false when the pairing check fails, and catches pairing errors itself
  return attempt(() => sigs.verify(sig.value, sigs.hash(message, dst), pk.value)) === true;
}

const verifyRaw = (key: Uint8Array, message: Uint8Array, signature: Uint8Array): boolean => verifyWith(key, message, signature, SIGNATURE_DST_BYTES);

function sumG1(keys: readonly Uint8Array[]): G1Point | undefined {
  let acc = G1.ZERO;
  for (const k of keys) {
    const p = g1(k);
    if (!p.ok) return undefined;
    acc = acc.add(p.value);
  }
  return acc;
}

/** A caller's keys: an array of this scheme's `VerifyingKey`s, read by index, each key's own bytes. */
const keysArg = (keys: unknown): Result<Uint8Array[], TypeMismatch> =>
  readArray(keys, "an array of BLS12-381-PoP VerifyingKeys", (k, i) => {
    const b = verifyingKeyBytes(k, NAME);
    return b.ok ? b : { ok: false, error: expectedAt("a BLS12-381-PoP VerifyingKey", i) };
  });

/** A caller's array of tuples, each read by index: its items checked by `items`, in order, refused by its place. */
function tuplesArg<T>(x: unknown, what: string, items: (tuple: readonly unknown[]) => T | undefined): Result<T[], TypeMismatch> {
  return readArray(x, `an array of ${what}`, (t, i) => {
    const checked = isArrayGuarded(t) ? items(t) : undefined;
    return checked === undefined ? { ok: false, error: expectedAt(what, i) } : { ok: true, value: checked };
  });
}

/** The bytes of a key, message or signature in a tuple: this scheme's key or signature, or a message's plain view. */
const keyIn = (x: unknown): Uint8Array | undefined => {
  const b = verifyingKeyBytes(x, NAME);
  return b.ok ? b.value : undefined;
};
const signatureIn = (x: unknown): Uint8Array | undefined => {
  const b = signatureBytes(x, NAME);
  return b.ok ? b.value : undefined;
};

// ------------------------------------------------------------------ EIP-2333

/** `HKDF_mod_r(IKM, key_info = "")`: the EIP-2333 key derivation into `[1, r)`. */
function hkdfModR(ikm: Uint8Array): bigint {
  let salt = utf8ToBytes("BLS-SIG-KEYGEN-SALT-");
  let sk = 0n;
  while (sk === 0n) {
    salt = sha256(salt);
    const prk = extract(sha256, concat(ikm, Uint8Array.of(0)), salt);
    sk = beToBig(expand(sha256, prk, Uint8Array.of(0, 48), 48)) % R;
  }
  return sk;
}

function lamportPublicKey(parent: Uint8Array, index: number): Uint8Array {
  const salt = u32be(index);
  const notIkm = parent.map((b) => b ^ 0xff);
  const chunks = (ikm: Uint8Array): Uint8Array[] => {
    const okm = expand(sha256, extract(sha256, ikm, salt), new Uint8Array(0), 255 * 32);
    return Array.from({ length: 255 }, (_, i) => sha256(okm.subarray(32 * i, 32 * i + 32)));
  };
  return sha256(concat(...chunks(parent), ...chunks(notIkm)));
}

export interface BlsScheme extends SchemeCodec<typeof NAME>, Verifier<typeof NAME> {
  /** A key that is not this scheme's `SigningKey`, or a message that is not bytes, is a `TypeMismatch`. */
  readonly sign: (key: SigningKey<typeof NAME>, message: Uint8Array) => Result<Signature<typeof NAME>, TypeMismatch>;
  /** The sum of one or more signatures; an empty list is an error. Signatures not this scheme's are a `TypeMismatch`. */
  readonly aggregate: (signatures: readonly Signature<typeof NAME>[]) => Result<Signature<typeof NAME>, CryptoRefusal>;
  /**
   * One message signed by every key (the keys must carry proofs of possession); no keys verifies false. Keys, a message
   * or a signature of the wrong type are a `TypeMismatch`, never a verdict.
   */
  readonly fastAggregateVerify: (keys: readonly VerifyingKey<typeof NAME>[], message: Uint8Array, signature: Signature<typeof NAME>) => Result<boolean, TypeMismatch>;
  /** `prod e(pk_i, H(m_i)) == e(g1, sig)`; no pairs, or the identity signature, verifies false. Checked as above. */
  readonly aggregateVerify: (
    pairs: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array])[],
    signature: Signature<typeof NAME>,
  ) => Result<boolean, TypeMismatch>;
  /** Every (key, message, signature) triple verifies; an empty list verifies false. Every triple is checked first. */
  readonly batchVerify: (triples: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array, Signature<typeof NAME>])[]) => Result<boolean, TypeMismatch>;
  /** A signature over the public key under DST `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`. */
  readonly popProve: (key: SigningKey<typeof NAME>) => Result<Signature<typeof NAME>, TypeMismatch>;
  /** Whether `proof` is `key`'s proof of possession; a key or proof not of this scheme is a `TypeMismatch`. */
  readonly popVerify: (key: VerifyingKey<typeof NAME>, proof: Signature<typeof NAME>) => Result<boolean, TypeMismatch>;
  /** `hash_to_curve` into G2 under `dst` (1 to 255 bytes), uncompressed `x.c1 || x.c0 || y.c1 || y.c0` (192 bytes). */
  readonly hashToG2: (message: Uint8Array, dst: Uint8Array) => Result<Uint8Array, CryptoRefusal>;
  /** EIP-2333 `derive_master_SK` (seeds of at least 32 bytes). */
  readonly deriveMasterSk: (seed: Uint8Array) => Result<SigningKey<typeof NAME>, CryptoRefusal>;
  /** EIP-2333 `derive_child_SK` (32-bit indices). A parent not this scheme's, or an index not a number, is a `TypeMismatch`. */
  readonly deriveChildSk: (parent: SigningKey<typeof NAME>, index: number) => Result<SigningKey<typeof NAME>, CryptoRefusal>;
  /** Key pairs from a seed through EIP-2333 `derive_master_SK` (`signing.md` 1.7). */
  readonly fromSeed: (seed: Uint8Array) => Result<KeyPair<typeof NAME>, CryptoRefusal>;
}

/** EIP-2333 `derive_master_SK`: the seed measured by its internal length, and read once, as a copy. */
function deriveMasterSk(seed: Uint8Array): Result<SigningKey<typeof NAME>, CryptoRefusal> {
  const view = viewOf(seed);
  if (view === undefined) return err(NOT_BYTES);
  return view.length >= 32 ? codec.signingKey(bigToBE(hkdfModR(Uint8Array.from(view)), 32)) : err(invalidKey("EIP-2333 seeds are at least 32 bytes"));
}


export const blsPop: BlsScheme = Object.freeze({
  ...codec,
  sign: checkedSign(NAME, (sk, message) => signRaw(sk, message, SIGNATURE_DST_BYTES)),
  verify: checkedVerify(NAME, verifiedMessage, verifyRaw),
  verifyBytes: verifyBytesWith(codec, verifiedMessage, verifyRaw),
  aggregate: (signatures: readonly Signature<typeof NAME>[]) => {
    const given = readArray(signatures, "an array of BLS12-381-PoP Signatures", (s, i) => {
      const b = signatureBytes(s, NAME);
      return b.ok ? b : { ok: false as const, error: expectedAt("a BLS12-381-PoP Signature", i) };
    });
    if (!given.ok) return given;
    if (given.value.length === 0) return err(invalidEncoding("BLS aggregate", "at least one signature is required"));
    let acc = G2.ZERO;
    for (const s of given.value) {
      const p = g2(s);
      if (!p.ok) return p;
      acc = acc.add(p.value);
    }
    return ok(signatureOf(NAME, encodeG2(acc)));
  },
  fastAggregateVerify: (keys: readonly VerifyingKey<typeof NAME>[], message: Uint8Array, signature: Signature<typeof NAME>) => {
    const ks = keysArg(keys);
    if (!ks.ok) return ks;
    const m = viewOf(message);
    if (m === undefined) return err(NOT_BYTES);
    const sb = signatureBytes(signature, NAME);
    if (!sb.ok) return sb;
    const sum = ks.value.length === 0 ? undefined : sumG1(ks.value);
    const sig = g2(sb.value);
    if (sum === undefined || sum.is0() || !sig.ok || sig.value.is0()) return verdict(false);
    return verdict(attempt(() => sigs.verify(sig.value, sigs.hash(m, SIGNATURE_DST_BYTES), sum)) === true);
  },
  aggregateVerify: (pairs: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array])[], signature: Signature<typeof NAME>) => {
    const given = tuplesArg(pairs, "a [VerifyingKey, Uint8Array] pair", (t) => {
      // each read guarded (a Proxy's trap may revoke the tuple); copied as read: caller code run by a later read
      // cannot change an earlier message
      const k = keyIn(readGuarded(t, 0));
      const m = takeBytes(readGuarded(t, 1));
      return k === undefined || m === undefined ? undefined : ([k, m] as const);
    });
    if (!given.ok) return given;
    const sb = signatureBytes(signature, NAME);
    if (!sb.ok) return sb;
    const sig = g2(sb.value);
    if (given.value.length === 0 || !sig.ok || sig.value.is0()) return verdict(false);
    const items: { message: G2Point; publicKey: G1Point }[] = [];
    for (const [k, m] of given.value) {
      const pk = g1(k);
      if (!pk.ok) return verdict(false);
      items.push({ message: sigs.hash(m, SIGNATURE_DST_BYTES), publicKey: pk.value });
    }
    return verdict(attempt(() => sigs.verifyBatch(sig.value, items)) === true);
  },
  batchVerify: (triples: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array, Signature<typeof NAME>])[]) => {
    const given = tuplesArg(triples, "a [VerifyingKey, Uint8Array, Signature] triple", (t) => {
      const k = keyIn(readGuarded(t, 0));
      const m = takeBytes(readGuarded(t, 1));
      const s = signatureIn(readGuarded(t, 2));
      return k === undefined || m === undefined || s === undefined ? undefined : ([k, m, s] as const);
    });
    if (!given.ok) return given;
    return verdict(given.value.length > 0 && given.value.every(([k, m, s]) => verifyRaw(k, m, s)));
  },
  popProve: (key: SigningKey<typeof NAME>) => {
    const k = signingKeyBytes(key, NAME);
    return k.ok ? ok(signatureOf(NAME, signRaw(k.value, publicOf(k.value), POP_DST_BYTES))) : k;
  },
  popVerify: (key: VerifyingKey<typeof NAME>, proof: Signature<typeof NAME>) => {
    const k = verifyingKeyBytes(key, NAME);
    if (!k.ok) return k;
    const p = signatureBytes(proof, NAME);
    return p.ok ? verdict(verifyWith(k.value, k.value, p.value, POP_DST_BYTES)) : p;
  },
  hashToG2: (message: Uint8Array, dst: Uint8Array) => {
    const m = viewOf(message);
    const d = viewOf(dst);
    if (m === undefined || d === undefined) return err(NOT_BYTES);
    if (d.length === 0 || d.length > 255) return err(invalidEncoding("hash-to-curve DST", "must be 1 to 255 bytes"));
    const p = bls12_381.G2.hashToCurve(m, { DST: Uint8Array.from(d) });
    return ok(p.toBytes(false));
  },
  deriveMasterSk,
  deriveChildSk: (parent: SigningKey<typeof NAME>, index: number) => {
    const k = signingKeyBytes(parent, NAME);
    if (!k.ok) return k;
    if (typeof (index as unknown) !== "number") return err(NOT_A_NUMBER);
    return Number.isInteger(index) && index >= 0 && index < 2 ** 32
      ? codec.signingKey(bigToBE(hkdfModR(lamportPublicKey(k.value, index)), 32))
      : err(invalidKey("EIP-2333 indices are 32-bit"));
  },
  fromSeed: (seed: Uint8Array) => {
    const sk = deriveMasterSk(seed);
    if (!sk.ok) return sk;
    const vk = codec.publicKey(sk.value);
    return vk.ok ? ok({ signing: sk.value, verifying: vk.value }) : vk;
  },
});
