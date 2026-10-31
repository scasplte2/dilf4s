/**
 * BLS12-381, Ethereum consensus profile (`signing.md` 1.6): minimal-pubkey-size (keys in G1, 48 bytes; signatures in
 * G2, 96 bytes), ciphersuite `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`, over noble's long-signature variant.
 */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { bls12_381 } from "@noble/curves/bls12-381.js";
import { expand, extract } from "@noble/hashes/hkdf.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { utf8ToBytes } from "@noble/hashes/utils.js";

import { invalidEncoding, invalidKey, lengthError, type SigningError } from "./errors.js";
import type { KeyPair, Signature, SigningKey, VerifyingKey } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, bigToBE, concat, u32be } from "./internal/bytes.js";
import { signatureOf, signingKeyOf, sized, verifyBytesWith, verifyingKeyOf, type SchemeCodec, type Verifier } from "./scheme.js";

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
function g1(bytes: Uint8Array): Result<G1Point, SigningError> {
  if (bytes.length !== 48) return err(lengthError("BLS public key", 48, bytes.length));
  const p = attempt(() => G1.fromBytes(bytes));
  if (p === undefined) return err(invalidKey("BLS public key does not decode"));
  return !p.is0() && attempt(() => p.isTorsionFree()) === true ? ok(p) : err(invalidKey("BLS public key is the identity or outside G1"));
}

/** A compressed G2 point in the subgroup; the identity decodes (and never verifies). */
function g2(bytes: Uint8Array): Result<G2Point, SigningError> {
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
    const x = beToBig(bytes);
    return x > 0n && x < R ? ok(signingKeyOf(NAME, bytes)) : err(invalidKey("BLS secret key must be a 32-byte scalar in [1, r)"));
  },
  verifyingKey: (bytes) => {
    const p = g1(bytes);
    return p.ok ? ok(verifyingKeyOf(NAME, bytes)) : p;
  },
  signature: (bytes) => {
    const p = g2(bytes);
    return p.ok ? ok(signatureOf(NAME, bytes)) : p;
  },
  publicKey: (key) => verifyingKeyOf(NAME, G1.BASE.multiply(beToBig(key.bytes)).toBytes(true)),
};

const signWith = (key: SigningKey<typeof NAME>, message: Uint8Array, dst: Uint8Array): Signature<typeof NAME> =>
  signatureOf(NAME, encodeG2(sigs.sign(sigs.hash(message, dst), key.bytes)));

function verifyWith(key: Uint8Array, message: Uint8Array, signature: Uint8Array, dst: Uint8Array): boolean {
  const pk = g1(key);
  const sig = g2(signature);
  if (!pk.ok || !sig.ok || sig.value.is0()) return false;
  // noble's verify returns false when the pairing check fails, and catches pairing errors itself
  return attempt(() => sigs.verify(sig.value, sigs.hash(message, dst), pk.value)) === true;
}

const verify = (key: VerifyingKey<typeof NAME>, message: Uint8Array, signature: Signature<typeof NAME>): boolean =>
  verifyWith(key.bytes, message, signature.bytes, SIGNATURE_DST_BYTES);

function sumG1(keys: readonly VerifyingKey<typeof NAME>[]): G1Point | undefined {
  let acc = G1.ZERO;
  for (const k of keys) {
    const p = g1(k.bytes);
    if (!p.ok) return undefined;
    acc = acc.add(p.value);
  }
  return acc;
}

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
  readonly sign: (key: SigningKey<typeof NAME>, message: Uint8Array) => Signature<typeof NAME>;
  /** The sum of one or more signatures; an empty list is an error. */
  readonly aggregate: (signatures: readonly Signature<typeof NAME>[]) => Result<Signature<typeof NAME>, SigningError>;
  /** One message signed by every key (the keys must carry proofs of possession); no keys verifies false. */
  readonly fastAggregateVerify: (keys: readonly VerifyingKey<typeof NAME>[], message: Uint8Array, signature: Signature<typeof NAME>) => boolean;
  /** `prod e(pk_i, H(m_i)) == e(g1, sig)`; no pairs, or the identity signature, verifies false. */
  readonly aggregateVerify: (
    pairs: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array])[],
    signature: Signature<typeof NAME>,
  ) => boolean;
  /** Every (key, message, signature) triple verifies; an empty list verifies false. */
  readonly batchVerify: (triples: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array, Signature<typeof NAME>])[]) => boolean;
  /** A signature over the public key under DST `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`. */
  readonly popProve: (key: SigningKey<typeof NAME>) => Signature<typeof NAME>;
  readonly popVerify: (key: VerifyingKey<typeof NAME>, proof: Signature<typeof NAME>) => boolean;
  /** `hash_to_curve` into G2 under `dst` (1 to 255 bytes), uncompressed `x.c1 || x.c0 || y.c1 || y.c0` (192 bytes). */
  readonly hashToG2: (message: Uint8Array, dst: Uint8Array) => Result<Uint8Array, SigningError>;
  /** EIP-2333 `derive_master_SK` (seeds of at least 32 bytes). */
  readonly deriveMasterSk: (seed: Uint8Array) => Result<SigningKey<typeof NAME>, SigningError>;
  /** EIP-2333 `derive_child_SK` (32-bit indices). */
  readonly deriveChildSk: (parent: SigningKey<typeof NAME>, index: number) => Result<SigningKey<typeof NAME>, SigningError>;
  /** Key pairs from a seed through EIP-2333 `derive_master_SK` (`signing.md` 1.7). */
  readonly fromSeed: (seed: Uint8Array) => Result<KeyPair<typeof NAME>, SigningError>;
}

const deriveMasterSk = (seed: Uint8Array): Result<SigningKey<typeof NAME>, SigningError> =>
  seed.length >= 32 ? codec.signingKey(bigToBE(hkdfModR(seed), 32)) : err(invalidKey("EIP-2333 seeds are at least 32 bytes"));

export const blsPop: BlsScheme = Object.freeze({
  ...codec,
  sign: (key: SigningKey<typeof NAME>, message: Uint8Array) => signWith(key, message, SIGNATURE_DST_BYTES),
  verify,
  verifyBytes: verifyBytesWith(codec, verify),
  aggregate: (signatures: readonly Signature<typeof NAME>[]) => {
    if (signatures.length === 0) return err(invalidEncoding("BLS aggregate", "at least one signature is required"));
    let acc = G2.ZERO;
    for (const s of signatures) {
      const p = g2(s.bytes);
      if (!p.ok) return p;
      acc = acc.add(p.value);
    }
    return ok(signatureOf(NAME, encodeG2(acc)));
  },
  fastAggregateVerify: (keys: readonly VerifyingKey<typeof NAME>[], message: Uint8Array, signature: Signature<typeof NAME>) => {
    const sum = keys.length === 0 ? undefined : sumG1(keys);
    const sig = g2(signature.bytes);
    if (sum === undefined || sum.is0() || !sig.ok || sig.value.is0()) return false;
    return attempt(() => sigs.verify(sig.value, sigs.hash(message, SIGNATURE_DST_BYTES), sum)) === true;
  },
  aggregateVerify: (pairs: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array])[], signature: Signature<typeof NAME>) => {
    const sig = g2(signature.bytes);
    if (pairs.length === 0 || !sig.ok || sig.value.is0()) return false;
    const items: { message: G2Point; publicKey: G1Point }[] = [];
    for (const [k, m] of pairs) {
      const pk = g1(k.bytes);
      if (!pk.ok) return false;
      items.push({ message: sigs.hash(m, SIGNATURE_DST_BYTES), publicKey: pk.value });
    }
    return attempt(() => sigs.verifyBatch(sig.value, items)) === true;
  },
  batchVerify: (triples: readonly (readonly [VerifyingKey<typeof NAME>, Uint8Array, Signature<typeof NAME>])[]) =>
    triples.length > 0 && triples.every(([k, m, s]) => verify(k, m, s)),
  popProve: (key: SigningKey<typeof NAME>) => signWith(key, codec.publicKey(key).bytes, POP_DST_BYTES),
  popVerify: (key: VerifyingKey<typeof NAME>, proof: Signature<typeof NAME>) => verifyWith(key.bytes, key.bytes, proof.bytes, POP_DST_BYTES),
  hashToG2: (message: Uint8Array, dst: Uint8Array) => {
    if (dst.length === 0 || dst.length > 255) return err(invalidEncoding("hash-to-curve DST", "must be 1 to 255 bytes"));
    const p = bls12_381.G2.hashToCurve(message, { DST: dst });
    return ok(p.toBytes(false));
  },
  deriveMasterSk,
  deriveChildSk: (parent: SigningKey<typeof NAME>, index: number) =>
    Number.isInteger(index) && index >= 0 && index < 2 ** 32
      ? codec.signingKey(bigToBE(hkdfModR(lamportPublicKey(parent.bytes, index)), 32))
      : err(invalidKey("EIP-2333 indices are 32-bit")),
  fromSeed: (seed: Uint8Array) => {
    const sk = deriveMasterSk(seed);
    return sk.ok ? ok({ signing: sk.value, verifying: codec.publicKey(sk.value) }) : sk;
  },
});
