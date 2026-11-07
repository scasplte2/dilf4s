/** ristretto255 Schnorr, the single-signer form of FROST(ristretto255, SHA-512), RFC 9591 (`signing.md` 1.5). */
import { err, ok, type Err, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { copyBytes, plainBytes } from "@kd5ujc/dilf4s-core/internal";
import { ristretto255 } from "@noble/curves/ed25519.js";
import { sha512 } from "@noble/hashes/sha2.js";
import { randomBytes, utf8ToBytes } from "@noble/hashes/utils.js";

import { invalidEncoding, invalidKey, lengthError } from "./errors.js";
import { ownSigningKey, type Signature, type SigningKey } from "./keys.js";
import { expected, NOT_BYTES, signedOf } from "./internal/check.js";
import { attempt } from "./internal/attempt.js";
import { concat, leToBig, mod } from "./internal/bytes.js";
import { L, scalarBytes } from "./internal/edwards.js";
import {
  checkedPublicKey,
  checkedVerify,
  signatureOf,
  signingKeyOf,
  sized,
  verifiedMessage,
  verifyBytesWith,
  verifyingKeyOf,
  type CryptoRefusal,
  type SchemeCodec,
  type SeedDerivation,
  type Verifier,
} from "./scheme.js";

const RPoint = ristretto255.Point;
type RistrettoPoint = InstanceType<typeof ristretto255.Point>;
const NAME = "ristretto255-Schnorr";
const CONTEXT = "FROST-RISTRETTO255-SHA512-v1";
const H2 = utf8ToBytes(`${CONTEXT}chal`);
const H3 = utf8ToBytes(`${CONTEXT}nonce`);

const hashToScalar = (label: Uint8Array, ...parts: readonly Uint8Array[]): bigint => mod(leToBig(sha512(concat(label, ...parts))), L);

/** RFC 9591 `DeserializeElement`: a canonical encoding of a non-identity element. */
function element(b: Uint8Array): RistrettoPoint | undefined {
  const e = b.length === 32 ? attempt(() => RPoint.fromBytes(b)) : undefined;
  return e === undefined || e.is0() ? undefined : e;
}

function canonicalScalar(b: Uint8Array): bigint | undefined {
  const s = leToBig(b);
  return b.length === 32 && s < L ? s : undefined;
}

const mulBase = (s: bigint): RistrettoPoint => (s === 0n ? RPoint.ZERO : RPoint.BASE.multiply(s));

const codec: SchemeCodec<typeof NAME> = {
  name: NAME,
  signingKey: (bytes) => {
    const b = sized("ristretto255 signing key", 32, bytes);
    if (!b.ok) return b;
    const s = canonicalScalar(b.value);
    return s !== undefined && s !== 0n ? ok(signingKeyOf(NAME, b.value)) : err(invalidKey("ristretto255 secret key must be a canonical non-zero scalar"));
  },
  verifyingKey: (bytes) => {
    const b = sized("ristretto255 verifying key", 32, bytes);
    if (!b.ok) return b;
    return element(b.value) === undefined
      ? err(invalidKey("ristretto255 verifying key is not a canonical non-identity element"))
      : ok(verifyingKeyOf(NAME, b.value));
  },
  signature: (bytes) => {
    const b = sized("ristretto255 signature", 64, bytes);
    if (!b.ok) return b;
    return element(b.value.subarray(0, 32)) !== undefined && canonicalScalar(b.value.subarray(32)) !== undefined
      ? ok(signatureOf(NAME, b.value))
      : err(invalidEncoding("ristretto255 signature", "expected R (non-identity element) || z (canonical scalar)"));
  },
  publicKey: checkedPublicKey(NAME, (sk) => publicOf(sk)),
};

/** The public key of a secret scalar's bytes. */
const publicOf = (sk: Uint8Array): Uint8Array => mulBase(leToBig(sk)).toBytes();

/** A signature by the secret `sk` over a message read once, with 32 bytes of nonce randomness, all owned and checked. */
function signWith(sk: Uint8Array, message: Uint8Array, randomness: Uint8Array): Signature<typeof NAME> {
  const x = leToBig(sk);
  const k = hashToScalar(H3, randomness, sk);
  const r = mulBase(k).toBytes();
  const c = hashToScalar(H2, r, publicOf(sk), message);
  return signatureOf(NAME, concat(r, scalarBytes(k + c * x)));
}

const NOT_A_KEY: Err<TypeMismatch> = Object.freeze(err(expected(`a ${NAME} SigningKey`)));
const NOT_A_MESSAGE: Err<TypeMismatch> = Object.freeze(err(NOT_BYTES));

/**
 * The caller's 32 bytes of nonce randomness, measured by their internal length and copied from their own memory (the
 * copy itself, no wrapper around it), or the refusal: `TypeMismatch` for what is not bytes, `InvalidEncoding` for
 * another length.
 */
function randomnessOf(x: unknown): Uint8Array | Err<CryptoRefusal> {
  const view = plainBytes(x);
  if (view === undefined) return NOT_A_MESSAGE;
  return view.length === 32 ? copyBytes(view) : err(lengthError("nonce randomness", 32, view.length));
}

/** Fresh nonce randomness from `@noble/hashes` `randomBytes`: it has nothing of the caller's to check. */
const freshRandomness = (): Uint8Array | Err<never> => randomBytes(32);

/**
 * A signature by `key` over `message` with the nonce randomness `randomness` reads from `given`, each checked in turn:
 * the key must be the scheme's (its own bytes read), the message is copied once, then the randomness. The functions
 * are the module's own, made once: a passing check allocates the two copies and no wrapper or closure.
 */
function signChecked<E>(key: unknown, message: unknown, given: unknown, randomness: (x: unknown) => Uint8Array | Err<E>): Result<Signature<typeof NAME>, TypeMismatch | E> {
  const k = ownSigningKey(key, NAME);
  if (k === undefined) return NOT_A_KEY;
  const m = signedOf(message);
  if (m === undefined) return NOT_A_MESSAGE;
  const r = randomness(given);
  return r instanceof Uint8Array ? ok(signWith(k, m, r)) : r;
}

const verify = (kb: Uint8Array, message: Uint8Array, sb: Uint8Array): boolean => {
  const pk = element(kb);
  const r = element(sb.subarray(0, 32));
  const z = canonicalScalar(sb.subarray(32));
  if (pk === undefined || r === undefined || z === undefined) return false;
  const c = hashToScalar(H2, sb.subarray(0, 32), kb, message);
  return RPoint.BASE.multiplyUnsafe(z).equals(r.add(pk.multiplyUnsafe(c)));
};

export interface Ristretto255Scheme extends SchemeCodec<typeof NAME>, Verifier<typeof NAME>, SeedDerivation<typeof NAME> {
  /** `k = H3(randomness || sk)`: caller-supplied 32 bytes of nonce randomness (known answers, external RNGs). */
  readonly sign: (key: SigningKey<typeof NAME>, message: Uint8Array, randomness: Uint8Array) => Result<Signature<typeof NAME>, CryptoRefusal>;
  /** With fresh randomness from `@noble/hashes` `randomBytes`. */
  readonly signRandom: (key: SigningKey<typeof NAME>, message: Uint8Array) => Result<Signature<typeof NAME>, TypeMismatch>;
}

export const ristretto255Schnorr: Ristretto255Scheme = Object.freeze({
  ...codec,
  sign: (key: SigningKey<typeof NAME>, message: Uint8Array, randomness: Uint8Array) => signChecked(key, message, randomness, randomnessOf),
  signRandom: (key: SigningKey<typeof NAME>, message: Uint8Array) => signChecked(key, message, undefined, freshRandomness),
  verify: checkedVerify(NAME, verifiedMessage, verify),
  verifyBytes: verifyBytesWith(codec, verifiedMessage, verify),
  /** `SHA-512(seed) mod L`, rejecting the (negligible) zero scalar. */
  fromSeed: (seed: Uint8Array) => {
    const s = sized("ristretto255 seed", 32, seed);
    if (!s.ok) return s;
    const sk = codec.signingKey(scalarBytes(leToBig(sha512(s.value))));
    if (!sk.ok) return sk;
    const vk = codec.publicKey(sk.value);
    return vk.ok ? ok({ signing: sk.value, verifying: vk.value }) : vk;
  },
});
