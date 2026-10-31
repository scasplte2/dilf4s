/** ristretto255 Schnorr, the single-signer form of FROST(ristretto255, SHA-512), RFC 9591 (`signing.md` 1.5). */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { ristretto255 } from "@noble/curves/ed25519.js";
import { sha512 } from "@noble/hashes/sha2.js";
import { randomBytes, utf8ToBytes } from "@noble/hashes/utils.js";

import { invalidEncoding, invalidKey, type SigningError } from "./errors.js";
import type { Signature, SigningKey, VerifyingKey } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { concat, leToBig, mod } from "./internal/bytes.js";
import { L, scalarBytes } from "./internal/edwards.js";
import {
  signatureOf,
  signingKeyOf,
  sized,
  verifyBytesWith,
  verifyingKeyOf,
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
    const s = canonicalScalar(bytes);
    return s !== undefined && s !== 0n ? ok(signingKeyOf(NAME, bytes)) : err(invalidKey("ristretto255 secret key must be a canonical non-zero scalar"));
  },
  verifyingKey: (bytes) => {
    const b = sized("ristretto255 verifying key", 32, bytes);
    if (!b.ok) return b;
    return element(bytes) === undefined
      ? err(invalidKey("ristretto255 verifying key is not a canonical non-identity element"))
      : ok(verifyingKeyOf(NAME, bytes));
  },
  signature: (bytes) => {
    const b = sized("ristretto255 signature", 64, bytes);
    if (!b.ok) return b;
    return element(bytes.subarray(0, 32)) !== undefined && canonicalScalar(bytes.subarray(32)) !== undefined
      ? ok(signatureOf(NAME, bytes))
      : err(invalidEncoding("ristretto255 signature", "expected R (non-identity element) || z (canonical scalar)"));
  },
  publicKey: (key) => verifyingKeyOf(NAME, mulBase(leToBig(key.bytes)).toBytes()),
};

function signWith(key: SigningKey<typeof NAME>, message: Uint8Array, randomness: Uint8Array): Signature<typeof NAME> {
  const sk = key.bytes;
  const x = leToBig(sk);
  const k = hashToScalar(H3, randomness, sk);
  const r = mulBase(k).toBytes();
  const c = hashToScalar(H2, r, codec.publicKey(key).bytes, message);
  return signatureOf(NAME, concat(r, scalarBytes(k + c * x)));
}

const verify = (key: VerifyingKey<typeof NAME>, message: Uint8Array, signature: Signature<typeof NAME>): boolean => {
  const kb = key.bytes;
  const sb = signature.bytes;
  const pk = element(kb);
  const r = element(sb.subarray(0, 32));
  const z = canonicalScalar(sb.subarray(32));
  if (pk === undefined || r === undefined || z === undefined) return false;
  const c = hashToScalar(H2, sb.subarray(0, 32), kb, message);
  return RPoint.BASE.multiplyUnsafe(z).equals(r.add(pk.multiplyUnsafe(c)));
};

export interface Ristretto255Scheme extends SchemeCodec<typeof NAME>, Verifier<typeof NAME>, SeedDerivation<typeof NAME> {
  /** `k = H3(randomness || sk)`: caller-supplied 32 bytes of nonce randomness (known answers, external RNGs). */
  readonly sign: (key: SigningKey<typeof NAME>, message: Uint8Array, randomness: Uint8Array) => Result<Signature<typeof NAME>, SigningError>;
  /** With fresh randomness from `@noble/hashes` `randomBytes`. */
  readonly signRandom: (key: SigningKey<typeof NAME>, message: Uint8Array) => Signature<typeof NAME>;
}

export const ristretto255Schnorr: Ristretto255Scheme = Object.freeze({
  ...codec,
  sign: (key: SigningKey<typeof NAME>, message: Uint8Array, randomness: Uint8Array) => {
    const r = sized("nonce randomness", 32, randomness);
    return r.ok ? ok(signWith(key, message, r.value)) : r;
  },
  signRandom: (key: SigningKey<typeof NAME>, message: Uint8Array) => signWith(key, message, randomBytes(32)),
  verify,
  verifyBytes: verifyBytesWith(codec, verify),
  /** `SHA-512(seed) mod L`, rejecting the (negligible) zero scalar. */
  fromSeed: (seed: Uint8Array) => {
    const s = sized("ristretto255 seed", 32, seed);
    if (!s.ok) return s;
    const sk = codec.signingKey(scalarBytes(leToBig(sha512(seed))));
    return sk.ok ? ok({ signing: sk.value, verifying: codec.publicKey(sk.value) }) : sk;
  },
});
