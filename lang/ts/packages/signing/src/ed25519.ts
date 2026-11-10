/** Ed25519 (`signing.md` 1.1) and Ed25519-BIP32 signing (1.2). */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { ed25519 as noble } from "@noble/curves/ed25519.js";

import { invalidKey } from "./errors.js";
import type { Signature, SigningKey, VerifyingKey } from "./keys.js";
import { equal, leToBig } from "./internal/bytes.js";
import {
  canonicalY,
  decodeZip215,
  hashToScalar,
  isSmallOrder,
  L,
  mulBase,
  mulPublic,
  Point,
  scalarBytes,
} from "./internal/edwards.js";
import {
  checkedPublicKey,
  checkedSign,
  checkedVerify,
  seedIsKey,
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

const SIG = 64;

/**
 * ZIP-215: `A` and `R` decode with non-canonical `y` and negative zero accepted, `S < L`, and the cofactored equation
 * `[8]([S]B - R - [k]A) = 0` with `k` over the encodings as given. This is noble's `zip215: true` rule, which the tests
 * pin case by case. Over this package's own plain bytes, unchecked.
 */
export function verifyZip215(key: Uint8Array, message: Uint8Array, signature: Uint8Array): boolean {
  return key.length === 32 && signature.length === SIG && noble.verify(signature, message, key, { zip215: true });
}

/**
 * libsodium's `crypto_sign_ed25519_verify_detached`: `S < L`; `A` with canonical `y` and not of small order; and the
 * cofactorless `[S]B - [k]A` encodes exactly to `R` and is not of small order. noble has no such rule, so it is built
 * on `Point`. Over this package's own plain bytes, unchecked.
 */
export function verifyLibsodiumRule(key: Uint8Array, message: Uint8Array, signature: Uint8Array): boolean {
  if (key.length !== 32 || signature.length !== SIG || !canonicalY(key)) return false;
  const s = leToBig(signature.subarray(32));
  if (s >= L) return false;
  const a = decodeZip215(key);
  if (a === undefined || isSmallOrder(a)) return false;
  const rBytes = signature.subarray(0, 32);
  const k = hashToScalar(rBytes, key, message);
  const r = mulPublic(Point.BASE, s).subtract(mulPublic(a, k));
  return equal(r.toBytes(), rBytes) && !isSmallOrder(r);
}

export interface Ed25519Scheme extends SchemeCodec<"Ed25519">, Verifier<"Ed25519">, SeedDerivation<"Ed25519"> {
  /** RFC 8032 section 5.1.6 (pure Ed25519). A key that is not an Ed25519 `SigningKey`, or a message that is not bytes, is a `TypeMismatch`. */
  readonly sign: (key: SigningKey<"Ed25519">, message: Uint8Array) => Result<Signature<"Ed25519">, TypeMismatch>;
  /** libsodium's rule (Cardano KES leaves); `verify` is ZIP-215. */
  readonly verifyLibsodium: (key: VerifyingKey<"Ed25519">, message: Uint8Array, signature: Signature<"Ed25519">) => Result<boolean, TypeMismatch>;
  /** libsodium's rule over raw encodings, decoded as `verifyBytes` decodes them. */
  readonly verifyLibsodiumBytes: (key: Uint8Array, message: Uint8Array, signature: Uint8Array) => Result<boolean, CryptoRefusal>;
}

const edCodec = Object.freeze<SchemeCodec<"Ed25519">>({
  name: "Ed25519",
  signingKey: (bytes) => {
    const b = sized("Ed25519 signing key", 32, bytes);
    return b.ok ? ok(signingKeyOf("Ed25519", b.value)) : b;
  },
  // ZIP-215 decoding: a non-canonical key is a key; one that is not a curve point is not
  verifyingKey: (bytes) => {
    const b = sized("Ed25519 verifying key", 32, bytes);
    if (!b.ok) return b;
    return decodeZip215(b.value) === undefined ? err(invalidKey("Ed25519 verifying key is not a curve point")) : ok(verifyingKeyOf("Ed25519", b.value));
  },
  // only the length: a non-canonical S, or an R that does not decode, verifies false
  signature: (bytes) => {
    const b = sized("Ed25519 signature", SIG, bytes);
    return b.ok ? ok(signatureOf("Ed25519", b.value)) : b;
  },
  publicKey: checkedPublicKey("Ed25519", (sk) => noble.getPublicKey(sk)),
});

export const ed25519: Ed25519Scheme = Object.freeze({
  ...edCodec,
  ...seedIsKey(edCodec),
  sign: checkedSign("Ed25519", (sk, message) => noble.sign(message, sk)),
  verify: checkedVerify("Ed25519", verifiedMessage, verifyZip215),
  verifyBytes: verifyBytesWith(edCodec, verifiedMessage, verifyZip215),
  verifyLibsodium: checkedVerify("Ed25519", verifiedMessage, verifyLibsodiumRule),
  verifyLibsodiumBytes: verifyBytesWith(edCodec, verifiedMessage, verifyLibsodiumRule),
});

// ------------------------------------------------------------------ Ed25519-BIP32 (Icarus, CIP-0003)

/** `kL` is a multiple of 8 with bit 255 clear. */
export const validKL = (kL: Uint8Array): boolean => kL.length === 32 && ((kL[0] ?? 0) & 0x07) === 0 && ((kL[31] ?? 0) & 0x80) === 0;

/** `A = [kL mod L]B`. */
export const extendedPublicKey = (kL: Uint8Array): Uint8Array => mulBase(leToBig(kL)).toBytes();

/** `r = SHA-512(kR || M) mod L`, `R = [r]B`, `S = r + SHA-512(R || A || M) kL mod L`; any `kL`, validated or not. */
export function signExtended(kL: Uint8Array, kR: Uint8Array, message: Uint8Array): Uint8Array {
  const a = extendedPublicKey(kL);
  const r = hashToScalar(kR, message);
  const rPoint = mulBase(r).toBytes();
  const k = hashToScalar(rPoint, a, message);
  const s = (r + k * (leToBig(kL) % L)) % L;
  const out = new Uint8Array(SIG);
  out.set(rPoint, 0);
  out.set(scalarBytes(s), 32);
  return out;
}

export interface Ed25519Bip32Scheme extends SchemeCodec<"Ed25519-BIP32">, Verifier<"Ed25519-BIP32"> {
  /** A key that is not an Ed25519-BIP32 `SigningKey`, or a message that is not bytes, is a `TypeMismatch`. */
  readonly sign: (key: SigningKey<"Ed25519-BIP32">, message: Uint8Array) => Result<Signature<"Ed25519-BIP32">, TypeMismatch>;
}

const bip32Codec = Object.freeze<SchemeCodec<"Ed25519-BIP32">>({
  name: "Ed25519-BIP32",
  signingKey: (bytes) => {
    const b = sized("Ed25519-BIP32 signing key", 64, bytes);
    if (!b.ok) return b;
    return validKL(b.value.subarray(0, 32)) ? ok(signingKeyOf("Ed25519-BIP32", b.value)) : err(invalidKey("kL must be a multiple of 8 with bit 255 clear"));
  },
  verifyingKey: (bytes) => {
    const b = sized("Ed25519 verifying key", 32, bytes);
    if (!b.ok) return b;
    return decodeZip215(b.value) === undefined ? err(invalidKey("Ed25519 verifying key is not a curve point")) : ok(verifyingKeyOf("Ed25519-BIP32", b.value));
  },
  signature: (bytes) => {
    const b = sized("Ed25519 signature", SIG, bytes);
    return b.ok ? ok(signatureOf("Ed25519-BIP32", b.value)) : b;
  },
  publicKey: checkedPublicKey("Ed25519-BIP32", (sk) => extendedPublicKey(sk.subarray(0, 32))),
});

export const ed25519Bip32: Ed25519Bip32Scheme = Object.freeze({
  ...bip32Codec,
  sign: checkedSign("Ed25519-BIP32", (sk, message) => signExtended(sk.subarray(0, 32), sk.subarray(32, 64), message)),
  verify: checkedVerify("Ed25519-BIP32", verifiedMessage, verifyZip215),
  verifyBytes: verifyBytesWith(bip32Codec, verifiedMessage, verifyZip215),
});
