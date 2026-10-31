/** ECDSA over P-256 and secp256k1 (`signing.md` 1.3): SHA-256(message), RFC 6979 nonces, compact `r || s`, low-S only. */
import { err, ok } from "@kd5ujc/dilf4s-core";
import type { ECDSA } from "@noble/curves/abstract/weierstrass.js";
import { p256 } from "@noble/curves/nist.js";
import { secp256k1 } from "@noble/curves/secp256k1.js";

import { invalidEncoding, invalidKey } from "./errors.js";
import type { Signature, SigningKey, VerifyingKey } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { beToBig } from "./internal/bytes.js";
import {
  seedIsKey,
  signatureOf,
  signingKeyOf,
  sized,
  verifyBytesWith,
  verifyingKeyOf,
  type SchemeCodec,
  type SeedDerivation,
  type Verifier,
} from "./scheme.js";

export type EcdsaSchemeName = "P-256-ECDSA" | "secp256k1-ECDSA";

export interface EcdsaScheme<S extends EcdsaSchemeName> extends SchemeCodec<S>, Verifier<S>, SeedDerivation<S> {
  /** Deterministic (RFC 6979 with HMAC-SHA-256 over SHA-256(message)); `s` is normalised low. */
  readonly sign: (key: SigningKey<S>, message: Uint8Array) => Signature<S>;
}

// every option is set explicitly: noble's defaults are not part of the contract
const SIGN_OPTS = { prehash: true, lowS: true, format: "compact", extraEntropy: false } as const;
const VERIFY_OPTS = { prehash: true, lowS: true, format: "compact" } as const;

function ecdsaScheme<S extends EcdsaSchemeName>(name: S, curve: ECDSA, label: string): EcdsaScheme<S> {
  const n = curve.Point.Fn.ORDER;
  const half = n / 2n;
  const codec: SchemeCodec<S> = {
    name,
    signingKey: (bytes) => {
      const b = sized(`${label} signing key`, 32, bytes);
      if (!b.ok) return b;
      const d = beToBig(bytes);
      return d >= 1n && d < n ? ok(signingKeyOf(name, bytes)) : err(invalidKey(`${label} secret key must be a scalar in [1, n)`));
    },
    // 33-byte compressed SEC1, on the curve (noble's decoder alone would also take 65-byte keys)
    verifyingKey: (bytes) => {
      const b = sized(`${label} verifying key`, 33, bytes);
      if (!b.ok) return b;
      const prefixOk = bytes[0] === 0x02 || bytes[0] === 0x03;
      return prefixOk && attempt(() => curve.Point.fromBytes(bytes)) !== undefined
        ? ok(verifyingKeyOf(name, bytes))
        : err(invalidKey(`${label} verifying key is not a valid point`));
    },
    // r, s in [1, n) and s <= n/2: the high-S twin is a decode error (one encoding per signature)
    signature: (bytes) => {
      const b = sized(`${label} signature`, 64, bytes);
      if (!b.ok) return b;
      const r = beToBig(bytes.subarray(0, 32));
      const s = beToBig(bytes.subarray(32, 64));
      return r >= 1n && r < n && s >= 1n && s <= half
        ? ok(signatureOf(name, bytes))
        : err(invalidEncoding(`${label} signature`, "r and s must be in range and s must be low (canonical)"));
    },
    publicKey: (key) => verifyingKeyOf(name, curve.getPublicKey(key.bytes, true)),
  };
  const verify = (key: VerifyingKey<S>, message: Uint8Array, signature: Signature<S>): boolean =>
    curve.verify(signature.bytes, message, key.bytes, VERIFY_OPTS);
  return Object.freeze({
    ...codec,
    ...seedIsKey(codec),
    sign: (key: SigningKey<S>, message: Uint8Array) => signatureOf(name, curve.sign(message, key.bytes, SIGN_OPTS)),
    verify,
    verifyBytes: verifyBytesWith(codec, verify),
  });
}

export const p256Ecdsa: EcdsaScheme<"P-256-ECDSA"> = ecdsaScheme("P-256-ECDSA", p256, "P-256");
export const secp256k1Ecdsa: EcdsaScheme<"secp256k1-ECDSA"> = ecdsaScheme("secp256k1-ECDSA", secp256k1, "secp256k1");
