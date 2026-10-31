/**
 * BIP-340 and dilf4s tagged Schnorr over secp256k1 (`signing.md` 1.4). The two schemes share key encodings, but
 * neither accepts the other's signatures, and the types keep them apart.
 */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { schnorr, secp256k1 } from "@noble/curves/secp256k1.js";
import { randomBytes } from "@noble/hashes/utils.js";

import { invalidKey, lengthError, type SigningError } from "./errors.js";
import type { Signature, SigningKey, VerifyingKey } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, concat, copy } from "./internal/bytes.js";
import { literalTaggedHash, TAGS } from "./internal/tags.js";
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

/** Exactly 32 bytes: the message type of standard BIP-340, so no other length is ever hashed or padded. */
export class Message32 {
  readonly #bytes: Uint8Array;
  private constructor(bytes: Uint8Array) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  static of(bytes: Uint8Array): Result<Message32, SigningError> {
    return bytes.length === 32 ? ok(new Message32(bytes)) : err(lengthError("BIP-340 message", 32, bytes.length));
  }

  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }
}

type SchnorrName = "BIP-340" | "secp256k1-tagged-Schnorr";
const N = secp256k1.Point.Fn.ORDER;

function schnorrCodec<S extends SchnorrName>(name: S): SchemeCodec<S> {
  return {
    name,
    signingKey: (bytes) => {
      const b = sized(`${name} signing key`, 32, bytes);
      if (!b.ok) return b;
      const d = beToBig(bytes);
      return d >= 1n && d < N ? ok(signingKeyOf(name, bytes)) : err(invalidKey("secp256k1 secret key must be a scalar in [1, n)"));
    },
    // a 32-byte x-only key that lifts to a curve point (even y)
    verifyingKey: (bytes) => {
      const b = sized(`${name} verifying key`, 32, bytes);
      if (!b.ok) return b;
      return attempt(() => secp256k1.Point.fromBytes(concat(Uint8Array.of(0x02), bytes))) !== undefined
        ? ok(verifyingKeyOf(name, bytes))
        : err(invalidKey(`${name} verifying key is not a valid x-only point`));
    },
    signature: (bytes) => {
      const b = sized(`${name} signature`, 64, bytes);
      return b.ok ? ok(signatureOf(name, bytes)) : b;
    },
    publicKey: (key) => verifyingKeyOf(name, schnorr.getPublicKey(key.bytes)),
  };
}

const auxOf = (aux: Uint8Array): Result<Uint8Array, SigningError> => sized("BIP-340 auxiliary randomness", 32, aux);

function signRaw<S extends SchnorrName>(name: S, key: SigningKey<S>, message32: Uint8Array, aux: Uint8Array): Result<Signature<S>, SigningError> {
  const a = auxOf(aux);
  return a.ok ? ok(signatureOf(name, schnorr.sign(message32, key.bytes, a.value))) : a;
}

const verifyRaw = (key: Uint8Array, message32: Uint8Array, signature: Uint8Array): boolean =>
  signature.length === 64 && key.length === 32 && schnorr.verify(signature, message32, key);

export interface Bip340Scheme extends SchemeCodec<"BIP-340">, Verifier<"BIP-340", Message32>, SeedDerivation<"BIP-340"> {
  /** With caller-supplied 32-byte auxiliary randomness (known answers, external RNGs). */
  readonly sign: (key: SigningKey<"BIP-340">, message: Message32, aux: Uint8Array) => Result<Signature<"BIP-340">, SigningError>;
  /** With fresh auxiliary randomness from `@noble/hashes` `randomBytes`. */
  readonly signRandom: (key: SigningKey<"BIP-340">, message: Message32) => Signature<"BIP-340">;
}

const bip340Codec = schnorrCodec("BIP-340");
const bip340Verify = (key: VerifyingKey<"BIP-340">, message: Message32, signature: Signature<"BIP-340">): boolean =>
  verifyRaw(key.bytes, message.bytes, signature.bytes);

export const bip340: Bip340Scheme = Object.freeze({
  ...bip340Codec,
  ...seedIsKey(bip340Codec),
  sign: (key: SigningKey<"BIP-340">, message: Message32, aux: Uint8Array) => signRaw("BIP-340", key, message.bytes, aux),
  signRandom: (key: SigningKey<"BIP-340">, message: Message32) => signatureOf("BIP-340", schnorr.sign(message.bytes, key.bytes, randomBytes(32))),
  verify: bip340Verify,
  verifyBytes: verifyBytesWith(bip340Codec, bip340Verify),
});

/** The dilf4s scheme signs `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)` with BIP-340. */
const digestOf = literalTaggedHash("SHA-256", TAGS.schnorr);

export interface TaggedSchnorrScheme
  extends SchemeCodec<"secp256k1-tagged-Schnorr">,
    Verifier<"secp256k1-tagged-Schnorr">,
    SeedDerivation<"secp256k1-tagged-Schnorr"> {
  readonly sign: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array, aux: Uint8Array) => Result<Signature<"secp256k1-tagged-Schnorr">, SigningError>;
  readonly signRandom: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array) => Signature<"secp256k1-tagged-Schnorr">;
}

const taggedCodec = schnorrCodec("secp256k1-tagged-Schnorr");
const taggedVerify = (key: VerifyingKey<"secp256k1-tagged-Schnorr">, message: Uint8Array, signature: Signature<"secp256k1-tagged-Schnorr">): boolean =>
  verifyRaw(key.bytes, digestOf(message), signature.bytes);

export const taggedSchnorr: TaggedSchnorrScheme = Object.freeze({
  ...taggedCodec,
  ...seedIsKey(taggedCodec),
  sign: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array, aux: Uint8Array) =>
    signRaw("secp256k1-tagged-Schnorr", key, digestOf(message), aux),
  signRandom: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array) =>
    signatureOf("secp256k1-tagged-Schnorr", schnorr.sign(digestOf(message), key.bytes, randomBytes(32))),
  verify: taggedVerify,
  verifyBytes: verifyBytesWith(taggedCodec, taggedVerify),
});
