/**
 * BIP-340 and dilf4s tagged Schnorr over secp256k1 (`signing.md` 1.4). The two schemes share key encodings, but
 * neither accepts the other's signatures, and the types keep them apart.
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { mint } from "@kd5ujc/dilf4s-core/internal";
import { schnorr, secp256k1 } from "@noble/curves/secp256k1.js";
import { randomBytes } from "@noble/hashes/utils.js";

import { invalidKey } from "./errors.js";
import { signingKeyBytes, type Signature, type SigningKey } from "./keys.js";
import { attempt } from "./internal/attempt.js";
import { beToBig, concat, copy } from "./internal/bytes.js";
import { expected } from "./internal/check.js";
import { INTERNAL, type Internal } from "./internal/token.js";
import { literalTaggedHash, TAGS } from "./internal/tags.js";
import {
  checkedPublicKey,
  checkedVerify,
  seedIsKey,
  signatureOf,
  signedMessage,
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

const MESSAGE32 = mint();
let message32Of!: (x: unknown) => Uint8Array | undefined;

/** Exactly 32 bytes: the message type of standard BIP-340, so no other length is ever hashed or padded. */
export class Message32 {
  readonly #bytes: Uint8Array;
  private constructor(token: Internal, bytes: Uint8Array) {
    this.#bytes = token === INTERNAL ? bytes : new Uint8Array(0);
    if (token === INTERNAL) MESSAGE32.stamp(this);
    Object.freeze(this);
  }

  static {
    message32Of = (x) => (MESSAGE32.has(x) ? (x as Message32).#bytes : undefined);
  }

  /**
   * Exactly 32 bytes, measured by their internal length (`InvalidEncoding` otherwise) and copied from their own memory;
   * what is not a `Uint8Array` is a `TypeMismatch`.
   */
  static of(bytes: Uint8Array): Result<Message32, CryptoRefusal> {
    const b = sized("BIP-340 message", 32, bytes);
    return b.ok ? ok(new Message32(INTERNAL, b.value)) : b;
  }

  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }
}

/** A `Message32` made by `Message32.of` (a private-brand check: an object with `bytes` is not one), its own bytes. */
const message32Arg = (m: unknown): Result<Uint8Array, TypeMismatch> => {
  const b = message32Of(m);
  return b === undefined ? err(expected("a Message32")) : ok(b);
};

type SchnorrName = "BIP-340" | "secp256k1-tagged-Schnorr";
const N = secp256k1.Point.Fn.ORDER;

function schnorrCodec<S extends SchnorrName>(name: S): SchemeCodec<S> {
  return {
    name,
    signingKey: (bytes) => {
      const b = sized(`${name} signing key`, 32, bytes);
      if (!b.ok) return b;
      const d = beToBig(b.value);
      return d >= 1n && d < N ? ok(signingKeyOf(name, b.value)) : err(invalidKey("secp256k1 secret key must be a scalar in [1, n)"));
    },
    // a 32-byte x-only key that lifts to a curve point (even y)
    verifyingKey: (bytes) => {
      const b = sized(`${name} verifying key`, 32, bytes);
      if (!b.ok) return b;
      return attempt(() => secp256k1.Point.fromBytes(concat(Uint8Array.of(0x02), b.value))) !== undefined
        ? ok(verifyingKeyOf(name, b.value))
        : err(invalidKey(`${name} verifying key is not a valid x-only point`));
    },
    signature: (bytes) => {
      const b = sized(`${name} signature`, 64, bytes);
      return b.ok ? ok(signatureOf(name, b.value)) : b;
    },
    publicKey: checkedPublicKey(name, (sk) => schnorr.getPublicKey(sk)),
  };
}

const auxOf = (aux: Uint8Array): Result<Uint8Array, CryptoRefusal> => sized("BIP-340 auxiliary randomness", 32, aux);

const verifyRaw = (key: Uint8Array, message32: Uint8Array, signature: Uint8Array): boolean =>
  signature.length === 64 && key.length === 32 && schnorr.verify(signature, message32, key);

/**
 * A signature by `key` over the 32 bytes `digest` derives from the caller's message (each argument checked: the key must
 * be the scheme's, the message read once), with auxiliary randomness from `auxOf`.
 */
function signWith<S extends SchnorrName, M, E>(
  name: S,
  key: unknown,
  message: unknown,
  messageOf: (m: unknown) => Result<M, TypeMismatch>,
  digest: (m: M) => Uint8Array,
  aux: () => Result<Uint8Array, E>,
): Result<Signature<S>, TypeMismatch | E> {
  const k = signingKeyBytes(key, name);
  if (!k.ok) return k;
  const m = messageOf(message);
  if (!m.ok) return m;
  const a = aux();
  return a.ok ? ok(signatureOf(name, schnorr.sign(digest(m.value), k.value, a.value))) : a;
}

const fresh = (): Result<Uint8Array, never> => ok(randomBytes(32));

export interface Bip340Scheme extends SchemeCodec<"BIP-340">, Verifier<"BIP-340", Message32>, SeedDerivation<"BIP-340"> {
  /** With caller-supplied 32-byte auxiliary randomness (known answers, external RNGs). */
  readonly sign: (key: SigningKey<"BIP-340">, message: Message32, aux: Uint8Array) => Result<Signature<"BIP-340">, CryptoRefusal>;
  /** With fresh auxiliary randomness from `@noble/hashes` `randomBytes`. */
  readonly signRandom: (key: SigningKey<"BIP-340">, message: Message32) => Result<Signature<"BIP-340">, TypeMismatch>;
}

const bip340Codec = schnorrCodec("BIP-340");
const own = (m: Uint8Array): Uint8Array => m;

export const bip340: Bip340Scheme = Object.freeze({
  ...bip340Codec,
  ...seedIsKey(bip340Codec),
  sign: (key: SigningKey<"BIP-340">, message: Message32, aux: Uint8Array) => signWith("BIP-340", key, message, message32Arg, own, () => auxOf(aux)),
  signRandom: (key: SigningKey<"BIP-340">, message: Message32) => signWith("BIP-340", key, message, message32Arg, own, fresh),
  verify: checkedVerify("BIP-340", message32Arg, verifyRaw),
  verifyBytes: verifyBytesWith(bip340Codec, message32Arg, verifyRaw),
});

/** The dilf4s scheme signs `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)` with BIP-340. */
const digestOf = literalTaggedHash("SHA-256", TAGS.schnorr);

export interface TaggedSchnorrScheme
  extends SchemeCodec<"secp256k1-tagged-Schnorr">,
    Verifier<"secp256k1-tagged-Schnorr">,
    SeedDerivation<"secp256k1-tagged-Schnorr"> {
  /** With caller-supplied 32-byte auxiliary randomness; arguments of the wrong type are a `TypeMismatch`. */
  readonly sign: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array, aux: Uint8Array) => Result<Signature<"secp256k1-tagged-Schnorr">, CryptoRefusal>;
  /** With fresh auxiliary randomness from `@noble/hashes` `randomBytes`. */
  readonly signRandom: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array) => Result<Signature<"secp256k1-tagged-Schnorr">, TypeMismatch>;
}

const taggedCodec = schnorrCodec("secp256k1-tagged-Schnorr");
const taggedVerify = (key: Uint8Array, message: Uint8Array, signature: Uint8Array): boolean => verifyRaw(key, digestOf(message), signature);

export const taggedSchnorr: TaggedSchnorrScheme = Object.freeze({
  ...taggedCodec,
  ...seedIsKey(taggedCodec),
  sign: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array, aux: Uint8Array) =>
    signWith("secp256k1-tagged-Schnorr", key, message, signedMessage, digestOf, () => auxOf(aux)),
  signRandom: (key: SigningKey<"secp256k1-tagged-Schnorr">, message: Uint8Array) =>
    signWith("secp256k1-tagged-Schnorr", key, message, signedMessage, digestOf, fresh),
  verify: checkedVerify("secp256k1-tagged-Schnorr", verifiedMessage, taggedVerify),
  verifyBytes: verifyBytesWith(taggedCodec, verifiedMessage, taggedVerify),
});
