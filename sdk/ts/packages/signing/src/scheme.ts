/**
 * The capabilities every signature scheme offers (Scala `SchemeCodec`, `PublicKeyOf`, `Verifier`, `SeedDerivation`).
 *
 * Every function of a scheme checks what a caller passes past the types once, in O(1) per argument, before reading
 * it: an argument of the wrong type is core's `TypeMismatch` (internal/check.ts), never a throw, never bytes read as
 * empty, and never a verdict. A key or signature must be one this package made for the scheme at hand (a private-brand
 * check, then its scheme): a key of another scheme is refused, not verified under rules it was not decoded by.
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { ownCopy } from "@kd5ujc/dilf4s-core/internal";

import { lengthError, type CryptoError } from "./errors.js";
import { ownSignature, ownSigningKey, ownVerifyingKey, Signature, SigningKey, VerifyingKey, type KeyPair, type SchemeName } from "./keys.js";
import { expected, NOT_BYTES, signedOf, verdict, viewOf } from "./internal/check.js";
import { INTERNAL } from "./internal/token.js";

/** A scheme's refusal, or an argument of the wrong type. */
export type CryptoRefusal = CryptoError | TypeMismatch;

/** Validating decoders: a wrong length is `InvalidEncoding`; a key failing the scheme's rules is `InvalidKey`. */
export interface SchemeCodec<S extends SchemeName> {
  readonly name: S;
  /** A signing key from its encoding, measured by its internal length and copied; non-bytes are a `TypeMismatch`. */
  readonly signingKey: (bytes: Uint8Array) => Result<SigningKey<S>, CryptoRefusal>;
  /** A verifying key from its encoding, as `signingKey` reads it. */
  readonly verifyingKey: (bytes: Uint8Array) => Result<VerifyingKey<S>, CryptoRefusal>;
  /** A signature from its encoding, as `signingKey` reads it. */
  readonly signature: (bytes: Uint8Array) => Result<Signature<S>, CryptoRefusal>;
  /** The verifying key of a signing key; one that is not this scheme's `SigningKey` is a `TypeMismatch`. */
  readonly publicKey: (key: SigningKey<S>) => Result<VerifyingKey<S>, TypeMismatch>;
}

/**
 * Verification of decoded values, and over raw encodings (the key is decoded first, then the signature). A message,
 * key or signature of the wrong type is a `TypeMismatch`, never `false` and never `true`.
 */
export interface Verifier<S extends SchemeName, M = Uint8Array> {
  /** The verdict over a decoded key and signature of this scheme. */
  readonly verify: (key: VerifyingKey<S>, message: M, signature: Signature<S>) => Result<boolean, TypeMismatch>;
  /** The verdict over raw encodings: the message's type checked, then the key decoded, then the signature. */
  readonly verifyBytes: (key: Uint8Array, message: M, signature: Uint8Array) => Result<boolean, CryptoRefusal>;
}

/** Deterministic key derivation from a 32-byte seed (`signing.md` section 1.7). */
export interface SeedDerivation<S extends SchemeName> {
  /** The key pair of a seed; a seed that is not a `Uint8Array` is a `TypeMismatch`. */
  readonly fromSeed: (seed: Uint8Array) => Result<KeyPair<S>, CryptoRefusal>;
}

export const signingKeyOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): SigningKey<S> =>
  new SigningKey(INTERNAL, scheme, bytes);
export const verifyingKeyOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): VerifyingKey<S> =>
  new VerifyingKey(INTERNAL, scheme, bytes);
export const signatureOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): Signature<S> => new Signature(INTERNAL, scheme, bytes);

/**
 * The caller's bytes as a fresh copy, if they are a `Uint8Array` of the expected length: measured by their internal
 * length before anything is copied (bytes of another length are refused unread), then copied once from their own
 * memory, so the bytes a decoder validates are the bytes it keeps, whatever another thread or a subclass does.
 */
export function sized(what: string, expected: number, bytes: Uint8Array): Result<Uint8Array, CryptoRefusal> {
  // measured before it is copied: bytes longer are refused uncopied, shorter ones once copied (they are short)
  const own = ownCopy(bytes, expected);
  if (own === undefined) return err(NOT_BYTES);
  const got = typeof own === "number" ? own : own.length;
  return typeof own !== "number" && got === expected ? ok(own) : err(lengthError(what, expected, got));
}

/**
 * How a verification reads its message: `read` gives the message as the scheme checks it, or `undefined` when it is
 * not of its type, refused then as `refusal`. Nothing is allocated for a message that passes but what `read` makes.
 */
export interface MessageReader<M> {
  readonly read: (m: unknown) => M | undefined;
  readonly refusal: TypeMismatch;
}

/** A message to verify, as a plain view (see `viewOf`); what is not a `Uint8Array` is a `TypeMismatch`. */
export const verifiedMessage: MessageReader<Uint8Array> = Object.freeze({ read: viewOf, refusal: NOT_BYTES });

/** A verification over the scheme's key, message and signature bytes, each already checked. */
export type RawVerify<M> = (key: Uint8Array, message: M, signature: Uint8Array) => boolean;

/**
 * `verify` for a scheme: the key and signature must be this scheme's (their own bytes are read, never copied), and the
 * message is read by `messages`. The checks allocate nothing (a refusal is a shared constant), the verdict is one of
 * two shared results, and the one allocation is the message's plain view, where `messages` makes one.
 */
export function checkedVerify<S extends SchemeName, M>(
  scheme: S,
  messages: MessageReader<M>,
  raw: RawVerify<M>,
): (key: VerifyingKey<S>, message: unknown, signature: Signature<S>) => Result<boolean, TypeMismatch> {
  // shared refusals, frozen: one a caller changed would answer every later call
  const notKey = Object.freeze(err(expected(`a ${scheme} VerifyingKey`)));
  const notSignature = Object.freeze(err(expected(`a ${scheme} Signature`)));
  const notMessage = Object.freeze(err(messages.refusal));
  return (key, message, signature) => {
    const k = ownVerifyingKey(key, scheme);
    if (k === undefined) return notKey;
    const s = ownSignature(signature, scheme);
    if (s === undefined) return notSignature;
    const m = messages.read(message);
    return m === undefined ? notMessage : verdict(raw(k, m, s));
  };
}

/** Composes the decoders with a raw verification: the message's type, then the key, then the signature. */
export function verifyBytesWith<S extends SchemeName, M>(
  codec: SchemeCodec<S>,
  messages: MessageReader<M>,
  raw: RawVerify<M>,
): (key: Uint8Array, message: unknown, signature: Uint8Array) => Result<boolean, CryptoRefusal> {
  const notMessage = Object.freeze(err(messages.refusal));
  return (key, message, signature) => {
    const m = messages.read(message);
    if (m === undefined) return notMessage;
    const k = codec.verifyingKey(key);
    if (!k.ok) return k;
    const s = codec.signature(signature);
    if (!s.ok) return s;
    // the decoded key and signature are this scheme's own: their bytes are read, never copied again
    const kb = ownVerifyingKey(k.value, codec.name);
    const sb = ownSignature(s.value, codec.name);
    return kb === undefined || sb === undefined ? err(expected(`a ${codec.name} key and signature`)) : verdict(raw(kb, m, sb));
  };
}

/** `publicKey` for a scheme: the key must be this scheme's `SigningKey`; `of` maps its own bytes to the public key's. */
export function checkedPublicKey<S extends SchemeName>(scheme: S, of: (secret: Uint8Array) => Uint8Array): (key: SigningKey<S>) => Result<VerifyingKey<S>, TypeMismatch> {
  const notKey = Object.freeze(err(expected(`a ${scheme} SigningKey`)));
  return (key) => {
    const k = ownSigningKey(key, scheme);
    return k === undefined ? notKey : ok(verifyingKeyOf(scheme, of(k)));
  };
}

/**
 * `sign` for a scheme: the key must be this scheme's `SigningKey` (its own bytes read, nothing allocated to check it),
 * and the message is copied once (see `signedOf`), the one allocation a passing check makes.
 */
export function checkedSign<S extends SchemeName>(scheme: S, raw: (secret: Uint8Array, message: Uint8Array) => Uint8Array): (key: SigningKey<S>, message: Uint8Array) => Result<Signature<S>, TypeMismatch> {
  const notKey = Object.freeze(err(expected(`a ${scheme} SigningKey`)));
  const notMessage = Object.freeze(err(NOT_BYTES));
  return (key, message) => {
    const k = ownSigningKey(key, scheme);
    if (k === undefined) return notKey;
    const m = signedOf(message);
    return m === undefined ? notMessage : ok(signatureOf(scheme, raw(k, m)));
  };
}

/** Seed derivation for schemes whose secret key is the seed itself (validated by the codec). */
export function seedIsKey<S extends SchemeName>(codec: SchemeCodec<S>): SeedDerivation<S> {
  return {
    fromSeed: (seed) => {
      const sk = codec.signingKey(seed);
      if (!sk.ok) return sk;
      const vk = codec.publicKey(sk.value);
      return vk.ok ? ok({ signing: sk.value, verifying: vk.value }) : vk;
    },
  };
}
