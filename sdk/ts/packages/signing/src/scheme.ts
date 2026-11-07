/**
 * The capabilities every signature scheme offers (Scala `SchemeCodec`, `PublicKeyOf`, `Verifier`, `SeedDerivation`).
 *
 * Every function of a scheme checks what a caller passes past the types once, in O(1) per argument, before reading
 * it: an argument of the wrong type is core's `TypeMismatch` (internal/check.ts), never a throw, never bytes read as
 * empty, and never a verdict. A key or signature must be one this package made for the scheme at hand (a private-brand
 * check, then its scheme): a key of another scheme is refused, not verified under rules it was not decoded by.
 */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { copyBytes, plainBytes } from "@kd5ujc/dilf4s-core/internal";

import { lengthError, type CryptoError } from "./errors.js";
import { Signature, signatureBytes, signingKeyBytes, SigningKey, VerifyingKey, verifyingKeyBytes, type KeyPair, type SchemeName } from "./keys.js";
import { NOT_BYTES, signedOf, verdict, viewOf } from "./internal/check.js";
import { INTERNAL } from "./internal/token.js";

/** A scheme's refusal, or an argument of the wrong type. */
export type CryptoRefusal = CryptoError | TypeMismatch;

/** Validating decoders: a wrong length is `InvalidEncoding`; a key failing the scheme's rules is `InvalidKey`. */
export interface SchemeCodec<S extends SchemeName> {
  readonly name: S;
  readonly signingKey: (bytes: Uint8Array) => Result<SigningKey<S>, CryptoRefusal>;
  readonly verifyingKey: (bytes: Uint8Array) => Result<VerifyingKey<S>, CryptoRefusal>;
  readonly signature: (bytes: Uint8Array) => Result<Signature<S>, CryptoRefusal>;
  /** The verifying key of a signing key; one that is not this scheme's `SigningKey` is a `TypeMismatch`. */
  readonly publicKey: (key: SigningKey<S>) => Result<VerifyingKey<S>, TypeMismatch>;
}

/**
 * Verification of decoded values, and over raw encodings (the key is decoded first, then the signature). A message,
 * key or signature of the wrong type is a `TypeMismatch`, never `false` and never `true`.
 */
export interface Verifier<S extends SchemeName, M = Uint8Array> {
  readonly verify: (key: VerifyingKey<S>, message: M, signature: Signature<S>) => Result<boolean, TypeMismatch>;
  readonly verifyBytes: (key: Uint8Array, message: M, signature: Uint8Array) => Result<boolean, CryptoRefusal>;
}

/** Deterministic key derivation from a 32-byte seed (`signing.md` section 1.7). */
export interface SeedDerivation<S extends SchemeName> {
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
  const view = plainBytes(bytes);
  if (view === undefined) return err(NOT_BYTES);
  return view.length === expected ? ok(copyBytes(view)) : err(lengthError(what, expected, view.length));
}

/** A message to verify, as a plain view (see `viewOf`), or a `TypeMismatch`. */
export const verifiedMessage = (m: unknown): Result<Uint8Array, TypeMismatch> => {
  const view = viewOf(m);
  return view === undefined ? err(NOT_BYTES) : ok(view);
};

/** A message to sign, read once into memory only this thread can change (see `signedOf`), or a `TypeMismatch`. */
export const signedMessage = (m: unknown): Result<Uint8Array, TypeMismatch> => {
  const own = signedOf(m);
  return own === undefined ? err(NOT_BYTES) : ok(own);
};

/** A verification over the scheme's key, message and signature bytes, each already checked. */
export type RawVerify<M> = (key: Uint8Array, message: M, signature: Uint8Array) => boolean;

/**
 * `verify` for a scheme: the key and signature must be this scheme's (their own bytes are read, never copied), and the
 * message is checked by `messageOf`. The verdict is one of two shared results: nothing is allocated for it.
 */
export function checkedVerify<S extends SchemeName, M>(
  scheme: S,
  messageOf: (m: unknown) => Result<M, TypeMismatch>,
  raw: RawVerify<M>,
): (key: VerifyingKey<S>, message: unknown, signature: Signature<S>) => Result<boolean, TypeMismatch> {
  return (key, message, signature) => {
    const k = verifyingKeyBytes(key, scheme);
    if (!k.ok) return k;
    const s = signatureBytes(signature, scheme);
    if (!s.ok) return s;
    const m = messageOf(message);
    return m.ok ? verdict(raw(k.value, m.value, s.value)) : m;
  };
}

/** Composes the decoders with a raw verification: the message's type, then the key, then the signature. */
export function verifyBytesWith<S extends SchemeName, M>(
  codec: SchemeCodec<S>,
  messageOf: (m: unknown) => Result<M, TypeMismatch>,
  raw: RawVerify<M>,
): (key: Uint8Array, message: unknown, signature: Uint8Array) => Result<boolean, CryptoRefusal> {
  return (key, message, signature) => {
    const m = messageOf(message);
    if (!m.ok) return m;
    const k = codec.verifyingKey(key);
    if (!k.ok) return k;
    const s = codec.signature(signature);
    if (!s.ok) return s;
    // the decoded key and signature are this scheme's own: their bytes are read, never copied again
    const kb = verifyingKeyBytes(k.value, codec.name);
    if (!kb.ok) return kb;
    const sb = signatureBytes(s.value, codec.name);
    return sb.ok ? verdict(raw(kb.value, m.value, sb.value)) : sb;
  };
}

/** `publicKey` for a scheme: the key must be this scheme's `SigningKey`; `of` maps its own bytes to the public key's. */
export function checkedPublicKey<S extends SchemeName>(scheme: S, of: (secret: Uint8Array) => Uint8Array): (key: SigningKey<S>) => Result<VerifyingKey<S>, TypeMismatch> {
  return (key) => {
    const k = signingKeyBytes(key, scheme);
    return k.ok ? ok(verifyingKeyOf(scheme, of(k.value))) : k;
  };
}

/** `sign` for a scheme: the key must be this scheme's `SigningKey`, and the message is read once (see `signedOf`). */
export function checkedSign<S extends SchemeName>(scheme: S, raw: (secret: Uint8Array, message: Uint8Array) => Uint8Array): (key: SigningKey<S>, message: Uint8Array) => Result<Signature<S>, TypeMismatch> {
  return (key, message) => {
    const k = signingKeyBytes(key, scheme);
    if (!k.ok) return k;
    const m = signedMessage(message);
    return m.ok ? ok(signatureOf(scheme, raw(k.value, m.value))) : m;
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
