/** The capabilities every signature scheme offers (Scala `SchemeCodec`, `PublicKeyOf`, `Verifier`, `SeedDerivation`). */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";

import { lengthError, type SigningError } from "./errors.js";
import { Signature, SigningKey, VerifyingKey, type KeyPair, type SchemeName } from "./keys.js";
import { INTERNAL } from "./internal/token.js";

/** Validating decoders: a wrong length is `InvalidEncoding`; a key failing the scheme's rules is `InvalidKey`. */
export interface SchemeCodec<S extends SchemeName> {
  readonly name: S;
  readonly signingKey: (bytes: Uint8Array) => Result<SigningKey<S>, SigningError>;
  readonly verifyingKey: (bytes: Uint8Array) => Result<VerifyingKey<S>, SigningError>;
  readonly signature: (bytes: Uint8Array) => Result<Signature<S>, SigningError>;
  /** The verifying key of a signing key. */
  readonly publicKey: (key: SigningKey<S>) => VerifyingKey<S>;
}

/** Verification of decoded values, and over raw encodings (the key is decoded first, then the signature). */
export interface Verifier<S extends SchemeName, M = Uint8Array> {
  readonly verify: (key: VerifyingKey<S>, message: M, signature: Signature<S>) => boolean;
  readonly verifyBytes: (key: Uint8Array, message: M, signature: Uint8Array) => Result<boolean, SigningError>;
}

/** Deterministic key derivation from a 32-byte seed (`signing.md` section 1.7). */
export interface SeedDerivation<S extends SchemeName> {
  readonly fromSeed: (seed: Uint8Array) => Result<KeyPair<S>, SigningError>;
}

export const signingKeyOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): SigningKey<S> =>
  new SigningKey(INTERNAL, scheme, bytes);
export const verifyingKeyOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): VerifyingKey<S> =>
  new VerifyingKey(INTERNAL, scheme, bytes);
export const signatureOf = <S extends SchemeName>(scheme: S, bytes: Uint8Array): Signature<S> => new Signature(INTERNAL, scheme, bytes);

/** The bytes, if they have the expected length. */
export const sized = (what: string, expected: number, bytes: Uint8Array): Result<Uint8Array, SigningError> =>
  bytes.length === expected ? ok(bytes) : err(lengthError(what, expected, bytes.length));

/** Composes the decoders with `verify`: key first, then signature. */
export function verifyBytesWith<S extends SchemeName, M>(
  codec: SchemeCodec<S>,
  verify: (key: VerifyingKey<S>, message: M, signature: Signature<S>) => boolean,
): (key: Uint8Array, message: M, signature: Uint8Array) => Result<boolean, SigningError> {
  return (key, message, signature) => {
    const k = codec.verifyingKey(key);
    if (!k.ok) return k;
    const s = codec.signature(signature);
    if (!s.ok) return s;
    return ok(verify(k.value, message, s.value));
  };
}

/** Seed derivation for schemes whose secret key is the seed itself (validated by the codec). */
export function seedIsKey<S extends SchemeName>(codec: SchemeCodec<S>): SeedDerivation<S> {
  return {
    fromSeed: (seed) => {
      const sk = codec.signingKey(seed);
      return sk.ok ? ok({ signing: sk.value, verifying: codec.publicKey(sk.value) }) : sk;
    },
  };
}
