/** Keys and signatures: nominal per scheme, immutable, with secret bytes that never print. */
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { mint, takeBytes } from "@kd5ujc/dilf4s-core/internal";

import { expected, NOT_BYTES } from "./internal/check.js";
import { copy, ctEqual, equal, hex } from "./internal/bytes.js";
import { INTERNAL, type Internal } from "./internal/token.js";

/** The signature schemes of `signing.md` section 1, by the names the vectors give them. */
export type SchemeName =
  | "Ed25519"
  | "Ed25519-BIP32"
  | "P-256-ECDSA"
  | "secp256k1-ECDSA"
  | "BIP-340"
  | "secp256k1-tagged-Schnorr"
  | "ristretto255-Schnorr"
  | "BLS12-381-PoP";

/** Node's `util.inspect` hook, so secrets stay redacted in `console.log` too. */
export const INSPECT: unique symbol = Symbol.for("nodejs.util.inspect.custom") as never;

// one brand per class, stamped only by a constructor given the token: a key built by calling a constructor from outside
// (JavaScript can) carries the class's fields but not its brand, so no check here accepts it
const SIGNING = mint();
const VERIFYING = mint();
const SIGNATURE = mint();
const SECRET = mint();

let signingOf!: (x: unknown) => Uint8Array | undefined;
let verifyingOf!: (x: unknown) => Uint8Array | undefined;
let signatureBytesOf!: (x: unknown) => Uint8Array | undefined;
let secretOf!: (x: unknown) => Uint8Array | undefined;
let secretFrom!: (bytes: Uint8Array) => Secret;

/**
 * A secret signing key of scheme `S`. Its bytes live in a `#private` field; `toString`, `toJSON` and `util.inspect` are
 * redacted, `bytes` returns a copy, and `equals` runs in constant time. Built only by a scheme's decoder or derivation.
 */
export class SigningKey<S extends string> {
  readonly #bytes: Uint8Array;
  constructor(
    token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    // without the token, nothing of the caller's is read or kept, and the instance is not branded
    if (token !== INTERNAL) this.scheme = "" as S;
    this.#bytes = token === INTERNAL ? copy(bytes) : new Uint8Array(0);
    if (token === INTERNAL) SIGNING.stamp(this);
    Object.freeze(this);
  }

  static {
    signingOf = (x) => (SIGNING.has(x) ? (x as SigningKey<string>).#bytes : undefined);
  }

  /** A copy of the secret bytes. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  /**
   * Constant-time equality with another key of the same scheme. Something that is not a `SigningKey` (from a JavaScript
   * caller, or a cast) equals nothing, and is never read.
   */
  equals(other: SigningKey<S>): boolean {
    const o = signingOf(other);
    return o !== undefined && this.scheme === other.scheme && ctEqual(this.#bytes, o);
  }

  toString(): string {
    return `SigningKey(${this.scheme}, <redacted>)`;
  }

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

/** A decoded, validated verifying key of scheme `S`. */
export class VerifyingKey<S extends string> {
  readonly #bytes: Uint8Array;
  constructor(
    token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    // without the token, nothing of the caller's is read or kept, and the instance is not branded
    if (token !== INTERNAL) this.scheme = "" as S;
    this.#bytes = token === INTERNAL ? copy(bytes) : new Uint8Array(0);
    if (token === INTERNAL) VERIFYING.stamp(this);
    Object.freeze(this);
  }

  static {
    verifyingOf = (x) => (VERIFYING.has(x) ? (x as VerifyingKey<string>).#bytes : undefined);
  }

  /** A copy of the encoding. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  /** Equality with another key of the same scheme; something that is not a `VerifyingKey` equals nothing, unread. */
  equals(other: VerifyingKey<S>): boolean {
    const o = verifyingOf(other);
    return o !== undefined && this.scheme === other.scheme && equal(this.#bytes, o);
  }

  toString(): string {
    return `VerifyingKey(${this.scheme}, ${hex(this.#bytes)})`;
  }
}

/** A decoded signature of scheme `S` (its decoder's checks passed). */
export class Signature<S extends string> {
  readonly #bytes: Uint8Array;
  constructor(
    token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    // without the token, nothing of the caller's is read or kept, and the instance is not branded
    if (token !== INTERNAL) this.scheme = "" as S;
    this.#bytes = token === INTERNAL ? copy(bytes) : new Uint8Array(0);
    if (token === INTERNAL) SIGNATURE.stamp(this);
    Object.freeze(this);
  }

  static {
    signatureBytesOf = (x) => (SIGNATURE.has(x) ? (x as Signature<string>).#bytes : undefined);
  }

  /** A copy of the encoding. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  /** Equality with another signature of the same scheme; something that is not a `Signature` equals nothing, unread. */
  equals(other: Signature<S>): boolean {
    const o = signatureBytesOf(other);
    return o !== undefined && this.scheme === other.scheme && equal(this.#bytes, o);
  }

  toString(): string {
    return `Signature(${this.scheme}, ${hex(this.#bytes)})`;
  }
}

export interface KeyPair<S extends string> {
  readonly signing: SigningKey<S>;
  readonly verifying: VerifyingKey<S>;
}

/** Secret byte material (seeds, entropy, chain-carrying roots): never printed, compared in constant time. */
export class Secret {
  readonly #bytes: Uint8Array;
  private constructor(token: Internal, bytes: Uint8Array) {
    this.#bytes = token === INTERNAL ? bytes : new Uint8Array(0);
    if (token === INTERNAL) SECRET.stamp(this);
    Object.freeze(this);
  }

  static {
    secretOf = (x) => (SECRET.has(x) ? (x as Secret).#bytes : undefined);
    secretFrom = (bytes) => new Secret(INTERNAL, bytes);
  }

  /**
   * A secret holding a copy of `bytes`, taken from their own memory (their own `length` and iterator are not believed).
   * What is not a `Uint8Array` is a `TypeMismatch`.
   */
  static of(bytes: Uint8Array): Result<Secret, TypeMismatch> {
    const own = takeBytes(bytes);
    return own === undefined ? err(NOT_BYTES) : ok(new Secret(INTERNAL, own));
  }

  /** A copy of the secret bytes. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  get length(): number {
    return this.#bytes.length;
  }

  /** Constant-time equality; something that is not a `Secret` equals nothing, unread. */
  equals(other: Secret): boolean {
    const o = secretOf(other);
    return o !== undefined && ctEqual(this.#bytes, o);
  }

  toString(): string {
    return `Secret(<${String(this.#bytes.length)} bytes redacted>)`;
  }

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

/** A secret of bytes this package owns (no copy is taken). */
export const ownSecret = (bytes: Uint8Array): Secret => secretFrom(bytes);

/**
 * The bytes of `x` if it is a key or signature of this package's of the class's kind and of `scheme`: a private-brand
 * check (no Proxy trap or caller code runs), then the frozen instance's `scheme`. The bytes are the instance's own, for
 * this package to read and never to change or hand out.
 */
function branded(of: (x: unknown) => Uint8Array | undefined, x: unknown, scheme: string, what: string): Result<Uint8Array, TypeMismatch> {
  const b = of(x);
  // a key of another scheme is refused: verifying under the wrong scheme's rules could accept what this scheme does not
  return b !== undefined && (x as { readonly scheme: unknown }).scheme === scheme ? ok(b) : err(expected(`a ${scheme} ${what}`));
}

/** The scheme of `x` if it is a `SigningKey` this package made (a brand check first), else `undefined`. */
export const signingKeySchemeOf = (x: unknown): string | undefined => (signingOf(x) === undefined ? undefined : (x as SigningKey<string>).scheme);

/** A verifying key's own bytes, if `x` is a `VerifyingKey` of `scheme` this package made, else `undefined`: nothing allocated. */
export function ownVerifyingKey(x: unknown, scheme: string): Uint8Array | undefined {
  const b = verifyingOf(x);
  return b !== undefined && (x as { readonly scheme: unknown }).scheme === scheme ? b : undefined;
}

/** A signing key's own bytes, as {@link ownVerifyingKey}. */
export function ownSigningKey(x: unknown, scheme: string): Uint8Array | undefined {
  const b = signingOf(x);
  return b !== undefined && (x as { readonly scheme: unknown }).scheme === scheme ? b : undefined;
}

/** A signature's own bytes, as {@link ownVerifyingKey}. */
export function ownSignature(x: unknown, scheme: string): Uint8Array | undefined {
  const b = signatureBytesOf(x);
  return b !== undefined && (x as { readonly scheme: unknown }).scheme === scheme ? b : undefined;
}

/** A signing key's own bytes, if `x` is a `SigningKey` of `scheme` this package made; else a `TypeMismatch`. */
export const signingKeyBytes = (x: unknown, scheme: string): Result<Uint8Array, TypeMismatch> => branded(signingOf, x, scheme, "SigningKey");
/** A verifying key's own bytes, as {@link signingKeyBytes}. */
export const verifyingKeyBytes = (x: unknown, scheme: string): Result<Uint8Array, TypeMismatch> => branded(verifyingOf, x, scheme, "VerifyingKey");
/** A signature's own bytes, as {@link signingKeyBytes}. */
export const signatureBytes = (x: unknown, scheme: string): Result<Uint8Array, TypeMismatch> => branded(signatureBytesOf, x, scheme, "Signature");

/**
 * Secret input as bytes: a `Secret`'s own bytes, or a copy of a `Uint8Array`'s own memory, or `undefined` for anything
 * else.
 */
export function secretBytes(x: unknown): Uint8Array | undefined {
  return secretOf(x) ?? takeBytes(x);
}
