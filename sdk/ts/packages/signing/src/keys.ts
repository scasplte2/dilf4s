/** Keys and signatures: nominal per scheme, immutable, with secret bytes that never print. */
import { copy, ctEqual, equal, hex } from "./internal/bytes.js";
import type { Internal } from "./internal/token.js";

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

/**
 * A secret signing key of scheme `S`. Its bytes live in a `#private` field; `toString`, `toJSON` and `util.inspect` are
 * redacted, `bytes` returns a copy, and `equals` runs in constant time. Built only by a scheme's decoder or derivation.
 */
export class SigningKey<S extends string> {
  readonly #bytes: Uint8Array;
  constructor(
    _token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  /** A copy of the secret bytes. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  /** Constant-time equality with another key of the same scheme. */
  equals(other: SigningKey<S>): boolean {
    return this.scheme === other.scheme && ctEqual(this.#bytes, other.#bytes);
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
    _token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  /** A copy of the encoding. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  equals(other: VerifyingKey<S>): boolean {
    return this.scheme === other.scheme && equal(this.#bytes, other.#bytes);
  }

  toString(): string {
    return `VerifyingKey(${this.scheme}, ${hex(this.#bytes)})`;
  }
}

/** A decoded signature of scheme `S` (its decoder's checks passed). */
export class Signature<S extends string> {
  readonly #bytes: Uint8Array;
  constructor(
    _token: Internal,
    readonly scheme: S,
    bytes: Uint8Array,
  ) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  /** A copy of the encoding. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  equals(other: Signature<S>): boolean {
    return this.scheme === other.scheme && equal(this.#bytes, other.#bytes);
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
  private constructor(bytes: Uint8Array) {
    this.#bytes = copy(bytes);
    Object.freeze(this);
  }

  /** A secret holding a copy of `bytes`. */
  static of(bytes: Uint8Array): Secret {
    return new Secret(bytes);
  }

  /** A copy of the secret bytes. */
  get bytes(): Uint8Array {
    return copy(this.#bytes);
  }

  get length(): number {
    return this.#bytes.length;
  }

  equals(other: Secret): boolean {
    return ctEqual(this.#bytes, other.#bytes);
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
