/**
 * Typed errors, generated from `shared/schemas/crypto-errors.cddl` (`models.cryptoErrors`): each family is a union on
 * `kind`, its kinds the Scala reference's cases, which the conformance vectors use. A kind added to the schema is a
 * member here.
 */
import type { models } from "@kd5ujc/dilf4s-core";

/** A key or signature its scheme does not accept, or a signing failure. `BackendFailure` is the JVM's alone. */
export type CryptoError = models.cryptoErrors.CryptoError;

/** A key-derivation failure. */
export type KeyError = models.cryptoErrors.KeyError;

/** Why a derivation path is refused: the first rule it breaks, its segments checked in order (`signing.md` 3). */
export type PathRefusal = models.cryptoErrors.PathRefusal;

/** A derivation path `parsePath` refuses; `reason` is a message, not normative. */
export type InvalidPath = Extract<KeyError, { readonly kind: "InvalidPath" }>;

/** A VRF failure. `Malformed` is a wrong length; every other kind is a rejection (the JLVM's `{valid: false}`). */
export type VrfError = models.cryptoErrors.VrfError;

/** A KES failure. */
export type KesError = models.cryptoErrors.KesError;

export const invalidEncoding = (what: string, reason: string): CryptoError => ({ kind: "InvalidEncoding", what, reason });
export const invalidKey = (reason: string): CryptoError => ({ kind: "InvalidKey", reason });
export const lengthError = (what: string, expected: number, got: number): CryptoError =>
  invalidEncoding(what, `expected ${String(expected)} bytes, got ${String(got)}`);
