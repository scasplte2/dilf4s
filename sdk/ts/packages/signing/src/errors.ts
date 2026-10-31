/** Typed errors. Kinds carry Scala's case-class names (docs: `.workspace/sdk-step2-notes.md`, contract). */

/** A signature-scheme decode or signing failure (Scala `CryptoError`). */
export type SigningError =
  | { readonly kind: "InvalidEncoding"; readonly what: string; readonly reason: string }
  | { readonly kind: "InvalidKey"; readonly reason: string };

/** A key-derivation failure (Scala `KeyError`). */
export type KeyError =
  | { readonly kind: "InvalidEntropy"; readonly reason: string }
  | { readonly kind: "InvalidMnemonic"; readonly reason: string }
  | { readonly kind: "InvalidWordList"; readonly reason: string }
  | { readonly kind: "InvalidDerivation"; readonly reason: string };

/**
 * A VRF failure (Scala `VrfError`). `Malformed` is a wrong length; every other kind is a rejection (the JLVM's
 * `{valid: false}`).
 */
export type VrfError =
  | { readonly kind: "Malformed"; readonly what: string; readonly reason: string }
  | { readonly kind: "InvalidPublicKey" }
  | { readonly kind: "SmallOrderPublicKey" }
  | { readonly kind: "InvalidGamma" }
  | { readonly kind: "NonCanonicalScalar" }
  | { readonly kind: "ChallengeMismatch" }
  | { readonly kind: "KeyMismatch" }
  | { readonly kind: "HashToCurveFailed"; readonly reason: string };

/** A KES failure (Scala `KesError`). */
export type KesError =
  | { readonly kind: "Malformed"; readonly what: string; readonly reason: string }
  | { readonly kind: "PeriodOutOfRange"; readonly period: number; readonly total: number }
  | { readonly kind: "Exhausted"; readonly period: number }
  | { readonly kind: "InconsistentKey"; readonly reason: string };

export const invalidEncoding = (what: string, reason: string): SigningError => ({ kind: "InvalidEncoding", what, reason });
export const invalidKey = (reason: string): SigningError => ({ kind: "InvalidKey", reason });
export const lengthError = (what: string, expected: number, got: number): SigningError =>
  invalidEncoding(what, `expected ${String(expected)} bytes, got ${String(got)}`);
