import { Preview } from "./preview.js";

/**
 * Why a canonical value, encoding or JSON text was refused (docs/specs/canonical-encoding.md). `kind` names each case
 * exactly as the conformance vectors do. A refusal describes caller input by its size or by a {@link Preview}, never by
 * the input itself (docs/specs/conformance.md section 8).
 */
export type CanonicalError =
  /** Text with an unpaired UTF-16 surrogate. */
  | { readonly kind: "UnpairedSurrogate" }
  /** A map or JSON object with the same key twice. */
  | { readonly kind: "DuplicateKey"; readonly key: Preview }
  /** A rational whose numerator or denominator is wider than `MAX_RATIONAL_BITS`, the data model's bound. */
  | { readonly kind: "RationalTooWide"; readonly limit: number }
  /** JSON text that is not RFC 8259 syntax; `offset` counts UTF-16 code units. */
  | { readonly kind: "JsonSyntax"; readonly offset: number; readonly reason: string }
  /** A JSON number that is malformed, or outside the digit or scale bound. */
  // the number's length and offset in UTF-16 code units, never its text, whose size the caller chose
  // `scale` is saturated at the 32-bit range, as the reference's
  | { readonly kind: "InvalidNumber"; readonly length: number; readonly offset: number; readonly scale?: number; readonly digits?: number }
  /** Bytes, a rational with no terminating decimal, or a number too long to read back: no JSON form. */
  | { readonly kind: "NotJsonRepresentable"; readonly what: "bytes" | "non-terminating rational" | "over-long number" }
  /** A typed value outside its schema. */
  | { readonly kind: "TypeMismatch"; readonly reason: string }
  /** The input ended inside an item. */
  | { readonly kind: "UnexpectedEnd"; readonly offset: number }
  /** Bytes after the top-level item. */
  | { readonly kind: "TrailingBytes"; readonly offset: number }
  /** Arrays and maps nested deeper than the limit. */
  | { readonly kind: "DepthExceeded"; readonly limit: number }
  /** A well-formed CBOR item that is not the canonical encoding of its value. */
  | { readonly kind: "NonCanonical"; readonly offset: number; readonly reason: string }
  /** CBOR outside the data model: floats, indefinite lengths, other simple values or tags. */
  | { readonly kind: "Unsupported"; readonly offset: number; readonly reason: string }
  /** A CBOR text string that is not well-formed UTF-8. */
  | { readonly kind: "InvalidUtf8"; readonly offset: number };

/**
 * A value whose canonical CBOR, commitment or JSON is longer than a limit, found without producing more than the limit.
 * It carries the limit only: how far past it the value goes is never measured (docs/specs/conformance.md).
 */
export interface TooLarge {
  readonly kind: "TooLarge";
  /** The limit, in bytes. */
  readonly limit: number;
}

/**
 * Why a canonical value is not the expected type, and where: `path` runs from the root (map keys and `[i]`). A key may
 * be the caller's, so each segment is a {@link Preview}; `reason` quotes caller input only through one.
 */
export interface DecodeFailure {
  readonly path: readonly Preview[];
  readonly reason: string;
}

export function decodeFailure(reason: string, path: readonly string[] = []): DecodeFailure {
  return { path: path.map((p) => Preview.of(p)), reason };
}

/** `failure`, one level further down. */
export function at(failure: DecodeFailure, segment: string): DecodeFailure {
  return { path: [Preview.of(segment), ...failure.path], reason: failure.reason };
}

export function describeFailure(failure: DecodeFailure): string {
  return `${failure.path.length === 0 ? "<root>" : failure.path.map((p) => p.short()).join(".")}: ${failure.reason}`;
}
