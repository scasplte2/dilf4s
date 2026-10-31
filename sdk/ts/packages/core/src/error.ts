/**
 * Why a canonical value, encoding or JSON text was refused (docs/specs/canonical-encoding.md). `kind` names each case
 * exactly as the conformance vectors do.
 */
export type CanonicalError =
  /** Text with an unpaired UTF-16 surrogate. */
  | { readonly kind: "UnpairedSurrogate" }
  /** A map or JSON object with the same key twice. */
  | { readonly kind: "DuplicateKey"; readonly key: string }
  /** JSON text that is not RFC 8259 syntax; `offset` counts UTF-16 code units. */
  | { readonly kind: "JsonSyntax"; readonly offset: number; readonly reason: string }
  /** A JSON number that is malformed, or whose scale exceeds the bound. */
  | { readonly kind: "InvalidNumber"; readonly source: string; readonly scale?: bigint }
  /** Bytes, or a rational with no terminating decimal: no JSON form. */
  | { readonly kind: "NotJsonRepresentable"; readonly what: "bytes" | "non-terminating rational" }
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

/** Why a canonical value is not the expected type, and where: `path` runs from the root (map keys and `[i]`). */
export interface DecodeFailure {
  readonly path: readonly string[];
  readonly reason: string;
}

export function decodeFailure(reason: string, path: readonly string[] = []): DecodeFailure {
  return { path, reason };
}

/** `failure`, one level further down. */
export function at(failure: DecodeFailure, segment: string): DecodeFailure {
  return { path: [segment, ...failure.path], reason: failure.reason };
}

export function describeFailure(failure: DecodeFailure): string {
  return `${failure.path.length === 0 ? "<root>" : failure.path.join(".")}: ${failure.reason}`;
}
