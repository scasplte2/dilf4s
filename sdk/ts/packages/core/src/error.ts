import { isArrayGuarded, isRevokedProxy } from "./guard.js";
import { isArrayLength } from "./iterator.js";
import { isPreview, Preview } from "./preview.js";

import type {
  CanonicalError,
  CanonicalErrorKind,
  JsonSyntaxReason,
  NonCanonicalReason,
  NotJson,
  NumberBound,
  UnsupportedItem,
} from "./generated/codec_errors.js";

/**
 * Why a canonical value, encoding or JSON text was refused: generated from `shared/schemas/codec-errors.cddl`, with the
 * same kinds and fields as the reference's and the Rust SDK's. A refusal describes caller input by its size or by a
 * {@link Preview}, never by the input itself (docs/specs/conformance.md section 8).
 */
export type { CanonicalError, CanonicalErrorKind, JsonSyntaxReason, NonCanonicalReason, NotJson, NumberBound, UnsupportedItem };

/**
 * The refusal of an argument that is not of its static type (a JavaScript caller's, or one cast past the types), found
 * in O(1) before it is read: `CanonicalError`'s `TypeMismatch`. `reason` says what was expected, never what was given.
 */
export type TypeMismatch = Extract<CanonicalError, { readonly kind: "TypeMismatch" }>;

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

/** What {@link describeFailure} gives for something that is not a {@link DecodeFailure}. */
export const NOT_A_FAILURE = "<not a decode failure>";

/**
 * `failure` as text: its path from the root (`<root>` when empty), then its reason. Something that is not a
 * `DecodeFailure` (from a JavaScript caller, or a cast) is {@link NOT_A_FAILURE}: its path must be an array of
 * previews and its reason a string, each read once, the path by index and its length once.
 */
export function describeFailure(failure: DecodeFailure): string {
  const given: unknown = failure;
  if (typeof given !== "object" || given === null || isRevokedProxy(given)) return NOT_A_FAILURE;
  const path: unknown = Reflect.get(given, "path");
  const reason: unknown = Reflect.get(given, "reason");
  const length: unknown = isArrayGuarded(path) ? (path as readonly unknown[]).length : undefined;
  if (!isArrayLength(length) || typeof reason !== "string") return NOT_A_FAILURE;
  const segments: Preview[] = [];
  for (let i = 0; i < length; i++) {
    const p: unknown = (path as readonly unknown[])[i];
    if (!isPreview(p)) return NOT_A_FAILURE;
    segments.push(p);
  }
  return `${segments.length === 0 ? "<root>" : segments.map((p) => p.short()).join(".")}: ${reason}`;
}
