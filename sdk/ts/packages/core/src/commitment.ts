/** The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2). */
import { checkedLimit, tooLarge } from "./budget.js";
import { encodeIntoWithin, encodeWithin, Feeding, MAX_ENCODED_BYTES, type Absorber, type EncodeError } from "./cbor.js";
import type { TypeMismatch } from "./error.js";
import { isAlgorithm, startHashOf, type HashAlgorithm } from "./hash.js";
import { isArrayGuarded, readGuarded } from "./guard.js";
import { isArrayLength } from "./iterator.js";
import { err, ok, type Result } from "./result.js";
import { takeBytes } from "./bytes.js";
import { bytesView, type Value } from "./value.js";

/** The commitment tag of a byte string. */
export const BYTES_TAG = 0x00;
/** The commitment tag of any other value. */
export const CBOR_TAG = 0x01;

const tagged = (tag: number, body: Uint8Array): Uint8Array => {
  const out = new Uint8Array(body.length + 1);
  out[0] = tag;
  out.set(body, 1);
  return out;
};

/**
 * `0x00 || b` for a byte string `b`, `0x01 || cbor(v)` for any other value. The tag keeps the mapping injective: the
 * bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) commit apart.
 *
 * {@link valueCommitmentWithin} at `cbor.MAX_ENCODED_BYTES`, as `cbor.encode`: a commitment longer is `TooLarge`, and
 * what is not a canonical value a `TypeMismatch`. To hash a commitment, use `hashCommitment`, which streams it.
 */
export function valueCommitment(value: Value): Result<Uint8Array, EncodeError> {
  return valueCommitmentWithin(value, MAX_ENCODED_BYTES);
}

/**
 * {@link valueCommitment}, if it is at most `maxBytes` long (tag included; a fraction rounds down, `NaN` or a negative
 * limit is 0, and no limit is above `cbor.MAX_ENCODED_BYTES`), with the bounds of `cbor.encodeWithin`: O(min(weight, `maxBytes`)), refused at once when the weight is
 * over the limit (a commitment is longer than its value's weight), and never holding more than the limit. A value that
 * is not a canonical value, or a limit that is not a `number`, is a `TypeMismatch`, checked first, in O(1).
 */
export function valueCommitmentWithin(value: Value, maxBytes: number): Result<Uint8Array, EncodeError> {
  const checked = checkedLimit(value, maxBytes);
  if (!checked.ok) return checked;
  const limit = Math.min(checked.value, MAX_ENCODED_BYTES);
  if (value.kind === "bytes") return value.length < limit ? ok(tagged(BYTES_TAG, bytesView(value))) : err(tooLarge(limit));
  if (limit === 0) return err(tooLarge(limit));
  // the body is budgeted one byte short, for the tag; the refusal names the caller's limit
  const body = encodeWithin(value, limit - 1);
  return body.ok ? ok(tagged(CBOR_TAG, body.value)) : err(tooLarge(limit));
}

const notAnAlgorithm: TypeMismatch = { kind: "TypeMismatch", reason: "expected a HashAlgorithm" };
const notAPrefix: TypeMismatch = { kind: "TypeMismatch", reason: "expected an array of Uint8Arrays" };

/**
 * `prefix`'s parts, each a fresh copy taken from its own slots (`takeBytes`: its own `length` is not believed, and it
 * cannot change between this check and its hashing), read by index, its length once and each part once; or
 * `undefined` if it is not an array of `Uint8Array`s. A prefix is a few tags, so copying them costs nothing that matters.
 */
function prefixParts(prefix: unknown): readonly Uint8Array[] | undefined {
  const length: unknown = isArrayGuarded(prefix) ? prefix.length : undefined;
  if (!isArrayLength(length) || !isArrayGuarded(prefix)) return undefined;
  const parts: Uint8Array[] = [];
  for (let i = 0; i < length; i++) {
    // guarded: an earlier part's read (a Proxy's trap) may have revoked the array
    const part = takeBytes(readGuarded(prefix, i));
    if (part === undefined) return undefined;
    parts.push(part);
  }
  return parts;
}

/**
 * `H(prefix[0] || prefix[1] || ... || commit(value))` with `algorithm`: the commitment streamed into a hash this
 * function creates, never built (memory as `STAGED_BYTES` states), with {@link valueCommitment}'s ceiling and refusal.
 * A tagged hash passes `[H(tag), H(tag)]` as the prefix. An algorithm that is not one of `HASH_ALGORITHMS`, a prefix
 * that is not an array of `Uint8Array`s, or a value that is not a canonical value, is a `TypeMismatch`, all checked
 * before anything is hashed.
 */
export function hashCommitment(algorithm: HashAlgorithm, prefix: readonly Uint8Array[], value: Value): Result<Uint8Array, EncodeError> {
  if (!isAlgorithm(algorithm)) return err(notAnAlgorithm);
  const parts = prefixParts(prefix);
  if (parts === undefined) return err(notAPrefix);
  const checked = checkedLimit(value, MAX_ENCODED_BYTES);
  if (!checked.ok) return checked;
  const h = startHashOf(algorithm);
  for (const p of parts) h.update(p);
  const c = valueCommitmentInto(h, value);
  return c.ok ? ok(h.digest()) : c;
}

/**
 * Streams {@link valueCommitment}'s bytes into `hash` without building them, with its budget and its refusals: memory
 * as `STAGED_BYTES` states. On a refusal the hash has absorbed a prefix: discard it. The hash is only
 * handed `Feeding`'s staging array, never the value's own bytes.
 */
export function valueCommitmentInto(hash: Absorber, value: Value): Result<void, EncodeError> {
  return valueCommitmentIntoWithin(hash, value, MAX_ENCODED_BYTES);
}

/**
 * {@link valueCommitmentInto} within `maxBytes`, refusing exactly what {@link valueCommitmentWithin} refuses, a
 * `TypeMismatch` before anything is fed.
 */
export function valueCommitmentIntoWithin(hash: Absorber, value: Value, maxBytes: number): Result<void, EncodeError> {
  const checked = checkedLimit(value, maxBytes);
  if (!checked.ok) return checked;
  const limit = Math.min(checked.value, MAX_ENCODED_BYTES);
  if (value.kind === "bytes") {
    if (value.length >= limit) return err(tooLarge(limit));
    const staged = new Feeding(hash);
    staged.byte(BYTES_TAG);
    staged.bytes(bytesView(value));
    staged.flush();
    return ok(undefined);
  }
  if (limit === 0) return err(tooLarge(limit));
  // as valueCommitmentWithin: the body within one byte less, the refusal naming the caller's limit
  const feeding = new Feeding(hash);
  feeding.byte(CBOR_TAG);
  const body = encodeIntoWithin(feeding, value, limit - 1);
  return body.ok ? ok(undefined) : err(tooLarge(limit));
}
