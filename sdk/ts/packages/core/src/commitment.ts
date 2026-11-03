/** The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2). */
import { byteLimit, tooLarge } from "./budget.js";
import { encodeIntoWithin, encodeWithin, Feeding, MAX_ENCODED_BYTES, type Absorber } from "./cbor.js";
import type { TooLarge } from "./error.js";
import { startHash, type HashAlgorithm } from "./hash.js";
import { err, ok, type Result } from "./result.js";
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
 * {@link valueCommitmentWithin} at `cbor.MAX_ENCODED_BYTES`, as `cbor.encode`: a commitment longer is `TooLarge`. To
 * hash a commitment, stream it with {@link valueCommitmentInto} instead.
 */
export function valueCommitment(value: Value): Result<Uint8Array, TooLarge> {
  return valueCommitmentWithin(value, MAX_ENCODED_BYTES);
}

/**
 * {@link valueCommitment}, if it is at most `maxBytes` long (tag included; a fraction rounds down, `NaN` or a negative
 * limit is 0), with the bounds of `cbor.encodeWithin`: O(min(weight, `maxBytes`)), refused at once when the weight is
 * over the limit (a commitment is longer than its value's weight), and never holding more than the limit.
 */
export function valueCommitmentWithin(value: Value, maxBytes: number): Result<Uint8Array, TooLarge> {
  const limit = byteLimit(maxBytes);
  if (value.kind === "bytes") return value.length < limit ? ok(tagged(BYTES_TAG, bytesView(value))) : err(tooLarge(limit));
  if (limit === 0) return err(tooLarge(limit));
  // the body is budgeted one byte short, for the tag; the refusal names the caller's limit
  const body = encodeWithin(value, limit - 1);
  return body.ok ? ok(tagged(CBOR_TAG, body.value)) : err(tooLarge(limit));
}

/**
 * `H(prefix[0] || prefix[1] || ... || commit(value))` with `algorithm`: the commitment streamed into a hash this
 * function creates, never built (memory as `STAGED_BYTES` states), with {@link valueCommitment}'s ceiling and refusal.
 * A tagged hash passes `[H(tag), H(tag)]` as the prefix.
 */
export function hashCommitment(algorithm: HashAlgorithm, prefix: readonly Uint8Array[], value: Value): Result<Uint8Array, TooLarge> {
  const h = startHash(algorithm);
  for (const p of prefix) h.update(p);
  const c = valueCommitmentInto(h, value);
  return c.ok ? ok(h.digest()) : c;
}

/**
 * Streams {@link valueCommitment}'s bytes into `hash` without building them, with its budget and its refusals: memory
 * as `STAGED_BYTES` states. On a refusal the hash has absorbed a prefix: discard it. The hash is only
 * handed `Feeding`'s staging array, never the value's own bytes.
 */
export function valueCommitmentInto(hash: Absorber, value: Value): Result<void, TooLarge> {
  return valueCommitmentIntoWithin(hash, value, MAX_ENCODED_BYTES);
}

/** {@link valueCommitmentInto} within `maxBytes`, refusing exactly what {@link valueCommitmentWithin} refuses. */
export function valueCommitmentIntoWithin(hash: Absorber, value: Value, maxBytes: number): Result<void, TooLarge> {
  const limit = byteLimit(maxBytes);
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
