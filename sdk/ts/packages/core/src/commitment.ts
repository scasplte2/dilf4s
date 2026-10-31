/** The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2). */
import { encode } from "./cbor.js";
import type { Value } from "./value.js";

/** The commitment tag of a byte string. */
export const BYTES_TAG = 0x00;
/** The commitment tag of any other value. */
export const CBOR_TAG = 0x01;

/**
 * `0x00 || b` for a byte string `b`, `0x01 || cbor(v)` for any other value. The tag keeps the mapping injective: the
 * bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) commit apart.
 */
export function valueCommitment(value: Value): Uint8Array {
  const [tag, body] = value.kind === "bytes" ? [BYTES_TAG, value.bytes] : [CBOR_TAG, encode(value)];
  const out = new Uint8Array(body.length + 1);
  out[0] = tag;
  out.set(body, 1);
  return out;
}
