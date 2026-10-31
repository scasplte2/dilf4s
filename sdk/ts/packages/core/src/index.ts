/**
 * dilf4s core: the canonical data model, its deterministic CBOR encoding and JSON mapping, tagged hashing, value
 * commitments, and the wire models generated from `shared/schemas`. The normative specification is
 * `docs/specs/canonical-encoding.md`; the conformance vectors in `shared/vectors` are part of it.
 */
export * as cbor from "./cbor.js";
export * as codec from "./codec.js";
export * as json from "./json.js";
export * as models from "./generated/index.js";
export { valueCommitment, BYTES_TAG, CBOR_TAG } from "./commitment.js";
export type { CanonicalError, DecodeFailure } from "./error.js";
export { describeFailure } from "./error.js";
export {
  contentHash,
  digest,
  DomainTag,
  HASH_ALGORITHMS,
  hashAlgorithm,
  hashParts,
  outputSize,
  TaggedHash,
  taggedHash,
  type HashAlgorithm,
  type InvalidDomainTag,
} from "./hash.js";
export { fromHex, toHex, type InvalidHex } from "./hex.js";
export { all, andThen, err, map, ok, type Err, type Ok, type Result } from "./result.js";
export { ArrayValue, BoolValue, BytesValue, IntValue, MapValue, MAX_DEPTH, NullValue, RationalValue, TextValue, Value } from "./value.js";
