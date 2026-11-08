/**
 * The bound on a verifier's caller input (docs/specs/conformance.md section 5): a proof, delta or head passed as a
 * canonical value. A value with shared parts is small in memory and can be exponentially heavy, and decoding walks it
 * as written out (copying every byte string each time it occurs), so each verifier decoder refuses a heavier input from
 * its cached weight, in O(1), before decoding it, as its malformed-input error.
 */
import { codec, models, type DecodeFailure, type Result, type Value } from "@kd5ujc/dilf4s-core";
import { bitLengthOf } from "@kd5ujc/dilf4s-core/internal";

/** The heaviest input a verifier decodes: 2^24, four times the largest snapshot chunk, so range proofs fit. */
export const MAX_PROOF_WEIGHT = BigInt(models.limits.MAX_PROOF_WEIGHT);

/**
 * `n`'s bit length, for a refusal's numeric field or message: core's `bitLength`, unchecked. Every `bigint` that reaches
 * it was checked to be one where it entered (a public entry point refuses anything else as a `TypeMismatch`), or was
 * decoded by a codec, so there is nothing for it to refuse.
 */
export const bitsOf = (n: bigint): number => bitLengthOf(n);

/** `model.decode(value)` within {@link MAX_PROOF_WEIGHT}. */
export const decodeInput = <T>(model: codec.Codec<T>, value: Value): Result<T, DecodeFailure> => codec.decodeWithin(model, value, MAX_PROOF_WEIGHT);
