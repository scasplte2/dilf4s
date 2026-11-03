/**
 * The bound on a verifier's caller input (docs/specs/conformance.md section 5): a proof, delta or head passed as a
 * canonical value. A value with shared parts is small in memory and can be exponentially heavy, and decoding walks it
 * as written out (copying every byte string each time it occurs), so each verifier decoder refuses a heavier input from
 * its cached weight, in O(1), before decoding it, as its malformed-input error.
 */
import { codec, models, type DecodeFailure, type Result, type Value } from "@kd5ujc/dilf4s-core";

/** The heaviest input a verifier decodes: 2^24, four times the largest snapshot chunk, so range proofs fit. */
export const MAX_PROOF_WEIGHT = BigInt(models.limits.MAX_PROOF_WEIGHT);

/** `model.decode(value)` within {@link MAX_PROOF_WEIGHT}. */
export const decodeInput = <T>(model: codec.Codec<T>, value: Value): Result<T, DecodeFailure> => codec.decodeWithin(model, value, MAX_PROOF_WEIGHT);
