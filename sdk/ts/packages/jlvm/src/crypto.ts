/**
 * The cryptographic and proof operations (jlvm.md section 6). Not implemented yet: they call core's hashing and the
 * `signing` and `proofs` packages, whose APIs are being reworked to return typed results. Every one of them reaches
 * this one place, after its base, its arguments and its size charge (section 7), so implementing them fills in
 * {@link CRYPTO}'s handlers and nothing else. Its type is a record over every {@link CryptoOp}, so a missing or extra
 * handler does not compile.
 */
import type { Unimplemented } from "./error.js";
import type { Heap, JValue } from "./jvalue.js";
import type { CryptoOp } from "./op.js";
import type { Out } from "./values.js";

/** A cryptographic operation's handler: its evaluated arguments, and the evaluation's heap. */
export type CryptoHandler = (args: readonly JValue[], heap: Heap) => Out | { readonly ok: false; readonly error: Unimplemented };

/** The one outcome of every operation not implemented yet. */
const unimplemented =
  (op: CryptoOp): CryptoHandler =>
  () => ({ ok: false, error: { kind: "Unimplemented", op } });

/** The cryptographic operations, one handler each. */
export const CRYPTO: Readonly<Record<CryptoOp, CryptoHandler>> = Object.freeze({
  ed25519_verify: unimplemented("ed25519_verify"),
  secp256k1_verify: unimplemented("secp256k1_verify"),
  bip340_verify: unimplemented("bip340_verify"),
  ecrecover: unimplemented("ecrecover"),
  bls_verify: unimplemented("bls_verify"),
  bls_aggregate_verify: unimplemented("bls_aggregate_verify"),
  poseidon: unimplemented("poseidon"),
  pmt_verify: unimplemented("pmt_verify"),
  groth16_verify: unimplemented("groth16_verify"),
  ecvrf_verify: unimplemented("ecvrf_verify"),
  bn254_add: unimplemented("bn254_add"),
  bn254_mul: unimplemented("bn254_mul"),
  bn254_pairing: unimplemented("bn254_pairing"),
  schnorr_verify: unimplemented("schnorr_verify"),
  sigma_verify: unimplemented("sigma_verify"),
  merkle_verify: unimplemented("merkle_verify"),
  merkle_consistency_verify: unimplemented("merkle_consistency_verify"),
  trie_verify: unimplemented("trie_verify"),
  trie_prefix_verify: unimplemented("trie_prefix_verify"),
  trie_range_verify: unimplemented("trie_range_verify"),
});
