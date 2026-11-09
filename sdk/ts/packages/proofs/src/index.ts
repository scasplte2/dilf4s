/**
 * dilf4s proof verifiers: Merkle trees (docs/specs/merkle.md), binary Patricia tries (trie.md), versioned tries
 * (versioned-trie.md), Poseidon with its Merkle tree (zk.md sections 3 and 4), the BN254 group operations (zk.md
 * section 1), and Groth16 with SP1's wrapper (`zk.groth16`, section 2). Malformed input is a typed error; a well-formed
 * proof that fails is `false` (jlvm.md section 6), but a Groth16 proof, which is refused with its reason, as the
 * reference's is (`PairingCheckFailed`, `InvalidProof`, `Rejected`); an argument of the wrong type (from JavaScript, or a
 * cast) is core's `TypeMismatch`, never a verdict. Library functions never throw.
 */
export { MAX_PROOF_WEIGHT } from "./input.js";
export * as merkle from "./merkle.js";
export * as trie from "./trie.js";
export * as poseidon from "./poseidon.js";
export * as versioned from "./versioned.js";
export * as zk from "./zk.js";
