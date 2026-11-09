"""Proof verifiers: Merkle trees, tries, versioned tries, Poseidon, BN254, Groth16 and SP1's verifier.

The specifications are docs/specs/merkle.md, trie.md, versioned-trie.md and zk.md. Malformed input is a typed error; a
well-formed proof that does not prove the claim is `False` (or `None`), as the JLVM's verification operations require.
The hash is chosen by name (SHA-256 unless given). Proofs are passed in their canonical form, a `Value`, and decoded
within `MAX_PROOF_WEIGHT`. Pairings, Groth16 and SP1 verification release the GIL.
"""

from typing import Final

from dilf4s import _native
from dilf4s._native import (
    ConsistencyProof,
    Groth16VerifyingKey,
    InclusionProof,
    KeyMode,
    MerkleHashing,
    PoseidonMerkleProof,
    Replica,
    TrieHashing,
    VerifiedState,
    VerifiedTrieProof,
    VersionedTrie,
    VersionKey,
    bn254_add,
    bn254_mul,
    bn254_pairing,
    fr_from_bytes,
    fr_to_bytes,
    groth16_verify,
    merkle_check_consistency,
    merkle_check_inclusion,
    merkle_consistency_verify,
    merkle_head,
    merkle_root,
    merkle_verify,
    merkle_verify_consistency,
    merkle_verify_inclusion,
    poseidon_compress,
    poseidon_hash,
    poseidon_merkle_root,
    poseidon_verify_absence,
    poseidon_verify_inclusion,
    poseidon_zero_hashes,
    sp1_hash_public_values,
    sp1_verify,
    state_key_from_bytes,
    state_key_parse,
    state_namespace_parse,
    trie_check,
    trie_prefix_verify,
    trie_range_verify,
    trie_root,
    trie_verify,
    trie_verify_proof,
)
from dilf4s._proof_types import (
    Absent,
    Delta,
    HeadChunk,
    HistoryChunk,
    Listing,
    Lookup,
    NotCovered,
    Present,
    SnapshotChunk,
    StateChunk,
    VersionedHead,
)
from dilf4s.errors.groth16 import Groth16Error, Groth16ErrorKind
from dilf4s.errors.merkle import MerkleError, MerkleErrorKind
from dilf4s.errors.poseidon import PoseidonError, PoseidonErrorKind
from dilf4s.errors.poseidon_merkle import PoseidonMerkleError, PoseidonMerkleErrorKind
from dilf4s.errors.state_key import StateKeyError, StateKeyErrorKind
from dilf4s.errors.trie import TrieError, TrieErrorKind
from dilf4s.errors.versioned_trie import VersionedTrieError, VersionedTrieErrorKind
from dilf4s.errors.zk import ZkError, ZkErrorKind

MAX_PROOF_WEIGHT: Final[int] = _native.PROOFS_MAX_PROOF_WEIGHT
"""The heaviest proof, delta or head a verifier decodes, and the most claims weigh together."""
POSEIDON_MAX_INPUTS: Final[int] = _native.POSEIDON_MAX_INPUTS
"""The most field elements Poseidon hashes at once."""
POSEIDON_MAX_DEPTH: Final[int] = _native.POSEIDON_MAX_DEPTH
"""The deepest Poseidon Merkle tree."""
FR_MODULUS: Final[int] = _native.POSEIDON_FR_MODULUS
"""BN254's scalar field order `r`: a field element is an `int` in `[0, r)`."""
TRIE_MAX_FIXED_BYTES: Final[int] = _native.TRIE_MAX_FIXED_BYTES
"""The longest `fixed(n)` key."""
TRIE_MAX_VARIABLE_BYTES: Final[int] = _native.TRIE_MAX_VARIABLE_BYTES
"""The largest `variable(max)` bound."""
GROTH16_PROOF_BYTES: Final[int] = _native.GROTH16_PROOF_BYTES
"""A Groth16 proof's length, and SP1's framed proof's."""
VERSIONED_MAX_VALUE_BYTES: Final[int] = _native.VERSIONED_MAX_VALUE_BYTES
"""The longest value a versioned trie holds, in canonical CBOR bytes."""
VERSIONED_MAX_VALUE_DEPTH: Final[int] = _native.VERSIONED_MAX_VALUE_DEPTH
"""The deepest value a versioned trie holds."""
VERSIONED_MAX_CHUNK_ENTRIES: Final[int] = _native.VERSIONED_MAX_CHUNK_ENTRIES
"""The most entries a snapshot chunk holds."""
VERSIONED_MAX_CHUNK_BYTES: Final[int] = _native.VERSIONED_MAX_CHUNK_BYTES
"""The longest snapshot chunk."""

__all__ = [
    "FR_MODULUS",
    "GROTH16_PROOF_BYTES",
    "MAX_PROOF_WEIGHT",
    "POSEIDON_MAX_DEPTH",
    "POSEIDON_MAX_INPUTS",
    "TRIE_MAX_FIXED_BYTES",
    "TRIE_MAX_VARIABLE_BYTES",
    "VERSIONED_MAX_CHUNK_BYTES",
    "VERSIONED_MAX_CHUNK_ENTRIES",
    "VERSIONED_MAX_VALUE_BYTES",
    "VERSIONED_MAX_VALUE_DEPTH",
    "Absent",
    "ConsistencyProof",
    "Delta",
    "Groth16Error",
    "Groth16ErrorKind",
    "Groth16VerifyingKey",
    "HeadChunk",
    "HistoryChunk",
    "InclusionProof",
    "KeyMode",
    "Listing",
    "Lookup",
    "MerkleError",
    "MerkleErrorKind",
    "MerkleHashing",
    "NotCovered",
    "PoseidonError",
    "PoseidonErrorKind",
    "PoseidonMerkleError",
    "PoseidonMerkleErrorKind",
    "PoseidonMerkleProof",
    "Present",
    "Replica",
    "SnapshotChunk",
    "StateChunk",
    "StateKeyError",
    "StateKeyErrorKind",
    "TrieError",
    "TrieErrorKind",
    "TrieHashing",
    "VerifiedState",
    "VerifiedTrieProof",
    "VersionKey",
    "VersionedHead",
    "VersionedTrie",
    "VersionedTrieError",
    "VersionedTrieErrorKind",
    "ZkError",
    "ZkErrorKind",
    "bn254_add",
    "bn254_mul",
    "bn254_pairing",
    "fr_from_bytes",
    "fr_to_bytes",
    "groth16_verify",
    "merkle_check_consistency",
    "merkle_check_inclusion",
    "merkle_consistency_verify",
    "merkle_head",
    "merkle_root",
    "merkle_verify",
    "merkle_verify_consistency",
    "merkle_verify_inclusion",
    "poseidon_compress",
    "poseidon_hash",
    "poseidon_merkle_root",
    "poseidon_verify_absence",
    "poseidon_verify_inclusion",
    "poseidon_zero_hashes",
    "sp1_hash_public_values",
    "sp1_verify",
    "state_key_from_bytes",
    "state_key_parse",
    "state_namespace_parse",
    "trie_check",
    "trie_prefix_verify",
    "trie_range_verify",
    "trie_root",
    "trie_verify",
    "trie_verify_proof",
]
