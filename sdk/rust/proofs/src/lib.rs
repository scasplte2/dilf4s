//! dilf4s proof verifiers: Merkle trees (`docs/specs/merkle.md`), binary Patricia tries (`trie.md`), versioned tries
//! (`versioned-trie.md`), BN254 and Groth16 (`zk.md` §1–2), and Poseidon with its Merkle tree (`zk.md` §3–4).
//!
//! Malformed input is a typed error; a well-formed proof that does not prove the claim is `false` (or `None`), as the
//! JLVM's verification ops require (`jlvm.md` §6). The crate is `no_std` (it needs `alloc`), pure Rust, and never
//! panics or recurses on input depth.

#![no_std]
#![forbid(unsafe_code)]

extern crate alloc;

pub mod bits;
pub mod merkle;
pub mod poseidon;
mod tag;
pub mod trie;
pub mod versioned;
pub mod zk;

pub use bits::Bits;

/// The heaviest caller input a verifier decodes: a proof, delta or head passed as a [`dilf4s_core::value::Value`]
/// (docs/specs/conformance.md section 5). 2^24, four times the largest snapshot chunk, so range proofs over large
/// listings fit. A heavier input is refused in O(1), from its cached weight, before it is decoded, as the verifier's
/// malformed-input error: a value with shared parts is small in memory, and decoding walks it as written out.
pub const MAX_PROOF_WEIGHT: u64 = 1 << 24;
