//! dilf4s proof verifiers: Merkle trees (`docs/specs/merkle.md`), binary Patricia tries (`trie.md`), versioned tries
//! (`versioned-trie.md`), and Poseidon with its Merkle tree (`zk.md` §3–4).
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

pub use bits::Bits;
