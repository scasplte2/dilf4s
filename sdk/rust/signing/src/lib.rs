//! dilf4s signatures, key derivation, verifiable random functions, key-evolving signatures and sigma proofs.
//!
//! The normative specifications are `docs/specs/signing.md`, `vrf.md`, `kes.md` and `sigma.md`; the conformance vectors in
//! `shared/vectors` are part of them.
//!
//! - **Verification** of every scheme is pure Rust, `no_std` + `alloc`, and needs no feature: an SP1 guest builds this
//!   crate with `--no-default-features`.
//! - **Signing, proving, key derivation and KES evolution** are behind the default feature `sign`.
//!
//! Malformed input (a wrong length, a key that does not decode) is a typed error; a well-formed input that fails
//! verification is `false` (or, for the VRF, a typed rejection). Secret keys never print their bytes, are zeroized on
//! drop and compare in constant time. Randomised signers take their randomness explicitly.

#![no_std]
#![forbid(unsafe_code)]
#![allow(
    clippy::many_single_char_names,
    clippy::similar_names,
    reason = "the code follows the specifications' notation (r, s, k, c, u, v, ...)"
)]

extern crate alloc;

pub mod bls;
pub mod ecdsa;
pub mod ecrecover;
pub mod ed25519;
#[cfg(feature = "sign")]
pub mod ed25519_bip32;
pub mod error;
pub mod kes;
pub mod keys;
pub mod ristretto255;
pub mod scheme;
pub mod schnorr;
#[cfg(feature = "sign")]
pub mod secret;
pub mod sigma;
mod util;
pub mod vrf;

pub use error::{KesError, KeyError, SigmaError, SigningError, VrfError};
pub use scheme::{Scheme, SchemeId};
