//! dilf4s core: the canonical data model, its deterministic CBOR encoding and JSON mapping, tagged hashing, value
//! commitments, and the wire models generated from `shared/schemas`.
//!
//! The normative specification is `docs/specs/canonical-encoding.md`; the conformance vectors in `shared/vectors`
//! are part of it. The crate is `no_std` (it needs `alloc`), pure Rust, and never panics on any input.

#![no_std]
#![forbid(unsafe_code)]

extern crate alloc;

pub mod cbor;
pub mod codec;
pub mod commitment;
pub mod error;
#[rustfmt::skip]
pub mod generated;
pub mod hash;
pub mod hex;
pub mod json;
pub mod value;

pub use error::{CanonicalError, DecodeFailure};
pub use num_bigint::BigInt;
pub use value::{Array, Int, MAX_DEPTH, MAX_WEIGHT, Map, Rational, Text, Value};
