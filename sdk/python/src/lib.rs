//! `dilf4s._native`: the dilf4s Python package's native module, `PyO3` bindings over the Rust SDK.
//!
//! The Python package (`python/dilf4s`) re-exports it with types (`_native.pyi`). Every function takes Python objects
//! and gives an `Ok` or an `Err` (see [`boundary`]); no refusal is ever a Python exception. Nothing here is `unsafe`
//! or panics: the crate denies both, as the Rust SDK does, and `PyO3` turns any panic that did occur into a Python
//! exception rather than an abort.

#![allow(
    clippy::many_single_char_names,
    reason = "arguments are named as the specifications and the Rust SDK name them (k, m, s, p, r, ...)"
)]

extern crate alloc;

mod boundary;
mod core_api;
mod errors;
mod jlvm;
mod proofs;
mod signing;
mod value;
mod versioned;

use pyo3::prelude::*;

/// The module.
#[pymodule]
fn _native(m: &Bound<'_, PyModule>) -> PyResult<()> {
    value::register(m)?;
    core_api::register(m)?;
    jlvm::register(m)?;
    signing::register(m)?;
    proofs::register(m)?;
    versioned::register(m)?;
    Ok(())
}
