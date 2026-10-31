//! Models generated from `schemas/*.cddl`, fixtures that exercise the Rust emitter where the real schemas do not
//! (for example, aliases whose types live in another module). Compiling this crate is the test.
//!
//! Regenerate with `sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain sdk/rust/fixtures/schemas - sdk/rust/fixtures/src/generated"`.

#![no_std]
#![forbid(unsafe_code)]

extern crate alloc;

/// The codec runtime, where generated code expects it (`crate::codec`).
pub use dilf4s_core::codec;

#[rustfmt::skip]
pub mod generated;
