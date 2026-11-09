//! The SP1 guest of a JLVM call (`docs/specs/zk-jlvm.md`): reads a [`Call`] in canonical form, evaluates it with the
//! Rust JLVM and commits exactly its statement's canonical CBOR as the public values. A call with no statement (an
//! input that does not decode, or a binding named twice) halts with exit code 1, and the JLVM failing to start with 2,
//! so neither has a proof that verifies.

#![no_main]

use dilf4s_zk_cases::call::{Call, jlvm, public_values, statement};

sp1_zkvm::entrypoint!(main);

/// The guest's entry point.
pub fn main() {
    let call: Call = sp1_zkvm::io::read();
    let Ok(jlvm) = jlvm() else { sp1_zkvm::syscalls::syscall_halt(2) };
    match statement(&jlvm, &call).map(|s| public_values(&s)) {
        Ok(Ok(bytes)) => sp1_zkvm::io::commit_slice(&bytes),
        Ok(Err(_)) | Err(_) => sp1_zkvm::syscalls::syscall_halt(1),
    }
}
