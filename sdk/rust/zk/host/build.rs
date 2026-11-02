//! Builds the guest for `riscv64im-succinct-zkvm-elf` with the succinct toolchain (`sp1up -v v6.10.0`), locked to its
//! `Cargo.lock` so the SP1 patches and `sp1-lib` resolve exactly as committed.

use std::process::{Command, Stdio};

fn main() {
    // sp1-build reads the guest's metadata without --locked before its locked build, which would quietly rewrite a
    // stale guest/Cargo.lock: refuse one first
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| String::from("cargo"));
    let checked = Command::new(cargo)
        .args(["metadata", "--locked", "--format-version", "1", "--manifest-path", "../guest/Cargo.toml"])
        .stdout(Stdio::null())
        .status();
    assert!(
        checked.as_ref().is_ok_and(std::process::ExitStatus::success),
        "guest/Cargo.lock is not current with guest/Cargo.toml (cargo metadata --locked: {checked:?})"
    );
    println!("cargo:rerun-if-changed=../guest/Cargo.lock");
    sp1_build::build_program_with_args("../guest", sp1_build::BuildArgs { locked: true, ..Default::default() });
}
