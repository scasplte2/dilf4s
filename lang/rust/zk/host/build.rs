//! Builds the guest's two programs for `riscv64im-succinct-zkvm-elf` with the succinct toolchain (`sp1up -v
//! v6.10.0`), locked to `lang/rust/Cargo.lock` so the SP1 patches and `sp1-lib` resolve exactly as committed.
//!
//! The ELFs must not depend on where the build ran: the JLVM call's verifying key is pinned in
//! `shared/vectors/zk_jlvm_v1.json`, and CI, a developer's machine and the prover must all derive the same one. Panic
//! locations embed source paths, so the checkout and `CARGO_HOME` (crates.io sources and git patch checkouts) are
//! remapped to fixed prefixes; and the guest's workspace root, `lang/rust/`, contains every path dependency, so their crate
//! hashes do not depend on the checkout's path either.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// `CARGO_HOME`, as cargo resolves it.
fn cargo_home() -> Option<PathBuf> {
    std::env::var_os("CARGO_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|h| Path::new(&h).join(".cargo")))
        .and_then(|p| p.canonicalize().ok())
}

fn remap(from: &Path, to: &str) -> String {
    format!("--remap-path-prefix={}={to}", from.display())
}

fn main() {
    // sp1-build reads the guest's metadata without --locked before its locked build, which would quietly rewrite a
    // stale lang/rust/Cargo.lock (the guest's, at its workspace root): refuse one first
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| String::from("cargo"));
    let checked = Command::new(cargo)
        .args(["metadata", "--locked", "--format-version", "1", "--manifest-path", "../guest/Cargo.toml"])
        .stdout(Stdio::null())
        .status();
    assert!(
        checked.as_ref().is_ok_and(std::process::ExitStatus::success),
        "lang/rust/Cargo.lock is not current with the guest's manifests (cargo metadata --locked: {checked:?})"
    );
    println!("cargo:rerun-if-changed=../../Cargo.lock");
    println!("cargo:rerun-if-changed=../../Cargo.toml");
    println!("cargo:rerun-if-env-changed=CARGO_HOME");
    // sp1-build would then reuse whatever ELF is cached, without building or watching the guest's sources, and the
    // strict key check (tests/jlvm_call.rs) would test a stale guest
    println!("cargo:rerun-if-env-changed=SP1_SKIP_PROGRAM_BUILD");
    assert!(
        !std::env::var("SP1_SKIP_PROGRAM_BUILD").is_ok_and(|v| v.eq_ignore_ascii_case("true")),
        "SP1_SKIP_PROGRAM_BUILD=true is refused: the guest must be built from its sources, so that its key is current"
    );
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../..");
    let root = root.canonicalize().unwrap_or(root);
    let mut rustflags = vec![remap(&root, "/dilf4s")];
    if let Some(home) = cargo_home() {
        rustflags.push(remap(&home, "/cargo"));
    }
    sp1_build::build_program_with_args(
        "../guest",
        sp1_build::BuildArgs { locked: true, rustflags, ..Default::default() },
    );
}
