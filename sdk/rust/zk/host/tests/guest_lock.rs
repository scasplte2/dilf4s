//! The guest's `Cargo.lock`, `sdk/Cargo.lock` at its workspace root (which `build.rs` builds `--locked`), resolves every patched crate to its SP1 patch and to
//! nothing else: cargo only warns about a `[patch]` entry that does not apply.

#![allow(clippy::panic, clippy::indexing_slicing, reason = "tests fail by panicking")]

/// A locked package: its name, version and source.
struct Locked {
    name: String,
    version: String,
    source: Option<String>,
}

fn field(block: &str, key: &str) -> Option<String> {
    block.lines().find_map(|l| l.strip_prefix(key)?.strip_prefix(" = \"")?.strip_suffix('"').map(String::from))
}

fn locked() -> Vec<Locked> {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../Cargo.lock");
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    text.split("[[package]]")
        .skip(1)
        .map(|b| Locked {
            name: field(b, "name").unwrap_or_else(|| panic!("a package with no name: {b}")),
            version: field(b, "version").unwrap_or_default(),
            source: field(b, "source"),
        })
        .collect()
}

/// The SP1 v6.10.0 patch of each crate: its repository and tag.
const PATCHED: [(&str, &str, &str); 8] = [
    ("k256", "elliptic-curves", "patch-k256-13.4-sp1-6.10.0"),
    ("p256", "elliptic-curves", "patch-p256-13.2-sp1-6.10.0"),
    ("curve25519-dalek", "curve25519-dalek", "patch-4.1.3-sp1-6.10.0"),
    ("bls12_381", "bls12_381", "patch-0.8.0-sp1-6.10.0"),
    ("sha2", "RustCrypto-hashes", "patch-sha2-0.10.9-sp1-6.10.0"),
    ("sha3", "RustCrypto-hashes", "patch-sha3-0.10.8-sp1-6.10.0"),
    ("substrate-bn", "bn", "patch-0.6.0-sp1-6.10.0-substrate-bn"),
    ("crypto-bigint", "RustCrypto-bigint", "patch-0.5.5-sp1-6.10.0"),
];

#[test]
fn every_patched_crate_resolves_to_its_patch_only() {
    let lock = locked();
    for (name, repo, tag) in PATCHED {
        let copies: Vec<&Locked> = lock.iter().filter(|p| p.name == name).collect();
        assert!(!copies.is_empty(), "{name} is not in the guest's graph");
        let want = format!("git+https://github.com/sp1-patches/{repo}?tag={tag}#");
        for p in copies {
            let source = p.source.as_deref().unwrap_or("a path");
            assert!(source.starts_with(&want), "{name} {} comes from {source}, not {want}", p.version);
        }
    }
}

#[test]
fn k256_uses_the_ecdsa_fork_and_sp1_lib_is_6_10_0() {
    let lock = locked();
    let fork = "git+https://github.com/sp1-patches/signatures?tag=sp1-skip-verify-on-recovery#";
    assert!(
        lock.iter().any(|p| p.name == "ecdsa" && p.source.as_deref().is_some_and(|s| s.starts_with(fork))),
        "the k256 patch's ecdsa fork is not in the graph"
    );
    let sp1: Vec<&Locked> = lock.iter().filter(|p| p.name == "sp1-lib" || p.name == "sp1-zkvm").collect();
    assert_eq!(sp1.len(), 2, "one sp1-lib and one sp1-zkvm");
    for p in sp1 {
        assert_eq!(p.version, "6.10.0", "{}", p.name);
    }
}
