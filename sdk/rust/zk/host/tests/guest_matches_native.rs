//! The SP1 guest, executed (no proving) over the shared vectors, family by family. Each family checks:
//! - the guest ran the patches' `target_os = "zkvm"` paths (it says so, and the precompiles each patch reaches for were
//!   called);
//! - every guest outcome equals the native one, byte for byte, errors included;
//! - every native outcome is what the vector says, so a bug the guest and native code share cannot pass.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::print_stdout,
    clippy::indexing_slicing,
    reason = "tests fail by panicking and report by printing"
)]

use dilf4s_zk_cases::vectors::{self, Expect, Named};
use dilf4s_zk_cases::{Body, Request, run};
use dilf4s_zk_host::execute;
use sp1_core_executor::SyscallCode;

/// Executes a family's cases in the guest and checks them; `syscalls` must each have been called.
async fn check(family: &str, named: Vec<Named>, syscalls: &[SyscallCode]) {
    assert!(!named.is_empty(), "{family}: no cases");
    let cases: Vec<_> = named.iter().map(|n| n.case.clone()).collect();
    let native = run(&cases);
    let (response, report) = execute(&Request::Run(cases)).await.unwrap_or_else(|e| panic!("{family}: {e}"));
    assert!(response.zkvm, "{family}: the guest was not built for target_os = \"zkvm\"");
    let Body::Run(guest) = response.body else { panic!("{family}: the guest answered another request") };
    assert_eq!(guest.len(), named.len(), "{family}: outcome count");

    let mut failures: Vec<String> = Vec::new();
    for ((n, g), x) in named.iter().zip(&guest).zip(&native) {
        if g != x {
            failures.push(format!("{}: guest {g:?}, native {x:?}", n.name));
        }
        if !n.expect.holds(x) {
            failures.push(format!("{}: native {x:?}, the vector says {:?}", n.name, n.expect));
        }
    }
    failures.extend(Expect::signer_pairs_hold(&named, &native));
    for code in syscalls {
        if report.syscall_counts[*code] == 0 {
            failures.push(format!("{family}: no {code:?} syscall: the patch's zkVM path did not run"));
        }
    }
    let precompiles: Vec<String> = report
        .syscall_counts
        .iter()
        .filter(|(code, count)| {
            **count > 0
                && !matches!(
                    code,
                    SyscallCode::WRITE
                        | SyscallCode::HINT_LEN
                        | SyscallCode::HINT_READ
                        | SyscallCode::COMMIT
                        | SyscallCode::HALT
                )
        })
        .map(|(code, count)| format!("{code:?} {count}"))
        .collect();
    println!(
        "{family}: {} cases, {} instructions, {} syscalls ({})",
        named.len(),
        report.total_instruction_count(),
        report.total_syscall_count(),
        precompiles.join(", ")
    );
    assert!(failures.is_empty(), "{family}: {} failures:\n{}", failures.len(), failures.join("\n"));
}

fn scheme(names: &'static [&'static str]) -> impl Fn(&str) -> bool {
    move |s| names.contains(&s)
}

#[tokio::test]
async fn ed25519() {
    let cases = vectors::signatures(scheme(&["Ed25519", "Ed25519-libsodium"])).unwrap();
    check("ed25519", cases, &[SyscallCode::ED_ADD, SyscallCode::ED_DECOMPRESS]).await;
}

#[tokio::test]
async fn ristretto255() {
    let cases = vectors::signatures(scheme(&["ristretto255-Schnorr"])).unwrap();
    check("ristretto255", cases, &[SyscallCode::ED_ADD]).await;
}

#[tokio::test]
async fn secp256k1() {
    let cases = vectors::signatures(scheme(&["secp256k1-ECDSA", "BIP-340", "secp256k1-tagged-Schnorr"])).unwrap();
    check("secp256k1", cases, &[SyscallCode::SECP256K1_ADD, SyscallCode::SECP256K1_DOUBLE, SyscallCode::SHA_COMPRESS])
        .await;
}

#[tokio::test]
async fn p256() {
    let cases = vectors::signatures(scheme(&["P-256-ECDSA"])).unwrap();
    check("p256", cases, &[SyscallCode::SECP256R1_ADD, SyscallCode::SECP256R1_DOUBLE]).await;
}

#[tokio::test]
async fn bls() {
    let cases = vectors::signatures(scheme(&["BLS12-381-PoP"])).unwrap();
    check("bls", cases, &[SyscallCode::BLS12381_FP_MUL, SyscallCode::BLS12381_FP2_MUL]).await;
}

#[tokio::test]
async fn ecrecover() {
    check("ecrecover", vectors::ecrecover().unwrap(), &[SyscallCode::SECP256K1_ADD, SyscallCode::KECCAK_PERMUTE]).await;
}

#[tokio::test]
async fn sigma() {
    let mut cases = vectors::sigma("sigma_v1.json").unwrap();
    cases.extend(vectors::sigma("sigma_dilf4s_v1.json").unwrap());
    check("sigma", cases, &[SyscallCode::SECP256K1_ADD, SyscallCode::SECP256K1_DOUBLE]).await;
}

#[tokio::test]
async fn bn254_and_groth16() {
    check(
        "bn254",
        vectors::bn254().unwrap(),
        &[SyscallCode::BN254_FP_ADD, SyscallCode::BN254_FP_MUL, SyscallCode::BN254_FP2_MUL],
    )
    .await;
}

#[tokio::test]
async fn poseidon() {
    check("poseidon", vectors::poseidon().unwrap(), &[]).await;
}

/// A run's cycles: instructions plus syscalls.
fn cycles(report: &sp1_sdk::ExecutionReport) -> u64 {
    report.total_instruction_count().saturating_add(report.total_syscall_count())
}

/// One VRF suite: every committed proof and rejection row, then the guest's cycles and gas per verification of a
/// committed proof (its run less an empty run's), and its `ED_ADD` calls.
async fn vrf(suite: &str) {
    check(suite, vectors::vrf(suite, true).unwrap(), &[SyscallCode::ED_ADD, SyscallCode::ED_DECOMPRESS]).await;
    let proofs: Vec<_> = vectors::vrf(suite, false).unwrap().into_iter().map(|n| n.case).collect();
    let (_, empty) = execute(&Request::Run(Vec::new())).await.unwrap();
    let (_, report) = execute(&Request::Run(proofs.clone())).await.unwrap();
    let n = u64::try_from(proofs.len()).unwrap();
    let per = |run: u64, base: u64| run.saturating_sub(base).checked_div(n).unwrap();
    let cycles_per = per(cycles(&report), cycles(&empty));
    let gas_per = per(report.gas().unwrap(), empty.gas().unwrap());
    let ed_add = report.syscall_counts[SyscallCode::ED_ADD].checked_div(n).unwrap();
    println!("{suite}: {cycles_per} cycles, {gas_per} gas, {ed_add} ED_ADD per verification ({} proofs)", proofs.len());
}

#[tokio::test]
async fn vrf_tai() {
    vrf("ECVRF-EDWARDS25519-SHA512-TAI").await;
}

#[tokio::test]
async fn vrf_ell2() {
    vrf("ECVRF-EDWARDS25519-SHA512-ELL2").await;
}

#[tokio::test]
async fn vrf_draft03() {
    vrf("ECVRF-ED25519-SHA512-Elligator2 (draft-03)").await;
}

/// The cryptographic operations' names, as JLVM programs call them.
const CRYPTO_OPS: [&str; 16] = [
    "ecrecover",
    "secp256k1_verify",
    "ed25519_verify",
    "bls_verify",
    "sigma",
    "groth16",
    "bn254",
    "poseidon",
    "keccak",
    "sha256",
    "p256",
    "schnorr",
    "vrf",
    "pmt_",
    "merkle",
    "trie",
];

/// Every JLVM case that calls a cryptographic operation on real inputs (not the argument-kind sweeps), and every
/// 50th other case.
#[tokio::test]
async fn jlvm() {
    let cases = vectors::jlvm(|i, c| {
        let crypto = CRYPTO_OPS.iter().any(|op| c.program.contains(op))
            && !c.name.contains(" × ")
            && !c.name.contains("no arguments");
        crypto || i % 50 == 0
    })
    .unwrap();
    check(
        "jlvm",
        cases,
        &[
            SyscallCode::SHA_COMPRESS,
            SyscallCode::KECCAK_PERMUTE,
            SyscallCode::SECP256K1_ADD,
            SyscallCode::ED_ADD,
            SyscallCode::BN254_FP_MUL,
            SyscallCode::BLS12381_FP_MUL,
        ],
    )
    .await;
}
