//! The SP1 guest, executed (no proving) over the shared vectors, family by family. Each family checks:
//! - the guest ran the patches' `target_os = "zkvm"` paths (it says so, and the precompiles each patch reaches for were
//!   called);
//! - every guest outcome equals the native one, byte for byte, errors included;
//! - every native outcome is what the vector says, so a bug the guest and native code share cannot pass.
//!
//! The JLVM rows are picked by operation, from the closed list (`vectors::jlvm`); two tests that execute nothing check
//! that pick: every operation runs with each outcome it gives, and an operation is read from the program, never from
//! its name's text.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::print_stdout,
    clippy::indexing_slicing,
    reason = "tests fail by panicking and report by printing"
)]

use std::collections::BTreeMap;

use dilf4s_core::generated::jlvm::JlvmOp;
use dilf4s_core::generated::jlvm_errors::JlvmSite;
use dilf4s_core::generated::jlvm_vectors::{JlvmCase, JlvmError, JlvmErrorKind, JlvmOutcome, JlvmValue};
use dilf4s_zk_cases::vectors::{self, Expect, Named, Shows};
use dilf4s_zk_cases::{Body, Case, Request, run};
use dilf4s_zk_host::execute;
use sp1_core_executor::SyscallCode;
use sp1_sdk::ExecutionReport;

/// Executes a family's cases in the guest and checks them; `syscalls` must each have been called.
async fn check(family: &str, named: Vec<Named>, syscalls: &[SyscallCode]) {
    check_beyond(family, named, syscalls, None).await;
}

/// [`check`], each of `syscalls` called more often than in `base`: a run that does what every run of the family does
/// besides its own work, so the family is not credited with calls it did not make.
async fn check_beyond(family: &str, named: Vec<Named>, syscalls: &[SyscallCode], base: Option<&ExecutionReport>) {
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
        let (count, floor) = (report.syscall_counts[*code], base.map_or(0, |b| b.syscall_counts[*code]));
        if count <= floor {
            failures.push(format!(
                "{family}: {count} {code:?} syscalls, no more than the {floor} of a run without its cases: the patch's \
                 zkVM path did not run"
            ));
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
    cases.extend(vectors::sigma_refusals().unwrap());
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
async fn kes() {
    check("kes", vectors::kes().unwrap(), &[SyscallCode::ED_ADD, SyscallCode::ED_DECOMPRESS]).await;
}

#[tokio::test]
async fn poseidon() {
    check("poseidon", vectors::poseidon().unwrap(), &[]).await;
}

/// A run's cycles: instructions plus syscalls.
fn cycles(report: &ExecutionReport) -> u64 {
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

/// What an operation gives besides a value, which every operation returns.
struct Gives {
    /// A negative verdict ([`Shows::False`]): `false`, or `ecvrf_verify`'s `{"valid": false}`.
    verdict: bool,
    /// A refusal: an error at its own site.
    refuses: bool,
}

/// What each operation of the closed list gives besides a value. An exhaustive match: a new operation does not compile
/// until it is classified here.
const fn gives(op: JlvmOp) -> Gives {
    let (verdict, refuses) = match op {
        // lazy and binding forms, and merge, which takes any values
        JlvmOp::If | JlvmOp::And | JlvmOp::Or | JlvmOp::Default | JlvmOp::Let | JlvmOp::Merge => (false, false),
        JlvmOp::Exists => (true, false),
        JlvmOp::Not
        | JlvmOp::Truthy
        | JlvmOp::Eq
        | JlvmOp::Ne
        | JlvmOp::StrictEq
        | JlvmOp::StrictNe
        | JlvmOp::Lt
        | JlvmOp::Le
        | JlvmOp::Gt
        | JlvmOp::Ge
        | JlvmOp::All
        | JlvmOp::Some
        | JlvmOp::None
        | JlvmOp::In
        | JlvmOp::Intersect
        | JlvmOp::StartsWith
        | JlvmOp::EndsWith
        | JlvmOp::Has
        | JlvmOp::Ed25519Verify
        | JlvmOp::Secp256k1Verify
        | JlvmOp::Bip340Verify
        | JlvmOp::BlsVerify
        | JlvmOp::BlsAggregateVerify
        | JlvmOp::PmtVerify
        | JlvmOp::Groth16Verify
        | JlvmOp::EcvrfVerify
        | JlvmOp::Bn254Pairing
        | JlvmOp::SchnorrVerify
        | JlvmOp::SigmaVerify
        | JlvmOp::MerkleVerify
        | JlvmOp::MerkleConsistencyVerify
        | JlvmOp::TrieVerify
        | JlvmOp::TriePrefixVerify
        | JlvmOp::TrieRangeVerify => (true, true),
        JlvmOp::Add
        | JlvmOp::Sub
        | JlvmOp::Mul
        | JlvmOp::Div
        | JlvmOp::Mod
        | JlvmOp::Min
        | JlvmOp::Max
        | JlvmOp::Abs
        | JlvmOp::Round
        | JlvmOp::RoundHalfUp
        | JlvmOp::Floor
        | JlvmOp::Ceil
        | JlvmOp::Pow
        | JlvmOp::Map
        | JlvmOp::Filter
        | JlvmOp::Reduce
        | JlvmOp::Find
        | JlvmOp::Count
        | JlvmOp::Unique
        | JlvmOp::Slice
        | JlvmOp::Reverse
        | JlvmOp::Flatten
        | JlvmOp::Length
        | JlvmOp::Cat
        | JlvmOp::Substr
        | JlvmOp::Lower
        | JlvmOp::Upper
        | JlvmOp::Join
        | JlvmOp::Split
        | JlvmOp::Trim
        | JlvmOp::Keys
        | JlvmOp::Values
        | JlvmOp::Entries
        | JlvmOp::Get
        | JlvmOp::Set
        | JlvmOp::Unset
        | JlvmOp::Missing
        | JlvmOp::MissingSome
        | JlvmOp::TypeOf
        | JlvmOp::Hex
        | JlvmOp::Unhex
        | JlvmOp::HexToInt
        | JlvmOp::BytesToInt
        | JlvmOp::IntToBytes
        | JlvmOp::Sha256
        | JlvmOp::Sha3_256
        | JlvmOp::Keccak256
        | JlvmOp::Blake2b256
        | JlvmOp::Ecrecover
        | JlvmOp::Poseidon
        | JlvmOp::Bn254Add
        | JlvmOp::Bn254Mul => (false, true),
    };
    Gives { verdict, refuses }
}

/// What an operation reaches of the SP1 patches (README.md, "The patches").
#[derive(Clone, Copy)]
enum Patched {
    /// None: the guest runs the same code as native builds.
    No,
    /// sha2 alone. Its precompiles cannot be credited to the operation's rows: SP1 hashes the guest's public values
    /// with them, as often as the output's length needs, so every family calls them, more as it has more cases (8
    /// `keccak256` rows, which hash nothing with SHA-256, make 25 `SHA_COMPRESS` calls; 10 `bn254_add` rows, 34).
    Sha2,
    /// Patches whose precompiles the operation's rows must call more often than a run without them.
    Calls(&'static [SyscallCode]),
}

impl Patched {
    /// The precompiles the operation's rows must be shown to call.
    const fn calls(self) -> &'static [SyscallCode] {
        match self {
            Self::No | Self::Sha2 => &[],
            Self::Calls(codes) => codes,
        }
    }
}

/// What each operation of the closed list reaches of the SP1 patches. An exhaustive match.
const fn patched(op: JlvmOp) -> Patched {
    match op {
        JlvmOp::Sha256
        | JlvmOp::MerkleVerify
        | JlvmOp::MerkleConsistencyVerify
        | JlvmOp::TrieVerify
        | JlvmOp::TriePrefixVerify
        | JlvmOp::TrieRangeVerify => Patched::Sha2,
        JlvmOp::Sha3_256 | JlvmOp::Keccak256 => Patched::Calls(&[SyscallCode::KECCAK_PERMUTE]),
        JlvmOp::Secp256k1Verify | JlvmOp::Bip340Verify | JlvmOp::SchnorrVerify | JlvmOp::SigmaVerify => {
            Patched::Calls(&[SyscallCode::SECP256K1_ADD, SyscallCode::SECP256K1_DOUBLE])
        }
        JlvmOp::Ecrecover => {
            Patched::Calls(&[SyscallCode::SECP256K1_ADD, SyscallCode::SECP256K1_DOUBLE, SyscallCode::KECCAK_PERMUTE])
        }
        JlvmOp::Ed25519Verify | JlvmOp::EcvrfVerify => Patched::Calls(&[SyscallCode::ED_ADD, SyscallCode::ED_DECOMPRESS]),
        JlvmOp::BlsVerify | JlvmOp::BlsAggregateVerify => {
            Patched::Calls(&[SyscallCode::BLS12381_FP_MUL, SyscallCode::BLS12381_FP2_MUL])
        }
        JlvmOp::Bn254Add | JlvmOp::Bn254Mul => Patched::Calls(&[SyscallCode::BN254_FP_ADD, SyscallCode::BN254_FP_MUL]),
        JlvmOp::Bn254Pairing | JlvmOp::Groth16Verify => {
            Patched::Calls(&[SyscallCode::BN254_FP_MUL, SyscallCode::BN254_FP2_MUL])
        }
        JlvmOp::If
        | JlvmOp::And
        | JlvmOp::Or
        | JlvmOp::Not
        | JlvmOp::Truthy
        | JlvmOp::Default
        | JlvmOp::Let
        | JlvmOp::Eq
        | JlvmOp::Ne
        | JlvmOp::StrictEq
        | JlvmOp::StrictNe
        | JlvmOp::Lt
        | JlvmOp::Le
        | JlvmOp::Gt
        | JlvmOp::Ge
        | JlvmOp::Add
        | JlvmOp::Sub
        | JlvmOp::Mul
        | JlvmOp::Div
        | JlvmOp::Mod
        | JlvmOp::Min
        | JlvmOp::Max
        | JlvmOp::Abs
        | JlvmOp::Round
        | JlvmOp::RoundHalfUp
        | JlvmOp::Floor
        | JlvmOp::Ceil
        | JlvmOp::Pow
        | JlvmOp::Map
        | JlvmOp::Filter
        | JlvmOp::Reduce
        | JlvmOp::All
        | JlvmOp::Some
        | JlvmOp::None
        | JlvmOp::Find
        | JlvmOp::Count
        | JlvmOp::Merge
        | JlvmOp::In
        | JlvmOp::Intersect
        | JlvmOp::Unique
        | JlvmOp::Slice
        | JlvmOp::Reverse
        | JlvmOp::Flatten
        | JlvmOp::Length
        | JlvmOp::Cat
        | JlvmOp::Substr
        | JlvmOp::Lower
        | JlvmOp::Upper
        | JlvmOp::Join
        | JlvmOp::Split
        | JlvmOp::Trim
        | JlvmOp::StartsWith
        | JlvmOp::EndsWith
        | JlvmOp::Keys
        | JlvmOp::Values
        | JlvmOp::Entries
        | JlvmOp::Get
        | JlvmOp::Has
        | JlvmOp::Set
        | JlvmOp::Unset
        | JlvmOp::Exists
        | JlvmOp::Missing
        | JlvmOp::MissingSome
        | JlvmOp::TypeOf
        | JlvmOp::Hex
        | JlvmOp::Unhex
        | JlvmOp::HexToInt
        | JlvmOp::BytesToInt
        | JlvmOp::IntToBytes
        // BLAKE2b and Poseidon are not patched
        | JlvmOp::Blake2b256
        | JlvmOp::Poseidon
        | JlvmOp::PmtVerify => Patched::No,
    }
}

/// Rows with real inputs the guest runs of each operation and each outcome a row shows of it ([`Shows`]), besides the
/// first [`vectors::PER_SHOWN`]: up to 8 of an operation that reaches an SP1 patch, since each gives that guest-only
/// code other inputs; none more of one that runs the same code in the guest as natively.
const fn real_inputs(op: JlvmOp) -> usize {
    if matches!(patched(op), Patched::No) { 0 } else { 8 }
}

/// The JLVM rows `vectors::jlvm` picks by operation, executed as one family per operation that reaches an SP1 patch (so
/// each operation's own rows are shown to call its patches' precompiles, sha2's aside: `Patched::Sha2`), then one
/// family of the rest.
#[tokio::test]
async fn jlvm() {
    let sample = vectors::jlvm(real_inputs).unwrap();
    // every JLVM run builds the cryptographic backend, which calls the BN254 precompiles: a run of one case that calls
    // no cryptographic operation is the floor each family's own calls must exceed
    let plain = Case::Jlvm {
        program: String::from(r#"{"+": [1, 2]}"#),
        data: String::from("null"),
        env: Vec::new(),
        built: Vec::new(),
        gas_limit: 1_000_000,
    };
    let (_, base) = execute(&Request::Run(vec![plain])).await.unwrap();
    let mut families: BTreeMap<JlvmOp, Vec<Named>> = BTreeMap::new();
    let mut rest: Vec<Named> = Vec::new();
    for row in sample.rows {
        match row.shows.map(|(op, _)| op).filter(|op| !matches!(patched(*op), Patched::No)) {
            Some(op) => families.entry(op).or_default().push(row.named),
            None => rest.push(row.named),
        }
    }
    for (op, named) in families {
        check_beyond(&format!("jlvm {}", op.wire()), named, patched(op).calls(), Some(&base)).await;
    }
    check("jlvm", rest, &[]).await;
}

/// Every operation of the closed list runs in the guest: with a value, with a negative verdict where it gives one, and
/// with a refusal where it refuses (`gives`); with every outcome `jlvm_v1.json` has of it; and an operation said never
/// to refuse has no refusal in the file.
#[test]
fn every_operation_runs_in_the_guest() {
    let sample = vectors::jlvm(real_inputs).unwrap();
    let empty = BTreeMap::new();
    let mut failures: Vec<String> = Vec::new();
    for op in JlvmOp::ALL {
        let name = op.wire();
        let run = sample.run.get(&op).unwrap_or(&empty);
        let file = sample.file.get(&op).unwrap_or(&empty);
        let refusal = |s: &Shows| matches!(s, Shows::Refusal(..));
        let Gives { verdict, refuses } = gives(op);
        if !run.contains_key(&Shows::Value) {
            failures.push(format!("{name}: no row run in the guest returns a value"));
        }
        if verdict && !run.contains_key(&Shows::False) {
            failures.push(format!("{name}: no row run in the guest returns a negative verdict"));
        }
        if refuses && !run.keys().any(refusal) {
            failures.push(format!("{name}: no row run in the guest refuses"));
        }
        if !refuses && file.keys().any(refusal) {
            failures.push(format!("{name}: said never to refuse, yet jlvm_v1.json has a refusal at its site"));
        }
        for shown in file.keys().filter(|s| !run.contains_key(*s)) {
            failures.push(format!("{name}: jlvm_v1.json shows {shown:?}, which no row run in the guest does"));
        }
        let per =
            |m: &BTreeMap<Shows, usize>| m.iter().map(|(s, n)| format!("{s:?} {n}")).collect::<Vec<_>>().join(", ");
        println!("{name}: {}", per(run));
    }
    assert!(failures.is_empty(), "{} failures:\n{}", failures.len(), failures.join("\n"));
}

/// A row as `jlvm_v1.json` has it, of the given program and data texts.
fn row(program: &str, data: &str, outcome: JlvmOutcome) -> JlvmCase {
    JlvmCase {
        name: String::from("a row"),
        program: program.into(),
        data: data.into(),
        env: None,
        built: None,
        gas_limit: None,
        outcome,
        gas_used: 0,
        warnings: 0,
    }
}

/// The selector reads which operation a row shows from the program's canonical form and the error's typed site, never
/// from text: an operation whose name is inside another's (`in` in `min`, `!` in `!=`, `hex` in `unhex`), or that is
/// named by a `var` path, a literal, an object key or the data, is not the one the row shows; an alias is the
/// operation it names.
#[test]
fn the_selector_reads_operations_not_names() {
    let value = || JlvmOutcome::JlvmValue(JlvmValue { cbor: String::from("f5"), json: None });
    let mut failures: Vec<String> = Vec::new();
    let mut pairs = 0usize;
    for outer in JlvmOp::ALL {
        for inner in JlvmOp::ALL.into_iter().filter(|o| *o != outer && outer.wire().contains(o.wire())) {
            pairs = pairs.saturating_add(1);
            let (a, b) = (inner.wire(), outer.wire());
            let program = format!(
                r#"{{"{b}": [{{"var": "{a}"}}, "{a}", {{"object": {{"{a}": "{a}"}}}}, {{"literal": {{"{a}": ["{a}"]}}}}]}}"#
            );
            let data = format!(r#"{{"{a}": "{a}"}}"#);
            let shown = vectors::shows(&row(&program, &data, value())).map(|(op, _)| op);
            if shown != Some(outer) {
                failures.push(format!("{program}: shows {shown:?}, not {b}"));
            }
        }
    }
    assert!(pairs >= 10, "only {pairs} operations' names are inside another's");
    for (alias, op) in
        [("getKey", JlvmOp::Get), ("setKey", JlvmOp::Set), ("deleteKey", JlvmOp::Unset), ("size", JlvmOp::Length)]
    {
        let program = format!(r#"{{"{alias}": [{{"var": "m"}}, "k"]}}"#);
        let shown = vectors::shows(&row(&program, "null", value())).map(|(o, _)| o);
        if shown != Some(op) {
            failures.push(format!("{program}: shows {shown:?}, not {}", op.wire()));
        }
    }
    // an error's site is typed: it is the operation that refused, whatever the root and whatever the text names
    let refused = JlvmOutcome::JlvmError(JlvmError {
        error: JlvmErrorKind::Type,
        op: Some(JlvmSite::JlvmOp(JlvmOp::Sha3_256)),
        cause: None,
    });
    let program = r#"{"cat": ["sha256", {"sha3_256": [{"var": "bls_aggregate_verify"}]}]}"#;
    let shown = vectors::shows(&row(program, "null", refused));
    if shown != Some((JlvmOp::Sha3_256, Shows::Refusal(String::from("Type"), None))) {
        failures.push(format!("{program}: shows {shown:?}, not a sha3_256 refusal"));
    }
    // a program that calls nothing shows nothing, whatever its path and its data name
    let shown = vectors::shows(&row(r#"{"var": "bip340_verify"}"#, r#"{"bip340_verify": "sha3_256"}"#, value()));
    if shown.is_some() {
        failures.push(format!("a var read naming bip340_verify shows {shown:?}"));
    }
    assert!(failures.is_empty(), "{} failures:\n{}", failures.len(), failures.join("\n"));
}
