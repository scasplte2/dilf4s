//! The JLVM call guest (docs/specs/zk-jlvm.md), executed (no proving):
//! - for each call of `zk_jlvm_v1.json`, the guest commits exactly the statement native code computes, which agrees
//!   with the call's `jlvm_v1.json` row; a success, refusals, out of gas and cryptographic operations among them;
//! - a call with no statement halts non-zero and commits nothing;
//! - the committed `zk_jlvm_v1.json` is what the guest's key and the native statements make it: a changed guest, call
//!   or statement fails here until the file is regenerated, and a pinned proof that no longer verifies, until the call
//!   is proved again.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::print_stdout,
    reason = "tests fail by panicking and report by printing"
)]

use dilf4s_zk_cases::call::{Call, decode_statement, jlvm, public_values, statement};
use dilf4s_zk_host::{execute_call, guest_key, vector};
use sp1_core_executor::SyscallCode;
use sp1_sdk::{SP1Proof, SP1ProofWithPublicValues};

#[tokio::test]
async fn the_guest_commits_the_native_statement_of_each_call() {
    let jlvm = jlvm().unwrap();
    for (row, call) in vector::calls().unwrap() {
        let native = vector::native(&jlvm, &row, &call).unwrap();
        let run = execute_call(&call).await.unwrap();
        assert_eq!(run.exit_code, 0, "{}: exit code", row.name);
        assert_eq!(run.public_values, public_values(&native).unwrap(), "{}: guest and native statements", row.name);
        assert_eq!(decode_statement(&run.public_values).unwrap(), native, "{}: the public values decode", row.name);
        println!("{}: {} cycles, gas {:?}, {:?}", row.name, run.cycles(), run.report.gas(), native.outcome);
        if row.name == vector::PROVED {
            for code in [SyscallCode::SECP256K1_ADD, SyscallCode::SECP256K1_DOUBLE, SyscallCode::KECCAK_PERMUTE] {
                assert!(run.report.syscall_counts[code] > 0, "{}: no {code:?}: the patched path did not run", row.name);
            }
        }
    }
}

fn call(program: &str, env: &[(&str, &str)]) -> Call {
    Call {
        program: program.into(),
        data: "null".into(),
        env: env.iter().map(|(k, v)| ((*k).into(), (*v).into())).collect(),
        gas_limit: 1_000_000,
    }
}

#[tokio::test]
async fn a_call_with_no_statement_halts_with_exit_code_1_and_commits_nothing() {
    let jlvm = jlvm().unwrap();
    for (what, c) in [
        ("a program that does not parse", call("{\"+\": [1", &[])),
        ("a binding that does not parse", call("{\"var\": \"$x\"}", &[("x", "{")])),
        ("a binding named twice", call("{\"var\": \"$x\"}", &[("x", "1"), ("x", "2")])),
    ] {
        assert!(statement(&jlvm, &c).is_err(), "{what}: natively, no statement");
        let run = execute_call(&c).await.unwrap();
        assert_eq!(run.exit_code, 1, "{what}: exit code");
        assert!(run.public_values.is_empty(), "{what}: committed {:?}", run.public_values);
    }
}

#[test]
fn a_binding_to_null_and_an_unbound_name_are_different_statements() {
    let jlvm = jlvm().unwrap();
    let bound = statement(&jlvm, &call("{\"var\": \"$x\"}", &[("x", "null")])).unwrap();
    let unbound = statement(&jlvm, &call("{\"var\": \"$x\"}", &[])).unwrap();
    assert_ne!(bound.bindings, unbound.bindings, "the bindings digest tells them apart");
    assert_ne!(bound.outcome, unbound.outcome, "null, against Malformed");
    // the same value under another name, and the bytes 0x6161 against the text "a", commit apart too
    let renamed = statement(&jlvm, &call("{\"var\": \"$y\"}", &[("y", "null")])).unwrap();
    assert_ne!(bound.bindings, renamed.bindings);
    let text = statement(&jlvm, &call("{\"var\": \"$x\"}", &[("x", "\"aa\"")])).unwrap();
    let bytes = statement(&jlvm, &call("{\"var\": \"$x\"}", &[("x", "\"0x6161\"")])).unwrap();
    assert_ne!(text.bindings, bytes.bindings);
    assert_ne!(text.outcome, bytes.outcome);
}

#[test]
fn the_gas_limit_and_the_program_are_in_the_statement() {
    let jlvm = jlvm().unwrap();
    let mut c = call("{\"+\": [1, 2]}", &[]);
    let a = statement(&jlvm, &c).unwrap();
    c.gas_limit = 2_000_000;
    let b = statement(&jlvm, &c).unwrap();
    assert_eq!(a.outcome, b.outcome);
    assert_ne!(a, b, "another limit is another call");
    // the surface text is transport: a program written another way is the same program
    let spaced = statement(&jlvm, &call("{ \"+\" : [ 1 , 2 ] }", &[])).unwrap();
    assert_eq!(spaced, a);
}

#[tokio::test]
async fn the_committed_vector_file_is_current() {
    let key = guest_key().await.unwrap();
    println!("guest vkey {}, ELF sha256 {}", key.vkey, key.elf_sha256);
    let committed = vector::committed().unwrap();
    let file = committed.as_ref().unwrap_or_else(|| panic!("{} is missing", vector::path().display()));
    // strict: the guest's key is the pinned one, or the pinned proof is of another program
    let pinned = format!("0x{}", file.program_vkey);
    assert!(
        key.vkey == pinned,
        "the JLVM call guest's verifying key changed: zk_jlvm_v1.json pins {pinned} (ELF sha256 {}), the guest built \
         here is {} (ELF sha256 {}). Any change to the code the guest links (sdk/rust/{{core,proofs,signing,jlvm}}, \
         sdk/rust/zk/{{cases,guest}}, sdk/Cargo.lock) does this, and the pinned proof no longer proves this guest; {}",
        file.elf_sha256,
        key.vkey,
        key.elf_sha256,
        vector::reprove()
    );
    let problems = vector::check_public_values(file);
    assert!(problems.is_empty(), "{}", problems.join("\n"));
    // the pinned proof verifies, under the pinned key, by dilf4s's verifier
    let proved = file.calls.iter().find(|c| c.name == vector::PROVED).expect("the proved call");
    let proof =
        proved.proof_bytes.as_deref().unwrap_or_else(|| panic!("{} has no proof; {}", proved.name, vector::reprove()));
    let public = dilf4s_core::hex::decode(&proved.public_values).unwrap();
    vector::verify(&key, &public, &dilf4s_core::hex::decode(proof).unwrap())
        .unwrap_or_else(|e| panic!("{}: the pinned proof does not verify ({e}); {}", proved.name, vector::reprove()));
    // and the rest of the file is what the guest and native code make it
    let expected = vector::expected(&key, committed.as_ref(), &[]).unwrap_or_else(|e| panic!("{e}"));
    let text = std::fs::read_to_string(vector::path()).unwrap();
    assert!(
        text == vector::render(&expected).unwrap(),
        "{} is stale against the calls' statements: regenerate it with `cargo run --release --manifest-path \
         host/Cargo.toml --bin dilf4s-zk-jlvm -- vector` in sdk/rust/zk (the pinned proof is kept while it verifies)",
        vector::path().display()
    );
}

/// A proof `prove` wrote, before it goes into the vectors: `DILF4S_ZK_PROOF` names its `proof-groth16.bin`, and
/// `DILF4S_ZK_CASE` the call (default: the one the vectors prove).
#[tokio::test]
#[ignore = "needs a proof file: DILF4S_ZK_PROOF=.../proof-groth16.bin"]
async fn a_proof_fixture_verifies_with_dilf4s_and_states_its_call() {
    let path = std::env::var("DILF4S_ZK_PROOF").expect("DILF4S_ZK_PROOF");
    let name = std::env::var("DILF4S_ZK_CASE").unwrap_or_else(|_| vector::PROVED.into());
    let proof = SP1ProofWithPublicValues::load(&path).unwrap();
    assert!(
        matches!(&proof.proof, SP1Proof::Groth16(p) if !p.encoded_proof.is_empty()),
        "{path}: not a real Groth16 proof"
    );
    let key = guest_key().await.unwrap();
    let public = proof.public_values.to_vec();
    let call = vector::call(&name).unwrap();
    let native = statement(&jlvm().unwrap(), &call).unwrap();
    assert_eq!(decode_statement(&public).unwrap(), native, "the proof states its call's statement");
    vector::verify(&key, &public, &proof.bytes()).unwrap();
    let mut other = public.clone();
    if let Some(b) = other.last_mut() {
        *b ^= 1;
    }
    assert!(vector::verify(&key, &other, &proof.bytes()).is_err(), "and only that");
}
