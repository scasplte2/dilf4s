//! `shared/vectors/zk_jlvm_v1.json` (docs/specs/zk-jlvm.md): each call's statement, computed here from its texts with
//! this crate's JLVM, is the file's, and its public values are that statement's canonical CBOR; each real SP1 Groth16
//! proof the file pins verifies under the guest's key, by the Rust SDK's verifier and through the JLVM's
//! `groth16_verify`, and a proof of other public values does not. The SP1 guest harness (`lang/rust/zk`) checks the guest
//! commits these same bytes and has this key.

#![allow(
    clippy::panic,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::indexing_slicing,
    reason = "tests fail by panicking and report by printing"
)]

extern crate alloc;

use alloc::collections::BTreeMap;

use dilf4s_core::codec::Model;
use dilf4s_core::commitment::value_commitment_into;
use dilf4s_core::generated::jlvm_vectors::JlvmError as ErrorRecord;
use dilf4s_core::generated::zk_jlvm::ZkJlvmStatement;
use dilf4s_core::generated::zk_jlvm_vectors::{
    ZkJlvmCall, ZkJlvmOutcomeHex, ZkJlvmResultHex, ZkJlvmStatementHex, ZkJlvmVectors,
};
use dilf4s_core::hash::{DomainTag, HashAlgorithm, Sha256, TaggedHash};
use dilf4s_core::{Value, cbor, hex, json};
use dilf4s_jlvm::{
    Config, CryptoCause, ErrorSite as _, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, VERSION, decode, parse,
    parse_value,
};
use dilf4s_proofs::zk::groth16;

fn load() -> ZkJlvmVectors {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../shared/vectors/zk_jlvm_v1.json");
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    ZkJlvmVectors::from_value(&json::parse(&text).unwrap()).unwrap()
}

fn digest(tag: &str, parts: impl FnOnce(&mut dilf4s_core::hash::Hashing<Sha256>)) -> String {
    let mut h = TaggedHash::<Sha256>::new(DomainTag::new(tag).unwrap()).start();
    parts(&mut h);
    hex::encode(h.finish().as_bytes())
}

fn commitment_digest(tag: &str, v: &Value) -> String {
    digest(tag, |h| value_commitment_into(h, v))
}

fn record(e: &JlvmError) -> ErrorRecord {
    use dilf4s_core::generated::jlvm_vectors as jv;
    let cause = match e {
        JlvmError::Crypto { cause: Some(c), .. } => Some(match c {
            CryptoCause::Signing { error } => {
                jv::JlvmCause::JlvmSigningCause(jv::JlvmSigningCause { signing: error.kind() })
            }
            CryptoCause::Sigma { error } => jv::JlvmCause::JlvmSigmaCause(jv::JlvmSigmaCause { sigma: error.kind() }),
            CryptoCause::Zk { error } => jv::JlvmCause::JlvmZkCause(jv::JlvmZkCause { zk: error.kind() }),
            CryptoCause::Groth16 { error } => {
                jv::JlvmCause::JlvmGroth16Cause(jv::JlvmGroth16Cause { groth16: error.kind() })
            }
            CryptoCause::Vrf { error } => jv::JlvmCause::JlvmVrfCause(jv::JlvmVrfCause { vrf: error.kind() }),
            CryptoCause::Hash { error } => jv::JlvmCause::JlvmHashCause(jv::JlvmHashCause { hash: error.kind() }),
            CryptoCause::Merkle { error } => {
                jv::JlvmCause::JlvmMerkleCause(jv::JlvmMerkleCause { merkle: error.kind() })
            }
            CryptoCause::Trie { error } => jv::JlvmCause::JlvmTrieCause(jv::JlvmTrieCause { trie: error.kind() }),
            CryptoCause::Poseidon { error } => {
                jv::JlvmCause::JlvmPoseidonCause(jv::JlvmPoseidonCause { poseidon: error.kind() })
            }
            CryptoCause::PoseidonMerkle { error } => {
                jv::JlvmCause::JlvmPoseidonMerkleCause(jv::JlvmPoseidonMerkleCause { poseidon_merkle: error.kind() })
            }
        }),
        _ => None,
    };
    ErrorRecord { error: e.kind(), op: e.site(), cause }
}

/// The statement of `call`, from the spec's definitions alone.
fn statement(jlvm: &Jlvm<Native>, call: &ZkJlvmCall) -> ZkJlvmStatementHex {
    let program = parse(&call.program).unwrap();
    let data = parse_value(&call.data).unwrap();
    let env: BTreeMap<String, Value> =
        call.env.iter().flatten().map(|(k, v)| (k.clone(), parse_value(v).unwrap())).collect();
    let config = Config { limit: GasLimit(Gas::new(call.gas_limit)), schedule: GasSchedule::LATEST };
    let outcome = jlvm.evaluate(&program, &data, &env, &config);
    let bindings = digest("dilf4s/zk/jlvm/bindings/v1", |all| {
        for (name, value) in &env {
            let e = digest("dilf4s/zk/jlvm/binding/v1", |h| {
                h.update(&cbor::encode(&Value::text(name.as_str())));
                value_commitment_into(h, value);
            });
            all.update(&hex::decode(&e).unwrap());
        }
    });
    ZkJlvmStatementHex {
        language: u64::from(VERSION),
        schedule: u64::from(GasSchedule::LATEST.version),
        program: hex::encode(&program.id(HashAlgorithm::Sha256).unwrap()),
        input: commitment_digest("dilf4s/zk/jlvm/input/v1", &data),
        bindings,
        gas_limit: call.gas_limit,
        gas_used: outcome.gas_used.value(),
        outcome: match &outcome.result {
            Ok(v) => ZkJlvmOutcomeHex::ZkJlvmResultHex(ZkJlvmResultHex {
                result: commitment_digest("dilf4s/zk/jlvm/result/v1", v),
            }),
            Err(e) => ZkJlvmOutcomeHex::JlvmError(record(e)),
        },
    }
}

/// The statement's hex form, as the file writes it.
fn as_hex(s: &ZkJlvmStatement) -> ZkJlvmStatementHex {
    use dilf4s_core::generated::zk_jlvm::ZkJlvmOutcome;
    ZkJlvmStatementHex {
        language: s.language,
        schedule: s.schedule,
        program: hex::encode(&s.program),
        input: hex::encode(&s.input),
        bindings: hex::encode(&s.bindings),
        gas_limit: s.gas_limit,
        gas_used: s.gas_used,
        outcome: match &s.outcome {
            ZkJlvmOutcome::ZkJlvmResult(r) => {
                ZkJlvmOutcomeHex::ZkJlvmResultHex(ZkJlvmResultHex { result: hex::encode(&r.result) })
            }
            ZkJlvmOutcome::JlvmError(e) => ZkJlvmOutcomeHex::JlvmError(e.clone()),
        },
    }
}

#[test]
fn each_statement_is_its_calls_and_the_public_values_are_its_encoding() {
    let v = load();
    let jlvm = Jlvm::new(Native::new().unwrap());
    assert_eq!(v.calls.len(), 8, "zk_jlvm_v1.json call coverage");
    for c in &v.calls {
        assert_eq!(statement(&jlvm, c), c.statement, "{}: the statement", c.name);
        let public = hex::decode(&c.public_values).unwrap();
        let decoded = ZkJlvmStatement::from_value(&cbor::decode(&public).unwrap()).unwrap();
        assert_eq!(as_hex(&decoded), c.statement, "{}: the public values decode to the statement", c.name);
        assert_eq!(cbor::encode(&decoded.to_value().unwrap()), public, "{}: canonical, once", c.name);
    }
}

/// The guest reads each call's program in canonical form (zk-jlvm.md section 1): the form of the call's text decodes
/// to the program the text parses to, whose identity is the statement's.
#[test]
fn each_calls_program_read_from_its_canonical_form_has_the_statements_identity() {
    let v = load();
    for c in &v.calls {
        let parsed = parse(&c.program).unwrap();
        let decoded = decode(&parsed.encode().unwrap()).unwrap();
        assert_eq!(decoded, parsed, "{}: the program", c.name);
        assert_eq!(hex::encode(&decoded.id(HashAlgorithm::Sha256).unwrap()), c.statement.program, "{}", c.name);
    }
}

fn run(jlvm: &Jlvm<Native>, vkey: &str, public: &str, proof: &str) -> Result<Value, JlvmError> {
    let program = parse(&format!(r#"{{"groth16_verify": ["0x{vkey}", "0x{public}", "0x{proof}"]}}"#)).unwrap();
    jlvm.evaluate(&program, &Value::Null, &BTreeMap::new(), &Config::default()).result
}

#[test]
fn each_pinned_proof_verifies_under_the_guests_key_and_only_for_its_public_values() {
    let v = load();
    let proved: Vec<&ZkJlvmCall> = v.calls.iter().filter(|c| c.proof_bytes.is_some()).collect();
    assert_eq!(
        proved.iter().map(|c| c.name.as_str()).collect::<Vec<_>>(),
        ["contract multisig_wallet: 2 of 3 owners sign"],
        "exact pinned Groth16 proof coverage"
    );
    let jlvm = Jlvm::new(Native::new().unwrap());
    let vkey = hex::decode(&v.program_vkey).unwrap();
    for c in proved {
        let proof = c.proof_bytes.as_deref().unwrap();
        let public = hex::decode(&c.public_values).unwrap();
        groth16::sp1::verify(&vkey, &public, &hex::decode(proof).unwrap())
            .unwrap_or_else(|e| panic!("{}: the SDK's verifier refuses it: {e:?}", c.name));
        assert_eq!(run(&jlvm, &v.program_vkey, &c.public_values, proof), Ok(Value::Bool(true)), "{}", c.name);
        // the same proof claims nothing else: another statement's bytes, or this one's with a byte changed
        let mut other = public.clone();
        let last = other.last_mut().unwrap();
        *last ^= 1;
        assert_eq!(run(&jlvm, &v.program_vkey, &hex::encode(&other), proof), Ok(Value::Bool(false)), "{}", c.name);
        let mut key = vkey.clone();
        key[31] ^= 1;
        assert_eq!(run(&jlvm, &hex::encode(&key), &c.public_values, proof), Ok(Value::Bool(false)), "{}", c.name);
    }
}
