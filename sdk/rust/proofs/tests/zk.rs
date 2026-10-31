//! `shared/vectors/zk_v1.json`: the BN254 operations on exact-width bytes and SP1 Groth16, each case with the outcome
//! the reference gives, by kind.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{bytes, load};
use dilf4s_core::generated::zk_vectors::{ZkBoolResult, ZkError as Kind, ZkGroth16Result, ZkPointResult, ZkVectors};
use dilf4s_proofs::zk::groth16::{Groth16Error, sp1};
use dilf4s_proofs::zk::{self, ZkError};

fn vectors() -> ZkVectors {
    load("zk_v1.json")
}

fn kind(e: &ZkError) -> Kind {
    match e {
        ZkError::Malformed { .. } => Kind::Malformed,
        ZkError::InvalidPoint { .. } => Kind::InvalidPoint,
    }
}

fn point(r: Result<[u8; 64], ZkError>) -> ZkPointResult {
    match r {
        Ok(p) => {
            ZkPointResult::ZkPoint(dilf4s_core::generated::zk_vectors::ZkPoint { point: dilf4s_core::hex::encode(&p) })
        }
        Err(e) => ZkPointResult::ZkFailure(dilf4s_core::generated::zk_vectors::ZkFailure { error: kind(&e) }),
    }
}

#[test]
fn add() {
    let v = vectors();
    assert_eq!(v.add.len(), 20);
    for c in v.add {
        assert_eq!(point(zk::add(&bytes(&c.a), &bytes(&c.b))), c.result, "{}", c.name);
    }
}

#[test]
fn mul() {
    let v = vectors();
    assert_eq!(v.mul.len(), 23);
    for c in v.mul {
        assert_eq!(point(zk::mul(&bytes(&c.point), &bytes(&c.scalar))), c.result, "{}", c.name);
    }
}

#[test]
fn pairing() {
    let v = vectors();
    assert_eq!(v.pairing.len(), 21);
    for c in v.pairing {
        let owned: Vec<(Vec<u8>, Vec<u8>)> = c.pairs.iter().map(|p| (bytes(&p.g1), bytes(&p.g2))).collect();
        let pairs: Vec<(&[u8], &[u8])> = owned.iter().map(|(a, b)| (a.as_slice(), b.as_slice())).collect();
        let got = match zk::pairing(&pairs) {
            Ok(value) => ZkBoolResult::ZkBool(dilf4s_core::generated::zk_vectors::ZkBool { value }),
            Err(e) => ZkBoolResult::ZkFailure(dilf4s_core::generated::zk_vectors::ZkFailure { error: kind(&e) }),
        };
        assert_eq!(got, c.result, "{}", c.name);
    }
}

fn outcome(r: &Result<(), Groth16Error>) -> ZkGroth16Result {
    match r {
        Ok(()) => ZkGroth16Result::Verified,
        Err(Groth16Error::Malformed { .. }) => ZkGroth16Result::Malformed,
        Err(Groth16Error::InvalidProof { .. }) => ZkGroth16Result::InvalidProof,
        Err(Groth16Error::PairingCheckFailed) => ZkGroth16Result::PairingCheckFailed,
        Err(Groth16Error::Rejected { .. }) => ZkGroth16Result::Rejected,
    }
}

#[test]
fn sp1_groth16() {
    let v = vectors();
    assert_eq!(v.sp1_groth16.len(), 16);
    let vk = sp1::verifying_key().expect("the SP1 verifying key's points are valid");
    assert_eq!(vk.public_inputs(), 5);
    for c in v.sp1_groth16 {
        let r = sp1::verify_with(&vk, &bytes(&c.program_vkey), &bytes(&c.public_values), &bytes(&c.proof_bytes));
        assert_eq!(outcome(&r), c.result, "{}: {r:?}", c.name);
    }
}

#[test]
fn sp1_verify_builds_its_own_key() {
    let c = vectors().sp1_groth16.into_iter().next().unwrap();
    assert_eq!(
        sp1::verify(&bytes(&c.program_vkey), &bytes(&c.public_values), &bytes(&c.proof_bytes)),
        Ok(()),
        "{}",
        c.name
    );
}

#[test]
fn the_public_values_hash_is_below_two_to_the_253() {
    let h = sp1::hash_public_values(b"any public values");
    assert_eq!(h[0] & 0xe0, 0);
}
