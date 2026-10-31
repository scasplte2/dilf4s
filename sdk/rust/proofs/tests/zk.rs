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

const G1_GEN: &str = "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002";
const G2_GEN: &str = "198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa";

fn g1(h: &str) -> [u8; 64] {
    bytes(h).try_into().unwrap()
}

fn g2(h: &str) -> [u8; 128] {
    bytes(h).try_into().unwrap()
}

#[test]
fn a_verifying_key_with_an_identity_point_is_refused() {
    use dilf4s_proofs::zk::groth16::VerifyingKey;
    let (p, q, o1, o2) = (g1(G1_GEN), g2(G2_GEN), [0u8; 64], [0u8; 128]);
    assert!(VerifyingKey::from_bytes(&p, &q, &q, &q, &[p, p]).is_ok(), "finite points make a key");
    for (alpha, beta, gamma, delta, ic) in [
        (o1, q, q, q, [p, p]),
        (p, o2, q, q, [p, p]),
        (p, q, o2, q, [p, p]),
        (p, q, q, o2, [p, p]),
        (p, q, q, q, [o1, p]),
        (p, q, q, q, [p, o1]),
    ] {
        assert!(
            matches!(VerifyingKey::from_bytes(&alpha, &beta, &gamma, &delta, &ic), Err(Groth16Error::Malformed { .. })),
            "an identity in the key"
        );
    }
    // the key that would accept A = P, B = Q, C = P for every input: alpha, beta, gamma and IC all the identity
    assert!(VerifyingKey::from_bytes(&o1, &o2, &o2, &q, &[o1, o1]).is_err());
}
