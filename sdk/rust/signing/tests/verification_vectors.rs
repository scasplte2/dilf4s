//! The verification side of `shared/vectors/signing_v1.json`, without the `sign` feature (the SP1 guest's build):
//! every committed signature and proof of possession verifies, and every `verification` verdict is reproduced.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{bytes, load};
use dilf4s_core::generated::signing_vectors::{SigningVectors, VerificationResult};
use dilf4s_signing::{SchemeId, SigningError, bls, ecdsa, ed25519, ristretto255, schnorr};

/// Decodes just the verifying key of a scheme, to tell a malformed key from a malformed signature.
fn key_decodes(id: SchemeId, key: &[u8]) -> Result<(), SigningError> {
    match id {
        SchemeId::Ed25519 | SchemeId::Ed25519Libsodium => ed25519::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::P256Ecdsa => ecdsa::p256::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::Secp256k1Ecdsa => ecdsa::secp256k1::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::Bip340 => schnorr::bip340::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::TaggedSchnorr => schnorr::tagged::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::Ristretto255Schnorr => ristretto255::VerifyingKey::from_bytes(key).map(|_| ()),
        SchemeId::BlsPop => bls::VerifyingKey::from_bytes(key).map(|_| ()),
    }
}

#[test]
fn verification_verdicts() {
    let v: SigningVectors = load("signing_v1.json");
    let failures: Vec<String> = v
        .verification
        .iter()
        .filter_map(|c| {
            let Some(id) = SchemeId::from_name(&c.scheme) else {
                return Some(format!("unknown scheme {}", c.scheme));
            };
            let (key, message, signature) = (bytes(&c.verifying_key), bytes(&c.message), bytes(&c.signature));
            let verdict = id.verify_bytes(&key, &message, &signature);
            // the key is decoded first, then the signature
            let actual = match (key_decodes(id, &key), &verdict) {
                (Err(_), Err(_)) => VerificationResult::MalformedKey,
                (Err(_), Ok(_)) => return Some(format!("{} {}: a malformed key verified", c.scheme, c.name)),
                (Ok(()), Ok(true)) => VerificationResult::Valid,
                (Ok(()), Ok(false)) => VerificationResult::Invalid,
                (Ok(()), Err(_)) => VerificationResult::MalformedSignature,
            };
            (actual != c.result).then(|| format!("{} {}: expected {:?}, got {actual:?}", c.scheme, c.name, c.result))
        })
        .collect();
    assert!(failures.is_empty(), "{} failures:\n{}", failures.len(), failures.join("\n"));
    assert!(v.verification.len() >= 400, "{} verification cases", v.verification.len());
}

#[test]
fn every_committed_signature_verifies() {
    let v: SigningVectors = load("signing_v1.json");
    let mut count = 0;
    for scheme in &v.signing {
        let id = SchemeId::from_name(&scheme.scheme).unwrap();
        for key in &scheme.keys {
            for s in &key.signatures {
                let vk = bytes(&key.verifying_key);
                assert_eq!(
                    id.verify_bytes(&vk, &bytes(&s.message), &bytes(&s.signature)),
                    Ok(true),
                    "{} {}",
                    scheme.scheme,
                    s.message
                );
                count += 1;
            }
        }
    }
    let icarus = &v.ed25519_bip32;
    for s in &icarus.signatures {
        let vk = bytes(&icarus.address_verifying_key);
        assert_eq!(SchemeId::Ed25519.verify_bytes(&vk, &bytes(&s.message), &bytes(&s.signature)), Ok(true));
        count += 1;
    }
    assert_eq!(count, 7 * 3 * 3 + 3);
}

#[test]
fn proofs_of_possession_verify() {
    let v: SigningVectors = load("signing_v1.json");
    for p in &v.bls_pop {
        let vk = bls::VerifyingKey::from_bytes(&bytes(&p.verifying_key)).unwrap();
        let proof = bls::Signature::from_bytes(&bytes(&p.proof)).unwrap();
        assert!(bls::pop_verify(&vk, &proof));
        assert!(!bls::verify(&vk, vk.as_bytes(), &proof), "a PoP is not a message signature");
    }
    assert_eq!(v.bls_pop.len(), 3);
}
