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
use dilf4s_core::generated::signing_vectors::{
    KeyDecodingResult, KeyPart, PathResult, SigningVectors, VerificationResult,
};
use dilf4s_signing::keys::path;
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

/// The kind a `keyDecoding` row records, as `SigningError::kind` names it.
pub fn expected_kind(r: &KeyDecodingResult) -> &'static str {
    match r {
        KeyDecodingResult::Accepted => "accepted",
        KeyDecodingResult::InvalidEncoding => "InvalidEncoding",
        KeyDecodingResult::InvalidKey => "InvalidKey",
    }
}

/// Every `keyDecoding` row for a verifying key: a wrong length is `InvalidEncoding`, and well-sized bytes that are no
/// key (an ECDSA key not tagged 02 or 03 among them) are `InvalidKey`.
#[test]
fn verifying_key_decoding() {
    let v: SigningVectors = load("signing_v1.json");
    let rows: Vec<_> = v.key_decoding.iter().filter(|c| c.part == KeyPart::VerifyingKey).collect();
    let failures: Vec<String> = rows
        .iter()
        .filter_map(|c| {
            let Some(id) = SchemeId::from_name(&c.scheme) else {
                return Some(format!("unknown scheme {}", c.scheme));
            };
            let actual = key_decodes(id, &bytes(&c.bytes)).map_or_else(|e| e.kind(), |()| "accepted");
            let expected = expected_kind(&c.result);
            (actual != expected).then(|| format!("{} {}: expected {expected}, got {actual}", c.scheme, c.name))
        })
        .collect();
    assert!(failures.is_empty(), "{} failures:\n{}", failures.len(), failures.join("\n"));
    assert!(rows.len() >= 40, "{} verifying-key rows", rows.len());
}

/// Every `paths` row: nothing trimmed, ASCII digits only, no empty segment; accepted paths give these indices.
#[test]
fn path_strictness() {
    let v: SigningVectors = load("signing_v1.json");
    assert!(v.paths.len() >= 30, "{} path rows", v.paths.len());
    for c in &v.paths {
        let actual =
            path::parse(&c.path).ok().map(|is| is.into_iter().map(|i| u64::from(i.serialized())).collect::<Vec<_>>());
        let expected = match &c.result {
            PathResult::PathIndices(p) => Some(p.indices.clone()),
            PathResult::Invalid => None,
        };
        assert_eq!(actual, expected, "path {:?}", c.path);
    }
}
