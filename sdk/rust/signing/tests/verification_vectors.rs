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
    CryptoErrorKind, KeyDecodingResult, KeyPart, PathIndices, PathResult, SigningVectors, VerificationPart,
    VerificationRefusal, VerificationResult,
};
use dilf4s_signing::keys::path;
use dilf4s_signing::{PathRefusal, SchemeId, SigningError, bls, ecdsa, ed25519, ristretto255, schnorr};

/// The schema's kind of a signing error. Exhaustive both ways: a kind the SDK adds, or one the schema adds, does not
/// compile until it is mapped (and then the coverage test asks for its row).
pub const fn crypto_kind(e: &SigningError) -> CryptoErrorKind {
    match e {
        SigningError::InvalidEncoding { .. } => CryptoErrorKind::InvalidEncoding,
        SigningError::InvalidKey { .. } => CryptoErrorKind::InvalidKey,
    }
}

/// The SDK's spelling of a schema kind, the other direction of [`crypto_kind`].
pub const fn crypto_kind_name(k: &CryptoErrorKind) -> &'static str {
    match k {
        CryptoErrorKind::InvalidEncoding => "InvalidEncoding",
        CryptoErrorKind::InvalidKey => "InvalidKey",
    }
}

fn refusal(part: VerificationPart, e: &SigningError) -> VerificationResult {
    VerificationResult::VerificationRefusal(VerificationRefusal { part, kind: crypto_kind(e) })
}

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
            // the key is decoded first, then the signature: the refusal is the key's exactly when the key alone fails
            let actual = match (key_decodes(id, &key), &verdict) {
                (Err(_), Ok(_)) => return Some(format!("{} {}: a malformed key verified", c.scheme, c.name)),
                (Err(_), Err(e)) => refusal(VerificationPart::VerifyingKey, e),
                (Ok(()), Err(e)) => refusal(VerificationPart::Signature, e),
                (Ok(()), Ok(true)) => VerificationResult::Valid,
                (Ok(()), Ok(false)) => VerificationResult::Invalid,
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

/// The coverage gate (conformance.md section 6) for `SigningError`: every kind of the schema has a `keyDecoding` row
/// and a `verification` row. [`crypto_kind`] maps every SDK variant onto a schema kind (exhaustively, so a new variant
/// does not compile until it is mapped), so every SDK kind has its rows too.
#[test]
fn every_signing_error_kind_has_a_row() {
    let v: SigningVectors = load("signing_v1.json");
    for kind in CryptoErrorKind::ALL {
        let name = crypto_kind_name(&kind);
        assert!(v.key_decoding.iter().any(|c| expected_kind(&c.result) == name), "no keyDecoding row pins {name}");
        assert!(
            v.verification
                .iter()
                .any(|c| matches!(&c.result, VerificationResult::VerificationRefusal(r) if r.kind == kind)),
            "no verification row pins {name}"
        );
    }
    for e in [
        SigningError::InvalidEncoding { what: "", reason: String::new() },
        SigningError::InvalidKey { reason: String::new() },
    ] {
        assert_eq!(crypto_kind_name(&crypto_kind(&e)), e.kind(), "the schema and the SDK spell {} alike", e.kind());
    }
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

/// The schema's spelling of a path refusal. Exhaustive: a refusal the SDK adds does not compile until it is mapped.
const fn path_result(r: PathRefusal) -> PathResult {
    match r {
        PathRefusal::MissingMaster => PathResult::MissingMaster,
        PathRefusal::Malformed => PathResult::Malformed,
        PathRefusal::LeadingZero => PathResult::LeadingZero,
        PathRefusal::TooManyDigits => PathResult::TooManyDigits,
        PathRefusal::OutOfRange => PathResult::OutOfRange,
        PathRefusal::TooDeep => PathResult::TooDeep,
    }
}

/// Every `paths` row: nothing trimmed, ASCII digits only, no empty segment or leading zero, at most ten digits and 255
/// segments; accepted paths give these indices, and a refused one is `InvalidPath` with the row's refusal.
#[test]
fn path_strictness() {
    let v: SigningVectors = load("signing_v1.json");
    assert!(v.paths.len() >= 54, "{} path rows", v.paths.len());
    for c in &v.paths {
        let actual = path::parse(&c.path).map_or_else(
            |e| path_result(e.refusal),
            |is| {
                PathResult::PathIndices(PathIndices {
                    indices: is.into_iter().map(|i| u64::from(i.serialized())).collect(),
                })
            },
        );
        assert_eq!(actual, c.result, "path {:?}", c.path.get(..40).unwrap_or(&c.path));
    }
}

/// The coverage gate (conformance.md section 6) for `PathRefusal`: every refusal the schema lists (`TEXTS`, beside
/// the indices) has a row. [`path_result`] maps every SDK refusal onto one of them, so every SDK refusal has a row too.
#[test]
fn every_path_refusal_has_a_row() {
    let v: SigningVectors = load("signing_v1.json");
    for refusal in PathResult::TEXTS {
        assert!(v.paths.iter().any(|c| c.result == refusal), "no paths row pins {refusal:?}");
    }
}
