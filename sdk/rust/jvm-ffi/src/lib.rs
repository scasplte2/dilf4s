//! The JVM's native Ed25519 verification: a C ABI over `dilf4s-signing`, so a JVM node runs the code the Rust SDK and
//! the SP1 guest run, at the cost `ed25519_verify` is priced from. The Scala `crypto` module loads it through JNA
//! (`Ed25519Native`).
//!
//! Each function returns a [`Verdict`] code. Nothing here can panic: the verdicts are computed by `dilf4s-signing`'s
//! verifiers, which reject every input with a value rather than a panic, and the crate's own code is held to the
//! workspace's lints (no indexing, unwrapping, arithmetic overflow or `as`). The C ABI and its `unsafe` are in [`ffi`],
//! and only there.

use dilf4s_signing::ed25519::{self, SIGNATURE_LENGTH, Signature, VERIFYING_KEY_LENGTH, VerifyingKey};

#[allow(unsafe_code, reason = "the C ABI: exported symbols and caller-provided pointers")]
pub mod ffi;

/// The ABI this library implements; the loader refuses any other.
pub const ABI_VERSION: i32 = 1;

/// The outcome of a call, as the integer code the C ABI returns. The key is decoded before the signature, so a call
/// with both malformed reports the key, as the Scala reference and the conformance vectors do.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// A well-formed signature that does not verify (`0`).
    Invalid,
    /// The signature verifies; for [`check_key`], the key decodes (`1`).
    Valid,
    /// The key is not 32 bytes (`-1`): Scala's `InvalidEncoding`.
    KeyLength,
    /// The key does not decode to a curve point under ZIP-215 (`-2`): Scala's `InvalidKey`.
    KeyNotAPoint,
    /// The signature is not 64 bytes (`-3`): Scala's `InvalidEncoding`.
    SignatureLength,
    /// A negative length, or a null pointer with a positive length (`-4`): a caller's bug, never an input's verdict.
    BadArgument,
}

impl Verdict {
    /// The code the C ABI returns.
    pub const fn code(self) -> i32 {
        match self {
            Self::Invalid => 0,
            Self::Valid => 1,
            Self::KeyLength => -1,
            Self::KeyNotAPoint => -2,
            Self::SignatureLength => -3,
            Self::BadArgument => -4,
        }
    }
}

/// The verification rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rule {
    /// ZIP-215, the consensus rule (`ed25519::verify`).
    Zip215,
    /// libsodium's `crypto_sign_ed25519_verify_detached` (`ed25519::verify_libsodium`).
    Libsodium,
}

/// The key, decoded under ZIP-215. Lengths are checked first, so a refusal never formats an error message.
fn key(bytes: &[u8]) -> Result<VerifyingKey, Verdict> {
    if bytes.len() == VERIFYING_KEY_LENGTH {
        VerifyingKey::from_bytes(bytes).map_err(|_| Verdict::KeyNotAPoint)
    } else {
        Err(Verdict::KeyLength)
    }
}

/// Whether `bytes` is a verifying key: [`Verdict::Valid`], or why it is not.
pub fn check_key(bytes: &[u8]) -> Verdict {
    key(bytes).map_or_else(|v| v, |_| Verdict::Valid)
}

/// Verifies `signature` on `message` under `key` with `rule`, decoding the key first.
pub fn verify(rule: Rule, key_bytes: &[u8], message: &[u8], signature: &[u8]) -> Verdict {
    let key = match key(key_bytes) {
        Ok(k) => k,
        Err(v) => return v,
    };
    if signature.len() != SIGNATURE_LENGTH {
        return Verdict::SignatureLength;
    }
    let Ok(signature) = Signature::from_bytes(signature) else {
        return Verdict::SignatureLength;
    };
    let verified = match rule {
        Rule::Zip215 => ed25519::verify(&key, message, &signature),
        Rule::Libsodium => ed25519::verify_libsodium(&key, message, &signature),
    };
    if verified { Verdict::Valid } else { Verdict::Invalid }
}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::unwrap_used,
        clippy::panic,
        clippy::indexing_slicing,
        clippy::std_instead_of_alloc,
        clippy::std_instead_of_core,
        reason = "tests fail by panicking, and run with std"
    )]

    use dilf4s_core::codec::Model;
    use dilf4s_core::generated::signing_vectors::{SigningVectors, VerificationResult};
    use dilf4s_core::{hex, json};

    use super::{Rule, Verdict, check_key, verify};

    pub(crate) fn vectors() -> SigningVectors {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../shared/vectors/signing_v1.json");
        let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
        SigningVectors::from_value(&json::parse(&text).unwrap()).unwrap()
    }

    pub(crate) fn bytes(text: &str) -> Vec<u8> {
        hex::decode(text).unwrap()
    }

    /// The vectors' verdict as a [`Verdict`]; a malformed key is either kind, which the bytes tell apart.
    pub(crate) fn expected(result: &VerificationResult, key: &[u8]) -> Verdict {
        match result {
            VerificationResult::Valid => Verdict::Valid,
            VerificationResult::Invalid => Verdict::Invalid,
            VerificationResult::MalformedKey if key.len() == 32 => Verdict::KeyNotAPoint,
            VerificationResult::MalformedKey => Verdict::KeyLength,
            VerificationResult::MalformedSignature => Verdict::SignatureLength,
        }
    }

    pub(crate) fn rule(scheme: &str) -> Option<Rule> {
        match scheme {
            "Ed25519" => Some(Rule::Zip215),
            "Ed25519-libsodium" => Some(Rule::Libsodium),
            _ => None,
        }
    }

    #[test]
    fn every_ed25519_verdict_in_the_vectors() {
        let v = vectors();
        let mut count = 0;
        for c in &v.verification {
            let Some(rule) = rule(&c.scheme) else { continue };
            let (k, m, s) = (bytes(&c.verifying_key), bytes(&c.message), bytes(&c.signature));
            assert_eq!(verify(rule, &k, &m, &s), expected(&c.result, &k), "{} {}", c.scheme, c.name);
            count += 1;
        }
        assert_eq!(count, 406, "Ed25519 and Ed25519-libsodium verification rows");
    }

    #[test]
    fn every_committed_ed25519_signature_verifies_under_both_rules() {
        let v = vectors();
        let scheme = v.signing.iter().find(|s| s.scheme == "Ed25519").unwrap();
        for key in &scheme.keys {
            let vk = bytes(&key.verifying_key);
            for s in &key.signatures {
                for rule in [Rule::Zip215, Rule::Libsodium] {
                    assert_eq!(verify(rule, &vk, &bytes(&s.message), &bytes(&s.signature)), Verdict::Valid);
                }
            }
        }
    }

    #[test]
    fn the_key_is_decoded_before_the_signature() {
        let short_key = [0u8; 31];
        let not_a_point = bytes("0200000000000000000000000000000000000000000000000000000000000000");
        assert_eq!(check_key(&not_a_point), Verdict::KeyNotAPoint);
        for rule in [Rule::Zip215, Rule::Libsodium] {
            assert_eq!(verify(rule, &short_key, b"", &[0u8; 63]), Verdict::KeyLength);
            assert_eq!(verify(rule, &not_a_point, b"", &[0u8; 63]), Verdict::KeyNotAPoint);
        }
    }

    #[test]
    fn codes_are_distinct() {
        let all = [
            Verdict::Invalid,
            Verdict::Valid,
            Verdict::KeyLength,
            Verdict::KeyNotAPoint,
            Verdict::SignatureLength,
            Verdict::BadArgument,
        ];
        let codes: std::collections::BTreeSet<i32> = all.iter().map(|v| v.code()).collect();
        assert_eq!(codes.len(), all.len());
    }

    /// The `[lints]` table is the workspace's, but for `unsafe_code` and this crate's two unsafe-code lints.
    #[test]
    fn lints_follow_the_workspace() {
        fn table<'a>(text: &'a str, header: &str) -> Vec<&'a str> {
            text.lines()
                .skip_while(|l| l.trim() != header)
                .skip(1)
                .take_while(|l| !l.starts_with('['))
                .map(str::trim)
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .collect()
        }
        let workspace = include_str!("../../Cargo.toml");
        let own = include_str!("../Cargo.toml");
        let mut expected: Vec<String> = table(workspace, "[workspace.lints.rust]")
            .into_iter()
            .chain(table(workspace, "[workspace.lints.clippy]"))
            .map(|l| if l == r#"unsafe_code = "forbid""# { r#"unsafe_code = "deny""#.to_owned() } else { l.to_owned() })
            .collect();
        expected.push(r#"undocumented_unsafe_blocks = "deny""#.to_owned());
        expected.push(r#"multiple_unsafe_ops_per_block = "deny""#.to_owned());
        let actual: Vec<&str> = table(own, "[lints.rust]").into_iter().chain(table(own, "[lints.clippy]")).collect();
        assert_eq!(actual, expected);
    }
}
