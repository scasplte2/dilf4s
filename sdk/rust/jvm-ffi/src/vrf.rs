//! ECVRF verification (`vrf.md`): the three suites of `dilf4s_signing::vrf`, chosen by an integer.

use dilf4s_signing::VrfError;
use dilf4s_signing::vrf::{self, OUTPUT_LENGTH, PROOF_LENGTH, SuiteId, VERIFYING_KEY_LENGTH};

/// The suite for a code: `0` TAI, `1` ELL2, `2` draft-03.
pub const fn suite(code: i32) -> Option<SuiteId> {
    match code {
        0 => Some(SuiteId::Tai),
        1 => Some(SuiteId::Ell2),
        2 => Some(SuiteId::Draft03),
        _ => None,
    }
}

/// The outcome of a VRF call, as the integer code the C ABI returns: `Valid`, or the `VrfError` kind. Lengths are
/// checked in `verify_bytes`'s order, the proof first, then the key.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VrfVerdict {
    /// The proof verifies (and `beta` was written); for [`check_key`], the key decodes (`1`).
    Valid,
    /// The proof is not 80 bytes (`-10`): `Malformed`.
    ProofLength,
    /// The key is not 32 bytes (`-11`): `Malformed`.
    KeyLength,
    /// `InvalidPublicKey` (`-12`).
    InvalidPublicKey,
    /// `SmallOrderPublicKey` (`-13`).
    SmallOrderPublicKey,
    /// `InvalidGamma` (`-14`).
    InvalidGamma,
    /// `NonCanonicalScalar` (`-15`).
    NonCanonicalScalar,
    /// `ChallengeMismatch` (`-16`).
    ChallengeMismatch,
    /// `HashToCurveFailed` (`-17`).
    HashToCurveFailed,
    /// `KeyMismatch` (`-18`): a signing-key error, which verification never returns; here so every kind has a code.
    KeyMismatch,
    /// An unknown suite, a negative length, a null pointer with a positive length, or an output buffer that is not 64
    /// bytes (`-4`, as for Ed25519): a caller's bug, never an input's verdict.
    BadArgument,
}

impl VrfVerdict {
    /// The code the C ABI returns.
    pub const fn code(self) -> i32 {
        match self {
            Self::Valid => 1,
            Self::ProofLength => -10,
            Self::KeyLength => -11,
            Self::InvalidPublicKey => -12,
            Self::SmallOrderPublicKey => -13,
            Self::InvalidGamma => -14,
            Self::NonCanonicalScalar => -15,
            Self::ChallengeMismatch => -16,
            Self::HashToCurveFailed => -17,
            Self::KeyMismatch => -18,
            Self::BadArgument => -4,
        }
    }

    /// A rejection's verdict. `Malformed` names the proof if the proof has the wrong length, else the key, as
    /// `verify_bytes` checks them.
    fn of(e: &VrfError, proof_len: usize) -> Self {
        match e {
            VrfError::Malformed { .. } if proof_len != PROOF_LENGTH => Self::ProofLength,
            VrfError::Malformed { .. } => Self::KeyLength,
            VrfError::InvalidPublicKey => Self::InvalidPublicKey,
            VrfError::SmallOrderPublicKey => Self::SmallOrderPublicKey,
            VrfError::InvalidGamma => Self::InvalidGamma,
            VrfError::NonCanonicalScalar => Self::NonCanonicalScalar,
            VrfError::ChallengeMismatch => Self::ChallengeMismatch,
            VrfError::HashToCurveFailed { .. } => Self::HashToCurveFailed,
            VrfError::KeyMismatch => Self::KeyMismatch,
        }
    }
}

/// Whether `key` is a verifying key of `suite`: [`VrfVerdict::Valid`], or why it is not. The length is checked first,
/// so a refusal never formats an error message.
pub fn check_key(suite: SuiteId, key: &[u8]) -> VrfVerdict {
    fn decodes<S: vrf::Suite>(key: &[u8]) -> Result<(), VrfError> {
        vrf::VerifyingKey::<S>::from_bytes(key).map(|_| ())
    }
    if key.len() != VERIFYING_KEY_LENGTH {
        return VrfVerdict::KeyLength;
    }
    let decoded = match suite {
        SuiteId::Tai => decodes::<vrf::Tai>(key),
        SuiteId::Ell2 => decodes::<vrf::Ell2>(key),
        SuiteId::Draft03 => decodes::<vrf::Draft03>(key),
    };
    decoded.map_or_else(|e| VrfVerdict::of(&e, PROOF_LENGTH), |()| VrfVerdict::Valid)
}

/// Verifies `proof` on `alpha` under `key`: `beta`, or why the proof is rejected. The lengths are checked first, in
/// `verify_bytes`'s order, so a refusal of a length never formats an error message.
pub fn verify(suite: SuiteId, key: &[u8], alpha: &[u8], proof: &[u8]) -> Result<[u8; OUTPUT_LENGTH], VrfVerdict> {
    if proof.len() != PROOF_LENGTH {
        return Err(VrfVerdict::ProofLength);
    }
    if key.len() != VERIFYING_KEY_LENGTH {
        return Err(VrfVerdict::KeyLength);
    }
    suite.verify_bytes(key, alpha, proof).map_err(|e| VrfVerdict::of(&e, proof.len()))
}

#[cfg(test)]
pub(crate) mod tests {
    #![allow(
        clippy::unwrap_used,
        clippy::panic,
        clippy::std_instead_of_alloc,
        reason = "tests fail by panicking, and run with std"
    )]

    use dilf4s_core::codec::Model;
    use dilf4s_core::generated::vrf_vectors::VrfVectors;
    use dilf4s_core::{hex, json};
    use dilf4s_signing::vrf::SuiteId;

    use super::{VrfVerdict, check_key, suite, verify};
    use crate::tests::bytes;

    pub(crate) fn vectors() -> VrfVectors {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../shared/vectors/vrf_v1.json");
        let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
        VrfVectors::from_value(&json::parse(&text).unwrap()).unwrap()
    }

    /// The suite's code in the C ABI.
    pub(crate) fn code(id: SuiteId) -> i32 {
        (0..3).find(|c| suite(*c) == Some(id)).unwrap()
    }

    /// The vectors' rejection kind as a verdict; `Malformed` names whichever length is wrong, the proof first.
    pub(crate) fn expected(kind: &str, key: &[u8], proof: &[u8]) -> VrfVerdict {
        match kind {
            "valid" => VrfVerdict::Valid,
            "Malformed" if proof.len() != 80 => VrfVerdict::ProofLength,
            "Malformed" if key.len() != 32 => VrfVerdict::KeyLength,
            "InvalidPublicKey" => VrfVerdict::InvalidPublicKey,
            "SmallOrderPublicKey" => VrfVerdict::SmallOrderPublicKey,
            "InvalidGamma" => VrfVerdict::InvalidGamma,
            "NonCanonicalScalar" => VrfVerdict::NonCanonicalScalar,
            "ChallengeMismatch" => VrfVerdict::ChallengeMismatch,
            other => panic!("unexpected kind {other}"),
        }
    }

    #[test]
    fn every_committed_proof_verifies_to_its_output() {
        let v = vectors();
        let mut count = 0;
        for s in &v.suites {
            let id = SuiteId::from_name(&s.suite).unwrap();
            for key in &s.keys {
                let vk = bytes(&key.verifying_key);
                assert_eq!(check_key(id, &vk), VrfVerdict::Valid);
                for p in &key.proofs {
                    let beta = verify(id, &vk, &bytes(&p.alpha), &bytes(&p.pi));
                    assert_eq!(beta.map(|b| hex::encode(&b)), Ok(p.beta.clone()), "{}", s.suite);
                    count += 1;
                }
            }
        }
        assert_eq!(count, 27);
    }

    #[test]
    fn every_rejection_in_the_vectors() {
        let v = vectors();
        for r in &v.rejections {
            let id = SuiteId::from_name(&r.suite).unwrap();
            let (k, a, p) = (bytes(&r.verifying_key), bytes(&r.alpha), bytes(&r.pi));
            let got = verify(id, &k, &a, &p).map_or_else(|v| v, |_| VrfVerdict::Valid);
            assert_eq!(got, expected(&r.result, &k, &p), "{} {}", r.suite, r.name);
        }
        assert_eq!(v.rejections.len(), 33);
    }

    #[test]
    fn suite_codes() {
        assert_eq!(suite(0), Some(SuiteId::Tai));
        assert_eq!(suite(1), Some(SuiteId::Ell2));
        assert_eq!(suite(2), Some(SuiteId::Draft03));
        assert_eq!(suite(3), None);
        assert_eq!(suite(-1), None);
    }

    #[test]
    fn codes_are_distinct_from_each_other_and_from_ed25519s() {
        use crate::Verdict;
        let vrf = [
            VrfVerdict::Valid,
            VrfVerdict::ProofLength,
            VrfVerdict::KeyLength,
            VrfVerdict::InvalidPublicKey,
            VrfVerdict::SmallOrderPublicKey,
            VrfVerdict::InvalidGamma,
            VrfVerdict::NonCanonicalScalar,
            VrfVerdict::ChallengeMismatch,
            VrfVerdict::HashToCurveFailed,
            VrfVerdict::KeyMismatch,
            VrfVerdict::BadArgument,
        ];
        let codes: std::collections::BTreeSet<i32> = vrf.iter().map(|v| v.code()).collect();
        assert_eq!(codes.len(), vrf.len());
        let ed = [Verdict::Invalid, Verdict::KeyLength, Verdict::KeyNotAPoint, Verdict::SignatureLength];
        assert!(ed.iter().all(|v| !codes.contains(&v.code())), "only Valid and BadArgument are shared");
    }
}
