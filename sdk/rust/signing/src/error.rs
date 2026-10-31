//! Typed errors. Their kinds carry the names of the Scala reference's cases (`CryptoError`, `KeyError`, `VrfError`,
//! `KesError`), which the conformance vectors use.

use alloc::string::String;
use core::fmt;

/// A key or signature that is not a valid encoding for its scheme (Scala's `CryptoError`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SigningError {
    /// A wrong length, or a value outside its range (a high-S ECDSA signature, a non-canonical scalar, ...).
    InvalidEncoding {
        /// What was being decoded.
        what: &'static str,
        /// Why it was refused.
        reason: String,
    },
    /// A key that does not decode, or is not a valid key (off the curve, the identity, a scalar outside `[1, n)`).
    InvalidKey {
        /// Why it was refused.
        reason: String,
    },
}

impl SigningError {
    /// The kind's name, as the reference and the vectors spell it.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::InvalidEncoding { .. } => "InvalidEncoding",
            Self::InvalidKey { .. } => "InvalidKey",
        }
    }
}

impl fmt::Display for SigningError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidEncoding { what, reason } => write!(f, "invalid {what}: {reason}"),
            Self::InvalidKey { reason } => write!(f, "invalid key: {reason}"),
        }
    }
}

impl core::error::Error for SigningError {}

/// A key-derivation failure (BIP-39, SLIP-10, BIP32, BIP32-Ed25519, paths).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeyError {
    /// Entropy of a length BIP-39 does not allow.
    InvalidEntropy {
        /// Why it was refused.
        reason: String,
    },
    /// A phrase with a wrong word count, an unknown word or a bad checksum.
    InvalidMnemonic {
        /// Why it was refused.
        reason: String,
    },
    /// A word list that is not 2048 distinct words.
    InvalidWordList {
        /// Why it was refused.
        reason: String,
    },
    /// A malformed path or index, an invalid derived key, or an operation the scheme does not allow.
    InvalidDerivation {
        /// Why it was refused.
        reason: String,
    },
}

impl KeyError {
    /// The kind's name.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::InvalidEntropy { .. } => "InvalidEntropy",
            Self::InvalidMnemonic { .. } => "InvalidMnemonic",
            Self::InvalidWordList { .. } => "InvalidWordList",
            Self::InvalidDerivation { .. } => "InvalidDerivation",
        }
    }
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidEntropy { reason }
            | Self::InvalidMnemonic { reason }
            | Self::InvalidWordList { reason }
            | Self::InvalidDerivation { reason } => write!(f, "{}: {reason}", self.kind()),
        }
    }
}

impl core::error::Error for KeyError {}

/// A VRF failure. [`VrfError::Malformed`] is the malformed case (a typed error to a JLVM program); every other kind is
/// a rejection of a well-formed input (`{valid: false}`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VrfError {
    /// A key or proof of the wrong length.
    Malformed {
        /// What was being decoded.
        what: &'static str,
        /// Why it was refused.
        reason: String,
    },
    /// The public key does not decode.
    InvalidPublicKey,
    /// The public key is of small order.
    SmallOrderPublicKey,
    /// `Gamma` does not decode.
    InvalidGamma,
    /// `s >= L`.
    NonCanonicalScalar,
    /// The recomputed challenge differs from `c`.
    ChallengeMismatch,
    /// A draft-03 secret key whose stored public key is not its seed's.
    KeyMismatch,
    /// Hash-to-curve found no point (TAI: all 256 counters failed, probability about 2^-256).
    HashToCurveFailed {
        /// Why.
        reason: String,
    },
}

impl VrfError {
    /// The kind's name, as the reference and `vrf_v1.json` spell it.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Malformed { .. } => "Malformed",
            Self::InvalidPublicKey => "InvalidPublicKey",
            Self::SmallOrderPublicKey => "SmallOrderPublicKey",
            Self::InvalidGamma => "InvalidGamma",
            Self::NonCanonicalScalar => "NonCanonicalScalar",
            Self::ChallengeMismatch => "ChallengeMismatch",
            Self::KeyMismatch => "KeyMismatch",
            Self::HashToCurveFailed { .. } => "HashToCurveFailed",
        }
    }

    /// Whether this is the malformed case rather than a rejection.
    pub const fn is_malformed(&self) -> bool {
        matches!(self, Self::Malformed { .. })
    }
}

impl fmt::Display for VrfError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Malformed { what, reason } => write!(f, "malformed {what}: {reason}"),
            Self::HashToCurveFailed { reason } => write!(f, "hash to curve failed: {reason}"),
            other => f.write_str(other.kind()),
        }
    }
}

impl core::error::Error for VrfError {}

/// A KES failure. Verification returns `bool`; only malformed sizes and out-of-range periods are errors there.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KesError {
    /// A key, signature, seed or parameter of the wrong size.
    Malformed {
        /// What was being decoded.
        what: &'static str,
        /// Why it was refused.
        reason: String,
    },
    /// A period outside `[0, total)`.
    PeriodOutOfRange {
        /// The period asked for.
        period: u64,
        /// The number of periods.
        total: u64,
    },
    /// The key cannot evolve further: its last period has been reached.
    Exhausted {
        /// The key's period.
        period: u64,
    },
    /// A key whose parts disagree (a seed and its verifying key, a stored root, an erased leaf).
    InconsistentKey {
        /// Why.
        reason: String,
    },
}

impl KesError {
    /// The kind's name.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Malformed { .. } => "Malformed",
            Self::PeriodOutOfRange { .. } => "PeriodOutOfRange",
            Self::Exhausted { .. } => "Exhausted",
            Self::InconsistentKey { .. } => "InconsistentKey",
        }
    }
}

impl fmt::Display for KesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Malformed { what, reason } => write!(f, "malformed {what}: {reason}"),
            Self::PeriodOutOfRange { period, total } => write!(f, "period {period} outside [0, {total})"),
            Self::Exhausted { period } => write!(f, "the key is exhausted at period {period}"),
            Self::InconsistentKey { reason } => write!(f, "inconsistent key: {reason}"),
        }
    }
}

impl core::error::Error for KesError {}

/// Malformed sigma input (`docs/specs/sigma.md` section 6). A well-formed proof that fails is `Ok(false)`, never one
/// of these. The Scala reference reports malformed input as a message; the kinds are this SDK's.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SigmaError {
    /// A proposition breaking section 3: an unknown type, a missing or extra field, an identity statement point, a
    /// connective with no children or more than 255, `k` outside `1..=n`, more than 64 levels or 4096 leaves, or a
    /// canonical form heavier than the heaviest valid proposition.
    InvalidProposition {
        /// Why it was refused.
        reason: String,
    },
    /// A group element that is not the canonical encoding of a point of secp256k1 (section 1).
    InvalidPoint {
        /// The field being decoded (`pk`, `g`, `h`, `u`, `v`, or `key`).
        what: &'static str,
        /// Why it was refused.
        reason: String,
    },
    /// A proof whose length is not the one its proposition fixes (section 5).
    ProofLength {
        /// The length the proposition fixes.
        expected: usize,
        /// The proof's length.
        actual: usize,
    },
    /// A response `z >= q`.
    ResponseOutOfRange,
}

impl SigmaError {
    /// The kind's name.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::InvalidProposition { .. } => "InvalidProposition",
            Self::InvalidPoint { .. } => "InvalidPoint",
            Self::ProofLength { .. } => "ProofLength",
            Self::ResponseOutOfRange => "ResponseOutOfRange",
        }
    }
}

impl fmt::Display for SigmaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidProposition { reason } => write!(f, "invalid proposition: {reason}"),
            Self::InvalidPoint { what, reason } => write!(f, "invalid point {what}: {reason}"),
            Self::ProofLength { expected, actual } => {
                write!(f, "a proof of this proposition is {expected} bytes, got {actual}")
            }
            Self::ResponseOutOfRange => f.write_str("a response must be below the group order"),
        }
    }
}

impl core::error::Error for SigmaError {}
