//! Typed errors, generated from `shared/schemas/crypto-errors.cddl` (`dilf4s_core::generated::crypto_errors`): each
//! family is an enum whose variants are the Scala reference's cases, its `kind()` the family's kind enum, which the
//! conformance vectors use. A kind added to the schema is a variant here.

use alloc::string::String;
use core::fmt;

pub use dilf4s_core::generated::crypto_errors::{
    CryptoError, CryptoErrorKind, KesError, KesErrorKind, KeyError, KeyErrorKind, PathRefusal, SigmaError,
    SigmaErrorKind, VrfError, VrfErrorKind,
};

/// A derivation path [`crate::keys::path::parse`] refuses: the narrow error of a parse, which converts into
/// [`KeyError::InvalidPath`] so `?` works in a function returning [`KeyError`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PathError {
    /// The first rule the path breaks.
    pub refusal: PathRefusal,
    /// Why it was refused, naming the segment by position (a message, not normative).
    pub reason: String,
}

impl PathError {
    /// The kind, as [`KeyError::kind`] gives it.
    pub const fn kind(&self) -> KeyErrorKind {
        KeyErrorKind::InvalidPath
    }
}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // as `KeyError::InvalidPath` renders it
        write!(f, "InvalidPath (refusal: {:?}, reason: {})", self.refusal, self.reason)
    }
}

impl core::error::Error for PathError {}

impl From<PathError> for KeyError {
    fn from(e: PathError) -> Self {
        Self::InvalidPath { refusal: e.refusal, reason: e.reason }
    }
}
