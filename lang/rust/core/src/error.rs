//! Typed errors of the canonical data model, its CBOR and JSON forms, and typed decoding. `CanonicalError` and its
//! reasons are generated from `shared/schemas/codec-errors.cddl`.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use crate::preview::Preview;

pub use crate::generated::codec_errors::{
    CanonicalError, CanonicalErrorKind, JsonSyntaxReason, NonCanonicalReason, NotJson, NumberBound, NumberBoundKind,
    UnsupportedItem, UnsupportedItemKind,
};

/// A size or offset as an error holds it: `usize` is at most 64 bits on every supported target.
pub(crate) fn size(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// Why a canonical value is not the expected type, and where: `path` runs from the root (map keys and `[i]`). A key
/// may be the caller's, so each segment is a [`Preview`]; `reason` quotes caller input only through one
/// (docs/specs/conformance.md section 8).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodeFailure {
    /// Map keys and `[i]` array indices from the root to the failing value.
    pub path: Vec<Preview>,
    /// What is wrong there.
    pub reason: String,
}

impl DecodeFailure {
    /// A failure at the value itself.
    pub fn new(reason: impl Into<String>) -> Self {
        Self { path: Vec::new(), reason: reason.into() }
    }

    /// This failure, one level further down: `segment` is prepended to the path.
    #[must_use]
    pub fn at(mut self, segment: impl AsRef<str>) -> Self {
        self.path.insert(0, Preview::of(segment.as_ref()));
        self
    }
}

impl fmt::Display for DecodeFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            f.write_str("<root>")?;
        }
        self.path.iter().enumerate().try_for_each(|(i, p)| {
            if i > 0 {
                f.write_str(".")?;
            }
            write!(f, "{}", p.short())
        })?;
        write!(f, ": {}", self.reason)
    }
}

impl core::error::Error for DecodeFailure {}
