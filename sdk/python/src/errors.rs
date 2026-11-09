//! Each Rust error family as its Python dataclasses (`dilf4s.errors`, generated from the same schemas).
//!
//! A family's kind names the Python class (both come from the schema's kind literal), and each match below is
//! exhaustive, so a kind the Rust SDK gains fails to compile here until it is mapped. Field names are the Rust fields'
//! own identifiers (`stringify!`), which the Rust emitter and `tools/gen_errors.py` both derive from the schema's
//! camelCase names.

use dilf4s_core::codec::Model;
use dilf4s_core::generated::codec_errors::{CanonicalError, NumberBound, StateKeyError, UnsupportedItem};
use dilf4s_core::generated::crypto_errors::{
    CryptoError, Groth16Error, KesError, KeyError, SigmaError, VrfError, ZkError,
};
use dilf4s_core::generated::hash_errors::HashError;
use dilf4s_core::generated::jlvm_errors::{JlvmCryptoCause, JlvmError};
use dilf4s_core::generated::proofs_errors::{
    MerkleError, PoseidonError, PoseidonMerkleError, TrieError, VersionedTrieError,
};
use dilf4s_core::generated::store_errors::StoreError;
use dilf4s_core::{NumberError, Preview};
use pyo3::IntoPyObjectExt;
use pyo3::prelude::*;

use crate::boundary::{Fail, Module, instance, py_int, py_none, py_str};

/// A Rust value as the Python one an error field holds.
pub(crate) trait Field {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>>;
}

impl Field for String {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        Ok(py_str(py, self))
    }
}

macro_rules! int_field {
    ($($t:ty),*) => {$(
        impl Field for $t {
            fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
                self.into_bound_py_any(py)
            }
        }
    )*};
}

int_field!(u64, i64, u32, i32);

impl Field for i128 {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_int(py, &dilf4s_core::BigInt::from(*self))
    }
}

impl Field for Preview {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        preview(py, self)
    }
}

impl<T: Field> Field for Option<T> {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        self.as_ref().map_or_else(|| Ok(py_none(py)), |v| v.field(py))
    }
}

/// A schema's choice of texts (a reason, a refusal, a site): its wire text.
macro_rules! text_field {
    ($($t:ty),*) => {$(
        impl Field for $t {
            fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
                let v = self.to_value().map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
                Ok(py_str(py, v.as_str().unwrap_or_default()))
            }
        }
    )*};
}

text_field!(
    dilf4s_core::generated::codec_errors::JsonSyntaxReason,
    dilf4s_core::generated::codec_errors::NonCanonicalReason,
    dilf4s_core::generated::codec_errors::NotJson,
    dilf4s_core::generated::crypto_errors::PathRefusal,
    dilf4s_core::generated::hash_errors::ExpansionRefusal,
    dilf4s_core::generated::proofs_errors::VtRootComponent,
    dilf4s_core::generated::jlvm_errors::JlvmSite
);

/// The SDK's `Preview`.
pub(crate) fn preview<'py>(py: Python<'py>, p: &Preview) -> PyResult<Bound<'py, PyAny>> {
    instance(
        py,
        Module::Preview,
        "Preview",
        &[
            ("head", py_str(py, p.head())),
            ("length", p.length().into_bound_py_any(py)?),
            ("truncated", pyo3::types::PyBool::new(py, p.is_truncated()).to_owned().into_any()),
        ],
    )
}

/// A Rust error as its Python dataclass.
pub(crate) trait Refusal {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>>;

    /// The refusal of a call.
    fn refuse<'py>(&self, py: Python<'py>) -> Fail<'py> {
        match self.py(py) {
            Ok(e) => Fail::Refused(e),
            Err(e) => Fail::Internal(e),
        }
    }
}

impl<T: Refusal> Field for T {
    fn field<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        self.py(py)
    }
}

/// The kind `$kind` of family module `$module`, with these fields (bound by the match arm, named as in Rust).
macro_rules! kind {
    ($py:expr, $module:expr, $kind:expr $(, $field:ident)*) => {
        instance($py, $module, $kind, &[$((stringify!($field), Field::field($field, $py)?)),*])
    };
}

impl Refusal for CanonicalError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Canonical;
        match self {
            Self::UnpairedSurrogate => kind!(py, m, &k),
            Self::DuplicateKey { key } => kind!(py, m, &k, key),
            Self::RationalTooWide { limit } | Self::IntegerTooWide { limit } | Self::DepthExceeded { limit } => {
                kind!(py, m, &k, limit)
            }
            Self::JsonSyntax { offset, reason } => kind!(py, m, &k, offset, reason),
            Self::InvalidNumber { length, offset, bound } => kind!(py, m, &k, length, offset, bound),
            Self::NotJsonRepresentable { what } => kind!(py, m, &k, what),
            Self::TypeMismatch { reason } => kind!(py, m, &k, reason),
            Self::UnexpectedEnd { offset } | Self::TrailingBytes { offset } | Self::InvalidUtf8 { offset } => {
                kind!(py, m, &k, offset)
            }
            Self::NonCanonical { offset, reason } => kind!(py, m, &k, offset, reason),
            Self::Unsupported { offset, item } => kind!(py, m, &k, offset, item),
        }
    }
}

impl Refusal for NumberBound {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::NumberBound;
        match self {
            Self::Digits { digits, limit } => kind!(py, m, &k, digits, limit),
            Self::Scale { scale, limit } => kind!(py, m, &k, scale, limit),
        }
    }
}

impl Refusal for UnsupportedItem {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::UnsupportedItem;
        match self {
            Self::FloatOrSimple { info } | Self::ReservedInfo { info } => kind!(py, m, &k, info),
            Self::SimpleValue { value } => kind!(py, m, &k, value),
            Self::IndefiniteLength | Self::NonTextKey => kind!(py, m, &k),
            Self::Tag { tag } => kind!(py, m, &k, tag),
        }
    }
}

impl Refusal for StateKeyError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::StateKey;
        match self {
            Self::Empty | Self::NotUtf8 => kind!(py, m, &k),
            Self::TooLong { limit } => kind!(py, m, &k, limit),
            Self::TooManySegments { key, limit } => kind!(py, m, &k, key, limit),
            Self::InvalidSegment { key, segment } => kind!(py, m, &k, key, segment),
            Self::SegmentTooLong { key, segment, limit } => kind!(py, m, &k, key, segment, limit),
            Self::NamespaceFull { namespace } => kind!(py, m, &k, namespace),
        }
    }
}

impl Refusal for HashError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Hash;
        match self {
            Self::InvalidLength { algorithm, expected, actual } => kind!(py, m, &k, algorithm, expected, actual),
            Self::InvalidHex { offset } => kind!(py, m, &k, offset),
            Self::InvalidDomainTag { tag } => kind!(py, m, &k, tag),
            Self::InvalidExpansion { refusal, algorithm, limit } => kind!(py, m, &k, refusal, algorithm, limit),
            Self::UnknownAlgorithm { name } => kind!(py, m, &k, name),
        }
    }
}

impl Refusal for CryptoError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Crypto;
        match self {
            Self::InvalidEncoding { what, reason } => kind!(py, m, &k, what, reason),
            Self::InvalidKey { reason } => kind!(py, m, &k, reason),
            Self::BackendFailure { operation, reason } => kind!(py, m, &k, operation, reason),
        }
    }
}

impl Refusal for KeyError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Key;
        match self {
            Self::InvalidEntropy { reason } | Self::InvalidMnemonic { reason } | Self::InvalidDerivation { reason } => {
                kind!(py, m, &k, reason)
            }
            Self::InvalidWordList { reason, separator } => kind!(py, m, &k, reason, separator),
            Self::InvalidPath { refusal, reason } => kind!(py, m, &k, refusal, reason),
        }
    }
}

impl Refusal for VrfError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Vrf;
        match self {
            Self::Malformed { what, reason } => kind!(py, m, &k, what, reason),
            Self::InvalidPublicKey
            | Self::SmallOrderPublicKey
            | Self::InvalidGamma
            | Self::NonCanonicalScalar
            | Self::ChallengeMismatch
            | Self::KeyMismatch => kind!(py, m, &k),
            Self::HashToCurveFailed { reason } => kind!(py, m, &k, reason),
        }
    }
}

impl Refusal for KesError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Kes;
        match self {
            Self::Malformed { what, reason } => kind!(py, m, &k, what, reason),
            Self::PeriodOutOfRange { period, total } => kind!(py, m, &k, period, total),
            Self::Exhausted { period } => kind!(py, m, &k, period),
            Self::InconsistentKey { reason } => kind!(py, m, &k, reason),
        }
    }
}

impl Refusal for SigmaError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Sigma;
        match self {
            Self::InvalidProposition { reason } => kind!(py, m, &k, reason),
            Self::InvalidPoint { what, reason } => kind!(py, m, &k, what, reason),
            Self::ProofLength { expected, actual } => kind!(py, m, &k, expected, actual),
            Self::ResponseOutOfRange => kind!(py, m, &k),
        }
    }
}

impl Refusal for ZkError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        match self {
            Self::Malformed { reason } | Self::InvalidPoint { reason } => {
                kind!(py, Module::Zk, &k, reason)
            }
        }
    }
}

impl Refusal for Groth16Error {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Groth16;
        match self {
            Self::Malformed { reason } | Self::InvalidProof { reason } | Self::Rejected { reason } => {
                kind!(py, m, &k, reason)
            }
            Self::PairingCheckFailed => kind!(py, m, &k),
        }
    }
}

impl Refusal for MerkleError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Merkle;
        match self {
            Self::IndexOutOfRange { index, size } => kind!(py, m, &k, index, size),
            Self::InvalidSizes { old_size, new_size } => kind!(py, m, &k, old_size, new_size),
            Self::InvalidProof { reason } | Self::Encoding { reason } => kind!(py, m, &k, reason),
            Self::RootMismatch { expected, computed } => kind!(py, m, &k, expected, computed),
            Self::TooLarge { limit } => kind!(py, m, &k, limit),
        }
    }
}

impl Refusal for TrieError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Trie;
        match self {
            Self::InvalidKey { reason }
            | Self::InvalidNode { reason }
            | Self::InvalidProof { reason }
            | Self::Encoding { reason } => kind!(py, m, &k, reason),
            Self::MissingNode { hash } => kind!(py, m, &k, hash),
            Self::RootMismatch { expected, computed } => kind!(py, m, &k, expected, computed),
            Self::NotCovered { what } => kind!(py, m, &k, what),
            Self::TooLarge { limit } => kind!(py, m, &k, limit),
        }
    }
}

impl Refusal for PoseidonError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Poseidon;
        match self {
            Self::InputCount { count, max } => kind!(py, m, &k, count, max),
            Self::NonCanonical { bits } => kind!(py, m, &k, bits),
        }
    }
}

impl Refusal for PoseidonMerkleError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::PoseidonMerkle;
        match self {
            Self::Position { position_bits, depth } => kind!(py, m, &k, position_bits, depth),
            Self::Depth { depth } => kind!(py, m, &k, depth),
            Self::OccupiedPosition { position_bits } => kind!(py, m, &k, position_bits),
            Self::Encoding { reason } => kind!(py, m, &k, reason),
        }
    }
}

impl Refusal for VersionedTrieError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::VersionedTrie;
        match self {
            Self::Key { error } => kind!(py, m, &k, error),
            Self::Store { error } => kind!(py, m, &k, error),
            Self::Trie { error } => kind!(py, m, &k, error),
            Self::InvalidVersion { reason } | Self::Malformed { reason } => {
                kind!(py, m, &k, reason)
            }
            Self::Empty => kind!(py, m, &k),
            Self::VersionNotIncreasing { head, next } => kind!(py, m, &k, head, next),
            Self::VersionConflict { expected, actual } => kind!(py, m, &k, expected, actual),
            Self::CommitRewrite { version } | Self::NotRetained { version } | Self::UnknownVersion { version } => {
                kind!(py, m, &k, version)
            }
            Self::RootMismatch { component, expected, computed } => kind!(py, m, &k, component, expected, computed),
            Self::NotCovered { what } => kind!(py, m, &k, what),
            Self::TooLarge { what, limit } => kind!(py, m, &k, what, limit),
            Self::TooDeep { what, depth, limit } => kind!(py, m, &k, what, depth, limit),
        }
    }
}

impl Refusal for StoreError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Store;
        match self {
            Self::Encoding { context, reason } => kind!(py, m, &k, context, reason),
            Self::NotFound { key_bytes, prefix } => kind!(py, m, &k, key_bytes, prefix),
            Self::VersionConflict { expected, actual } => kind!(py, m, &k, expected, actual),
            Self::UnknownVersion { requested, latest } => kind!(py, m, &k, requested, latest),
            Self::Pruned { requested, floor } => kind!(py, m, &k, requested, floor),
            Self::Backend { operation, reason } => kind!(py, m, &k, operation, reason),
            Self::Corrupt { reason } => kind!(py, m, &k, reason),
        }
    }
}

impl Refusal for JlvmError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::Jlvm;
        match self {
            Self::Malformed { message } | Self::Limit { message } => kind!(py, m, &k, message),
            Self::Type { op, message }
            | Self::Arity { op, message }
            | Self::Arithmetic { op, message }
            | Self::Domain { op, message } => kind!(py, m, &k, op, message),
            Self::Crypto { op, message, cause } => kind!(py, m, &k, op, message, cause),
            Self::OutOfGas { required, available } => kind!(py, m, &k, required, available),
        }
    }
}

impl Refusal for JlvmCryptoCause {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let k = self.kind().to_string();
        let m = Module::JlvmCryptoCause;
        match self {
            Self::Signing { error } => kind!(py, m, &k, error),
            Self::Sigma { error } => kind!(py, m, &k, error),
            Self::Zk { error } => kind!(py, m, &k, error),
            Self::Groth16 { error } => kind!(py, m, &k, error),
            Self::Vrf { error } => kind!(py, m, &k, error),
            Self::Hash { error } => kind!(py, m, &k, error),
            Self::Merkle { error } => kind!(py, m, &k, error),
            Self::Trie { error } => kind!(py, m, &k, error),
            Self::Poseidon { error } => kind!(py, m, &k, error),
            Self::PoseidonMerkle { error } => kind!(py, m, &k, error),
        }
    }
}

/// `Value.number`'s refusals: the canonical family's two widths, or the SDK's `DivisionByZero`.
impl Refusal for NumberError {
    fn py<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        match self {
            Self::DivisionByZero => instance(py, Module::Refusals, "DivisionByZero", &[]),
            Self::RationalTooWide => CanonicalError::RationalTooWide { limit: dilf4s_core::MAX_RATIONAL_BITS }.py(py),
            Self::IntegerTooWide => CanonicalError::IntegerTooWide { limit: dilf4s_core::MAX_INTEGER_BITS }.py(py),
        }
    }
}

/// The SDK's `TooLarge`: an encoding, a rendering or a weight over a limit, carrying the limit alone.
pub(crate) fn too_large(py: Python<'_>, limit: u64) -> Fail<'_> {
    match limit.into_bound_py_any(py).and_then(|l| instance(py, Module::Refusals, "TooLarge", &[("limit", l)])) {
        Ok(e) => Fail::Refused(e),
        Err(e) => Fail::Internal(e),
    }
}
