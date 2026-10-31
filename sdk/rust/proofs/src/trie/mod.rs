//! The binary Patricia Merkle trie (docs/specs/trie.md): key modes, node hashing, proofs (partial tries) and their
//! verification, a root builder, and the JLVM-shaped verifiers.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use dilf4s_core::commitment::value_commitment;
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;
use num_traits::ToPrimitive;

use crate::bits::Bits;
use crate::tag::Tag;

mod build;
mod jlvm;
mod proof;

pub use build::{root_from_entries, root_from_sorted};
pub use jlvm::{trie_prefix_verify, trie_range_verify, trie_verify};
pub use proof::{Listing, TrieProof, VerifiedProof};

/// The value tag.
pub const VALUE_TAG: &str = "dilf4s/trie/value/v1";
/// The leaf tag.
pub const LEAF_TAG: &str = "dilf4s/trie/leaf/v1";
/// The branch tag.
pub const BRANCH_TAG: &str = "dilf4s/trie/branch/v1";
/// The empty-trie tag.
pub const EMPTY_TAG: &str = "dilf4s/trie/empty/v1";

/// Why a trie operation failed. `RootMismatch` is a well-formed proof of another trie; `NotCovered` a proof that does
/// not reach the query; the rest are malformed input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TrieError {
    /// A key, or a key mode, invalid for the mode's limits.
    InvalidKey {
        /// Why.
        reason: String,
    },
    /// A node that breaks the branch invariant.
    InvalidNode {
        /// Why.
        reason: String,
    },
    /// A partial trie that breaks the mode's structural rules (docs/specs/trie.md §4 step 2).
    InvalidProof {
        /// Why.
        reason: String,
    },
    /// The partial trie hashes to another root.
    RootMismatch {
        /// The trusted root, hex.
        expected: String,
        /// The recomputed root, hex.
        computed: String,
    },
    /// A proof that reaches a stub where the query needs an answer: neither presence nor absence is established.
    NotCovered {
        /// What was queried.
        what: String,
    },
    /// Records, segments, digests or arguments that do not decode.
    Encoding {
        /// Why.
        reason: String,
    },
}

impl TrieError {
    /// The Scala case-class name of this kind.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::InvalidKey { .. } => "InvalidKey",
            Self::InvalidNode { .. } => "InvalidNode",
            Self::InvalidProof { .. } => "InvalidProof",
            Self::RootMismatch { .. } => "RootMismatch",
            Self::NotCovered { .. } => "NotCovered",
            Self::Encoding { .. } => "Encoding",
        }
    }
}

impl fmt::Display for TrieError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidKey { reason } => write!(f, "invalid key: {reason}"),
            Self::InvalidNode { reason } => write!(f, "invalid node: {reason}"),
            Self::InvalidProof { reason } => write!(f, "invalid proof: {reason}"),
            Self::RootMismatch { expected, computed } => {
                write!(f, "root mismatch: expected {expected}, computed {computed}")
            }
            Self::NotCovered { what } => write!(f, "the proof does not cover {what}"),
            Self::Encoding { reason } => write!(f, "encoding: {reason}"),
        }
    }
}

impl core::error::Error for TrieError {}

pub(crate) fn encoding(reason: impl fmt::Display) -> TrieError {
    TrieError::Encoding { reason: alloc::format!("{reason}") }
}

fn invalid_key(reason: impl fmt::Display) -> TrieError {
    TrieError::InvalidKey { reason: alloc::format!("{reason}") }
}

/// The longest fixed key, in bytes.
pub const MAX_FIXED_BYTES: u16 = 1024;
/// The largest variable-mode key limit, in bytes.
pub const MAX_VARIABLE_BYTES: u16 = u16::MAX;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Mode {
    Fixed(u16),
    Variable(u16),
}

/// How keys are validated (docs/specs/trie.md §1): `fixed(n)` for exactly `n` bytes, `1 <= n <= 1024`, or
/// `variable(max)` for `0..=max` bytes, `max <= 65535`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KeyMode(Mode);

impl KeyMode {
    /// `fixed(n)`.
    pub fn fixed(bytes: u64) -> Result<Self, TrieError> {
        match u16::try_from(bytes) {
            Ok(n) if (1..=MAX_FIXED_BYTES).contains(&n) => Ok(Self(Mode::Fixed(n))),
            _ => Err(invalid_key(alloc::format!("fixed key length must be 1..{MAX_FIXED_BYTES}, not {bytes}"))),
        }
    }

    /// `variable(max)`.
    pub fn variable(max_bytes: u64) -> Result<Self, TrieError> {
        u16::try_from(max_bytes).map(|n| Self(Mode::Variable(n))).map_err(|_| {
            invalid_key(alloc::format!("variable key limit must be 0..{MAX_VARIABLE_BYTES}, not {max_bytes}"))
        })
    }

    /// A mode from its JLVM form, `{"fixed": n}` or `{"variable": max}`.
    pub fn from_value(value: &Value) -> Result<Self, TrieError> {
        let shape = || encoding("a mode is {\"fixed\": n} or {\"variable\": max}");
        let Value::Map(m) = value else { return Err(shape()) };
        match m.entries() {
            [(k, Value::Int(n))] if k == "fixed" => {
                n.to_u64().map_or_else(|| Err(invalid_key("fixed key length out of range")), Self::fixed)
            }
            [(k, Value::Int(n))] if k == "variable" => {
                n.to_u64().map_or_else(|| Err(invalid_key("variable key limit out of range")), Self::variable)
            }
            _ => Err(shape()),
        }
    }

    /// `Some(n)` for `fixed(n)`.
    pub const fn fixed_bytes(self) -> Option<u16> {
        match self.0 {
            Mode::Fixed(n) => Some(n),
            Mode::Variable(_) => None,
        }
    }

    /// `Some(max)` for `variable(max)`.
    pub const fn variable_max(self) -> Option<u16> {
        match self.0 {
            Mode::Variable(n) => Some(n),
            Mode::Fixed(_) => None,
        }
    }

    /// The longest key, in bits.
    pub fn max_bits(self) -> u64 {
        match self.0 {
            Mode::Fixed(n) | Mode::Variable(n) => u64::from(n).saturating_mul(8),
        }
    }

    /// Whether branches may hold values (variable mode).
    pub const fn allows_branch_values(self) -> bool {
        matches!(self.0, Mode::Variable(_))
    }

    /// Checks a key's length against the mode.
    pub fn check_key(self, key: &[u8]) -> Result<(), TrieError> {
        let len = key.len();
        match self.0 {
            Mode::Fixed(n) if len != usize::from(n) => {
                Err(invalid_key(alloc::format!("fixed({n}) mode needs {n}-byte keys, got {len}")))
            }
            Mode::Variable(n) if len > usize::from(n) => {
                Err(invalid_key(alloc::format!("variable({n}) mode allows at most {n} bytes, got {len}")))
            }
            _ => Ok(()),
        }
    }

    /// A key's path (its bits, MSB first), if the key is valid in this mode.
    pub fn path(self, key: &[u8]) -> Result<Bits, TrieError> {
        self.check_key(key).map(|()| Bits::from_bytes(key))
    }
}

impl fmt::Display for KeyMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Mode::Fixed(n) => write!(f, "fixed({n})"),
            Mode::Variable(n) => write!(f, "variable({n})"),
        }
    }
}

/// What a verified proof says about one key.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lookup<H: Hasher> {
    /// The key holds this value hash.
    Present(Digest<H>),
    /// The key is absent.
    Absent,
    /// The proof does not reach the key: neither presence nor absence is established.
    NotCovered,
}

// ---- canonical CBOR of node commitments, written directly (docs/specs/trie.md §3)

const MAJOR_UINT: u8 = 0x00;
const MAJOR_BYTES: u8 = 0x40;
const MAJOR_MAP: u8 = 0xa0;

/// A CBOR head in its shortest form.
fn head(out: &mut Vec<u8>, major: u8, n: u64) {
    if let Ok(small) = u8::try_from(n) {
        if small < 24 {
            out.push(major | small);
        } else {
            out.push(major | 0x18);
            out.push(small);
        }
    } else if let Ok(v) = u16::try_from(n) {
        out.push(major | 0x19);
        out.extend_from_slice(&v.to_be_bytes());
    } else if let Ok(v) = u32::try_from(n) {
        out.push(major | 0x1a);
        out.extend_from_slice(&v.to_be_bytes());
    } else {
        out.push(major | 0x1b);
        out.extend_from_slice(&n.to_be_bytes());
    }
}

fn bytes_item(out: &mut Vec<u8>, b: &[u8]) {
    head(out, MAJOR_BYTES, u64::try_from(b.len()).unwrap_or(u64::MAX));
    out.extend_from_slice(b);
}

/// `"x"` as a one-character text key.
fn key(out: &mut Vec<u8>, k: u8) {
    out.push(0x61);
    out.push(k);
}

/// `{"b": bytes, "n": bits}`.
fn segment_item(out: &mut Vec<u8>, segment: &Bits) {
    head(out, MAJOR_MAP, 2);
    key(out, b'b');
    bytes_item(out, segment.padded_bytes());
    key(out, b'n');
    head(out, MAJOR_UINT, segment.len());
}

/// The canonical encoding of a node commitment `{"l"?, "r"?, "s", "v"?}` (keys in canonical order).
pub(crate) fn node_cbor(segment: &Bits, value: Option<&[u8]>, left: Option<&[u8]>, right: Option<&[u8]>) -> Vec<u8> {
    let count = 1u64
        .saturating_add(u64::from(value.is_some()))
        .saturating_add(u64::from(left.is_some()))
        .saturating_add(u64::from(right.is_some()));
    let mut out = Vec::with_capacity(segment.padded_bytes().len().saturating_add(128));
    head(&mut out, MAJOR_MAP, count);
    if let Some(l) = left {
        key(&mut out, b'l');
        bytes_item(&mut out, l);
    }
    if let Some(r) = right {
        key(&mut out, b'r');
        bytes_item(&mut out, r);
    }
    key(&mut out, b's');
    segment_item(&mut out, segment);
    if let Some(v) = value {
        key(&mut out, b'v');
        bytes_item(&mut out, v);
    }
    out
}

/// The trie's hash functions (docs/specs/trie.md §3) over `H`, with the tag hashes computed once.
pub struct TrieHashing<H: Hasher> {
    value: Tag<H>,
    leaf: Tag<H>,
    branch: Tag<H>,
    empty: Digest<H>,
}

impl<H: Hasher> Clone for TrieHashing<H> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            leaf: self.leaf.clone(),
            branch: self.branch.clone(),
            empty: self.empty.clone(),
        }
    }
}

impl<H: Hasher> Default for TrieHashing<H> {
    fn default() -> Self {
        Self::new()
    }
}

impl<H: Hasher> TrieHashing<H> {
    /// The hash functions.
    pub fn new() -> Self {
        Self {
            value: Tag::new(VALUE_TAG),
            leaf: Tag::new(LEAF_TAG),
            branch: Tag::new(BRANCH_TAG),
            empty: Tag::<H>::new(EMPTY_TAG).hash(&[]),
        }
    }

    /// The root of the empty trie.
    pub fn empty(&self) -> Digest<H> {
        self.empty.clone()
    }

    /// `valueHash(b)` over value bytes. Crate-private: the public API commits values by `commit(v)` (`value_of`).
    pub(crate) fn value(&self, bytes: &[u8]) -> Digest<H> {
        self.value.hash(&[bytes])
    }

    /// `valueHash(commit(v))`: the hash a trie commits for a canonical value, as a JLVM program claims it.
    pub fn value_of(&self, value: &Value) -> Digest<H> {
        self.value(&value_commitment(value))
    }

    /// A leaf's hash.
    pub fn leaf(&self, segment: &Bits, value: &Digest<H>) -> Digest<H> {
        self.leaf.hash(&[&node_cbor(segment, Some(value.as_bytes()), None, None)])
    }

    /// A branch's hash; absent fields are omitted. The caller keeps the branch invariant.
    pub fn branch(
        &self,
        segment: &Bits,
        value: Option<&Digest<H>>,
        left: Option<&Digest<H>>,
        right: Option<&Digest<H>>,
    ) -> Digest<H> {
        let cbor =
            node_cbor(segment, value.map(Digest::as_bytes), left.map(Digest::as_bytes), right.map(Digest::as_bytes));
        self.branch.hash(&[&cbor])
    }
}

/// The branch invariant: two children, or one child and a value.
pub(crate) const fn valid_branch(has_value: bool, children: u8) -> bool {
    children == 2 || (children == 1 && has_value)
}
