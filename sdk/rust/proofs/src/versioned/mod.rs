//! The versioned trie (docs/specs/versioned-trie.md): state keys and namespaces, version keys, the top trie that
//! binds a head to one root, proof decoding and every check of §6, the chunked snapshot reader, and a replica that
//! follows deltas (§7).

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use dilf4s_core::cbor;
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::{MAX_DEPTH, Value};

use crate::tag::hex;
use crate::trie::{KeyMode, TrieError, TrieHashing, root_from_entries};

mod keys;
mod replica;
mod snapshot;
mod verifier;
mod wire;

pub use keys::{BytesKey, StateKey, StateKeyError, StateNamespace, U64Key, VersionKey};
pub use replica::Replica;
pub use snapshot::{Snapshot, SnapshotChunk, SnapshotReader};
pub use verifier::VerifiedState;
pub use wire::{At, Delta, VersionedTrieProof};

/// Typed failures of a versioned trie, named as Scala's `VersionedTrieError`. Versions appear as their version keys.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VersionedTrieError {
    /// A state key or namespace that breaks the grammar.
    Key(StateKeyError),
    /// A version the version key cannot encode, or bytes it cannot decode.
    InvalidVersion {
        /// Why.
        reason: String,
    },
    /// A delta whose version is not above the replica's head.
    VersionNotIncreasing {
        /// The head's version key.
        head: Vec<u8>,
        /// The delta's version key.
        next: Vec<u8>,
    },
    /// The version was never committed.
    UnknownVersion {
        /// Its version key.
        version: Vec<u8>,
    },
    /// A recomputed root differs from the claimed or trusted one; `component` names which.
    RootMismatch {
        /// `root`, `state`, `history` or `parent`.
        component: &'static str,
        /// The trusted or claimed root, hex (or `none`).
        expected: String,
        /// The recomputed root, hex (or `none`).
        computed: String,
    },
    /// A proof that does not reach the queried key, prefix, range or slot.
    NotCovered {
        /// What was queried.
        what: String,
    },
    /// A proof, delta or snapshot whose shape or contents are inconsistent with what it claims.
    Malformed {
        /// Why.
        reason: String,
    },
    /// A value or snapshot chunk over its limit, in canonical CBOR bytes.
    TooLarge {
        /// What.
        what: String,
        /// Its size; for a value, a lower bound once past the limit (a value is measured only up to the limit), and
        /// `u64::MAX` when too long to count.
        size: u64,
        /// The limit.
        max: u64,
    },
    /// A value nested deeper than every wire format can carry.
    TooDeep {
        /// What.
        what: String,
        /// Its depth.
        depth: usize,
        /// The limit.
        max: usize,
    },
    /// A trie failure, including a trie root mismatch.
    Trie(TrieError),
}

impl VersionedTrieError {
    /// The Scala case-class name of this kind.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Key(_) => "Key",
            Self::InvalidVersion { .. } => "InvalidVersion",
            Self::VersionNotIncreasing { .. } => "VersionNotIncreasing",
            Self::UnknownVersion { .. } => "UnknownVersion",
            Self::RootMismatch { .. } => "RootMismatch",
            Self::NotCovered { .. } => "NotCovered",
            Self::Malformed { .. } => "Malformed",
            Self::TooLarge { .. } => "TooLarge",
            Self::TooDeep { .. } => "TooDeep",
            Self::Trie(_) => "Trie",
        }
    }
}

impl fmt::Display for VersionedTrieError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(e) => write!(f, "{e}"),
            Self::InvalidVersion { reason } => write!(f, "invalid version: {reason}"),
            Self::VersionNotIncreasing { head, next } => {
                write!(
                    f,
                    "version {} is not above the head {}",
                    dilf4s_core::hex::encode(next),
                    dilf4s_core::hex::encode(head)
                )
            }
            Self::UnknownVersion { version } => {
                write!(f, "version {} was never committed", dilf4s_core::hex::encode(version))
            }
            Self::RootMismatch { component, expected, computed } => {
                write!(f, "{component} root mismatch: expected {expected}, computed {computed}")
            }
            Self::NotCovered { what } => write!(f, "the proof does not cover {what}"),
            Self::Malformed { reason } => write!(f, "malformed: {reason}"),
            Self::TooLarge { what, size, max } => write!(f, "{what} is {size} bytes, over {max}"),
            Self::TooDeep { what, depth, max } => write!(f, "{what} is {depth} levels deep, over {max}"),
            Self::Trie(e) => write!(f, "{e}"),
        }
    }
}

impl core::error::Error for VersionedTrieError {}

impl From<TrieError> for VersionedTrieError {
    fn from(e: TrieError) -> Self {
        Self::Trie(e)
    }
}

impl From<StateKeyError> for VersionedTrieError {
    fn from(e: StateKeyError) -> Self {
        Self::Key(e)
    }
}

pub(crate) fn malformed(reason: impl fmt::Display) -> VersionedTrieError {
    VersionedTrieError::Malformed { reason: alloc::format!("{reason}") }
}

pub(crate) fn mismatch<H: Hasher>(
    component: &'static str,
    expected: &Digest<H>,
    computed: &Digest<H>,
) -> VersionedTrieError {
    VersionedTrieError::RootMismatch { component, expected: hex(expected), computed: hex(computed) }
}

/// The largest value a key may hold, as canonical CBOR (1 MiB).
pub const MAX_VALUE_BYTES: u64 = 1 << 20;
/// At most this many entries or versions in one snapshot chunk.
pub const MAX_CHUNK_ENTRIES: usize = 1024;
/// The largest snapshot chunk, as canonical CBOR (4 MiB): refused before it is decoded.
pub const MAX_CHUNK_BYTES: u64 = 1 << 22;
/// The deepest value a key may hold: a delta or snapshot chunk wraps it in a map, an array and a map.
pub const MAX_VALUE_DEPTH: usize = MAX_DEPTH - 3;
/// The state trie's key limit: a state key's 256 bytes.
pub const STATE_KEY_BYTES: u64 = 256;

/// The top trie's slots.
pub const VERSION_SLOT: u8 = 0x00;
/// The state root's slot.
pub const STATE_SLOT: u8 = 0x01;
/// The history root's slot.
pub const HISTORY_SLOT: u8 = 0x02;

/// What one root commits to: the head version, its state root and the history root.
pub struct Head<V, H: Hasher> {
    /// The head version.
    pub version: V,
    /// The state trie's root at it.
    pub state_root: Digest<H>,
    /// The history trie's root, which maps every committed version up to and including it to its state root.
    pub history_root: Digest<H>,
}

/// A committed version and its state root.
pub type Version<V, H> = (V, Digest<H>);

impl<V: Clone, H: Hasher> Clone for Head<V, H> {
    fn clone(&self) -> Self {
        Self {
            version: self.version.clone(),
            state_root: self.state_root.clone(),
            history_root: self.history_root.clone(),
        }
    }
}

impl<V: PartialEq, H: Hasher> PartialEq for Head<V, H> {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version && self.state_root == other.state_root && self.history_root == other.history_root
    }
}

impl<V: Eq, H: Hasher> Eq for Head<V, H> {}

impl<V: fmt::Debug, H: Hasher> fmt::Debug for Head<V, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Head")
            .field("version", &self.version)
            .field("state_root", &self.state_root)
            .field("history_root", &self.history_root)
            .finish()
    }
}

/// A value every wire format can carry: at most [`MAX_VALUE_DEPTH`] deep and [`MAX_VALUE_BYTES`] long.
///
/// O(min(weight, [`MAX_VALUE_BYTES`])) and allocation-free: the encoding is measured, not built, and the measure stops
/// at the limit, so a compact value with a vast shared expansion is refused without expanding it.
pub fn check_value(key: &StateKey, value: &Value) -> Result<(), VersionedTrieError> {
    let what = || alloc::format!("the value of {}", key.as_str());
    if value.depth() > MAX_VALUE_DEPTH {
        return Err(VersionedTrieError::TooDeep { what: what(), depth: value.depth(), max: MAX_VALUE_DEPTH });
    }
    let max = usize::try_from(MAX_VALUE_BYTES).unwrap_or(usize::MAX);
    cbor::encoded_len_within(value, max).map(|_| ()).map_err(|e| VersionedTrieError::TooLarge {
        what: what(),
        size: e.at_least.and_then(|n| u64::try_from(n).ok()).unwrap_or(u64::MAX),
        max: MAX_VALUE_BYTES,
    })
}

/// A versioned trie's configuration: the version key (its width is part of what a verifier trusts) and the trie
/// hashing. Every check and decoder is a method.
pub struct VersionedTrie<K: VersionKey, H: Hasher> {
    key: K,
    hashing: TrieHashing<H>,
    top: KeyMode,
    state: KeyMode,
    history: KeyMode,
}

impl<K: VersionKey + Clone, H: Hasher> Clone for VersionedTrie<K, H> {
    fn clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            hashing: self.hashing.clone(),
            top: self.top,
            state: self.state,
            history: self.history,
        }
    }
}

impl<K: VersionKey, H: Hasher> VersionedTrie<K, H> {
    /// The configuration for version key `key`.
    pub fn new(key: K) -> Result<Self, VersionedTrieError> {
        let history = KeyMode::fixed(u64::from(key.width()))?;
        Ok(Self {
            key,
            hashing: TrieHashing::new(),
            top: KeyMode::fixed(1)?,
            state: KeyMode::variable(STATE_KEY_BYTES)?,
            history,
        })
    }

    /// The version key.
    pub const fn version_key(&self) -> &K {
        &self.key
    }

    /// The trie hashing.
    pub const fn hashing(&self) -> &TrieHashing<H> {
        &self.hashing
    }

    /// The top trie's mode, `fixed(1)`.
    pub const fn top_mode(&self) -> KeyMode {
        self.top
    }

    /// The state trie's mode, `variable(256)`.
    pub const fn state_mode(&self) -> KeyMode {
        self.state
    }

    /// The history trie's mode, `fixed(w)`.
    pub const fn history_mode(&self) -> KeyMode {
        self.history
    }

    /// `vkey(v)`.
    pub fn vkey(&self, version: &K::Version) -> Result<Vec<u8>, VersionedTrieError> {
        self.key.encode(version)
    }

    /// The hash a trie commits for a value: `valueHash(commit(v))`.
    pub fn value_hash(&self, value: &Value) -> Digest<H> {
        self.hashing.value_of(value)
    }

    /// The hash a trie commits for a byte string (top slots and history entries): `valueHash(0x00 || b)`, which is
    /// `value_hash(&Value::Bytes(b))`.
    pub fn bytes_hash(&self, bytes: &[u8]) -> Digest<H> {
        let mut committed = Vec::with_capacity(bytes.len().saturating_add(1));
        committed.push(dilf4s_core::commitment::BYTES_TAG);
        committed.extend_from_slice(bytes);
        self.hashing.value(&committed)
    }

    /// The commitment root of `head`: the root of its top trie (§3).
    pub fn root(&self, head: &Head<K::Version, H>) -> Result<Digest<H>, VersionedTrieError> {
        let vkey = self.vkey(&head.version)?;
        let slots = [
            (VERSION_SLOT, self.bytes_hash(&vkey)),
            (STATE_SLOT, self.bytes_hash(head.state_root.as_bytes())),
            (HISTORY_SLOT, self.bytes_hash(head.history_root.as_bytes())),
        ];
        let keys = [[VERSION_SLOT], [STATE_SLOT], [HISTORY_SLOT]];
        Ok(root_from_entries(&self.hashing, self.top, keys.iter().zip(slots).map(|(k, (_, h))| (k.as_slice(), h)))?)
    }
}
